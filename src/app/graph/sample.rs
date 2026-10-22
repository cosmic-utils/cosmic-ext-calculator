use super::numeric;
use crate::app::operations::{Calculator, Evaluation, parse_output};
use crate::fl;
use std::io::Read;
use std::ops::RangeInclusive;
use std::process::{Command, Stdio};
use std::time::Duration;

impl Calculator {
    pub fn sample(
        &self,
        expression: &str,
        range: &RangeInclusive<f64>,
        samples: usize,
        superseded: impl Fn() -> bool + Sync,
    ) -> Result<Vec<Option<f64>>, String> {
        // Sums, products and integrals are computed natively, where qalc cannot.
        if let Some(formula) = numeric::Formula::shared(expression, self.decimal_comma) {
            let points = sample_native(&formula, range, samples, range, &superseded, &|_| {})?;
            return Ok(points.into_iter().map(|(_, y)| y).collect());
        }
        let (start, end) = (*range.start(), *range.end());
        let cores = std::thread::available_parallelism().map_or(1, |cores| cores.get());
        let jobs = cores.min(4).min(samples / 50);
        let superseded = &superseded;
        if jobs < 2 {
            return self.sample_run(expression, start, end, samples, superseded);
        }
        let at = |index: usize| {
            if index + 1 == samples {
                end
            } else {
                start + (end - start) * index as f64 / (samples - 1) as f64
            }
        };
        // Consecutive runs of samples, one per process, so the spacing is the whole's.
        let runs: Vec<_> = (0..jobs)
            .map(|job| (samples * job / jobs, samples * (job + 1) / jobs))
            .collect();
        std::thread::scope(|scope| {
            let workers: Vec<_> = runs
                .iter()
                .map(|&(first, next)| {
                    scope.spawn(move || {
                        let count = next - first;
                        self.sample_run(expression, at(first), at(next - 1), count, superseded)
                    })
                })
                .collect();
            let mut values = Vec::with_capacity(samples);
            for worker in workers {
                values.extend(worker.join().map_err(|_| "Sampling failed".to_string())??);
            }
            Ok(values)
        })
    }

    fn sample_run(
        &self,
        expression: &str,
        from: f64,
        to: f64,
        count: usize,
        superseded: &impl Fn() -> bool,
    ) -> Result<Vec<Option<f64>>, String> {
        let separator = if self.decimal_comma { "; " } else { ", " };
        let number = |value: f64| {
            let text = value.to_string();
            if self.decimal_comma {
                text.replace('.', ",")
            } else {
                text
            }
        };
        let call = [
            expression.trim().to_string(),
            number(from),
            number(to),
            count.to_string(),
            "x".to_string(),
        ]
        .join(separator);
        let mut command = self.qalc_command(false);
        // Every digit qalc has: 18 fixed decimals would flatten the curve far from the origin.
        command.args(["-set", "maxdeci off"]);
        let text = run(command, format!("genvector({call})"), superseded)?;
        let inner = text
            .strip_prefix('[')
            .and_then(|text| text.strip_suffix(']'))
            .ok_or_else(|| "Malformed expression".to_string())?;
        let values: Vec<_> = inner
            .split("  ")
            .map(|item| parse_number(item, self.decimal_comma))
            .collect();
        // A run that comes back short or long would shift every sample after it.
        if values.len() != count {
            return Err("Malformed expression".to_string());
        }
        Ok(values)
    }
}

pub(super) fn sample_native(
    formula: &numeric::Formula,
    range: &RangeInclusive<f64>,
    samples: usize,
    view: &RangeInclusive<f64>,
    superseded: &(impl Fn() -> bool + Sync),
    publish: &(impl Fn(numeric::Points) + Sync),
) -> Result<numeric::Points, String> {
    let points = formula.sample(range, samples, view, superseded, publish);
    points.map_err(|fault| match fault {
        numeric::Fault::Inaccurate => fl!("numeric-inaccurate"),
        _ => "Superseded".to_string(),
    })
}

pub(super) fn evaluate_native(
    formula: &numeric::Formula,
    x: &str,
    decimal_comma: bool,
    superseded: &dyn Fn() -> bool,
) -> Result<Evaluation, String> {
    let x = numeric::constant(x, decimal_comma).map_err(|_| "Malformed expression".to_string())?;
    let value = formula.evaluate(x, superseded, numeric::TOLERANCE);
    let (value, digits) = value.map_err(|fault| match fault {
        numeric::Fault::Inaccurate => fl!("numeric-inaccurate"),
        numeric::Fault::Cancelled => "Superseded".to_string(),
        _ => fl!("numeric-undefined"),
    })?;
    // Only the digits the value is known to, within the 18 decimals shown everywhere.
    let outcome = numeric::format(numeric::round(value, digits), 18, decimal_comma);
    Ok(Evaluation {
        exact: outcome.clone(),
        outcome,
    })
}

fn run(
    mut command: Command,
    expression: String,
    superseded: &impl Fn() -> bool,
) -> Result<String, String> {
    let mut child = command
        .arg(expression)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|_| "Failed to execute qalc command".to_string())?;
    // Read while it runs: a full pipe would keep qalc from ever exiting.
    let mut stdout = child.stdout.take().expect("stdout is piped");
    let reader = std::thread::spawn(move || {
        let mut output = Vec::new();
        stdout.read_to_end(&mut output).map(|_| output)
    });
    while let Ok(None) = child.try_wait() {
        if superseded() {
            let _ = child.kill();
            let _ = child.wait();
            return Err("Superseded".to_string());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    let output = reader
        .join()
        .ok()
        .and_then(Result::ok)
        .ok_or_else(|| "Sampling failed".to_string())?;
    Ok(parse_output(output))
}

fn parse_number(item: &str, decimal_comma: bool) -> Option<f64> {
    let item = item.trim().replace('−', "-");
    let item = if decimal_comma {
        item.replace(',', ".")
    } else {
        item
    };
    item.parse::<f64>().ok().filter(|value| value.is_finite())
}

#[cfg(test)]
mod tests {
    use super::run;
    use crate::app::operations::{Calculator, clean_paste};
    use std::time::{Duration, Instant};

    #[test]
    fn sample_handles_gaps_and_locale() {
        let calc = Calculator::new();
        let values = calc.sample("1/x", &(-1.0..=1.0), 5, || false).unwrap();
        assert_eq!(
            values,
            vec![Some(-1.0), Some(-2.0), None, Some(2.0), Some(1.0)]
        );
        let values = calc.sample("sqrt(x)", &(-1.0..=1.0), 3, || false).unwrap();
        assert_eq!(values, vec![None, Some(0.0), Some(1.0)]);
        let values = calc.sample("ln(x)", &(0.0..=1.0), 2, || false).unwrap();
        assert_eq!(values, vec![None, Some(0.0)]);
        let comma = Calculator {
            decimal_comma: true,
            ..Calculator::new()
        };
        let values = comma.sample("x/2", &(-1.0..=1.0), 5, || false).unwrap();
        assert_eq!(
            values,
            vec![Some(-0.5), Some(-0.25), Some(0.0), Some(0.25), Some(0.5)]
        );
        let values = calc
            .sample("sin(x)*x", &(-10.0..=10.0), 400, || false)
            .unwrap();
        assert_eq!(values.len(), 400);
        assert!(values.iter().all(Option::is_some));
    }

    #[test]
    fn samples_taken_in_runs_keep_the_spacing_of_the_whole() {
        for decimal_comma in [false, true] {
            let calc = Calculator {
                decimal_comma,
                ..Calculator::new()
            };
            let values = calc.sample("x", &(0.0..=1.0), 1000, || false).unwrap();
            assert_eq!(values.len(), 1000);
            for (index, value) in values.iter().enumerate() {
                let expected = index as f64 / 999.0;
                assert!((value.unwrap() - expected).abs() < 1e-9, "sample {index}");
            }
        }
    }

    #[test]
    fn samples_keep_their_digits_far_from_the_origin() {
        let calc = Calculator::new();
        let values = calc.sample("e^(-x)", &(30.0..=40.0), 2, || false).unwrap();
        for (value, x) in values.iter().zip([30.0f64, 40.0]) {
            let expected = (-x).exp();
            assert!(
                (value.unwrap() - expected).abs() <= expected * 1e-15,
                "{value:?}"
            );
        }
        let values = calc
            .sample("x*10^(-25)", &(1.0..=2.0), 2, || false)
            .unwrap();
        assert_eq!(values, vec![Some(1e-25), Some(2e-25)]);
    }

    #[test]
    fn a_plot_stops_once_superseded() {
        // A numerical integral at every sample: minutes of work for qalc.
        let slow = "genvector(integrate(sin(cos(y)), 0, x, y), -30, -10, 600, x)".to_string();
        let qalc = Calculator::new().qalc_command(false);
        let started = Instant::now();
        let newer = || started.elapsed() > Duration::from_millis(200);
        assert_eq!(run(qalc, slow, &newer), Err("Superseded".to_string()));
        assert!(started.elapsed() < Duration::from_secs(5));
    }

    #[test]
    fn pasted_latex_plots_like_its_formula() {
        let calc = Calculator::new();
        let pasted = clean_paste(
            r"f(x)=\frac{(x^2-4)(x^2-1)}{10}+\sin(3x)\cos(2x)+\frac{e^{-x^2/8}\sin(5x)}{2}+\ln(x^2+1)-\frac{\sqrt{x+5}}{3}",
            false,
        )
        .unwrap();
        let formula = |x: f64| {
            (x * x - 4.0) * (x * x - 1.0) / 10.0
                + (3.0 * x).sin() * (2.0 * x).cos()
                + (-x * x / 8.0).exp() * (5.0 * x).sin() / 2.0
                + (x * x + 1.0).ln()
                - (x + 5.0).sqrt() / 3.0
        };
        let values = calc.sample(&pasted, &(-10.0..=10.0), 9, || false).unwrap();
        assert_eq!(values.len(), 9);
        for (i, value) in values.iter().enumerate() {
            let x = -10.0 + 2.5 * i as f64;
            if x < -5.0 {
                assert_eq!(*value, None);
            } else {
                assert!((value.unwrap() - formula(x)).abs() < 1e-9, "x = {x}");
            }
        }
        let sum = clean_paste(r"\sum_{k=1}^{20}\frac{\sin(kx)}{k}", false).unwrap();
        let values = calc.sample(&sum, &(1.0..=2.0), 2, || false).unwrap();
        for (value, x) in values.iter().zip([1.0, 2.0]) {
            let expected: f64 = (1..=20)
                .map(|k| (f64::from(k) * x).sin() / f64::from(k))
                .sum();
            assert!((value.unwrap() - expected).abs() < 1e-9);
        }
        let root = clean_paste(r"\sqrt[3]{x}", false).unwrap();
        let values = calc.sample(&root, &(-8.0..=-1.0), 2, || false).unwrap();
        assert_eq!(values, vec![Some(-2.0), Some(-1.0)]);
        let constant = clean_paste(r"\int_0^1 t^2\,dt", false).unwrap();
        let values = calc.sample(&constant, &(-1.0..=1.0), 3, || false).unwrap();
        assert!(
            values
                .iter()
                .all(|v| (v.unwrap() - 1.0 / 3.0).abs() < 1e-12)
        );
    }
}
