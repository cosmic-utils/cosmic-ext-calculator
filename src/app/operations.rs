use crate::app::{latex, operator::Operator};
use cosmic::iced::futures::channel::oneshot;
use semver::Version;
use serde::{Deserialize, Serialize};
use std::{
    fmt::Display,
    ops::RangeInclusive,
    process::{Command, Stdio},
};

#[derive(Debug, Clone)]
pub struct Evaluation {
    pub outcome: String,
    pub exact: String,
}

pub async fn blocking<T: Send + 'static>(work: impl FnOnce() -> T + Send + 'static) -> T {
    let (sender, receiver) = oneshot::channel();
    std::thread::spawn(move || {
        let _ = sender.send(work());
    });
    receiver.await.expect("worker thread panicked")
}

#[derive(Debug, Default, Clone, PartialEq, Deserialize, Serialize)]
pub struct Calculator {
    pub expression: String,
    pub outcome: String,
    pub decimal_comma: bool,
    #[serde(skip)]
    pub outcome_exact: String,
    #[serde(skip)]
    pub chain_exact: bool,
}

impl Display for Calculator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.outcome)
    }
}

pub enum Message {
    Evaluate,
}

impl Calculator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function(expression: String, decimal_comma: bool) -> Self {
        Self {
            expression,
            decimal_comma,
            ..Self::default()
        }
    }

    /// History entries of plotted functions are the ones without an outcome.
    pub fn is_function(&self) -> bool {
        self.outcome.is_empty()
    }

    pub fn add_operator(&mut self, operator: Operator) {
        self.expression.push_str(operator.expression(self.decimal_comma));
    }

    pub fn on_number_press(&mut self, number: f32) {
        self.expression.push_str(&number.to_string());
    }

    /// Expression to evaluate: the exact form of the last result replaces the
    /// displayed one when it is the untouched prefix and an operator follows.
    pub fn eval_expression(&self) -> String {
        let expression = self.expression.trim();
        let chainable =
            self.chain_exact && !self.outcome.is_empty() && !self.outcome_exact.is_empty();
        match expression.strip_prefix(self.outcome.as_str()) {
            Some(tail)
                if chainable
                    && tail.starts_with(['+', '-', '*', '/', '×', '÷', '%', '^', '!']) =>
            {
                format!("({}){}", self.outcome_exact, tail)
            }
            _ => expression.to_string(),
        }
    }

    /// Builds the qalc command: exact output for chaining, 18 decimals for display.
    pub fn qalc_command(&self, exact: bool) -> Command {
        let mut command = Command::new("qalc");
        // Never let qalc block waiting on stdin.
        command.stdin(Stdio::null());
        command.env("LANG", "C");
        command.args(["-t", "-u8", "-set", "precision 50"]);
        if exact {
            command.args(["-set", "fr exact", "-set", "maxdeci off"]);
            command.args(["-set", "unicode off"]);
        } else {
            command.args(["-set", "maxdeci 18"]);
        }
        if self.decimal_comma {
            command.args(["-set", "decimal comma on"]);
        } else {
            command.args(["-set", "decimal comma off"]);
        }
        if autocalc() {
            command.args(["-set", "autocalc off"]);
        }
        command
    }

    pub fn evaluate(&self) -> Result<Evaluation, String> {
        let expression = self.eval_expression();
        let output = self
            .qalc_command(false)
            .arg(&expression)
            .output()
            .map_err(|_| "Failed to execute qalc command".to_string())?;
        let outcome = parse_output(output.stdout);
        // qalc warns on stderr even on success, so only an empty stdout is a failure.
        if outcome.is_empty() {
            let error = String::from_utf8(output.stderr).unwrap_or_default();
            if error.is_empty() {
                return Err("Failed to parse qalc output".to_string());
            }
            tracing::error!("An error occurred: {}", error);
            return Err("An error occurred".to_string());
        }
        // Second pass: keep an exact (or high-precision) value for chaining.
        let exact = self
            .qalc_command(true)
            .arg(&expression)
            .output()
            .ok()
            .map(|output| parse_output(output.stdout))
            .filter(|exact| !exact.is_empty() && exact.chars().count() <= 1000)
            .unwrap_or_else(|| outcome.clone());
        Ok(Evaluation { outcome, exact })
    }

    pub fn sample(
        &self,
        expression: &str,
        range: &RangeInclusive<f64>,
        samples: usize,
    ) -> Result<Vec<Option<f64>>, String> {
        let (start, end) = (*range.start(), *range.end());
        let cores = std::thread::available_parallelism().map_or(1, |cores| cores.get());
        let jobs = cores.min(4).min(samples / 50);
        if jobs < 2 {
            return self.sample_run(expression, start, end, samples);
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
                        self.sample_run(expression, at(first), at(next - 1), next - first)
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
        let output = self
            .qalc_command(false)
            .arg(format!("genvector({call})"))
            .output()
            .map_err(|_| "Failed to execute qalc command".to_string())?;
        let text = parse_output(output.stdout);
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

    pub fn on_operator_press(&mut self, operator: &Operator) -> Option<Message> {
        match operator {
            Operator::Add
            | Operator::Subtract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulus
            | Operator::Point
            | Operator::ParenthesesOpen
            | Operator::ParenthesesClose
            | Operator::Power
            | Operator::SquareRoot
            | Operator::Comma
            | Operator::Log
            | Operator::Ln
            | Operator::Log2
            | Operator::Factorial
            | Operator::Sin
            | Operator::Cos
            | Operator::Tan
            | Operator::Asin 
            | Operator::Acos 
            | Operator::Atan
            | Operator::Pi
            | Operator::E
            | Operator::Reciprocal => self.add_operator(operator.clone()),

            Operator::Clear => self.clear(),
            Operator::Negate => self.toggle_sign(),
            Operator::Equal => return Some(Message::Evaluate),
            Operator::Backspace => {
                self.expression.pop();
            }
        };
        None
    }
    pub fn toggle_sign(&mut self) {
        // Start index of the trailing number, if the expression ends with one.
        let Some(num_start) = self
            .expression
            .char_indices()
            .rev()
            .take_while(|(_, c)| c.is_ascii_digit() || *c == '.' || *c == ',')
            .last()
            .map(|(i, _)| i)
        else {
            return; // empty or not ending in a number: nothing to negate
        };

        let before = &self.expression[..num_start];
        // A '-' is unary at the start or right after an operator or '('.
        let is_unary_minus = before.ends_with('-')
            && matches!(
                before[..before.len() - 1].chars().next_back(),
                None | Some('+' | '-' | '*' | '/' | '×' | '÷' | '%' | '^' | '(')
            );

        if is_unary_minus {
            self.expression.remove(num_start - 1);
        } else {
            self.expression.insert(num_start, '-');
        }
    }

    pub fn clear(&mut self) {
        self.expression.clear();
        self.outcome = String::new();
        self.outcome_exact = String::new();
        self.chain_exact = false;
    }

    pub(crate) fn on_input(&mut self, input: String) {
        // Exact chaining stays valid only while the result prefix is untouched.
        self.chain_exact =
            self.chain_exact && !self.outcome.is_empty() && input.starts_with(&self.outcome);
        if input.chars().all(is_allowed) {
            self.expression = input;
        }
    }

    /// The field pasted: `content` is its whole new text, the clipboard inserted at the cursor.
    pub fn on_paste(&mut self, content: &str) -> Result<(), PasteError> {
        self.on_input(clean_paste(content, self.decimal_comma)?);
        Ok(())
    }
}

/// Permissive, since qalc validates the expression.
fn is_allowed(c: char) -> bool {
    c.is_alphanumeric()
        || c.is_whitespace()
        || matches!(
            c,
            '+' | '-'
                | '*'
                | '/'
                | '÷'
                | '×'
                | '%'
                | '.'
                | ','
                | ';'
                | '('
                | ')'
                | '^'
                | '√'
                | '!'
                | '\u{8}'
                | '−'
                | '·'
                | '⋅'
                | '|'
                | '°'
        )
}

#[derive(Debug, PartialEq, Eq)]
pub enum PasteError {
    Equation,
    Unsupported(char),
    Latex(String),
}

pub fn clean_paste(text: &str, decimal_comma: bool) -> Result<String, PasteError> {
    // Invisible operators and spaces that formulas copied from web pages carry along.
    let visible: String = text
        .chars()
        .filter(|c| !matches!(c, '\u{200b}'..='\u{200d}' | '\u{2060}'..='\u{2064}' | '\u{feff}'))
        .collect();
    let line = visible.split_whitespace().collect::<Vec<_>>().join(" ");
    let line = strip_math_delimiters(&line);
    let expression = match split_definition(line) {
        None => line,
        Some((head, tail)) => {
            let (head, tail) = (head.trim(), tail.trim());
            if tail.is_empty() {
                head
            } else if is_definition(head) {
                tail
            } else {
                return Err(PasteError::Equation);
            }
        }
    };
    if expression.contains(['\\', '{', '}']) {
        return latex::to_expression(expression, decimal_comma).map_err(PasteError::Latex);
    }
    match expression.chars().find(|&c| !is_allowed(c)) {
        Some(c) => Err(PasteError::Unsupported(c)),
        None => Ok(expression.to_string()),
    }
}

fn strip_math_delimiters(line: &str) -> &str {
    let line = line.trim();
    for (open, close) in [("$$", "$$"), ("$", "$"), ("\\(", "\\)"), ("\\[", "\\]")] {
        if let Some(inner) = line
            .strip_prefix(open)
            .and_then(|rest| rest.strip_suffix(close))
        {
            return inner.trim();
        }
    }
    line
}

/// Splits at the first `=` outside braces: the one in `\sum_{k=1}` defines nothing.
fn split_definition(line: &str) -> Option<(&str, &str)> {
    let mut depth = 0_usize;
    for (i, c) in line.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            '=' if depth == 0 => return Some((&line[..i], &line[i + 1..])),
            _ => {}
        }
    }
    None
}

fn is_definition(head: &str) -> bool {
    let head = head.replace("\\left", "").replace("\\right", "");
    let name = |s: &str| {
        s.chars().next().is_some_and(char::is_alphabetic)
            && s.chars().all(|c| c.is_alphanumeric() || c == '_')
    };
    match head.split_once('(') {
        None => name(head.trim()),
        Some((function, arguments)) => {
            name(function.trim_end())
                && arguments
                    .strip_suffix(')')
                    .is_some_and(|arguments| arguments.split([',', ';']).all(|a| name(a.trim())))
        }
    }
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

/// Cleans qalc stdout into one line, stripping any interactive-prompt artifact.
pub fn parse_output(stdout: Vec<u8>) -> String {
    String::from_utf8(stdout)
        .unwrap_or_default()
        .replace(['\n', '\r'], "")
        .replace("> ", "")
        .trim()
        .to_string()
}

/// Returns the version of the `qalc` command-line tool.
fn qalc_version() -> Option<String> {
    let output = Command::new("qalc").arg("--version").output().ok()?;

    if !output.status.success() {
        return None;
    }

    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Some(version)
}

/// Checks if the system uses a decimal comma instead of a decimal point.
pub async fn uses_decimal_comma() -> bool {
    let spawn_result = Command::new("locale")
        .arg("-ck")
        .arg("decimal_point")
        .stderr(Stdio::null())
        .output();

    if let Ok(output) = spawn_result
        && let Ok(string) = String::from_utf8(output.stdout)
    {
        return string.contains("decimal_point=\",\"");
    }

    false
}

pub fn autocalc() -> bool {
    let min_version = Version::parse("5.4.0").unwrap();
    qalc_version()
        .and_then(|version| Version::parse(&version).ok())
        .is_some_and(|current| current >= min_version)
}
#[cfg(test)]
mod tests {
    use super::{Calculator, PasteError, clean_paste};

    #[test]
    fn sample_handles_gaps_and_locale() {
        let calc = Calculator::new();
        let values = calc.sample("1/x", &(-1.0..=1.0), 5).unwrap();
        assert_eq!(
            values,
            vec![Some(-1.0), Some(-2.0), None, Some(2.0), Some(1.0)]
        );
        let values = calc.sample("sqrt(x)", &(-1.0..=1.0), 3).unwrap();
        assert_eq!(values, vec![None, Some(0.0), Some(1.0)]);
        let values = calc.sample("ln(x)", &(0.0..=1.0), 2).unwrap();
        assert_eq!(values, vec![None, Some(0.0)]);
        let comma = Calculator {
            decimal_comma: true,
            ..Calculator::new()
        };
        let values = comma.sample("x/2", &(-1.0..=1.0), 5).unwrap();
        assert_eq!(
            values,
            vec![Some(-0.5), Some(-0.25), Some(0.0), Some(0.25), Some(0.5)]
        );
        let values = calc.sample("sin(x)*x", &(-10.0..=10.0), 400).unwrap();
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
            let values = calc.sample("x", &(0.0..=1.0), 1000).unwrap();
            assert_eq!(values.len(), 1000);
            for (index, value) in values.iter().enumerate() {
                let expected = index as f64 / 999.0;
                assert!((value.unwrap() - expected).abs() < 1e-9, "sample {index}");
            }
        }
    }

    #[test]
    fn paste_is_cleaned_for_evaluation() {
        let clean = |text: &str| clean_paste(text, false);
        assert_eq!(clean("y = x^2"), Ok("x^2".to_string()));
        assert_eq!(clean("f(x) = sin(x)·x"), Ok("sin(x)·x".to_string()));
        assert_eq!(clean("g(x, y)=x+y"), Ok("x+y".to_string()));
        assert_eq!(clean("2+3="), Ok("2+3".to_string()));
        assert_eq!(clean("x^2 = 4"), Err(PasteError::Equation));
        assert_eq!(clean("sin(x)^2 = 1"), Err(PasteError::Equation));
        assert_eq!(
            clean(" sin(x)\n *\u{a0}x\u{2061} \n"),
            Ok("sin(x) * x".to_string())
        );
        assert_eq!(clean("√(1−x²)·|x|"), Ok("√(1−x²)·|x|".to_string()));
        assert_eq!(clean("x ≤ 2"), Err(PasteError::Unsupported('≤')));
    }

    #[test]
    fn latex_is_read_like_the_rest_of_the_paste() {
        let clean = |text: &str| clean_paste(text, false);
        assert_eq!(
            clean(r"f(x)=\frac{x^2-1}{2}\sin(3x)"),
            Ok("(x^2-1)/2*sin(3*x)".to_string())
        );
        assert_eq!(clean(r"$y = \sqrt{x}$"), Ok("sqrt(x)".to_string()));
        assert_eq!(
            clean(r"\[ f\left(x\right) = \left|x\right|^3 \]"),
            Ok("abs(x)^3".to_string())
        );
        assert_eq!(
            clean(r"\sum_{k=1}^{3} kx"),
            Ok("sum(y*x, 1, 3, y)".to_string())
        );
        assert_eq!(
            clean_paste(r"\max(1.5x, 2)", true),
            Ok("max(1,5*x; 2)".to_string())
        );
        assert_eq!(clean(r"\sin(x) = 0.5"), Err(PasteError::Equation));
        assert_eq!(
            clean(r"\begin{cases} x \end{cases}"),
            Err(PasteError::Latex("\\begin".to_string()))
        );
        assert_eq!(
            clean(r"x_1+\alpha"),
            Err(PasteError::Latex("_".to_string()))
        );
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
        let values = calc.sample(&pasted, &(-10.0..=10.0), 9).unwrap();
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
        let values = calc.sample(&sum, &(1.0..=2.0), 2).unwrap();
        for (value, x) in values.iter().zip([1.0, 2.0]) {
            let expected: f64 = (1..=20)
                .map(|k| (f64::from(k) * x).sin() / f64::from(k))
                .sum();
            assert!((value.unwrap() - expected).abs() < 1e-9);
        }
        let root = clean_paste(r"\sqrt[3]{x}", false).unwrap();
        let values = calc.sample(&root, &(-8.0..=-1.0), 2).unwrap();
        assert_eq!(values, vec![Some(-2.0), Some(-1.0)]);
        let constant = clean_paste(r"\int_0^1 t^2\,dt", false).unwrap();
        let values = calc.sample(&constant, &(-1.0..=1.0), 3).unwrap();
        assert!(
            values
                .iter()
                .all(|v| (v.unwrap() - 1.0 / 3.0).abs() < 1e-12)
        );
    }

    #[test]
    fn pasted_text_reaches_the_field_and_qalc() {
        let mut calc = Calculator::new();
        calc.on_input("−5+".into());
        assert_eq!(calc.expression, "−5+");
        calc.on_input("−5+@".into());
        assert_eq!(calc.expression, "−5+");

        calc.on_input("2+".into());
        assert_eq!(calc.on_paste("2+x**2"), Ok(()));
        assert_eq!(calc.expression, "2+x**2");
        assert_eq!(calc.on_paste("2+@x"), Err(PasteError::Unsupported('@')));
        assert_eq!(calc.expression, "2+x**2");
        assert_eq!(calc.on_paste(r"2+x**2+\frac{1}{2}"), Ok(()));
        assert_eq!(calc.expression, "2+x^2+1/2");

        let pasted = clean_paste("y = √(1−x²)·|x|", false).unwrap();
        let values = calc.sample(&pasted, &(-1.0..=1.0), 5).unwrap();
        let expected = [
            0.0,
            0.433_012_701_892_219_3,
            0.0,
            0.433_012_701_892_219_3,
            0.0,
        ];
        assert_eq!(values.len(), expected.len());
        for (value, expected) in values.iter().zip(expected) {
            assert!((value.unwrap() - expected).abs() < 1e-12);
        }
    }

    #[test]
    fn history_entries_tell_functions_from_calculations() {
        assert!(Calculator::function("sin(x)*x".into(), false).is_function());
        let mut calc = Calculator::new();
        calc.expression = "1+1".into();
        calc.outcome = calc.evaluate().unwrap().outcome;
        assert!(!calc.is_function());
    }

    #[test]
    fn evaluate_runs_both_passes() {
        let mut calc = Calculator::new();
        calc.expression = "1/3".into();
        let evaluation = calc.evaluate().unwrap();
        assert_eq!(evaluation.outcome, "0.333333333333333333");
        assert_eq!(evaluation.exact, "1/3");
        calc.outcome = evaluation.outcome.clone();
        calc.outcome_exact = evaluation.exact;
        calc.chain_exact = true;
        calc.expression = format!("{}×3", evaluation.outcome);
        assert_eq!(calc.evaluate().unwrap().outcome, "1");
    }
}
