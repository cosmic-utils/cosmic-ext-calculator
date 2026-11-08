//! Native evaluation of sums, products and integrals, which qalc cannot compute, to 50 digits.

mod fourier;
mod plan;
mod qd;
mod quad;
mod series;

use crate::app::latex::{self, Node, Op};
pub use qd::Qd;
use std::cell::Cell;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};
use std::time::{Duration, Instant};

/// Relative error a value must be known within before it is shown: 50 digits.
pub const TOLERANCE: f64 = 1e-50;
/// Relative error past which a value is dropped: fewer than 6 right digits.
pub const MINIMUM: f64 = 1e-6;
/// Relative error a point of the curve must be known within: its f64's 16 digits, all the curve can show.
pub const CURVE_TOLERANCE: f64 = 1e-16;
/// Rounding a sum of `Qd` takes on with each term, relative to the largest.
const ROUNDING: f64 = 1e-62;
const KEPT: usize = 1 << 18;
const RUN: usize = 64;
const SHOWN: Duration = Duration::from_millis(500);

pub type Points = Vec<(f64, Option<f64>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Fault {
    Undefined,
    Inaccurate,
    /// Something the engine does not compute; qalc keeps such formulas.
    Unsupported(String),
    Cancelled,
}

pub(crate) struct Control<'a> {
    superseded: &'a dyn Fn() -> bool,
    ticks: Cell<u64>,
    tolerance: f64,
}

impl Control<'_> {
    pub(crate) fn tick(&self) -> Result<(), Fault> {
        let ticks = self.ticks.get() + 1;
        self.ticks.set(ticks);
        if ticks.is_multiple_of(4096) && (self.superseded)() {
            return Err(Fault::Cancelled);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum Unary {
    Sin,
    Cos,
    Tan,
    Cot,
    Sec,
    Csc,
    Sinh,
    Cosh,
    Tanh,
    Coth,
    Sech,
    Csch,
    Asin,
    Acos,
    Atan,
    Acot,
    Asinh,
    Acosh,
    Atanh,
    Ln,
    Log10,
    Exp,
    Sqrt,
    Abs,
    Floor,
    Ceil,
    Round,
    Trunc,
    Factorial,
}

#[derive(Debug, Clone)]
enum Expr {
    Const(Qd),
    X,
    Slot(usize),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Unary(Unary, Box<Expr>),
    Log(Box<Expr>, Box<Expr>),
    Root(Box<Expr>, Box<Expr>),
    Binomial(Box<Expr>, Box<Expr>),
    Max(Vec<Expr>, bool),
    Series {
        product: bool,
        body: Box<Expr>,
        from: Box<Expr>,
        to: Box<Expr>,
        slot: usize,
    },
    Integral {
        body: Box<Expr>,
        from: Box<Expr>,
        to: Box<Expr>,
        slot: usize,
    },
    /// Σ_{n≥from} coefficient·sin(frequency·n + phase)/n^power, or with cos: summed in closed form.
    Fourier {
        sine: bool,
        power: u32,
        frequency: Box<Expr>,
        phase: Box<Expr>,
        coefficient: Box<Expr>,
        from: Box<Expr>,
    },
    /// A part without x, remembered for each value of the variables it reads.
    Memo(Arc<plan::Memo>),
    /// The first, or the second where the first has no value: a faster plan, then the plain one.
    Either(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Estimate {
    pub(crate) value: Qd,
    pub(crate) error: f64,
}

impl Estimate {
    pub(crate) fn exact(value: Qd) -> Estimate {
        Estimate { value, error: 0.0 }
    }

    pub(crate) fn relative(self) -> f64 {
        let size = self.value.abs().to_f64();
        match self.error {
            0.0 => 0.0,
            _ if size == 0.0 => f64::INFINITY,
            error => error / size,
        }
    }

    pub(crate) fn plus(self, other: Estimate) -> Estimate {
        Estimate {
            value: self.value + other.value,
            error: self.error + other.error,
        }
    }

    pub(crate) fn scaled(self, factor: Qd) -> Estimate {
        Estimate {
            value: self.value * factor,
            error: weighted(factor.abs().to_f64(), self.error),
        }
    }

    pub(crate) fn times(self, other: Estimate) -> Estimate {
        let (a, b) = (self.value.abs().to_f64(), other.value.abs().to_f64());
        Estimate {
            value: self.value * other.value,
            error: weighted(a, other.error)
                + weighted(b, self.error)
                + weighted(self.error, other.error),
        }
    }

    pub(crate) fn over(self, other: Estimate) -> Estimate {
        let divisor = other.value.abs().to_f64();
        let ratio = (self.value / other.value).abs().to_f64();
        let error = match other.error {
            0.0 => self.error / divisor,
            error if error < divisor => (self.error + ratio * error) / (divisor - error),
            _ => f64::INFINITY,
        };
        Estimate {
            value: self.value / other.value,
            error,
        }
    }
}

/// An error scaled by `size`: an exact zero takes away even an unbounded one.
pub(crate) fn weighted(size: f64, error: f64) -> f64 {
    if size == 0.0 { 0.0 } else { size * error }
}

type Known = Result<(Qd, usize), Fault>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Sampled {
    Value(f64),
    Undefined,
    Inaccurate,
}

impl Sampled {
    fn known(value: Qd, digits: usize) -> Sampled {
        // Short of an f64's digits, only those the point is known to are kept: none drawn is wrong.
        let value = match digits {
            ..16 => round(value, digits).to_f64(),
            _ => value.to_f64(),
        };
        if value.is_finite() {
            Sampled::Value(value)
        } else {
            Sampled::Undefined
        }
    }

    fn value(self) -> Option<f64> {
        match self {
            Sampled::Value(value) => Some(value),
            Sampled::Undefined | Sampled::Inaccurate => None,
        }
    }
}

#[derive(Debug)]
pub struct Formula {
    root: Expr,
    slots: usize,
    /// The points of its curve found so far, by the bits of their x: a pan takes only what is new.
    curve: Mutex<HashMap<u64, Sampled>>,
}

impl Formula {
    /// The field's formula, when it needs the engine and the engine reads all of it.
    pub fn calculus(text: &str, decimal_comma: bool) -> Option<Formula> {
        let node = latex::parse(text, decimal_comma).ok()?;
        if !has_calculus(&node) {
            return None;
        }
        let mut compiler = Compiler::default();
        let root = plan::plan(compiler.compile(&node).ok()?);
        Some(Formula {
            root,
            slots: compiler.slots,
            curve: Mutex::default(),
        })
    }

    pub fn evaluate(&self, x: Qd, superseded: &dyn Fn() -> bool, tolerance: f64) -> Known {
        let control = Control {
            superseded,
            ticks: Cell::new(0),
            tolerance,
        };
        let mut slots = vec![Qd::ZERO; self.slots];
        let estimate = eval(&self.root, x, &mut slots, &control)?;
        if !estimate.value.is_finite() {
            return Err(Fault::Undefined);
        }
        let relative = estimate.relative();
        if relative.is_nan() || relative > MINIMUM {
            return Err(Fault::Inaccurate);
        }
        let aimed = (-tolerance.log10()).floor() as usize;
        let digits = match relative {
            0.0 => aimed,
            relative => ((-relative.log10()).floor() as usize).min(aimed),
        };
        Ok((estimate.value, digits))
    }

    /// One formula per text, so that its memos serve every plot and readout of it.
    pub fn shared(text: &str, decimal_comma: bool) -> Option<Arc<Formula>> {
        static LAST: Mutex<Option<(String, bool, Arc<Formula>)>> = Mutex::new(None);
        let mut last = LAST.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some((kept, comma, formula)) = &*last
            && kept == text
            && *comma == decimal_comma
        {
            return Some(Arc::clone(formula));
        }
        let formula = Arc::new(Formula::calculus(text, decimal_comma)?);
        *last = Some((text.to_string(), decimal_comma, Arc::clone(&formula)));
        Some(formula)
    }

    /// Every 32nd point of the view, then of the rest, then all of them; known points come at once.
    pub fn sample(
        &self,
        range: &RangeInclusive<f64>,
        samples: usize,
        view: &RangeInclusive<f64>,
        superseded: &(impl Fn() -> bool + Sync),
        publish: &(impl Fn(Points) + Sync),
    ) -> Result<Points, Fault> {
        if superseded() {
            return Err(Fault::Cancelled);
        }
        let (start, end) = (*range.start(), *range.end());
        let at = |i: usize| sample_x(range, samples, i);
        let (first, last) = match samples {
            0 | 1 => (0, samples.saturating_sub(1)),
            _ => {
                let last = (samples - 1) as f64;
                let index = |x: f64| ((x - start) / (end - start) * last).clamp(0.0, last);
                let first = index(*view.start()).ceil() as usize;
                (first, (index(*view.end()).floor() as usize).max(first))
            }
        };
        // Where the points fall among the multiples of their spacing: every 32nd stays so after a pan.
        let spacing = (end - start) / samples.saturating_sub(1).max(1) as f64;
        let offset = match start / spacing {
            index if index.abs() < 2f64.powi(53) => index.round() as i64,
            _ => 0,
        };
        let every = |i: usize, stride: i64| (offset + i as i64).rem_euclid(stride) == 0;
        let outside = |i: usize| first.saturating_sub(i).max(i.saturating_sub(last));
        let known: Vec<Option<Sampled>> = {
            let curve = lock(&self.curve);
            (0..samples)
                .map(|i| curve.get(&at(i).to_bits()).copied())
                .collect()
        };
        let mut queued: Vec<bool> = known.iter().map(Option::is_some).collect();
        let mut runs: Vec<Vec<usize>> = Vec::new();
        let finer = [16, 8, 4, 2, 1];
        let passes = [(32, true), (32, false)]
            .into_iter()
            .chain(finer.map(|stride| (stride, true)))
            .chain(finer.map(|stride| (stride, false)));
        for (stride, inside) in passes {
            let mut batch: Vec<usize> = (0..samples)
                .filter(|&i| {
                    !queued[i] && (first..=last).contains(&i) == inside && every(i, stride)
                })
                .collect();
            batch.sort_by_key(|&i| outside(i));
            batch.iter().for_each(|&i| queued[i] = true);
            runs.extend(batch.chunks(RUN).map(<[usize]>::to_vec));
        }
        let points = |known: &[Option<Sampled>]| -> Points {
            let point =
                |(i, sampled): (usize, &Option<Sampled>)| Some((at(i), (*sampled)?.value()));
            known.iter().enumerate().filter_map(point).collect()
        };
        if !runs.is_empty() && known.iter().any(Option::is_some) {
            publish(points(&known));
        }
        let (known, shown) = (Mutex::new(known), Mutex::new(Instant::now()));
        let cores = std::thread::available_parallelism().map_or(1, |cores| cores.get());
        let jobs = cores.clamp(1, 4);
        let count = runs.len();
        for (step, run) in runs.into_iter().enumerate() {
            // Each worker takes the next point left: their cost varies with x.
            let next = AtomicUsize::new(0);
            std::thread::scope(|scope| {
                for _ in 0..jobs.min(run.len()) {
                    scope.spawn(|| {
                        while !superseded() {
                            let Some(&i) = run.get(next.fetch_add(1, Ordering::Relaxed)) else {
                                break;
                            };
                            let x = Qd::from_f64(at(i));
                            let sampled = match self.evaluate(x, superseded, CURVE_TOLERANCE) {
                                Err(Fault::Cancelled) => break,
                                Err(Fault::Inaccurate) => Sampled::Inaccurate,
                                Err(_) => Sampled::Undefined,
                                Ok((value, digits)) => Sampled::known(value, digits),
                            };
                            let mut curve = lock(&self.curve);
                            if curve.len() >= KEPT {
                                curve.clear();
                            }
                            curve.insert(at(i).to_bits(), sampled);
                            drop(curve);
                            let mut known = lock(&known);
                            known[i] = Some(sampled);
                            let mut shown = lock(&shown);
                            if shown.elapsed() >= SHOWN {
                                *shown = Instant::now();
                                publish(points(&known));
                            }
                        }
                    });
                }
            });
            if superseded() {
                return Err(Fault::Cancelled);
            }
            if step + 1 < count {
                *lock(&shown) = Instant::now();
                publish(points(&lock(&known)));
            }
        }
        let known = known.into_inner().unwrap_or_else(PoisonError::into_inner);
        let points = points(&known);
        if points.iter().all(|(_, y)| y.is_none()) && known.contains(&Some(Sampled::Inaccurate)) {
            return Err(Fault::Inaccurate);
        }
        Ok(points)
    }
}

/// The value behind a lock, even one a panicking worker left.
fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(PoisonError::into_inner)
}

pub fn sample_x(range: &RangeInclusive<f64>, samples: usize, i: usize) -> f64 {
    let (start, end) = (*range.start(), *range.end());
    match samples {
        0 | 1 => start,
        _ if i + 1 == samples => end,
        _ => start + (end - start) * i as f64 / (samples - 1) as f64,
    }
}

pub fn constant(text: &str, decimal_comma: bool) -> Result<Qd, Fault> {
    let node = latex::parse(text, decimal_comma).map_err(Fault::Unsupported)?;
    let mut compiler = Compiler {
        no_x: true,
        ..Compiler::default()
    };
    let root = plan::plan(compiler.compile(&node)?);
    Formula {
        root,
        slots: compiler.slots,
        curve: Mutex::default(),
    }
    .evaluate(Qd::ZERO, &|| false, TOLERANCE)
    .map(|(value, _)| value)
}

pub fn round(value: Qd, digits: usize) -> Qd {
    match value.decimal_digits(digits.clamp(1, 64)) {
        Some((negative, digits, exponent)) if digits.len() < 63 => {
            let digits: String = digits.iter().map(|&d| char::from(b'0' + d)).collect();
            let sign = if negative { "-" } else { "" };
            Qd::parse(&format!("{sign}0.{digits}e{exponent}")).unwrap_or(value)
        }
        _ => value,
    }
}

pub fn format(value: Qd, decimals: usize, decimal_comma: bool) -> String {
    let Some((negative, digits, exponent)) = value.decimal_digits(64) else {
        return value.to_f64().to_string();
    };
    let sign = if negative { "-" } else { "" };
    let mark = if decimal_comma { ',' } else { '.' };
    let all_zero = digits.iter().all(|&d| d == 0);
    if all_zero {
        return "0".to_string();
    }
    let digit = |d: &u8| char::from(b'0' + d);
    if (-(decimals as i32) < exponent) && exponent <= 30 {
        let places = (decimals as i32 + exponent).clamp(1, 64) as usize;
        let Some((negative, digits, exponent)) = value.decimal_digits(places) else {
            return value.to_f64().to_string();
        };
        let sign = if negative { "-" } else { "" };
        let int_len = exponent.max(0) as usize;
        let mut int: String = digits.iter().take(int_len).map(digit).collect();
        int.extend(std::iter::repeat_n(
            '0',
            int_len.saturating_sub(digits.len()),
        ));
        let zeros = (-exponent).max(0) as usize;
        let fraction: String = std::iter::repeat_n('0', zeros)
            .chain(digits.iter().skip(int_len).map(digit))
            .collect();
        let fraction = fraction.trim_end_matches('0');
        let int = if int.is_empty() { "0".to_string() } else { int };
        return match fraction {
            "" if int.chars().all(|c| c == '0') => "0".to_string(),
            "" => format!("{sign}{int}"),
            _ => format!("{sign}{int}{mark}{fraction}"),
        };
    }
    let Some((_, digits, _)) = value.decimal_digits(decimals.clamp(1, 63) + 1) else {
        return value.to_f64().to_string();
    };
    let mantissa: String = digits.iter().skip(1).map(digit).collect();
    let mantissa = mantissa.trim_end_matches('0');
    let lead = digit(&digits[0]);
    let point = if mantissa.is_empty() {
        String::new()
    } else {
        format!("{mark}{mantissa}")
    };
    format!("{sign}{lead}{point}e{}", exponent - 1)
}

fn has_calculus(node: &Node) -> bool {
    match node {
        Node::Call(name, arguments) => {
            matches!(name.as_str(), "sum" | "product" | "integrate")
                || arguments.iter().any(has_calculus)
        }
        Node::Negate(inner) | Node::Factorial(inner) | Node::Degrees(inner) => has_calculus(inner),
        Node::Binary(_, left, right) => has_calculus(left) || has_calculus(right),
        Node::Number(_) | Node::Variable(_) => false,
    }
}

fn mentions(node: &Node, name: &str) -> bool {
    match node {
        Node::Variable(variable) => variable == name,
        Node::Call(_, arguments) => arguments.iter().any(|argument| mentions(argument, name)),
        Node::Negate(inner) | Node::Factorial(inner) | Node::Degrees(inner) => {
            mentions(inner, name)
        }
        Node::Binary(_, left, right) => mentions(left, name) || mentions(right, name),
        Node::Number(_) => false,
    }
}

fn factors<'a>(node: &'a Node, exponent: i32, out: &mut Vec<(&'a Node, i32)>, negative: &mut bool) {
    match node {
        Node::Binary(Op::Multiply, left, right) => {
            factors(left, exponent, out, negative);
            factors(right, exponent, out, negative);
        }
        Node::Binary(Op::Divide, left, right) => {
            factors(left, exponent, out, negative);
            factors(right, -exponent, out, negative);
        }
        Node::Negate(inner) => {
            *negative = !*negative;
            factors(inner, exponent, out, negative);
        }
        _ => out.push((node, exponent)),
    }
}

fn whole(node: &Node) -> Option<i64> {
    match node {
        Node::Number(text) => {
            let value = text.parse::<f64>().ok()?;
            (value.fract() == 0.0 && value.abs() < 1e6).then_some(value as i64)
        }
        Node::Negate(inner) => whole(inner).map(|value| -value),
        _ => None,
    }
}

#[derive(Default)]
struct Compiler {
    scopes: Vec<(String, usize)>,
    slots: usize,
    no_x: bool,
}

impl Compiler {
    fn compile(&mut self, node: &Node) -> Result<Expr, Fault> {
        let boxed = |expr| Box::new(expr);
        Ok(match node {
            Node::Number(text) => {
                Expr::Const(Qd::parse(text).ok_or_else(|| Fault::Unsupported(text.clone()))?)
            }
            Node::Variable(name) => self.variable(name)?,
            Node::Negate(inner) => Expr::Neg(boxed(self.compile(inner)?)),
            // e^a as exp(a): exact for e itself, and without a logarithm at every point.
            Node::Binary(Op::Power, base, power)
                if matches!(&**base, Node::Variable(name) if name == "e")
                    && self.scopes.iter().all(|(name, _)| name != "e") =>
            {
                Expr::Unary(Unary::Exp, boxed(self.compile(power)?))
            }
            Node::Binary(op, left, right) => {
                Expr::Binary(*op, boxed(self.compile(left)?), boxed(self.compile(right)?))
            }
            Node::Factorial(inner) => Expr::Unary(Unary::Factorial, boxed(self.compile(inner)?)),
            Node::Degrees(inner) => Expr::Binary(
                Op::Multiply,
                boxed(self.compile(inner)?),
                boxed(Expr::Const(Qd::PI / Qd::from_i64(180))),
            ),
            Node::Call(name, arguments) => self.call(name, arguments)?,
        })
    }

    fn variable(&self, name: &str) -> Result<Expr, Fault> {
        if let Some((_, slot)) = self.scopes.iter().rev().find(|(bound, _)| bound == name) {
            return Ok(Expr::Slot(*slot));
        }
        Ok(match name {
            "x" if !self.no_x => Expr::X,
            "pi" => Expr::Const(Qd::PI),
            "tau" => Expr::Const(Qd::TAU),
            "e" => Expr::Const(Qd::E),
            "infinity" => Expr::Const(Qd::INFINITY),
            _ => return Err(Fault::Unsupported(name.to_string())),
        })
    }

    /// Σ K·sin(αn+β)/n^p, or with cos: no sign pattern to accelerate, but a closed form.
    fn fourier(&mut self, body: &Node, from: &Node, n: &str) -> Result<Option<Expr>, Fault> {
        let (mut parts, mut negative) = (Vec::new(), false);
        factors(body, 1, &mut parts, &mut negative);
        let (mut power, mut wave, mut constant) = (0i64, None, Vec::new());
        for (node, exponent) in parts {
            if !mentions(node, n) {
                constant.push((node, exponent));
                continue;
            }
            match node {
                Node::Variable(_) => power += i64::from(exponent),
                Node::Binary(Op::Power, base, index) if matches!(&**base, Node::Variable(base) if base == n) =>
                {
                    let Some(index) = whole(index) else {
                        return Ok(None);
                    };
                    power += i64::from(exponent) * index;
                }
                Node::Call(name, arguments)
                    if matches!(name.as_str(), "sin" | "cos")
                        && arguments.len() == 1
                        && exponent == 1
                        && wave.is_none() =>
                {
                    wave = Some((name == "sin", &arguments[0]));
                }
                _ => return Ok(None),
            }
        }
        let Some((sine, argument)) = wave else {
            return Ok(None);
        };
        // Only decaying powers of n: with none the terms do not even shrink.
        let power = match u32::try_from(-power) {
            Ok(power) if power >= 1 => power,
            _ => return Ok(None),
        };
        let Some((frequency, phase)) = self.linear(argument, n)? else {
            return Ok(None);
        };
        let sign = if negative { -Qd::ONE } else { Qd::ONE };
        let mut coefficient = Expr::Const(sign);
        for (node, exponent) in constant {
            let op = if exponent > 0 {
                Op::Multiply
            } else {
                Op::Divide
            };
            coefficient = Expr::Binary(op, Box::new(coefficient), Box::new(self.compile(node)?));
        }
        Ok(Some(Expr::Fourier {
            sine,
            power,
            frequency: Box::new(frequency),
            phase: Box::new(phase),
            coefficient: Box::new(coefficient),
            from: Box::new(self.compile(from)?),
        }))
    }

    fn linear(&mut self, node: &Node, n: &str) -> Result<Option<(Expr, Expr)>, Fault> {
        let number = |value: i64| Expr::Const(Qd::from_i64(value));
        let binary = |op, left, right| Expr::Binary(op, Box::new(left), Box::new(right));
        if !mentions(node, n) {
            return Ok(Some((number(0), self.compile(node)?)));
        }
        Ok(match node {
            Node::Variable(_) => Some((number(1), number(0))),
            Node::Negate(inner) => self
                .linear(inner, n)?
                .map(|(a, b)| (Expr::Neg(Box::new(a)), Expr::Neg(Box::new(b)))),
            Node::Binary(op @ (Op::Add | Op::Subtract), left, right) => {
                match (self.linear(left, n)?, self.linear(right, n)?) {
                    (Some((a, b)), Some((c, d))) => Some((binary(*op, a, c), binary(*op, b, d))),
                    _ => None,
                }
            }
            Node::Binary(Op::Multiply, left, right)
                if !mentions(left, n) || !mentions(right, n) =>
            {
                let (factor, other) = if mentions(left, n) {
                    (right, left)
                } else {
                    (left, right)
                };
                let factor = self.compile(factor)?;
                self.linear(other, n)?.map(|(a, b)| {
                    let scaled = binary(Op::Multiply, factor.clone(), a);
                    (scaled, binary(Op::Multiply, factor, b))
                })
            }
            Node::Binary(Op::Divide, left, right) if !mentions(right, n) => {
                let divisor = self.compile(right)?;
                self.linear(left, n)?.map(|(a, b)| {
                    let scaled = binary(Op::Divide, a, divisor.clone());
                    (scaled, binary(Op::Divide, b, divisor))
                })
            }
            _ => None,
        })
    }

    fn call(&mut self, name: &str, arguments: &[Node]) -> Result<Expr, Fault> {
        let unsupported = || Fault::Unsupported(name.to_string());
        if matches!(name, "sum" | "product" | "integrate") {
            // qalc's default variable is `x` when the call does not name one.
            let (body, from, to, variable) = match arguments {
                [body, from, to] => (body, from, to, "x"),
                [body, from, to, Node::Variable(variable)] => (body, from, to, variable.as_str()),
                _ => return Err(unsupported()),
            };
            if name == "sum"
                && matches!(to, Node::Variable(to) if to == "infinity")
                && let Some(fourier) = self.fourier(body, from, variable)?
            {
                return Ok(fourier);
            }
            let (from, to) = (self.compile(from)?, self.compile(to)?);
            let slot = self.slots;
            self.slots += 1;
            self.scopes.push((variable.to_string(), slot));
            let body = self.compile(body);
            self.scopes.pop();
            let (body, from, to) = (Box::new(body?), Box::new(from), Box::new(to));
            return Ok(match name {
                "integrate" => Expr::Integral {
                    body,
                    from,
                    to,
                    slot,
                },
                _ => Expr::Series {
                    product: name == "product",
                    body,
                    from,
                    to,
                    slot,
                },
            });
        }
        let mut arguments = arguments
            .iter()
            .map(|argument| self.compile(argument))
            .collect::<Result<Vec<_>, _>>()?;
        if matches!(name, "max" | "min") && !arguments.is_empty() {
            return Ok(Expr::Max(arguments, name == "max"));
        }
        let pair = |arguments: &mut Vec<Expr>| match (arguments.pop(), arguments.pop()) {
            (Some(second), Some(first)) if arguments.is_empty() => {
                Ok((Box::new(first), Box::new(second)))
            }
            _ => Err(unsupported()),
        };
        match name {
            "log" if arguments.len() == 2 => {
                let (value, base) = pair(&mut arguments)?;
                return Ok(Expr::Log(value, base));
            }
            "root" => {
                let (value, index) = pair(&mut arguments)?;
                return Ok(Expr::Root(value, index));
            }
            "binomial" => {
                let (total, chosen) = pair(&mut arguments)?;
                return Ok(Expr::Binomial(total, chosen));
            }
            _ => {}
        }
        let function = match name {
            "sin" => Unary::Sin,
            "cos" => Unary::Cos,
            "tan" => Unary::Tan,
            "cot" => Unary::Cot,
            "sec" => Unary::Sec,
            "csc" => Unary::Csc,
            "sinh" => Unary::Sinh,
            "cosh" => Unary::Cosh,
            "tanh" => Unary::Tanh,
            "coth" => Unary::Coth,
            "sech" => Unary::Sech,
            "csch" => Unary::Csch,
            "asin" => Unary::Asin,
            "acos" => Unary::Acos,
            "atan" => Unary::Atan,
            "acot" => Unary::Acot,
            "asinh" => Unary::Asinh,
            "acosh" => Unary::Acosh,
            "atanh" => Unary::Atanh,
            // qalc's log of one argument is the natural one.
            "ln" | "log" => Unary::Ln,
            "log10" => Unary::Log10,
            "exp" => Unary::Exp,
            "sqrt" => Unary::Sqrt,
            "abs" => Unary::Abs,
            "floor" => Unary::Floor,
            "ceil" => Unary::Ceil,
            "round" => Unary::Round,
            "trunc" => Unary::Trunc,
            _ => return Err(unsupported()),
        };
        match (arguments.pop(), arguments.is_empty()) {
            (Some(argument), true) => Ok(Expr::Unary(function, Box::new(argument))),
            _ => Err(unsupported()),
        }
    }
}

fn eval(expr: &Expr, x: Qd, slots: &mut [Qd], control: &Control) -> Result<Estimate, Fault> {
    let value = |expr: &Expr, slots: &mut [Qd]| eval(expr, x, slots, control);
    Ok(match expr {
        Expr::Const(value) => Estimate::exact(*value),
        Expr::X => Estimate::exact(x),
        Expr::Slot(slot) => Estimate::exact(slots[*slot]),
        Expr::Neg(inner) => value(inner, slots)?.scaled(-Qd::ONE),
        Expr::Binary(op, left, right) => {
            let (left, right) = (value(left, slots)?, value(right, slots)?);
            match op {
                Op::Add => left.plus(right),
                Op::Subtract => left.plus(right.scaled(-Qd::ONE)),
                Op::Multiply => left.times(right),
                Op::Divide => left.over(right),
                Op::Power => apply2(Qd::pow, left, right),
            }
        }
        Expr::Unary(Unary::Factorial, argument) => {
            let argument = value(argument, slots)?;
            if argument.error > 0.0 {
                return Err(Fault::Inaccurate);
            }
            Estimate::exact(unary(Unary::Factorial, argument.value)?)
        }
        Expr::Unary(function, argument) => {
            let argument = value(argument, slots)?;
            unary(*function, argument.value)?;
            apply(|a| unary(*function, a).unwrap_or(Qd::NAN), argument)
        }
        Expr::Log(argument, base) => apply2(
            |a, b| a.ln() / b.ln(),
            value(argument, slots)?,
            value(base, slots)?,
        ),
        Expr::Root(argument, index) => apply2(root, value(argument, slots)?, value(index, slots)?),
        Expr::Binomial(total, chosen) => {
            let (total, chosen) = (value(total, slots)?, value(chosen, slots)?);
            if total.error > 0.0 || chosen.error > 0.0 {
                return Err(Fault::Inaccurate);
            }
            Estimate::exact(binomial(total.value, chosen.value)?)
        }
        Expr::Max(arguments, max) => {
            let mut best: Option<Estimate> = None;
            for argument in arguments {
                let item = value(argument, slots)?;
                best = Some(match best {
                    Some(best) => Estimate {
                        value: if *max {
                            best.value.max(item.value)
                        } else {
                            best.value.min(item.value)
                        },
                        error: best.error.max(item.error),
                    },
                    None => item,
                });
            }
            best.unwrap_or(Estimate::exact(Qd::NAN))
        }
        Expr::Series {
            product,
            body,
            from,
            to,
            slot,
        } => {
            let (from, to) = (value(from, slots)?, value(to, slots)?);
            // Bounds known only roughly leave the count of terms itself in doubt.
            if from.error > 0.0 || to.error > 0.0 {
                return Err(Fault::Inaccurate);
            }
            let mut term = |n: Qd| {
                control.tick()?;
                slots[*slot] = n;
                eval(body, x, slots, control)
            };
            series::sum(&mut term, from.value, to.value, *product, control.tolerance)?
        }
        Expr::Integral {
            body,
            from,
            to,
            slot,
        } => {
            let (from, to) = (value(from, slots)?, value(to, slots)?);
            let mut integrand = |t: Qd| {
                control.tick()?;
                slots[*slot] = t;
                eval(body, x, slots, control)
            };
            let mut integral =
                quad::integrate(&mut integrand, from.value, to.value, control.tolerance)?;
            // A bound off by δ moves the integral by about |f(bound)|·δ.
            for bound in [from, to] {
                if bound.error > 0.0 {
                    let edge = integrand(bound.value)?;
                    integral.error += edge.value.abs().to_f64() * bound.error + edge.error;
                }
            }
            integral
        }
        Expr::Fourier {
            sine,
            power,
            frequency,
            phase,
            coefficient,
            from,
        } => {
            let from = value(from, slots)?;
            if from.error > 0.0 {
                return Err(Fault::Inaccurate);
            }
            // n = 0 is a pole of 1/n^p, and a sum starts at a whole n.
            let first = from.value;
            if !first.is_finite() || first.floor() != first || first < Qd::ONE {
                return Err(Fault::Undefined);
            }
            // Past 10^63 a `Qd` no longer counts by ones.
            if first + Qd::ONE == first {
                return Err(Fault::Inaccurate);
            }
            let alpha = value(frequency, slots)?;
            let (beta, k) = (value(phase, slots)?, value(coefficient, slots)?);
            let sums = |a: Qd| -> Result<(Estimate, Estimate), Fault> {
                let (mut cosines, mut sines) = fourier::sums(*power, a, control.tolerance);
                let mut n = Qd::ONE;
                while n < first {
                    control.tick()?;
                    let (sin, cos) = (n * a).sin_cos();
                    let scale = n.powi(-i64::from(*power));
                    cosines.value -= cos * scale;
                    sines.value -= sin * scale;
                    cosines.error += ROUNDING;
                    sines.error += ROUNDING;
                    n += Qd::ONE;
                }
                Ok((cosines, sines))
            };
            let (mut cosines, mut sines) = sums(alpha.value)?;
            if alpha.error > 0.0 {
                let shift = Qd::from_f64(alpha.error);
                for end in [alpha.value - shift, alpha.value + shift] {
                    let (c, s) = sums(end)?;
                    let moved = |a: Qd, b: Qd| match (a - b).abs().to_f64() {
                        change if change.is_nan() => f64::INFINITY,
                        change => change,
                    };
                    cosines.error += moved(c.value, cosines.value);
                    sines.error += moved(s.value, sines.value);
                }
            }
            // sin(αn+β) = sin(αn)cos β + cos(αn)sin β, and cos(αn+β) = cos(αn)cos β − sin(αn)sin β.
            let (cos_b, sin_b) = (apply(|b| b.cos(), beta), apply(|b| b.sin(), beta));
            let (main, other) = if *sine {
                (sines, cosines)
            } else {
                (cosines, sines)
            };
            let mut total = main.times(cos_b);
            // Skipped when β = 0, so that Σ sin(αn)/n at α = 0 is 0 and not 0·∞.
            if !(sin_b.value.is_zero() && sin_b.error == 0.0) {
                let part = other.times(sin_b);
                total = total.plus(if *sine { part } else { part.scaled(-Qd::ONE) });
            }
            total.times(k)
        }
        Expr::Memo(memo) => {
            let key = memo.key(slots, control.tolerance);
            if let Some(known) = memo.get(&key) {
                return Ok(known);
            }
            let known = eval(&memo.inner, x, slots, control)?;
            memo.keep(key, known);
            known
        }
        Expr::Either(first, second) => match value(first, slots) {
            Err(Fault::Inaccurate | Fault::Undefined) => value(second, slots)?,
            other => other?,
        },
    })
}

fn apply(f: impl Fn(Qd) -> Qd, a: Estimate) -> Estimate {
    let value = f(a.value);
    Estimate {
        value,
        error: spread(&f, a, value),
    }
}

fn apply2(f: impl Fn(Qd, Qd) -> Qd, a: Estimate, b: Estimate) -> Estimate {
    let value = f(a.value, b.value);
    let error = spread(&|a| f(a, b.value), a, value) + spread(&|b| f(a.value, b), b, value);
    Estimate { value, error }
}

fn spread(f: &dyn Fn(Qd) -> Qd, a: Estimate, value: Qd) -> f64 {
    if a.error == 0.0 {
        return 0.0;
    }
    let shift = Qd::from_f64(a.error);
    [a.value - shift, a.value + shift]
        .into_iter()
        .map(|end| (f(end) - value).abs().to_f64())
        .map(|change| {
            if change.is_nan() {
                f64::INFINITY
            } else {
                change
            }
        })
        .fold(0.0, f64::max)
}

/// An n-th root, real for odd n and negative values, as qalc's root() gives it.
fn root(argument: Qd, index: Qd) -> Qd {
    let half = index / Qd::from_i64(2);
    let odd = index.floor() == index && half.floor() != half;
    if argument.is_sign_negative() && odd {
        -(-argument).pow(index.recip())
    } else {
        argument.pow(index.recip())
    }
}

fn unary(function: Unary, a: Qd) -> Result<Qd, Fault> {
    Ok(match function {
        Unary::Sin => a.sin(),
        Unary::Cos => a.cos(),
        Unary::Tan => a.tan(),
        Unary::Cot => a.tan().recip(),
        Unary::Sec => a.cos().recip(),
        Unary::Csc => a.sin().recip(),
        Unary::Sinh => a.sinh(),
        Unary::Cosh => a.cosh(),
        Unary::Tanh => a.tanh(),
        Unary::Coth => a.tanh().recip(),
        Unary::Sech => a.cosh().recip(),
        Unary::Csch => a.sinh().recip(),
        Unary::Asin => a.asin(),
        Unary::Acos => a.acos(),
        Unary::Atan => a.atan(),
        Unary::Acot => a.recip().atan(),
        Unary::Asinh => a.asinh(),
        Unary::Acosh => a.acosh(),
        Unary::Atanh => a.atanh(),
        Unary::Ln => a.ln(),
        Unary::Log10 => a.ln() / Qd::LN_10,
        Unary::Exp => a.exp(),
        Unary::Sqrt => a.sqrt(),
        Unary::Abs => a.abs(),
        Unary::Floor => a.floor(),
        Unary::Ceil => a.ceil(),
        Unary::Round => a.round(),
        Unary::Trunc => a.trunc(),
        Unary::Factorial => {
            // Whole numbers only: qalc's gamma function is not part of the engine.
            if a.floor() != a || a.is_sign_negative() || a > Qd::from_i64(10_000) {
                return Err(Fault::Unsupported("!".to_string()));
            }
            let n = a.to_f64() as i64;
            (2..=n).fold(Qd::ONE, |product, k| product * Qd::from_i64(k))
        }
    })
}

fn binomial(total: Qd, chosen: Qd) -> Result<Qd, Fault> {
    let whole = |value: Qd| value.floor() == value && value.abs() < Qd::from_i64(1 << 40);
    if !whole(total) || !whole(chosen) {
        return Err(Fault::Unsupported("binomial".to_string()));
    }
    let (n, k) = (total.to_f64() as i64, chosen.to_f64() as i64);
    if k < 0 || (n >= 0 && k > n) {
        return Ok(Qd::ZERO);
    }
    Ok((0..k).fold(Qd::ONE, |product, i| {
        product * Qd::from_i64(n - i) / Qd::from_i64(i + 1)
    }))
}

#[cfg(test)]
mod tests {
    use super::{Fault, Formula, Qd, constant, format};

    fn at(text: &str, x: f64) -> Result<Qd, Fault> {
        let formula = Formula::calculus(text, false).unwrap();
        let (value, digits) = formula.evaluate(Qd::from_f64(x), &|| false, super::TOLERANCE)?;
        assert_eq!(digits, 50, "{text}");
        Ok(value)
    }

    fn close(value: Qd, expected: Qd) -> bool {
        (value - expected).abs() <= expected.abs() * Qd::from_f64(1e-50)
    }

    #[test]
    fn only_takes_the_formulas_qalc_cannot_do() {
        assert!(Formula::calculus("sin(x)*x", false).is_none());
        assert!(Formula::calculus("integrate(gamma(y), 0, 1, y)", false).is_none());
        assert!(Formula::calculus("sum(y*a, 1, 3, y)", false).is_none());
        assert!(Formula::calculus("sum(1/y^2, 1, infinity, y)", false).is_some());
    }

    #[test]
    fn evaluates_to_fifty_digits() {
        let pi = Qd::PI;
        assert!(close(
            at("integrate(e^(-y), 0, infinity, y)", 0.0).unwrap(),
            Qd::ONE
        ));
        assert!(close(
            at("sum(1/y^2, 1, infinity, y)", 0.0).unwrap(),
            pi * pi / Qd::from_i64(6)
        ));
        let laplace = at("integrate(e^(-x*y), 0, infinity, y)", 2.0).unwrap();
        assert!(close(laplace, Qd::from_f64(0.5)));
        let taylor = at("sum(x^y/y!, 0, infinity, y)", 3.0).unwrap();
        assert!(close(taylor, Qd::from_i64(3).exp()));
        // Nested: ∫_0^x ∫_0^y 1 dz dy = x²/2.
        let nested = at("integrate(integrate(1, 0, y, z), 0, x, y)", 3.0).unwrap();
        assert!(close(nested, Qd::from_f64(4.5)));
        // Sums of integrals: Σ 1/(2^n (n+1)) = 2 ln 2, and a finite one, H₁₁ − 1.
        let halves = at("sum(integrate((z/2)^y, 0, 1, z), 0, infinity, y)", 0.0).unwrap();
        assert!(close(halves, Qd::LN_2 + Qd::LN_2));
        let harmonic = at("sum(integrate(z^y, 0, 1, z), 1, 10, y)", 0.0).unwrap();
        let expected = (2..=11).fold(Qd::ZERO, |sum, k| sum + Qd::from_i64(k).recip());
        assert!(close(harmonic, expected));
        assert_eq!(at("sum(1/y, 1, infinity, y)", 0.0), Err(Fault::Inaccurate));
        assert!(at("integrate(1/y, 0, 1, y)+x", 0.0).is_err());
    }

    #[test]
    fn samples_a_curve_on_several_threads() {
        let laplace = Formula::calculus("integrate(e^(-x*y), 0, infinity, y)", false).unwrap();
        let sample = |range: std::ops::RangeInclusive<f64>, superseded: bool| {
            laplace.sample(&range, 3, &range, &|| superseded, &|_| {})
        };
        let points = sample(1.0..=2.0, false).unwrap();
        let expected = [(1.0, Some(1.0)), (1.5, Some(2.0 / 3.0)), (2.0, Some(0.5))];
        assert_eq!(points, expected);
        let points = sample(-1.0..=1.0, false).unwrap();
        assert_eq!(points, [(-1.0, None), (0.0, None), (1.0, Some(1.0))]);
        assert_eq!(sample(1.0..=2.0, true), Err(Fault::Cancelled));
    }

    #[test]
    fn cancellation_is_not_mistaken_for_digits() {
        // √π·e^-225 ≈ 3.4e-98 drowns in the 1e-62 noise of terms near 1: no digit of it is known.
        let gaussian = "integrate(e^(-y^2)*cos(x*y), -infinity, infinity, y)";
        let formula = Formula::calculus(gaussian, false).unwrap();
        let far = formula.evaluate(Qd::from_i64(30), &|| false, super::CURVE_TOLERANCE);
        assert_eq!(far, Err(Fault::Inaccurate));
        let near = formula.evaluate(Qd::from_i64(3), &|| false, super::CURVE_TOLERANCE);
        let expected = Qd::PI.sqrt() * Qd::from_f64(-2.25).exp();
        let (near, digits) = near.unwrap();
        assert!((near - expected).abs() < expected * Qd::from_f64(1e-16) && digits == 16);
        assert_eq!(at("integrate(y, -x, x, y)", 2.0), Ok(Qd::ZERO));
    }

    #[test]
    fn keeps_what_has_six_digits_and_stops_below() {
        // ∫ t^-0.95: its mass sits closer to 0 than f64 nodes reach; short of 16 digits, not of 6.
        let singular = Formula::calculus("integrate(y^(-0.95), 0, x, y)", false).unwrap();
        let (value, digits) = singular
            .evaluate(Qd::ONE, &|| false, super::CURVE_TOLERANCE)
            .unwrap();
        assert!((6..16).contains(&digits), "{digits}");
        assert!((value - Qd::from_i64(20)).abs() < Qd::from_f64(1e-6));
        let one = 1.0..=1.0;
        assert_eq!(
            singular.sample(&one, 1, &one, &|| false, &|_| {}),
            Ok(vec![(1.0, Some(20.0))])
        );
        assert_eq!(super::round(Qd::PI, 6), Qd::parse("3.14159").unwrap());
        let hopeless = Formula::calculus("integrate(y^(-0.999), 0, x, y)", false).unwrap();
        let started = std::time::Instant::now();
        let value = hopeless.evaluate(Qd::ONE, &|| false, super::CURVE_TOLERANCE);
        assert_eq!(value, Err(Fault::Inaccurate));
        assert!(started.elapsed() < std::time::Duration::from_secs(1));
        assert_eq!(
            hopeless.sample(&one, 1, &one, &|| false, &|_| {}),
            Err(Fault::Inaccurate)
        );
    }

    #[test]
    fn reads_the_field_in_decimal_comma_locales() {
        let formula = Formula::calculus("integrate(e^(-y); 0; 1,5; y)", true).unwrap();
        let (value, _) = formula
            .evaluate(Qd::ZERO, &|| false, super::TOLERANCE)
            .unwrap();
        assert!(close(value, Qd::ONE - Qd::from_f64(-1.5).exp()));
        assert_eq!(constant("2,5*2", true), Ok(Qd::from_i64(5)));
    }

    #[test]
    fn computes_what_qalc_reads() {
        // A thousandth, the natural log and 2^9, as qalc reads them: not 1·e−3, log10 or 8².
        let thousandth = at("sum(1e-3*x,1,1,y)", 10.0).unwrap();
        assert!(close(thousandth, Qd::parse("0.01").unwrap()));
        assert!(close(at("sum(log(x),1,1,y)", 10.0).unwrap(), Qd::LN_10));
        let power = at("sum(2^3^2*x,1,1,y)", 10.0).unwrap();
        assert!(close(power, Qd::from_i64(5120)));
        let tight = at("sum(1/2x,1,1,y)", 4.0).unwrap();
        assert!(close(tight, Qd::parse("0.125").unwrap()));
        assert!(close(at("sum(1/2 x,1,1,y)", 4.0).unwrap(), Qd::from_i64(2)));
    }

    #[test]
    fn formats_like_the_rest_of_the_calculator() {
        let third = Qd::ONE / Qd::from_i64(3);
        assert_eq!(format(third, 18, false), "0.333333333333333333");
        assert_eq!(format(Qd::from_i64(2), 18, false), "2");
        assert_eq!(format(-Qd::from_f64(0.5), 18, false), "-0.5");
        assert_eq!(format(Qd::PI, 18, true), "3,141592653589793238");
        assert_eq!(format(Qd::parse("1e-25").unwrap(), 18, false), "1e-25");
        assert_eq!(format(Qd::ZERO, 18, false), "0");
    }

    #[test]
    fn refuses_instead_of_guessing() {
        assert_eq!(
            at("sum(sin(y)/(y^2+1), 1, infinity, y)", 0.0),
            Err(Fault::Inaccurate)
        );
        assert_eq!(
            at("integrate(sin(y)/y, 0, infinity, y)", 0.0),
            Err(Fault::Inaccurate)
        );
        assert_eq!(at("sum(1, 1, 10^70, y)", 0.0), Err(Fault::Inaccurate));
    }

    #[test]
    fn sums_fourier_series_in_closed_form() {
        // Clausen's Cl₂(1), and a cosine series with a phase from n = 2 (mpmath, 55 digits).
        let clausen = at("sum(sin(y*x)/y^2, 1, infinity, y)", 1.0).unwrap();
        let expected = "1.013959132360768504294574338885914687561179280077717";
        assert!(close(clausen, Qd::parse(expected).unwrap()));
        let shifted = at("sum(cos(2*y*x+1)/y^3, 2, infinity, y)", 0.25).unwrap();
        let expected = "-0.1051257718160140210590807877944183812109006235970080276";
        assert!(close(shifted, Qd::parse(expected).unwrap()));
        assert_eq!(at("sum(sin(y*x)/y, 1, infinity, y)", 0.0), Ok(Qd::ZERO));
        assert_eq!(
            at("sum(cos(y*x)/y, 1, infinity, y)", 0.0),
            Err(Fault::Undefined)
        );
        let sawtooth = Formula::calculus("sum(sin(y*x)/y, 1, infinity, y)", false).unwrap();
        let turn = constant("2π", false).unwrap();
        let value = sawtooth.evaluate(turn, &|| false, super::TOLERANCE);
        assert_eq!(value, Ok((Qd::ZERO, 50)));
    }

    #[test]
    fn integrates_past_kinks_and_oscillations() {
        let kink = at("integrate(abs(y-1), 0, 3, y)", 0.0).unwrap();
        assert!(close(kink, Qd::from_f64(2.5)));
        let damped = at("integrate(e^(-y)*cos(x*y), 0, infinity, y)", 30.0).unwrap();
        assert!(close(damped, Qd::ONE / Qd::from_i64(901)));
        let text = "integrate(e^(-y)*cos(x*y^2)/(1+y^2)*sum(sin(z*y)/z^2, 1, infinity, z)^2, 0, infinity, y)";
        let formula = Formula::calculus(text, false).unwrap();
        let two = Qd::from_i64(2);
        let (value, digits) = formula
            .evaluate(two, &|| false, super::CURVE_TOLERANCE)
            .unwrap();
        // mpmath, 40 digits.
        let expected = Qd::parse("0.13127753348609027711999108185696629715").unwrap();
        assert!(digits == 16 && (value - expected).abs() < expected * Qd::from_f64(1e-16));
    }

    #[test]
    fn sums_integrals_as_the_integral_of_the_sum() {
        // Σ ∫₀¹ tⁿ/n² dt = ∫₀¹ Li₂(t) dt = ζ(2) − 1, the sum computed once at each node.
        let swapped = at("sum(integrate(z^y, 0, 1, z)/y^2, 1, infinity, y)", 0.0).unwrap();
        let expected = "0.6449340668482264364724151666460251892189499012067984377";
        assert!(close(swapped, Qd::parse(expected).unwrap()));
    }

    #[test]
    fn samples_coarse_everywhere_then_the_view_and_each_point_once() {
        let laplace = Formula::calculus("integrate(e^(-x*y), 0, infinity, y)", false).unwrap();
        let steps = std::sync::Mutex::new(Vec::new());
        let publish = |points| steps.lock().unwrap().push(points);
        let points = laplace.sample(&(1.0..=4.0), 97, &(2.0..=3.0), &|| false, &publish);
        let points = points.unwrap();
        assert!(points.len() == 97 && points.iter().all(|(_, y)| y.is_some()));
        let first = std::mem::take(&mut *steps.lock().unwrap());
        assert_eq!(first[0], [(2.0, Some(0.5)), (3.0, Some(1.0 / 3.0))]);
        let xs: Vec<f64> = first[1].iter().map(|(x, _)| *x).collect();
        assert_eq!(xs, [1.0, 2.0, 3.0, 4.0]);
        let in_view = first[6].iter().filter(|(x, _)| (2.0..=3.0).contains(x));
        assert_eq!((in_view.count(), first[6].len()), (33, 35));
        let points = laplace.sample(&(2.0..=5.0), 97, &(3.0..=4.0), &|| false, &publish);
        assert_eq!(points.map(|points| points.len()), Ok(97));
        let steps = steps.into_inner().unwrap();
        assert_eq!((steps[0].len(), steps[1].len()), (65, 66));
    }

    /// The series of integrals of the stress-test formula, against mpmath; a minute of work, so on demand.
    #[test]
    #[ignore]
    fn matches_mpmath_on_a_series_of_integrals() {
        let text = "sum(1/y^2*integrate(ln(1+x^2*z^2)/(1+z^y), 0, 1, z), 1, infinity, y)";
        let formula = Formula::calculus(text, false).unwrap();
        let (value, _) = formula
            .evaluate(Qd::from_f64(0.5), &|| false, super::TOLERANCE)
            .unwrap();
        let expected = Qd::parse("0.082041170794796133580438065814148166368777456013651").unwrap();
        assert!(
            (value - expected).abs() < Qd::from_f64(1e-50),
            "{}",
            format(value, 50, false)
        );
    }
}
