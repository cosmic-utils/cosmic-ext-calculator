//! Rewrites a formula so a curve costs less: factoring out, Tonelli's swap, memos of what lacks x.

use super::{Estimate, Expr, Qd, Unary};
use crate::app::latex::Op;
use std::collections::{BTreeSet, HashMap};
use std::sync::{Arc, Mutex};

/// Values one memo keeps at most: the nodes of a few dozen integrals, some ten megabytes.
const KEPT: usize = 1 << 17;

#[derive(Debug)]
pub(super) struct Memo {
    pub(super) inner: Expr,
    keys: Vec<usize>,
    values: Mutex<HashMap<Vec<u64>, Estimate>>,
}

impl Memo {
    /// What the value depends on: its variables, and the tolerance it was computed to.
    pub(super) fn key(&self, slots: &[Qd], tolerance: f64) -> Vec<u64> {
        let variables = self
            .keys
            .iter()
            .flat_map(|&slot| slots[slot].0.map(f64::to_bits));
        variables.chain([tolerance.to_bits()]).collect()
    }

    pub(super) fn get(&self, key: &[u64]) -> Option<Estimate> {
        let values = self
            .values
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        values.get(key).copied()
    }

    pub(super) fn keep(&self, key: Vec<u64>, value: Estimate) {
        let mut values = self
            .values
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if values.len() < KEPT {
            values.insert(key, value);
        }
    }
}

pub(super) fn plan(expr: Expr) -> Expr {
    memoize(optimize(expr))
}

fn children(expr: &Expr) -> Vec<&Expr> {
    match expr {
        Expr::Const(_) | Expr::X | Expr::Slot(_) => Vec::new(),
        Expr::Neg(a) | Expr::Unary(_, a) => vec![a],
        Expr::Binary(_, a, b)
        | Expr::Log(a, b)
        | Expr::Root(a, b)
        | Expr::Binomial(a, b)
        | Expr::Either(a, b) => vec![a, b],
        Expr::Max(list, _) => list.iter().collect(),
        Expr::Series { body, from, to, .. } | Expr::Integral { body, from, to, .. } => {
            vec![body, from, to]
        }
        Expr::Fourier {
            frequency,
            phase,
            coefficient,
            from,
            ..
        } => vec![frequency, phase, coefficient, from],
        Expr::Memo(memo) => vec![&memo.inner],
    }
}

fn rebuild(expr: Expr, f: &mut impl FnMut(Expr) -> Expr) -> Expr {
    let mut boxed = |part: Box<Expr>| Box::new(f(*part));
    match expr {
        Expr::Neg(a) => Expr::Neg(boxed(a)),
        Expr::Unary(function, a) => Expr::Unary(function, boxed(a)),
        Expr::Binary(op, a, b) => Expr::Binary(op, boxed(a), boxed(b)),
        Expr::Log(a, b) => Expr::Log(boxed(a), boxed(b)),
        Expr::Root(a, b) => Expr::Root(boxed(a), boxed(b)),
        Expr::Binomial(a, b) => Expr::Binomial(boxed(a), boxed(b)),
        Expr::Either(a, b) => Expr::Either(boxed(a), boxed(b)),
        Expr::Max(list, max) => Expr::Max(list.into_iter().map(&mut *f).collect(), max),
        Expr::Series {
            product,
            body,
            from,
            to,
            slot,
        } => Expr::Series {
            product,
            body: boxed(body),
            from: boxed(from),
            to: boxed(to),
            slot,
        },
        Expr::Integral {
            body,
            from,
            to,
            slot,
        } => Expr::Integral {
            body: boxed(body),
            from: boxed(from),
            to: boxed(to),
            slot,
        },
        Expr::Fourier {
            sine,
            power,
            frequency,
            phase,
            coefficient,
            from,
        } => Expr::Fourier {
            sine,
            power,
            frequency: boxed(frequency),
            phase: boxed(phase),
            coefficient: boxed(coefficient),
            from: boxed(from),
        },
        leaf => leaf,
    }
}

fn uses_x(expr: &Expr) -> bool {
    matches!(expr, Expr::X) || children(expr).into_iter().any(uses_x)
}

fn uses(expr: &Expr, slot: usize) -> bool {
    matches!(expr, Expr::Slot(read) if *read == slot)
        || children(expr).into_iter().any(|part| uses(part, slot))
}

fn free(expr: &Expr, out: &mut BTreeSet<usize>) {
    match expr {
        Expr::Slot(slot) => {
            out.insert(*slot);
        }
        Expr::Series {
            body,
            from,
            to,
            slot,
            ..
        }
        | Expr::Integral {
            body,
            from,
            to,
            slot,
        } => {
            free(from, out);
            free(to, out);
            let mut inner = BTreeSet::new();
            free(body, &mut inner);
            inner.remove(slot);
            out.extend(inner);
        }
        _ => children(expr).into_iter().for_each(|part| free(part, out)),
    }
}

fn factors(expr: Expr, exponent: i32, out: &mut Vec<(Expr, i32)>, negative: &mut bool) {
    match expr {
        Expr::Binary(Op::Multiply, a, b) => {
            factors(*a, exponent, out, negative);
            factors(*b, exponent, out, negative);
        }
        Expr::Binary(Op::Divide, a, b) => {
            factors(*a, exponent, out, negative);
            factors(*b, -exponent, out, negative);
        }
        Expr::Neg(a) => {
            *negative = !*negative;
            factors(*a, exponent, out, negative);
        }
        other => out.push((other, exponent)),
    }
}

fn product(negative: bool, factors: Vec<(Expr, i32)>) -> Expr {
    let (above, below): (Vec<_>, Vec<_>) = factors.into_iter().partition(|(_, e)| *e > 0);
    let binary = |op, a, b| Expr::Binary(op, Box::new(a), Box::new(b));
    let mut result = above
        .into_iter()
        .map(|(factor, _)| factor)
        .reduce(|a, b| binary(Op::Multiply, a, b))
        .unwrap_or(Expr::Const(Qd::ONE));
    for (factor, _) in below {
        result = binary(Op::Divide, result, factor);
    }
    if negative {
        Expr::Neg(Box::new(result))
    } else {
        result
    }
}

fn optimize(expr: Expr) -> Expr {
    match rebuild(expr, &mut optimize) {
        Expr::Series {
            product: false,
            body,
            from,
            to,
            slot,
        } => sum(*body, *from, *to, slot),
        Expr::Integral {
            body,
            from,
            to,
            slot,
        } => {
            let (mut parts, mut negative) = (Vec::new(), false);
            factors(*body, 1, &mut parts, &mut negative);
            let (inside, outside): (Vec<_>, Vec<_>) = parts
                .into_iter()
                .partition(|(factor, _)| uses(factor, slot));
            let integral = Expr::Integral {
                body: Box::new(product(false, inside)),
                from,
                to,
                slot,
            };
            pulled(negative, outside, integral)
        }
        other => other,
    }
}

fn pulled(negative: bool, mut outside: Vec<(Expr, i32)>, inner: Expr) -> Expr {
    if outside.is_empty() && !negative {
        return inner;
    }
    outside.push((inner, 1));
    product(negative, outside)
}

fn sum(body: Expr, from: Expr, to: Expr, slot: usize) -> Expr {
    let (mut parts, mut negative) = (Vec::new(), false);
    factors(body, 1, &mut parts, &mut negative);
    let (inside, outside): (Vec<_>, Vec<_>) = parts
        .into_iter()
        .partition(|(factor, _)| uses(factor, slot));
    let series = |inside: Vec<(Expr, i32)>, from: Expr, to: Expr| Expr::Series {
        product: false,
        body: Box::new(product(false, inside)),
        from: Box::new(from),
        to: Box::new(to),
        slot,
    };
    let infinite = matches!(&to, Expr::Const(to) if *to == Qd::INFINITY);
    let integrals: Vec<usize> = (0..inside.len())
        .filter(|&i| matches!(inside[i], (Expr::Integral { .. }, 1)))
        .collect();
    let swapped = match integrals[..] {
        [i] if infinite => swap(&inside, i, &from, &to, slot),
        _ => None,
    };
    let original = series(inside, from, to);
    let planned = match swapped {
        Some(swapped) => Expr::Either(Box::new(swapped), Box::new(original)),
        None => original,
    };
    pulled(negative, outside, planned)
}

/// Σ c_n ∫ g dt as ∫ Σ c_n g dt, which Tonelli allows when every c_n·g(n, t) has one sign.
fn swap(inside: &[(Expr, i32)], at: usize, from: &Expr, to: &Expr, slot: usize) -> Option<Expr> {
    let Expr::Integral {
        body,
        from: start,
        to: end,
        slot: variable,
    } = &inside[at].0
    else {
        return None;
    };
    if uses(start, slot) || uses(end, slot) {
        return None;
    }
    let mut terms: Vec<(Expr, i32)> = inside
        .iter()
        .enumerate()
        .filter(|&(i, _)| i != at)
        .map(|(_, part)| part.clone())
        .collect();
    terms.push(((**body).clone(), 1));
    let integrand = product(false, terms);
    let mut ranges = HashMap::new();
    let first = range(from, &ranges);
    ranges.insert(slot, Range::new(first.lo.ceil(), f64::INFINITY));
    let (a, b) = (range(start, &ranges), range(end, &ranges));
    ranges.insert(*variable, Range::new(a.lo.min(b.lo), a.hi.max(b.hi)));
    let sign = range(&integrand, &ranges);
    if !(sign.lo >= 0.0 || sign.hi <= 0.0) {
        return None;
    }
    let series = Expr::Series {
        product: false,
        body: Box::new(integrand),
        from: Box::new(from.clone()),
        to: Box::new(to.clone()),
        slot,
    };
    Some(optimize(Expr::Integral {
        body: Box::new(series),
        from: start.clone(),
        to: end.clone(),
        slot: *variable,
    }))
}

fn memoize(expr: Expr) -> Expr {
    let expr = rebuild(expr, &mut memoize);
    let costly = matches!(
        expr,
        Expr::Series { .. } | Expr::Integral { .. } | Expr::Fourier { .. }
    );
    if !costly || uses_x(&expr) {
        return expr;
    }
    let mut keys = BTreeSet::new();
    free(&expr, &mut keys);
    Expr::Memo(Arc::new(Memo {
        inner: expr,
        keys: keys.into_iter().collect(),
        values: Mutex::default(),
    }))
}

/// Bounds on a value, rounded outward, so that a sign they prove is the value's sign.
#[derive(Clone, Copy, Debug, PartialEq)]
struct Range {
    lo: f64,
    hi: f64,
}

const ALL: Range = Range {
    lo: f64::NEG_INFINITY,
    hi: f64::INFINITY,
};

impl Range {
    fn new(lo: f64, hi: f64) -> Range {
        if lo.is_nan() || hi.is_nan() || lo > hi {
            ALL
        } else {
            Range { lo, hi }
        }
    }

    fn of(value: Qd) -> Range {
        let near = value.to_f64();
        if Qd::from_f64(near) == value {
            Range::new(near, near)
        } else {
            Range::new(near.next_down(), near.next_up())
        }
    }

    fn contains_zero(self) -> bool {
        self.lo <= 0.0 && 0.0 <= self.hi
    }

    fn add(self, other: Range) -> Range {
        Range::new(plus(self.lo, other.lo).0, plus(self.hi, other.hi).1)
    }

    fn neg(self) -> Range {
        Range::new(-self.hi, -self.lo)
    }

    fn mul(self, other: Range) -> Range {
        let corners = [
            times(self.lo, other.lo),
            times(self.lo, other.hi),
            times(self.hi, other.lo),
            times(self.hi, other.hi),
        ];
        Range::new(
            corners.iter().map(|c| c.0).fold(f64::INFINITY, f64::min),
            corners
                .iter()
                .map(|c| c.1)
                .fold(f64::NEG_INFINITY, f64::max),
        )
    }

    fn recip(self) -> Range {
        if self.contains_zero() {
            return ALL;
        }
        let (a, b) = (inverse(self.hi), inverse(self.lo));
        Range::new(a.0, b.1)
    }

    fn increasing(self, f: impl Fn(f64) -> f64, exact: impl Fn(f64) -> bool) -> Range {
        let bound = |x: f64, low: bool| {
            let value = f(x);
            match () {
                _ if exact(x) || value.is_infinite() => value,
                _ if low => value.next_down().next_down(),
                _ => value.next_up().next_up(),
            }
        };
        Range::new(bound(self.lo, true), bound(self.hi, false))
    }

    /// x^k for a whole k, by repeated products so that exact ones stay exact.
    fn powi(self, k: i64) -> Range {
        if k == 0 {
            return Range::new(1.0, 1.0);
        }
        if k < 0 {
            return self.powi(-k).recip();
        }
        // An even power of a range around 0 starts at 0.
        let base = if k % 2 == 0 {
            let (a, b) = (self.lo.abs(), self.hi.abs());
            match self.contains_zero() {
                true => Range::new(0.0, a.max(b)),
                false => Range::new(a.min(b), a.max(b)),
            }
        } else {
            self
        };
        let mut result = base;
        for _ in 1..k.min(64) {
            result = result.mul(base);
        }
        if k > 64 {
            return match (result.lo >= 0.0, result.hi <= 0.0) {
                (true, _) => Range::new(0.0, f64::INFINITY),
                (_, true) => Range::new(f64::NEG_INFINITY, 0.0),
                _ => ALL,
            };
        }
        result
    }

    /// base^power for a base that is not negative: monotone in each, so bounded by the corners.
    fn pow(self, power: Range) -> Range {
        if self.lo < 0.0 {
            return ALL;
        }
        let corner = |b: f64, e: f64, low: bool| {
            let value = b.powf(e);
            let exact = e == 0.0 || b == 1.0 || b == 0.0 || value.is_infinite();
            match () {
                _ if exact => value,
                _ if low => value.next_down().next_down().max(0.0),
                _ => value.next_up().next_up(),
            }
        };
        let corners = [
            (self.lo, power.lo),
            (self.lo, power.hi),
            (self.hi, power.lo),
            (self.hi, power.hi),
        ];
        Range::new(
            corners
                .iter()
                .map(|&(b, e)| corner(b, e, true))
                .fold(f64::INFINITY, f64::min),
            corners
                .iter()
                .map(|&(b, e)| corner(b, e, false))
                .fold(f64::NEG_INFINITY, f64::max),
        )
    }
}

fn plus(a: f64, b: f64) -> (f64, f64) {
    let s = a + b;
    if s.is_nan() {
        return (f64::NEG_INFINITY, f64::INFINITY);
    }
    if s.is_infinite() {
        return match (a.is_finite() && b.is_finite(), s > 0.0) {
            (true, true) => (f64::MAX, s),
            (true, false) => (s, f64::MIN),
            _ => (s, s),
        };
    }
    let back = s - a;
    let error = (a - (s - back)) + (b - back);
    (
        if error < 0.0 { s.next_down() } else { s },
        if error > 0.0 { s.next_up() } else { s },
    )
}

/// a·b rounded down and up, with 0·∞ = 0 as befits bounds: exact products stay exact.
fn times(a: f64, b: f64) -> (f64, f64) {
    if a == 0.0 || b == 0.0 {
        return (0.0, 0.0);
    }
    let p = a * b;
    if p.is_infinite() {
        return match (a.is_finite() && b.is_finite(), p > 0.0) {
            (true, true) => (f64::MAX, p),
            (true, false) => (p, f64::MIN),
            _ => (p, p),
        };
    }
    // Near the bottom of the range the residual is no longer exact: a step either way.
    if p.abs() < 1e-290 {
        return (p.next_down(), p.next_up());
    }
    let error = a.mul_add(b, -p);
    (
        if error < 0.0 { p.next_down() } else { p },
        if error > 0.0 { p.next_up() } else { p },
    )
}

fn inverse(a: f64) -> (f64, f64) {
    let r = 1.0 / a;
    if r == 0.0 || r.is_infinite() {
        return (r, r);
    }
    let error = -r.mul_add(a, -1.0);
    (
        if error < 0.0 { r.next_down() } else { r },
        if error > 0.0 { r.next_up() } else { r },
    )
}

fn range(expr: &Expr, ranges: &HashMap<usize, Range>) -> Range {
    let of = |part: &Expr| range(part, ranges);
    match expr {
        Expr::Const(value) => Range::of(*value),
        Expr::Slot(slot) => ranges.get(slot).copied().unwrap_or(ALL),
        Expr::Neg(a) => of(a).neg(),
        Expr::Binary(op, a, b) => {
            let (left, right) = (of(a), of(b));
            match op {
                Op::Add => left.add(right),
                Op::Subtract => left.add(right.neg()),
                Op::Multiply => left.mul(right),
                Op::Divide => left.mul(right.recip()),
                Op::Power => match &**b {
                    Expr::Const(k) if k.floor() == *k && k.abs() < Qd::from_i64(1 << 20) => {
                        left.powi(k.to_f64() as i64)
                    }
                    _ => left.pow(right),
                },
            }
        }
        Expr::Unary(function, a) => {
            let a = of(a);
            let never = |_: f64| false;
            match function {
                Unary::Exp => a.increasing(f64::exp, |x| x == 0.0 || x.is_infinite()),
                Unary::Ln if a.lo >= 0.0 => a.increasing(f64::ln, |x| x == 1.0 || x == 0.0),
                Unary::Sqrt if a.lo >= 0.0 => a.increasing(f64::sqrt, |x| x == 0.0 || x == 1.0),
                Unary::Sinh => a.increasing(f64::sinh, |x| x == 0.0),
                Unary::Asinh => a.increasing(f64::asinh, |x| x == 0.0),
                Unary::Atan => a.increasing(f64::atan, |x| x == 0.0),
                Unary::Tanh => a.increasing(f64::tanh, |x| x == 0.0),
                Unary::Sin | Unary::Cos => Range::new(-1.0, 1.0),
                Unary::Cosh => Range::new(1.0, f64::INFINITY),
                Unary::Abs if a.contains_zero() => Range::new(0.0, a.hi.max(-a.lo)),
                Unary::Abs => Range::new(a.lo.abs().min(a.hi.abs()), a.lo.abs().max(a.hi.abs())),
                Unary::Floor | Unary::Ceil | Unary::Round | Unary::Trunc => {
                    a.add(Range::new(-1.0, 1.0)).increasing(|x| x, never)
                }
                _ => ALL,
            }
        }
        _ => ALL,
    }
}

#[cfg(test)]
mod tests {
    use super::{ALL, Range, range};
    use crate::app::graph::numeric::{Compiler, Expr, Formula};
    use crate::app::latex::Op;
    use std::collections::HashMap;

    fn bare(expr: &Expr) -> &Expr {
        match expr {
            Expr::Memo(memo) => &memo.inner,
            other => other,
        }
    }

    fn planned(text: &str) -> Expr {
        Formula::calculus(text, false).unwrap().root
    }

    #[test]
    fn swaps_only_sums_of_integrals_of_one_sign() {
        let swapped = |text: &str| matches!(planned(text), Expr::Either(first, _) if matches!(bare(&first), Expr::Integral { .. }));
        assert!(swapped("sum(integrate(z^y, 0, 1, z)/y^2, 1, infinity, y)"));
        assert!(!swapped(
            "sum(integrate(sin(z*y), 0, 1, z)/y^2, 1, infinity, y)"
        ));
        assert!(!swapped("sum(integrate(z^y, 0, 1, z), 1, 5, y)"));
    }

    #[test]
    fn takes_out_what_does_not_vary_and_remembers_what_has_no_x() {
        let pulled = planned("integrate(x*e^(-y), 0, infinity, y)");
        assert!(matches!(
            pulled,
            Expr::Binary(Op::Multiply, left, right)
                if matches!(*left, Expr::X) && matches!(*right, Expr::Memo(_))
        ));
    }

    fn bounds(text: &str, slots: &[(usize, Range)]) -> Range {
        let node = crate::app::latex::parse(text, false).unwrap();
        let mut compiler = Compiler {
            scopes: vec![("y".to_string(), 0), ("z".to_string(), 1)],
            slots: 2,
            ..Compiler::default()
        };
        let expr: Expr = compiler.compile(&node).unwrap();
        range(&expr, &slots.iter().copied().collect::<HashMap<_, _>>())
    }

    #[test]
    fn proves_the_signs_tonelli_needs() {
        let n = (0, Range::new(1.0, f64::INFINITY));
        let t = (1, Range::new(0.0, 1.0));
        let term = bounds("1/y^2*ln(1+x^2*z^2)/(1+z^y)", &[n, t]);
        assert!(term.lo >= 0.0, "{term:?}");
        assert_eq!(bounds("ln(1+z^2)", &[t]).lo, 0.0);
        assert!(bounds("sin(z)/y^2", &[n, t]).contains_zero());
        assert_eq!(bounds("y", &[]), ALL);
        assert!(bounds("e^(-z)*z", &[t]).lo >= 0.0);
        assert!(bounds("-(1+z)", &[t]).hi <= -1.0);
    }
}
