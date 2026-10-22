//! Sums and products, finite or infinite; slow infinite sums are accelerated, with an error estimate.

use super::{Estimate, Fault, MINIMUM, Qd};
use std::collections::HashMap;

/// Terms added one by one before a series that neither shrinks geometrically nor slowly is given up.
const DIRECT: usize = 2_000;
fn orders(tolerance: f64) -> (usize, usize) {
    // Each term gains log10(3 + √8) ≈ 0.766 digits.
    let terms = (-tolerance.log10() / 0.766).ceil() as usize + 12;
    (terms, terms - 10)
}

pub(super) fn sum(
    term: &mut dyn FnMut(Qd) -> Result<Estimate, Fault>,
    from: Qd,
    to: Qd,
    product: bool,
    tolerance: f64,
) -> Result<Estimate, Fault> {
    let empty = Estimate::exact(if product { Qd::ONE } else { Qd::ZERO });
    let whole = |n: Qd| n.is_finite() && n.floor() == n;
    if !whole(from) || !(whole(to) || to == Qd::INFINITY) {
        return Err(Fault::Undefined);
    }
    if to < from {
        return Ok(empty);
    }
    if to.is_finite() {
        // Past 10^63 a `Qd` no longer counts by ones.
        if from + Qd::ONE == from || to + Qd::ONE == to {
            return Err(Fault::Inaccurate);
        }
        // However many terms: each is a step `term` counts, so a newer request still stops it.
        let (mut total, mut n) = (empty, from);
        while n <= to {
            let value = term(n)?;
            total = if product {
                total.times(value)
            } else {
                total.plus(value)
            };
            n += Qd::ONE;
        }
        return Ok(total);
    }
    if product {
        // An infinite product of positive factors is the exponential of a sum.
        let logarithm = infinite(
            &mut |n| match term(n)? {
                factor if factor.value > Qd::ZERO => Ok(Estimate {
                    value: factor.value.ln(),
                    error: factor.error / factor.value.to_f64(),
                }),
                _ => Err(Fault::Inaccurate),
            },
            from,
            tolerance,
        )?;
        let value = logarithm.value.exp();
        return Ok(Estimate {
            value,
            error: value.to_f64() * logarithm.error,
        });
    }
    infinite(term, from, tolerance)
}

fn infinite(
    term: &mut dyn FnMut(Qd) -> Result<Estimate, Fault>,
    from: Qd,
    tolerance: f64,
) -> Result<Estimate, Fault> {
    let limit = Qd::from_f64(tolerance);
    let mut terms: Vec<Qd> = Vec::new();
    let mut total = Estimate::exact(Qd::ZERO);
    let ratio = |terms: &[Qd], i: usize| match (terms[i].abs(), terms[i - 1].abs()) {
        (now, _) if now.is_zero() => Qd::ZERO,
        (_, before) if before.is_zero() => Qd::INFINITY,
        (now, before) => now / before,
    };
    let bound = Qd::from_f64(0.9);
    loop {
        let k = terms.len();
        if k >= DIRECT {
            return Err(Fault::Inaccurate);
        }
        let estimate = term(from + Qd::from_i64(k as i64))?;
        total = total.plus(estimate);
        let value = estimate.value;
        terms.push(value);
        if k < 20 {
            continue;
        }
        // Geometric or faster: the largest of the last ten terms is a third of the ten before at most.
        let peak = |first: usize| {
            (first..first + 10)
                .map(|i| terms[i].abs())
                .fold(Qd::ZERO, Qd::max)
        };
        let (recent, older) = (peak(k - 9), peak(k - 19));
        if recent.is_zero() || recent <= older * Qd::from_f64(0.35) {
            let rate = if recent.is_zero() {
                Qd::ZERO
            } else {
                recent / older
            };
            let tail = Qd::from_i64(10) * recent * rate / (Qd::ONE - rate);
            if tail <= limit * total.value.abs() {
                total.error += tail.to_f64();
                return Ok(total);
            }
        }
        // Shrinking, but by less than a tenth per term: slower than geometric, so accelerate.
        let slow = |i| (bound..Qd::ONE).contains(&ratio(&terms, i));
        if k >= 30
            && (k - 9..=k).all(slow)
            && value.abs() <= terms[k / 2].abs() * Qd::from_f64(0.75)
        {
            break;
        }
    }
    let start = from + Qd::from_i64(terms.len() as i64);
    let recent = &terms[terms.len() - 16..];
    let alternating = recent
        .windows(2)
        .all(|pair| !pair[0].is_zero() && pair[0].is_sign_negative() != pair[1].is_sign_negative());
    let positive = recent.iter().all(|value| *value > Qd::ZERO);
    let negative = recent.iter().all(|value| *value < Qd::ZERO);
    let decreasing = recent.windows(2).all(|pair| pair[1].abs() <= pair[0].abs());
    let tail = if alternating {
        let sign = if recent[recent.len() - 1].is_sign_negative() {
            Qd::ONE
        } else {
            -Qd::ONE
        };
        let magnitudes = (0..orders(tolerance).0)
            .map(|j| {
                let estimate = term(start + Qd::from_i64(j as i64))?;
                Ok(Estimate {
                    value: estimate.value.abs(),
                    error: estimate.error,
                })
            })
            .collect::<Result<Vec<_>, Fault>>()?;
        accelerated(&magnitudes, tolerance)?.scaled(sign)
    } else if (positive || negative) && decreasing {
        let sign = if positive { Qd::ONE } else { -Qd::ONE };
        let mut c = |m| Ok(term(start - Qd::ONE + m)?.scaled(sign));
        condensed(&mut c, tolerance)?.scaled(sign)
    } else {
        return Err(Fault::Inaccurate);
    };
    Ok(total.plus(tail))
}

/// Σ_{m≥1} c(m) for positive, decreasing terms: the van Wijngaarden transform makes it alternating.
fn condensed(
    c: &mut dyn FnMut(Qd) -> Result<Estimate, Fault>,
    tolerance: f64,
) -> Result<Estimate, Fault> {
    let negligible = Qd::from_f64(tolerance * 1e-3);
    let terms = orders(tolerance).0;
    let mut known: HashMap<u64, Estimate> = HashMap::new();
    let mut value = |m: f64| -> Result<Estimate, Fault> {
        if let Some(value) = known.get(&m.to_bits()) {
            return Ok(*value);
        }
        let value = c(Qd::from_f64(m))?;
        known.insert(m.to_bits(), value);
        Ok(value)
    };
    // b_j = Σ_i 2^i c(2^i j), the terms of the alternating series Σ (-1)^(j-1) b_j.
    let mut b = Vec::with_capacity(terms);
    for j in 1..=terms {
        let (mut total, mut scale, mut small) = (Estimate::exact(Qd::ZERO), 1.0f64, 0);
        while small < 3 {
            let m = scale * j as f64;
            if !m.is_finite() || m > 1e300 {
                return Err(Fault::Inaccurate);
            }
            let part = value(m)?.scaled(Qd::from_f64(scale));
            total = total.plus(part);
            small = if part.value.abs() <= negligible * total.value.abs() {
                small + 1
            } else {
                0
            };
            scale *= 2.0;
        }
        b.push(total);
    }
    accelerated(&b, tolerance)
}

/// Σ_{k≥0} (-1)^k b_k, accelerated; the two orders' disagreement counts in its error.
fn accelerated(b: &[Estimate], tolerance: f64) -> Result<Estimate, Fault> {
    let full = cohen_villegas_zagier(b);
    let check = cohen_villegas_zagier(&b[..orders(tolerance).1]);
    let estimate = Estimate {
        value: full.value,
        error: full.error + (full.value - check.value).abs().to_f64(),
    };
    if estimate.relative() <= MINIMUM {
        Ok(estimate)
    } else {
        Err(Fault::Inaccurate)
    }
}

/// Algorithm 1 of Cohen, Rodriguez Villegas and Zagier (2000), with n = b.len() terms.
pub(super) fn cohen_villegas_zagier(b: &[Estimate]) -> Estimate {
    let n = b.len() as i64;
    let mut d = (Qd::from_i64(3) + Qd::from_i64(8).sqrt()).powi(n);
    d = (d + d.recip()) / Qd::from_i64(2);
    let (mut weight, mut c, mut total, mut error) = (-Qd::ONE, -d, Qd::ZERO, 0.0f64);
    for (k, term) in (0i64..).zip(b) {
        c = weight - c;
        total += c * term.value;
        error += (c / d).abs().to_f64() * term.error;
        weight = weight * Qd::from_i64((k + n) * (k - n))
            / (Qd::from_f64(k as f64 + 0.5) * Qd::from_i64(k + 1));
    }
    Estimate {
        value: total / d,
        error,
    }
}

#[cfg(test)]
mod tests {
    use super::{Estimate, Fault, Qd, sum};
    use crate::app::graph::numeric::TOLERANCE;

    fn close(value: Qd, expected: Qd) -> bool {
        (value - expected).abs() <= expected.abs() * Qd::from_f64(1e-50)
    }

    fn infinite(term: fn(Qd) -> Qd, from: i64) -> Result<Qd, Fault> {
        let mut exact = |n| Ok(Estimate::exact(term(n)));
        let total = sum(
            &mut exact,
            Qd::from_i64(from),
            Qd::INFINITY,
            false,
            TOLERANCE,
        )?;
        Ok(total.value)
    }

    #[test]
    fn sums_series_to_fifty_digits() {
        let pi = Qd::PI;
        let half = infinite(|n| Qd::from_f64(0.5).powi(n.to_f64() as i64), 0).unwrap();
        assert!(close(half, Qd::from_i64(2)));
        let mut factorial = Qd::ONE;
        let mut term = |n: Qd| {
            if !n.is_zero() {
                factorial /= n;
            }
            Ok(Estimate::exact(factorial))
        };
        let e = sum(&mut term, Qd::ZERO, Qd::INFINITY, false, TOLERANCE).unwrap();
        assert!(close(e.value, Qd::E));
        let ln2 = infinite(
            |n| Qd::from_i64(if n.to_f64() as i64 % 2 == 1 { 1 } else { -1 }) / n,
            1,
        );
        assert!(close(ln2.unwrap(), Qd::LN_2));
        let basel = infinite(|n| (n * n).recip(), 1).unwrap();
        assert!(close(basel, pi * pi / Qd::from_i64(6)));
        let leibniz = infinite(
            |n| Qd::from_i64(1 - 2 * (n.to_f64() as i64 % 2)) / (n + n + Qd::ONE),
            0,
        );
        assert!(close(leibniz.unwrap(), pi / Qd::from_i64(4)));
    }

    #[test]
    fn finite_sums_and_products() {
        let three = Qd::from_i64(3);
        let mut exact = |n| Ok(Estimate::exact(n));
        let squares = sum(
            &mut |n| Ok(Estimate::exact(n * n)),
            Qd::ONE,
            three,
            false,
            TOLERANCE,
        );
        assert_eq!(squares.unwrap().value, Qd::from_i64(14));
        let factorial = sum(&mut exact, Qd::ONE, Qd::from_i64(5), true, TOLERANCE);
        assert_eq!(factorial.unwrap().value, Qd::from_i64(120));
        let empty = sum(&mut exact, three, Qd::ONE, false, TOLERANCE);
        assert_eq!(empty.unwrap().value, Qd::ZERO);
    }

    #[test]
    fn refuses_what_it_cannot_vouch_for() {
        // Divergent: the harmonic series, and Grandi's, which acceleration alone would call 1/2.
        assert!(infinite(|n| n.recip(), 1).is_err());
        assert!(infinite(|n| Qd::from_i64(1 - 2 * (n.to_f64() as i64 % 2)), 0).is_err());
        assert_eq!(infinite(|n| n.sin() / (n * n), 1), Err(Fault::Inaccurate));
    }
}
