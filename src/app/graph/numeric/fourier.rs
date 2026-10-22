//! Σ cos(nθ)/n^p and Σ sin(nθ)/n^p, from Li_p(e^{iθ}) expanded around θ = 0.

use super::{Estimate, Qd, series};
use std::sync::OnceLock;

/// Powers above this are summed term by term: 10^(66/40), about 45 terms, reach the precision.
const DIRECT: u32 = 40;
/// ζ(k) is kept for k up to this: (θ/2π)^2j ≤ 4^-j is below 10^-66 well before.
const LAST: usize = 224;
/// Digits the expansion is carried to at most: past the 62 of a `Qd`.
const DIGITS: f64 = 66.0;
const GUARD: f64 = 6.0;
/// What a few dozen `Qd` operations leave, relative to the largest term.
const ROUNDING: f64 = 1e-61;
/// What is left of θ after whole turns, below this fraction of θ, is only θ's rounding.
const TURNS: f64 = 1e-60;

/// ζ(k) for 2 ≤ k ≤ LAST, from the alternating η(k) accelerated, computed once.
fn zeta(k: usize) -> Qd {
    static TABLE: OnceLock<Vec<Qd>> = OnceLock::new();
    TABLE.get_or_init(|| {
        (0..=LAST)
            .map(|k| {
                if k < 2 {
                    return Qd::NAN;
                }
                let terms: Vec<Estimate> = (1..=90)
                    .map(|n| Estimate::exact(Qd::from_i64(n).recip().powi(k as i64)))
                    .collect();
                let eta = series::cohen_villegas_zagier(&terms).value;
                eta / (Qd::ONE - Qd::from_f64(2f64.powi(1 - k as i32)))
            })
            .collect()
    })[k]
}

/// 2ζ(2j)(2π)^-2j·(2j-1)!(p-1)!/(2j+p-1)! for j ≥ 1: the coefficients of θ^2j in the expansion.
fn coefficients(p: u32) -> &'static [Qd] {
    static TABLES: [OnceLock<Vec<Qd>>; DIRECT as usize + 1] =
        [const { OnceLock::new() }; DIRECT as usize + 1];
    TABLES[p as usize].get_or_init(|| {
        let step = (Qd::TAU * Qd::TAU).recip();
        let mut scale = Qd::ONE;
        (1..=LAST / 2)
            .map(|j| {
                scale *= step;
                let rising = (0..p).fold(Qd::ONE, |product, i| {
                    let i = i64::from(i);
                    product * Qd::from_i64(2 * j as i64 + i) / Qd::from_i64(i.max(1))
                });
                Qd::from_i64(2) * zeta(2 * j) * scale / rising
            })
            .collect()
    })
}

pub(super) fn sums(p: u32, theta: Qd, tolerance: f64) -> (Estimate, Estimate) {
    // Whole turns off exactly: θ = 10^100 is no worse than 1, and a rounded 2π is still a whole turn.
    let reduced = match theta.rem_tau() {
        rest if rest.abs().to_f64() <= theta.abs().to_f64() * TURNS => Qd::ZERO,
        rest => rest,
    };
    let digits = (GUARD - tolerance.log10()).min(DIGITS);
    let ((cosines, sines), (real, imaginary), cut) = if p > DIRECT {
        let (sums, sizes) = direct(p, reduced);
        (sums, sizes, 0.0)
    } else {
        expansion(p, reduced, digits)
    };
    (
        Estimate {
            value: cosines,
            error: real * ROUNDING + cut,
        },
        Estimate {
            value: sines,
            error: imaginary * ROUNDING + cut,
        },
    )
}

fn direct(p: u32, theta: Qd) -> ((Qd, Qd), (f64, f64)) {
    let last = 10f64.powf(66.0 / f64::from(p)).ceil() as i64 + 1;
    let (sin, cos) = theta.sin_cos();
    let (mut s, mut c) = (Qd::ZERO, Qd::ONE);
    let (mut cosines, mut sines) = (Qd::ZERO, Qd::ZERO);
    for n in 1..=last {
        (s, c) = (s * cos + c * sin, c * cos - s * sin);
        let scale = Qd::from_i64(n).recip().powi(i64::from(p));
        cosines += c * scale;
        sines += s * scale;
    }
    ((cosines, sines), (1.0, 1.0))
}

/// Li_p(e^{iθ}) = Σ_{k≠p-1} ζ(p-k)(iθ)^k/k! + (iθ)^{p-1}/(p-1)!·(H_{p-1} - ln(-iθ)), |θ| ≤ π.
fn expansion(p: u32, theta: Qd, digits: f64) -> ((Qd, Qd), (f64, f64), f64) {
    if theta.is_zero() {
        let at_one = if p >= 2 {
            zeta(p as usize)
        } else {
            Qd::INFINITY
        };
        return ((at_one, Qd::ZERO), (at_one.to_f64(), 0.0), 0.0);
    }
    let (mut real, mut imaginary) = (Qd::ZERO, Qd::ZERO);
    let (mut real_size, mut imaginary_size) = (0.0f64, 0.0f64);
    // ζ(p-k)(iθ)^k/k! for k ≤ p-2: ζ at whole numbers from p down to 2.
    let mut power = Qd::ONE;
    for k in 0..p.saturating_sub(1) {
        if k > 0 {
            power = power * theta / Qd::from_i64(i64::from(k));
        }
        let term = zeta((p - k) as usize) * power;
        let size = term.abs().to_f64();
        if k % 2 == 0 {
            real_size = real_size.max(size);
        } else {
            imaginary_size = imaginary_size.max(size);
        }
        match k % 4 {
            0 => real += term,
            1 => imaginary += term,
            2 => real -= term,
            _ => imaginary -= term,
        }
    }
    // ζ at the negative odd numbers, through ζ(2j): a series in θ², each term (θ/2π)² of the last.
    let table = coefficients(p);
    let ratio = (theta.abs().to_f64() / std::f64::consts::TAU).powi(2);
    let count = ((-digits / ratio.log10()).ceil() as usize + 1).min(table.len());
    let square = theta * theta;
    let rest = table[..count]
        .iter()
        .rev()
        .fold(Qd::ZERO, |sum, &c| (sum + c) * square);
    // Each coefficient is below 2ζ(2)(2π)^-2j: what is left is below a geometric tail.
    let left =
        2.0 * std::f64::consts::PI.powi(2) / 6.0 * ratio.powi(count as i32 + 1) / (1.0 - ratio);
    // (iθ)^{p-1}/(p-1)! times W = H_{p-1} - ln|θ| + rest + i((π/2)·sgn θ - θ/2p); ζ(0) gave -θ/2p.
    let harmonic = (1..p).fold(Qd::ZERO, |sum, i| sum + Qd::from_i64(i64::from(i)).recip());
    let w_real = harmonic - theta.abs().ln() + rest;
    let sign = if theta.is_sign_negative() {
        -Qd::ONE
    } else {
        Qd::ONE
    };
    let w_imaginary = Qd::FRAC_PI_2 * sign - theta / Qd::from_i64(2 * i64::from(p));
    let lead = (1..p).fold(Qd::ONE, |product, i| {
        product * theta / Qd::from_i64(i64::from(i))
    });
    let (a, b) = match (p - 1) % 4 {
        0 => (w_real, w_imaginary),
        1 => (-w_imaginary, w_real),
        2 => (-w_real, -w_imaginary),
        _ => (w_imaginary, -w_real),
    };
    let lead_size = lead.abs().to_f64();
    let sizes = (
        real_size.max(lead_size * a.abs().to_f64()),
        imaginary_size.max(lead_size * b.abs().to_f64()),
    );
    (
        (real + lead * a, imaginary + lead * b),
        sizes,
        lead_size * left,
    )
}

#[cfg(test)]
mod tests {
    use super::sums;
    use crate::app::graph::numeric::Qd;

    fn close(value: Qd, expected: &str) -> bool {
        let expected = Qd::parse(expected).unwrap();
        (value - expected).abs() <= expected.abs() * Qd::from_f64(1e-50)
    }

    #[test]
    fn matches_mpmath_clausen_functions() {
        // mpmath clsin(p, θ) and clcos(p, θ) at 70 digits.
        let (c, s) = sums(2, Qd::ONE, 1e-60);
        assert!(close(
            s.value,
            "1.013959132360768504294574338885914687561179280077717"
        ));
        assert!(close(
            c.value,
            "0.3241377400533298172410934750062737471203652015192455"
        ));
        let (c, s) = sums(1, Qd::from_i64(2), 1e-60);
        assert!(close(
            s.value,
            "0.5707963267948966192313216916397514420985846996875529"
        ));
        assert!(close(
            c.value,
            "-0.5205434342908536309038211455942674982353917254638504"
        ));
        let (_, s) = sums(2, Qd::from_i64(-40), 1e-60);
        assert!(close(
            s.value,
            "-0.5575209273342949362659750115293202813463030347922784"
        ));
        let (c, s) = sums(5, Qd::from_f64(0.25), 1e-60);
        assert!(close(
            s.value,
            "0.2665487204162648984056403575703999425580424557987538"
        ));
        assert!(close(
            c.value,
            "0.9999282232290315283854383578390458863472493851621107"
        ));
        let (c, _) = sums(50, Qd::from_i64(3), 1e-60);
        assert!(close(
            c.value,
            "-0.9899924966004446044690462237562251684591234734038446"
        ));
        let (c, s) = sums(2, Qd::ZERO, 1e-60);
        let basel = "1.644934066848226436472415166646025189218949901206798";
        assert!(close(c.value, basel) && s.value.is_zero());
        assert!(!sums(1, Qd::ZERO, 1e-60).0.value.is_finite());
        let (c, s) = sums(1, Qd::TAU, 1e-60);
        assert!(s.value.is_zero() && !c.value.is_finite());
        assert!(sums(1, Qd::TAU * Qd::from_i64(-3), 1e-60).1.value.is_zero());
        let (_, s) = sums(1, Qd::TAU + Qd::from_f64(1e-40), 1e-60);
        let above = Qd::FRAC_PI_2 - Qd::from_f64(0.5e-40);
        assert!((s.value - above).abs() < Qd::from_f64(1e-55));
    }
}
