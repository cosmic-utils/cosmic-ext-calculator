//! Quad-double floating point (about 62 significant digits), after Hida, Li & Bailey (2001).

use std::cmp::Ordering;
use std::f64::consts;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

/// Unevaluated sum of four non-overlapping doubles of decreasing magnitude.
#[derive(Clone, Copy, Debug, Default)]
pub struct Qd(pub [f64; 4]);

#[inline]
fn two_sum(a: f64, b: f64) -> (f64, f64) {
    let s = a + b;
    let v = s - a;
    (s, (a - (s - v)) + (b - v))
}

#[inline]
fn quick_two_sum(a: f64, b: f64) -> (f64, f64) {
    let s = a + b;
    (s, b - (s - a))
}

#[inline]
fn two_prod(a: f64, b: f64) -> (f64, f64) {
    let p = a * b;
    (p, a.mul_add(b, -p))
}

#[inline]
fn three_sum(a: f64, b: f64, c: f64) -> (f64, f64, f64) {
    let (t1, t2) = two_sum(a, b);
    let (s, t3) = two_sum(c, t1);
    let (u, v) = two_sum(t2, t3);
    (s, u, v)
}

#[inline]
fn renorm<const N: usize>(mut c: [f64; N]) -> Qd {
    let mut s = c[N - 1];
    for i in (0..N - 1).rev() {
        (s, c[i + 1]) = quick_two_sum(c[i], s);
    }
    let mut out = [0.0; 4];
    let mut k = 0;
    for &x in &c[1..] {
        if k == 3 {
            s += x;
            continue;
        }
        let (hi, lo) = quick_two_sum(s, x);
        if lo == 0.0 {
            s = hi;
        } else {
            out[k] = hi;
            k += 1;
            s = lo;
        }
    }
    out[k] = s;
    Qd(out)
}

/// Replaces a non-finite result by the sign-correct infinity or NaN of a double estimate.
#[inline]
fn guard(r: Qd, approx: f64) -> Qd {
    if r.0.iter().all(|c| c.is_finite()) {
        r
    } else if approx.is_nan() {
        Qd::NAN
    } else {
        Qd::from_f64(f64::INFINITY.copysign(approx))
    }
}

#[inline]
fn pow2(k: i32) -> f64 {
    f64::from_bits(((k + 1023) as u64) << 52)
}

/// x·2^k with a single rounding (musl scalbn).
fn ldexp(mut x: f64, mut k: i32) -> f64 {
    if k > 1023 {
        x *= pow2(1023);
        k -= 1023;
        if k > 1023 {
            x *= pow2(1023);
            k = (k - 1023).min(1023);
        }
    } else if k < -1022 {
        x *= pow2(-969);
        k += 969;
        if k < -1022 {
            x *= pow2(-969);
            k = (k + 969).max(-1022);
        }
    }
    x * pow2(k)
}

/// e with 2^(e-1) ≤ |x| < 2^e, for finite nonzero x.
fn exponent(x: f64) -> i32 {
    let e = ((x.to_bits() >> 52) & 0x7ff) as i32;
    match e {
        0 if x == 0.0 => -1100,
        0 => exponent(x * pow2(64)) - 64,
        _ => e - 1022,
    }
}

fn accumulate(m: &[f64]) -> Qd {
    let (mut u, mut v) = quick_two_sum(m[0], m[1]);
    let mut x = [0.0; 4];
    let (mut k, mut n) = (0, 2);
    while k < 4 && n < m.len() {
        let (s1, e1) = two_sum(v, m[n]);
        let (s, e2) = two_sum(u, s1);
        n += 1;
        if e1 != 0.0 && e2 != 0.0 {
            x[k] = s;
            k += 1;
            (u, v) = (e2, e1);
        } else {
            (u, v) = (s, if e1 == 0.0 { e2 } else { e1 });
        }
    }
    if k < 3 {
        (x[k], x[k + 1]) = (u, v);
    } else if k == 3 {
        x[3] = u + v;
    } else {
        x[3] += u + v + m[n..].iter().rev().sum::<f64>();
    }
    renorm(x)
}

/// Accurate ("IEEE") addition: merge by magnitude, then accumulate exactly.
fn qd_add(a: Qd, b: Qd) -> Qd {
    let approx = a.0[0] + b.0[0];
    if approx.is_infinite() && a.is_finite() && b.is_finite() {
        // the leading parts can overflow while the full sum does not
        return ldexp_qd(qd_add(a.scale(0.5), b.scale(0.5)), 1);
    }
    if !approx.is_finite() || b.0[0] == 0.0 && a.0[0] == 0.0 {
        return Qd::from_f64(approx);
    }
    if b.0[0] == 0.0 || a.0[0] == 0.0 {
        return if b.0[0] == 0.0 { a } else { b };
    }
    let (a, b) = (a.0, b.0);
    let mut m = [0.0; 8];
    let (mut i, mut j) = (0, 0);
    for slot in &mut m {
        *slot = if j == 4 || (i < 4 && a[i].abs() > b[j].abs()) {
            i += 1;
            a[i - 1]
        } else {
            j += 1;
            b[j - 1]
        };
    }
    guard(accumulate(&m), approx)
}

/// Accurate multiplication (Hida, Li & Bailey, accurate variant).
fn qd_mul(a: Qd, b: Qd) -> Qd {
    let approx = a.0[0] * b.0[0];
    if approx.is_infinite() && a.is_finite() && b.is_finite() && (a.0[0] * 0.5 * b.0[0]).is_finite()
    {
        return ldexp_qd(qd_mul(a.scale(0.5), b), 1);
    }
    if !approx.is_finite() || approx == 0.0 {
        return Qd::from_f64(approx);
    }
    let ([a0, a1, a2, a3], [b0, b1, b2, b3]) = (a.0, b.0);
    let (p0, q0) = two_prod(a0, b0);
    let (p1, q1) = two_prod(a0, b1);
    let (p2, q2) = two_prod(a1, b0);
    let (p3, q3) = two_prod(a0, b2);
    let (p4, q4) = two_prod(a1, b1);
    let (p5, q5) = two_prod(a2, b0);
    let (p1, p2, q0) = three_sum(p1, p2, q0);
    let (p2, q1, q2) = three_sum(p2, q1, q2);
    let (p3, p4, p5) = three_sum(p3, p4, p5);
    let (s0, t0) = two_sum(p2, p3);
    let (s1, t1) = two_sum(q1, p4);
    let (s1, t0) = two_sum(s1, t0);
    let s2 = q2 + p5 + (t0 + t1);
    let (p6, q6) = two_prod(a0, b3);
    let (p7, q7) = two_prod(a1, b2);
    let (p8, q8) = two_prod(a2, b1);
    let (p9, q9) = two_prod(a3, b0);
    let (q0, q3) = two_sum(q0, q3);
    let (q4, q5) = two_sum(q4, q5);
    let (p6, p7) = two_sum(p6, p7);
    let (p8, p9) = two_sum(p8, p9);
    let (t0, t1) = two_sum(q0, q4);
    let t1 = t1 + (q3 + q5);
    let (r0, r1) = two_sum(p6, p8);
    let r1 = r1 + (p7 + p9);
    let (q3, q4) = two_sum(t0, r0);
    let q4 = q4 + (t1 + r1);
    let (t0, t1) = two_sum(q3, s1);
    let t1 = t1 + q4 + (a1 * b3 + a2 * b2 + a3 * b1 + q6 + q7 + q8 + q9 + s2);
    guard(renorm([p0, p1, s0, t0, t1]), approx)
}

fn mul_f64(a: Qd, b: f64) -> Qd {
    let approx = a.0[0] * b;
    if !approx.is_finite() || approx == 0.0 {
        return Qd::from_f64(approx);
    }
    let (p0, q0) = two_prod(a.0[0], b);
    let (p1, q1) = two_prod(a.0[1], b);
    let (p2, q2) = two_prod(a.0[2], b);
    let (p3, q3) = two_prod(a.0[3], b);
    let (s1, e1) = two_sum(q0, p1);
    let (s2, e2, e3) = three_sum(e1, q1, p2);
    let (u, v) = two_sum(e2, q2);
    let (s3, w) = two_sum(u, p3);
    guard(renorm([p0, s1, s2, s3, e3 + v + w + q3]), approx)
}

fn qd_div(a: Qd, b: Qd) -> Qd {
    let q0 = a.0[0] / b.0[0];
    if !q0.is_finite() || q0 == 0.0 {
        return Qd::from_f64(q0);
    }
    if a.0[0].abs() > 1e300 {
        // keep b·q from overflowing near the top of the range
        return ldexp_qd(qd_div(ldexp_qd(a, -64), b), 64);
    }
    let mut r = a - mul_f64(b, q0);
    let q1 = r.0[0] / b.0[0];
    r -= mul_f64(b, q1);
    let q2 = r.0[0] / b.0[0];
    r -= mul_f64(b, q2);
    let q3 = r.0[0] / b.0[0];
    r -= mul_f64(b, q3);
    let q4 = r.0[0] / b.0[0];
    guard(renorm([q0, q1, q2, q3, q4]), q0)
}

impl Add for Qd {
    type Output = Qd;
    #[inline]
    fn add(self, rhs: Qd) -> Qd {
        qd_add(self, rhs)
    }
}

impl Sub for Qd {
    type Output = Qd;
    #[inline]
    fn sub(self, rhs: Qd) -> Qd {
        qd_add(self, -rhs)
    }
}

impl Mul for Qd {
    type Output = Qd;
    #[inline]
    fn mul(self, rhs: Qd) -> Qd {
        qd_mul(self, rhs)
    }
}

impl Div for Qd {
    type Output = Qd;
    #[inline]
    fn div(self, rhs: Qd) -> Qd {
        qd_div(self, rhs)
    }
}

impl Neg for Qd {
    type Output = Qd;
    #[inline]
    fn neg(self) -> Qd {
        Qd(self.0.map(|c| -c))
    }
}

impl AddAssign for Qd {
    #[inline]
    fn add_assign(&mut self, rhs: Qd) {
        *self = qd_add(*self, rhs);
    }
}

impl SubAssign for Qd {
    #[inline]
    fn sub_assign(&mut self, rhs: Qd) {
        *self = qd_add(*self, -rhs);
    }
}

impl MulAssign for Qd {
    #[inline]
    fn mul_assign(&mut self, rhs: Qd) {
        *self = qd_mul(*self, rhs);
    }
}

impl DivAssign for Qd {
    #[inline]
    fn div_assign(&mut self, rhs: Qd) {
        *self = qd_div(*self, rhs);
    }
}

impl PartialEq for Qd {
    fn eq(&self, other: &Qd) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for Qd {
    fn partial_cmp(&self, other: &Qd) -> Option<Ordering> {
        let (a, b) = (self.0[0], other.0[0]);
        if !a.is_finite() || !b.is_finite() {
            return a.partial_cmp(&b);
        }
        // the leading parts decide unless the tails could bridge the gap
        if (a - b).abs() > 2.0 * (self.0[1].abs() + other.0[1].abs()) {
            return a.partial_cmp(&b);
        }
        (*self - *other).0[0].partial_cmp(&0.0)
    }
}

impl From<f64> for Qd {
    fn from(x: f64) -> Qd {
        Qd::from_f64(x)
    }
}

impl From<i64> for Qd {
    fn from(x: i64) -> Qd {
        Qd::from_i64(x)
    }
}

impl Qd {
    pub const ZERO: Qd = Qd::from_f64(0.0);
    pub const ONE: Qd = Qd::from_f64(1.0);
    pub const NAN: Qd = Qd::from_f64(f64::NAN);
    pub const INFINITY: Qd = Qd::from_f64(f64::INFINITY);
    pub const NEG_INFINITY: Qd = Qd::from_f64(f64::NEG_INFINITY);
    pub const PI: Qd = Qd([
        consts::PI,
        1.2246467991473532e-16,
        -2.9947698097183397e-33,
        1.1124542208633653e-49,
    ]);
    pub const TAU: Qd = Qd([
        consts::TAU,
        2.4492935982947064e-16,
        -5.989539619436679e-33,
        2.2249084417267306e-49,
    ]);
    pub const FRAC_PI_2: Qd = Qd([
        consts::FRAC_PI_2,
        6.123233995736766e-17,
        -1.4973849048591698e-33,
        5.562271104316826e-50,
    ]);
    pub const E: Qd = Qd([
        consts::E,
        1.4456468917292502e-16,
        -2.1277171080381768e-33,
        1.5156301598412191e-49,
    ]);
    pub const LN_2: Qd = Qd([
        consts::LN_2,
        2.3190468138462996e-17,
        5.707708438416212e-34,
        -3.5824322106018114e-50,
    ]);
    pub const LN_10: Qd = Qd([
        consts::LN_10,
        -2.1707562233822494e-16,
        -9.984262454465777e-33,
        -4.023357454450206e-49,
    ]);
    pub const fn from_f64(x: f64) -> Qd {
        Qd([x, 0.0, 0.0, 0.0])
    }

    pub fn from_i64(x: i64) -> Qd {
        let hi = x as f64;
        Qd([hi, (i128::from(x) - hi as i128) as f64, 0.0, 0.0])
    }

    pub fn to_f64(self) -> f64 {
        let [a0, a1, a2, a3] = self.0;
        let (s, e) = two_sum(a0, a1);
        if !s.is_finite() {
            // the halved value rounds the same way, and doubling decides overflow
            return if a0.is_finite() {
                2.0 * self.scale(0.5).to_f64()
            } else {
                a0 + a1
            };
        }
        let t = a2 + a3;
        // the tail only matters when a0 + a1 sits exactly halfway
        if e != 0.0 && t != 0.0 && (e > 0.0) == (t > 0.0) {
            let n = if e > 0.0 { s.next_up() } else { s.next_down() };
            if n - s == 2.0 * e {
                return n;
            }
        }
        s
    }

    pub fn is_nan(self) -> bool {
        self.0[0].is_nan()
    }

    pub fn is_finite(self) -> bool {
        self.0[0].is_finite()
    }

    pub fn is_infinite(self) -> bool {
        self.0[0].is_infinite()
    }

    pub fn is_zero(self) -> bool {
        self.0[0] == 0.0
    }

    pub fn is_sign_negative(self) -> bool {
        self.0[0].is_sign_negative()
    }

    pub fn abs(self) -> Qd {
        if self.is_sign_negative() { -self } else { self }
    }

    pub fn floor(self) -> Qd {
        self.round_with(f64::floor)
    }

    pub fn ceil(self) -> Qd {
        self.round_with(f64::ceil)
    }

    pub fn trunc(self) -> Qd {
        if self.is_sign_negative() {
            self.ceil()
        } else {
            self.floor()
        }
    }

    pub fn round(self) -> Qd {
        let t = self.trunc();
        let f = self - t;
        if f.0[0].abs() < 0.5 || f.abs() < Qd::from_f64(0.5) {
            t
        } else {
            t + Qd::from_f64(1.0f64.copysign(self.0[0]))
        }
    }

    fn round_with(self, f: fn(f64) -> f64) -> Qd {
        let mut x = [0.0; 4];
        for (r, &c) in x.iter_mut().zip(&self.0) {
            *r = f(c);
            if *r != c || !r.is_finite() {
                break;
            }
        }
        let r = if x[0].is_finite() {
            renorm(x)
        } else {
            Qd::from_f64(x[0])
        };
        // a zero result keeps the input's sign, as in IEEE
        if r.is_zero() {
            Qd::from_f64(0.0f64.copysign(self.0[0]))
        } else {
            r
        }
    }

    pub fn recip(self) -> Qd {
        Qd::ONE / self
    }

    pub fn sqrt(self) -> Qd {
        let a = self.0[0];
        if a == 0.0 || a == f64::INFINITY {
            return self;
        }
        if a.is_nan() || a < 0.0 {
            return Qd::NAN;
        }
        let h = exponent(a).div_euclid(2);
        let x = ldexp_qd(self, -2 * h);
        let mut r = Qd::from_f64(1.0 / x.0[0].sqrt());
        for _ in 0..2 {
            r += r * (Qd::ONE - x * r * r).scale(0.5);
        }
        let y = x * r;
        ldexp_qd(y + r * (x - y * y).scale(0.5), h)
    }

    pub fn powi(self, n: i64) -> Qd {
        let m = n.unsigned_abs();
        let a = self.0[0].abs();
        // past 2^10 squarings exp∘ln is more accurate, unless x is a power of two
        if m > 1024
            && a > 0.0
            && a.is_finite()
            && (self.0[1] != 0.0 || ldexp(a, -exponent(a)) != 0.5)
        {
            let r = (Qd::from_i64(n) * self.abs().ln()).exp();
            return if self.0[0] < 0.0 && m % 2 == 1 { -r } else { r };
        }
        let (p, e) = powu(self, m);
        let (p, e) = if n < 0 { (p.recip(), -e) } else { (p, e) };
        ldexp_qd(p, e.clamp(-1 << 20, 1 << 20) as i32)
    }

    pub fn pow(self, y: Qd) -> Qd {
        if y.is_zero() || self == Qd::ONE || (self == -Qd::ONE && y.is_infinite()) {
            return Qd::ONE;
        }
        if self.is_nan() || y.is_nan() {
            return Qd::NAN;
        }
        if self.is_zero() {
            return if y.0[0] > 0.0 { Qd::ZERO } else { Qd::INFINITY };
        }
        let int = y.floor() == y;
        if int && y.abs() < Qd::from_f64(4611686018427387904.0) {
            return self.powi(y.0.iter().map(|&c| c as i128).sum::<i128>() as i64);
        }
        if self.0[0] > 0.0 {
            return (y * self.ln()).exp();
        }
        if !int {
            return Qd::NAN;
        }
        // huge integral y: only parts below 2^53 can be odd
        let odd =
            y.0.iter()
                .filter(|c| c.abs() < 9007199254740992.0 && *c % 2.0 != 0.0)
                .count()
                % 2;
        let r = (y * (-self).ln()).exp();
        if odd == 1 { -r } else { r }
    }

    pub fn max(self, other: Qd) -> Qd {
        if self.is_nan() || other > self {
            other
        } else {
            self
        }
    }

    pub fn min(self, other: Qd) -> Qd {
        if self.is_nan() || other < self {
            other
        } else {
            self
        }
    }

    #[inline]
    fn scale(self, f: f64) -> Qd {
        Qd(self.0.map(|c| c * f))
    }
}

/// x^m by binary powering as (mantissa, binary exponent), so nothing overflows midway.
fn powu(x: Qd, mut m: u64) -> (Qd, i64) {
    let norm = |q: Qd, e: i64| {
        if !q.is_finite() || q.is_zero() {
            return (q, e);
        }
        let k = exponent(q.0[0]);
        (ldexp_qd(q, -k), (e + i64::from(k)).clamp(-1 << 40, 1 << 40))
    };
    let (mut acc, mut ae) = (Qd::ONE, 0);
    let (mut base, mut be) = norm(x, 0);
    while m > 0 {
        if m & 1 == 1 {
            (acc, ae) = norm(acc * base, ae + be);
        }
        m >>= 1;
        if m > 0 {
            (base, be) = norm(base * base, 2 * be);
        }
    }
    (acc, ae)
}

fn ldexp_qd(x: Qd, k: i32) -> Qd {
    let r = Qd(x.0.map(|c| ldexp(c, k)));
    if !r.0[0].is_finite() {
        Qd::from_f64(r.0[0])
    } else if r.0[0].abs() < 1e-290 {
        renorm(r.0)
    } else {
        r
    }
}

impl Qd {
    /// Parses `[+-]digits[.digits][(e|E)[+-]digits]`; `None` on bad syntax.
    pub fn parse(text: &str) -> Option<Qd> {
        let s = text.as_bytes();
        let neg = s.first() == Some(&b'-');
        let mut i = usize::from(matches!(s.first(), Some(b'+' | b'-')));
        let (mut mant, mut chunk, mut len) = (Qd::ZERO, 0.0, 0);
        let (mut sig, mut exp, mut digits, mut dot) = (0, 0i64, 0, false);
        while let Some(&b) = s.get(i) {
            if b == b'.' && !dot {
                dot = true;
            } else if b.is_ascii_digit() {
                digits += 1;
                if sig < 72 && (sig > 0 || b != b'0') {
                    sig += 1;
                    chunk = chunk * 10.0 + f64::from(b - b'0');
                    len += 1;
                    if len == 15 {
                        mant = mul_f64(mant, 1e15) + Qd::from_f64(chunk);
                        (chunk, len) = (0.0, 0);
                    }
                    exp -= i64::from(dot);
                } else if sig == 0 {
                    exp -= i64::from(dot);
                } else if !dot {
                    exp += 1;
                }
            } else {
                break;
            }
            i += 1;
        }
        if digits == 0 {
            return None;
        }
        if let Some(b'e' | b'E') = s.get(i) {
            i += 1;
            let eneg = s.get(i) == Some(&b'-');
            i += usize::from(matches!(s.get(i), Some(b'+' | b'-')));
            let start = i;
            let mut e = 0i64;
            while let Some(&b) = s.get(i).filter(|b| b.is_ascii_digit()) {
                e = (e * 10 + i64::from(b - b'0')).min(1 << 40);
                i += 1;
            }
            if i == start {
                return None;
            }
            exp += if eneg { -e } else { e };
        }
        if i != s.len() {
            return None;
        }
        let scale = (0..len).fold(1.0, |p, _| p * 10.0);
        mant = mul_f64(mant, scale) + Qd::from_f64(chunk);
        let v = match exp {
            _ if mant.is_zero() => Qd::ZERO,
            401.. => Qd::INFINITY,
            ..-500 => Qd::ZERO,
            _ => mul_pow10(mant, exp as i32),
        };
        Some(if neg { -v } else { v })
    }

    /// Correctly rounded digits: (negative, digits, e) with value = ±0.d1d2…dn × 10^e.
    pub fn decimal_digits(self, count: usize) -> Option<(bool, Vec<u8>, i32)> {
        if !self.is_finite() {
            return None;
        }
        let neg = self.is_sign_negative();
        let (mut d, low) = exact_decimal(self.abs());
        if d.is_empty() {
            return Some((neg, vec![0; count], 0));
        }
        let mut e = d.len() as i32 + low;
        if d.len() > count {
            // round half to even on the exact expansion
            let odd = count > 0 && d[count - 1] % 2 == 1;
            let up =
                d[count] > 5 || (d[count] == 5 && (odd || d[count + 1..].iter().any(|&x| x > 0)));
            d.truncate(count);
            if up {
                if let Some(p) = d.iter().rposition(|&x| x < 9) {
                    d[p] += 1;
                    d[p + 1..].fill(0);
                } else {
                    d.fill(0);
                    if let Some(f) = d.first_mut() {
                        *f = 1;
                    }
                    e += 1;
                }
            }
        }
        d.resize(count, 0);
        Some((neg, d, e))
    }
}

fn mul_pow10(x: Qd, k: i32) -> Qd {
    let ten = Qd::from_f64(10.0);
    match k {
        0.. => x * ten.powi(k.into()),
        -300.. => x / ten.powi((-k).into()),
        _ => mul_pow10(x / ten.powi(300), k + 300),
    }
}

fn exact_decimal(x: Qd) -> (Vec<u8>, i32) {
    const M: i128 = 0xffff_ffff;
    let parts: Vec<(i128, i32)> = (x.0.iter().filter(|c| **c != 0.0))
        .map(|&c| {
            let b = c.to_bits();
            let (be, f) = (((b >> 52) & 0x7ff) as i32, i128::from(b & ((1 << 52) - 1)));
            let (m, k) = if be == 0 {
                (f, -1074)
            } else {
                (f | 1 << 52, be - 1075)
            };
            (if c < 0.0 { -m } else { m }, k)
        })
        .collect();
    let lo = parts.iter().map(|p| p.1).min().unwrap_or(0);
    let hi = parts.iter().map(|p| p.1).max().unwrap_or(0);
    // signed base-2^32 limbs, carried afterwards
    let mut w = vec![0i64; ((hi - lo) / 32 + 4) as usize];
    for (m, k) in parts {
        let (q, v) = (((k - lo) / 32) as usize, m << ((k - lo) % 32));
        w[q] += (v & M) as i64;
        w[q + 1] += ((v >> 32) & M) as i64;
        w[q + 2] += (v >> 64) as i64;
    }
    let mut carry = 0;
    let mut n: Vec<u64> = (w.iter())
        .map(|&l| {
            let t = l + carry;
            carry = t >> 32;
            (t & 0xffff_ffff) as u64
        })
        .collect();
    // N·2^lo = N·5^-lo·10^lo
    let (base, step, mut left) = if lo < 0 { (5u64, 13, -lo) } else { (2, 31, lo) };
    while left > 0 {
        let f = base.pow(left.min(step) as u32);
        left -= left.min(step);
        let mut c = 0;
        for l in &mut n {
            let t = *l * f + c;
            (*l, c) = (t & 0xffff_ffff, t >> 32);
        }
        while c > 0 {
            n.push(c & 0xffff_ffff);
            c >>= 32;
        }
    }
    let mut chunks = Vec::new();
    while n.last() == Some(&0) {
        n.pop();
    }
    while !n.is_empty() {
        let mut r = 0;
        for l in n.iter_mut().rev() {
            let t = r << 32 | *l;
            (*l, r) = (t / 1_000_000_000, t % 1_000_000_000);
        }
        chunks.push(r);
        while n.last() == Some(&0) {
            n.pop();
        }
    }
    let mut d: Vec<u8> = (chunks.iter().rev())
        .flat_map(|&c| (0..9).rev().map(move |p| (c / 10u64.pow(p) % 10) as u8))
        .collect();
    d.drain(..d.iter().take_while(|&&x| x == 0).count());
    (d, lo.min(0))
}

impl Qd {
    pub fn exp(self) -> Qd {
        match self.0[0] {
            x if x.is_nan() => Qd::NAN,
            x if x > 709.8 => Qd::INFINITY,
            x if x < -745.14 => Qd::ZERO,
            _ => {
                let (s, k) = exp_parts(self);
                ldexp_qd(Qd::ONE + s, k)
            }
        }
    }

    pub fn ln(self) -> Qd {
        let a = self.0[0];
        if a.is_nan() || a < 0.0 {
            return Qd::NAN;
        }
        if a == 0.0 {
            return Qd::NEG_INFINITY;
        }
        if a == f64::INFINITY {
            return self;
        }
        // x = m·2^k with m in [1/√2, √2)
        let mut k = exponent(a) - 1;
        if ldexp(a, -k) > consts::SQRT_2 {
            k += 1;
        }
        let r = ln1p_core(ldexp_qd(self, -k) - Qd::ONE);
        if k == 0 {
            r
        } else {
            mul_f64(Qd::LN_2, f64::from(k)) + r
        }
    }

    pub fn sin(self) -> Qd {
        self.sin_cos().0
    }

    pub fn cos(self) -> Qd {
        self.sin_cos().1
    }

    pub fn sin_cos(self) -> (Qd, Qd) {
        if !self.is_finite() {
            return (Qd::NAN, Qd::NAN);
        }
        if self.is_zero() {
            return (self, Qd::ONE);
        }
        let (t, j) = reduce(self);
        let (s, c) = sin_cos_taylor(t);
        let k = j % 8;
        // add kπ/16, then a quarter turn per octant pair
        let (s, c) = if k == 0 {
            (s, c)
        } else {
            let (sk, ck) = (Qd(SIN16[k]), Qd(SIN16[8 - k]));
            (sk * c + ck * s, ck * c - sk * s)
        };
        match j / 8 {
            0 => (s, c),
            1 => (c, -s),
            2 => (-s, -c),
            _ => (-c, s),
        }
    }

    /// The angle in [−π, π] that is `self` less whole turns, reduced exactly however large it is.
    pub fn rem_tau(self) -> Qd {
        if !self.is_finite() {
            return Qd::NAN;
        }
        let (t, j) = reduce(self);
        let j = if j > 16 { j as i64 - 32 } else { j as i64 };
        t + Qd::PI.scale(1.0 / 16.0) * Qd::from_i64(j)
    }

    pub fn tan(self) -> Qd {
        let (s, c) = self.sin_cos();
        s / c
    }

    pub fn asin(self) -> Qd {
        let a = self.abs();
        if a.is_nan() || a > Qd::ONE {
            return Qd::NAN;
        }
        self.atan2(((Qd::ONE - a) * (Qd::ONE + a)).sqrt())
    }

    pub fn acos(self) -> Qd {
        if self.is_nan() || self.abs() > Qd::ONE {
            return Qd::NAN;
        }
        ((Qd::ONE - self) * (Qd::ONE + self)).sqrt().atan2(self)
    }

    pub fn atan(self) -> Qd {
        if self.is_nan() {
            return Qd::NAN;
        }
        let a = self.abs();
        let r = if a.0[0] > 1.0 {
            Qd::FRAC_PI_2 - atan_core(a.recip())
        } else {
            atan_core(a)
        };
        if self.is_sign_negative() { -r } else { r }
    }

    pub fn atan2(self, x: Qd) -> Qd {
        let y = self;
        if y.is_nan() || x.is_nan() {
            return Qd::NAN;
        }
        if y.is_infinite() || x.is_infinite() {
            // infinities become unit steps, finite values signed zeros
            let unit = |v: Qd| Qd::from_f64(f64::from(u8::from(v.is_infinite())).copysign(v.0[0]));
            return unit(y).atan2(unit(x));
        }
        let pi_y = if y.is_sign_negative() {
            -Qd::PI
        } else {
            Qd::PI
        };
        if y.is_zero() && x.is_zero() {
            return if x.is_sign_negative() { pi_y } else { y };
        }
        if y.abs() <= x.abs() {
            let z = (y / x).atan();
            if x.is_sign_negative() { z + pi_y } else { z }
        } else {
            pi_y.scale(0.5) - (x / y).atan()
        }
    }

    pub fn sinh(self) -> Qd {
        let a = self.abs();
        let r = match a.0[0] {
            v if v.is_nan() => return Qd::NAN,
            v if v > 800.0 => Qd::INFINITY,
            _ => match exp_parts(a) {
                (s, 0) => (s + s / (Qd::ONE + s)).scale(0.5),
                (s, k) => ldexp_qd(Qd::ONE + s, k - 1) - ldexp_qd((Qd::ONE + s).recip(), -k - 1),
            },
        };
        if self.is_sign_negative() { -r } else { r }
    }

    pub fn cosh(self) -> Qd {
        let a = self.abs();
        match a.0[0] {
            v if v.is_nan() => Qd::NAN,
            v if v > 800.0 => Qd::INFINITY,
            _ => {
                let (s, k) = exp_parts(a);
                ldexp_qd(Qd::ONE + s, k - 1) + ldexp_qd((Qd::ONE + s).recip(), -k - 1)
            }
        }
    }

    pub fn tanh(self) -> Qd {
        if self.is_nan() {
            return self;
        }
        if self.0[0].abs() > 80.0 {
            return Qd::from_f64(1.0f64.copysign(self.0[0]));
        }
        let s = expm1(self.scale(2.0));
        s / (s + Qd::from_f64(2.0))
    }

    pub fn asinh(self) -> Qd {
        let a = self.abs();
        let r = if a.0[0] > 1e34 {
            a.ln() + Qd::LN_2
        } else {
            let a2 = a * a;
            ln1p(a + a2 / (Qd::ONE + (Qd::ONE + a2).sqrt()))
        };
        if self.is_sign_negative() { -r } else { r }
    }

    pub fn acosh(self) -> Qd {
        if self.is_nan() || self < Qd::ONE {
            return Qd::NAN;
        }
        if self.0[0] > 1e34 {
            return self.ln() + Qd::LN_2;
        }
        let d = self - Qd::ONE;
        ln1p(d + (d * (d + Qd::from_f64(2.0))).sqrt())
    }

    pub fn atanh(self) -> Qd {
        let a = self.abs();
        if a.is_nan() || a > Qd::ONE {
            return Qd::NAN;
        }
        let r = if a == Qd::ONE {
            Qd::INFINITY
        } else {
            ln1p(a.scale(2.0) / (Qd::ONE - a)).scale(0.5)
        };
        if self.is_sign_negative() { -r } else { r }
    }
}

/// Splits e^x = (1 + s)·2^k for |x| ≲ 750, with s accurate relative to itself.
fn exp_parts(x: Qd) -> (Qd, i32) {
    let k = (x.0[0] / consts::LN_2).round();
    if k == 0.0 {
        return (expm1_core(x), 0);
    }
    // k·ln2 = p + e exactly; x − p cancels exactly
    let (p, e) = two_prod(k, LN2X[0]);
    let tail = mul_f64(Qd([LN2X[1], LN2X[2], LN2X[3], LN2X[4]]), k);
    let r = (x - Qd::from_f64(p)) - (tail + Qd::from_f64(e));
    (expm1_core(r), k as i32)
}

/// e^r − 1 for |r| ≲ 0.6 by Taylor series on r/2^m and m squarings of 1 + s.
fn expm1_core(r: Qd) -> Qd {
    if r.is_zero() {
        return r;
    }
    let m = (exponent(r.0[0]) + 10).max(0);
    let t = r.scale(pow2(-m));
    let a = t.0[0].abs();
    let (mut n, mut g) = (1, a / 2.0);
    while g > 6e-67 && n < 34 {
        n += 1;
        g *= a / (n + 1) as f64;
    }
    let mut p = Qd(INV_FACT[n]);
    for j in (1..n).rev() {
        p = Qd(INV_FACT[j]) + t * p;
    }
    let mut s = t * p;
    for _ in 0..m {
        s = s.scale(2.0) + s * s;
    }
    s
}

fn expm1(x: Qd) -> Qd {
    match exp_parts(x) {
        (s, 0) => s,
        (s, k) => ldexp_qd(Qd::ONE + s, k) - Qd::ONE,
    }
}

/// ln(1 + d) for |d| ≤ 0.42: a double guess y0, then ln(1 + δ) to fourth order.
fn ln1p_core(d: Qd) -> Qd {
    if d.is_zero() {
        return d;
    }
    let y0 = d.0[0].ln_1p();
    // δ = (1 + d)·e^-y0 − 1, formed without cancelling against 1
    let delta = d + expm1_core(Qd::from_f64(-y0)) * (Qd::ONE + d);
    let dl = delta.0[0];
    Qd::from_f64(y0) + delta - (delta * delta).scale(0.5)
        + Qd::from_f64(dl * dl * dl * (1.0 / 3.0 - dl / 4.0))
}

fn ln1p(u: Qd) -> Qd {
    if u.0[0].abs() <= 0.42 {
        ln1p_core(u)
    } else {
        (Qd::ONE + u).ln()
    }
}

/// atan(a) for 0 ≤ a ≤ 1 as atan(k/16) + atan of (a − k/16)/(1 + a·k/16).
fn atan_core(a: Qd) -> Qd {
    let k = (a.0[0] * 16.0).round();
    let v = if k == 0.0 {
        a
    } else {
        let c = k / 16.0;
        (a - Qd::from_f64(c)) / (Qd::ONE + mul_f64(a, c))
    };
    let w = v.0[0] * v.0[0];
    let (mut n, mut h) = (0, w);
    while h / (2 * n + 3) as f64 > 6e-67 && n < 21 {
        n += 1;
        h *= w;
    }
    let u = v * v;
    let mut p = Qd(INV_ODD[n]);
    for j in (0..n).rev() {
        p = Qd(INV_ODD[j]) - u * p;
    }
    let r = v * p;
    if k == 0.0 {
        r
    } else {
        Qd(ATAN16[k as usize]) + r
    }
}

/// Reduces x exactly (Payne–Hanek) to t with |t| ≤ π/32 and x ≡ t + j·π/16 (mod 2π).
fn reduce(x: Qd) -> (Qd, usize) {
    if x.0[0].abs() < 0.09 {
        return (x, 0);
    }
    // x/(2π) mod 1 as 512-bit fractions, positive and negative parts apart
    let mut acc = [[0u64; 8]; 2];
    for c in x.0 {
        let b = c.abs().to_bits();
        let (be, f) = ((b >> 52) as i32, b & ((1 << 52) - 1));
        let (m, e) = if be == 0 {
            (f, -1074)
        } else {
            (f | 1 << 52, be - 1075)
        };
        for (k, &l) in INV_2PI.iter().enumerate().skip((e.max(0) / 64) as usize) {
            let sh = e - 64 * (k as i32 + 1) + 512;
            if sh <= -117 {
                break;
            }
            let v = u128::from(m) * u128::from(l);
            add_shifted(
                &mut acc[usize::from(c < 0.0)],
                v.checked_shr(sh.min(0).unsigned_abs()).unwrap_or(0),
                sh.max(0),
            );
        }
    }
    let [pos, neg] = acc;
    let mut f = [0u64; 8];
    let mut borrow = false;
    for i in 0..8 {
        let (d, b1) = pos[i].overflowing_sub(neg[i]);
        let (d, b2) = d.overflowing_sub(u64::from(borrow));
        (f[i], borrow) = (d, b1 || b2);
    }
    // j = round(32·f); the rest, |r| ≤ 1/2, is r = ±g·2^-507
    let half = (f[7] >> 58) & 1;
    let j = ((f[7] >> 59) + half) as usize % 32;
    f[7] &= (1 << 59) - 1;
    if half == 1 {
        let mut borrow = false;
        for l in &mut f {
            let (d, b1) = 0u64.overflowing_sub(*l);
            let (d, b2) = d.overflowing_sub(u64::from(borrow));
            (*l, borrow) = (d, b1 || b2);
        }
        f[7] &= (1 << 59) - 1;
    }
    let top = f.iter().rposition(|&l| l != 0).unwrap_or(0);
    let mut parts = [0.0; 10];
    for (i, p) in parts.iter_mut().enumerate() {
        // 32-bit pieces from the top, each exact as a double
        let pos = 64 * top as i32 + 32 - 32 * i as i32;
        if pos >= 0 {
            *p = ldexp(
                ((f[pos as usize / 64] >> (pos % 64)) & 0xffff_ffff) as f64,
                pos - 507,
            );
        }
    }
    let r = renorm(parts) * Qd::PI.scale(1.0 / 16.0);
    (if half == 1 { -r } else { r }, j)
}

fn add_shifted(p: &mut [u64; 8], v: u128, sh: i32) {
    let (li, bit) = ((sh / 64) as usize, sh % 64);
    let w = [v as u64, (v >> 64) as u64];
    let s = match bit {
        0 => [w[0], w[1], 0],
        _ => [
            w[0] << bit,
            w[1] << bit | w[0] >> (64 - bit),
            w[1] >> (64 - bit),
        ],
    };
    let mut carry = 0;
    for (i, l) in p.iter_mut().enumerate().skip(li) {
        let t = u128::from(*l) + u128::from(*s.get(i - li).unwrap_or(&0)) + carry;
        (*l, carry) = (t as u64, t >> 64);
        if carry == 0 && i >= li + 2 {
            break;
        }
    }
}

/// (sin t, cos t) for |t| ≤ π/32 by Taylor series in t².
fn sin_cos_taylor(t: Qd) -> (Qd, Qd) {
    let a2 = t.0[0] * t.0[0];
    let (mut n, mut g) = (0, a2 / 2.0);
    while g > 6e-67 && n < 16 {
        n += 1;
        g *= a2 / ((2 * n + 1) * (2 * n + 2)) as f64;
    }
    let u = t * t;
    let (mut s, mut c) = (Qd(INV_FACT[2 * n + 1]), Qd(INV_FACT[2 * n]));
    for j in (0..n).rev() {
        s = Qd(INV_FACT[2 * j + 1]) - u * s;
        c = Qd(INV_FACT[2 * j]) - u * c;
    }
    (t * s, c)
}

/// Reciprocal factorials 1/k!.
#[rustfmt::skip]
const INV_FACT: [[f64; 4]; 35] = [
    [1.0, 0.0, 0.0, 0.0],
    [1.0, 0.0, 0.0, 0.0],
    [0.5, 0.0, 0.0, 0.0],
    [0.16666666666666666, 9.25185853854297e-18, 5.135813185032629e-34, 2.850949024098342e-50],
    [0.041666666666666664, 2.3129646346357427e-18, 1.2839532962581572e-34, 7.127372560245855e-51],
    [0.008333333333333333, 1.1564823173178714e-19, 1.6049416203226965e-36, 2.2273039250768297e-53],
    [0.001388888888888889, -5.300543954373577e-20, -1.7386867553495878e-36, -1.6333562117230084e-52],
    [0.0001984126984126984, 1.7209558293420705e-22, 1.4926912391394127e-40, 1.2947032674600247e-58],
    [2.48015873015873e-05, 2.1511947866775882e-23, 1.865864048924266e-41, 1.6183790843250309e-59],
    [2.7557319223985893e-06, -1.858393274046472e-22, 8.491754604881993e-39, -5.726616407894296e-55],
    [2.755731922398589e-07, 2.3767714622250297e-23, -3.263188903340883e-40, 1.6143511186040442e-56],
    [2.505210838544172e-08, -1.448814070935912e-24, 2.0426735146714455e-41, -8.496326720071632e-58],
    [2.08767569878681e-09, -1.20734505911326e-25, 1.702227928892871e-42, 1.416095321503967e-58],
    [1.6059043836821613e-10, 1.2585294588752098e-26, -5.31334602762985e-43, 3.5402147259760553e-59],
    [1.1470745597729725e-11, 2.0655512752830745e-28, 6.889079232466646e-45, 5.729200026551091e-61],
    [7.647163731819816e-13, 7.03872877733453e-30, -7.827539277162583e-48, 1.9213864944379024e-64],
    [4.779477332387385e-14, 4.399205485834081e-31, -4.892212048226615e-49, 1.200866559023689e-65],
    [2.8114572543455206e-15, 1.6508842730861433e-31, -2.877771793074479e-50, 4.2711068925629355e-67],
    [1.5619206968586225e-16, 1.1910679660273754e-32, -4.577506059629983e-49, 2.874941423408996e-67],
    [8.22063524662433e-18, 2.2141894119604265e-34, -1.508914023774199e-50, 1.4007295151478155e-67],
    [4.110317623312165e-19, 1.4412973378659527e-36, -5.285627548789812e-53, -4.147647256357657e-70],
    [1.9572941063391263e-20, -1.3643503830087908e-36, 1.3392348251125064e-53, -6.821089424149331e-70],
    [8.896791392450574e-22, -7.911402614872376e-38, -3.1877976790570933e-54, 1.2705781017520566e-70],
    [3.868170170630684e-23, -8.843177655482344e-40, 3.8718157106173247e-56, -1.9565257531522557e-72],
    [1.6117375710961184e-24, -3.6846573564509766e-41, 1.613256546090552e-57, -8.1521906381344e-74],
    [6.446950284384474e-26, -1.9330404233703465e-42, -1.5213023807039144e-58, 6.643772737212958e-75],
    [2.4795962632247976e-27, -1.2953730964765229e-43, 6.403390159849962e-60, -8.460245627706746e-77],
    [9.183689863795546e-29, 1.4303150396787322e-45, -8.551226774650505e-62, 8.381467100234538e-78],
    [3.279889237069838e-30, 1.5117542744029879e-46, 8.058517719519716e-63, -9.096480530710929e-81],
    [1.1309962886447716e-31, 1.0498015412959506e-47, -4.346150929397795e-64, -4.966779800140056e-81],
    [3.7699876288159054e-33, 2.5870347832750324e-49, 3.23789002742564e-66, 2.5612859105788573e-82],
    [1.216125041553518e-34, 5.586290567888806e-51, 6.615948578082792e-68, -3.162044228952086e-84],
    [3.8003907548547434e-36, 1.7457158024652518e-52, 2.0674839306508725e-69, -9.881388215475268e-86],
    [1.151633562077195e-37, -6.09957445788454e-54, -5.3447496196594105e-70, 2.625312623850008e-86],
    [3.387157535521162e-39, 5.09056148151085e-56, 3.989567349036344e-72, -1.1495129447909262e-88],
];

/// Reciprocal odd numbers 1/(2n+1).
#[rustfmt::skip]
const INV_ODD: [[f64; 4]; 22] = [
    [1.0, 0.0, 0.0, 0.0],
    [0.3333333333333333, 1.850371707708594e-17, 1.0271626370065257e-33, 5.701898048196684e-50],
    [0.2, -1.1102230246251566e-17, 6.162975822039155e-34, -3.4211388289180106e-50],
    [0.14285714285714285, 7.93016446160826e-18, 4.4021255871708246e-34, 2.443670592084293e-50],
    [0.1111111111111111, 6.1679056923619804e-18, 3.423875456688419e-34, 1.900632682732228e-50],
    [0.09090909090909091, -2.523234146875356e-18, 7.003381615953585e-35, -1.9438288800670514e-51],
    [0.07692307692307693, -4.270088556250602e-18, 2.370375316168906e-34, -1.3158226265069272e-50],
    [0.06666666666666667, 9.251858538542971e-19, 1.2839532962581572e-35, 1.7818431400614637e-52],
    [0.058823529411764705, 8.163404592832033e-19, 1.1328999672866093e-35, 1.5722145353483504e-52],
    [0.05263157894736842, 2.921639538487254e-18, 1.6218357426418827e-34, 9.00299691820529e-51],
    [0.047619047619047616, 2.64338815386942e-18, 1.4673751957236082e-34, 8.145568640280977e-51],
    [0.043478260869565216, 1.206764157201257e-18, 3.349443381543019e-35, 9.296572904668506e-52],
    [0.04, -8.326672684688674e-19, -3.0814879110195774e-35, 6.414635304221269e-52],
    [0.037037037037037035, 2.05596856412066e-18, 1.1412918188961397e-34, 6.335442275774093e-51],
    [0.034482758620689655, 4.785444071660157e-19, 6.64113773926633e-36, 9.216430034800675e-53],
    [0.03225806451612903, 8.953411488912552e-19, 2.48507089598353e-35, 6.897457316366956e-52],
    [0.030303030303030304, -8.410780489584519e-19, 2.334460538651195e-35, -6.479429600223505e-52],
    [0.02857142857142857, 8.921435019309293e-19, 8.804251174341649e-35, 2.7491294160948295e-51],
    [0.02702702702702703, -1.50030138462859e-18, 8.328345705458318e-35, -4.623160579618933e-51],
    [0.02564102564102564, 8.896017825522087e-19, 7.901251053896352e-35, 2.741297138556098e-51],
    [0.024390243902439025, -8.46206573647223e-19, -1.8789560433046204e-35, 6.518938317298038e-52],
    [0.023255813953488372, 3.2273925134452225e-19, 4.4789068474121763e-36, 6.215731883935339e-53],
];

/// sin(kπ/16).
#[rustfmt::skip]
const SIN16: [[f64; 4]; 9] = [
    [0.0, 0.0, 0.0, 0.0],
    [0.19509032201612828, -7.991079068461731e-18, 6.184627002422071e-34, -3.5840270918032937e-50],
    [0.3826834323650898, -1.0050772696461588e-17, -2.0605316302806695e-34, -1.2717724698085205e-50],
    [0.5555702330196022, 4.709410940561677e-17, -2.064052038368292e-33, 1.2290163188567138e-49],
    [consts::FRAC_1_SQRT_2, -4.833646656726457e-17, 2.0693376543497068e-33, 2.4677734957341755e-50],
    [0.8314696123025452, 1.4073856984728024e-18, 4.6951315383980835e-35, -2.023388151938257e-52],
    [0.9238795325112867, 1.7645047084336677e-17, -5.044253732158682e-34, -4.047867771682389e-50],
    [0.9807852804032304, 1.8546939997825006e-17, -1.0696564445530757e-33, 6.666817447526496e-50],
    [1.0, 0.0, 0.0, 0.0],
];

/// atan(k/16).
#[rustfmt::skip]
const ATAN16: [[f64; 4]; 17] = [
    [0.0, 0.0, 0.0, 0.0],
    [0.06241880999595735, -1.5490756308295046e-18, -2.3447954298848344e-35, -1.0305422006694132e-51],
    [0.12435499454676144, -3.1253241424539383e-18, -1.7914844536654056e-34, 9.890858390188382e-51],
    [0.18534794999569476, 4.180692268843079e-18, -1.7067621314286706e-34, 2.365210468236154e-51],
    [0.24497866312686414, 1.0698755618734451e-17, 1.0079104836654304e-34, 5.814081148887967e-51],
    [0.3028848683749714, -1.1010827903001369e-17, -4.863137182713637e-34, 1.4947311025858167e-50],
    [0.35877067027057225, -2.4623815582638635e-17, -1.6682139707747893e-34, -2.2325146941511067e-51],
    [0.4124104415973873, -1.587652227770689e-17, -1.5000714146959223e-34, 1.8461561662572777e-51],
    [0.4636476090008061, 2.2698777452961687e-17, -5.247356382839165e-34, -3.4595227018992464e-50],
    [0.5123894603107377, -2.5462781472855804e-17, 9.793306210593216e-34, -2.3241840233763395e-50],
    [0.5585993153435624, -5.4556305485916264e-18, 4.1587722120912616e-35, -2.2215658802237217e-51],
    [0.6022873461349642, 2.950430737228402e-17, 3.0722627931262134e-33, 6.223172109267152e-50],
    [0.6435011087932844, 1.5834785051444286e-17, -4.479136282913368e-34, 3.9284694358202935e-50],
    [0.6823165548747481, 6.943223671560008e-18, 3.904816305754126e-34, 1.0523935536077366e-50],
    [0.7188299996216245, -2.1478388444456983e-17, 8.217094605489785e-34, 5.553925224119375e-50],
    [0.7531512809621944, -2.4256934659182068e-17, 5.733733310288812e-34, 3.996842632016255e-50],
    [consts::FRAC_PI_4, 3.061616997868383e-17, -7.486924524295849e-34, 2.781135552158413e-50],
];

/// Bits of 1/(2π) after the binary point, 64 per limb, most significant first.
#[rustfmt::skip]
const INV_2PI: [u64; 27] = [
    0x28be60db9391054a, 0x7f09d5f47d4d3770, 0x36d8a5664f10e410,
    0x7f9458eaf7aef158, 0x6dc91b8e909374b8, 0x01924bba82746487,
    0x3f877ac72c4a69cf, 0xba208d7d4baed121, 0x3a671c09ad17df90,
    0x4e64758e60d4ce7d, 0x272117e2ef7e4a0e, 0xc7fe25fff7816603,
    0xfbcbc462d6829b47, 0xdb4d9fb3c9f2c26d, 0xd3d18fd9a797fa8b,
    0x5d49eeb1faf97c5e, 0xcf41ce7de294a4ba, 0x9afed7ec47e35742,
    0x1580cc11bf1edaea, 0xfc33ef0826bd0d87, 0x6a78e45857b986c2,
    0x19666157c5281a10, 0x237ff620135cc9cc, 0x41818555b29cea32,
    0x58389ef0231ad1f1, 0x0670d9f3773a024a, 0xa0d6711da2e58729,
];

/// ln 2 to about 80 digits, for argument reduction.
#[rustfmt::skip]
const LN2X: [f64; 5] = [consts::LN_2, 2.3190468138462996e-17, 5.707708438416212e-34, -3.5824322106018114e-50, -1.352169675798863e-66];

#[cfg(test)]
mod tests {
    use super::Qd;

    fn digits(x: Qd, n: usize) -> String {
        let (neg, d, e) = x.decimal_digits(n).unwrap();
        let s: String = d.iter().map(|&c| char::from(b'0' + c)).collect();
        format!("{}0.{s}e{e}", if neg { "-" } else { "" })
    }

    fn close(a: Qd, b: Qd, tol: f64) -> bool {
        ((a - b).abs() / b.abs().max(Qd::ONE)).to_f64() < tol
    }

    #[test]
    fn constants() {
        let pi = "0.3141592653589793238462643383279502884197169399375105820974944592e1";
        let e = "0.2718281828459045235360287471352662497757247093699959574966967628e1";
        let ln2 = "0.6931471805599453094172321214581765680755001343602552541206800095e0";
        assert_eq!(digits(Qd::PI, 64), pi);
        assert_eq!(digits(Qd::E, 64), e);
        assert_eq!(digits(Qd::LN_2, 64), ln2);
        assert_eq!(digits(Qd::ONE.exp(), 64), e);
        assert_eq!(digits(Qd::from_f64(2.0).ln(), 64), ln2);
        assert_eq!(digits(Qd::ONE.atan().scale(4.0), 64), pi);
    }

    #[test]
    fn identities() {
        let half = Qd::from_f64(0.5);
        // π − Qd::PI is the fifth term of π's expansion
        assert!(close(
            Qd::PI.sin(),
            Qd::from_f64(5.672231979640316e-66),
            1e-15
        ));
        assert!(close((Qd::PI / Qd::from_f64(6.0)).sin(), half, 1e-63));
        let three = Qd::from_f64(3.0);
        assert!(close(three.ln().exp(), three, 1e-62));
        let two = Qd::from_f64(2.0);
        assert!(close(two.sqrt() * two.sqrt(), two, 1e-63));
        assert_eq!(Qd::from_i64(i64::MIN).to_f64(), i64::MIN as f64);
        assert_eq!(
            Qd::from_i64(i64::MAX - 1) - Qd::from_i64(i64::MAX),
            -Qd::ONE
        );
    }

    #[test]
    fn round_trip() {
        let t = "-0.1234567890123456789012345678901234567890123456789012345678901234e-17";
        let x = Qd::parse("-1.234567890123456789012345678901234567890123456789012345678901234e-18")
            .unwrap();
        assert_eq!(digits(x, 64), t);
        assert_eq!(digits(Qd::parse("0.1").unwrap(), 3), "0.100e0");
        assert_eq!(digits(Qd::ZERO, 2), "0.00e0");
        assert!(
            Qd::parse("1e").is_none() && Qd::parse(".").is_none() && Qd::parse("1.2.3").is_none()
        );
        assert!(Qd::NAN.decimal_digits(5).is_none());
    }

    #[test]
    fn specials() {
        assert!(Qd::from_f64(-1.0).sqrt().is_nan() && Qd::from_f64(-1.0).ln().is_nan());
        assert_eq!(Qd::ZERO.ln(), Qd::NEG_INFINITY);
        assert_eq!(Qd::from_f64(1e5).exp(), Qd::INFINITY);
        assert!(Qd::NAN != Qd::NAN && Qd::NAN.partial_cmp(&Qd::ONE).is_none());
        assert_eq!(Qd::ZERO.pow(Qd::from_f64(-2.0)), Qd::INFINITY);
        assert!(Qd::from_f64(-2.0).pow(Qd::from_f64(0.5)).is_nan());
        assert_eq!(
            Qd::from_f64(-2.0).pow(Qd::from_f64(3.0)),
            Qd::from_f64(-8.0)
        );
        assert!(
            Qd::from_f64(-0.3).ceil().is_sign_negative() && Qd::from_f64(-0.4).round().is_zero()
        );
        let big = "-0.8178819121159085970458852827554262120114283038903840464637395861e0";
        assert_eq!(digits(Qd::from_f64(1e300).sin(), 64), big);
        let c = "-0.7935061076585659671815224004938294833533104768457916822885399094e0";
        assert_eq!(digits(Qd::from_f64(476280.5).cos(), 64), c);
    }
}
