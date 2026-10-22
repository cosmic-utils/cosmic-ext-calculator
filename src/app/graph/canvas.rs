use super::Message;
use super::numeric::Points;
use crate::app;
use cosmic::Theme;
use cosmic::iced::widget::canvas::{self, Action, Event, Frame, Geometry, Path, Stroke, Text};
use cosmic::iced::{Color, Pixels, Point, Rectangle, Renderer, Size, alignment, mouse};
use std::ops::RangeInclusive;

pub struct Graph {
    /// Visible x range; the y range follows from it, at the same scale on both axes.
    pub range: RangeInclusive<f64>,
    /// y at the middle of the view; `None` puts it where the curve is.
    pub y_center: Option<f64>,
    /// x range of the latest sampling: wider than the view, so a pan has curve to show.
    pub sampled: RangeInclusive<f64>,
    pub sampling: String,
    /// The function the points belong to, which is not always what the field says.
    pub expression: String,
    pub points: Points,
    /// Pixels of a view width: a sample every one or two of them.
    pub samples: usize,
    /// Id of the plot being sampled, so only one request runs at a time.
    pub pending: Option<u64>,
    pub progressive: bool,
    /// Whether the latest sampling got to its end: cut short, it is taken up again.
    pub complete: bool,
}

#[derive(Debug, Clone)]
pub struct Plot {
    pub expression: String,
    pub points: Points,
    /// Asked for by the user, as opposed to taken again after a zoom or a pan.
    pub requested: bool,
    pub done: bool,
}

/// Drag in progress: its start and the view then, so a pan is computed from the anchor.
#[derive(Default)]
pub struct State {
    dragging: Option<(Point, RangeInclusive<f64>, f64)>,
}

impl Graph {
    pub const DEFAULT_RANGE: RangeInclusive<f64> = -10.0..=10.0;

    pub fn new() -> Self {
        Self {
            range: Self::DEFAULT_RANGE,
            y_center: None,
            sampled: Self::DEFAULT_RANGE,
            sampling: String::new(),
            expression: String::new(),
            points: Vec::new(),
            samples: 800,
            pending: None,
            progressive: false,
            complete: false,
        }
    }

    pub fn reset(&mut self) {
        let samples = self.samples;
        *self = Self::new();
        self.samples = samples;
    }

    pub fn resize(&mut self, width: f32) {
        self.samples = (width as usize).clamp(400, 2000);
    }

    pub fn reset_view(&mut self) {
        self.range = Self::DEFAULT_RANGE;
        self.y_center = None;
    }

    /// A view width more on each side, at a power-of-two spacing: pans and zooms reuse the points.
    pub fn sampling(&self, view: &RangeInclusive<f64>) -> (RangeInclusive<f64>, usize) {
        let (start, end) = (*view.start(), *view.end());
        let width = end - start;
        let (from, to) = (start - width, end + width);
        // No finer than the f64s that far out can tell apart.
        let finest = from.abs().max(to.abs()) * f64::EPSILON;
        let pixel = (width / (self.samples - 1) as f64).max(finest);
        let spacing = 2f64.powi(pixel.log2().ceil() as i32);
        let (first, last) = ((from / spacing).floor(), (to / spacing).ceil());
        if !(spacing > 0.0 && first.is_finite() && last.is_finite() && last > first) {
            return (from..=to, 3 * (self.samples - 1) + 1);
        }
        (
            (first * spacing)..=(last * spacing),
            (last - first) as usize + 1,
        )
    }

    pub fn is_stale(&self) -> bool {
        let (start, end) = (*self.sampled.start(), *self.sampled.end());
        let (first, last) = (*self.range.start(), *self.range.end());
        // The view width when they were taken, which sat in the middle third.
        let taken = (end - start) / 3.0;
        let scale = (last - first) / taken;
        if !(taken > 0.0 && taken.is_finite() && scale > 0.0 && scale.is_finite()) {
            return false;
        }
        first < start + taken / 2.0 || last > end - taken / 2.0 || !(0.67..=1.5).contains(&scale)
    }

    pub fn needs_sampling(&self) -> bool {
        // A native plot can take minutes, so the view it no longer serves supersedes it.
        let free = self.pending.is_none() || self.progressive;
        let cut = !self.complete && self.pending.is_none();
        !self.points.is_empty()
            && self.sampling == self.expression
            && free
            && (self.is_stale() || cut)
    }

    fn visible(&self) -> Vec<f64> {
        self.points
            .iter()
            .filter(|(x, _)| self.range.contains(x))
            .filter_map(|(_, y)| *y)
            .collect()
    }

    fn height(&self, size: Size) -> f64 {
        let aspect = if size.width > 0.0 {
            f64::from(size.height / size.width)
        } else {
            1.0
        };
        (self.range.end() - self.range.start()) * aspect
    }

    fn center(&self, size: Size) -> f64 {
        self.y_center
            .unwrap_or_else(|| centered(&self.visible(), self.height(size)))
    }

    fn y_range(&self, size: Size) -> RangeInclusive<f64> {
        let (center, half) = (self.center(size), self.height(size) / 2.0);
        (center - half)..=(center + half)
    }

    fn to_graph(&self, bounds: Rectangle, point: Point) -> (f64, f64) {
        Self::map(&self.range, &self.y_range(bounds.size()), bounds, point)
    }

    fn map(
        range: &RangeInclusive<f64>,
        y_range: &RangeInclusive<f64>,
        bounds: Rectangle,
        point: Point,
    ) -> (f64, f64) {
        let fx = f64::from(point.x / bounds.width);
        let fy = f64::from(point.y / bounds.height);
        (
            range.start() + fx * (range.end() - range.start()),
            y_range.end() - fy * (y_range.end() - y_range.start()),
        )
    }

    fn sample_at(&self, x: f64) -> Option<(f64, f64)> {
        let (first, last) = (self.points.first()?.0, self.points.last()?.0);
        if !(first..=last).contains(&x) {
            return None;
        }
        let next = self.points.partition_point(|(at, _)| *at < x);
        let (at, y) = [next.saturating_sub(1), next]
            .into_iter()
            .filter_map(|i| self.points.get(i))
            .min_by(|a, b| (a.0 - x).abs().total_cmp(&(b.0 - x).abs()))?;
        Some((*at, (*y)?))
    }

    fn render(&self, frame: &mut Frame, theme: &Theme, cursor: Option<Point>) {
        let size = frame.size();
        let cosmic = theme.cosmic();
        let axis = Stroke::default()
            .with_color(cosmic.on_bg_color().into())
            .with_width(1.0);
        let grid = Stroke::default()
            .with_color(cosmic.bg_divider().into())
            .with_width(1.0);
        let curve = Stroke::default()
            .with_color(cosmic.accent_color().into())
            .with_width(2.0);

        let (x0, x1) = (*self.range.start(), *self.range.end());
        let y_range = self.y_range(size);
        let (y0, y1) = (*y_range.start(), *y_range.end());
        // Points far outside the view (poles) are clamped so they cannot overflow the coordinates.
        let pixel = |value: f64| {
            if value.is_nan() {
                0.0
            } else {
                value.clamp(-1e6, 1e6) as f32
            }
        };
        let to_screen = |x: f64, y: f64| {
            Point::new(
                pixel((x - x0) / (x1 - x0) * f64::from(size.width)),
                pixel((y1 - y) / (y1 - y0) * f64::from(size.height)),
            )
        };

        let label_color = cosmic.on_bg_color().into();
        let step = grid_step(x1 - x0);
        let places = decimals(step);
        for x in ticks(x0, x1, step) {
            let top = to_screen(x, y1);
            frame.stroke(&Path::line(top, to_screen(x, y0)), grid);
            frame.fill_text(Text {
                content: format_tick(x, places),
                position: Point::new(top.x + 2.0, size.height - 2.0),
                color: label_color,
                size: Pixels(10.0),
                align_y: alignment::Vertical::Bottom,
                ..Text::default()
            });
        }
        for y in ticks(y0, y1, step) {
            let left = to_screen(x0, y);
            frame.stroke(&Path::line(left, to_screen(x1, y)), grid);
            frame.fill_text(Text {
                content: format_tick(y, places),
                position: Point::new(2.0, left.y - 2.0),
                color: label_color,
                size: Pixels(10.0),
                align_y: alignment::Vertical::Bottom,
                ..Text::default()
            });
        }

        if y0 <= 0.0 && 0.0 <= y1 {
            frame.stroke(&Path::line(to_screen(x0, 0.0), to_screen(x1, 0.0)), axis);
        }
        if x0 <= 0.0 && 0.0 <= x1 {
            frame.stroke(&Path::line(to_screen(0.0, y0), to_screen(0.0, y1)), axis);
        }

        // Broken wherever a point is undefined, and across poles.
        let beyond = |y: f64| {
            if y > 0.0 {
                y1 + (y1 - y0)
            } else {
                y0 - (y1 - y0)
            }
        };
        let path = Path::new(|builder| {
            let mut previous: Option<(f64, f64)> = None;
            for &(x, value) in &self.points {
                let Some(y) = value else {
                    previous = None;
                    continue;
                };
                let point = to_screen(x, y);
                match previous {
                    Some((before_x, before)) if is_pole(before, y, y1 - y0) => {
                        builder.line_to(to_screen(before_x, beyond(before)));
                        builder.move_to(to_screen(x, beyond(y)));
                        builder.line_to(point);
                    }
                    Some(_) => builder.line_to(point),
                    None => builder.move_to(point),
                }
                previous = Some((x, y));
            }
        });
        frame.stroke(&path, curve);

        let Some(cursor) = cursor else {
            return;
        };
        let (x, _) = Self::map(&self.range, &y_range, Rectangle::with_size(size), cursor);
        let Some((sx, sy)) = self.sample_at(x) else {
            return;
        };
        let marker = to_screen(sx, sy);
        frame.stroke(
            &Path::line(Point::new(marker.x, 0.0), Point::new(marker.x, size.height)),
            grid,
        );
        frame.fill(
            &Path::circle(marker, 4.0),
            Color::from(cosmic.accent_color()),
        );
        let label = format!("x = {}   y = {}", format_value(sx), format_value(sy));
        let width = label.chars().count() as f32 * 6.5;
        let on_left = marker.x + 8.0 + width > size.width && marker.x - 8.0 - width >= 0.0;
        frame.fill_text(Text {
            content: label,
            position: Point::new(
                if on_left {
                    marker.x - 8.0
                } else {
                    marker.x + 8.0
                },
                (marker.y - 8.0).max(14.0).min(size.height - 2.0),
            ),
            color: label_color,
            size: Pixels(12.0),
            align_x: if on_left {
                alignment::Horizontal::Right.into()
            } else {
                alignment::Horizontal::Left.into()
            },
            align_y: alignment::Vertical::Bottom,
            ..Text::default()
        });
    }
}

impl canvas::Program<app::Message, Theme> for Graph {
    type State = State;

    fn update(
        &self,
        state: &mut State,
        event: &Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<Action<app::Message>> {
        let Event::Mouse(event) = event else {
            return None;
        };
        match event {
            mouse::Event::WheelScrolled { delta } => {
                let position = cursor.position_in(bounds)?;
                let lines = match delta {
                    mouse::ScrollDelta::Lines { y, .. } => *y,
                    mouse::ScrollDelta::Pixels { y, .. } => y / 40.0,
                };
                let factor = f64::from(0.85f32.powf(lines));
                let at = self.to_graph(bounds, position);
                let (range, center) = zoomed(&self.range, self.center(bounds.size()), at, factor);
                Some(Action::publish(Message::View(range, center).into()).and_capture())
            }
            mouse::Event::ButtonPressed(mouse::Button::Left) => {
                let start = cursor.position_in(bounds)?;
                state.dragging = Some((start, self.range.clone(), self.center(bounds.size())));
                Some(Action::capture())
            }
            mouse::Event::ButtonReleased(mouse::Button::Left) => {
                state.dragging.take().map(|_| Action::capture())
            }
            mouse::Event::CursorMoved { .. } => {
                let Some((start, range, center)) = &state.dragging else {
                    return Some(Action::request_redraw());
                };
                let to = cursor.position()?;
                let by = (to.x - bounds.x - start.x, to.y - bounds.y - start.y);
                let (range, center) = panned(range, *center, by, bounds.width)?;
                Some(Action::publish(Message::View(range, center).into()).and_capture())
            }
            _ => None,
        }
    }

    fn draw(
        &self,
        _state: &State,
        renderer: &Renderer,
        theme: &Theme,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let mut frame = Frame::new(renderer, bounds.size());
        self.render(&mut frame, theme, cursor.position_in(bounds));
        vec![frame.into_geometry()]
    }

    fn mouse_interaction(
        &self,
        state: &State,
        _bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        if state.dragging.is_some() {
            mouse::Interaction::Grabbing
        } else {
            mouse::Interaction::Grab
        }
    }
}

fn zoomed(
    range: &RangeInclusive<f64>,
    center: f64,
    (x, y): (f64, f64),
    factor: f64,
) -> (RangeInclusive<f64>, f64) {
    (
        (x - (x - range.start()) * factor)..=(x + (range.end() - x) * factor),
        y + (center - y) * factor,
    )
}

fn panned(
    range: &RangeInclusive<f64>,
    center: f64,
    by: (f32, f32),
    width: f32,
) -> Option<(RangeInclusive<f64>, f64)> {
    if width <= 0.0 {
        return None;
    }
    let per_pixel = (range.end() - range.start()) / f64::from(width);
    let (dx, dy) = (-f64::from(by.0) * per_pixel, f64::from(by.1) * per_pixel);
    Some(((range.start() + dx)..=(range.end() + dx), center + dy))
}

/// Where to center a view of `height` on a curve: the axis, unless too little of it shows there.
fn centered(values: &[f64], height: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let on_axis = values.iter().filter(|v| v.abs() <= height / 2.0).count();
    if on_axis * 10 >= values.len() {
        return 0.0;
    }
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    sorted[sorted.len() / 2]
}

/// Whether two samples straddle a pole: opposite signs, further apart than the view is high.
fn is_pole(before: f64, after: f64, height: f64) -> bool {
    before.is_sign_negative() != after.is_sign_negative() && (after - before).abs() > height
}

fn grid_step(span: f64) -> f64 {
    let raw = span / 8.0;
    if !(raw > 0.0 && raw.is_finite()) {
        return 0.0;
    }
    let magnitude = 10f64.powf(raw.log10().floor());
    [1.0, 2.0, 5.0, 10.0]
        .into_iter()
        .map(|m| m * magnitude)
        .find(|&s| s >= raw)
        .unwrap_or(magnitude)
}

fn ticks(lo: f64, hi: f64, step: f64) -> Vec<f64> {
    if !(step > 0.0 && step.is_finite()) {
        return Vec::new();
    }
    let first = (lo / step).ceil() as i64;
    let last = (hi / step).floor() as i64;
    // A canvas that thin would draw a wall of lines.
    if last.saturating_sub(first) > 200 {
        return Vec::new();
    }
    (first..=last).map(|i| i as f64 * step).collect()
}

fn decimals(step: f64) -> usize {
    (-step.log10().floor()).clamp(0.0, 12.0) as usize
}

fn format_value(value: f64) -> String {
    if value != 0.0 && (value.abs() >= 1e9 || value.abs() < 1e-6) {
        return scientific(value);
    }
    let text = format!("{value:.6}");
    let text = text.trim_end_matches('0').trim_end_matches('.');
    if text == "-0" {
        "0".to_string()
    } else {
        text.to_string()
    }
}

fn format_tick(value: f64, decimals: usize) -> String {
    if value == 0.0 {
        return "0".to_string();
    }
    if value.abs() >= 1e7 || decimals > 6 {
        return scientific(value);
    }
    let text = format!("{value:.decimals$}");
    if text.contains('.') {
        text.trim_end_matches('0').trim_end_matches('.').to_string()
    } else {
        text
    }
}

fn scientific(value: f64) -> String {
    let text = format!("{value:.5e}");
    let (mantissa, exponent) = text.split_once('e').unwrap_or((&text, "0"));
    let mantissa = mantissa.trim_end_matches('0').trim_end_matches('.');
    format!("{mantissa}e{exponent}")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn graph(f: impl Fn(f64) -> f64) -> Graph {
        let mut graph = Graph::new();
        let (range, count) = graph.sampling(&graph.range);
        graph.points = (0..count)
            .map(|i| crate::app::graph::numeric::sample_x(&range, count, i))
            .map(|x| (x, Some(f(x)).filter(|y| y.is_finite())))
            .collect();
        graph.sampled = range;
        (graph.sampling, graph.expression) = ("f".to_string(), "f".to_string());
        graph.complete = true;
        graph
    }

    #[test]
    fn both_axes_keep_one_scale() {
        let graph = Graph::new();
        let height = |width, height| {
            let range = graph.y_range(Size::new(width, height));
            range.end() - range.start()
        };
        assert_eq!(height(800.0, 400.0), 10.0);
        assert_eq!(height(500.0, 500.0), 20.0);
        assert_eq!(height(400.0, 800.0), 40.0);
        assert_eq!(height(0.0, 0.0), 20.0);
    }

    #[test]
    fn zooming_keeps_the_point_under_the_cursor() {
        let (range, center) = zoomed(&(-10.0..=10.0), 0.0, (5.0, 3.0), 0.5);
        assert_eq!((range, center), (-2.5..=7.5, 1.5));
        let (range, center) = zoomed(&(-10.0..=10.0), 2.0, (0.0, 2.0), 2.0);
        assert_eq!((range, center), (-20.0..=20.0, 2.0));
    }

    #[test]
    fn panning_moves_both_axes_by_the_same_distance() {
        let moved = panned(&(-10.0..=10.0), 0.0, (40.0, 20.0), 800.0);
        assert_eq!(moved, Some((-11.0..=9.0, 0.5)));
        assert_eq!(panned(&(-10.0..=10.0), 0.0, (1.0, 1.0), 0.0), None);
    }

    #[test]
    fn the_view_centers_on_the_curve_only_when_the_axis_misses_it() {
        let height = 17.5;
        assert_eq!(centered(&[], height), 0.0);
        assert_eq!(centered(&[5.0; 10], height), 0.0);
        assert_eq!(centered(&[-3.0, 2.0, 400.0, 900.0], height), 0.0);
        assert_eq!(centered(&[100.0; 10], height), 100.0);
        let parabola: Vec<f64> = (-10..=10).map(|x| f64::from(x * x) + 100.0).collect();
        assert_eq!(centered(&parabola, height), 125.0);
    }

    #[test]
    fn the_curve_decides_the_center_until_the_view_is_moved() {
        let mut offset = graph(|x| x * x + 100.0);
        let size = Size::new(800.0, 500.0);
        assert!(offset.center(size) > 100.0);
        offset.y_center = Some(3.0);
        assert_eq!(offset.center(size), 3.0);
        let range = offset.y_range(size);
        assert_eq!((range.start() + range.end()) / 2.0, 3.0);
        assert_eq!(graph(f64::tan).center(size), 0.0);
    }

    #[test]
    fn a_new_function_starts_from_the_default_view() {
        let mut moved = Graph::new();
        moved.range = 5.0..=55.0;
        moved.y_center = Some(7.0);
        moved.reset_view();
        assert_eq!((moved.range, moved.y_center), (-10.0..=10.0, None));
    }

    #[test]
    fn samples_fall_on_the_same_points_after_a_pan_or_a_zoom_by_two() {
        let graph = Graph::new();
        // Three views of 800 pixels, 1/32 apart: the power of two just above 20/799.
        assert_eq!(graph.sampling(&graph.range), (-30.0..=30.0, 1921));
        let (range, count) = graph.sampling(&(-9.3..=10.7));
        assert_eq!((range, count), (-29.3125..=30.71875, 1922));
        assert_eq!(graph.sampling(&(-20.0..=20.0)), (-60.0..=60.0, 1921));
        let (range, count) = graph.sampling(&(1e15..=1e15 + 1.0));
        assert_eq!((range, count), ((1e15 - 1.0)..=(1e15 + 2.0), 13));
    }

    #[test]
    fn only_samples_in_the_view_count() {
        let visible = graph(|x| x).visible();
        assert!(visible.iter().all(|x| (-10.0..=10.0).contains(x)));
        assert_eq!(visible.len(), 641);
    }

    #[test]
    fn the_readout_takes_the_nearest_point_known() {
        let mut line = Graph::new();
        line.points = vec![
            (0.0, Some(0.0)),
            (1.0, None),
            (4.0, Some(4.0)),
            (4.5, Some(4.5)),
        ];
        assert_eq!(line.sample_at(0.4), Some((0.0, 0.0)));
        assert_eq!(line.sample_at(1.2), None);
        assert_eq!(line.sample_at(3.0), Some((4.0, 4.0)));
        assert_eq!(line.sample_at(9.0), None);
    }

    #[test]
    fn samples_go_stale_when_the_view_nears_their_edge_or_changes_scale() {
        let mut line = graph(|x| x);
        assert!(!line.is_stale());
        line.range = -4.0..=16.0;
        assert!(!line.is_stale());
        line.range = 4.0..=24.0;
        assert!(line.is_stale());
        line.range = -24.0..=-4.0;
        assert!(line.is_stale());
        line.range = -7.0..=7.0;
        assert!(!line.is_stale());
        line.range = -5.0..=5.0;
        assert!(line.is_stale());
        line.range = -14.0..=14.0;
        assert!(!line.is_stale());
        line.range = -16.0..=16.0;
        assert!(line.is_stale());
    }

    #[test]
    fn a_zoom_out_is_either_sampled_already_or_sampled_again() {
        for factor in [1.1, 1.3, 1.45, 1.6, 2.0, 4.0] {
            for anchor in [-10.0, -4.0, 0.0, 7.0, 10.0] {
                let mut line = graph(|x| x);
                (line.range, _) = zoomed(&line.range, 0.0, (anchor, 0.0), factor);
                let (first, last) = (*line.range.start(), *line.range.end());
                let covered = line.sampled.contains(&first) && line.sampled.contains(&last);
                assert!(line.needs_sampling() || covered, "×{factor} at {anchor}");
            }
        }
    }

    #[test]
    fn a_sampling_cut_short_is_taken_up_again() {
        let mut line = graph(|x| x);
        assert!(!line.needs_sampling());
        line.complete = false;
        assert!(line.needs_sampling());
        line.pending = Some(1);
        assert!(!line.needs_sampling());
        line.range = 25.0..=45.0;
        line.progressive = true;
        assert!(line.needs_sampling());
        line.progressive = false;
        assert!(!line.needs_sampling());
        (line.pending, line.sampling) = (None, "g".to_string());
        assert!(!line.needs_sampling());
    }

    #[test]
    fn nothing_is_stale_without_a_usable_view() {
        let mut line = graph(|x| x);
        line.range = 0.0..=0.0;
        assert!(!line.is_stale());
        line.range = f64::NAN..=1.0;
        assert!(!line.is_stale());
    }

    #[test]
    fn the_pen_lifts_across_poles_only() {
        assert!(is_pole(80.0, -80.0, 20.0));
        assert!(!is_pole(-1.0, 1.0, 2.4));
        assert!(!is_pole(25.0, 6400.0, 20.0));
        assert!(!is_pole(-0.1, 0.1, 20.0));
    }

    #[test]
    fn the_grid_is_round_and_sparse() {
        assert_eq!(grid_step(20.0), 5.0);
        assert_eq!(grid_step(1.0), 0.2);
        assert_eq!(ticks(-10.0, 10.0, 5.0), [-10.0, -5.0, 0.0, 5.0, 10.0]);
        assert_eq!(ticks(0.0, 1.0, 0.2).len(), 6);
        assert!(ticks(0.0, 1.0, 0.0).is_empty());
        assert!(ticks(0.0, 1.0, f64::NAN).is_empty());
        assert!(ticks(0.0, 1e300, 1e-300).is_empty());
        assert_eq!(grid_step(0.0), 0.0);
        assert_eq!(grid_step(f64::NAN), 0.0);
        for (lo, hi) in [(0.0, 1e43), (-1e-12, 1e-12), (1e20 - 1e6, 1e20 + 1e6)] {
            let positions = ticks(lo, hi, grid_step(hi - lo));
            assert!((1..=20).contains(&positions.len()), "{lo} {hi}");
        }
    }

    #[test]
    fn labels_keep_only_the_decimals_the_step_needs() {
        assert_eq!(decimals(5.0), 0);
        assert_eq!(decimals(0.5), 1);
        assert_eq!(decimals(0.2), 1);
        assert_eq!(decimals(0.05), 2);
        assert_eq!(decimals(0.0), 12);
        assert_eq!(format_tick(0.5, 1), "0.5");
        assert_eq!(format_tick(1.0, 1), "1");
        assert_eq!(format_tick(-5.0, 0), "-5");
        assert_eq!(format_tick(100.0, 0), "100");
        assert_eq!(format_tick(0.0015, 4), "0.0015");
        assert_eq!(format_tick(0.0, 3), "0");
        assert_eq!(format_tick(-0.0, 3), "0");
    }

    #[test]
    fn huge_and_tiny_numbers_use_scientific_notation() {
        assert_eq!(format_tick(2.5e10, 0), "2.5e10");
        assert_eq!(format_tick(3.0000000000000004e-10, 10), "3e-10");
        assert_eq!(format_value(2.6881171418161356e43), "2.68812e43");
        assert_eq!(format_value(-0.0000001), "-1e-7");
        assert_eq!(format_value(1.5), "1.5");
        assert_eq!(format_value(-0.0), "0");
    }
}
