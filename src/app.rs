// SPDX-License-Identifier: GPL-3.0-only

use std::any::TypeId;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::app::{config::CONFIG_VERSION, operations::Calculator, operator::Operator};
use crate::core::{icons, key_binds::key_binds};
use crate::fl;
use cosmic::widget::RcElementWrapper;
use cosmic::{
    Application, ApplicationExt, Element,
    app::{Core, Task, context_drawer},
    cosmic_config,
    cosmic_config::Update,
    cosmic_theme,
    cosmic_theme::ThemeMode,
    iced::{
        Alignment, Event, Length, Subscription, clipboard, event,
        keyboard::Event as KeyEvent,
        keyboard::key::Named,
        keyboard::{Key, Modifiers},
        window,
    },
    theme,
    widget::{
        self, ToastId,
        about::About,
        menu::{self, Action, ItemHeight, ItemWidth},
        nav_bar,
    },
};

mod config;
mod graph;
mod latex;
mod operations;
mod operator;
pub mod settings;

pub struct CosmicCalculator {
    core: Core,
    about: About,
    context_page: ContextPage,
    key_binds: HashMap<menu::KeyBind, MenuAction>,
    nav: nav_bar::Model,
    modifiers: Modifiers,
    config_handler: Option<cosmic_config::Config>,
    config: config::CalculatorConfig,
    calculator: Calculator,
    toasts: widget::Toasts<Message>,
    input_id: widget::Id,
    button_font_size: f32,
    input_font_size: f32,
    previous: String,
    mode: Mode,
    graph: graph::Graph,
    readout: graph::Readout,
    // Incremented per request: a superseded one has its results dropped and its sampling killed.
    eval_id: Arc<AtomicU64>,
}

#[derive(Debug, Clone)]
pub enum Message {
    Number(f32),
    Operator(Operator),
    Input(String),
    Paste(String),
    PasteShortcut,
    Clipboard(Option<String>),
    ToggleContextPage(ContextPage),
    ToggleContextDrawer,
    Key(Modifiers, Key, Option<String>),
    Modifiers(Modifiers),
    SystemThemeModeChange,
    NavMenuAction(NavMenuAction),
    CleanHistory,
    ShowToast(String),
    CloseToast(ToastId),
    Open(String),
    SetDecimalComma(bool),
    Evaluate,
    Evaluated(u64, Calculator, Result<operations::Evaluation, String>),
    Graph(graph::Message),
    Window,
    Resized(cosmic::iced::Size),
    Toggle(Mode),
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum ContextPage {
    #[default]
    About,
}

#[derive(Clone, Debug)]
pub struct Flags {
    pub config_handler: Option<cosmic_config::Config>,
    pub config: config::CalculatorConfig,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MenuAction {
    About,
    ClearHistory,
    Toggle(Mode),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    Basic,
    Scientific,
    Graph,
}

impl menu::action::MenuAction for MenuAction {
    type Message = Message;

    fn message(&self) -> Self::Message {
        match self {
            MenuAction::About => Message::ToggleContextPage(ContextPage::About),
            MenuAction::ClearHistory => Message::CleanHistory,
            MenuAction::Toggle(mode) => Message::Toggle(*mode),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NavMenuAction {
    Delete(nav_bar::Id),
}

impl menu::action::MenuAction for NavMenuAction {
    type Message = cosmic::Action<Message>;

    fn message(&self) -> Self::Message {
        cosmic::Action::App(Message::NavMenuAction(*self))
    }
}

impl CosmicCalculator {
    pub fn button<'a>(&'a self, message: Message, class: theme::Button) -> Element<'a, Message> {
        let theme = cosmic::theme::active();

        let label = match &message {
            Message::Number(num) => num.to_string(),
            Message::Operator(operator) => operator.display(self.calculator.decimal_comma).to_string(),
            _ => String::new(),
        };

        let text_color = match class {
            theme::Button::Suggested => theme.cosmic().accent_button.on,
            theme::Button::Destructive => theme.cosmic().accent_button.on,
            _ => theme.cosmic().button_color(),
        };

        widget::button::custom(
            widget::container(
                widget::text(label)
                    .size(self.button_font_size)
                    .class(theme::Text::Color(text_color.into()))
                    .line_height(1.0),
            )
            .center(Length::Fill)
            .width(Length::Fill)
            .height(Length::Fill),
        )
        .class(class)
        .padding(0)
        .width(Length::FillPortion(1))
        .height(Length::Fill)
        .on_press(message)
        .into()
    }
}

impl Application for CosmicCalculator {
    type Executor = cosmic::executor::Default;

    type Flags = Flags;

    type Message = Message;

    const APP_ID: &'static str = "dev.edfloreshz.Calculator";

    fn core(&self) -> &Core {
        &self.core
    }

    fn core_mut(&mut self) -> &mut Core {
        &mut self.core
    }

    fn nav_model(&self) -> Option<&nav_bar::Model> {
        Some(&self.nav)
    }

    fn on_nav_select(&mut self, id: nav_bar::Id) -> Task<Self::Message> {
        self.nav.activate(id);
        let Some(data) = self.nav.active_data::<Calculator>().cloned() else {
            return Task::none();
        };
        if data.is_function() {
            self.mode = Mode::Graph;
            self.previous.clear();
            self.calculator.expression = data.expression.clone();
            self.calculator.outcome = String::new();
            return self.plot(data.expression, false);
        }
        // Restore the result and the formula line that produced it.
        self.next_request();
        if self.mode == Mode::Graph {
            self.mode = Mode::Basic;
        }
        self.previous = format!("{} =", data.expression.trim());
        self.calculator.expression = data.outcome.to_string();
        self.calculator.outcome = String::new();
        Task::none()
    }

    fn init(core: Core, flags: Self::Flags) -> (Self, Task<Self::Message>) {
        let mut nav = nav_bar::Model::default();

        for entry in &flags.config.history {
            nav.insert().text(history_label(entry)).data(entry.clone());
        }

        let about = About::default()
            .name(fl!("app-title"))
            .icon(widget::icon::from_name(Self::APP_ID))
            .version("0.2.1")
            .author("Eduardo Flores")
            .license("GPL-3.0-only")
            .links([
                (
                    fl!("support"),
                    "https://github.com/cosmic-utils/calculator/issues",
                ),
                (
                    fl!("repository"),
                    "https://github.com/cosmic-utils/calculator",
                ),
            ])
            .developers([("Eduardo Flores", "edfloreshz@gmail.com")]);

        let mut app = CosmicCalculator {
            core,
            about,
            context_page: ContextPage::default(),
            key_binds: key_binds(),
            nav,
            modifiers: Modifiers::empty(),
            config_handler: flags.config_handler,
            config: flags.config,
            calculator: Calculator::new(),
            toasts: widget::toaster::Toasts::new(Message::CloseToast),
            input_id: widget::Id::unique(),
            button_font_size: 20.0,
            input_font_size: 32.0,
            previous: String::new(),
            mode: Mode::Basic, // Defaults to basic mode
            graph: graph::Graph::new(),
            readout: graph::Readout::default(),
            eval_id: Arc::default(),
        };

        let mut tasks = vec![];

        tasks.push(app.set_window_title(fl!("app-title"), app.core.main_window_id().unwrap()));
        tasks.push(widget::text_input::focus(app.input_id.clone()));
        tasks.push(Task::perform(
            async move { operations::uses_decimal_comma().await },
            |decimal_comma| cosmic::Action::App(Message::SetDecimalComma(decimal_comma)),
        ));

        (app, Task::batch(tasks))
    }

    fn header_start<'a>(&'a self) -> Vec<Element<'a, Self::Message>> {
        let mode_items = [Mode::Basic, Mode::Scientific, Mode::Graph]
            .into_iter()
            .filter(|mode| *mode != self.mode)
            .map(|mode| {
                let label = match mode {
                    Mode::Basic => fl!("basic-mode"),
                    Mode::Scientific => fl!("scientific-mode"),
                    Mode::Graph => fl!("graph-mode"),
                };
                menu::Item::Button(
                    label,
                    Some(icons::get_handle("settings-symbolic", 14)),
                    MenuAction::Toggle(mode),
                )
            });

        let mut items = mode_items.collect::<Vec<_>>();
        items.extend([
            menu::Item::Button(
                fl!("clear-history"),
                Some(icons::get_handle("large-brush-symbolic", 14)),
                MenuAction::ClearHistory,
            ),
            menu::Item::Divider,
            menu::Item::Button(
                fl!("about"),
                Some(icons::get_handle("settings-symbolic", 14)),
                MenuAction::About,
            ),
        ]);

        let menu_bar = menu::bar(vec![menu::Tree::with_children(
            RcElementWrapper::new(menu::root(fl!("view")).into()),
            menu::items(&self.key_binds, items),
        )])
        .item_height(ItemHeight::Dynamic(40))
        .item_width(ItemWidth::Uniform(240))
        .spacing(4.0);

        vec![menu_bar.into()]
    }

    fn nav_context_menu(&self) -> Option<Vec<menu::Tree<cosmic::Action<Self::Message>>>> {
        let items = self.nav.iter().map(|entity| {
            let mut items: Vec<widget::menu::Item<NavMenuAction, String>> = Vec::with_capacity(1);
            items.push(cosmic::widget::menu::Item::Button(
                fl!("delete"),
                Some(icons::get_handle("user-trash-symbolic", 14)),
                NavMenuAction::Delete(entity),
            ));
            items
        });

        Some(cosmic::widget::menu::nav_context(
            &HashMap::new(),
            items.collect(),
        ))
    }

    fn view<'a>(&'a self) -> Element<'a, Self::Message> {
        let spacing = cosmic::theme::active().cosmic().spacing;

        // Build the keypad column with more rows in scientific mode
        let mut keypad = widget::column::with_capacity(
            match self.mode {
                Mode::Basic | Mode::Graph => 7,
                Mode::Scientific => 11,
            }
        );

        // Scientific buttons at the top of the keypad
        if self.mode == Mode::Scientific {
            keypad = keypad
                .push(
                    widget::row::with_capacity(4)
                        .push(self.button(Message::Operator(Operator::Log), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Ln), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Log2), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Factorial), theme::Button::Standard))
                        .width(Length::Fill)
                        .height(Length::Fill)
                        .spacing(spacing.space_xs),
                )
                .push(
                    widget::row::with_capacity(4)
                        .push(self.button(Message::Operator(Operator::Sin), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Cos), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Tan), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Comma), theme::Button::Standard))
                        .width(Length::Fill)
                        .height(Length::Fill)
                        .spacing(spacing.space_xs),
                )
                .push(
                    widget::row::with_capacity(4)
                        .push(self.button(Message::Operator(Operator::Asin), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Acos), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Atan), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Reciprocal), theme::Button::Standard))
                        .width(Length::Fill)
                        .height(Length::Fill)
                        .spacing(spacing.space_xs),
                )
                .push(
                    widget::row::with_capacity(4)
                        .push(self.button(Message::Operator(Operator::E), theme::Button::Standard))
                        .push(self.button(Message::Operator(Operator::Pi), theme::Button::Standard))
                        .width(Length::Fill)
                        .height(Length::Fill)
                        .spacing(spacing.space_xs),
                );
        }

        // Standard buttons and toaster to the keypad
        keypad = keypad
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Operator(Operator::Clear), theme::Button::Destructive))
                    .push(self.button(Message::Operator(Operator::Negate), theme::Button::Standard))
                    .push(self.button(Message::Operator(Operator::Modulus), theme::Button::Standard))
                    .push(self.button(Message::Operator(Operator::Power), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Operator(Operator::ParenthesesOpen), theme::Button::Standard))
                    .push(self.button(Message::Operator(Operator::ParenthesesClose), theme::Button::Standard))
                    .push(self.button(Message::Operator(Operator::SquareRoot), theme::Button::Standard))
                    .push(self.button(Message::Operator(Operator::Divide), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Number(7.0), theme::Button::Text))
                    .push(self.button(Message::Number(8.0), theme::Button::Text))
                    .push(self.button(Message::Number(9.0), theme::Button::Text))
                    .push(self.button(Message::Operator(Operator::Multiply), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Number(4.0), theme::Button::Text))
                    .push(self.button(Message::Number(5.0), theme::Button::Text))
                    .push(self.button(Message::Number(6.0), theme::Button::Text))
                    .push(self.button(Message::Operator(Operator::Subtract), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Number(1.0), theme::Button::Text))
                    .push(self.button(Message::Number(2.0), theme::Button::Text))
                    .push(self.button(Message::Number(3.0), theme::Button::Text))
                    .push(self.button(Message::Operator(Operator::Add), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(
                widget::row::with_capacity(4)
                    .push(self.button(Message::Number(0.0), theme::Button::Text))
                    .push(self.button(Message::Operator(Operator::Point), theme::Button::Text))
                    .push(self.button(Message::Operator(Operator::Backspace), theme::Button::Destructive))
                    .push(self.button(Message::Operator(Operator::Equal), theme::Button::Suggested))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .spacing(spacing.space_xs),
            )
            .push(widget::row(vec![widget::toaster(
                &self.toasts,
                widget::space::horizontal(),
            )]))
            .max_width(1000.0)
            .width(Length::Fill)
            .height(Length::Fill)
            .align_x(Alignment::Center)
            .spacing(spacing.space_xs);

        // Final outer column combining the input field and the completed keypad
        widget::column::with_capacity(2)
            .push(
                widget::column::with_capacity(2)
                    .push(
                        widget::container(
                            widget::text(&self.previous)
                                .size((self.input_font_size * 0.45).max(9.0))
                                .class(theme::Text::Accent),
                        )
                        .width(Length::Fill)
                        .align_x(Alignment::End),
                    )
                    .push(
                        widget::text_input("", &self.calculator.expression)
                            .on_input(Message::Input)
                            .on_paste(Message::Paste)
                            .on_submit(|_| Message::Operator(Operator::Equal))
                            .id(self.input_id.clone())
                            .size(self.input_font_size)
                            .padding(spacing.space_xxs)
                            .width(Length::Fill),
                    )
                    .push_maybe((self.mode == Mode::Graph).then(|| self.readout_row()))
                    .spacing(2),
            )
            .push(match self.mode {
                Mode::Graph => self.plot_area(),
                _ => Element::from(keypad),
            })
            .align_x(Alignment::Center)
            .spacing(spacing.space_s)
            .padding([0, spacing.space_xxs, spacing.space_xxs, spacing.space_xxs])
            .into()
    }

    fn update(&mut self, message: Self::Message) -> Task<Self::Message> {
        let mut tasks = vec![];
        match message {
            Message::Open(url) => {
                if let Err(err) = open::that_detached(url) {
                    tracing::error!("{err}")
                }
            }
            Message::ShowToast(message) => {
                tasks.push(
                    self.toasts
                        .push(widget::toaster::Toast::new(message))
                        .map(cosmic::Action::App),
                );
            }
            Message::CloseToast(id) => self.toasts.remove(id),
            Message::ToggleContextPage(context_page) => {
                if self.context_page == context_page {
                    self.core.window.show_context = !self.core.window.show_context;
                } else {
                    self.context_page = context_page;
                    self.core.window.show_context = true;
                }
            }
            Message::ToggleContextDrawer => {
                self.core.window.show_context = !self.core.window.show_context;
            }
            Message::SetDecimalComma(decimal_comma) => {
                self.calculator.decimal_comma = decimal_comma;
                tracing::info!("Calculator initialized");
            }
            Message::Number(num) => self.calculator.on_number_press(num),
            Message::Input(input) => self.calculator.on_input(input),
            Message::Paste(content) => {
                if let Err(error) = self.calculator.on_paste(&content) {
                    tasks.push(self.update(Message::ShowToast(paste_error(&error))));
                }
            }
            Message::PasteShortcut => {
                tasks.push(
                    clipboard::read().map(|text| cosmic::Action::App(Message::Clipboard(text))),
                );
            }
            Message::Clipboard(text) => {
                let Some(text) = text else {
                    return Task::batch(tasks);
                };
                let content = format!("{}{}", self.calculator.expression, text);
                tasks.push(self.update(Message::Paste(content)));
                tasks.push(widget::text_input::focus(self.input_id.clone()));
                tasks.push(widget::text_input::move_cursor_to_end(
                    self.input_id.clone(),
                ));
            }
            Message::Operator(operator) => {
                if operator == Operator::Clear {
                    self.previous.clear();
                    self.graph.reset();
                    self.readout.reset();
                    // A plot still being sampled belongs to what was just cleared.
                    self.next_request();
                }
                if let Some(operations::Message::Evaluate) =
                    self.calculator.on_operator_press(&operator)
                {
                    tasks.push(self.update(Message::Evaluate));
                }
            }
            Message::Evaluate => {
                // An empty expression drops qalc into interactive mode, which hangs.
                if self.calculator.expression.trim().is_empty() {
                    return Task::batch(tasks);
                }
                if self.mode == Mode::Graph {
                    let expression = self.calculator.expression.clone();
                    tasks.push(self.plot(expression, true));
                } else {
                    let id = self.next_request();
                    let calculator = self.calculator.clone();
                    tasks.push(Task::perform(
                        operations::blocking(move || {
                            let evaluation = calculator.evaluate();
                            (calculator, evaluation)
                        }),
                        move |(asked, evaluation)| {
                            cosmic::Action::App(Message::Evaluated(id, asked, evaluation))
                        },
                    ));
                }
            }
            Message::Evaluated(id, asked, evaluation) => {
                // Only for the latest request, and while the field still holds what it asked.
                if id != self.eval_id.load(Ordering::Relaxed)
                    || asked.expression != self.calculator.expression
                {
                    return Task::batch(tasks);
                }
                let evaluation = match evaluation {
                    Ok(evaluation) => evaluation,
                    Err(error) => {
                        tracing::error!("{}", error);
                        tasks.push(self.update(Message::ShowToast(error)));
                        return Task::batch(tasks);
                    }
                };

                self.previous = format!("{} =", asked.expression.trim());
                self.calculator = Calculator {
                    outcome: evaluation.outcome.clone(),
                    outcome_exact: evaluation.exact,
                    chain_exact: true,
                    ..asked
                };

                let entry = self.calculator.clone();
                tasks.push(self.add_to_history(entry));

                self.calculator.expression = evaluation.outcome;
            }
            Message::Graph(message) => tasks.push(self.update_graph(message)),
            Message::Key(modifiers, key, text) => {
                for (key_bind, action) in &self.key_binds {
                    if key_bind.matches(modifiers, &key, None) {
                        return self.update(action.message());
                    }
                }

                // Ignore modified keys so menu keybinds keep working.
                if modifiers.control() || modifiers.alt() || modifiers.logo() {
                    return Task::batch(tasks);
                }

                // `text` carries the layout-resolved character; the key is a fallback.
                let character = match (text, &key) {
                    (Some(t), _) => Some(t),
                    (None, Key::Character(c)) => Some(c.to_string()),
                    _ => None,
                };
                if let Some(c) = character {
                    let operator = match c.as_str() {
                        "+" => Some(Operator::Add),
                        "-" => Some(Operator::Subtract),
                        "*" | "×" => Some(Operator::Multiply),
                        "/" | "÷" => Some(Operator::Divide),
                        "%" => Some(Operator::Modulus),
                        "(" => Some(Operator::ParenthesesOpen),
                        ")" => Some(Operator::ParenthesesClose),
                        "^" => Some(Operator::Power),
                        "=" => Some(Operator::Equal),
                        "√" => Some(Operator::SquareRoot),
                        "!" => Some(Operator::Factorial),
                        "π" => Some(Operator::Pi),
                        _ => None,
                    };
                    if let Some(operator) = operator {
                        return self.update(Message::Operator(operator));
                    }

                    // Digits and decimal separators reuse on_input validation.
                    if !c.is_empty()
                        && c.chars()
                            .all(|ch| ch.is_ascii_digit() || ch == '.' || ch == ',')
                    {
                        let mut expression = self.calculator.expression.clone();
                        expression.push_str(&c);
                        return self.update(Message::Input(expression));
                    }
                }
                match key {
                    Key::Named(Named::Enter) => {
                        return self.update(Message::Operator(Operator::Equal));
                    }
                    Key::Named(Named::Backspace) => {
                        return self.update(Message::Operator(Operator::Backspace));
                    }
                    Key::Named(Named::Delete) | Key::Named(Named::Escape) => {
                        return self.update(Message::Operator(Operator::Clear));
                    }
                    _ => {}
                }
            }
            Message::Modifiers(modifiers) => {
                self.modifiers = modifiers;
            }
            Message::NavMenuAction(action) => {
                match action {
                    NavMenuAction::Delete(entity) => {
                        if let Some(data) = self.nav.data::<Calculator>(entity).cloned() {
                            let mut history = self.config.history.clone();
                            history.retain(|calc| calc != &data);
                            if let Some(config_handler) = &self.config_handler
                                && let Err(err) = self.config.set_history(config_handler, history)
                            {
                                tracing::error!("Failed to save history: {}", err);
                                tasks.push(self.update(Message::ShowToast(
                                    "Failed to save history".to_string(),
                                )));
                            }
                            self.nav.remove(entity);
                        }
                    }
                }
            }
            Message::SystemThemeModeChange => {
                return self.update_config();
            }
            Message::CleanHistory => {
                if let Some(config_handler) = &self.config_handler
                    && let Err(err) = self.config.set_history(config_handler, vec![])
                {
                    tracing::error!("Failed to save history: {}", err);
                    tasks.push(
                        self.update(Message::ShowToast("Failed to save history".to_string())),
                    );
                }
                self.nav.clear();
            }
            Message::Window => {
                return widget::text_input::focus(self.input_id.clone());
            }
            Message::Resized(size) => {
                self.button_font_size = (size.height / 22.0).clamp(10.0, 48.0);
                self.input_font_size = (size.height * 0.065).clamp(14.0, 32.0);
                self.graph.resize(size.width);
            }
            Message::Toggle(mode) => self.mode = mode,
        }
        Task::batch(tasks)
    }

    fn context_drawer<'a>(&'a self) -> Option<context_drawer::ContextDrawer<'a, Self::Message>> {
        if !self.core.window.show_context {
            return None;
        }

        Some(match self.context_page {
            ContextPage::About => context_drawer::about(
                &self.about,
                |url| Message::Open(url.to_string()),
                Message::ToggleContextDrawer,
            ),
        })
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        struct ConfigSubscription;
        struct ThemeSubscription;

        let subscriptions = vec![
            event::listen_with(|event, status, _id| match event {
                // The field handles Ctrl+V itself when focused; this is for when it is not.
                Event::Keyboard(KeyEvent::KeyPressed {
                    key,
                    modifiers,
                    physical_key,
                    ..
                }) if status == event::Status::Ignored
                    && modifiers.control()
                    && key.to_latin(physical_key) == Some('v') =>
                {
                    Some(Message::PasteShortcut)
                }
                Event::Keyboard(KeyEvent::KeyPressed { key, modifiers, text, .. }) => match status {
                    event::Status::Ignored => {
                        Some(Message::Key(modifiers, key, text.map(|t| t.to_string())))
                    }
                    event::Status::Captured => None,
                },
                Event::Keyboard(KeyEvent::ModifiersChanged(modifiers)) => {
                    Some(Message::Modifiers(modifiers))
                }
                Event::Window(window::Event::Focused) => Some(Message::Window),
                Event::Window(window::Event::Resized(size)) => Some(Message::Resized(size)),
                _ => None,
            }),
            cosmic_config::config_subscription(
                TypeId::of::<ConfigSubscription>(),
                Self::APP_ID.into(),
                CONFIG_VERSION,
            )
            .map(|update: Update<ThemeMode>| {
                if !update.errors.is_empty() {
                    tracing::info!(
                        "errors loading config {:?}: {:?}",
                        update.keys,
                        update.errors
                    );
                }
                Message::SystemThemeModeChange
            }),
            cosmic_config::config_subscription::<_, cosmic_theme::ThemeMode>(
                TypeId::of::<ThemeSubscription>(),
                cosmic_theme::THEME_MODE_ID.into(),
                cosmic_theme::ThemeMode::version(),
            )
            .map(|update: Update<ThemeMode>| {
                if !update.errors.is_empty() {
                    tracing::info!(
                        "errors loading theme mode {:?}: {:?}",
                        update.keys,
                        update.errors
                    );
                }
                Message::SystemThemeModeChange
            }),
        ];

        Subscription::batch(subscriptions)
    }
}

impl CosmicCalculator {
    fn update_config(&mut self) -> Task<Message> {
        cosmic::command::set_theme(self.config.app_theme.theme())
    }

    // Starts a request, superseding whatever an older one is still running.
    fn next_request(&self) -> u64 {
        self.eval_id.fetch_add(1, Ordering::Relaxed) + 1
    }

    fn add_to_history(&mut self, entry: Calculator) -> Task<Message> {
        let mut history = self.config.history.clone();
        history.push(entry.clone());
        let mut task = Task::none();
        if let Some(config_handler) = &self.config_handler
            && let Err(err) = self.config.set_history(config_handler, history)
        {
            tracing::error!("Failed to save history: {}", err);
            task = self.update(Message::ShowToast("Failed to save history".to_string()));
        }
        self.nav.insert().text(history_label(&entry)).data(entry);
        task
    }
}

fn paste_error(error: &operations::PasteError) -> String {
    match error {
        operations::PasteError::Equation => fl!("paste-equation"),
        operations::PasteError::Unsupported(c) => {
            fl!("paste-unsupported", character = c.to_string())
        }
        operations::PasteError::Latex(token) => fl!("paste-latex", token = token.clone()),
    }
}

fn history_label(entry: &Calculator) -> String {
    const MAX: usize = 28;
    let label = if entry.is_function() {
        format!("y = {}", entry.expression)
    } else {
        entry.expression.clone()
    };
    if label.chars().count() > MAX {
        label.chars().take(MAX).collect::<String>() + "…"
    } else {
        label
    }
}

#[cfg(test)]
impl CosmicCalculator {
    pub(crate) fn headless() -> Self {
        CosmicCalculator {
            core: Core::default(),
            about: About::default(),
            context_page: ContextPage::default(),
            key_binds: key_binds(),
            nav: nav_bar::Model::default(),
            modifiers: Modifiers::empty(),
            config_handler: None,
            config: config::CalculatorConfig::default(),
            calculator: Calculator::new(),
            toasts: widget::toaster::Toasts::new(Message::CloseToast),
            input_id: widget::Id::unique(),
            button_font_size: 20.0,
            input_font_size: 32.0,
            previous: String::new(),
            mode: Mode::Basic,
            graph: graph::Graph::new(),
            readout: graph::Readout::default(),
            eval_id: Arc::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Calculator, CosmicCalculator, Message, Ordering, operations::Evaluation};
    use cosmic::Application;

    fn answer(outcome: &str) -> Result<Evaluation, String> {
        Ok(Evaluation {
            outcome: outcome.to_string(),
            exact: outcome.to_string(),
        })
    }

    fn ask(app: &mut CosmicCalculator, expression: &str) -> (u64, Calculator) {
        app.calculator.expression = expression.to_string();
        let _ = app.update(Message::Evaluate);
        (app.eval_id.load(Ordering::Relaxed), app.calculator.clone())
    }

    #[test]
    fn a_result_lands_only_on_the_field_that_asked_for_it() {
        let mut app = CosmicCalculator::headless();
        let (id, asked) = ask(&mut app, "2+2");
        let _ = app.update(Message::Input("2+3".to_string()));
        let _ = app.update(Message::Evaluated(id, asked, answer("4")));
        assert_eq!(
            (app.calculator.expression.as_str(), app.nav.len()),
            ("2+3", 0)
        );
        let (id, asked) = ask(&mut app, "25");
        let entry = Calculator {
            expression: "5*5".to_string(),
            outcome: "25".to_string(),
            ..Calculator::new()
        };
        app.nav.insert().text("5*5").data(entry);
        let _ = app.on_nav_select(app.nav.iter().last().unwrap());
        let _ = app.update(Message::Evaluated(id, asked, answer("25")));
        assert_eq!((app.previous.as_str(), app.nav.len()), ("5*5 =", 1));
        let (id, asked) = ask(&mut app, "2+2");
        let _ = app.update(Message::Evaluated(id, asked, answer("4")));
        assert_eq!(
            (app.previous.as_str(), app.calculator.expression.as_str()),
            ("2+2 =", "4")
        );
        let last = app.nav.iter().last().unwrap();
        let saved = app.nav.data::<Calculator>(last).unwrap();
        assert_eq!(
            (saved.expression.as_str(), saved.outcome.as_str()),
            ("2+2", "4")
        );
    }
}
