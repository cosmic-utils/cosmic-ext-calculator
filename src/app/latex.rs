/// Translates LaTeX math into a qalc expression; the error is the text that could not be read.
pub fn to_expression(latex: &str, decimal_comma: bool) -> Result<String, String> {
    let node = read(
        tokens(latex, Syntax::Latex, false)?,
        Syntax::Latex,
        decimal_comma,
    )?;
    let mut expression = String::new();
    node.write(&mut expression, 0, decimal_comma);
    Ok(expression)
}

/// Reads an expression as the field holds it, decimal commas included, into the tree qalc reads.
pub(crate) fn parse(text: &str, decimal_comma: bool) -> Result<Node, String> {
    read(
        tokens(text, Syntax::Field, decimal_comma)?,
        Syntax::Field,
        false,
    )
}

/// What is read: pasted LaTeX, or the field's text, which follows qalc where the two differ.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Syntax {
    Latex,
    Field,
}

/// The tree of a whole formula; `decimal_comma` is how its numbers will be written back.
fn read(
    (tokens, spaced): (Vec<Token>, Vec<bool>),
    syntax: Syntax,
    decimal_comma: bool,
) -> Result<Node, String> {
    if tokens.len() > MAX_TOKENS {
        return Err("…".to_string());
    }
    let mut parser = Parser {
        tokens,
        spaced,
        syntax,
        pos: 0,
        decimal_comma,
        scopes: Vec::new(),
        bars: 0,
        integrals: 0,
        bare: 0,
        depth: 0,
    };
    let node = parser.expr()?;
    if let Some(token) = parser.peek() {
        return Err(describe(token));
    }
    Ok(node)
}

/// Nesting and length beyond what any formula needs, refused so that a paste cannot exhaust the stack.
const MAX_DEPTH: usize = 50;
const MAX_TOKENS: usize = 2_000;

/// Names qalc gives to variables bound by `\sum` and `\int`: most single letters are units there.
const BOUND: [char; 3] = ['y', 'z', 'n'];

/// Functions LaTeX writes as a command, which are also read without their backslash.
const FUNCTIONS: [&str; 26] = [
    "sin", "cos", "tan", "cot", "sec", "csc", "sinh", "cosh", "tanh", "coth", "sech", "csch",
    "arcsin", "arccos", "arctan", "arccot", "ln", "log", "lg", "exp", "max", "min", "gcd", "lcm",
    "Gamma", "zeta",
];

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(String),
    /// A variable: letters side by side are a product, so each one is its own token.
    Name(char),
    Command(String),
    Function(String),
    Symbol(char),
    OpenBar,
    CloseBar,
}

fn describe(token: &Token) -> String {
    match token {
        Token::Number(text) | Token::Function(text) => text.clone(),
        Token::Name(c) | Token::Symbol(c) => c.to_string(),
        Token::Command(name) => format!("\\{name}"),
        Token::OpenBar | Token::CloseBar => "|".to_string(),
    }
}

fn is_function(name: &str) -> bool {
    FUNCTIONS.contains(&name)
}

fn qalc_function(name: &str, syntax: Syntax) -> (String, Option<String>) {
    if let Some(inverse) = name.strip_prefix("arc") {
        return (format!("a{inverse}"), None);
    }
    match name {
        "sin" | "cos" | "tan" | "cot" | "sec" | "csc" | "sinh" | "cosh" | "tanh" | "coth" => {
            (name.to_string(), Some(format!("a{name}")))
        }
        // The field's `log` is qalc's, natural with one argument, and qalc has no `lg`.
        "log" | "lg" if syntax == Syntax::Latex => ("log10".to_string(), None),
        "Gamma" => ("gamma".to_string(), None),
        _ => (name.to_string(), None),
    }
}

struct Lexer {
    chars: Vec<char>,
    pos: usize,
    decimal_comma: bool,
}

impl Lexer {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn take_while(&mut self, mut accept: impl FnMut(char) -> bool) -> String {
        let start = self.pos;
        while self.peek().is_some_and(&mut accept) {
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn skip_spaces(&mut self) {
        self.take_while(char::is_whitespace);
    }

    fn parenthesis_follows(&self) -> bool {
        self.chars[self.pos..].iter().find(|c| !c.is_whitespace()) == Some(&'(')
    }

    fn number(&mut self) -> Result<Token, String> {
        let mut text = String::new();
        while let Some(c) = self.peek() {
            let digit_follows = self
                .chars
                .get(self.pos + 1)
                .is_some_and(|c| c.is_ascii_digit());
            let mark = c == '.' || (c == ',' && self.decimal_comma && digit_follows);
            if mark && !text.contains('.') {
                text.push('.');
            } else if c.is_ascii_digit() {
                text.push(c);
            } else {
                break;
            }
            self.pos += 1;
        }
        if !text.contains(|c: char| c.is_ascii_digit()) {
            return Err(text);
        }
        if let Some(exponent) = self.exponent() {
            text.push_str(&exponent);
            // qalc reads `1e3.5` and `1e3e2` in ways nothing here does.
            if self.number_follows() {
                return Err(text);
            }
        }
        Ok(Token::Number(text))
    }

    /// `e-3` or `E3` after digits, as qalc reads them; `2e` alone is 2·e.
    fn exponent(&mut self) -> Option<String> {
        let at = |i: usize| self.chars.get(self.pos + i).copied();
        let mark = at(0).filter(|&c| c == 'e' || c == 'E')?;
        let sign = match at(1) {
            Some('+') => "+",
            Some('-' | '−') => "-",
            _ => "",
        };
        let first = 1 + sign.len();
        at(first).filter(char::is_ascii_digit)?;
        self.pos += first;
        let digits = self.take_while(|c| c.is_ascii_digit());
        Some(format!("{mark}{sign}{digits}"))
    }

    fn number_follows(&self) -> bool {
        let at = |i: usize| self.chars.get(self.pos + i).copied();
        let digit = |i: usize| at(i).is_some_and(|c| c.is_ascii_digit());
        match at(0) {
            Some('.') => digit(1),
            Some(',') => self.decimal_comma && digit(1),
            Some('e' | 'E') => digit(1) || (matches!(at(1), Some('+' | '-' | '−')) && digit(2)),
            _ => false,
        }
    }

    fn base_follows(&self) -> bool {
        let at = |i: usize| self.chars.get(self.pos + i).copied();
        matches!(at(0), Some('x' | 'X' | 'b' | 'B' | 'o' | 'O'))
            && at(1).is_some_and(|c| c.is_ascii_alphanumeric())
    }

    fn word(&mut self, tokens: &mut Vec<Token>) {
        let word = self.take_while(|c| c.is_ascii_alphabetic());
        let after_word = self.pos;
        let digits = self.take_while(|c| c.is_ascii_digit());
        if word.len() >= 2 && !digits.is_empty() && self.parenthesis_follows() {
            tokens.push(Token::Function(word + &digits));
            return;
        }
        self.pos = after_word;
        if is_function(&word) || matches!(word.as_str(), "sqrt" | "pi" | "tau" | "infinity") {
            tokens.push(Token::Command(word));
        } else if word.len() >= 2 && self.parenthesis_follows() {
            tokens.push(Token::Function(word));
        } else {
            tokens.extend(word.chars().map(Token::Name));
        }
    }

    fn braced_word(&mut self) -> Option<String> {
        self.skip_spaces();
        let start = self.pos;
        if self.peek() == Some('{') {
            self.pos += 1;
            let word = self.take_while(|c| c.is_ascii_alphabetic());
            if !word.is_empty() && self.peek() == Some('}') {
                self.pos += 1;
                return Some(word);
            }
        }
        self.pos = start;
        None
    }

    fn command(&mut self, side: &mut Option<bool>) -> Result<Option<Token>, String> {
        let Some(first) = self.peek() else {
            return Err("\\".to_string());
        };
        if !first.is_ascii_alphabetic() {
            self.pos += 1;
            return match first {
                ',' | ';' | ':' | '!' | ' ' | '>' | '(' | ')' | '[' | ']' => Ok(None),
                _ => Err(format!("\\{first}")),
            };
        }
        let name = self.take_while(|c| c.is_ascii_alphabetic());
        Ok(match name.as_str() {
            "left" | "right" => {
                *side = Some(name == "left");
                None
            }
            "big" | "Big" | "bigg" | "Bigg" | "bigl" | "bigr" | "Bigl" | "Bigr" | "biggl"
            | "biggr" | "Biggl" | "Biggr" | "displaystyle" | "textstyle" | "scriptstyle"
            | "limits" | "nolimits" | "quad" | "qquad" => None,
            "lvert" => Some(Token::OpenBar),
            "rvert" => Some(Token::CloseBar),
            "infty" => Some(Token::Command("infinity".to_string())),
            "operatorname" | "mathrm" => {
                let word = self.braced_word().ok_or_else(|| format!("\\{name}"))?;
                match word.chars().next() {
                    Some(letter) if name == "mathrm" && word.len() == 1 => {
                        Some(Token::Name(letter))
                    }
                    _ => Some(Token::Function(word)),
                }
            }
            _ => Some(Token::Command(name)),
        })
    }
}

fn symbol(c: char, delimiter: Option<bool>) -> Result<Token, String> {
    Ok(match c {
        '|' => match delimiter {
            Some(true) => Token::OpenBar,
            Some(false) => Token::CloseBar,
            None => Token::Symbol('|'),
        },
        '+' | '-' | '*' | '/' | '^' | '_' | '!' | ',' | ';' | '=' | '(' | ')' | '[' | ']' | '{'
        | '}' | '°' => Token::Symbol(c),
        '−' => Token::Symbol('-'),
        '×' | '·' | '⋅' => Token::Symbol('*'),
        '÷' => Token::Symbol('/'),
        'π' => Token::Command("pi".to_string()),
        '∞' => Token::Command("infinity".to_string()),
        '√' => Token::Symbol('√'),
        _ => return Err(c.to_string()),
    })
}

fn tokens(
    text: &str,
    syntax: Syntax,
    decimal_comma: bool,
) -> Result<(Vec<Token>, Vec<bool>), String> {
    let mut lexer = Lexer {
        chars: text.chars().collect(),
        pos: 0,
        decimal_comma,
    };
    let (mut tokens, mut spaced) = (Vec::new(), Vec::new());
    let mut side = None;
    let mut space = false;
    while let Some(c) = lexer.peek() {
        if c.is_whitespace() || matches!(c, '$' | '~') {
            lexer.pos += 1;
            space = true;
            continue;
        }
        let start = tokens.len();
        let delimiter = side.take();
        if c == '.' && delimiter.is_some() {
            lexer.pos += 1;
        } else if c.is_ascii_digit() || c == '.' {
            let number = lexer.number()?;
            // qalc joins digits a space apart, `1 000` being 1000, and reads `0x10` in hexadecimal.
            let zero = number == Token::Number("0".to_string());
            if syntax == Syntax::Field
                && (matches!(tokens.last(), Some(Token::Number(_)))
                    || (zero && lexer.base_follows()))
            {
                return Err(describe(&number));
            }
            tokens.push(number);
        } else if c.is_ascii_alphabetic() {
            lexer.word(&mut tokens);
        } else if c == '\\' {
            lexer.pos += 1;
            tokens.extend(lexer.command(&mut side)?);
        } else {
            lexer.pos += 1;
            match c {
                '*' if lexer.peek() == Some('*') => {
                    lexer.pos += 1;
                    tokens.push(Token::Symbol('^'));
                }
                '²' | '³' => {
                    // One exponent, as qalc reads it: `x²³` is x^23.
                    let digit = |c: char| if c == '²' { '2' } else { '3' };
                    let mut digits = digit(c).to_string();
                    while let Some(next) = lexer.peek().filter(|&c| c == '²' || c == '³') {
                        digits.push(digit(next));
                        lexer.pos += 1;
                    }
                    tokens.push(Token::Symbol('^'));
                    tokens.push(Token::Number(digits));
                }
                _ => tokens.push(symbol(c, delimiter)?),
            }
        }
        spaced.extend((start..tokens.len()).map(|i| i == start && space));
        space = false;
    }
    Ok((tokens, spaced))
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(Debug)]
pub(crate) enum Node {
    Number(String),
    Variable(String),
    Negate(Box<Node>),
    Binary(Op, Box<Node>, Box<Node>),
    Factorial(Box<Node>),
    Degrees(Box<Node>),
    Call(String, Vec<Node>),
}

const SUM: u8 = 1;
const SIGN: u8 = 2;
const PRODUCT: u8 = 3;
const POWER: u8 = 4;
const POSTFIX: u8 = 5;
const ATOM: u8 = 6;

impl Node {
    fn binary(op: Op, left: Node, right: Node) -> Node {
        Node::Binary(op, Box::new(left), Box::new(right))
    }

    fn call(name: &str, arguments: Vec<Node>) -> Node {
        Node::Call(name.to_string(), arguments)
    }

    fn negate_if(self, negative: bool) -> Node {
        if negative {
            Node::Negate(Box::new(self))
        } else {
            self
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            Node::Number(_) | Node::Variable(_) | Node::Call(..) => ATOM,
            Node::Factorial(_) | Node::Degrees(_) => POSTFIX,
            Node::Binary(Op::Power, ..) => POWER,
            Node::Binary(Op::Multiply | Op::Divide, ..) => PRODUCT,
            Node::Negate(_) => SIGN,
            Node::Binary(Op::Add | Op::Subtract, ..) => SUM,
        }
    }

    /// Writes the node with the fewest parentheses that keep it one operand of precedence `min`.
    fn write(&self, out: &mut String, min: u8, comma: bool) {
        let wrap = self.precedence() < min;
        if wrap {
            out.push('(');
        }
        match self {
            Node::Number(text) | Node::Variable(text) => out.push_str(text),
            Node::Negate(inner) => {
                out.push('-');
                inner.write(out, PRODUCT, comma);
            }
            Node::Binary(op, left, right) => {
                let (symbol, left_min, right_min) = match op {
                    Op::Add => ('+', SUM, PRODUCT),
                    Op::Subtract => ('-', SUM, PRODUCT),
                    Op::Multiply => ('*', SIGN, POWER),
                    Op::Divide => ('/', SIGN, POWER),
                    Op::Power => ('^', ATOM, ATOM),
                };
                left.write(out, left_min, comma);
                out.push(symbol);
                // `e^(sin(x))`: a call in an exponent reads ambiguously without parentheses.
                let call = matches!((op, &**right), (Op::Power, Node::Call(..)));
                right.write(out, if call { ATOM + 1 } else { right_min }, comma);
            }
            Node::Factorial(inner) => {
                inner.write(out, ATOM, comma);
                out.push('!');
            }
            Node::Degrees(inner) => {
                inner.write(out, ATOM, comma);
                out.push('°');
            }
            Node::Call(name, arguments) => {
                out.push_str(name);
                out.push('(');
                for (i, argument) in arguments.iter().enumerate() {
                    if i > 0 {
                        out.push_str(if comma { "; " } else { ", " });
                    }
                    argument.write(out, 0, comma);
                }
                out.push(')');
            }
        }
        if wrap {
            out.push(')');
        }
    }

    /// `d`, `dy`: the start of a differential, which makes `\frac{d}{dx}` a derivative.
    fn leads_with_d(&self) -> bool {
        match self {
            Node::Variable(name) => name == "d",
            Node::Binary(Op::Multiply, left, _) => left.leads_with_d(),
            _ => false,
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    /// Whether a space comes before each token: in the field, `1/2 x` is not `1/2x`.
    spaced: Vec<bool>,
    syntax: Syntax,
    pos: usize,
    decimal_comma: bool,
    scopes: Vec<(char, char)>,
    /// Plain `|` currently open, to tell a closing one from an opening one.
    bars: usize,
    /// Integrals being read: their body ends at the differential.
    integrals: usize,
    bare: usize,
    depth: usize,
}

impl Parser {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Result<Token, String> {
        let token = self.tokens.get(self.pos).cloned().ok_or("…")?;
        self.pos += 1;
        Ok(token)
    }

    fn eat(&mut self, token: &Token) -> bool {
        let found = self.peek() == Some(token);
        if found {
            self.pos += 1;
        }
        found
    }

    fn expect(&mut self, token: Token) -> Result<(), String> {
        match self.peek() {
            Some(found) if *found == token => {
                self.pos += 1;
                Ok(())
            }
            Some(found) => Err(describe(found)),
            None => Err(describe(&token)),
        }
    }

    fn signs(&mut self) -> bool {
        let mut negative = false;
        while let Some(Token::Symbol(sign @ ('+' | '-'))) = self.peek() {
            negative ^= *sign == '-';
            self.pos += 1;
        }
        negative
    }

    fn at_differential(&self) -> bool {
        self.integrals > 0
            && self.peek() == Some(&Token::Name('d'))
            && matches!(self.tokens.get(self.pos + 1), Some(Token::Name(_)))
    }

    fn starts_operand(&self, token: &Token) -> bool {
        let starts = match token {
            Token::Number(_) | Token::Name(_) | Token::Function(_) | Token::OpenBar => true,
            Token::Command(name) => {
                is_function(name)
                    || matches!(
                        name.as_str(),
                        "frac"
                            | "dfrac"
                            | "tfrac"
                            | "binom"
                            | "dbinom"
                            | "tbinom"
                            | "sqrt"
                            | "pi"
                            | "tau"
                            | "infinity"
                            | "sum"
                            | "prod"
                            | "int"
                            | "lfloor"
                            | "lceil"
                    )
            }
            Token::Symbol('(' | '[' | '{' | '√') => true,
            Token::Symbol('|') => self.bars == 0,
            _ => false,
        };
        starts && !self.at_differential()
    }

    fn starts_bare_operand(&self, token: &Token) -> bool {
        let starts = match token {
            Token::Number(_) | Token::Name(_) | Token::Symbol('√') => true,
            Token::Command(name) => matches!(
                name.as_str(),
                "frac" | "dfrac" | "tfrac" | "binom" | "sqrt" | "pi" | "tau" | "infinity"
            ),
            _ => false,
        };
        starts && !self.at_differential()
    }

    fn variable(&self, name: char) -> String {
        self.scopes
            .iter()
            .rev()
            .find(|(variable, _)| *variable == name)
            .map_or_else(|| name.to_string(), |(_, bound)| bound.to_string())
    }

    fn expr(&mut self) -> Result<Node, String> {
        let mut node = self.signed_term()?;
        loop {
            let op = match self.peek() {
                Some(Token::Symbol('+')) => Op::Add,
                Some(Token::Symbol('-')) => Op::Subtract,
                _ => break,
            };
            self.pos += 1;
            node = Node::binary(op, node, self.signed_term()?);
        }
        Ok(node)
    }

    /// A term and the signs in front of it, which apply to all of it: `-x^2/8` is `-(x^2/8)`.
    fn signed_term(&mut self) -> Result<Node, String> {
        let negative = self.signs();
        Ok(self.term()?.negate_if(negative))
    }

    fn term(&mut self) -> Result<Node, String> {
        let mut node = self.group()?;
        loop {
            let (op, explicit) = match self.peek() {
                Some(Token::Symbol('*')) => (Op::Multiply, true),
                Some(Token::Symbol('/')) => (Op::Divide, true),
                Some(Token::Command(name)) if matches!(name.as_str(), "cdot" | "times" | "ast") => {
                    (Op::Multiply, true)
                }
                Some(Token::Command(name)) if name == "div" => (Op::Divide, true),
                Some(token) if self.starts_operand(token) => (Op::Multiply, false),
                _ => break,
            };
            let right = if explicit {
                self.pos += 1;
                let negative = self.signs();
                self.group()?.negate_if(negative)
            } else {
                self.group()?
            };
            node = Node::binary(op, node, right);
        }
        Ok(node)
    }

    /// Operands the field writes side by side, which qalc multiplies first: `1/2x` is `1/(2x)`.
    fn group(&mut self) -> Result<Node, String> {
        let mut node = self.power()?;
        while self.syntax == Syntax::Field
            && !self.spaced.get(self.pos).copied().unwrap_or(false)
            && self.peek().is_some_and(|token| self.starts_operand(token))
        {
            node = Node::binary(Op::Multiply, node, self.power()?);
        }
        Ok(node)
    }

    fn power(&mut self) -> Result<Node, String> {
        let mut node = self.primary()?;
        loop {
            if self.eat(&Token::Symbol('^')) {
                node = if self.degrees() {
                    Node::Degrees(Box::new(node))
                } else {
                    Node::binary(Op::Power, node, self.exponent()?)
                };
            } else if self.peek() == Some(&Token::Symbol('!')) {
                // qalc reads `3!!` as a double factorial and drops the `!` of `|x|!`.
                let after = self.tokens.get(self.pos.wrapping_sub(1));
                if self.syntax == Syntax::Field && matches!(after, Some(Token::Symbol('!' | '|'))) {
                    return Err("!".to_string());
                }
                self.pos += 1;
                node = Node::Factorial(Box::new(node));
            } else if self.eat(&Token::Symbol('°')) {
                node = Node::Degrees(Box::new(node));
            } else {
                return Ok(node);
            }
        }
    }

    fn degrees(&mut self) -> bool {
        let circ = Token::Command("circ".to_string());
        if self.eat(&circ) {
            return true;
        }
        let braced = [Token::Symbol('{'), circ, Token::Symbol('}')];
        let found = self.tokens[self.pos..].starts_with(&braced);
        if found {
            self.pos += braced.len();
        }
        found
    }

    /// Read from the right as qalc does: `2^3^2` is `2^(3^2)`; in the field `2^3!` is `2^(3!)`.
    fn exponent(&mut self) -> Result<Node, String> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err("^".to_string());
        }
        let negative = self.signs();
        let node = match self.syntax {
            Syntax::Field => self.power()?,
            Syntax::Latex => {
                let base = self.primary()?;
                if self.eat(&Token::Symbol('^')) {
                    Node::binary(Op::Power, base, self.exponent()?)
                } else {
                    base
                }
            }
        };
        self.depth -= 1;
        Ok(node.negate_if(negative))
    }

    /// What follows `^` or `_`: a group or a single token, a whole number as people mean it.
    fn argument(&mut self) -> Result<Node, String> {
        let negative = self.signs();
        Ok(self.primary()?.negate_if(negative))
    }

    /// What follows `\frac` or `\sqrt`: like `argument`, but a number is one digit, as in LaTeX.
    fn strict_argument(&mut self) -> Result<Node, String> {
        if let Some(Token::Number(digits)) = self.tokens.get_mut(self.pos)
            && digits.len() > 1
            && digits.chars().all(|c| c.is_ascii_digit())
        {
            return Ok(Node::Number(digits.remove(0).to_string()));
        }
        self.argument()
    }

    fn bare_argument(&mut self) -> Result<Node, String> {
        let negative = self.signs();
        if self.syntax == Syntax::Field {
            let start = self.pos;
            self.bare += 1;
            let node = self.group()?;
            self.bare -= 1;
            // qalc gives a `!` there to the whole call: `sin x!` is `(sin x)!`.
            if self.tokens[start..self.pos].contains(&Token::Symbol('!')) {
                return Err("!".to_string());
            }
            return Ok(node.negate_if(negative));
        }
        let mut node = self.power()?;
        while self
            .peek()
            .is_some_and(|token| self.starts_bare_operand(token))
        {
            node = Node::binary(Op::Multiply, node, self.power()?);
        }
        Ok(node.negate_if(negative))
    }

    fn arguments(&mut self) -> Result<Vec<Node>, String> {
        let mut arguments = vec![self.expr()?];
        while self.eat(&Token::Symbol(',')) || self.eat(&Token::Symbol(';')) {
            arguments.push(self.expr()?);
        }
        self.expect(Token::Symbol(')'))?;
        Ok(arguments)
    }

    fn primary(&mut self) -> Result<Node, String> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err("(".to_string());
        }
        let node = self.operand()?;
        self.depth -= 1;
        Ok(node)
    }

    fn operand(&mut self) -> Result<Node, String> {
        match self.next()? {
            Token::Number(text) => Ok(Node::Number(if self.decimal_comma {
                text.replace('.', ",")
            } else {
                text
            })),
            Token::Name(name) => Ok(Node::Variable(self.variable(name))),
            Token::Symbol(open @ ('(' | '[' | '{')) => {
                let node = self.expr()?;
                let close = match open {
                    '(' => ')',
                    '[' => ']',
                    _ => '}',
                };
                self.expect(Token::Symbol(close))?;
                Ok(node)
            }
            Token::OpenBar | Token::Symbol('|') => {
                self.bars += 1;
                let node = self.expr()?;
                match self.next()? {
                    Token::CloseBar | Token::Symbol('|') => {}
                    other => return Err(describe(&other)),
                }
                self.bars -= 1;
                Ok(Node::call("abs", vec![node]))
            }
            Token::Function(name) => self.function(&name),
            // qalc gives a function word without parentheses there nothing to apply to: `√sin x`.
            Token::Command(name)
                if self.bare > 0
                    && (is_function(&name) || name == "sqrt")
                    && self.peek() != Some(&Token::Symbol('(')) =>
            {
                Err(name)
            }
            Token::Command(name) => self.command(&name),
            Token::Symbol('√') => self.command("sqrt"),
            other => Err(describe(&other)),
        }
    }

    fn command(&mut self, name: &str) -> Result<Node, String> {
        match name {
            "frac" | "dfrac" | "tfrac" => {
                let numerator = self.strict_argument()?;
                let denominator = self.strict_argument()?;
                let derivative = numerator.leads_with_d()
                    && denominator.leads_with_d()
                    && matches!(denominator, Node::Binary(..));
                if derivative {
                    return Err("d/dx".to_string());
                }
                Ok(Node::binary(Op::Divide, numerator, denominator))
            }
            "binom" | "dbinom" | "tbinom" => {
                let total = self.strict_argument()?;
                let chosen = self.strict_argument()?;
                Ok(Node::call("binomial", vec![total, chosen]))
            }
            // The field's `√` is qalc's, a function: `√2x` is `√(2x)`.
            "sqrt" if self.syntax == Syntax::Field => self.function(name),
            "sqrt" => {
                let index = if self.eat(&Token::Symbol('[')) {
                    let index = self.expr()?;
                    self.expect(Token::Symbol(']'))?;
                    Some(index)
                } else {
                    None
                };
                let radicand = self.strict_argument()?;
                Ok(match index {
                    Some(index) => Node::call("root", vec![radicand, index]),
                    None => Node::call("sqrt", vec![radicand]),
                })
            }
            "pi" | "tau" | "infinity" => Ok(Node::Variable(name.to_string())),
            "lfloor" | "lceil" => {
                let (close, function) = if name == "lfloor" {
                    ("rfloor", "floor")
                } else {
                    ("rceil", "ceil")
                };
                let node = self.expr()?;
                self.expect(Token::Command(close.to_string()))?;
                Ok(Node::call(function, vec![node]))
            }
            "sum" | "prod" => self.big_operator(name),
            "int" => self.integral(),
            _ if is_function(name) => self.function(name),
            _ => Err(format!("\\{name}")),
        }
    }

    fn function(&mut self, name: &str) -> Result<Node, String> {
        // qalc reads `sin^2(x)` and `log_2(x)` otherwise than LaTeX does.
        if self.syntax == Syntax::Field && matches!(self.peek(), Some(Token::Symbol('^' | '_'))) {
            return Err(name.to_string());
        }
        let (mut power, mut base) = (None, None);
        loop {
            if self.eat(&Token::Symbol('^')) {
                power = Some(self.argument()?);
            } else if self.eat(&Token::Symbol('_')) {
                base = Some(self.argument()?);
            } else {
                break;
            }
        }
        let mut arguments = if self.eat(&Token::Symbol('(')) {
            self.arguments()?
        } else {
            vec![self.bare_argument()?]
        };
        let (mut qalc, inverse) = qalc_function(name, self.syntax);
        if let Some(base) = base {
            if name != "log" {
                return Err("_".to_string());
            }
            arguments.push(base);
        }
        if name == "log" && arguments.len() > 1 {
            qalc = "log".to_string();
        }
        Ok(match (power, inverse) {
            (Some(Node::Negate(one)), Some(inverse)) if matches!(&*one, Node::Number(digits) if digits == "1") => {
                Node::Call(inverse, arguments)
            }
            (Some(power), _) => Node::binary(Op::Power, Node::Call(qalc, arguments), power),
            (None, _) => Node::Call(qalc, arguments),
        })
    }

    fn bind(&mut self, variable: char, command: &str) -> Result<char, String> {
        // A letter the formula writes stays free: `\sum_{k=1}^{3} ky` keeps its own `y`.
        let name = BOUND
            .into_iter()
            .find(|name| {
                !self.tokens.contains(&Token::Name(*name))
                    && self.scopes.iter().all(|(_, used)| used != name)
            })
            .ok_or_else(|| command.to_string())?;
        self.scopes.push((variable, name));
        Ok(name)
    }

    fn limits<T>(
        &mut self,
        mut lower: impl FnMut(&mut Self) -> Result<T, String>,
    ) -> Result<(Option<T>, Option<Node>), String> {
        let (mut start, mut end) = (None, None);
        loop {
            if self.eat(&Token::Symbol('_')) {
                start = Some(lower(self)?);
            } else if self.eat(&Token::Symbol('^')) {
                end = Some(self.argument()?);
            } else {
                return Ok((start, end));
            }
        }
    }

    fn counter(&mut self) -> Result<(char, Node), String> {
        self.expect(Token::Symbol('{'))?;
        let variable = match self.next()? {
            Token::Name(variable) => variable,
            other => return Err(describe(&other)),
        };
        self.expect(Token::Symbol('='))?;
        let start = self.expr()?;
        self.expect(Token::Symbol('}'))?;
        Ok((variable, start))
    }

    fn big_operator(&mut self, name: &str) -> Result<Node, String> {
        let command = format!("\\{name}");
        let ((variable, start), end) = match self.limits(Self::counter)? {
            (Some(counter), Some(end)) => (counter, end),
            _ => return Err(command),
        };
        let bound = self.bind(variable, &command)?;
        let body = self.term()?;
        self.scopes.pop();
        let function = if name == "sum" { "sum" } else { "product" };
        Ok(Node::call(
            function,
            vec![body, start, end, Node::Variable(bound.to_string())],
        ))
    }

    fn integral(&mut self) -> Result<Node, String> {
        let command = "\\int".to_string();
        let (start, end) = match self.limits(Self::argument)? {
            (Some(start), Some(end)) => (start, end),
            _ => return Err(command),
        };
        let variable = self.differential().ok_or_else(|| command.clone())?;
        let bound = self.bind(variable, &command)?;
        self.integrals += 1;
        let body = self.expr()?;
        self.integrals -= 1;
        self.expect(Token::Name('d'))?;
        self.expect(Token::Name(variable))?;
        self.scopes.pop();
        Ok(Node::call(
            "integrate",
            vec![body, start, end, Node::Variable(bound.to_string())],
        ))
    }

    fn differential(&self) -> Option<char> {
        let (mut depth, mut nested) = (0_usize, 0_usize);
        for (i, token) in self.tokens.iter().enumerate().skip(self.pos) {
            match token {
                Token::Symbol('(' | '[' | '{') | Token::OpenBar => depth += 1,
                Token::Symbol(')' | ']' | '}') | Token::CloseBar => depth = depth.checked_sub(1)?,
                Token::Command(name) if name == "int" => nested += 1,
                Token::Name('d') if depth == 0 => {
                    if let Some(Token::Name(variable)) = self.tokens.get(i + 1) {
                        if nested == 0 {
                            return Some(*variable);
                        }
                        nested -= 1;
                    }
                }
                _ => {}
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, to_expression};

    fn read(latex: &str) -> String {
        to_expression(latex, false).unwrap()
    }

    fn refused(latex: &str) -> String {
        to_expression(latex, false).unwrap_err()
    }

    fn field(text: &str) -> Result<String, String> {
        let node = parse(text, false)?;
        let mut expression = String::new();
        node.write(&mut expression, 0, false);
        Ok(expression)
    }

    #[test]
    fn reads_the_field_as_qalc_does() {
        let cases = [
            ("1e-3*x+2E3+2e-x", "1e-3*x+2E3+2*e-x"),
            ("log(x)+log(x, 2)", "log(x)+log(x, 2)"),
            ("2^3^2+2^-3^2+x²³", "2^(3^2)+2^(-3^2)+x^23"),
            ("2^3!", "2^(3!)"),
            ("1/2x+1/2 x", "1/(2*x)+1/2*x"),
            ("√2x+√2 x+sin 2x", "sqrt(2*x)+sqrt(2)*x+sin(2*x)"),
        ];
        for (text, expected) in cases {
            assert_eq!(field(text), Ok(expected.to_string()), "{text}");
        }
        for text in [
            "0x10", "1 000", "1e3.5", "sin^2(x)", "log_2(x)", "3!!", "|x|!", "sin x!", "√sin x",
        ] {
            assert!(field(text).is_err(), "{text}");
        }
    }

    #[test]
    fn keeps_what_the_field_already_holds() {
        assert_eq!(read(r"6.02E23\cdot 2^3^2"), "6.02E23*2^(3^2)");
        assert_eq!(read(r"\log x+x^2!"), "log10(x)+(x^2)!");
    }

    #[test]
    fn reads_the_notation_people_paste() {
        let cases = [
            (
                r"\frac{(x^2-4)(x^2-1)}{10}+\sin(3x)\cos(2x)+\frac{e^{-x^2/8}\sin(5x)}{2}+\ln(x^2+1)-\frac{\sqrt{x+5}}{3}",
                "(x^2-4)*(x^2-1)/10+sin(3*x)*cos(2*x)+e^(-x^2/8)*sin(5*x)/2+ln(x^2+1)-sqrt(x+5)/3",
            ),
            (r"\sin^2(x)+\cos^{2}x", "sin(x)^2+cos(x)^2"),
            (r"\sin^{-1}(x)+\arctan x", "asin(x)+atan(x)"),
            (r"\sin 2x+\cos\frac{x}{2}", "sin(2*x)+cos(x/2)"),
            (r"\frac12+\frac{1}{2x}", "1/2+1/(2*x)"),
            (r"\sqrt[3]{x}+\sqrt2", "root(x, 3)+sqrt(2)"),
            (r"|x|+\left|x-1\right|", "abs(x)+abs(x-1)"),
            (r"||x|-1|", "abs(abs(x)-1)"),
            (r"\lfloor x\rfloor+\lceil x\rceil", "floor(x)+ceil(x)"),
            (r"\log_2 x+\log x+\ln x", "log(x, 2)+log10(x)+ln(x)"),
            (r"\operatorname{erf}(x)+\Gamma(x)", "erf(x)+gamma(x)"),
            (r"\binom{5}{2}x", "binomial(5, 2)*x"),
            (r"\sin(x^\circ)", "sin(x°)"),
            (r"x^2!+(x+1)!", "(x^2)!+(x+1)!"),
            (r"e^{\sin(x)}", "e^(sin(x))"),
            (r"-x^2/8+2^{-x}", "-x^2/8+2^(-x)"),
            (r"a\cdot b\times c\div d", "a*b*c/d"),
            (r"\pi x+2\pi", "pi*x+2*pi"),
            (
                r"\sum_{k=1}^{20}\frac{\sin(kx)}{k}",
                "sum(sin(y*x)/y, 1, 20, y)",
            ),
            (r"\prod_{n=1}^{4}(x+n)", "product(x+y, 1, 4, y)"),
            (
                r"\sum_{k=1}^{3}\sum_{j=1}^{2}kjx",
                "sum(sum(y*z*x, 1, 2, z), 1, 3, y)",
            ),
            (r"\int_0^x e^{-t^2}\,dt", "integrate(e^(-y^2), 0, x, y)"),
            (
                r"\int_0^x\int_0^t 1\,ds\,dt",
                "integrate(integrate(1, 0, y, z), 0, x, y)",
            ),
            (
                r"\int_0^\infty e^{-xt}\,dt",
                "integrate(e^(-x*y), 0, infinity, y)",
            ),
            (
                r"\int_{-\infty}^{+\infty} e^{-t^2}\,dt",
                "integrate(e^(-y^2), -infinity, infinity, y)",
            ),
            (
                r"\sum_{k=1}^{\infty}\frac{1}{k^2}",
                "sum(1/y^2, 1, infinity, y)",
            ),
        ];
        for (latex, expected) in cases {
            assert_eq!(read(latex), expected, "{latex}");
            // What it writes reads back the same, as when more LaTeX is pasted after it.
            assert_eq!(read(expected), expected, "{expected}");
        }
    }

    #[test]
    fn reads_the_spellings_already_in_the_field() {
        assert_eq!(read("sin(x)**2+x²"), "sin(x)^2+x^2");
        assert_eq!(read("√(1−x²)·|x|"), "sqrt(1-x^2)*abs(x)");
        assert_eq!(read(r"\left(x+1\right)^{2}"), "(x+1)^2");
        assert_eq!(read(r"$\frac{1}{2}$"), "1/2");
        assert_eq!(
            read(r"\int_0^∞ e^{-t}\,dt"),
            "integrate(e^(-y), 0, infinity, y)"
        );
    }

    #[test]
    fn writes_decimals_and_separators_for_the_locale() {
        assert_eq!(
            to_expression(r"\max(1.5x, 2)", true).unwrap(),
            "max(1,5*x; 2)"
        );
        assert_eq!(to_expression(r"\frac{x}{2.5}", true).unwrap(), "x/2,5");
    }

    #[test]
    fn bound_variables_never_take_a_letter_the_formula_writes() {
        assert_eq!(read(r"\sum_{k=1}^{3} ky"), "sum(z*y, 1, 3, z)");
        assert_eq!(read(r"\sum_{k=1}^{y} k"), "sum(z, 1, y, z)");
        assert_eq!(read(r"\int_0^y y\,dy"), "integrate(z, 0, y, z)");
        assert_eq!(
            read(r"\sum_{k=1}^{2}\sum_{j=1}^{2} kjz"),
            "sum(sum(y*n*z, 1, 2, n), 1, 2, y)"
        );
        assert_eq!(refused(r"\sum_{k=1}^{2} kyzn"), r"\sum");
    }

    #[test]
    fn names_what_it_cannot_read() {
        assert_eq!(refused(r"\begin{cases}x\end{cases}"), r"\begin");
        assert_eq!(refused(r"\alpha x"), r"\alpha");
        assert_eq!(refused(r"x \le 2"), r"\le");
        assert_eq!(refused(r"\lim_{x\to 0}\frac{\sin x}{x}"), r"\lim");
        assert_eq!(refused("x_1"), "_");
        assert_eq!(refused("2@x"), "@");
        // A derivative would otherwise read as the fraction d/(d·x).
        assert_eq!(refused(r"\frac{d}{dx}(x^2)"), "d/dx");
        assert_eq!(refused("(x+1"), ")");
        assert_eq!(refused(r"\frac{1}"), "…");
        assert_eq!(refused(r"\int x\,dx"), r"\int");
        assert_eq!(refused(r"\sum_{k=1} k"), r"\sum");
    }

    #[test]
    fn refuses_what_could_exhaust_the_stack() {
        assert!(to_expression(&"(".repeat(10_000), false).is_err());
        assert!(to_expression(&format!("{}x{}", "(".repeat(60), ")".repeat(60)), false).is_err());
        assert!(to_expression(&format!("x{}", "!".repeat(10_000)), false).is_err());
        assert!(to_expression(&r"\sin ".repeat(10_000), false).is_err());
        assert!(to_expression(&"1+".repeat(10_000), false).is_err());
        let powers = format!("{}2", "2^".repeat(600));
        assert!(to_expression(&powers, false).is_err());
        assert!(parse(&powers, false).is_err());
    }
}
