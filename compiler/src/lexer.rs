use crate::error::BobaError;
use std::iter::Peekable;
use std::str::CharIndices;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Semicolon,
    LeftBrace,
    RightBrace,
    Comma,
    Percent,
    Colon,
    Comment,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Arrow,
    AddAssign,
    SubAssign,
    MultiplyAssign,
    DivideAssign,

    Number(i64),
    Boolean(bool),
    Unknown,
    Identifier(String),
    StringLiteral(String),
    Character(char),
    Print,
    Let,
    Mutable,
    Nil,
    If,
    Else,
    While,
    For,
    Fn,
    Return,
    And,
    Or,
    Type(String),
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenType::*;
        match self {
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            Dot => write!(f, "."),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Star => write!(f, "*"),
            Semicolon => write!(f, ";"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            Comma => write!(f, ","),
            Percent => write!(f, "%"),
            Slash => write!(f, "/"),
            Bang => write!(f, "!"),
            BangEqual => write!(f, "!="),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Number(num) => write!(f, "{num}"),
            Boolean(value) => write!(f, "{value}"),
            Unknown => write!(f, "unknown token"),
            Identifier(name) => write!(f, "{name}"),
            StringLiteral(lexeme) => write!(f, "{lexeme}"),
            Print => write!(f, "keyword print"),
            Let => write!(f, "keyword Let"),
            Nil => write!(f, "keyword nil"),
            If => write!(f, "keyword if"),
            Else => write!(f, "keyword else"),
            While => write!(f, "keyword while"),
            For => write!(f, "keyword for"),
            Fn => write!(f, "keyword fn"),
            Return => write!(f, "keyword return"),
            And => write!(f, "keyword and"),
            Or => write!(f, "keyword or"),
            Arrow => write!(f, "Arrow"),
            Character(c) => write!(f, "Character({c})"),
            Mutable => write!(f, "keyword mut"),
            Colon => write!(f, ":"),
            AddAssign => write!(f, "AddAssign"),
            SubAssign => write!(f, "SubtractAssign"),
            MultiplyAssign => write!(f, "MultiplyAssign"),
            DivideAssign => write!(f, "DivideAssign"),
            Comment => write!(f, "Comment"),
            Type(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.span.hash(state);
    }
}

impl Eq for Token {}

#[derive(Default, Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Default, Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Position {
    pub line_number: u16,
    pub column_number: u16,
}

impl Token {
    pub const fn new(kind: TokenType, span: Span) -> Self {
        Self { kind, span }
    }

    pub const fn is_identifier(&self) -> bool {
        matches!(self.kind, TokenType::Identifier(_))
    }

    pub const fn is_type(&self) -> bool {
        matches!(self.kind, TokenType::Type(_))
    }

    pub fn identifier_name(&self) -> String {
        if let TokenType::Identifier(name) = &self.kind {
            name.clone()
        } else {
            panic!("Token is not an identifier")
        }
    }
}

pub struct Lexer<'src> {
    cursor: Peekable<CharIndices<'src>>,
    tokens: Vec<Token>,
    line: u16,
    column: u16,
    start_of_line: u16,
}

impl<'src> Lexer<'src> {
    pub fn new(text: &'src str) -> Self {
        Self {
            cursor: text.char_indices().peekable(),
            line: 1,
            column: 0,
            start_of_line: 0,
            tokens: Vec::with_capacity(text.len()),
        }
    }

    fn make_span(&mut self, start: usize, end: usize) -> Span {
        let end = start + end;
        let start_column = self.column(start);
        let end_column = self.column(end);

        let start = Position {
            line_number: self.line,
            column_number: start_column,
        };

        let end = Position {
            line_number: self.line,
            column_number: end_column,
        };

        Span { start, end }
    }

    fn next_line(&mut self, index: usize) {
        self.line += 1;
        self.start_of_line = (index + 1) as u16;
    }

    fn column(&mut self, start_pos: usize) -> u16 {
        self.column = start_pos as u16 - self.start_of_line;
        self.column
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, BobaError> {
        while let Some(&(start_pos, c)) = self.cursor.peek() {
            match c {
                '(' | ')' | '.' | ';' | '{' | '}' | ',' | '%' | ':' => {
                    self.scan_single_token()
                }
                '!' | '=' | '>' | '<' | '-' | '+' | '*' | '/' => {
                    self.scan_double_token()
                }
                ' ' | '\r' | '\t' => {
                    self.cursor.next();
                }
                '\n' => {
                    let (index, _) = self.cursor.next().unwrap();
                    self.next_line(index);
                }
                '\'' => {
                    let token = self.scan_character()?;
                    self.tokens.push(token);
                }
                '"' => {
                    let token = self.scan_string()?;
                    self.tokens.push(token);
                }
                _ => {
                    if c.is_ascii_digit() {
                        self.scan_number(start_pos)
                    } else if c.is_ascii_alphanumeric() || c == '_' {
                        self.scan_identifier(start_pos);
                    } else {
                        self.cursor.next();
                        let span = self.make_span(start_pos, c.len_utf8());
                        self.tokens.push(Token::new(TokenType::Unknown, span));
                    }
                }
            }
        }
        Ok(self.tokens.clone())
    }

    fn scan_single_token(&mut self) {
        let (start_pos, c) = self.cursor.next().unwrap();
        let kind = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '.' => TokenType::Dot,
            ';' => TokenType::Semicolon,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '%' => TokenType::Percent,
            ':' => TokenType::Colon,
            _ => unreachable!(),
        };
        let span = self.make_span(start_pos, c.len_utf8());
        self.tokens.push(Token::new(kind, span));
    }

    fn scan_comment(&mut self) -> Token {
        for (index, ch) in self.cursor.by_ref() {
            if ch == '\n' {
                self.next_line(index);
                return Token::new(
                    TokenType::Comment,
                    self.make_span(self.start_of_line as usize, index),
                );
            }
        }
        Token {
            kind: TokenType::Comment,
            span: Span::default(),
        }
    }

    fn check_next_is(
        &mut self,
        start_pos: usize,
        len: usize,
        this: TokenType,
        that: TokenType,
        expected: char,
    ) -> Token {
        if let Some((_, ch)) = self.cursor.next_if(|x| x.1 == expected) {
            Token::new(this, self.make_span(start_pos, ch.len_utf8()))
        } else {
            Token::new(that, self.make_span(start_pos, len))
        }
    }

    fn check_next(
        &mut self,
        start_pos: usize,
        len: usize,
        this: TokenType,
        that: TokenType,
    ) -> Token {
        self.check_next_is(start_pos, len, this, that, '=')
    }

    fn scan_double_token(&mut self) {
        let (start_pos, c) = self.cursor.next().unwrap();
        let token = match c {
            '-' => {
                if let Some((_, ch)) = self.cursor.next_if(|x| x.1 == '=') {
                    Token::new(
                        TokenType::SubAssign,
                        self.make_span(start_pos, ch.len_utf8()),
                    )
                } else if let Some((_, ch)) =
                    self.cursor.next_if(|x| x.1 == '>')
                {
                    Token::new(
                        TokenType::Arrow,
                        self.make_span(start_pos, ch.len_utf8()),
                    )
                } else {
                    Token::new(
                        TokenType::Minus,
                        self.make_span(start_pos, c.len_utf8()),
                    )
                }
            }
            '+' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::AddAssign,
                TokenType::Plus,
            ),
            '*' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::MultiplyAssign,
                TokenType::Star,
            ),
            '/' => {
                if let Some((_, ch)) = self.cursor.next_if(|x| x.1 == '=') {
                    Token::new(
                        TokenType::DivideAssign,
                        self.make_span(start_pos, ch.len_utf8()),
                    )
                } else if self.cursor.next_if(|x| x.1 == '/').is_some() {
                    self.scan_comment()
                } else {
                    Token::new(
                        TokenType::Slash,
                        self.make_span(start_pos, c.len_utf8()),
                    )
                }
            }
            '!' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::BangEqual,
                TokenType::Bang,
            ),
            '=' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::EqualEqual,
                TokenType::Equal,
            ),
            '>' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::GreaterEqual,
                TokenType::Greater,
            ),
            '<' => self.check_next(
                start_pos,
                c.len_utf8(),
                TokenType::LessEqual,
                TokenType::Less,
            ),
            _ => unreachable!(),
        };
        
        if token.kind != TokenType::Comment {
            self.tokens.push(token);
        }
    }

    fn scan_character(&mut self) -> Result<Token, BobaError> {
        let (start_pos, _) = self.cursor.next().unwrap(); // consume the starting single quote
        if let Some((_end_pos, character)) = self.cursor.next() {
            if self.cursor.next_if(|x| x.1 == '\'').is_some() {
                Ok(Token::new(
                    TokenType::Character(character),
                    self.make_span(start_pos, character.len_utf8()),
                ))
            } else {
                Err(BobaError::UnterminatedCharacter(
                    self.make_span(start_pos, character.len_utf8()),
                ))
            }
        } else {
            Err(BobaError::EmptyCharacter(self.make_span(start_pos, 1)))
        }
    }

    fn scan_string(&mut self) -> Result<Token, BobaError> {
        let mut lexeme = String::from("");
        let (start_pos, _) = self.cursor.next().unwrap(); // skip opening quotes
        let start = start_pos + 1;
        while let Some((_, ch)) = self.cursor.next_if(|x| x.1 != '"') {
            lexeme.push(ch);
        }
        let length = lexeme.len();
        if self.cursor.peek().map_or(false, |x| x.1 == '"') {
            let _ = self.cursor.next();
            Ok(Token::new(
                TokenType::StringLiteral(lexeme),
                self.make_span(start, length),
            ))
        } else {
            Err(BobaError::UnterminatedString(self.make_span(start, length)))
        }
    }

    fn scan_number(&mut self, start_pos: usize) {
        let mut lexeme = String::from("");
        while let Some((_, num)) = self.cursor.next_if(|x| x.1.is_ascii_digit())
        {
            lexeme.push(num);
        }
        if self.cursor.peek().map_or(false, |x| x.1 == '.') {
            lexeme.push('.');
            let _ = self.cursor.next();
            while let Some((_, num)) =
                self.cursor.next_if(|x| x.1.is_ascii_digit())
            {
                lexeme.push(num);
            }
        }
        let num = lexeme.parse::<i64>().expect("Unable to parse number.");
        let span = self.make_span(start_pos, lexeme.len());
        self.tokens.push(Token::new(TokenType::Number(num), span));
    }

    fn scan_identifier(&mut self, start_pos: usize) {
        let mut lexeme = String::from("");
        while let Some((_, ch)) =
            self.cursor.next_if(|x| x.1.is_ascii_alphanumeric())
        {
            lexeme.push(ch);
        }
        let len = lexeme.len();
        let kind = match lexeme.as_str() {
            "and" => TokenType::And,
            "else" => TokenType::Else,
            "false" => TokenType::Boolean(false),
            "for" => TokenType::For,
            "fn" => TokenType::Fn,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "println" => TokenType::Print,
            "return" => TokenType::Return,
            "true" => TokenType::Boolean(true),
            "let" => TokenType::Let,
            "mut" => TokenType::Mutable,
            "while" => TokenType::While,
            "String" | "Number" | "Bool" | "Char" => TokenType::Type(lexeme),
            _ => TokenType::Identifier(lexeme),
        };
        let span = self.make_span(start_pos, len);
        self.tokens.push(Token::new(kind, span));
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    fn test_runner(src: &str, expected_tokens: &[TokenType]) -> bool {
        match Lexer::new(src).scan() {
            Ok(tokens) => {
                let output = tokens
                    .iter()
                    .map(|t| t.kind.clone())
                    .collect::<Vec<TokenType>>();
                output.as_slice() == expected_tokens
            }
            Err(error) => {
                eprintln!("{error:?}");
                false
            }
        }
    }

    #[test]
    fn single_char() {
        assert!(test_runner(".", &[TokenType::Dot]))
    }

    #[test]
    fn double_char() {
        assert!(test_runner("==", &[TokenType::EqualEqual]))
    }

    #[test]
    fn keyword() {
        assert!(test_runner(
            "while () {}",
            &[
                TokenType::While,
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace
            ]
        ));
    }

    #[test]
    fn string() {
        assert!(test_runner(
            "\"hello\"",
            &[TokenType::StringLiteral("hello".into())]
        ))
    }

    #[test]
    fn numbers() {
        assert!(test_runner("1", &[TokenType::Number(1)]));
        assert!(test_runner("123", &[TokenType::Number(123)]));
        assert!(test_runner("1.00", &[TokenType::Number(1)]));
    }

    #[test]
    fn bools() {
        assert!(test_runner(
            "false and true",
            &[
                TokenType::Boolean(false),
                TokenType::And,
                TokenType::Boolean(true),
            ]
        ));
    }

    #[test]
    fn identifier() {
        assert!(test_runner(
            "human",
            &[TokenType::Identifier("human".into())]
        ));
    }

    #[test]
    fn function_declarations() {
        assert!(test_runner(
            "
fn factorial(num: Number) -> Number {
    let a = 1 + 2;
}
",
            &[
                TokenType::Fn,
                TokenType::Identifier("factorial".into()),
                TokenType::LeftParen,
                TokenType::Identifier("num".into()),
                TokenType::Colon,
                TokenType::Identifier("Number".into()),
                TokenType::RightParen,
                TokenType::Arrow,
                TokenType::Identifier("Number".into()),
                TokenType::LeftBrace,
                TokenType::Let,
                TokenType::Identifier("a".into()),
                TokenType::Equal,
                TokenType::Number(1),
                TokenType::Plus,
                TokenType::Number(2),
                TokenType::Semicolon,
                TokenType::RightBrace,
            ]
        ))
    }

    #[test]
    fn character_literals() {
        assert!(test_runner("'a'", &[TokenType::Character('a')]))
    }
}
