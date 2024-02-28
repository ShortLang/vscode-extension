use std::fmt;

use chumsky::{prelude::*, text::newline, Stream};

use crate::{
    errors::{Error, Pattern},
    span::Span,
};

#[derive(Debug, Hash, Clone, PartialEq, Eq, Copy)]
pub enum Op {
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Add,
    Sub,
    Times,
    Div,
    Increase,
    Decrease,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Greater => ">",
                Self::Less => "<",
                Self::GreaterEq => ">=",
                Self::LessEq => "<=",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Times => "*",
                Self::Div => "/",
                Self::Increase => "++",
                Self::Decrease => "--",
                Self::AddEq => "+=",
                Self::SubEq => "-=",
                Self::MulEq => "*=",
                Self::DivEq => "/=",
            }
        )
    }
}
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,
    Square,
    Brace,
}

#[derive(Eq, Debug, Clone, PartialEq, Hash)]
pub enum Token {
    Op(Op),
    Open(Delimiter),
    Close(Delimiter),
    Int(String),
    Float(String),
    Str(String),
    Bool(bool),
    Ident(String),
    Nil,
    Comment,
    Error(char),
    Newline,
    Equal,
    Colon,
    Semicolon,
    Dollar,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Open(d) => {
                write!(
                    f,
                    "`{}`",
                    match d {
                        Delimiter::Paren => "(",
                        Delimiter::Square => "[",
                        Delimiter::Brace => "{{",
                    }
                )
            }
            Self::Close(d) => {
                write!(
                    f,
                    "`{}`",
                    match d {
                        Delimiter::Paren => ")",
                        Delimiter::Square => "]",
                        Delimiter::Brace => "}}",
                    }
                )
            }
            Self::Dollar => write!(f, "`$`"),
            Self::Newline => write!(f, "newline"),
            Self::Semicolon => write!(f, "`;`"),
            Self::Ident(i) => write!(f, "`{}`", i),
            Self::Op(op) => write!(f, "`{}`", op),
            Self::Int(i) => write!(f, "`{}`", i),
            Self::Float(float) => write!(f, "`{}`", float),
            Self::Bool(b) => write!(f, "`{}`", b),
            Self::Str(string) => write!(f, "`{:?}`", string),
            Self::Comment => write!(f, "comment"),
            Self::Nil => write!(f, "`nil`"),
            Self::Error(c) => write!(f, "`{}`", c),
            Self::Equal => write!(f, "`=`"),
            Self::Colon => write!(f, "`:`"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    let int = text::int(10).map(Token::Int);
    let float = text::int(10)
        .or_not()
        .then(just('.').ignore_then(text::digits(10).or_not()))
        .map(|(a, b)| {
            Token::Float(format!(
                "{}.{}",
                a.unwrap_or("".to_string()),
                b.unwrap_or("".to_string())
            ))
        });
    let hex = choice((one_of("0123456789"), one_of("abcdef"), one_of("ABCDEF")))
        .repeated()
        .collect::<Vec<_>>()
        .map(|x| String::from_utf8(x.iter().map(|e| *e as u8).collect::<Vec<_>>()).unwrap());
    let hex_num = just("0x").ignore_then(hex.clone()).map(Token::Int);
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('x').ignore_then(hex).map(|n| {
                let hex = u32::from_str_radix(&n, 16).unwrap();
                hex as u8 as char
            }))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("string");

    let operator = choice((
        just(">").to(Op::Greater),
        just("<").to(Op::Less),
        just(">=").to(Op::GreaterEq),
        just("<=").to(Op::LessEq),
        just("+").to(Op::Add),
        just("-").to(Op::Sub),
        just("*").to(Op::Times),
        just("/").to(Op::Div),
        just("++").to(Op::Increase),
        just("--").to(Op::Decrease),
        just("+=").to(Op::AddEq),
        just("-=").to(Op::SubEq),
        just("*=").to(Op::MulEq),
        just("/=").to(Op::DivEq),
    ))
    .map(Token::Op);

    let ident = text::ident().map(Token::Ident);

    let comment = just("#")
        .then_ignore(none_of('\n').ignored().repeated().ignored())
        .map(|_| Token::Comment);

    let newline = just('\n').map(|_| Token::Newline);

    let delimiters = choice((
        just('(').to(Token::Open(Delimiter::Paren)),
        just(')').to(Token::Close(Delimiter::Paren)),
        just('[').to(Token::Open(Delimiter::Square)),
        just(']').to(Token::Close(Delimiter::Square)),
        just('{').to(Token::Open(Delimiter::Brace)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    let symbols = choice((
        just(';').to(Token::Semicolon),
        just(':').to(Token::Colon),
        just('=').to(Token::Equal),
        just('$').to(Token::Dollar),
    ));

    let token = choice((
        float, int, hex_num, ident, string, operator, newline, delimiters, symbols, comment,
    ))
    .or(any().map(Token::Error).validate(|t, span, emit| {
        emit(Error::expected_input_found(span, None, Some(t.clone())));
        t
    }));

    let ws = just(' ').or(just('\r')).or(just('\t'));

    let token = token
        .map_with_span(|token, span| (token, span))
        .padded_by(ws.or_not())
        .recover_with(skip_then_retry_until([]));

    token.repeated().padded_by(ws.or_not()).then_ignore(end())
}

macro_rules! p {
    ($expr: ty) => {
        impl chumsky::Parser<Token, $expr, Error = Error>
    };
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    String(String),
    Ident(String),
    Error,
}

#[derive(Debug, Clone)]
pub struct Expr {
    inner: ExprKind,
    span: Span,
}
pub fn nested_parser<'a, T: 'a>(
    parser: impl Parser<Token, T, Error = Error> + 'a,
    delimiter: Delimiter,
    f: impl Fn(Span) -> T + Clone + 'a,
) -> impl Parser<Token, T, Error = Error> + 'a {
    parser
        .delimited_by(just(Token::Open(delimiter)), just(Token::Close(delimiter)))
        .recover_with(nested_delimiters(
            Token::Open(delimiter),
            Token::Close(delimiter),
            [
                (
                    Token::Open(Delimiter::Paren),
                    Token::Close(Delimiter::Paren),
                ),
                (
                    Token::Open(Delimiter::Square),
                    Token::Close(Delimiter::Square),
                ),
                (
                    Token::Open(Delimiter::Brace),
                    Token::Close(Delimiter::Brace),
                ),
            ],
            f,
        ))
        .boxed()
}

pub fn parser() -> p!(Vec<Expr>) {
    let literal = select! {
        Token::Int(n) => ExprKind::Int(n.parse().unwrap()),
        Token::Float(f) => ExprKind::Float(f.parse().unwrap()),
        Token::Str(s) => ExprKind::String(s),
        Token::Ident(ident) => ExprKind::Ident(ident),
    }
    .map_err(|e: Error| e.expected(Pattern::Literal))
    .map_with_span(|lit, span| Expr { inner: lit, span });

    literal.repeated().collect::<Vec<_>>()
}
#[test]
fn e() {
    let src = String::from(r#""Hello, World!\n" 5 5.6 100 ="#);
    let len = src.len();
    let span = |i| Span::new(i, i + 1, "file".into());
    let stream = Stream::from_iter(
        span(len),
        src.chars().enumerate().map(|(i, c)| (c, span(i))),
    );
    let l = lexer().parse_recovery(stream);
    match l.0 {
        Some(x) => {
            let output = parser().parse_recovery(Stream::from_iter(span(x.len()), x.into_iter()));
            println!("{:?}", output);
        }
        None => {}
    }
    println!("{:?}", l.1);
}
