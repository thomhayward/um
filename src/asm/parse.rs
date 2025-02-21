// Copyright (C) 2025 Thom Hayward.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.
//
use super::Token;
use crate::Register;
use logos::{Logos, Source};
use std::{borrow::Cow, collections::HashMap, iter::Peekable, ops::Range, str::CharIndices};

pub fn parse(_unit: impl std::fmt::Display, source: &str) -> Result<ParsedProgram, Error> {
    Parser::new(source).parse()
}

#[derive(Debug)]
pub enum NodeType<'s> {
    Pragma(Pragma<'s>),
    Instruction(Instruction<'s>),
    Comment(#[allow(unused)] &'s str),
}

impl NodeType<'_> {
    pub fn size(&self) -> usize {
        match self {
            Self::Pragma(pragma) => match &pragma.payload {
                PragmaType::U32 { .. } => 1,
                PragmaType::WideString { value } => value.len() + 1,
            },
            // Instructions are always one platter.
            Self::Instruction(_) => 1,
            Self::Comment(_) => 0,
        }
    }
}

#[derive(Debug)]
pub struct Node<'s> {
    pub labels: Vec<&'s str>,
    pub entity: NodeType<'s>,
    #[allow(unused)]
    pub span: Range<usize>,
}

impl Node<'_> {
    /// Compute encoded size of the node in platters.
    #[inline]
    pub fn size(&self) -> usize {
        self.entity.size()
    }
}

#[derive(Debug)]
pub struct ParsedProgram<'s> {
    #[allow(unused)]
    pub source: &'s str,
    nodes: Vec<Node<'s>>,
}

impl<'s> ParsedProgram<'s> {
    pub fn nodes(&self) -> &[Node<'s>] {
        &self.nodes
    }
}

#[derive(Debug, Default)]
pub struct Parser<'s> {
    source: &'s str,
    labels: HashMap<&'s str, Range<usize>>,
    active_labels: Vec<&'s str>,
}

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            source,
            ..Default::default()
        }
    }

    fn parse(mut self) -> Result<ParsedProgram<'s>, Error> {
        let mut lexer = Token::lexer(self.source);
        let mut spanned = vec![];
        while let Some(res) = lexer.next() {
            match res {
                Ok(token) => {
                    spanned.push((token, lexer.span()));
                }
                Err(error) => Err(Error::new(format!("lex: {error:?}"), &lexer.span()))?,
            }
        }

        let mut nodes = vec![];
        let mut tokens = spanned.into_iter().peekable();
        while let Some((token, span)) = tokens.peek() {
            let node = match token {
                Token::Label(_) => {
                    self.consume_label(&mut tokens)?;
                    continue;
                }
                Token::Pragma(_) => self.consume_pragma(&mut tokens)?,
                Token::Ident(_) => self.consume_instruction(&mut tokens)?,
                Token::Comment(comment) => {
                    let node = Node {
                        labels: vec![],
                        entity: NodeType::Comment(comment),
                        span: span.clone(),
                    };
                    tokens.next();
                    node
                }
                Token::Newline => {
                    tokens.next();
                    continue;
                }
                _ => Err(Error::new(format!("unexpected token {token:?}"), span))?,
            };

            nodes.push(node);
        }

        Ok(ParsedProgram {
            source: self.source,
            nodes,
        })
    }

    /// Consumes a label from the token stream.
    fn consume_label<I>(&mut self, tokens: &mut I) -> Result<(), Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        let Some((Token::Label(label_ident), span)) = tokens.next() else {
            unreachable!("consume_label called on non-label token");
        };

        // Add the label to the set of observed labels.
        let label_span = self
            .labels
            .entry(label_ident)
            .or_insert_with(|| span.clone());

        // If the span of the current token is not equal to
        // `label_span`, then we have already seen label with the
        // same identifier.
        if label_span != &span {
            return Err(Error::new(
                format!("duplicate label '{label_ident}', original label span: {label_span:?}"),
                &span,
            ));
        }

        self.active_labels.push(label_ident);
        Ok(())
    }

    fn consume_pragma<I>(&mut self, tokens: &mut Peekable<I>) -> Result<Node<'s>, Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        assert!(
            matches!(tokens.peek(), Some((Token::Pragma(_), _))),
            "consume_pragma called on non-pragma token"
        );

        let labels = std::mem::take(&mut self.active_labels);
        let (pragma, span) = Pragma::consume(tokens)?;

        Ok(Node {
            labels,
            entity: NodeType::Pragma(pragma),
            span,
        })
    }

    fn consume_instruction<I>(&mut self, tokens: &mut Peekable<I>) -> Result<Node<'s>, Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        assert!(
            matches!(tokens.peek(), Some((Token::Ident(_), _))),
            "consume_instruction called on non-ident token"
        );

        let labels = std::mem::take(&mut self.active_labels);
        let (instr, span) = Instruction::consume(tokens)?;
        Ok(Node {
            labels,
            entity: NodeType::Instruction(instr),
            span,
        })
    }
}

/// An error encountered during parsing.
#[derive(Debug)]
#[allow(unused)]
pub struct Error(pub String, pub Range<usize>);

impl Error {
    fn new(message: impl ToString, span: &Range<usize>) -> Self {
        Self(message.to_string(), span.clone())
    }

    fn eof() -> Self {
        Self("unexpected eof".into(), 0..0)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Default)]
pub struct Location {
    pub block: Register,
    pub offset: Register,
}

impl Location {
    pub fn consume<'s, I>(tokens: &mut Peekable<I>) -> Result<(Self, Range<usize>), Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        // Require a '[' token.
        let start_span = match tokens.next() {
            Some((Token::AddressOpen, span)) => span,
            Some((_, span)) => Err(Error::new("expected an address opening bracket", &span))?,
            _ => Err(Error::eof())?,
        };

        let (block, _) = consume_register(tokens)?;
        let (offset, _) = consume_register(tokens)?;

        // Require a ']' token.
        let end_span = match tokens.next() {
            Some((Token::AddressClose, span)) => span,
            Some((_, span)) => Err(Error::new("expected an address closing bracket", &span))?,
            _ => Err(Error::eof())?,
        };

        Ok((Self { block, offset }, merge_spans(&start_span, &end_span)))
    }
}

#[derive(Debug)]
pub struct Expr<'s> {
    pub label: &'s str,
}

#[derive(Debug)]
pub enum PragmaType<'s> {
    U32 { value: u32 },
    WideString { value: Cow<'s, str> },
}

#[derive(Debug)]
pub struct Pragma<'s> {
    #[allow(unused)]
    relocatable: bool,
    pub payload: PragmaType<'s>,
}

impl<'s> Pragma<'s> {
    pub fn consume<I>(tokens: &mut Peekable<I>) -> Result<(Self, Range<usize>), Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        let relocatable = true;
        let token = tokens.next().ok_or(Error::eof())?;
        match token {
            (Token::Pragma("u32"), start_span) => {
                let (value, end_span) = consume_number(tokens)?;
                Ok((
                    Self {
                        relocatable,
                        payload: PragmaType::U32 { value },
                    },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Pragma("wstr"), start_span) => {
                let (value, end_span) = consume_string(tokens)?;
                Ok((
                    Self {
                        relocatable,
                        payload: PragmaType::WideString { value },
                    },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Pragma(command), span) => Err(Error::new(
                format!("unknown pragma command {command}"),
                &span,
            ))?,
            (_, span) => Err(Error::new("unexpected token", &span))?,
        }
    }
}

#[derive(Debug)]
pub enum Instruction<'s> {
    /// Operation #0.
    ConditionalMove {
        destination: Register,
        source: Register,
        condition: Register,
    },
    /// Operation #13.
    Address {
        destination: Register,
        reference: Expr<'s>,
    },
    /// Operation #13.
    LiteralMove {
        destination: Register,
        literal: u32,
    },
    Load {
        destination: Register,
        address: Location,
    },
    Store {
        source: Register,
        address: Location,
    },
    Add {
        destination: Register,
        a: Register,
        b: Register,
    },
    AddAssign {
        destination: Register,
        a: Register,
    },
    AddSelf {
        destination: Register,
    },
    Mul {
        destination: Register,
        a: Register,
        b: Register,
    },
    MulAssign {
        destination: Register,
        a: Register,
    },
    MulSelf {
        destination: Register,
    },
    Div {
        destination: Register,
        a: Register,
        b: Register,
    },
    DivAssign {
        destination: Register,
        a: Register,
    },
    DivSelf {
        destination: Register,
    },
    Nand {
        destination: Register,
        a: Register,
        b: Register,
    },
    NandAssign {
        destination: Register,
        a: Register,
    },
    NandSelf {
        destination: Register,
    },
    Halt,
    Alloc {
        destination: Register,
        length: Register,
    },
    Free {
        block: Register,
    },
    Out {
        source: Register,
    },
    In {
        destination: Register,
    },
    Jmp {
        location: Location,
    },
}

impl<'s> Instruction<'s> {
    pub fn consume<I>(tokens: &mut Peekable<I>) -> Result<(Self, Range<usize>), Error>
    where
        I: Iterator<Item = (Token<'s>, Range<usize>)>,
    {
        let ident = tokens.next().unwrap();
        match ident {
            (Token::Ident("halt"), span) => Ok((Self::Halt, span)),
            (Token::Ident("adr"), start_span) => {
                let (destination, _) = consume_register(tokens)?;
                let (identifier, end_span) = consume_ident(tokens)?;
                Ok((
                    Self::Address {
                        destination,
                        reference: Expr { label: identifier },
                    },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Ident("mov"), start_span) => {
                let (destination, _) = consume_register(tokens)?;
                if peek_register(tokens)?.is_some() {
                    let (source, _) = consume_register(tokens)?;
                    let (condition, end_span) = consume_register(tokens)?;
                    Ok((
                        Self::ConditionalMove {
                            destination,
                            source,
                            condition,
                        },
                        merge_spans(&start_span, &end_span),
                    ))
                } else {
                    let (literal, end_span) = consume_number(tokens)?;
                    Ok((
                        Self::LiteralMove {
                            destination,
                            literal,
                        },
                        merge_spans(&start_span, &end_span),
                    ))
                }
            }
            (Token::Ident("ldr"), start_span) => {
                let (destination, _) = consume_register(tokens)?;
                let (address, end_span) = Location::consume(tokens)?;
                Ok((
                    Self::Load {
                        destination,
                        address,
                    },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Ident("str"), start_span) => {
                let (source, _) = consume_register(tokens)?;
                let (address, end_span) = Location::consume(tokens)?;
                Ok((
                    Self::Store { source, address },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Ident("out"), start_span) => {
                let (source, end_span) = consume_register(tokens)?;
                Ok((Self::Out { source }, merge_spans(&start_span, &end_span)))
            }
            (Token::Ident("in"), start_span) => {
                let (destination, end_span) = consume_register(tokens)?;
                Ok((
                    Self::In { destination },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Ident("alloc"), start_span) => {
                let (destination, _) = consume_register(tokens)?;
                let (length, end_span) = consume_register(tokens)?;
                Ok((
                    Self::Alloc {
                        length,
                        destination,
                    },
                    merge_spans(&start_span, &end_span),
                ))
            }
            (Token::Ident("free"), start_span) => {
                let (block, end_span) = consume_register(tokens)?;
                Ok((Self::Free { block }, merge_spans(&start_span, &end_span)))
            }
            (Token::Ident("jmp"), start_span) => {
                let (location, end_span) = Location::consume(tokens)?;
                Ok((Self::Jmp { location }, merge_spans(&start_span, &end_span)))
            }
            (Token::Ident("add"), start_span) => {
                let (destination, mid_span) = consume_register(tokens)?;
                let a = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                let b = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                match (a, b) {
                    (Some((a, _)), Some((b, end_span))) => Ok((
                        Self::Add { destination, a, b },
                        merge_spans(&start_span, &end_span),
                    )),
                    (Some((a, end_span)), None) => Ok((
                        Self::AddAssign { destination, a },
                        merge_spans(&start_span, &end_span),
                    )),
                    (None, None) => Ok((
                        Self::AddSelf { destination },
                        merge_spans(&start_span, &mid_span),
                    )),
                    _ => unreachable!(),
                }
            }
            (Token::Ident("mul"), start_span) => {
                let (destination, mid_span) = consume_register(tokens)?;
                let a = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                let b = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                match (a, b) {
                    (Some((a, _)), Some((b, end_span))) => Ok((
                        Self::Mul { destination, a, b },
                        merge_spans(&start_span, &end_span),
                    )),
                    (Some((a, end_span)), None) => Ok((
                        Self::MulAssign { destination, a },
                        merge_spans(&start_span, &end_span),
                    )),
                    (None, None) => Ok((
                        Self::MulSelf { destination },
                        merge_spans(&start_span, &mid_span),
                    )),
                    _ => unreachable!(),
                }
            }
            (Token::Ident("div"), start_span) => {
                let (destination, mid_span) = consume_register(tokens)?;
                let a = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                let b = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                match (a, b) {
                    (Some((a, _)), Some((b, end_span))) => Ok((
                        Self::Div { destination, a, b },
                        merge_spans(&start_span, &end_span),
                    )),
                    (Some((a, end_span)), None) => Ok((
                        Self::DivAssign { destination, a },
                        merge_spans(&start_span, &end_span),
                    )),
                    (None, None) => Ok((
                        Self::DivSelf { destination },
                        merge_spans(&start_span, &mid_span),
                    )),
                    _ => unreachable!(),
                }
            }
            (Token::Ident("nand"), start_span) => {
                let (destination, mid_span) = consume_register(tokens)?;
                let a = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                let b = peek_register(tokens)?.and_then(|_| consume_register(tokens).ok());
                match (a, b) {
                    (Some((a, _)), Some((b, end_span))) => Ok((
                        Self::Nand { destination, a, b },
                        merge_spans(&start_span, &end_span),
                    )),
                    (Some((a, end_span)), None) => Ok((
                        Self::NandAssign { destination, a },
                        merge_spans(&start_span, &end_span),
                    )),
                    (None, None) => Ok((
                        Self::NandSelf { destination },
                        merge_spans(&start_span, &mid_span),
                    )),
                    _ => unreachable!(),
                }
            }
            (_, span) => Err(Error::new("unrecognised instruction", &span))?,
        }
    }
}

impl std::fmt::Display for Instruction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ConditionalMove {
                destination,
                source,
                condition,
            } => write!(f, "mov  {destination}, {source}, {condition}"),
            Self::Load {
                destination,
                address,
            } => write!(
                f,
                "ldr  {destination}, [{}, {}]",
                address.block, address.offset
            ),
            Self::Store { source, address } => {
                write!(f, "str  {source}, [{}, {}]", address.block, address.offset)
            }
            Self::Add { destination, a, b } => write!(f, "add  {destination}, {a}, {b}"),
            Self::AddAssign { destination, a } => write!(f, "add  {destination}, {a}"),
            Self::AddSelf { destination } => write!(f, "add  {destination}"),
            Self::Mul { destination, a, b } => write!(f, "mul  {destination}, {a}, {b}"),
            Self::MulAssign { destination, a } => write!(f, "mul  {destination}, {a}"),
            Self::MulSelf { destination } => write!(f, "mul  {destination}"),
            Self::Div { destination, a, b } => write!(f, "div  {destination}, {a}, {b}"),
            Self::DivAssign { destination, a } => write!(f, "div  {destination}, {a}"),
            Self::DivSelf { destination } => write!(f, "div  {destination}"),
            Self::Nand { destination, a, b } => write!(f, "nand {destination}, {a}, {b}"),
            Self::NandAssign { destination, a } => write!(f, "nand {destination}, {a}"),
            Self::NandSelf { destination } => write!(f, "nand {destination}"),
            Self::Halt => write!(f, "halt"),
            Self::Out { source } => write!(f, "out  {source}"),
            Self::In { destination } => write!(f, "in   {destination}"),
            Self::Alloc {
                length,
                destination,
            } => write!(f, "alloc {destination}, {length}"),
            Self::Free { block } => {
                write!(f, "free {block}")
            }
            Self::Jmp { location } => write!(f, "jmp  [{}, {}]", location.block, location.offset),
            Self::LiteralMove {
                destination,
                literal,
            } => write!(f, "mov  {destination}, {literal}"),
            Self::Address {
                destination,
                reference,
            } => write!(f, "adr  {destination}, {}", reference.label),
        }
    }
}

/// Peeks at the next token and returns it iff it is a Register.
fn peek_register<'s, I>(tokens: &mut Peekable<I>) -> Result<Option<Register>, Error>
where
    I: Iterator<Item = (Token<'s>, Range<usize>)>,
{
    match tokens.peek() {
        Some((Token::Register(r), _)) => Ok(Some(*r)),
        Some(_) => Ok(None),
        None => Err(Error::new("unexpected eof", &(0..0))),
    }
}

fn consume_register<'s, I>(tokens: &mut I) -> Result<(Register, Range<usize>), Error>
where
    I: Iterator<Item = (Token<'s>, Range<usize>)>,
{
    match tokens.next() {
        Some((Token::Register(r), span)) => Ok((r, span)),
        Some((token, span)) => Err(Error::new(
            format!("expected a register, found: {token:?}"),
            &span,
        )),
        None => Err(Error::eof()),
    }
}

fn consume_ident<'s, I>(tokens: &mut I) -> Result<(&'s str, Range<usize>), Error>
where
    I: Iterator<Item = (Token<'s>, Range<usize>)>,
{
    match tokens.next() {
        Some((Token::Ident(ident), span)) => Ok((ident, span)),
        Some((token, span)) => Err(Error::new(
            format!("expected an identifier, found: {token:?}"),
            &span,
        )),
        None => Err(Error::eof()),
    }
}

fn consume_number<'s, I>(tokens: &mut I) -> Result<(u32, Range<usize>), Error>
where
    I: Iterator<Item = (Token<'s>, Range<usize>)>,
{
    match tokens.next() {
        Some((Token::Number(value), span)) => Ok((value, span)),
        Some((token, span)) => Err(Error::new(
            format!("expected a number literal, found: {token:?}"),
            &span,
        )),
        None => Err(Error::eof()),
    }
}

fn consume_string<'s, I>(tokens: &mut I) -> Result<(Cow<'s, str>, Range<usize>), Error>
where
    I: Iterator<Item = (Token<'s>, Range<usize>)>,
{
    match tokens.next() {
        Some((Token::String(value), span)) => {
            let unescaped = unescape_str(value).map_err(|_| Error::eof())?;
            Ok((unescaped, span))
        }
        Some((token, span)) => Err(Error::new(
            format!("expected a number literal, found: {token:?}"),
            &span,
        )),
        None => Err(Error::eof()),
    }
}

fn merge_spans(start: &Range<usize>, end: &Range<usize>) -> Range<usize> {
    start.start..end.end
}

#[derive(Debug)]
#[allow(unused)]
pub struct InvalidCharacterEscape(pub char, pub usize);

pub fn unescape_str(s: &str) -> Result<Cow<str>, InvalidCharacterEscape> {
    fn escape_inner(c: &str, i: &mut CharIndices<'_>) -> Result<String, InvalidCharacterEscape> {
        let mut buffer = c.to_owned();
        let mut in_escape = true;

        for (index, c) in i {
            match (in_escape, c) {
                (false, '\\') => {
                    in_escape = true;
                    continue;
                }
                (false, c) => buffer.push(c),
                (true, '\\') => buffer.push('\\'),
                (true, 'n') => buffer.push('\n'),
                (true, '0') => buffer.push('\0'),
                (true, '"') => buffer.push('"'),
                (true, '\'') => buffer.push('\''),
                (true, 'r') => buffer.push('\r'),
                (true, 't') => buffer.push('\t'),
                (true, c) => Err(InvalidCharacterEscape(c, index))?,
            }

            in_escape = false;
        }

        Ok(buffer)
    }

    let mut char_indicies = s.char_indices();
    for (index, c) in &mut char_indicies {
        let scanned = &s[..index];
        if c == '\\' {
            return Ok(Cow::Owned(escape_inner(scanned, &mut char_indicies)?));
        }
    }

    Ok(Cow::Borrowed(s))
}
