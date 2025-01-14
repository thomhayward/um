use crate::Register;
use logos::{Lexer, Logos};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Extras {
    pub line: usize,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\f,]+", extras = Extras)]
pub enum Token<'source> {
    #[token("\n", lex_newline)]
    Newline,

    #[regex("[a-zA-Z]+[a-zA-Z0-9_]*:", lex_label)]
    Label(&'source str),

    #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*", |lexer| lexer.slice())]
    Ident(&'source str),

    #[regex(r#"\.([a-zA-Z0-9]+)"#, |lexer| &lexer.slice()[1..])]
    Pragma(&'source str),

    #[token("[")]
    AddressOpen,

    #[token("]")]
    AddressClose,

    #[regex("r[0-7]", lex_register, priority = 10)]
    Register(Register),

    #[token("#")]
    Pound,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token(".")]
    Here,

    #[regex(r#"(0x[a-fA-F0-9]+)|([0-9]+)"#, lex_number)]
    Number(u32),

    #[token("\"", lex_string_literal)]
    String(&'source str),

    #[token(";", lex_comment)]
    Comment(&'source str),
}

fn lex_newline<'source>(lexer: &mut Lexer<'source, Token<'source>>) {
    lexer.extras.line += 1;
}

fn lex_label<'source>(lex: &mut Lexer<'source, Token<'source>>) -> &'source str {
    let slice = lex.slice();
    &slice[..slice.len() - 1]
}

fn lex_number<'source>(lex: &mut Lexer<'source, Token<'source>>) -> u32 {
    let slice = &lex.slice();
    if slice.starts_with("0x") {
        u32::from_str_radix(slice.trim_start_matches("0x"), 16).unwrap()
    } else {
        slice.parse().unwrap()
    }
}

fn lex_string_literal<'source>(lexer: &mut Lexer<'source, Token<'source>>) -> &'source str {
    let remainder = lexer.remainder();

    let mut in_escape = false;
    let mut complete = false;
    let mut final_index = 0;
    for (index, character) in remainder.char_indices() {
        if complete {
            lexer.bump(index);
            return &remainder[..final_index];
        }

        if character == '\\' {
            in_escape = true;
            continue;
        }

        if character == '"' && in_escape {
            continue;
        }

        if character == '"' && !in_escape {
            complete = true;
            final_index = index;
            continue;
        }

        in_escape = false;
    }

    lexer.bump(remainder.len());
    remainder
}

fn lex_register<'source>(lex: &mut Lexer<'source, Token<'source>>) -> Register {
    let slice = lex.slice();
    let index = slice[1..]
        .parse()
        .expect("regex for register tokens should make the infallible");

    Register::from_u8(index)
}

fn lex_comment<'source>(lex: &mut Lexer<'source, Token<'source>>) -> &'source str {
    let remainder = lex.remainder();
    for (position, c) in remainder.char_indices() {
        if c == '\n' {
            lex.bump(position);
            return &remainder[..position];
        }
    }

    lex.bump(remainder.len());
    remainder
}
