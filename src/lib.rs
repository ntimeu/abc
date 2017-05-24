//! This is Another Brainf*ck Compiler.
//!
//! It contains all the required tools to build a BF program using only
//! in-house programming and the standard library. The aim is to provide :
//!
//! * a lexer (easy because of the simplicity of the language)
//! * a parser generating an AST
//! * a full set of optimizations
//! * a code generator (native or intermediate code for a VM)

use std::iter::Iterator;
use std::str::Chars;

#[derive(PartialEq)]
/// List of tokens.
pub enum Token {
    /// Represent the '+' opcode
    PLUS,
    /// Represent the '-' opcode
    MINUS,
    /// Represent the '<' opcode
    LEFT,
    /// Represent the '>' opcode
    RIGHT,
    /// Represent the '[' opcode
    SCOND,
    /// Represent the ']' opcode
    ECOND,
    /// Represent the '.' opcode
    PRINT,
    /// Represent the ',' opcode
    INPUT
}

/// The BF lexer.
///
/// This is a pull lexer (lexing on-demand). Just an iterator over a list of
/// `Chars`.
pub struct Tokenizer<'a> {
    instructions: Chars<'a>,
}

impl<'a> Tokenizer<'a> {
    /// Create the brainf*ck lexer from a text containing a valid brainf*ck
    /// program.
    pub fn new(program: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            instructions: program.chars(),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.instructions.next() {
                Some('+')   => return Some(Token::PLUS),
                Some('-')   => return Some(Token::MINUS),
                Some('<')   => return Some(Token::LEFT),
                Some('>')   => return Some(Token::RIGHT),
                Some('[')   => return Some(Token::SCOND),
                Some(']')   => return Some(Token::ECOND),
                Some('.')   => return Some(Token::PRINT),
                Some(',')   => return Some(Token::INPUT),
                Some(_)     => continue,
                None        => return None,
            }
        }
    }
}
