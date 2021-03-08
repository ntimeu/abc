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
#[cfg_attr(test, derive(Debug))]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_parsing_plus() {
        let expected = vec![Token::PLUS];
        let parsed: Vec<Token>= Tokenizer::new("+").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_minus() {
        let expected = vec![Token::MINUS];
        let parsed: Vec<Token> = Tokenizer::new("-").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_left() {
        let expected = vec![Token::LEFT];
        let parsed: Vec<Token> = Tokenizer::new("<").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_right() {
        let expected = vec![Token::RIGHT];
        let parsed: Vec<Token> = Tokenizer::new(">").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_scond() {
        let expected = vec![Token::SCOND];
        let parsed: Vec<Token> = Tokenizer::new("[").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_econd() {
        let expected = vec![Token::ECOND];
        let parsed: Vec<Token> = Tokenizer::new("]").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_print() {
        let expected = vec![Token::PRINT];
        let parsed: Vec<Token> = Tokenizer::new(".").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_parsing_input() {
        let expected = vec![Token::INPUT];
        let parsed: Vec<Token> = Tokenizer::new(",").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_empty_string() {
        let expected: Vec<Token> = Vec::new();
        let parsed: Vec<Token> = Tokenizer::new("").into_iter().collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_correct_program() {
        let expected: Vec<Token> = vec![
            Token::RIGHT,
            Token::PLUS,
            Token::MINUS,
            Token::LEFT,
            Token::PLUS,
            Token::SCOND,
            Token::MINUS,
            Token::ECOND
        ];

        let parsed: Vec<Token> = Tokenizer::new(">+-<+[-]")
            .into_iter()
            .collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_valid_with_incorrect_chars() {
        let expected = vec![
            Token::PLUS,
            Token::MINUS
        ];

        let parsed: Vec<Token> = Tokenizer::new("AZERTY+123456-")
            .into_iter()
            .collect();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn check_spaces() {
        let expected: Vec<Token> = Vec::new();

        let parsed: Vec<Token> = Tokenizer::new("         ")
            .into_iter()
            .collect();

        assert_eq!(parsed, expected);
    }
}
