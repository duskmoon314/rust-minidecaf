use nom::character::complete::{alphanumeric0, anychar};
use nom::character::is_alphabetic;
use nom::IResult;
use nom::*;

use crate::lexer::token::*;

pub fn is_underline(chr: u8) -> bool {
    chr == 0x5F
}

pub fn lex_identifiers(input: &str) -> IResult<&str, Token> {
    let s = input.bytes().nth(0).unwrap();
    if is_alphabetic(s) || is_underline(s) {
        let (reset, start) = anychar(input)?;
        let (reset, body) = alphanumeric0(reset)?;
        Ok((reset, Token::Identifier(format!("{}{}", start, body))))
    } else {
        Err(Err::Error((input, nom::error::ErrorKind::AlphaNumeric)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_identifiers() {
        assert_eq!(
            lex_identifiers("_a1b2c3 aaa"),
            Ok((" aaa", Token::Identifier("_a1b2c3".to_string())))
        );
    }
}
