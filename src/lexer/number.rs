use nom::character::complete::digit1;
use nom::IResult;

use crate::lexer::token::*;

pub fn lex_integers(input: &str) -> IResult<&str, Token> {
    let (reset, integer) = digit1(input)?;
    Ok((reset, Token::Integer(integer.parse::<i32>().unwrap())))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_numbers() {
        assert_eq!(lex_integers("123"), Ok(("", Token::Integer(123))));
    }
}
