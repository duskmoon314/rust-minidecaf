use nom::IResult;
use nom::*;

use crate::lexer::token::*;

pub fn lex_identifiers(input: &str) -> IResult<&str, Token> {
    let re = regex::Regex::new(r"^[_A-Za-z]{1}\w*").unwrap();
    if let Some(m) = re.find(input) {
        Ok((
            &input[m.end()..],
            Token::Identifier(input[m.start()..m.end()].to_string()),
        ))
    } else {
        Err(Err::Error(error_position!(
            input,
            nom::error::ErrorKind::RegexpFind
        )))
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
        assert_eq!(
            lex_identifiers("a1_b 2"),
            Ok((" 2", Token::Identifier("a1_b".to_string())))
        );
        assert_eq!(
            lex_identifiers("1a"),
            Err(Err::Error(error_position!(
                "1a",
                nom::error::ErrorKind::RegexpFind
            )))
        );
    }
}
