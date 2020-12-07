pub mod identifiers;
pub mod keywords;
pub mod number;
pub mod operators;
pub mod symbols;
pub mod token;
pub mod types;

use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::IResult;

pub fn lex_all(input: &str) -> IResult<&str, token::Token> {
    let (input, _) = multispace0(input)?;
    if input.is_empty() {
        Ok(("", token::Token::Symbol(token::Symbol::EOF)))
    } else {
        let (reset, tk) = alt((
            symbols::lex_symbols,
            keywords::lex_keywords,
            types::lex_types,
            identifiers::lex_identifiers,
            operators::lex_operators,
            number::lex_integers,
        ))(input)?;
        Ok((reset, tk))
    }
}

pub fn lexer(input: &str) -> Vec<token::Token> {
    let mut tokens = Vec::<token::Token>::new();
    let mut reset = input;
    while !reset.is_empty() {
        let res = lex_all(reset);
        let result = match res {
            Ok(result) => result,
            Err(error) => panic!("Lex Failed: {:?}", error),
        };
        tokens.push(result.1);
        reset = result.0;
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lexer() {
        let v = vec![
            token::Token::Type(token::Type::Int),
            token::Token::Identifier("main".to_string()),
            token::Token::Symbol(token::Symbol::Lparen),
            token::Token::Symbol(token::Symbol::Rparen),
            token::Token::Symbol(token::Symbol::Lbrace),
            token::Token::Keyword(token::Keyword::Return),
            token::Token::Integer(0),
            token::Token::Symbol(token::Symbol::Semicolon),
            token::Token::Symbol(token::Symbol::Rbrace),
        ];
        assert_eq!(lexer("int main(){return 0;}"), v);
    }
}
