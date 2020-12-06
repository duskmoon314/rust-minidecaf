use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_types<&str, Token>,
    alt!(
        tag!("int") => { |_| Token::Type(Type::Int)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_types() {
        assert_eq!(lex_types("int"), Ok(("", Token::Type(Type::Int))));
    }
}
