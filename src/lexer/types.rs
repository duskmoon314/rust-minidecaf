use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_types<&str, Token>,
    alt!(
        tag!("int") => { |_| Token::Type(Type::Int)} |
        tag!("double") => { |_| Token::Type(Type::Double)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_types() {
        assert_eq!(lex_types("int"), Ok(("", Token::Type(Type::Int))));
        assert_eq!(lex_types("double"), Ok(("", Token::Type(Type::Double))));
    }
}
