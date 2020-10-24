use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_keywords<&str, Token>,
    alt!(
        tag!("return")  => { |_| Token::Keyword(Keyword::Return)}   |
        tag!("if")      => { |_| Token::Keyword(Keyword::If)}       |
        tag!("else")    => { |_| Token::Keyword(Keyword::Else)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_keywords() {
        assert_eq!(
            lex_keywords("return"),
            Ok(("", Token::Keyword(Keyword::Return)))
        );
    }
}
