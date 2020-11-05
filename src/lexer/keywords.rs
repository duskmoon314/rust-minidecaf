use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_keywords<&str, Token>,
    alt!(
        tag!("return")  => { |_| Token::Keyword(Keyword::Return)}   |
        tag!("if")      => { |_| Token::Keyword(Keyword::If)}       |
        tag!("else")    => { |_| Token::Keyword(Keyword::Else)}     |
        tag!("for")     => { |_| Token::Keyword(Keyword::For)}      |
        tag!("while")   => { |_| Token::Keyword(Keyword::While)}    |
        tag!("do")      => { |_| Token::Keyword(Keyword::Do)}       |
        tag!("break")   => { |_| Token::Keyword(Keyword::Break)}    |
        tag!("continue")=> { |_| Token::Keyword(Keyword::Continue)}
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
