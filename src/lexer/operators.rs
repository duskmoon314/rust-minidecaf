use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_operators<&str, Token>,
    alt!(
        tag!("-")   => { |_| Token::Operator(Operator::Minus)}      |
        tag!("!")   => { |_| Token::Operator(Operator::Not)}        |
        tag!("~")   => { |_| Token::Operator(Operator::BitwiseNot)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_operators() {
        assert_eq!(
            lex_operators("-"),
            Ok(((""), Token::Operator(Operator::Minus)))
        );
        assert_eq!(
            lex_operators("!"),
            Ok(((""), Token::Operator(Operator::Not)))
        );
        assert_eq!(
            lex_operators("~"),
            Ok(((""), Token::Operator(Operator::BitwiseNot)))
        );
    }
}
