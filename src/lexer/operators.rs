use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_operators<&str, Token>,
    alt!(
        tag!("+")   => { |_| Token::Operator(Operator::Plus)}       |
        tag!("-")   => { |_| Token::Operator(Operator::Minus)}      |
        tag!("*")   => { |_| Token::Operator(Operator::Asterisk)}   |
        tag!("/")   => { |_| Token::Operator(Operator::Slash)}      |
        tag!("\\")  => { |_| Token::Operator(Operator::BackSlash)}  |
        tag!("&&")  => { |_| Token::Operator(Operator::And)}        |
        tag!("&")   => { |_| Token::Operator(Operator::BitwiseAnd)} |
        tag!("||")  => { |_| Token::Operator(Operator::Or)}         |
        tag!("|")   => { |_| Token::Operator(Operator::BitwiseOr)}  |
        tag!("!")   => { |_| Token::Operator(Operator::Not)}        |
        tag!("~")   => { |_| Token::Operator(Operator::BitwiseNot)} |
        tag!("%")   => { |_| Token::Operator(Operator::Percent)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_operators() {
        assert_eq!(
            lex_operators("+"),
            Ok(((""), Token::Operator(Operator::Plus)))
        );
        assert_eq!(
            lex_operators("-"),
            Ok(((""), Token::Operator(Operator::Minus)))
        );
        assert_eq!(
            lex_operators("*"),
            Ok(((""), Token::Operator(Operator::Asterisk)))
        );
        assert_eq!(
            lex_operators("/"),
            Ok(((""), Token::Operator(Operator::Slash)))
        );
        assert_eq!(
            lex_operators("\\"),
            Ok(((""), Token::Operator(Operator::BackSlash)))
        );
        assert_eq!(
            lex_operators("&&"),
            Ok(((""), Token::Operator(Operator::And)))
        );
        assert_eq!(
            lex_operators("& "),
            Ok(((" "), Token::Operator(Operator::BitwiseAnd)))
        );
        assert_eq!(
            lex_operators("||"),
            Ok(((""), Token::Operator(Operator::Or)))
        );
        assert_eq!(
            lex_operators("| "),
            Ok(((" "), Token::Operator(Operator::BitwiseOr)))
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
