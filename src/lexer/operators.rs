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
        tag!("!=")  => { |_| Token::Operator(Operator::NEQ)}        |
        tag!("!")   => { |_| Token::Operator(Operator::Not)}        |
        tag!("~")   => { |_| Token::Operator(Operator::BitwiseNot)} |
        tag!("%")   => { |_| Token::Operator(Operator::Percent)}    |
        tag!("==")  => { |_| Token::Operator(Operator::EQ)}         |
        tag!("<=")  => { |_| Token::Operator(Operator::LE)}         |
        tag!("<")   => { |_| Token::Operator(Operator::LT)}         |
        tag!(">=")  => { |_| Token::Operator(Operator::GE)}         |
        tag!(">")   => { |_| Token::Operator(Operator::GT)}         |
        tag!("=")   => { |_| Token::Operator(Operator::Assign)}     |
        tag!("?")   => { |_| Token::Operator(Operator::Condition)}  |
        tag!(":")   => { |_| Token::Operator(Operator::Colon)}
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
            lex_operators("! "),
            Ok(((" "), Token::Operator(Operator::Not)))
        );
        assert_eq!(
            lex_operators("~"),
            Ok(((""), Token::Operator(Operator::BitwiseNot)))
        );
        assert_eq!(
            lex_operators("!="),
            Ok(((""), Token::Operator(Operator::NEQ)))
        );
        assert_eq!(
            lex_operators("=="),
            Ok(((""), Token::Operator(Operator::EQ)))
        );
        assert_eq!(
            lex_operators("<="),
            Ok(((""), Token::Operator(Operator::LE)))
        );
        assert_eq!(
            lex_operators(">="),
            Ok(((""), Token::Operator(Operator::GE)))
        );
    }
}
