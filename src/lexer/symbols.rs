use nom::*;

use crate::lexer::token::*;

named!(
    pub lex_symbols<&str, Token>,
    alt!(
        tag!("(") => { |_| Token::Symbol(Symbol::Lparen)} |
        tag!(")") => { |_| Token::Symbol(Symbol::Rparen)} |
        tag!("{") => { |_| Token::Symbol(Symbol::Lbrace)} |
        tag!("}") => { |_| Token::Symbol(Symbol::Rbrace)} |
        tag!("[") => { |_| Token::Symbol(Symbol::Lbrack)} |
        tag!("]") => { |_| Token::Symbol(Symbol::Rbrack)} |
        tag!(";") => { |_| Token::Symbol(Symbol::Semicolon)} |
        tag!(",") => { |_| Token::Symbol(Symbol::Comma)}
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_symbols() {
        assert_eq!(lex_symbols("("), Ok(("", Token::Symbol(Symbol::Lparen))));
        assert_eq!(lex_symbols(")"), Ok(("", Token::Symbol(Symbol::Rparen))));
        assert_eq!(lex_symbols("{"), Ok(("", Token::Symbol(Symbol::Lbrace))));
        assert_eq!(lex_symbols("}"), Ok(("", Token::Symbol(Symbol::Rbrace))));
        assert_eq!(lex_symbols("["), Ok(("", Token::Symbol(Symbol::Lbrack))));
        assert_eq!(lex_symbols("]"), Ok(("", Token::Symbol(Symbol::Rbrack))));
        assert_eq!(lex_symbols(";"), Ok(("", Token::Symbol(Symbol::Semicolon))));
        assert_eq!(lex_symbols(","), Ok(("", Token::Symbol(Symbol::Comma))));
    }
}
