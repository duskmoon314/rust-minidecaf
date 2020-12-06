use crate::lexer::token;
use peek_nth::IteratorExt;

pub mod ast;

pub fn parser(tokens: Vec<token::Token>) -> ast::Program {
    let mut tks = tokens.iter().peekable_nth();
    ast::parse_program(&mut tks)
}
