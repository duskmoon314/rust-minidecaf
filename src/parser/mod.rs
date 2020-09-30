use crate::lexer::token;

pub mod ast;

pub fn parser(tokens: Vec<token::Token>) -> ast::Program {
    let mut tks = tokens.iter().peekable();
    ast::parse_program(&mut tks)
}
