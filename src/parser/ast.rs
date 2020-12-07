/*!
```markdown
Program      -> Function
Function     -> Type Identifier Lparen Rparen Lbrace Statement Rbrace
Type         -> Type(Int | Double)
Statement    -> Return Expression Semicolon
Expression   -> Factor
Factor       -> | Integer(i32)
                | (~ ! -) Factor
```
*/

use crate::lexer::token::*;
use peek_nth::PeekableNth;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub t: Type,
    pub name: String,
    pub statement: Statement,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Const(i32),
    Unary(Operator, Box<Expression>),
}

/// Factor => (<expression>) | Unary | Const(i32)
fn parse_factor(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.next() {
        Some(next) => match next {
            Token::Symbol(Symbol::Lparen) => {
                // ( <expression> )
                let expr = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::Symbol(Symbol::Rparen)) => expr,
                    Some(_) | None => panic!("Expression Error: Missing ) to close expression"),
                }
            }
            Token::Operator(unary_op) if unary_op.is_unary() => {
                let expr = parse_factor(tokens);
                Expression::Unary(*unary_op, Box::new(expr))
            }
            Token::Integer(num) => Expression::Const(*num), // <int>
            _ => panic!("Expression Error: Expected factor, Nothing found"),
        },
        _ => panic!("Nothing Left"),
    }
}

/// Expression => Factor
fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    parse_factor(tokens)
}

fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.peek() {
        Some(Token::Keyword(Keyword::Return)) => {
            // return expr;
            tokens.next();
            let expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => Statement::Return(expr),
                _ => panic!("Statement Error: Expecting ; at the end"),
            }
        }
        _ => panic!("Unknown Statement"),
    }
}

#[allow(unreachable_patterns)]
pub fn parse_function(tokens: &mut PeekableNth<Iter<Token>>) -> Function {
    match tokens.next() {
        Some(Token::Type(t)) => match t {
            Type::Int => match tokens.next() {
                Some(Token::Identifier(id)) => match tokens.next() {
                    Some(Token::Symbol(Symbol::Lparen)) => match tokens.next() {
                        Some(Token::Symbol(Symbol::Rparen)) => match tokens.next() {
                            Some(Token::Symbol(Symbol::Lbrace)) => {
                                let stat = parse_statement(tokens);
                                match tokens.next() {
                                    Some(Token::Symbol(Symbol::Rbrace)) => Function {
                                        t: *t,
                                        name: id.to_owned(),
                                        statement: stat,
                                    },
                                    _ => panic!("Expecting } of Function"),
                                }
                            }
                            _ => panic!("Expecting { of Function"),
                        },
                        _ => panic!("Expecting ) of Function"),
                    },
                    _ => panic!("Expecting ( of Function"),
                },
                _ => panic!("Expecting Function name"),
            },
            _ => panic!("Expecting common type: Int"),
        },
        _ => panic!("Expecting Function start with type"),
    }
}

pub fn parse_program(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
    let func = parse_function(tokens);
    if tokens.peek().is_some() && *tokens.peek().unwrap() != &Token::Symbol(Symbol::EOF) {
        panic!("Expecting Only One Function Now");
    }
    // Only One Function: main
    if func.name != "main" {
        panic!("Expecting main function");
    }
    Program { function: func }
}
