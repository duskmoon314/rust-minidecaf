use crate::lexer::token::*;
use std::iter::Peekable;
use std::slice::Iter;

/*
 * Program      -> Function
 * Function     -> Type Identifier Lparen Rparen Lbrace Statement Rbrace
 * Type         -> Type(Int | Double)
 * Statement    -> Return Expression Semicolon
 * Expression   -> Integer(i32) | Unary(UnaryOp, Box<Expression>)
 */

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

pub fn parse_factor(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    match tokens.next() {
        Some(next) => match next {
            Token::Symbol(Symbol::Lparen) => {
                // ( <expression> )
                let expr = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::Symbol(Symbol::Rparen)) => return expr,
                    Some(_) | None => panic!("Missing ) to close expression"),
                }
            }
            Token::Integer(num) => return Expression::Const(*num), // <int>
            _ => panic!("Expected factor, Nothing found"),
        },
        _ => panic!("Nothing Left"),
    }
}

pub fn parse_expression(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    match tokens.next() {
        Some(Token::Integer(int)) => return Expression::Const(*int),
        Some(Token::Operator(unary_op)) if unary_op.is_unary() => {
            let expr = parse_expression(tokens);
            return Expression::Unary(*unary_op, Box::new(expr));
        }
        _ => panic!("Now can only parse an integer"),
    }
}

pub fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Statement {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Return)) => {
            let expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => {
                    return Statement::Return(expr);
                }
                _ => panic!("Expecting ; at the end of Statement"),
            }
        }
        _ => panic!("Unknown Statement"),
    }
}

pub fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Function {
    match tokens.next() {
        Some(Token::Type(t)) => match t {
            Type::Int => match tokens.next() {
                Some(Token::Identifier(id)) => match tokens.next() {
                    Some(Token::Symbol(Symbol::Lparen)) => match tokens.next() {
                        Some(Token::Symbol(Symbol::Rparen)) => match tokens.next() {
                            Some(Token::Symbol(Symbol::Lbrace)) => {
                                let stat = parse_statement(tokens);
                                match tokens.next() {
                                    Some(Token::Symbol(Symbol::Rbrace)) => {
                                        return Function {
                                            t: *t,
                                            name: id.to_owned(),
                                            statement: stat,
                                        }
                                    }
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

pub fn parse_program(tokens: &mut Peekable<Iter<Token>>) -> Program {
    let func = parse_function(tokens);
    if tokens.len() != 0 {
        if *tokens.next().unwrap() != Token::Symbol(Symbol::EOF) {
            panic!("Expecting Only One Function Now");
        }
    }
    // Only One Function: main
    if func.name != "main" {
        panic!("Expecting main function");
    }
    Program { function: func }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression() {
        /*
         * 123
         */
        let tokens = vec![Token::Integer(123)];
        assert_eq!(
            parse_expression(&mut tokens.iter().peekable()),
            Expression::Const(123)
        );
        let tokens = vec![Token::Operator(Operator::Minus), Token::Integer(10)];
        assert_eq!(
            parse_expression(&mut tokens.iter().peekable()),
            Expression::Unary(Operator::Minus, Box::new(Expression::Const(10)))
        );
    }

    #[test]
    fn test_parse_statement() {
        /*
         * return 0;
         */
        let tokens = vec![
            Token::Keyword(Keyword::Return),
            Token::Integer(0),
            Token::Symbol(Symbol::Semicolon),
        ];
        assert_eq!(
            parse_statement(&mut tokens.iter().peekable()),
            Statement::Return(Expression::Const(0))
        );
    }

    #[test]
    fn test_parse_function() {
        /*
         * int main ()
         * {
         * return 0;
         * }
         */
        let tokens = vec![
            Token::Type(Type::Int),
            Token::Identifier("main".to_string()),
            Token::Symbol(Symbol::Lparen),
            Token::Symbol(Symbol::Rparen),
            Token::Symbol(Symbol::Lbrace),
            Token::Keyword(Keyword::Return),
            Token::Integer(0),
            Token::Symbol(Symbol::Semicolon),
            Token::Symbol(Symbol::Rbrace),
        ];
        assert_eq!(
            parse_function(&mut tokens.iter().peekable()),
            Function {
                t: Type::Int,
                name: "main".to_string(),
                statement: Statement::Return(Expression::Const(0))
            }
        );
    }
}
