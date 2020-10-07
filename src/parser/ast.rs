use crate::lexer::token::*;
use std::iter::Peekable;
use std::slice::Iter;

/*
 * Program      -> Function
 * Function     -> Type Identifier Lparen Rparen Lbrace Statement Rbrace
 * Type         -> Type(Int | Double)
 * Statement    -> Return Expression Semicolon
 * Expression   ->  | Logical_Or
 * Factor       ->  | Integer(i32)
 *                  | ~ ! - Factor
 *                  | ( Expression )
 * Multiplicative -> Factor | Multiplicative * / % Factor
 * Additive     -> Multiplicative | Additive + - Multiplicative
 * Relational   -> Additive | Relational < > <= >= Additive
 * Equality     -> Relational | Equality == != Relational
 * Logical_And  -> Equality | Logical_And && Equality
 * Logical_Or   -> Logical_And | Logical_Or || Logical_And
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
    Binary(Operator, Box<Expression>, Box<Expression>), // Binary Operations (+ a b)
                                                        // Additive(Box<Expression>, Operator, Box<Expression>),
                                                        // Multiplicative(Box<Expression>, Operator, Box<Expression>)
                                                        // Relational(Box<Expression>, Operator, Box<Expression>)
                                                        // Equality(Box<Expression>, Operator, Box<Expression>)
}

// Factor => (<expression>) | Unary | Const(i32)
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
            Token::Operator(unary_op) if unary_op.is_unary() => {
                let expr = parse_factor(tokens);
                return Expression::Unary(*unary_op, Box::new(expr));
            }
            Token::Integer(num) => return Expression::Const(*num), // <int>
            _ => panic!("Expected factor, Nothing found"),
        },
        _ => panic!("Nothing Left"),
    }
}

// Multiplicative => factor * / % factor
pub fn parse_multiplicative(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_factor(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(mul_op)) if mul_op.is_multiplicative() => {
                tokens.next();
                let r_factor = parse_factor(tokens);
                l_factor = Expression::Binary(*mul_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

// Additive => Multiplicative + - Multiplicative
pub fn parse_additive(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_multiplicative(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(add_op)) if add_op.is_additive() => {
                tokens.next();
                let r_factor = parse_multiplicative(tokens);
                l_factor = Expression::Binary(*add_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

// Relational => Additive < > <= >= Additive
pub fn parse_relational(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_additive(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(rt_op)) if rt_op.is_relational() => {
                tokens.next();
                let r_factor = parse_additive(tokens);
                l_factor = Expression::Binary(*rt_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

// Equality => Relational == != Relational
pub fn parse_equality(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_relational(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(eq_op)) if eq_op.is_equality() => {
                tokens.next();
                let r_factor = parse_relational(tokens);
                l_factor = Expression::Binary(*eq_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

// Logical_And => Equality && Equality
pub fn parse_logical_and(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_equality(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(Operator::And)) => {
                tokens.next();
                let r_factor = parse_equality(tokens);
                l_factor = Expression::Binary(Operator::And, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

// Logical_Or => Logical_And && Logical_And
pub fn parse_logical_or(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_logical_and(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(Operator::Or)) => {
                tokens.next();
                let r_factor = parse_logical_and(tokens);
                l_factor = Expression::Binary(Operator::Or, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

pub fn parse_expression(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    parse_logical_or(tokens)
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
    fn test_parse_multiplicative() {
        let tokens = vec![
            Token::Integer(1),
            Token::Operator(Operator::Asterisk),
            Token::Integer(2),
        ];
        assert_eq!(
            parse_multiplicative(&mut tokens.iter().peekable()),
            Expression::Binary(
                Operator::Asterisk,
                Box::new(Expression::Const(1)),
                Box::new(Expression::Const(2))
            )
        );
        let tokens = vec![
            Token::Operator(Operator::Minus),
            Token::Integer(1),
            Token::Operator(Operator::Slash),
            Token::Symbol(Symbol::Lparen),
            Token::Integer(1),
            Token::Symbol(Symbol::Rparen),
            Token::Symbol(Symbol::Semicolon),
        ];
        assert_eq!(
            parse_multiplicative(&mut tokens.iter().peekable()),
            Expression::Binary(
                Operator::Slash,
                Box::new(Expression::Unary(
                    Operator::Minus,
                    Box::new(Expression::Const(1))
                )),
                Box::new(Expression::Const(1))
            )
        );
    }

    #[test]
    fn test_parse_additive() {
        let tokens = vec![
            Token::Integer(1),
            Token::Operator(Operator::Plus),
            Token::Operator(Operator::Minus),
            Token::Integer(1),
            Token::Operator(Operator::Slash),
            Token::Symbol(Symbol::Lparen),
            Token::Integer(1),
            Token::Symbol(Symbol::Rparen),
        ];
        assert_eq!(
            parse_additive(&mut tokens.iter().peekable()),
            Expression::Binary(
                Operator::Plus,
                Box::new(Expression::Const(1)),
                Box::new(Expression::Binary(
                    Operator::Slash,
                    Box::new(Expression::Unary(
                        Operator::Minus,
                        Box::new(Expression::Const(1))
                    )),
                    Box::new(Expression::Const(1))
                ))
            )
        );
    }

    #[test]
    fn test_parse_relational() {
        let tokens = vec![
            Token::Integer(1),
            Token::Operator(Operator::LT),
            Token::Integer(2),
        ];
        assert_eq!(
            parse_relational(&mut tokens.iter().peekable()),
            Expression::Binary(
                Operator::LT,
                Box::new(Expression::Const(1)),
                Box::new(Expression::Const(2))
            )
        )
    }

    #[test]
    fn test_parse_logical() {
        let tokens = vec![
            Token::Integer(1),
            Token::Operator(Operator::NEQ),
            Token::Integer(2),
            Token::Operator(Operator::And),
            Token::Integer(1),
            Token::Operator(Operator::EQ),
            Token::Integer(1),
            Token::Operator(Operator::Or),
            Token::Integer(1),
        ];
        assert_eq!(
            parse_logical_or(&mut tokens.iter().peekable()),
            Expression::Binary(
                Operator::Or,
                Box::new(Expression::Binary(
                    Operator::And,
                    Box::new(Expression::Binary(
                        Operator::NEQ,
                        Box::new(Expression::Const(1)),
                        Box::new(Expression::Const(2))
                    )),
                    Box::new(Expression::Binary(
                        Operator::EQ,
                        Box::new(Expression::Const(1)),
                        Box::new(Expression::Const(1))
                    ))
                )),
                Box::new(Expression::Const(1))
            )
        );
    }

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
