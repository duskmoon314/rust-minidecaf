use crate::lexer::token::*;
use peek_nth::PeekableNth;
use std::slice::Iter;

/*
 * Program      -> Function
 * Function     -> Type Identifier Lparen Rparen Lbrace Statement* Rbrace
 * Type         -> Type(Int | Double)
 * Statement    ->  | Return Expression Semicolon
 *                  | Expression? Semicolon
 *                  | Declaration
 * Declaration  -> Type Identifier (= Expression)? Semicolon
 * Expression   ->  | Assignment
 * Factor       ->  | Integer(i32)
 *                  | ~ ! - Factor
 *                  | ( Expression )
 *                  | Identifier
 * Multiplicative -> Factor | Multiplicative * / % Factor
 * Additive     -> Multiplicative | Additive + - Multiplicative
 * Relational   -> Additive | Relational < > <= >= Additive
 * Equality     -> Relational | Equality == != Relational
 * Logical_And  -> Equality | Logical_And && Equality
 * Logical_Or   -> Logical_And | Logical_Or || Logical_And
 * Assignment   -> Logical_Or | Identifier = Expression
 */

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub t: Type,
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Declaration(Declaration),
    Expression(Option<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub t: Type,
    pub name: String,
    pub expression: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Const(i32),
    Unary(Operator, Box<Expression>),
    Binary(Operator, Box<Expression>, Box<Expression>), // Binary Operations (+ a b)
    // Additive(Box<Expression>, Operator, Box<Expression>)
    // Multiplicative(Box<Expression>, Operator, Box<Expression>)
    // Relational(Box<Expression>, Operator, Box<Expression>)
    // Equality(Box<Expression>, Operator, Box<Expression>)
    Assignment(String, Box<Expression>), // var = ();
    Variable(String),                    // var
}

// Factor => (<expression>) | Unary | Const(i32)
pub fn parse_factor(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.next() {
        Some(next) => match next {
            Token::Symbol(Symbol::Lparen) => {
                // ( <expression> )
                let expr = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::Symbol(Symbol::Rparen)) => return expr,
                    _ => panic!("Missing ) to close expression"),
                }
            }
            Token::Operator(unary_op) if unary_op.is_unary() => {
                let expr = parse_factor(tokens);
                return Expression::Unary(*unary_op, Box::new(expr));
            }
            Token::Integer(num) => return Expression::Const(*num), // <int>
            Token::Identifier(name) => return Expression::Variable(name.to_owned()),
            _ => panic!("Expected factor, Nothing found"),
        },
        _ => panic!("Nothing Left"),
    }
}

// Multiplicative => factor * / % factor
pub fn parse_multiplicative(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
pub fn parse_additive(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
pub fn parse_relational(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
pub fn parse_equality(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
pub fn parse_logical_and(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
pub fn parse_logical_or(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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

// Assignment => identifier = ? parse_assignment : parse_logical_or
pub fn parse_assignment(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.peek() {
        Some(Token::Identifier(name)) => match tokens.peek_nth(1) {
            Some(Token::Operator(Operator::Assign)) => {
                tokens.next();
                tokens.next();
                let expr = parse_expression(tokens);
                Expression::Assignment(name.to_owned(), Box::new(expr))
            }
            _ => parse_logical_or(tokens),
        },
        _ => parse_logical_or(tokens),
    }
}

pub fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    parse_assignment(tokens)
}

pub fn parse_declaration(tokens: &mut PeekableNth<Iter<Token>>) -> Declaration {
    match tokens.next() {
        Some(Token::Type(t)) => match tokens.next() {
            Some(Token::Identifier(name)) => match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => Declaration {
                    t: *t,
                    name: name.to_owned(),
                    expression: None,
                },
                Some(Token::Operator(Operator::Assign)) => {
                    let expr = parse_expression(tokens);
                    match tokens.next() {
                        Some(Token::Symbol(Symbol::Semicolon)) => Declaration {
                            t: *t,
                            name: name.to_owned(),
                            expression: Some(expr),
                        },
                        _ => panic!("Declaration Err: Expecting ; at the end"),
                    }
                }
                _ => panic!("Declaration Err: Expecting ; or ="),
            },
            _ => panic!("Declaration Err: Expecting identifier name"),
        },
        _ => panic!("Declaration Err: Expecting type token"),
    }
}

pub fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.peek() {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            let expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => {
                    return Statement::Return(expr);
                }
                _ => panic!("Statement Err: Expecting ; at the end"),
            }
        }
        Some(Token::Type(_)) => {
            let decl = parse_declaration(tokens);
            return Statement::Declaration(decl);
        }
        Some(Token::Symbol(Symbol::Semicolon)) => {
            tokens.next();
            return Statement::Expression(None);
        }
        _ => {
            let expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => {
                    return Statement::Expression(Some(expr));
                }
                _ => panic!("Statement Err: Expecting ; at the end"),
            }
        }
    }
}

pub fn parse_function(tokens: &mut PeekableNth<Iter<Token>>) -> Function {
    match tokens.next() {
        Some(Token::Type(t)) => match t {
            Type::Int => match tokens.next() {
                Some(Token::Identifier(id)) => match tokens.next() {
                    Some(Token::Symbol(Symbol::Lparen)) => match tokens.next() {
                        Some(Token::Symbol(Symbol::Rparen)) => match tokens.next() {
                            Some(Token::Symbol(Symbol::Lbrace)) => {
                                let mut statements: Vec<Statement> = Vec::new();
                                while tokens.peek() != Some(&&Token::Symbol(Symbol::Rbrace)) {
                                    statements.push(parse_statement(tokens));
                                }
                                // check return
                                match statements.last() {
                                    Some(Statement::Return(_)) => (),
                                    _ => {
                                        if id == "main" {
                                            statements.push(Statement::Return(Expression::Const(0)))
                                        }
                                    }
                                }
                                match tokens.next() {
                                    Some(Token::Symbol(Symbol::Rbrace)) => {
                                        return Function {
                                            t: *t,
                                            name: id.to_owned(),
                                            statements: statements,
                                        }
                                    }
                                    _ => panic!("Function Err: Expecting }"),
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
    use peek_nth::IteratorExt;

    #[test]
    fn test_parse_multiplicative() {
        let tokens = vec![
            Token::Integer(1),
            Token::Operator(Operator::Asterisk),
            Token::Integer(2),
        ];
        assert_eq!(
            parse_multiplicative(&mut tokens.iter().peekable_nth()),
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
            parse_multiplicative(&mut tokens.iter().peekable_nth()),
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
            parse_additive(&mut tokens.iter().peekable_nth()),
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
            parse_relational(&mut tokens.iter().peekable_nth()),
            Expression::Binary(
                Operator::LT,
                Box::new(Expression::Const(1)),
                Box::new(Expression::Const(2))
            )
        );
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
            parse_logical_or(&mut tokens.iter().peekable_nth()),
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
            parse_expression(&mut tokens.iter().peekable_nth()),
            Expression::Const(123)
        );
        let tokens = vec![Token::Operator(Operator::Minus), Token::Integer(10)];
        assert_eq!(
            parse_expression(&mut tokens.iter().peekable_nth()),
            Expression::Unary(Operator::Minus, Box::new(Expression::Const(10)))
        );
    }

    #[test]
    fn test_parse_declaration() {
        let tokens = vec![
            Token::Type(Type::Int),
            Token::Identifier("a".to_string()),
            Token::Operator(Operator::Assign),
            Token::Integer(1),
            Token::Symbol(Symbol::Semicolon),
        ];
        assert_eq!(
            parse_declaration(&mut tokens.iter().peekable_nth()),
            Declaration {
                t: Type::Int,
                name: "a".to_string(),
                expression: Some(Expression::Const(1))
            }
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
            parse_statement(&mut tokens.iter().peekable_nth()),
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
            parse_function(&mut tokens.iter().peekable_nth()),
            Function {
                t: Type::Int,
                name: "main".to_string(),
                statements: vec![Statement::Return(Expression::Const(0))]
            }
        );
    }
}
