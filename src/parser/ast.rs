use crate::lexer::token::*;
use peek_nth::PeekableNth;
use std::slice::Iter;

/*
 * Program      -> Function
 * Function     -> Type Identifier Lparen Rparen Compound_Statement
 * Type         -> Type(Int | Double)
 * BlockItem    -> Statement | Declaration
 * Compound_Statement -> Lbrace Statement* Rbrace
 * Statement    ->  | Return Expression Semicolon
 *                  | Expression? Semicolon
 *                  | If ( Expression ) Statement (Else statement)?
 *                  | Compound_Statement
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
 * Conditional  -> Logical_Or | Logical_Or ? Expression : Conditional
 * Assignment   -> Conditional | Identifier = Expression
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
    Condition(Expression, Box<Statement>, Option<Box<Statement>>), // 由 parse 函数确保不会有 Declaration 作为子句
    Compound(Vec<Statement>),
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
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // condition ? expression : ternary
    Assignment(String, Box<Expression>),                        // var = ()
    Variable(String),                                           // var
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

// Conditional => cond ? t_expr : f_expr
pub fn parse_conditional(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let cond = parse_logical_or(tokens);
    match tokens.peek() {
        Some(Token::Operator(Operator::Condition)) => {
            tokens.next();
            let t_expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Operator(Operator::Colon)) => {
                    let f_expr = parse_conditional(tokens);
                    Expression::Ternary(Box::new(cond), Box::new(t_expr), Box::new(f_expr))
                }
                _ => panic!("Conditional Error: Expecting : in ternary"),
            }
        }
        _ => cond,
    }
}

// Assignment => identifier = ? parse_assignment : parse_conditional
pub fn parse_assignment(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.peek() {
        Some(Token::Identifier(name)) => match tokens.peek_nth(1) {
            Some(Token::Operator(Operator::Assign)) => {
                tokens.next();
                tokens.next();
                let expr = parse_expression(tokens);
                Expression::Assignment(name.to_owned(), Box::new(expr))
            }
            _ => parse_conditional(tokens),
        },
        _ => parse_conditional(tokens),
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
        Some(Token::Symbol(Symbol::Lbrace)) => {
            // { statement* }
            tokens.next(); // consume {
            let mut stmts: Vec<Statement> = Vec::new();
            while tokens.peek() != Some(&&Token::Symbol(Symbol::Rbrace)) {
                stmts.push(parse_block_item(tokens));
            }
            tokens.next(); // consume }
            return Statement::Compound(stmts);
        }
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
        Some(Token::Keyword(Keyword::If)) => {
            tokens.next(); // consume if
            match tokens.next() {
                Some(Token::Symbol(Symbol::Lparen)) => {}
                _ => panic!("IfElse Error: Expecting ("),
            }
            let cond = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Rparen)) => {}
                _ => panic!("IfElse Error: Expecting )"),
            }
            let t_stmt = parse_statement(tokens);
            match tokens.peek() {
                Some(Token::Keyword(Keyword::Else)) => {
                    tokens.next(); // consume else
                    let f_stmt = parse_statement(tokens);
                    Statement::Condition(cond, Box::new(t_stmt), Some(Box::new(f_stmt)))
                }
                _ => Statement::Condition(cond, Box::new(t_stmt), None),
            }
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

pub fn parse_block_item(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.peek() {
        Some(Token::Type(_)) => {
            let decl = parse_declaration(tokens);
            return Statement::Declaration(decl);
        }
        _ => parse_statement(tokens),
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
                                    statements.push(parse_block_item(tokens));
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

        /*
         * {
         * a = 1 ? 1 : 0;
         * }
         */
        let tokens = vec![
            Token::Symbol(Symbol::Lbrace),
            Token::Identifier("a".to_string()),
            Token::Operator(Operator::Assign),
            Token::Integer(1),
            Token::Operator(Operator::Condition),
            Token::Integer(1),
            Token::Operator(Operator::Colon),
            Token::Integer(0),
            Token::Symbol(Symbol::Semicolon),
            Token::Symbol(Symbol::Rbrace),
        ];
        assert_eq!(
            parse_statement(&mut tokens.iter().peekable_nth()),
            Statement::Compound(vec![Statement::Expression(Some(Expression::Assignment(
                "a".to_string(),
                Box::new(Expression::Ternary(
                    Box::new(Expression::Const(1)),
                    Box::new(Expression::Const(1)),
                    Box::new(Expression::Const(0))
                ))
            )))])
        );
    }

    #[test]
    fn test_parse_condition() {
        /*
         * if (1)
         * if (2)
         *     ;
         * else
         *     ;
         */
        let tokens = vec![
            Token::Keyword(Keyword::If),
            Token::Symbol(Symbol::Lparen),
            Token::Integer(1),
            Token::Symbol(Symbol::Rparen),
            Token::Keyword(Keyword::If),
            Token::Symbol(Symbol::Lparen),
            Token::Integer(2),
            Token::Symbol(Symbol::Rparen),
            Token::Symbol(Symbol::Semicolon),
            Token::Keyword(Keyword::Else),
            Token::Symbol(Symbol::Semicolon),
        ];
        assert_eq!(
            parse_statement(&mut tokens.iter().peekable_nth()),
            Statement::Condition(
                Expression::Const(1),
                Box::new(Statement::Condition(
                    Expression::Const(2),
                    Box::new(Statement::Expression(None)),
                    Some(Box::new(Statement::Expression(None)))
                )),
                None
            )
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
