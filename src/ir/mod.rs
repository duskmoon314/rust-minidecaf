use crate::lexer::token::*;
use crate::parser::ast::*;

/*
 * IR command
 *
 * push x       : put x in stack, stack_pointer + 1
 * ret          : pop stack top, stack_pointer - 1
 * Neg          : pop, neg, push back
 * Not          : pop, bitwise not, push back
 * LogicalNot   : pop, cpp-like logical not, push back
 * add          : pop twice, add, push ans back, stack_pointer - 1
 * sub          : pop twice, sub, push ans back, stack_pointer - 1
 * mul
 * div
 * rem
 */

#[derive(Debug, PartialEq)]
pub enum IRStatement {
    Push(i32),
    Neg,
    Not,
    LogicalNot,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Return,
}

#[derive(Debug)]
pub struct IRProgram {
    pub function: IRFunction,
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: String,
    pub statement: Vec<IRStatement>,
}

pub fn ir(program: &Program) -> IRProgram {
    IRProgram {
        function: ir_func(&program.function),
    }
}

pub fn ir_func(function: &Function) -> IRFunction {
    let mut statements: Vec<IRStatement> = Vec::new();
    match &function.statement {
        Statement::Return(expr) => {
            ir_expr(&mut statements, expr);
            statements.push(IRStatement::Return);
        }
    }
    IRFunction {
        name: function.name.to_owned(),
        statement: statements,
    }
}

pub fn ir_expr(ir_statements: &mut Vec<IRStatement>, expr: &Expression) {
    match expr {
        Expression::Const(int32) => ir_statements.push(IRStatement::Push(*int32)),
        Expression::Unary(unary_op, left) => {
            ir_expr(ir_statements, left);
            match *unary_op {
                Operator::Minus => ir_statements.push(IRStatement::Neg),
                Operator::Not => ir_statements.push(IRStatement::LogicalNot),
                Operator::BitwiseNot => ir_statements.push(IRStatement::Not),
                _ => panic!("Expecting unary operators"),
            };
        }
        Expression::Binary(op, left, right) => {
            ir_expr(ir_statements, left);
            ir_expr(ir_statements, right);
            match *op {
                Operator::Asterisk => ir_statements.push(IRStatement::Mul),
                Operator::Slash => ir_statements.push(IRStatement::Div),
                Operator::Percent => ir_statements.push(IRStatement::Rem),
                Operator::Plus => ir_statements.push(IRStatement::Add),
                Operator::Minus => ir_statements.push(IRStatement::Sub),
                _ => panic!("Expecting multiplicative operators"),
            }
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_ir_expr() {
        let expr = Expression::Unary(
            Operator::Minus,
            Box::new(Expression::Unary(
                Operator::Not,
                Box::new(Expression::Unary(
                    Operator::BitwiseNot,
                    Box::new(Expression::Const(1)),
                )),
            )),
        );
        let mut ir_stmt: Vec<IRStatement> = Vec::new();
        ir_expr(&mut ir_stmt, &expr);
        assert_eq!(
            ir_stmt,
            [
                IRStatement::Push(1),
                IRStatement::Not,
                IRStatement::LogicalNot,
                IRStatement::Neg
            ]
        );
        let expr = Expression::Binary(
            Operator::Plus,
            Box::new(Expression::Binary(
                Operator::Asterisk,
                Box::new(Expression::Const(1)),
                Box::new(Expression::Const(2)),
            )),
            Box::new(Expression::Const(3)),
        );
        let mut ir_stmt: Vec<IRStatement> = Vec::new();
        ir_expr(&mut ir_stmt, &expr);
        assert_eq!(
            ir_stmt,
            [
                IRStatement::Push(1),
                IRStatement::Push(2),
                IRStatement::Mul,
                IRStatement::Push(3),
                IRStatement::Add
            ]
        )
    }
}
