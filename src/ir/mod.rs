use crate::lexer::token::*;
use crate::parser::ast::*;

/*
 * IR command
 *
 * push x   : put x in stack, stack_pointer + 1
 * ret      : pop stack top, stack_pointer - 1
 */

#[derive(Debug, PartialEq)]
pub enum IRStatement {
    Push(i32),
    Neg,
    Not,
    LogicalNot,
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
        _ => (),
    }
}

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
    }
}
