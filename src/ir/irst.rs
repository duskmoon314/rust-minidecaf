use super::command::*;
use crate::lexer::token::*;
use crate::parser::ast::*;

#[derive(Debug)]
pub struct IRProgram {
    pub function: IRFunction,
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: String,
    pub statement: Vec<IRStatement>,
}

pub fn ir_program(program: &Program) -> IRProgram {
    IRProgram {
        function: ir_function(&program.function),
    }
}

pub fn ir_function(function: &Function) -> IRFunction {
    let mut statements: Vec<IRStatement> = Vec::new();
    ir_statements(&function.statement, &mut statements);
    IRFunction {
        name: function.name.to_owned(),
        statement: statements,
    }
}

fn ir_statements(statement: &Statement, ir_statements: &mut Vec<IRStatement>) {
    match statement {
        Statement::Return(expr) => {
            ir_expression(expr, ir_statements);
            ir_statements.push(IRStatement::Return);
        }
    }
}

#[allow(unreachable_patterns)]
fn ir_expression(expr: &Expression, ir_statements: &mut Vec<IRStatement>) {
    match expr {
        Expression::Const(int32) => ir_statements.push(IRStatement::Push(*int32)),
        Expression::Unary(unary_op, left) => {
            ir_expression(left, ir_statements);
            match *unary_op {
                Operator::Minus => ir_statements.push(IRStatement::Neg),
                Operator::Not => ir_statements.push(IRStatement::LogicalNot),
                Operator::BitwiseNot => ir_statements.push(IRStatement::Not),
                _ => panic!("Expecting unary operators"),
            };
        }
    }
}
