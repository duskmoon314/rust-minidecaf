use crate::parser::ast::*;

/*
 * IR command
 *
 * push x   : put x in stack, stack_pointer + 1
 * ret      : pop stack top, stack_pointer - 1
 */

#[derive(Debug)]
pub enum IRStatement {
    Push(i32),
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
    }
}
