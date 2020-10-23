use crate::lexer::token::*;
use crate::parser::ast::*;
use std::collections::HashMap;

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
 * eq           : pop twice, push eq ? 1 : 0, stack_pointer - 1
 * ne
 * le
 * ge
 * lt
 * gt
 * LogicalAnd
 * LogicalOr
 * frameaddr i k: put ith frame kth element addr in stack, stack_pointer + 1
 * load         : pop, load, put back
 * store        : pop => addr, pop => value, save value to addr, stack_pointer - 1
 * pop          : pop, stack_pointer - 1
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
    EQ,
    NEQ,
    LE,
    GE,
    LT,
    GT,
    LogicalAnd,
    LogicalOr,
    FrameAddr(u32, u32),
    Load,
    Store,
    Pop,
    Return,
}

#[derive(Debug, PartialEq)]
pub struct IRProgram {
    pub function: IRFunction,
}

type SymbolMap = HashMap<String, u32>;

// 作用域
#[derive(Debug, PartialEq)]
pub struct Scope {
    pub symbol_map: SymbolMap,
    parent: Option<Box<Scope>>,
    pub statements: Vec<IRStatement>,
}

impl Scope {
    // 递归寻找变量，返回 (递归次数, 变量索引)
    pub fn lookup(&self, id: &str, iter: u32) -> (u32, u32) {
        if let Some(idx) = self.symbol_map.get(id) {
            (iter, *idx)
        } else {
            match &self.parent {
                Some(parent_scope) => parent_scope.lookup(id, iter + 1),
                _ => panic!("Var {} not defined", id),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub scope: Scope,
}

pub fn ir(program: &Program) -> IRProgram {
    IRProgram {
        function: ir_func(&program.function),
    }
}

pub fn ir_func(function: &Function) -> IRFunction {
    let mut scope = Scope {
        symbol_map: SymbolMap::new(),
        parent: None,
        statements: Vec::new(),
    };
    for s in &function.statements {
        ir_stmt(&mut scope, s)
    }
    IRFunction {
        name: function.name.to_owned(),
        scope: scope,
    }
}

#[allow(unreachable_patterns)]
pub fn ir_stmt(scope: &mut Scope, statement: &Statement) {
    match statement {
        Statement::Return(expr) => {
            ir_expr(scope, expr);
            scope.statements.push(IRStatement::Return);
        }
        Statement::Declaration(decl) => ir_decl(scope, decl),
        Statement::Expression(expr) => match expr {
            Some(e) => {
                ir_expr(scope, e);
                // additive multiplicative logical => stack + 1
                // don't pop in assign, so expr => stack + 1
                // Add pop here to restore
                scope.statements.push(IRStatement::Pop)
            }
            None => {}
        },
        _ => unimplemented!(),
    }
}

pub fn ir_decl(scope: &mut Scope, declaration: &Declaration) {
    let idx = scope.symbol_map.len() as u32;
    if scope
        .symbol_map
        .insert(declaration.name.to_owned(), idx)
        .is_some()
    {
        panic!("Declaration Error: Redefine Var {}", declaration.name);
    }
    // int a = x; int a;
    // 后者默认给 0
    // 执行一次赋值操作
    match &declaration.expression {
        Some(expr) => ir_expr(scope, &expr),
        None => scope.statements.push(IRStatement::Push(0)),
    }
    scope.statements.push(IRStatement::FrameAddr(0, idx));
    scope.statements.push(IRStatement::Store);
    scope.statements.push(IRStatement::Pop);
}

#[allow(unreachable_patterns)]
pub fn ir_expr(scope: &mut Scope, expr: &Expression) {
    match expr {
        Expression::Const(int32) => scope.statements.push(IRStatement::Push(*int32)),
        Expression::Unary(unary_op, left) => {
            ir_expr(scope, left);
            match *unary_op {
                Operator::Minus => scope.statements.push(IRStatement::Neg),
                Operator::Not => scope.statements.push(IRStatement::LogicalNot),
                Operator::BitwiseNot => scope.statements.push(IRStatement::Not),
                _ => panic!("Expecting unary operators"),
            };
        }
        Expression::Binary(op, left, right) => {
            ir_expr(scope, left);
            ir_expr(scope, right);
            match *op {
                Operator::Asterisk => scope.statements.push(IRStatement::Mul),
                Operator::Slash => scope.statements.push(IRStatement::Div),
                Operator::Percent => scope.statements.push(IRStatement::Rem),
                Operator::Plus => scope.statements.push(IRStatement::Add),
                Operator::Minus => scope.statements.push(IRStatement::Sub),
                Operator::LT => scope.statements.push(IRStatement::LT),
                Operator::GT => scope.statements.push(IRStatement::GT),
                Operator::LE => scope.statements.push(IRStatement::LE),
                Operator::GE => scope.statements.push(IRStatement::GE),
                Operator::EQ => scope.statements.push(IRStatement::EQ),
                Operator::NEQ => scope.statements.push(IRStatement::NEQ),
                Operator::And => scope.statements.push(IRStatement::LogicalAnd),
                Operator::Or => scope.statements.push(IRStatement::LogicalOr),
                _ => panic!("Expecting binary operators"),
            }
        }
        Expression::Variable(id) => {
            let (iter, idx) = scope.lookup(id, 0);
            scope.statements.push(IRStatement::FrameAddr(iter, idx));
            scope.statements.push(IRStatement::Load);
        }
        Expression::Assignment(id, expr) => {
            ir_expr(scope, expr);
            let (iter, idx) = scope.lookup(id, 0);
            scope.statements.push(IRStatement::FrameAddr(iter, idx));
            scope.statements.push(IRStatement::Store);
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
        let mut scope = Scope {
            symbol_map: SymbolMap::new(),
            parent: None,
            statements: Vec::new(),
        };
        ir_expr(&mut scope, &expr);
        assert_eq!(
            scope.statements,
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
        let mut scope = Scope {
            symbol_map: SymbolMap::new(),
            parent: None,
            statements: Vec::new(),
        };
        ir_expr(&mut scope, &expr);
        assert_eq!(
            scope.statements,
            [
                IRStatement::Push(1),
                IRStatement::Push(2),
                IRStatement::Mul,
                IRStatement::Push(3),
                IRStatement::Add
            ]
        )
    }

    #[test]
    fn test_ir_func() {
        /*
        int main ()
        {
            int a = 1;
            a = a + 1;
            return a;
        }
        */
        let fun = Function {
            name: "main".to_string(),
            t: Type::Int,
            statements: vec![
                Statement::Declaration(Declaration {
                    t: Type::Int,
                    name: "a".to_string(),
                    expression: Some(Expression::Const(1)),
                }),
                Statement::Expression(Some(Expression::Assignment(
                    "a".to_string(),
                    Box::new(Expression::Binary(
                        Operator::Plus,
                        Box::new(Expression::Variable("a".to_string())),
                        Box::new(Expression::Const(1)),
                    )),
                ))),
                Statement::Return(Expression::Variable("a".to_string())),
            ],
        };
        let mut s_map = SymbolMap::new();
        s_map.insert("a".to_string(), 0);
        assert_eq!(
            ir_func(&fun),
            IRFunction {
                name: "main".to_string(),
                scope: Scope {
                    parent: None,
                    symbol_map: s_map,
                    statements: vec![
                        // int a = 1;
                        IRStatement::Push(1),
                        IRStatement::FrameAddr(0, 0),
                        IRStatement::Store,
                        IRStatement::Pop,
                        // a = a + 1;
                        IRStatement::FrameAddr(0, 0),
                        IRStatement::Load,
                        IRStatement::Push(1),
                        IRStatement::Add,
                        IRStatement::FrameAddr(0, 0),
                        IRStatement::Store,
                        IRStatement::Pop,
                        // return a;
                        IRStatement::FrameAddr(0, 0),
                        IRStatement::Load,
                        IRStatement::Return
                    ]
                }
            }
        )
    }
}
