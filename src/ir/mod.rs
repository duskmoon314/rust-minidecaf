use crate::lexer::token::*;
use crate::parser::ast::*;
use std::cmp::max;
use std::collections::HashMap;

type Label = String;

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
 * frameaddr k  : put kth element addr in stack, stack_pointer + 1
 * load         : pop, load, put back
 * store        : pop => addr, pop => value, save value to addr, stack_pointer - 1
 * pop          : pop, stack_pointer - 1
 * label(str)   : do nothing, mark a destination for some j or branch command
 * beqz(str)    : pop, if zero, jump to str, stack_pointer - 1
 * bnez(str)    : pop, if not zero, jump to str, stack_pointer - 1
 * br(str)      : jump to str
 * comment(str) : write "# {str}" to asm file
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
    FrameAddr(u32),
    Load,
    Store,
    Pop,
    Label(Label),
    Br(Label),
    Beqz(Label),
    Bnez(Label),
    Comment(String),
    Return,
}

#[derive(Debug, PartialEq)]
pub struct IRProgram {
    pub function: IRFunction,
    // pub label_cnt: u32,
}

fn new_label(label_cnt: &mut u32) -> u32 {
    let cnt = *label_cnt;
    *label_cnt += 1;
    return cnt;
}

// SymbolMap (id_name, index)
type SymbolMap = HashMap<String, u32>;

// 作用域
#[derive(Debug, Clone)]
pub struct Scope {
    symbol_map: SymbolMap,
    parent_var_cnt: u32,
}

impl Scope {
    pub fn lookup(&self, id: &str) -> Option<u32> {
        if let Some(i) = self.symbol_map.get(id) {
            return Some(i + self.parent_var_cnt);
        } else {
            return None;
        }
    }
    pub fn insert(&mut self, id: &str) -> u32 {
        let idx = self.symbol_map.len() as u32;
        if self.symbol_map.insert(id.to_owned(), idx).is_some() {
            panic!("Declaration Error: Redefine Var {}", id);
        }
        return idx + self.parent_var_cnt;
    }
}

fn new_scope(scope_stack: &mut Vec<Scope>) {
    let c = scope_stack.last().unwrap();
    let parent_var_cnt = c.parent_var_cnt + c.symbol_map.len() as u32;
    scope_stack.push(Scope {
        symbol_map: SymbolMap::new(),
        parent_var_cnt: parent_var_cnt,
    })
}

fn lookup(scope_stack: &Vec<Scope>, id: &str) -> u32 {
    for s in scope_stack.iter().rev() {
        match s.lookup(id) {
            Some(i) => {
                return i;
            }
            _ => {}
        }
    }
    panic!("Var {} Not Found", id);
}

fn insert(scope_stack: &mut Vec<Scope>, id: &str) -> u32 {
    let s = scope_stack.last_mut().unwrap();
    s.insert(id)
}

#[derive(Debug, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub statements: Vec<IRStatement>,
    pub var_max: u32,
}

pub fn ir(program: &Program) -> IRProgram {
    let mut label_cnt: u32 = 0;
    let mut scope_stack: Vec<Scope> = Vec::new();
    IRProgram {
        function: ir_func(&program.function, &mut label_cnt, &mut scope_stack),
    }
}

pub fn ir_func(
    function: &Function,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
) -> IRFunction {
    scope_stack.push(Scope {
        symbol_map: SymbolMap::new(),
        parent_var_cnt: 0,
    });
    let mut statements: Vec<IRStatement> = Vec::new();
    let mut var_max: u32 = 0;
    for s in &function.statements {
        ir_stmt(&s, label_cnt, scope_stack, &mut var_max, &mut statements);
    }
    let s = scope_stack.last_mut().unwrap();
    var_max = max(var_max, s.parent_var_cnt + s.symbol_map.len() as u32);
    scope_stack.pop();

    return IRFunction {
        name: function.name.to_owned(),
        statements: statements,
        var_max: var_max,
    };
}

#[allow(unreachable_patterns)]
pub fn ir_stmt(
    statement: &Statement,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    var_max: &mut u32,
    ir_statements: &mut Vec<IRStatement>,
) {
    match statement {
        Statement::Return(expr) => {
            ir_expr(expr, label_cnt, scope_stack, ir_statements);
            ir_statements.push(IRStatement::Return);
        }
        Statement::Declaration(decl) => ir_decl(decl, label_cnt, scope_stack, ir_statements),
        Statement::Expression(expr) => match expr {
            Some(e) => {
                ir_expr(e, label_cnt, scope_stack, ir_statements);
                // additive multiplicative logical => stack + 1
                // don't pop in assign, so expr => stack + 1
                // Add pop here to restore
                ir_statements.push(IRStatement::Pop)
            }
            None => {}
        },
        Statement::Condition(cond, t_stmt, f_stmt) => {
            let lable_id = new_label(label_cnt);
            ir_statements.push(IRStatement::Comment(format!("If-Else Label: {}", lable_id)));
            ir_expr(cond, label_cnt, scope_stack, ir_statements);
            match f_stmt {
                None => {
                    ir_statements.push(IRStatement::Beqz(format!("IfElse_End_{}", lable_id)));
                    ir_stmt(t_stmt, label_cnt, scope_stack, var_max, ir_statements);
                    ir_statements.push(IRStatement::Label(format!("IfElse_End_{}", lable_id)));
                }
                Some(s) => {
                    ir_statements.push(IRStatement::Beqz(format!("Else_{}", lable_id)));
                    ir_stmt(t_stmt, label_cnt, scope_stack, var_max, ir_statements);

                    ir_statements.push(IRStatement::Br(format!("IfElse_End_{}", lable_id)));
                    ir_statements.push(IRStatement::Label(format!("Else_{}", lable_id)));

                    ir_stmt(s, label_cnt, scope_stack, var_max, ir_statements);
                    ir_statements.push(IRStatement::Label(format!("IfElse_End_{}", lable_id)))
                }
            }
        }
        Statement::Compound(stmts) => {
            new_scope(scope_stack);
            for s in stmts {
                ir_stmt(s, label_cnt, scope_stack, var_max, ir_statements);
            }
            let s = scope_stack.last_mut().unwrap();
            *var_max = max(*var_max, s.parent_var_cnt + s.symbol_map.len() as u32);
            scope_stack.pop();
        }
        _ => unimplemented!(),
    }
}

pub fn ir_decl(
    declaration: &Declaration,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    ir_statements: &mut Vec<IRStatement>,
) {
    let idx = insert(scope_stack, &declaration.name);
    // int a = x; int a;
    // 后者默认给 0
    // 执行一次赋值操作
    match &declaration.expression {
        Some(expr) => ir_expr(&expr, label_cnt, scope_stack, ir_statements),
        None => ir_statements.push(IRStatement::Push(0)),
    }
    ir_statements.push(IRStatement::FrameAddr(idx));
    ir_statements.push(IRStatement::Store);
    ir_statements.push(IRStatement::Pop);
}

#[allow(unreachable_patterns)]
pub fn ir_expr(
    expr: &Expression,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    ir_statements: &mut Vec<IRStatement>,
) {
    match expr {
        Expression::Const(int32) => ir_statements.push(IRStatement::Push(*int32)),
        Expression::Unary(unary_op, left) => {
            ir_expr(left, label_cnt, scope_stack, ir_statements);
            match *unary_op {
                Operator::Minus => ir_statements.push(IRStatement::Neg),
                Operator::Not => ir_statements.push(IRStatement::LogicalNot),
                Operator::BitwiseNot => ir_statements.push(IRStatement::Not),
                _ => panic!("Expecting unary operators"),
            };
        }
        Expression::Binary(op, left, right) => {
            ir_expr(left, label_cnt, scope_stack, ir_statements);
            ir_expr(right, label_cnt, scope_stack, ir_statements);
            match *op {
                Operator::Asterisk => ir_statements.push(IRStatement::Mul),
                Operator::Slash => ir_statements.push(IRStatement::Div),
                Operator::Percent => ir_statements.push(IRStatement::Rem),
                Operator::Plus => ir_statements.push(IRStatement::Add),
                Operator::Minus => ir_statements.push(IRStatement::Sub),
                Operator::LT => ir_statements.push(IRStatement::LT),
                Operator::GT => ir_statements.push(IRStatement::GT),
                Operator::LE => ir_statements.push(IRStatement::LE),
                Operator::GE => ir_statements.push(IRStatement::GE),
                Operator::EQ => ir_statements.push(IRStatement::EQ),
                Operator::NEQ => ir_statements.push(IRStatement::NEQ),
                Operator::And => ir_statements.push(IRStatement::LogicalAnd),
                Operator::Or => ir_statements.push(IRStatement::LogicalOr),
                _ => panic!("Expecting binary operators"),
            }
        }
        Expression::Variable(id) => {
            let idx = lookup(scope_stack, id);
            ir_statements.push(IRStatement::FrameAddr(idx));
            ir_statements.push(IRStatement::Load);
        }
        Expression::Assignment(id, expr) => {
            ir_expr(expr, label_cnt, scope_stack, ir_statements);
            let idx = lookup(scope_stack, id);
            ir_statements.push(IRStatement::FrameAddr(idx));
            ir_statements.push(IRStatement::Store);
        }
        Expression::Ternary(cond, t_expr, f_expr) => {
            let lable_id = new_label(label_cnt);
            ir_statements.push(IRStatement::Comment(format!("Ternary Label: {}", lable_id)));
            ir_expr(cond, label_cnt, scope_stack, ir_statements);
            ir_statements.push(IRStatement::Beqz(format!("Ternary_False_{}", lable_id)));
            ir_expr(t_expr, label_cnt, scope_stack, ir_statements);
            ir_statements.push(IRStatement::Br(format!("Ternary_End_{}", lable_id)));
            ir_statements.push(IRStatement::Label(format!("Ternary_False_{}", lable_id)));
            ir_expr(f_expr, label_cnt, scope_stack, ir_statements);
            ir_statements.push(IRStatement::Label(format!("Ternary_End_{}", lable_id)));
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_ir_expr() {
        // let expr = Expression::Unary(
        //     Operator::Minus,
        //     Box::new(Expression::Unary(
        //         Operator::Not,
        //         Box::new(Expression::Unary(
        //             Operator::BitwiseNot,
        //             Box::new(Expression::Const(1)),
        //         )),
        //     )),
        // );
        // let mut scope = Scope {
        //     symbol_map: SymbolMap::new(),
        //     parent: None,
        //     statements: Vec::new(),
        //     label_cnt: Rc::new(RefCell::new(0)),
        // };
        // ir_expr(&mut scope, &expr);
        // assert_eq!(
        //     ir_statements,
        //     [
        //         IRStatement::Push(1),
        //         IRStatement::Not,
        //         IRStatement::LogicalNot,
        //         IRStatement::Neg
        //     ]
        // );
        // let expr = Expression::Binary(
        //     Operator::Plus,
        //     Box::new(Expression::Binary(
        //         Operator::Asterisk,
        //         Box::new(Expression::Const(1)),
        //         Box::new(Expression::Const(2)),
        //     )),
        //     Box::new(Expression::Const(3)),
        // );
        // let mut scope = Scope {
        //     symbol_map: SymbolMap::new(),
        //     parent: None,
        //     statements: Vec::new(),
        //     label_cnt: Rc::new(RefCell::new(0)),
        // };
        // ir_expr(&mut scope, &expr);
        // assert_eq!(
        //     ir_statements,
        //     [
        //         IRStatement::Push(1),
        //         IRStatement::Push(2),
        //         IRStatement::Mul,
        //         IRStatement::Push(3),
        //         IRStatement::Add
        //     ]
        // )
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
        // let fun = Function {
        //     name: "main".to_string(),
        //     t: Type::Int,
        //     statements: vec![
        //         Statement::Declaration(Declaration {
        //             t: Type::Int,
        //             name: "a".to_string(),
        //             expression: Some(Expression::Const(1)),
        //         }),
        //         Statement::Expression(Some(Expression::Assignment(
        //             "a".to_string(),
        //             Box::new(Expression::Binary(
        //                 Operator::Plus,
        //                 Box::new(Expression::Variable("a".to_string())),
        //                 Box::new(Expression::Const(1)),
        //             )),
        //         ))),
        //         Statement::Return(Expression::Variable("a".to_string())),
        //     ],
        // };
        // let mut s_map = SymbolMap::new();
        // s_map.insert("a".to_string(), 0);
        // let label_cnt = Rc::new(RefCell::new(0));
        // assert_eq!(
        //     ir_func(&fun, &label_cnt),
        //     IRFunction {
        //         name: "main".to_string(),
        //         label_cnt: Rc::new(RefCell::new(0)),
        //         scope: Scope {
        //             parent: None,
        //             symbol_map: s_map,
        //             label_cnt: Rc::new(RefCell::new(0)),
        //             statements: vec![
        //                 // int a = 1;
        //                 IRStatement::Push(1),
        //                 IRStatement::FrameAddr(0, 0),
        //                 IRStatement::Store,
        //                 IRStatement::Pop,
        //                 // a = a + 1;
        //                 IRStatement::FrameAddr(0, 0),
        //                 IRStatement::Load,
        //                 IRStatement::Push(1),
        //                 IRStatement::Add,
        //                 IRStatement::FrameAddr(0, 0),
        //                 IRStatement::Store,
        //                 IRStatement::Pop,
        //                 // return a;
        //                 IRStatement::FrameAddr(0, 0),
        //                 IRStatement::Load,
        //                 IRStatement::Return
        //             ]
        //         }
        //     }
        // )
    }
}
