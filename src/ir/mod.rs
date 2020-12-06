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
 * call(str, u32)    : call function name <str>, parameter_cnt: u32
 * globaladdr id: load global var id addr stack_pointer + 1
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
    Call(String, u32),
    GlobalAddr(String),
    Return,
}

#[derive(Debug, PartialEq)]
pub struct GlobalVarProps {
    pub init: Option<i32>,
    pub size: u32,
}

// GlobalVarMap (var_id, {init?, size})
type GlobalVarMap = HashMap<String, GlobalVarProps>;

#[derive(Debug, PartialEq)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
    pub label_cnt: u32,
    pub global_vars: GlobalVarMap,
}

fn new_label(label_cnt: &mut u32) -> u32 {
    let cnt = *label_cnt;
    *label_cnt += 1;
    cnt
}

// SymbolMap (id_name, index)
type SymbolMap = HashMap<String, u32>;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionProps {
    parameters: Vec<Type>,
    return_type: Type,
    is_defined: bool,
    idx: u32,
}

// FunctionMap (func_name, FunctionProps)
type FunctionMap = HashMap<String, FunctionProps>;

// 作用域
#[derive(Debug, Clone)]
pub struct Scope {
    symbol_map: SymbolMap,
    parent_var_cnt: u32,
}

impl Scope {
    pub fn lookup(&self, id: &str) -> Option<u32> {
        if let Some(i) = self.symbol_map.get(id) {
            Some(i + self.parent_var_cnt)
        } else {
            None
        }
    }
    pub fn insert(&mut self, id: &str) -> u32 {
        let idx = self.symbol_map.len() as u32;
        if self.symbol_map.insert(id.to_owned(), idx).is_some() {
            panic!("Declaration Error: Redefine Var {}", id);
        }
        idx + self.parent_var_cnt
    }
}

fn new_scope(scope_stack: &mut Vec<Scope>) {
    let c = scope_stack.last().unwrap();
    let parent_var_cnt = c.parent_var_cnt + c.symbol_map.len() as u32;
    scope_stack.push(Scope {
        symbol_map: SymbolMap::new(),
        parent_var_cnt,
    })
}

// (idx, is_global)
fn lookup(scope_stack: &[Scope], global_vars: &GlobalVarMap, id: &str) -> (u32, bool) {
    for s in scope_stack.iter().rev() {
        if let Some(i) = s.lookup(id) {
            return (i, false);
        }
    }
    if global_vars.contains_key(id) {
        return (0, true);
    }
    panic!("Var {} Not Found", id);
}

fn insert(scope_stack: &mut Vec<Scope>, id: &str) -> u32 {
    let s = scope_stack.last_mut().unwrap();
    s.insert(id)
}

fn new_fun(map: &mut FunctionMap, function_cnt: &mut u32, fun: &Function) -> u32 {
    let props = FunctionProps {
        parameters: fun
            .parameters
            .iter()
            .map(|decl: &Declaration| -> Type { decl.t })
            .collect(),
        return_type: fun.t,
        is_defined: !fun.statements.is_empty(),
        idx: *function_cnt,
    };

    // 已声明
    if let Some(f) = map.get_mut(&fun.name) {
        // 未定义，语句不为空
        if !f.is_defined && !fun.statements.is_empty() {
            if props.parameters != f.parameters {
                panic!("Function Implementation Error: Different parameters types between Declaration and Implementation\nFunction : {:?}\nDeclaration : {:?}\nImplementation : {:?}", fun.name, f.parameters, props.parameters);
            }
            f.is_defined = true;
        } else {
            // 未定义，语句为空 => 重复声明
            // 已定义 => 多次定义或定义后声明
            panic!("Function Declaration Error: Declare or Implementation more than one times or declare after Implementation\n Function : {:?}", fun.name)
        }
        f.idx
    } else {
        // 未声明或定义
        map.insert(fun.name.to_owned(), props);
        *function_cnt += 1;
        // 返回增加函数后的函数数量，应比 Vec<IRFunction>.len() 大1
        *function_cnt
    }
}

fn check_fun_defined(map: &FunctionMap) {
    for (key, val) in map.iter() {
        if !val.is_defined {
            panic!(
                "Function Implementation Error: Function {} not defined",
                key
            );
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IRFunction {
    pub name: String,
    pub statements: Vec<IRStatement>,
    pub param_cnt: u32,
    pub var_max: u32,
}

struct FunctionStaticProps<'a> {
    name: &'a str,
    function_map: &'a FunctionMap,
    global_vars: &'a GlobalVarMap,
    loop_label: Vec<(u32, u32)>,
    var_max: u32,
}

pub fn ir(program: &Program) -> IRProgram {
    let mut label_cnt: u32 = 0;
    let mut function_cnt: u32 = 0;
    let mut scope_stack: Vec<Scope> = Vec::new();
    let mut functions: Vec<IRFunction> = Vec::new();
    let mut function_map = FunctionMap::new();
    let mut global_vars = GlobalVarMap::new();

    for g in &program.global_vars {
        if global_vars
            .insert(
                g.name.to_owned(),
                GlobalVarProps {
                    init: match g.expression {
                        None => None,
                        Some(Expression::Const(x)) => Some(x),
                        _ => panic!("can't be"),
                    },
                    size: match g.t {
                        Type::Int => 4,
                        _ => panic!("Unimplemented"),
                    },
                },
            )
            .is_some()
        {
            panic!("Global Var Declaration Error: Redefine Var {}", g.name);
        }
    }

    for f in &program.functions {
        let idx = new_fun(&mut function_map, &mut function_cnt, f);
        let func = ir_func(
            f,
            &mut label_cnt,
            &mut scope_stack,
            &function_map,
            &global_vars,
        );
        if idx == functions.len() as u32 + 1 {
            functions.push(func);
        } else {
            functions[idx as usize] = func;
        }
    }
    check_fun_defined(&function_map);
    IRProgram {
        functions,
        label_cnt,
        global_vars,
    }
}

pub fn ir_func(
    function: &Function,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    function_map: &FunctionMap,
    global_vars: &GlobalVarMap,
) -> IRFunction {
    if global_vars.contains_key(&function.name) {
        panic!("Function Global Var Conflict: Same identifier {function.name}");
    }
    scope_stack.push(Scope {
        symbol_map: SymbolMap::new(),
        parent_var_cnt: 0,
    });
    let mut statements: Vec<IRStatement> = Vec::new();
    let mut props = FunctionStaticProps {
        name: &function.name,
        function_map,
        global_vars,
        loop_label: Vec::new(),
        var_max: 0,
    };
    for p in &function.parameters {
        // 只需插入，不需赋值
        insert(scope_stack, &p.name);
    }

    for s in &function.statements {
        ir_stmt(s, label_cnt, scope_stack, &mut props, &mut statements);
    }
    let s = scope_stack.last_mut().unwrap();
    props.var_max = max(props.var_max, s.parent_var_cnt + s.symbol_map.len() as u32);
    scope_stack.pop();

    IRFunction {
        name: function.name.to_owned(),
        statements,
        param_cnt: function.parameters.len() as u32,
        var_max: props.var_max,
    }
}

#[allow(unreachable_patterns)]
fn ir_stmt(
    statement: &Statement,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    props: &mut FunctionStaticProps,
    ir_statements: &mut Vec<IRStatement>,
) {
    match statement {
        Statement::Return(expr) => {
            ir_expr(expr, label_cnt, scope_stack, props, ir_statements);
            ir_statements.push(IRStatement::Return);
        }
        Statement::Declaration(decl) => ir_decl(decl, label_cnt, scope_stack, props, ir_statements),
        Statement::Expression(expr) => match expr {
            Some(e) => {
                ir_expr(e, label_cnt, scope_stack, props, ir_statements);
                // additive multiplicative logical => stack + 1
                // don't pop in assign, so expr => stack + 1
                // Add pop here to restore
                ir_statements.push(IRStatement::Pop);
            }
            None => {}
        },
        Statement::Condition(cond, t_stmt, f_stmt) => {
            let end_label = new_label(label_cnt);
            ir_statements.push(IRStatement::Comment(String::from("If-Else")));
            ir_expr(cond, label_cnt, scope_stack, props, ir_statements);
            match f_stmt {
                None => {
                    ir_statements.push(IRStatement::Beqz(format!(
                        ".L.{}.IfElse_End.{}",
                        props.name, end_label
                    )));
                    ir_stmt(t_stmt, label_cnt, scope_stack, props, ir_statements);
                    ir_statements.push(IRStatement::Label(format!(
                        ".L.{}.IfElse_End.{}",
                        props.name, end_label
                    )));
                }
                Some(s) => {
                    let else_label = new_label(label_cnt);
                    ir_statements.push(IRStatement::Beqz(format!(
                        ".L.{}.IfElse_Else.{}",
                        props.name, else_label
                    )));
                    ir_stmt(t_stmt, label_cnt, scope_stack, props, ir_statements);

                    ir_statements.push(IRStatement::Br(format!(
                        ".L.{}.IfElse_End.{}",
                        props.name, end_label
                    )));
                    ir_statements.push(IRStatement::Label(format!(
                        ".L.{}.IfElse_Else.{}",
                        props.name, else_label
                    )));

                    ir_stmt(s, label_cnt, scope_stack, props, ir_statements);
                    ir_statements.push(IRStatement::Label(format!(
                        ".L.{}.IfElse_End.{}",
                        props.name, end_label
                    )))
                }
            }
        }
        Statement::Compound(stmts) => {
            new_scope(scope_stack);
            for s in stmts {
                ir_stmt(s, label_cnt, scope_stack, props, ir_statements);
            }
            let s = scope_stack.last_mut().unwrap();
            props.var_max = max(props.var_max, s.parent_var_cnt + s.symbol_map.len() as u32);
            scope_stack.pop();
        }
        Statement::Loop(pre, cond, body, post) => {
            /*
             *  {
             *      pre     (for do_while: body)
             *      cond_label
             *      cond
             *      body
             *      continue_label
             *      post
             *  }
             *  break_label
             */
            if **body == Statement::Expression(None) {
                return;
            }
            let cond_label = new_label(label_cnt);
            let (break_label, continue_label) = (new_label(label_cnt), new_label(label_cnt));
            props.loop_label.push((break_label, continue_label));
            new_scope(scope_stack);
            if let Some(pre) = pre {
                ir_stmt(pre, label_cnt, scope_stack, props, ir_statements)
            }
            ir_statements.push(IRStatement::Label(format!(
                ".L.{}.Loop_Cond.{}",
                props.name, cond_label
            )));
            if let Some(cond) = cond {
                ir_expr(cond, label_cnt, scope_stack, props, ir_statements);
                ir_statements.push(IRStatement::Beqz(format!(
                    ".L.{}.Loop_Break.{}",
                    props.name, break_label
                )))
            }
            ir_stmt(body, label_cnt, scope_stack, props, ir_statements);
            ir_statements.push(IRStatement::Label(format!(
                ".L.{}.Loop_Continue.{}",
                props.name, continue_label
            )));
            if let Some(post) = post {
                ir_expr(post, label_cnt, scope_stack, props, ir_statements);
                ir_statements.push(IRStatement::Pop);
            }
            ir_statements.push(IRStatement::Br(format!(
                ".L.{}.Loop_Cond.{}",
                props.name, cond_label
            )));
            let s = scope_stack.last_mut().unwrap();
            props.var_max = max(props.var_max, s.parent_var_cnt + s.symbol_map.len() as u32);
            scope_stack.pop();
            ir_statements.push(IRStatement::Label(format!(
                ".L.{}.Loop_Break.{}",
                props.name, break_label
            )));
            props.loop_label.pop();
        }
        Statement::Break => ir_statements.push(IRStatement::Br(format!(
            ".L.{}.Loop_Break.{}",
            props.name,
            props.loop_label.last().expect("Error: break out of loop").0
        ))),
        Statement::Continue => ir_statements.push(IRStatement::Br(format!(
            ".L.{}.Loop_Continue.{}",
            props.name,
            props
                .loop_label
                .last()
                .expect("Error: continue out of loop")
                .1
        ))),
        _ => unimplemented!(),
    }
}

fn ir_decl(
    declaration: &Declaration,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    props: &FunctionStaticProps,
    ir_statements: &mut Vec<IRStatement>,
) {
    let idx = insert(scope_stack, &declaration.name);
    // int a = x; int a;
    // 后者默认给 0
    // 执行一次赋值操作
    match &declaration.expression {
        Some(expr) => ir_expr(&expr, label_cnt, scope_stack, props, ir_statements),
        None => ir_statements.push(IRStatement::Push(0)),
    }
    ir_statements.push(IRStatement::FrameAddr(idx));
    ir_statements.push(IRStatement::Store);
    ir_statements.push(IRStatement::Pop);
}

#[allow(unreachable_patterns)]
fn ir_expr(
    expr: &Expression,
    label_cnt: &mut u32,
    scope_stack: &mut Vec<Scope>,
    props: &FunctionStaticProps,
    ir_statements: &mut Vec<IRStatement>,
) {
    match expr {
        Expression::Const(int32) => ir_statements.push(IRStatement::Push(*int32)),
        Expression::Unary(unary_op, left) => {
            ir_expr(left, label_cnt, scope_stack, props, ir_statements);
            match *unary_op {
                Operator::Minus => ir_statements.push(IRStatement::Neg),
                Operator::Not => ir_statements.push(IRStatement::LogicalNot),
                Operator::BitwiseNot => ir_statements.push(IRStatement::Not),
                _ => panic!("Expecting unary operators"),
            };
        }
        Expression::Binary(op, left, right) => {
            ir_expr(left, label_cnt, scope_stack, props, ir_statements);
            ir_expr(right, label_cnt, scope_stack, props, ir_statements);
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
            let (idx, is_global) = lookup(scope_stack, props.global_vars, id);
            match is_global {
                true => ir_statements.push(IRStatement::GlobalAddr(id.to_owned())),
                false => ir_statements.push(IRStatement::FrameAddr(idx)),
            }
            ir_statements.push(IRStatement::Load);
        }
        Expression::Assignment(id, expr) => {
            ir_expr(expr, label_cnt, scope_stack, props, ir_statements);
            let (idx, is_global) = lookup(scope_stack, props.global_vars, id);
            match is_global {
                true => ir_statements.push(IRStatement::GlobalAddr(id.to_owned())),
                false => ir_statements.push(IRStatement::FrameAddr(idx)),
            }
            ir_statements.push(IRStatement::Store);
        }
        Expression::Ternary(cond, t_expr, f_expr) => {
            let lable_id = new_label(label_cnt);
            ir_statements.push(IRStatement::Comment(format!("Ternary Label: {}", lable_id)));
            ir_expr(cond, label_cnt, scope_stack, props, ir_statements);
            ir_statements.push(IRStatement::Beqz(format!("Ternary_False_{}", lable_id)));
            ir_expr(t_expr, label_cnt, scope_stack, props, ir_statements);
            ir_statements.push(IRStatement::Br(format!("Ternary_End_{}", lable_id)));
            ir_statements.push(IRStatement::Label(format!("Ternary_False_{}", lable_id)));
            ir_expr(f_expr, label_cnt, scope_stack, props, ir_statements);
            ir_statements.push(IRStatement::Label(format!("Ternary_End_{}", lable_id)));
        }
        Expression::Function(name, params) => {
            let f = props
                .function_map
                .get(name)
                .expect("Call Function Error: Call before Declaration");
            if params.len() != f.parameters.len() {
                panic!("Call Function Error: Wrong parameters");
            }
            for param in params.iter().rev() {
                ir_expr(param, label_cnt, scope_stack, props, ir_statements);
            }
            ir_statements.push(IRStatement::Call(
                name.to_owned(),
                f.parameters.len() as u32,
            ));
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
