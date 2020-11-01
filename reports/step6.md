# Step 6

> 贺鲲鹏 2018011169

## 实验内容

### 目的

增加 if else 和 问号表达式

### 实现

#### `lexer`

增加对 `?` `:` 的解析

关键字 `if` `else` 先前已经添加

#### `parser`

修改生成式

```diff
  * Program      -> Function
  * Function     -> Type Identifier Lparen Rparen Lbrace Statement* Rbrace
  * Type         -> Type(Int | Double)
+ * BlockItem    -> Statement | Declaration
  * Statement    ->  | Return Expression Semicolon
  *                  | Expression? Semicolon
- *                  | Declaration
+ *                  | If ( Expresion ) Statement (Else statement)?
  * Declaration  -> Type Identifier (= Expression)? Semicolon
  * Expression   ->  | Assignment
  * Factor       ->  | Integer(i32)
@@ -21,7 +22,8 @@ use std::slice::Iter;
  * Equality     -> Relational | Equality == != Relational
  * Logical_And  -> Equality | Logical_And && Equality
  * Logical_Or   -> Logical_And | Logical_Or || Logical_And
- * Assignment   -> Logical_Or | Identifier = Expression
+ * Conditional  -> Logical_Or | Logical_Or ? Expresion : Conditional
+ * Assignment   -> Conditional | Identifier = Expression
  */
```

修改对应的结构

```diff
pub enum Statement {
     Return(Expression),
     Declaration(Declaration),
     Expression(Option<Expression>),
+    Condition(Expression, Box<Statement>, Option<Box<Statement>>), // 由 parse 函数确保不会有 Declaration 作为子句
 }

@@ -59,8 +62,9 @@ pub enum Expression {
     // Multiplicative(Box<Expression>, Operator, Box<Expression>)
     // Relational(Box<Expression>, Operator, Box<Expression>)
     // Equality(Box<Expression>, Operator, Box<Expression>)
-    Assignment(String, Box<Expression>), // var = ();
-    Variable(String),                    // var
+    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // condition ? expression : ternary
+    Assignment(String, Box<Expression>),                        // var = ()
+    Variable(String),                                           // var
 }
```

这里给问号表达式起名为三元运算，为了和之前的一元二元统一

加入对问号表达式和ifelse的解析函数

```rust
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
/* ===== */
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
```

#### `ir`

添加ir的生成代码中添加label和跳转的部分

```rust
Statement::Condition(cond, t_stmt, f_stmt) => {
    let lable_id = new_label(&scope.label_cnt);
    scope
        .statements
        .push(IRStatement::Comment(format!("If-Else Label: {}", lable_id)));
    ir_expr(scope, cond);
    match f_stmt {
        None => {
            scope
                .statements
                .push(IRStatement::Beqz(format!("IfElse_End_{}", lable_id)));
            ir_stmt(scope, t_stmt);
            scope
                .statements
                .push(IRStatement::Label(format!("IfElse_End_{}", lable_id)));
        }
        Some(s) => {
            scope
                .statements
                .push(IRStatement::Beqz(format!("Else_{}", lable_id)));
            ir_stmt(scope, t_stmt);
            let mut stmts = vec![
                IRStatement::Br(format!("IfElse_End_{}", lable_id)),
                IRStatement::Label(format!("Else_{}", lable_id)),
            ];
            scope.statements.append(&mut stmts);
            ir_stmt(scope, s);
            scope
                .statements
                .push(IRStatement::Label(format!("IfElse_End_{}", lable_id)))
        }
    }
}
/* ===== */
Expression::Ternary(cond, t_expr, f_expr) => {
    let lable_id = new_label(&scope.label_cnt);
    scope
        .statements
        .push(IRStatement::Comment(format!("Ternary Label: {}", lable_id)));
    ir_expr(scope, cond);
    scope
        .statements
        .push(IRStatement::Beqz(format!("Ternary_False_{}", lable_id)));
    ir_expr(scope, t_expr);
    let mut stmts = vec![
        IRStatement::Br(format!("Ternary_End_{}", lable_id)),
        IRStatement::Label(format!("Ternary_False_{}", lable_id)),
    ];
    scope.statements.append(&mut stmts);
    ir_expr(scope, f_expr);
    scope
        .statements
        .push(IRStatement::Label(format!("Ternary_End_{}", lable_id)))
}
```

两个部分高度类似

#### `asm`

我添加了一个 `Comment` 中间码，用于在汇编中输出一些注释信息

```rust
writeln!(w, "# {:?}", s)?;
match s {
    IRStatement::Comment(_) => {
        // Use {:?} write comment already
    }
```

分支跳转直接翻译即可

```rust
IRStatement::Label(l) => {
    writeln!(w, "{}:", l)?;
}
IRStatement::Br(l) => {
    writeln!(w, "    j     {}", l)?;
}
IRStatement::Beqz(l) | IRStatement::Bnez(l) => {
    let op = match s {
        IRStatement::Beqz(_) => "beqz",
        IRStatement::Bnez(_) => "bnez",
        _ => panic!("Expecting branch operation"),
    };
    writeln!(w, "    lw    t1, 0(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    {}  t1, {}", op, l)?;
}
```

## 思考题

必须使用大括号，方便分析作用域和解析代码进行编译。由于 `rust` 并没有垃圾回收的机制，需要编译器分析变量何时释放，大括号可以极大的简化分析。

可能由于if后接的判断部分必须是右值，不需要再特别的用小括号包裹突出，也不需要特殊的解析。