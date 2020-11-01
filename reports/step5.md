# Step 5

> 贺鲲鹏 2018011169

## 实验内容

### 目标

增加变量的声明和赋值

### 实现

#### `lexer`

增加一个赋值符号（等号）的解析

```diff
 #[derive(Debug, PartialEq, Clone, Copy)]
 pub enum Operator {
...
+    Assign,
 }
```

#### `parser`

参照指南，修改生成式为

```diff
 /*
  * Program      -> Function
- * Function     -> Type Identifier Lparen Rparen Lbrace Statement Rbrace
+ * Function     -> Type Identifier Lparen Rparen Lbrace Statement* Rbrace
  * Type         -> Type(Int | Double)
- * Statement    -> Return Expression Semicolon
- * Expression   ->  | Logical_Or
+ * Statement    ->  | Return Expression Semicolon
+ *                  | Expression? Semicolon
+ *                  | Declaration
+ * Declaration  -> Type Identifier (= Expression)? Semicolon
+ * Expression   ->  | Assignment
  * Factor       ->  | Integer(i32)
  *                  | ~ ! - Factor
  *                  | ( Expression )
+ *                  | Identifier
  * Multiplicative -> Factor | Multiplicative * / % Factor
  * Additive     -> Multiplicative | Additive + - Multiplicative
  * Relational   -> Additive | Relational < > <= >= Additive
  * Equality     -> Relational | Equality == != Relational
  * Logical_And  -> Equality | Logical_And && Equality
  * Logical_Or   -> Logical_And | Logical_Or || Logical_And
+ * Assignment   -> Logical_Or | Identifier = Expression
  */
```

相应的修改结构体

```diff
 pub struct Function {
     pub t: Type,
     pub name: String,
-    pub statement: Statement,
+    pub statements: Vec<Statement>,
 }

 #[derive(Debug, PartialEq)]
 pub enum Statement {
     Return(Expression),
+    Declaration(Declaration),
+    Expression(Option<Expression>),
+}
+
+#[derive(Debug, PartialEq)]
+pub struct Declaration {
+    pub t: Type,
+    pub name: String,
+    pub expression: Option<Expression>,
 }

 pub enum Expression {
     Const(i32),
     Unary(Operator, Box<Expression>),
     Binary(Operator, Box<Expression>, Box<Expression>), // Binary Operations (+ a b)
+    Assignment(String, Box<Expression>), // var = ();
+    Variable(String),                    // var
 }
```

并添加解析赋值和声明变量的函数

```diff
+// Assignment => identifier = ? parse_assignment : parse_logical_or
+pub fn parse_assignment(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
+    match tokens.peek() {
+        Some(Token::Identifier(name)) => match tokens.peek_nth(1) {
+            Some(Token::Operator(Operator::Assign)) => {
+                tokens.next();
+                tokens.next();
+                let expr = parse_expression(tokens);
+                Expression::Assignment(name.to_owned(), Box::new(expr))
+            }
+            _ => parse_logical_or(tokens),
+        },
+        _ => parse_logical_or(tokens),
+    }
+}

+pub fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
+    parse_assignment(tokens)
+}
+
+pub fn parse_declaration(tokens: &mut PeekableNth<Iter<Token>>) -> Declaration {
     match tokens.next() {
+        Some(Token::Type(t)) => match tokens.next() {
+            Some(Token::Identifier(name)) => match tokens.next() {
+                Some(Token::Symbol(Symbol::Semicolon)) => Declaration {
+                    t: *t,
+                    name: name.to_owned(),
+                    expression: None,
+                },
+                Some(Token::Operator(Operator::Assign)) => {
+                    let expr = parse_expression(tokens);
+                    match tokens.next() {
+                        Some(Token::Symbol(Symbol::Semicolon)) => Declaration {
+                            t: *t,
+                            name: name.to_owned(),
+                            expression: Some(expr),
+                        },
+                        _ => panic!("Declaration Err: Expecting ; at the end"),
+                    }
+                }
+                _ => panic!("Declaration Err: Expecting ; or ="),
+            },
+            _ => panic!("Declaration Err: Expecting identifier name"),
+        },
+        _ => panic!("Declaration Err: Expecting type token"),
+    }
+}
+ 
+pub fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
+    match tokens.peek() {
         Some(Token::Keyword(Keyword::Return)) => {
+            tokens.next();
             let expr = parse_expression(tokens);
             match tokens.next() {
                 Some(Token::Symbol(Symbol::Semicolon)) => {
                     return Statement::Return(expr);
                 }
-                _ => panic!("Expecting ; at the end of Statement"),
+                _ => panic!("Statement Err: Expecting ; at the end"),
+            }
+        }
+        Some(Token::Type(_)) => {
+            let decl = parse_declaration(tokens);
+            return Statement::Declaration(decl);
+        }
+        Some(Token::Symbol(Symbol::Semicolon)) => {
+            tokens.next();
+            return Statement::Expression(None);
+        }
+        _ => {
+            let expr = parse_expression(tokens);
+            match tokens.next() {
+                Some(Token::Symbol(Symbol::Semicolon)) => {
+                    return Statement::Expression(Some(expr));
+                }
+                _ => panic!("Statement Err: Expecting ; at the end"),
             }
         }
-        _ => panic!("Unknown Statement"),
     }
 }
```

这里使用了一个新的库 `peek_nth` 来查看“下两个 `Token`”，以确定是变量+赋值符号，还是变量+其他运算符。

我选择在生成ast的阶段进行返回值的判断，如果是 `main` 函数且无返回值，会自动补一个 `return 0;`

```rust
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
```

#### `ir`

我选择使用哈希表来存储变量，下面这个设计结果可能存在很多问题，step 7的时候可能需要进行彻底的重构

```rust
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
```

我选择将函数绑定到作用域上，作用域的符号表可以通过一个指针去递归查找更上级的符号表。在生成中间码的时候，每次遇到声明就检查表中是否有该名字变量，没有则添加。

```rust
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
```

这里我选择给 `FrameAddr` 留两个参数，之后可能会用到第一个参数用于加载上级作用域的变量，也可能废弃此设计。

```
frameaddr i k: put ith frame kth element addr in stack, stack_pointer + 1
```

#### `asm`

汇编在函数首尾增加建立和销毁栈帧的部分

```diff
+ writeln!(w, "{}_prologue:", ir_function.name)?;
+ let frame_size: u32 = ir_function.scope.symbol_map.len() as u32 * 4 + 8;
+ writeln!(w, "    addi  sp, sp, -{}", frame_size)?;
+ writeln!(w, "    sw    ra, {}(sp)", frame_size - 4)?;
+ writeln!(w, "    sw    fp, {}(sp)", frame_size - 8)?;
+ writeln!(w, "    addi  fp, sp, {}", frame_size)?;
...
+ writeln!(w, "{}_epilogue:", ir_function.name)?;
+ writeln!(w, "    lw    fp, {}(sp)", frame_size - 8)?;
+ writeln!(w, "    lw    ra, {}(sp)", frame_size - 4)?;
+ writeln!(w, "    addi  sp, sp, {}", frame_size)?;
+ writeln!(w, "    ret")?;
```

对于变量寻址，一个不成熟的想法是利用栈帧中的 `fp` 寄存器，循环多次找到对应的上级作用域。这个设计在step 7中可能需要彻底重构

```rust
IRStatement::FrameAddr(iter, idx) => {
    let mut i = *iter;
    writeln!(w, "    mv    t1, fp")?;
    loop {
        if i == 0 {
            break;
        } else {
            // load old fp
            writeln!(w, "    lw    t1, -4(t1)")?;
            i = i - 1;
        }
    }
    writeln!(w, "    li    t2, {}", *idx)?;
    writeln!(w, "    slli  t2, t2, 2")?;
    writeln!(w, "    addi  t2, t2, 12")?;
    writeln!(w, "    sub   t1, t1, t2")?;
    writeln!(w, "    addi  sp, sp, -4")?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
```

## 思考题

### 1

栈帧构成分为：返回地址与保存的寄存器（sp、fp及之后函数参数对应的传参用寄存器），局部变量，运算栈

第一个部分最少需要返回地址和旧的fp（或sp），共两个字（64位）的空间。（考虑32位的汇编）

局部变量如果没有，可以不存在这部分。

运算栈在使用完后应该 `pop` 到不占有空间。

### 2

由于我们现在仅考虑32位整型数据，所以只需在定义变量的时候不检查是否已经存在即可。所有更改对应同一个地址，便可以实现这种多次定义的逻辑。

查找变量的时候，如果没有查到，跟现在一样报错即可。