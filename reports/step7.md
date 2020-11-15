# Step 7

> 贺鲲鹏 2018011169

## 实验内容

### 目标

- 实现作用域和块语句

### 实现

#### `lexer`

本节没有任何改动

#### `parser`

按照指南修改生成式：

```diff
-* Function     -> Type Identifier Lparen Rparen Lbrace Statement* Rbrace
+* Function     -> Type Identifier Lparen Rparen Compound_Statement
 * Type         -> Type(Int | Double)
 * BlockItem    -> Statement | Declaration
+* Compound_Statement -> Lbrace Statement* Rbrace
 * Statement    ->  | Return Expression Semicolon
 *                  | Expression? Semicolon
-*                  | If ( Expresion ) Statement (Else statement)?
+*                  | If ( Expression ) Statement (Else statement)?
+*                  | Compound_Statement
```

在 `Statement` 中加入新的枚举 `Compound(Vec<Statement>)`

```rust
pub enum Statement {
    Return(Expression),
    Declaration(Declaration),
    Expression(Option<Expression>),
    Condition(Expression, Box<Statement>, Option<Box<Statement>>), // 由 parse 函数确保不会有 Declaration 作为子句
    Compound(Vec<Statement>),
}
```

编辑相应的解析函数部分：

```rust
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
```

#### `ir`

根据指南对先前的错误设计进行了较大的改动：

- label_cnt 改为参数进行传递，取消使用 rust 的智能指针。
- 语句改回 `IRFunction` 中，`Scope` 中仅放符号表和一个索引偏置
- 使用一个 Vector 来存储符号表，查找符号时从后往前遍历查找
- 将上述 Vector 的可变引用作为参数传递
- `IRFunction` 中加入 `var_max` 字段，用于指定汇编生成时给局部变量预留的栈空间大小

```rust
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
```

#### `asm`

由于 IR 逻辑改变，可以将汇编生成改成简单的根据偏置取变量

## 思考题

### 1

只需让最外层的 `x` 取 0 即可。内层的 `x` 赋值不会影响最外层的 `x` 。

### 2

如果语言支持先使用后定义，则需要先进行名称解析，再进行中间代码生成。