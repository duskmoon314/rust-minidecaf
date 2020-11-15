# Step 8

> 贺鲲鹏 2018011169

## 实验内容

### 目标

- 加入循环语句

### 实现

#### `lexer`

加入对关键词 `if` `else` `for` `while` `do` 的匹配

#### `parser`

根据指南添加生成式

```diff
 * Statement    ->  | Return Expression Semicolon
 *                  | Expression? Semicolon
 *                  | If ( Expression ) Statement (Else statement)?
 *                  | Compound_Statement
+*                  | For ( Expression? Semicolon Expression? Semicolon Expression? Semicolon ) Statement
+*                  | For ( Declaration Expression? Semicolon Expression? Semicolon ) Statement
+*                  | While ( Expression ) Statement
+*                  | Do Statement While ( Expression ) Semicolon
+*                  | Break Semicolon
+*                  | Continue Semicolon
```

添加相应的枚举：

```rust
pub enum Statement {
    Return(Expression),
    Declaration(Declaration),
    Expression(Option<Expression>),
    Condition(Expression, Box<Statement>, Option<Box<Statement>>), // 由 parse 函数确保不会有 Declaration 作为子句
    Compound(Vec<Statement>),
    Break,
    Continue,
    Loop(
        Option<Box<Statement>>,
        Option<Expression>,
        Box<Statement>,
        Option<Expression>,
    ),
    // Loop(pre, cond, body, post)
}
```

编写需要的解析函数：

```rust
// Expression ; | <Empty> ; | <Empty> )
// Use in for-loop: for(;;){}
pub fn parse_expression_option(tokens: &mut PeekableNth<Iter<Token>>) -> Option<Expression> {
    match tokens.peek() {
        Some(Token::Symbol(Symbol::Semicolon)) | Some(Token::Symbol(Symbol::Rparen)) => None,
        _ => {
            let expr = parse_expression(tokens);
            match tokens.peek() {
                Some(Token::Symbol(Symbol::Semicolon)) | Some(Token::Symbol(Symbol::Rparen)) => {
                    Some(expr)
                }
                _ => panic!("Expression Option Error: Expecting ; ) after Option<Expression>"),
            }
        }
    }
}
```

```rust
Some(Token::Keyword(Keyword::For)) => {
    // for(pre; cond; post) body
    tokens.next(); // consume for
    match tokens.next() {
        Some(Token::Symbol(Symbol::Lparen)) => {}
        _ => panic!("For Error: Expecting ("),
    }
    let pre: Statement;
    match tokens.peek() {
        Some(Token::Type(_)) => pre = Statement::Declaration(parse_declaration(tokens)),
        _ => {
            let expr = parse_expression_option(tokens);
            tokens.next();
            pre = Statement::Expression(expr);
        }
    };
    match pre {
        Statement::Expression(_) | Statement::Declaration(_) => {}
        _ => panic!("For Error: Expecting expression/declaration as pre"),
    }
    let cond = parse_expression_option(tokens);
    if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
        panic!("For Error: Expecting ; after cond");
    }
    let post = parse_expression_option(tokens);
    if tokens.next() != Some(&Token::Symbol(Symbol::Rparen)) {
        panic!("For Error: Expecting ) after post, Found {:?}");
    }
    return Statement::Loop(
        Some(Box::new(pre)),
        cond,
        Box::new(parse_statement(tokens)),
        post,
    );
}
Some(Token::Keyword(Keyword::While)) => {
    // while(cond) body
    tokens.next(); // consume while
    if tokens.next() != Some(&Token::Symbol(Symbol::Lparen)) {
        panic!("While Error: Expecting (")
    }
    let cond = parse_expression(tokens);
    if tokens.next() != Some(&Token::Symbol(Symbol::Rparen)) {
        panic!("While Error: Expecting )")
    }
    return Statement::Loop(None, Some(cond), Box::new(parse_statement(tokens)), None);
}
Some(Token::Keyword(Keyword::Do)) => {
    // do body while(cond)
    tokens.next(); // consume do
    let body = parse_statement(tokens);
    if tokens.next() != Some(&Token::Keyword(Keyword::While)) {
        panic!("DoWhile Error: Expecting while after do")
    }
    if tokens.next() != Some(&Token::Symbol(Symbol::Lparen)) {
        panic!("DoWhile Error: Expecting (")
    }
    let cond = parse_expression(tokens);
    if tokens.next() != Some(&Token::Symbol(Symbol::Rparen)) {
        panic!("DoWhile Error: Expecting )")
    }
    if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
        panic!("DoWhile Error: Expecting ;")
    }
    return Statement::Loop(
        Some(Box::new(body.clone())),
        Some(cond),
        Box::new(body),
        None,
    );
}
Some(Token::Keyword(Keyword::Break)) => {
    tokens.next(); // consume break
    match tokens.next() {
        Some(Token::Symbol(Symbol::Semicolon)) => {
            return Statement::Break;
        }
        _ => panic!("Statement Err: Expecting ; at the end"),
    }
}
Some(Token::Keyword(Keyword::Continue)) => {
    tokens.next(); // consume continue
    match tokens.next() {
        Some(Token::Symbol(Symbol::Semicolon)) => {
            return Statement::Continue;
        }
        _ => panic!("Statement Err: Expecting ; at the end"),
    }
}
```

#### `ir`

将需要共享的“静态参数”存于一个结构体

```rust
struct FunctionStaticProps<'a> {
    name: &'a str,
    loop_label: Vec<(u32, u32)>,
    var_max: u32,
}
```

编辑循环体的中间代码生成

```rust
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
        ir_expr(cond, label_cnt, scope_stack, ir_statements);
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
        ir_expr(post, label_cnt, scope_stack, ir_statements);
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
```

#### `asm`

本节不需要更改汇编生成

## 思考题

第一种翻译较好。考虑只执行一次循环体，第二种会多进行一次条件判断。