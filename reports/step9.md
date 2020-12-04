# Step 9

> 贺鲲鹏 2018011169

## 实验内容

### 目标

- 加入对多函数的支持

### 实现

总体上采用 `cargo clippy` 对代码格式进行优化。

#### `lexer`

除代码格式调整外无更改。

#### `parser`

产生式有如下变化：

```diff
- Program      -> Function
- Function     -> Type Identifier Lparen Rparen Compound_Statement
+ Program      -> Function*
+ Function     -> Type Identifier Lparen Parameter_List Rparen (Compound_Statement | ;)
+ Parameter_List -> (Type Identifier (, Type Identifier)*)?
+ Expression_List -> (Expression (, Expression)*)?

  Factor       ->  | Integer(i32)
                   | ~ ! - Factor
                   | ( Expression )
                   | Identifier
+                  | Identifier ( Expression )
```

相应调整结构体：

```diff
  pub struct Program {
-     pub function: Function,
+     pub functions: Vec<Function>,
  }

  pub struct Function {
      pub t: Type,
      pub name: String,
+     pub parameters: Vec<Declaration>,
      pub statements: Vec<Statement>,
  }

  pub enum Expression {
      ...
+     Function(String, Vec<Expression>),
  }
```

这里考虑到函数定义时，参数 `int a` 与变量声明很像，故使用 `Declaration` 组成的向量进行存储。

加入函数参数的解析，循环尝试解析为参数：

```rust
fn parse_parameter(tokens: &mut PeekableNth<Iter<Token>>) -> Vec<Declaration> {
    let mut list: Vec<Declaration> = Vec::new();

    if tokens.peek() == Some(&&Token::Symbol(Symbol::Rparen)) {
        return list;
    }

    match tokens.next() {
        Some(Token::Type(t)) => match tokens.next() {
            Some(Token::Identifier(name)) => list.push(Declaration {
                t: *t,
                name: name.to_string(),
                expression: None,
            }),
            _ => panic!("Parameter Error: Expecting identifier"),
        },
        _ => panic!("Parameter Error: Expecting type"),
    }

    loop {
        if tokens.peek() == Some(&&Token::Symbol(Symbol::Rparen)) {
            break;
        } else {
            match tokens.next() {
                Some(Token::Symbol(Symbol::Comma)) => match tokens.next() {
                    Some(Token::Type(t)) => match tokens.next() {
                        Some(Token::Identifier(name)) => list.push(Declaration {
                            t: *t,
                            name: name.to_string(),
                            expression: None,
                        }),
                        _ => panic!("Parameter Error: Expecting identifier"),
                    },
                    _ => panic!("Parameter Error: Expecting type"),
                },
                _ => panic!("Parameter Error: Expecting , between two parameters"),
            }
        }
    }
    list
}
```

更改标识符在表达式中的解析，根据是否有括号解析为变量或函数调用：

```rust
Token::Identifier(name) => {
    match tokens.peek() {
        Some(Token::Symbol(Symbol::Lparen)) => {
            tokens.next(); // consume (
            let expr_list = parse_expression_list(tokens);
            if tokens.next() == Some(&Token::Symbol(Symbol::Rparen)) {
                Expression::Function(name.to_owned(), expr_list)
            } else {
                panic!("Expecting ) to close fun(expr)")
            }
        }
        _ => Expression::Variable(name.to_owned()),
    }
}
```

加入对函数调用时参数列表的解析：

```rust
pub fn parse_expression_list(tokens: &mut PeekableNth<Iter<Token>>) -> Vec<Expression> {
    let mut list: Vec<Expression> = Vec::new();

    if tokens.peek() == Some(&&Token::Symbol(Symbol::Rparen)) {
        return list;
    }

    list.push(parse_expression(tokens));

    loop {
        if tokens.peek() == Some(&&Token::Symbol(Symbol::Rparen)) {
            break;
        } else {
            match tokens.next() {
                Some(Token::Symbol(Symbol::Comma)) => list.push(parse_expression(tokens)),
                _ => panic!("Parameter Error: Expecting , between two parameters"),
            }
        }
    }
    list
}
```

修改函数的解析，支持对参数的解析，支持声明和定义两种。代码略。

修改程序的解析，支持多函数。所有函数解析完成后判断是否有 `main` 函数，没有则报错。

```rust
pub fn parse_program(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
    let mut functions: Vec<Function> = Vec::new();
    let mut have_main = false;
    while tokens.peek().is_some() && *tokens.peek().unwrap() != &Token::Symbol(Symbol::EOF) {
        functions.push(parse_function(tokens));
        if let Some(f) = functions.last() {
            if f.name == "main" {
                have_main = true;
            }
        }
    }
    if !have_main {
        panic!("Program Error: No Main Function");
    }
    Program { functions }
}
```

#### `ir`

加入新的语句 `Call(String, u32)`，用于传入调用函数名和参数数量。后者用于函数结束时弹栈消去传参时压栈的参数。

类似 `parser` 更改结构 `IRProgram` 支持多函数。更改 `IRFunction` 记录参数数量，用于后续判断变量是局部变量还是参数而采用不同的获取地址的汇编语句。

加入函数表，结构如下：

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionProps {
    parameters: Vec<Type>,
    return_type: Type,
    is_defined: bool,
    idx: u32,
}

// FunctionMap (func_name, FunctionProps)
type FunctionMap = HashMap<String, FunctionProps>;
```

`FunctionProps` 包含函数的参数类型、返回值类型、是否定义和在 `IRProgram.functions` 中的序号。

为方便后续代码编写，添加函数 `new_fun` 用于翻译函数时判断是否声明、定义，并在已声明未定义时更新函数表，在未声明时加入函数表，返回该函数的序号。如果已声明未定义而参数数量或类型不一样，或多个声明语句，或定义后的声明与定义，皆报错。

```rust
fn new_fun(map: &mut FunctionMap, function_cnt: &mut u32, fun: &Function) -> u32 {...}
```

在翻译完所有函数后，使用 `check_fun_defined` 检查是否所有函数都已有定义。这里要求所有的函数都必须要有定义，与 gcc 的未使用可以只声明不同。

```rust
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
```

更改所有的翻译函数，传参 `FunctionStaticProps`，其中包含函数表、标签栈等。

```rust
struct FunctionStaticProps<'a> {
    name: &'a str,
    function_map: &'a FunctionMap,
    loop_label: Vec<(u32, u32)>,
    var_max: u32,
}
```

修改程序的翻译函数，支持对多函数的翻译：

```rust
pub fn ir(program: &Program) -> IRProgram {
    let mut label_cnt: u32 = 0;
    let mut function_cnt: u32 = 0;
    let mut scope_stack: Vec<Scope> = Vec::new();
    let mut functions: Vec<IRFunction> = Vec::new();
    let mut function_map = FunctionMap::new();
    for f in &program.functions {
        let idx = new_fun(&mut function_map, &mut function_cnt, f);
        let func = ir_func(f, &mut label_cnt, &mut scope_stack, &function_map);
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
    }
}
```

在函数翻译时，首先将所有参数存入符号表，但不赋值。这一步用于占据符号表中相应的变量名，阻止重定义等问题：

```rust
for p in &function.parameters {
    // 只需插入，不需赋值
    insert(scope_stack, &p.name);
}
```

在表达式翻译中加入对函数调用的翻译：

```rust
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
```

#### `asm`

拆分生成汇编的函数，用于支持多函数

```rust
pub fn asm_program(ir_program: &IRProgram, w: &mut impl Write) -> Result<()> {
    writeln!(w, "    .text")?;
    writeln!(w, "    j     main")?;
    for func in &ir_program.functions {
        asm_function(func, w)?;
    }
    Ok(())
}

#[allow(unreachable_patterns)]
pub fn asm_function(ir_function: &IRFunction, w: &mut impl Write) -> Result<()> {...}
```

这里使用 `j main` 来保证汇编从 `main` 开始运行。实际 gcc 生成时会将 `main` 提到最前面，这里出于简单起见未实现类似功能。

在取变量时，根据是否时参数采用不同的取址方法。由于参数和局部变量中有返回地址和栈指针的存储，两部分取址略有不同。

```rust
IRStatement::FrameAddr(idx) => {
    writeln!(w, "    li    t2, {}", idx)?;
    writeln!(w, "    slli  t2, t2, 2")?;
    match idx.cmp(&ir_function.param_cnt) {
        Ordering::Less => {
            writeln!(w, "    add   t1, fp, t2")?;
            writeln!(w, "    addi  sp, sp, -4")?;
            writeln!(w, "    sw    t1, 0(sp)")?;
        }
        _ => {
            writeln!(w, "    addi  t2, t2, 12")?;
            writeln!(w, "    sub   t1, fp, t2")?;
            writeln!(w, "    addi  sp, sp, -4")?;
            writeln!(w, "    sw    t1, 0(sp)")?;
        }
    }
}
```

在调用函数最后，根据参数数量进行弹栈

```rust
IRStatement::Call(func_name, params) => {
    writeln!(w, "    jal   {}", func_name)?;
    for _i in 1..*params {
        writeln!(w, "    addi  sp, sp, 4")?;
    }
    if *params == 0 {
        writeln!(w, "    addi  sp, sp, -4")?;
    }
    writeln!(w, "    sw    a0, 0(sp)")?;
}
```

## 思考题

目前 minidecaf 中只有赋值表达式会更改变量值，并会根据参数求值顺序影响函数调用结果。

```c
int a = 0;
foo(a = 1, a + 1);
```

如果从左往右求值，调用的是`foo(1, 2)`；如果从右往左求值，调用的是`foo(1, 1)`。

实验中实现的是从右往左求值并压栈传参。