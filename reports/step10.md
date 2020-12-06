# Step 10

## 实验内容

### 目标

- 支持全局变量

### 实现

#### `lexer`

无变动

#### `parser`

解析 `Program` 时加入对全局变量的解析，这里只允许使用非负常整数赋值的情况：

```rust
pub fn parse_program(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
    let mut functions: Vec<Function> = Vec::new();
    let mut global_vars: Vec<Declaration> = Vec::new();
    let mut have_main = false;
    while tokens.peek().is_some() && *tokens.peek().unwrap() != &Token::Symbol(Symbol::EOF) {
        match tokens.peek_nth(2) {
            Some(Token::Symbol(Symbol::Lparen)) => {
                functions.push(parse_function(tokens));
                if let Some(f) = functions.last() {
                    if f.name == "main" {
                        have_main = true;
                    }
                }
            }
            _ => {
                let decl = parse_declaration(tokens);
                match decl.expression {
                    Some(Expression::Const(_)) | None => {
                        global_vars.push(decl);
                    }
                    _ => panic!("Global Variable can only use integer to initialize"),
                }
            }
        }
    }
    if !have_main {
        panic!("Program Error: No Main Function");
    }
    Program {
        functions,
        global_vars,
    }
}
```

首先通过查看后续的第三个负号是否是左小括号，决定是函数还是全局变量的定义。对于全局变量，只允许整型常数和无定义的情况，又由 `lexer` 保证只会有非负常整数的情况。

#### `ir`

添加存储全局变量信息的符号表

```rust
#[derive(Debug, PartialEq)]
pub struct GlobalVarProps {
    pub init: Option<i32>,
    pub size: u32,
}

// GlobalVarMap (var_id, {init?, size})
type GlobalVarMap = HashMap<String, GlobalVarProps>;
```

更改符号查找的函数，在一般的作用域找不到的情况下查询全局变量，返回值中加入布尔变量表示是否是全局变量。由于对全局变量的调用只需要使用变量名，故只需要返回一个 `true` 即可。

```rust
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
```

生成 `ir` 时，首先根据变量类型和是否有初始化，按顺序更新全局变量表

```rust
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
```

由于现在查找变量时需要访问全局符号表，故将全局符号表作为参数传入 `ir_func()` 函数，在生成 `ir` 的最开始检查是否已有同名的全局变量

```rust
if global_vars.contains_key(&function.name) {
    panic!("Function Global Var Conflict: Same identifier {function.name}");
}
```

使用变量时，根据返回结果是否在全局变量中，略作不同处理即可

```rust
let (idx, is_global) = lookup(scope_stack, props.global_vars, id);
match is_global {
    true => ir_statements.push(IRStatement::GlobalAddr(id.to_owned())),
    false => ir_statements.push(IRStatement::FrameAddr(idx)),
}
```

#### `asm`

在翻译函数语句前先翻译全局变量，根据是否初始化做不同处理。这种实现使的函数内部总可以调用全局变量，无论定义于何处，这与 C 的处理是一致的。

```rust
for (key, value) in ir_program.global_vars.iter() {
    writeln!(w, "\n# Global Var: {:?}", key)?;
    match value.init {
        Some(x) => {
            writeln!(w, "    .data")?;
            writeln!(w, "    .global {}", key)?;
            writeln!(w, "    .align 4")?;
            writeln!(w, "    .size {}, {}", key, value.size)?;
            writeln!(w, "{}:", key)?;
            writeln!(w, "    .word {}", x)?;
        }
        None => {
            writeln!(w, "    .comm {}, {}, 4", key, value.size)?;
        }
    }
}
```

对于调用全局变量的处理很简单，采用指南中给的汇编即可

```rust
IRStatement::GlobalAddr(id) => {
    writeln!(w, "    addi  sp, sp, -4")?;
    writeln!(w, "    la    t1, {}", id)?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
```

## 思考题

```mips
la  t0, a
lw  t0, 0(t0)
```
