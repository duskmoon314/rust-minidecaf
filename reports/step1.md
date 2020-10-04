# Step 1

> 2018011169 无 85 贺鲲鹏

## 实验内容

### 目标

使用 `Rust` 实现基本的 `lexer`、`parser`，并生成中间码和 `RISCV` 汇编代码

本节需要实现解析如下代码：

```c
int main()
{
    return 0;
}

```

并生成汇编：

```mips
    .text
    .global main
main:
    addi  sp, sp, -4
    li    t1, 0
    sw    t1, 0(sp)
    lw    a0, 0(sp)
    addi  sp, sp, 4
    jr    ra
```

### 实现

我选择使用 `Rust` 语言，结合库 `nom` 来实现 `lexer`。使用 `Rust` 的`match`实现 `parser`，进而生成中间码和汇编。目前仅基于匹配实现了非常简单的 `lexer` 和 `parser`，后续可能考虑基于一些高级方法进行重构。

同时我使用库 `clap` 实现一个更完善的命令行程序，在兼容原有的使用 `minidecaf <sourcecode>`将汇编打印到屏幕的同时，可以添加标志 `-lpi` 将 `lexer`、`parser` 和中间码输出到 `--log-file <log-file>` 中。这里还使用了库 `chrono` 获取使用程序的时间，一并输出到文件中。

目前整个项目的代码结果如下：

```
src
├── asm
│   └── mod.rs
├── ir
│   └── mod.rs
├── lexer
│   ├── identifiers.rs
│   ├── keywords.rs
│   ├── mod.rs
│   ├── number.rs
│   ├── symbols.rs
│   ├── token.rs
│   └── types.rs
├── lib.rs
├── main.rs
└── parser
    ├── ast.rs
    └── mod.rs
```

下面分模块介绍具体的设计。

#### `lexer`

`lexer` 的功能为解析输入的字符串，返回一个由各种类型的 `Token` 组成的向量。

`Token` 构成如下：

```
Token |
      | Symbol      => ( ) [ ] { } ; , EOF
      | Keyword     => Return For If Else
      | Type        => Int Double
      | Identifier  => String
      | Integer     => i32
```

这里额外添加了一些 step 1 本身不需要用到的部分，仅用于解析字符串的测试。在 `parser` 中并未实现相应功能，`lexer` 中也缺少对其中一部分的解析。

`lexer` 中主要使用 `nom` 的 `tag` 函数（及 `tag!` 宏），对关键字进行匹配。如对符号进行匹配的 `lex_symbols` 函数的定义：

```rust
named!(
    pub lex_symbols<&str, Token>,
    alt!(
        tag!("(") => { |_| Token::Symbol(Symbol::Lparen)} |
        tag!(")") => { |_| Token::Symbol(Symbol::Rparen)} |
        tag!("{") => { |_| Token::Symbol(Symbol::Lbrace)} |
        tag!("}") => { |_| Token::Symbol(Symbol::Rbrace)} |
        tag!("[") => { |_| Token::Symbol(Symbol::Lbrack)} |
        tag!("]") => { |_| Token::Symbol(Symbol::Rbrack)} |
        tag!(";") => { |_| Token::Symbol(Symbol::Semicolon)} |
        tag!(",") => { |_| Token::Symbol(Symbol::Comma)}
    )
);
```

而对于标识符，则使用库 `regex` 和 `nom` 配合进行解析：

```rust
pub fn lex_identifiers(input: &str) -> IResult<&str, Token> {
    let re = regex::Regex::new(r"^[_A-Za-z]{1}\w*").unwrap();
    if let Some(m) = re.find(input) {
        Ok((
            &input[m.end()..],
            Token::Identifier(input[m.start()..m.end()].to_string()),
        ))
    } else {
        Err(Err::Error(error_position!(
            input,
            nom::error::ErrorKind::RegexpFind
        )))
    }
}
```

对一串字符进行整体解析使用如下两个函数完成：

```rust
pub fn lex_all(input: &str) -> IResult<&str, token::Token> {
    let (input, _) = multispace0(input)?;
    if input.len() == 0 {
        Ok(("", token::Token::Symbol(token::Symbol::EOF)))
    } else {
        let (reset, tk) = alt((
            symbols::lex_symbols,
            keywords::lex_keywords,
            types::lex_types,
            identifiers::lex_identifiers,
            number::lex_integers,
        ))(input)?;
        Ok((reset, tk))
    }
}

pub fn lexer(input: &str) -> Vec<token::Token> {
    let mut tokens = Vec::<token::Token>::new();
    let mut reset = input;
    while reset.len() > 0 {
        let res = lex_all(reset);
        let result = match res {
            Ok(result) => result,
            Err(error) => panic!("Lex Failed: {:?}", error),
        };
        tokens.push(result.1);
        reset = result.0;
    }
    return tokens;
}
```

思路为先匹配若干个空格、制表符、换行符，如果后面没有字符则以 `Symbol(EOF)` 终结解析；否则进入各类 `Token` 的匹配环节，按照符号、关键词、类型、标识符、数字的顺序进行匹配。匹配到的 `Token` 放入向量中，并进行下一次匹配。

最终对本节编译的源代码解析结果为：

```
[Type(Int), Identifier("main"), Symbol(Lparen), Symbol(Rparen), Symbol(Lbrace), Keyword(Return), Integer(0), Symbol(Semicolon), Symbol(Rbrace), Symbol(EOF)]
```

目前的实现一次性将整个代码进行解析，对于较大的代码可能出现存储空间不足的问题，后续可能需要改写为类似 `generator` 的形式。

#### `parser`

`parser` 的目标是将 `lexer` 生成的 `Token` 向量转化为一个“抽象语法树”。目前我选择定义一系列的结构体和枚举，使用它们来表示“抽象语法树”。

对于 step 1，“抽象语法树”为：

```
Program
{
    functon: <Function>
    {
        t:          <Type>
            => Int | Double
        name:       <String>
        statement:  <Statement>
            => Return
                (
                    Expression
                        => Const(i32)
                )
    }
}
```

即为对下面这个文法的抽象：

```
Program      -> Function
Function     -> Type Identifier Lparen Rparen Lbrace Statement Rbrace
Type         -> Type(Int | Double)
Statement    -> Return Expression Semicolon
Expression   -> Integer(i32)
```

然后使用 `match` 对所有的 `Token` 进行从左到右的判断，解析为语法树的各个部分。根据生成式的需要，定义了 `parse_program`、`parse_functon`、`parse_statement`、`parse_expression`四个函数。

目前 `Program` 结构中只有一个函数，且函数名不为“main”便会报错。之后引入多函数支持时应考虑换成 `Function` 组成的向量，在解析函数时判断是否出现过函数名为“main”。

对本节编译代码经 `lexer` 处理后的 `Token` 向量串处理得到：

```
Program { function: Function { t: Int, name: "main", statement: Return(Const(0)) } }
```

#### 中间码生成

> 由于看完指导书后并不理解应该如何简便地表示和存储中间码，并使用中间码生成汇编，这两部分较多的参考了参考实现的 `commit: step1`

中间码也是一个特殊的“语法树”，且结构为：

```
IRProgram
{
    function: <IRFunction>
    {
        name:       <String>
        statement:  Vec<IRStatement>
    }
}

IRStatement => Push(i32) | Return
```

在这种表示中，中间码并不显式地表示“栈”的变化。“栈”的大小也被抽象化，对其具体的考虑放在汇编生成部分。

对于 step 1，只需要 `Push(i32)` 和 `Return` 两个中间码，只需要对 `parser` 生成的 AST 进行分析后判断选择使用哪个中间码便可。以下参考参考实现，使用两个函数来对 AST 进行翻译：

```rust
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
```

这两个函数实现的是遇到 `Return(Const(i32))`，先由内部的 `Const(i32)` 生成 `Push(i32)`，再由外部的 `Return()` 生成一个 `Return`。从而实现了返回一个值的功能。

对本节编译代码经 `parser` 得到的“抽象语法树”进行处理得到：

```
IRProgram { function: IRFunction { name: "main", statement: [Push(0), Return] } }
```

#### 汇编代码生成

> 汇编代码主要参考了参考实现的向屏幕输出的方式。

由于中间码是栈式机，很容易对应到汇编代码。这里也是一个比较简单的 `match` 便可以处理的部分。

```rust
pub fn write_asm(ir_program: &IRProgram, w: &mut impl Write) -> Result<()> {
    let ir_function = &ir_program.function;
    writeln!(w, "    .text")?;
    writeln!(w, "    .global {}", ir_function.name)?;
    writeln!(w, "{}:", ir_function.name)?;
    for s in &ir_function.statement {
        writeln!(w, "# {:?}", s)?;
        match s {
            IRStatement::Push(int32) => {
                writeln!(w, "    addi  sp, sp, -4")?;
                writeln!(w, "    li    t1, {}", int32)?;
                writeln!(w, "    sw    t1, 0(sp)")?;
            }
            IRStatement::Return => {
                writeln!(w, "    lw    a0, 0(sp)")?;
                writeln!(w, "    addi  sp, sp, 4")?;
                writeln!(w, "    jr    ra")?;
            }
        }
    }
    Ok(())
}
```

故最终得到汇编为：

```mips
    .text
    .global main
main:
# Push(0)
    addi  sp, sp, -4
    li    t1, 0
    sw    t1, 0(sp)
# Return
    lw    a0, 0(sp)
    addi  sp, sp, 4
    jr    ra
```

但实际编译这样一个函数不需要使用这么多寄存器和栈空间。实际使用 gcc，`O2`优化编译得到的结果（已略去不相关信息）：

```mips
    .text
    .global main
main:
    li    a0,0
    ret
```

如何基于中间码进行优化还有待思考。

## 思考题

### 1

修改 minilexer 的输入（`lexer.setInput` 的参数），使得 lex 报错，给出一个简短的例子。

只需要添加一个正则表达式不匹配的字符即可，比如在 `int` 前添加一个小于号，便可以得到报错信息：

```
    """\
    <int main() {
        return 123;
    }
    """

token kind   text
-----------  -------------------
Traceback (most recent call last):
  File "step1/minilexer.py", line 94, in <module>
    dumpLexerTokens(default())
  File "step1/minilexer.py", line 89, in dumpLexerTokens
    for tok in lexer.lex():
  File "step1/minilexer.py", line 45, in lex
    raise Exception(f"lex error at input position {self.pos}")
Exception: lex error at input position 4
```

### 2

修改 minilexer 的输入，使得 lex 不报错但 parse 报错，给出一个简短的例子。

只需要让 lexer 给出错误的 token 即可，如将 `int` 改为 `intt`，就会匹配为 `identifier`。

```
...intt main()...

Traceback (most recent call last):
  File "miniparser.py", line 70, in <module>
    print(default().parse("program"))
  File "miniparser.py", line 44, in parse
    children.append(self.parse(child))
  File "miniparser.py", line 44, in parse
    children.append(self.parse(child))
  File "miniparser.py", line 41, in parse
    raise Exception(f"syntax error, {child} expected but {tok.kind.name} found")
Exception: syntax error, Int expected but Identifier found
```

### 3

在 riscv 中，哪个寄存器是用来存储函数返回值的？

查阅 RISC-V Assembly Programmer’s
Handbook，约定使用 a0 和 a1 寄存器来存储函数返回值。
