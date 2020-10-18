# Step 4

> 贺鲲鹏 无 85 2018011169

## 实验内容

### 目标

增加逻辑比较 `> < >= <= == != && ||`

### 实现

#### `lexer`

与 step 2 和 step 3 相似，增加相应的 `Token`，并增加 `nom` 匹配的语句。

截止目前的匹配函数：

```rust
named!(
    pub lex_operators<&str, Token>,
    alt!(
        tag!("+")   => { |_| Token::Operator(Operator::Plus)}       |
        tag!("-")   => { |_| Token::Operator(Operator::Minus)}      |
        tag!("*")   => { |_| Token::Operator(Operator::Asterisk)}   |
        tag!("/")   => { |_| Token::Operator(Operator::Slash)}      |
        tag!("\\")  => { |_| Token::Operator(Operator::BackSlash)}  |
        tag!("&&")  => { |_| Token::Operator(Operator::And)}        |
        tag!("&")   => { |_| Token::Operator(Operator::BitwiseAnd)} |
        tag!("||")  => { |_| Token::Operator(Operator::Or)}         |
        tag!("|")   => { |_| Token::Operator(Operator::BitwiseOr)}  |
        tag!("!=")  => { |_| Token::Operator(Operator::NEQ)}        |
        tag!("!")   => { |_| Token::Operator(Operator::Not)}        |
        tag!("~")   => { |_| Token::Operator(Operator::BitwiseNot)} |
        tag!("%")   => { |_| Token::Operator(Operator::Percent)}    |
        tag!("==")  => { |_| Token::Operator(Operator::EQ)}         |
        tag!("<=")  => { |_| Token::Operator(Operator::LE)}         |
        tag!("<")   => { |_| Token::Operator(Operator::LT)}         |
        tag!(">=")  => { |_| Token::Operator(Operator::GE)}         |
        tag!(">")   => { |_| Token::Operator(Operator::GT)}
    )
);
```

这里必须注意符号匹配的尝试顺序，否则不能正确区分 `>=` 和 `>`。

#### `parser`

生成式改为：

```diff
- Expression   ->  | Additive
+ Expression   ->  | Logical_Or
  Factor       ->  | Integer(i32)
                   | ~ ! - Factor
                   | ( Expression )
  Multiplicative -> Factor | Multiplicative * / % Factor
  Additive     -> Multiplicative | Additive + - Multiplicative
+ Relational   -> Additive | Relational < > <= >= Additive
+ Equality     -> Relational | Equality == != Relational
+ Logical_And  -> Equality | Logical_And && Equality
+ Logical_Or   -> Logical_And | Logical_Or || Logical_And
```

与 step3 高度一致，继续增加解析函数：

```rust
pub fn parse_relational(tokens: &mut Peekable<Iter<Token>>) -> Expression {}

pub fn parse_equality(tokens: &mut Peekable<Iter<Token>>) -> Expression {}

pub fn parse_logical_and(tokens: &mut Peekable<Iter<Token>>) -> Expression {}

pub fn parse_logical_or(tokens: &mut Peekable<Iter<Token>>) -> Expression {}
```

这四个函数的结构与解析加减乘除高度一致。

对于 `1 != 2 && 1 == 1 || 1` ，即`[ Token::Integer(1), Token::Operator(Operator::NEQ), Token::Integer(2), Token::Operator(Operator::And), Token::Integer(1), Token::Operator(Operator::EQ), Token::Integer(1), Token::Operator(Operator::Or), Token::Integer(1), ]`，解析得到：

```rust
Expression::Binary(
    Operator::Or,
    Box::new(Expression::Binary(
        Operator::And,
        Box::new(Expression::Binary(
            Operator::NEQ,
            Box::new(Expression::Const(1)),
            Box::new(Expression::Const(2))
        )),
        Box::new(Expression::Binary(
            Operator::EQ,
            Box::new(Expression::Const(1)),
            Box::new(Expression::Const(1))
        ))
    )),
    Box::new(Expression::Const(1))
)
```

#### `ir`

与 step 3 一致，增加对二元运算的相应匹配：

```diff
 Expression::Binary(op, left, right) => {
     ir_expr(ir_statements, left);
     ir_expr(ir_statements, right);
     match *op {
         Operator::Asterisk => ir_statements.push(IRStatement::Mul),
         Operator::Slash => ir_statements.push(IRStatement::Div),
         Operator::Percent => ir_statements.push(IRStatement::Rem),
         Operator::Plus => ir_statements.push(IRStatement::Add),
         Operator::Minus => ir_statements.push(IRStatement::Sub),
+        Operator::LT => ir_statements.push(IRStatement::LT),
+        Operator::GT => ir_statements.push(IRStatement::GT),
+        Operator::LE => ir_statements.push(IRStatement::LE),
+        Operator::GE => ir_statements.push(IRStatement::GE),
+        Operator::EQ => ir_statements.push(IRStatement::EQ),
+        Operator::NEQ => ir_statements.push(IRStatement::NEQ),
+        Operator::And => ir_statements.push(IRStatement::LogicalAnd),
+        Operator::Or => ir_statements.push(IRStatement::LogicalOr),
         _ => panic!("Expecting binary operators"),
     }
 }
```

#### `asm`

不同表达式的汇编结构差异略大，通过使用工具链进行编译，整理得到翻译中间码的代码：

```rust
IRStatement::Add
| IRStatement::Sub
| IRStatement::Mul
| IRStatement::Div
| IRStatement::Rem
| IRStatement::GT
| IRStatement::LT => {
    let op = match s {
        IRStatement::Add => "add",
        IRStatement::Sub => "sub",
        IRStatement::Mul => "mul",
        IRStatement::Div => "div",
        IRStatement::Rem => "rem",
        IRStatement::GT => "sgt",
        IRStatement::LT => "slt",
        _ => panic!("Expecting binary operator"),
    };
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    {}   t1, t1, t2", op)?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
IRStatement::GE | IRStatement::LE => {
    let op = match s {
        IRStatement::GE => "slt",
        IRStatement::LE => "sgt",
        _ => panic!("Expecting binary operator"),
    };
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    {}   t1, t1, t2", op)?;
    writeln!(w, "    xori  t1, t1, 1")?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
IRStatement::EQ | IRStatement::NEQ => {
    let op = match s {
        IRStatement::EQ => "seqz",
        IRStatement::NEQ => "snez",
        _ => panic!("Expecting binary operator"),
    };
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    sub   t1, t1, t2")?;
    writeln!(w, "    {}  t1, t1", op)?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
IRStatement::LogicalAnd => {
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    snez  t1, t1")?;
    writeln!(w, "    snez  t2, t2")?;
    writeln!(w, "    and   t1, t1, t2")?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
IRStatement::LogicalOr => {
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    or    t1, t1, t2")?;
    writeln!(w, "    snez  t1, t1")?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
```

简而言之，大于小于与加减乘除一致；大于等于和小于等于通过先判断小于大于，再取反得到；等于和不等于通过相减后使用等于零置位得到；逻辑与和逻辑或参照指导书进行实现，不支持短路。

#### 结果

测例中的一份代码：

```c
int main() {
    return (1 || 0) && 0;
}
```

得到结果

```mips
    .text
    .global main
main:
# Push(1)
    addi  sp, sp, -4
    li    t1, 1
    sw    t1, 0(sp)
# Push(0)
    addi  sp, sp, -4
    li    t1, 0
    sw    t1, 0(sp)
# LogicalOr
    lw    t2, 0(sp)
    lw    t1, 4(sp)
    addi  sp, sp, 4
    or    t1, t1, t2
    snez  t1, t1
    sw    t1, 0(sp)
# Push(0)
    addi  sp, sp, -4
    li    t1, 0
    sw    t1, 0(sp)
# LogicalAnd
    lw    t2, 0(sp)
    lw    t1, 4(sp)
    addi  sp, sp, 4
    snez  t1, t1
    snez  t2, t2
    and   t1, t1, t2
    sw    t1, 0(sp)
# Return
    lw    a0, 0(sp)
    addi  sp, sp, 4
    jr    ra
```

显然结果是对的。

## 思考题

### 1

问题本身显然答案是否定的，我们可以使用惰性求值的方法，将表达式完全展开后再进行求解。这样可能部分运算可以被化为其他运算的组合，从而部分地参与了运算。

对于我们使用的中间码和汇编，必须计算出左右子式的值，放在栈的最顶上，才能取出进行运算。

不过其中较为特殊的逻辑运算，理论上可以修改进而实现短路求值，从而不需要算出所有的操作数。这一功能一般是通过分支跳转实现的，我们并未实现，故还是需要计算出所有的操作数。

### 2

对于非常复杂的逻辑表达式，短路求值可以较快地得出结果，缩短了运算时间。同时这一特性与人类分析逻辑表达式的行为一致，便于分析程序的功能。
