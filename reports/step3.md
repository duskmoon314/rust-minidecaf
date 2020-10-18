# Step 3

> 贺鲲鹏 无 85 2018011169

## 实验内容

### 目标

加入加减乘除模运算

### 实现

#### `lexer`

在 step 2 中已经添加了加减乘除，只需增加模，即百分号。

```diff
#[derive(Debug, PartialEq, Clone, Copy)]
 pub enum Operator {
     Plus,
     Minus,
     Asterisk,
     Slash,
     BackSlash,
     And,
     BitwiseAnd,
     Or,
     BitwiseOr,
     Not,
     BitwiseNot,
+    Percent,
 }
```

使用 `nom` 匹配与 step 2 没有太多区别。

#### `parser`

生成式修改为

```diff
- Expression   -> Integer(i32) | Unary(UnaryOp, Box<Expression>)
+ Expression   ->  | Additive
+ Factor       ->  | Integer(i32)
+                  | Unary(UnaryOp, Unary)
+                  | ( Expression )
+ Multiplicative -> Factor | Factor * / % Factor
+ Additive     -> Multiplicative | Multiplicative + - Multiplicative
```

但这种结构过于复杂，不适合代码实现。观察到这些都是二元运算，step4 中要增加的运算也是二元运算，以增加中间码生成时匹配的复杂度换此处的简单结构（二元运算的前缀表达式），修改如下：

```diff
#[derive(Debug, PartialEq)]
pub enum Expression {
    Const(i32),
    Unary(Operator, Box<Expression>),
+    Binary(Operator, Box<Expression>, Box<Expression>), // Binary Operations (+ a b)
+    // Additive(Box<Expression>, Operator, Box<Expression>),
+    // Multiplicative(Box<Expression>, Operator, Box<Expression>)
+    // Relational(Box<Expression>, Operator, Box<Expression>)
+    // Equality(Box<Expression>, Operator, Box<Expression>)
}
```

进行解析时，分级使用不同的函数解析进行即可。最小的单元为 `Factor`，即“因子”，为整数、一元运算、括号中的表达式。之后是结构基本一致的解析函数：

```rust
pub fn parse_multiplicative(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_factor(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(mul_op)) if mul_op.is_multiplicative() => {
                tokens.next();
                let r_factor = parse_factor(tokens);
                l_factor = Expression::Binary(*mul_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}

pub fn parse_additive(tokens: &mut Peekable<Iter<Token>>) -> Expression {
    let mut l_factor = parse_multiplicative(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(add_op)) if add_op.is_additive() => {
                tokens.next();
                let r_factor = parse_multiplicative(tokens);
                l_factor = Expression::Binary(*add_op, Box::new(l_factor), Box::new(r_factor))
            }
            _ => break,
        }
    }
    l_factor
}
```

这两个函数通过循环中每次调用更小的单元解析函数（换言之，优先级更高的单元），之后查看下一个符号是不是预期的符号，如果是则匹配右子式。之后将整体作为左子式再进入循环体，从而实现左结合性。

对表达式 `1 + -1 / 1`，其 `Token` 串为 `[ Token::Integer(1), Token::Operator(Operator::Plus), Token::Operator(Operator::Minus), Token::Integer(1), Token::Operator(Operator::Slash), Token::Symbol(Symbol::Lparen), Token::Integer(1), Token::Symbol(Symbol::Rparen), ]`，进行解析，得到下方结构：

```rust
Expression::Binary(
    Operator::Plus,
    Box::new(Expression::Const(1)),
    Box::new(Expression::Binary(
        Operator::Slash,
        Box::new(Expression::Unary(
            Operator::Minus,
            Box::new(Expression::Const(1))
        )),
        Box::new(Expression::Const(1))
    ))
)
```

#### `ir`

中间码增加加减乘除模对应的五个运算，作用为弹出栈顶的两个数，进行操作后结果压栈。故先解析左右子式，再压入五个运算操作：

```rust
Expression::Binary(op, left, right) => {
    ir_expr(ir_statements, left);
    ir_expr(ir_statements, right);
    match *op {
        Operator::Asterisk => ir_statements.push(IRStatement::Mul),
        Operator::Slash => ir_statements.push(IRStatement::Div),
        Operator::Percent => ir_statements.push(IRStatement::Rem),
        Operator::Plus => ir_statements.push(IRStatement::Add),
        Operator::Minus => ir_statements.push(IRStatement::Sub),
        _ => panic!("Expecting binary operators"),
    }
}
```

#### `asm`

通过汇编工具确定汇编指令后，与 step 2 类似地进行配对：

```rust
IRStatement::Add
| IRStatement::Sub
| IRStatement::Mul
| IRStatement::Div
| IRStatement::Rem => {
    let op = match s {
        IRStatement::Add => "add",
        IRStatement::Sub => "sub",
        IRStatement::Mul => "mul",
        IRStatement::Div => "div",
        IRStatement::Rem => "rem",
        _ => panic!("Expecting binary operator"),
    };
    writeln!(w, "    lw    t2, 0(sp)")?;
    writeln!(w, "    lw    t1, 4(sp)")?;
    writeln!(w, "    addi  sp, sp, 4")?;
    writeln!(w, "    {}   t1, t1, t2", op)?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
```

先取出左右子式的值，修改栈，运算，最后压栈。

## 思考题

### 1

> 请给出将寄存器 t0 中的数值压入栈中所需的 riscv 汇编指令序列；请给出将栈顶的数值弹出到寄存器 t0 中所需的 riscv 汇编指令序列。

```mips
# 压栈
addi  sp, sp, -4
sw    t0, 0(sp)
```

```mips
# 弹栈
lw    t0, 0(sp)
addi  sp, sp, 4
```

### 2

考虑右侧为等于 `false` 的逻辑表达式，或者整型除法（小除以大，得 0），实质等价于除以零。

```c
#include <stdio.h>

int main()
{
    int a = 1;
    int b = 1 == 0;
    // int b = 1 / __INT_MAX__;
    printf("%d\n", a / b);
    return 0;
}
```

在 x86-64 的 wsl2 中进行测试，结果如下，与 `1 / 0` 的结果一致

```bash
$ gcc test.c -o b.out
$ ./b.out
Floating point exception
$ riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 test.c
$ qemu-riscv32 a.out
-1
```
