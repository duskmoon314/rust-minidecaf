# Step 2

> 贺鲲鹏 无 85 2018011169

## 实验内容

### 目标

增加一元运算：`-`、`~`、`!`，可以解析形如`-!~!`的多个一元运算逐个作用的表达式。

### 实现

#### `lexer`

首先在 `token.rs` 中定义“运算符”枚举（顺便加入了一些常用的运算符，但本节未实现相关功能）：

```rust
/*
 * Operator
 * + Plus
 * - Minus
 * * Asterisk
 * / Slash
 * \ BackSlash
 * && And
 * & BitwiseAnd
 * || Or
 * | BitwiseOr
 * ! Not
 * ~ BitwiseNot
 */

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
}
```

为了后面解析时可以判断是否是一元运算符，实现 `is_unary` 方法：

```rust
impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Minus | Operator::Not | Operator::BitwiseNot => true,
            _ => false,
        }
    }
}
```

为了之后修改方便，运算符的匹配定义在 `operators.rs` 中。与 step 1 中符号匹配类似，使用 `nom` 实现一个简单的匹配：

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
        tag!("!")   => { |_| Token::Operator(Operator::Not)}        |
        tag!("~")   => { |_| Token::Operator(Operator::BitwiseNot)}
    )
);
```

#### `parser`

考虑修改生成式为：

```
Expression   -> Integer(i32) | Unary(UnaryOp, Box<Expression>)
```

即表达式可以是一个整数，或者一个一元运算符加一个表达式的指针。后者可以实现嵌套的操作，即解析 `-!~123` 这样的表达式。

故修改 `parse_expression` 函数如下：

```diff
 pub fn parse_expression(tokens: &mut Peekable<Iter<Token>>) -> Expression {
     match tokens.next() {
         Some(Token::Integer(int)) => return Expression::Const(*int),
+        Some(Token::Operator(unary_op)) if unary_op.is_unary() => {
+            let expr = parse_expression(tokens);
+            return Expression::Unary(*unary_op, Box::new(expr));
+        }
         _ => panic!("Now can only parse an integer"),
     }
 }
```

假设 `lexer` 得到的 Token 向量为：`vec![Token::Operator(Operator::Minus), Token::Integer(10)];`，使用此函数解析得到表达式 `Expression::Unary(Operator::Minus, Box::new(Expression::Const(10)))`。

#### `ir`

引入三个新的中间码

```diff
-#[derive(Debug)]
+#[derive(Debug, PartialEq)]
 pub enum IRStatement {
     Push(i32),
+    Neg,
+    Not,
+    LogicalNot,
     Return,
 }
```

根据语法树进行匹配，转换成中间码

```diff
match expr {
+       Expression::Unary(unary_op, left) => {
+           ir_expr(ir_statements, left);
+           match *unary_op {
+               Operator::Minus => ir_statements.push(IRStatement::Neg),
+               Operator::Not => ir_statements.push(IRStatement::LogicalNot),
+               Operator::BitwiseNot => ir_statements.push(IRStatement::Not),
+               _ => panic!("Expecting unary operators"),
+           };
+       }
    }
```

若语法树为

```rust
Expression::Unary(
    Operator::Minus,
    Box::new(Expression::Unary(
        Operator::Not,
        Box::new(Expression::Unary(
            Operator::BitwiseNot,
            Box::new(Expression::Const(1)),
        )),
    )),
);
```

得到中间码为

```rust
[
    IRStatement::Push(1),
    IRStatement::Not,
    IRStatement::LogicalNot,
    IRStatement::Neg
]
```

#### `asm`

这三个一元运算都是改变栈顶，汇编逻辑为取出栈顶的数、进行运算、存回栈顶。使用 gcc 确认对应汇编指令后，实现代码如下：

```rust
IRStatement::Neg | IRStatement::Not | IRStatement::LogicalNot => {
    let op = match s {
        IRStatement::Neg => "neg",
        IRStatement::Not => "not",
        IRStatement::LogicalNot => "seqz",
        _ => panic!("Can't be"),
    };
    writeln!(w, "    lw    t1, 0(sp)")?;
    writeln!(w, "    {}   t1, t1", op)?;
    writeln!(w, "    sw    t1, 0(sp)")?;
}
```

#### 结果

以测例中一段代码为例：

```c
int main() {
    return -~0;
}
```

```mips
    .text
    .global main
main:
# Push(0)
    addi  sp, sp, -4
    li    t1, 0
    sw    t1, 0(sp)
# Not
    lw    t1, 0(sp)
    not   t1, t1
    sw    t1, 0(sp)
# Neg
    lw    t1, 0(sp)
    neg   t1, t1
    sw    t1, 0(sp)
# Return
    lw    a0, 0(sp)
    addi  sp, sp, 4
    jr    ra
```

## 思考题

要想实现越界，必须引入加减。三个一元运算中，只有 `-` 是“按位取反加一”引入了加减。又只有 0 按位取反能用 1 填满所有位，再加一实现越界。故 `-!p` （p 为正整型）即可实现越界。不过由于整型截断低位，而低位此时为 0，最终结果不会出错。
