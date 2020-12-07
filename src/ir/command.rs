/**
*IR command*
- push x        : put x in stack, stack_pointer + 1
- ret           : pop stack top, stack_pointer - 1
- Neg           : pop, neg, push back
- Not           : pop, bitwise not, push back
- LogicalNot    : pop, cpp-like logical not, push back
*/

#[derive(Debug)]
pub enum IRStatement {
    Push(i32),
    Return,
    Neg,
    Not,
    LogicalNot,
}
