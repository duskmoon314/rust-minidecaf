/**
*IR command*
- push x: put x in stack, stack_pointer + 1
- ret: pop stack top, stack_pointer - 1
*/

#[derive(Debug)]
pub enum IRStatement {
    Push(i32),
    Return,
}
