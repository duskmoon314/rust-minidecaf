/**
*Symbol*
- `(` Lparen
- `)` Rparen
- `[` Lbrack
- `]` Rbrack
- `{` Lbrace
- `}` Rbrace
- `;` Semicolon
- `,` Comma
**/

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,
    Lbrace,
    Rbrace,
    Semicolon,
    Comma,
    EOF,
}

/**
*Operator*
- `-` Minus
- `!` Not
- `~` BitwiseNot
 */

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Minus,
    Not,
    BitwiseNot,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        matches!(self, Operator::Minus | Operator::Not | Operator::BitwiseNot)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Return,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Symbol(Symbol),
    Operator(Operator),
    Keyword(Keyword),
    Type(Type),
    Identifier(String),
    Integer(i32),
}
