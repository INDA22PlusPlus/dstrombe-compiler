//stdlib fromhex

enum Keyword {

    // types
    Str,
    Float,
    Integer,
    Addr,
    Boolean,
    
    // function
    Function,
    If,
    Elif,
    Else,
    For,
    While
}

enum Identifier {
    Function(String),
    Variable(String),
}

enum Constant {
    Str(String),
    Float(f64),
    Integer(i128),
    Addr(u128),
}

// Serve to group other tokens together mostly, for processing in conjunction
// with eg. if-statements or grouping function args together
enum SpecialChar {
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
}

enum BinaryOperator {
    Plus,
    Minus,
    Mult,
    Div,

    LShift,
    RShift,

    Assign,
    
    GreaterOrEq,
    LessOrEq,
    Equals,

    Or,
    And,
    Xor,
    
}

enum UnaryOperator {
    Not
}
// Main token representation
enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    Constant(Constant),
    SpecialChar(SpecialChar),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator)
}



