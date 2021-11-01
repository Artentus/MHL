#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Not,
    AddressOf,
    Deref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    ShortCircuitAnd,
    ShortCircuitOr,
    And,
    Or,
    Xor,
    Equals,
    NotEquals,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Word,
    UWord,
    Int,
    UInt,
    Long,
    ULong,
    Char,
    Custom(String),
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerConstant(i128),
    CharConstant(char),
    StringConstant(String),
    Variable(String),
    SizeOf(Type),
    FunctionCall(String, Vec<Expression>),
    ArrayAccess(Box<Expression>, Box<Expression>),
    DotAccess(Box<Expression>, String),
    Cast(Type, Box<Expression>),
    UnaryOperator(UnaryOperator, Box<Expression>),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub identifier: String,
    pub const_type: Type,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub var_type: Type,
    pub initial_value: Option<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    Normal,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub target: Expression,
    pub kind: AssignmentKind,
    pub source: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub true_body: Vec<Statement>,
    pub false_body: Option<Vec<Statement>>,
}

//#[derive(Debug, Clone)]
//pub struct SwitchStatement {
//    pub condition: Expression,
//    pub true_body: Vec<Statement>,
//    pub false_body: Vec<Statement>,
//}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

//#[derive(Debug, Clone)]
//pub struct ForLoop {
//    pub condition: Expression,
//    pub body: Vec<Statement>,
//}

#[derive(Debug, Clone)]
pub enum Statement {
    ConstantDeclaration(ConstantDeclaration),
    VariableDeclaration(VariableDeclaration),
    Assignment(Assignment),
    If(IfStatement),
    //Switch(SwitchStatement),
    While(WhileLoop),
    //For(ForLoop),
    Break(usize),
    Continue(usize),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub struct ArgumentDefinition {
    pub identifier: String,
    pub arg_type: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub identifier: String,
    pub return_type: Option<Type>,
    pub args: Vec<ArgumentDefinition>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub identifier: String,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub identifier: String,
    pub fields: Vec<FieldDefinition>,
}

#[derive(Debug, Clone)]
pub enum TopLevelStatement {
    ConstantDeclaration(ConstantDeclaration),
    FunctionDefinition(FunctionDefinition),
    StructDefinition(StructDefinition),
    UnionDefinition(StructDefinition),
}
