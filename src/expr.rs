enum ExprType {
    #[default]
    Undefined,

    LoadTrue,
    LoadFalse,
    LoadInt,
    LoadReal,
    LoadString,
    LoadLocalVariable,
    SetLocalVariable,
    Equal,
    NotEqual,
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Less,
    LessEq,
    Not,
    And,
    Or,
    Skip,
    Quit,
    Return,
    If,
    For,
    Dot,
    Args,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    PowAssign,
    Assert,
    Script,
}

trait Expr {
    fn get_expr_type(&self) -> ExprType;
    fn get_src_line(&self) -> ExprType;
}

struct ExprBase {
    expr_type: ExprType,
    src_line: usize,
    parent_expr: Option<ExprBase>,
}

impl ExprBase {
    pub fn new(expr_type: ExprType, src_line: usize, parent_expr: Option<ExprBase>) -> ExprBase {
        return ExprBase { expr_type, src_line, parent_expr };
    }
}

///////////////////////////////////////////////////////////

struct ExprLoadTrue {

}
