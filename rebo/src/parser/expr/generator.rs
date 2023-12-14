use petgraph::graph::DiGraph;
use super::*;

pub fn transform_generator<'a, 'i>(parser: &mut Parser<'a, '_, 'i>, gen_token: TokenGen, fun: ExprFunctionDefinition<'a, 'i>) -> ExprFunctionDefinition<'a, 'i> {
    let yield_type = match fun.sig.ret_type {
        Some((_, typ)) => typ,
        None => ExprType::Unit(
            TokenOpenParen { span: fun.sig.span() },
            TokenCloseParen { span: fun.sig.span() },
        ),
    };

    let BlockBody { exprs, terminated_with_semicolon } = fun.body.body;
    let exprs = exprs.into_iter().map(|expr| transform_expr(parser, expr)).collect();

    ExprFunctionDefinition {
        sig: ExprFunctionSignature {
            fn_token: fun.sig.fn_token,
            name: fun.sig.name,
            generics: fun.sig.generics,
            open: fun.sig.open,
            self_arg: fun.sig.self_arg,
            self_arg_comma: fun.sig.self_arg_comma,
            args: fun.sig.args,
            varargs: fun.sig.varargs,
            close: fun.sig.close,
            ret_type: Some((TokenArrow { span: gen_token.span }, ExprType::Generator(Box::new(yield_type)))),
        },
        captures: fun.captures,
        body: ExprBlock {
            open: fun.body.open,
            body: BlockBody { exprs, terminated_with_semicolon },
            close: fun.body.close,
        },
    }
}

enum Yield {
    Encountered,
    Nope,
}

struct State<'a, 'i> {
    expr
}
struct GenericTransformator {
    graph: DiGraph<>
}

pub fn transform_expr<'a, 'i>(parser: &mut Parser<'a, '_, 'i>, expr: &'a Expr<'a, 'i>) -> (&'a Expr<'a, 'i>, Yield) {
    match expr {
        lit @ Expr::Literal(_) => (lit, Yield::Nope),
        Expr::FormatString(_) => todo!(),
        Expr::Bind(_) => todo!(),
        Expr::Static(_) => todo!(),
        Expr::Assign(_) => todo!(),
        // unops
        Expr::BoolNot(_) => todo!(),
        Expr::Neg(_) => todo!(),
        // binops
        Expr::Add(_) => todo!(),
        Expr::Sub(_) => todo!(),
        Expr::Mul(_) => todo!(),
        Expr::Div(_) => todo!(),
        Expr::Mod(_) => todo!(),
        Expr::Xor(_) => todo!(),
        Expr::BoolAnd(_) => todo!(),
        Expr::BoolOr(_) => todo!(),
        // binop-assign
        Expr::AddAssign(_) => todo!(),
        Expr::SubAssign(_) => todo!(),
        Expr::MulAssign(_) => todo!(),
        Expr::DivAssign(_) => todo!(),
        Expr::ModAssign(_) => todo!(),
        Expr::XorAssign(_) => todo!(),
        Expr::BoolAndAssign(_) => todo!(),
        Expr::BoolOrAssign(_) => todo!(),
        // comparison ops
        Expr::LessThan(_) => todo!(),
        Expr::LessEquals(_) => todo!(),
        Expr::Equals(_) => todo!(),
        Expr::NotEquals(_) => todo!(),
        Expr::GreaterEquals(_) => todo!(),
        Expr::GreaterThan(_) => todo!(),
        Expr::Block(_) => todo!(),
        Expr::Variable(_) => todo!(),
        Expr::Access(_) => todo!(),
        Expr::Parenthesized(_) => todo!(),
        Expr::IfElse(_) => todo!(),
        Expr::Match(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::For(_) => todo!(),
        Expr::Loop(_) => todo!(),
        Expr::Break(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Return(_) => todo!(),
        Expr::Yield(_) => todo!(),
        Expr::FunctionCall(_) => todo!(),
        Expr::FunctionDefinition(_) => todo!(),
        Expr::StructDefinition(_) => todo!(),
        Expr::StructInitialization(_) => todo!(),
        Expr::EnumDefinition(_) => todo!(),
        Expr::EnumInitialization(_) => todo!(),
        Expr::ImplBlock(_) => todo!(),
    }
}
