use crate::parser::{Expr, ExprKind, ImCompleteSemanticToken};

pub enum LegendType {
    Function,
    Variable,
    String,
    Comment,
    Number,
    Keyword,
    Operator,
    Parameter,
}

pub fn semantic_token_from_ast(ast: &Vec<Expr>) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    ast.iter().for_each(|expr| {
        semantic_token_from_expr(expr, &mut semantic_tokens);
    });

    semantic_tokens
}

pub fn semantic_token_from_expr(expr: &Expr, semantic_tokens: &mut Vec<ImCompleteSemanticToken>) {
    match &expr.inner {
        ExprKind::Call(string, params) => {
            if let Some(params) = params {
                params.iter().for_each(|param| {
                    semantic_token_from_expr(param, semantic_tokens);
                });
            }
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: string.len(),
                token_type: LegendType::Function as usize,
            });
        }
        ExprKind::Ident(string) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: string.len(),
                token_type: LegendType::Variable as usize,
            });
        }
        ExprKind::Bool(_) | ExprKind::Nil | ExprKind::Continue | ExprKind::Break => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LegendType::Keyword as usize,
            });
        }
        ExprKind::Binary(lhs, _, rhs)
        | ExprKind::AddIndex(lhs, rhs)
        | ExprKind::SubIndex(lhs, rhs)
        | ExprKind::MulIndex(lhs, rhs)
        | ExprKind::DivIndex(lhs, rhs) => {
            semantic_token_from_expr(lhs, semantic_tokens);
            semantic_token_from_expr(rhs, semantic_tokens);
        }
        ExprKind::Index(lhs, rhs) => {
            semantic_token_from_expr(lhs, semantic_tokens);
            semantic_token_from_expr(rhs, semantic_tokens);
        }
        ExprKind::Unary(_, rhs) => {
            semantic_token_from_expr(rhs, semantic_tokens);
        }

        ExprKind::Int(_) | ExprKind::Float(_) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LegendType::Number as usize,
            });
        }
        ExprKind::String(_) | ExprKind::FString(_) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LegendType::String as usize,
            });
        }
        ExprKind::Array(exprs) => {
            exprs.iter().for_each(|expr| {
                semantic_token_from_expr(expr, semantic_tokens);
            });
        }
        ExprKind::Ternary(cond, then, els) => {
            semantic_token_from_expr(cond, semantic_tokens);
            then.iter().for_each(|expr| {
                semantic_token_from_expr(expr, semantic_tokens);
            });
            if let Some(els) = els {
                els.iter().for_each(|expr| {
                    semantic_token_from_expr(expr, semantic_tokens);
                });
            }
        }
        ExprKind::InlineFunction(name, params, body) => {
            semantic_token_from_expr(body, semantic_tokens);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: name.len(),
                token_type: LegendType::Function as usize,
            });
            let params = params
                .iter()
                .map(|param| param.len() + 1)
                .fold(0, |acc, len| acc + len);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start + name.len(),
                length: params,
                token_type: LegendType::Parameter as usize,
            });
        }
        ExprKind::MultilineFunction(name, params, body) => {
            body.iter().for_each(|expr| {
                semantic_token_from_expr(expr, semantic_tokens);
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: name.len(),
                token_type: LegendType::Function as usize,
            });
            let params = params
                .iter()
                .map(|param| param.len() + 1)
                .fold(0, |acc, len| acc + len);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start + name.len(),
                length: params,
                token_type: LegendType::Parameter as usize,
            });
        }
        ExprKind::Postfix(lhs, _) => {
            semantic_token_from_expr(lhs, semantic_tokens);
        }
        _ => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LegendType::Variable as usize,
            });
        }
    }
}
