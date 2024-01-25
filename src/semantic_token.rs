use crate::chumsky::{Expr, ExprKind, ImCompleteSemanticToken};
use std::collections::HashMap;
use tower_lsp::lsp_types::SemanticTokenType;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
];

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
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: 0,
                length: string.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            if let Some(params) = params {
                params.iter().for_each(|param| {
                    semantic_token_from_expr(param, semantic_tokens);
                });
                params.iter().for_each(|exprs| {});
            }
        }
        ExprKind::Ident(string) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: 0,
                length: string.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
        ExprKind::Bool(_) | ExprKind::Nil | ExprKind::Continue | ExprKind::Break => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
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
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::NUMBER)
                    .unwrap(),
            });
        }
        ExprKind::String(_) | ExprKind::FString(_) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::STRING)
                    .unwrap(),
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
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: name.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start + name.len(),
                length: expr.span.end,
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::PARAMETER)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens);
        }
        ExprKind::MultilineFunction(name, params, body) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: 0,
                length: name.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::PARAMETER)
                    .unwrap(),
            });
        }
        _ => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: expr.span.start,
                length: expr.span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
    }
}
