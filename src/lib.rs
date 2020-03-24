extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Group, Ident, Literal, TokenTree};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, parse_quote, Expr, LitInt, Result};

#[proc_macro_attribute]
pub fn impl_for(attr: TokenStream, input: TokenStream) -> TokenStream {
    let ProcForAttr { ident, expr } = parse_macro_input!(attr as ProcForAttr);
    match expr {
        IterableExpr::Array(v) | IterableExpr::Tuple(v) => {
            let tts = match into_tt(v) {
                Ok(t) => t,
                Err(e) => return e.to_compile_error().into(),
            };
            gen_impl(tts.into_iter(), &ident, input.into())
        }
        IterableExpr::Range(range) => gen_range(range, &ident, input.into()),
    }
    .into()
}

fn gen_range(range: Range, ident: &Ident, input: TokenStream2) -> TokenStream2 {
    let Range { from, to, suffix } = range;
    let into_lit = |i: i64| match &*suffix {
        "u8" => Literal::u8_suffixed(i as _),
        "u16" => Literal::u16_suffixed(i as _),
        "u32" => Literal::u32_suffixed(i as _),
        "u64" => Literal::u64_suffixed(i as _),
        "usize" => Literal::usize_suffixed(i as _),
        "i8" => Literal::i8_suffixed(i as _),
        "i16" => Literal::i16_suffixed(i as _),
        "i32" => Literal::i32_suffixed(i as _),
        "i64" => Literal::i64_suffixed(i as _),
        "isize" => Literal::isize_suffixed(i as _),
        _ => Literal::i64_unsuffixed(i),
    };
    gen_impl(
        (from..to).map(|i| TokenTree::from(into_lit(i))),
        ident,
        input,
    )
}

fn gen_impl(
    iter: impl Iterator<Item = TokenTree>,
    ident: &Ident,
    input: TokenStream2,
) -> TokenStream2 {
    let res: TokenStream2 = iter
        .map(|lit| {
            input
                .clone()
                .into_iter()
                .map(|tt| replace_by(tt, ident, &lit))
                .collect::<TokenStream2>()
        })
        .collect();

    return res;
    fn replace_by(tt: TokenTree, ident: &Ident, replace: &TokenTree) -> TokenTree {
        match tt {
            TokenTree::Group(g) => {
                let stream: TokenStream2 = g
                    .stream()
                    .into_iter()
                    .map(|tt| replace_by(tt, ident, replace))
                    .collect();
                TokenTree::Group(Group::new(g.delimiter(), stream))
            }
            TokenTree::Ident(id) => {
                if id == *ident {
                    replace.clone()
                } else {
                    TokenTree::Ident(id).into()
                }
            }
            TokenTree::Punct(p) => TokenTree::Punct(p).into(),
            TokenTree::Literal(l) => TokenTree::Literal(l).into(),
        }
    }
}

macro_rules! bail {
    ($span: ident, $msg: expr) => {
        return Err(syn::Error::new_spanned($span, $msg));
    };
}

fn into_tt(v: Vec<Expr>) -> Result<Vec<TokenTree>> {
    let mut res = vec![];
    for expr in v {
        match expr {
            Expr::Lit(lit) => res.push(parse_quote!(#lit)),
            Expr::Path(path) => res.push(parse_quote!(#path)),
            _ => bail!(expr, "Expected lit or path"),
        }
    }
    Ok(res)
}

struct ProcForAttr {
    ident: Ident,
    expr: IterableExpr,
}

impl Parse for ProcForAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        let _: syn::token::Eq = input.parse()?;
        let expr: Expr = input.parse()?;
        Ok(ProcForAttr {
            ident,
            expr: IterableExpr::new(expr)?,
        })
    }
}

struct Range {
    from: i64,
    to: i64,
    suffix: String,
}

enum IterableExpr {
    Array(Vec<Expr>),
    Range(Range),
    Tuple(Vec<Expr>),
}

impl IterableExpr {
    fn new(expr: Expr) -> Result<Self> {
        match &expr {
            Expr::Array(arr) => {
                let numbers = arr.elems.iter().cloned().collect::<Vec<_>>();
                Ok(IterableExpr::Array(numbers))
            }
            Expr::Range(range) => {
                let syn::ExprRange { from, to, .. } = &range;
                let (from, to) = match (from, to) {
                    (Some(f), Some(t)) => (int_from_expr(&*f)?, int_from_expr(&*t)?),
                    _ => bail!(range, "Only full range(e.g., 0..3) is supported"),
                };
                let suffix = if from.suffix().len() == 0 {
                    to.suffix().to_string()
                } else {
                    from.suffix().to_string()
                };
                Ok(IterableExpr::Range(Range {
                    from: from.base10_parse()?,
                    to: to.base10_parse()?,
                    suffix,
                }))
            }
            Expr::Tuple(tup) => {
                let numbers = tup.elems.iter().cloned().collect::<Vec<_>>();
                Ok(IterableExpr::Tuple(numbers))
            }
            _ => bail!(expr, "Only Array/Range/Tuple is supported"),
        }
    }
}

fn int_from_expr(expr: &Expr) -> Result<LitInt> {
    match expr {
        Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(i),
            ..
        }) => Ok(i.clone()),
        _ => bail!(expr, "Expected integer"),
    }
}
