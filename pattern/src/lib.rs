#![feature(let_chains)]

use std::collections::HashMap;

use proc_macro2::{Spacing, TokenStream, TokenTree};
use quote::quote;

macro_rules! as_token {
    ($e:expr, $m:path) => {
        match $e {
            $m(r) => Some(r),
            _ => None,
        }
    };
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Node {
    Wildcard,
    Named(String),
}

#[derive(Debug)]
enum BranchType {
    Unconditional,
    Then,
    Else,
}

fn parse_node(iter: &mut impl Iterator<Item = TokenTree>) -> Node {
    let token = iter.next().unwrap();
    match token {
        TokenTree::Ident(_) | TokenTree::Literal(_) => Node::Named(token.to_string()),
        TokenTree::Punct(punct) if punct.as_char() == '$' && punct.spacing() == Spacing::Alone => {
            Node::Wildcard
        }
        _ => unimplemented!(),
    }
}

fn parse_branch_type(iter: &mut impl Iterator<Item = TokenTree>) -> BranchType {
    match iter.next().unwrap() {
        TokenTree::Ident(ident) => match ident.to_string().as_str() {
            "then" => BranchType::Then,
            "else" => BranchType::Else,
            _ => unimplemented!(),
        },
        TokenTree::Punct(punct) if punct.as_char() == '-' && punct.spacing() == Spacing::Joint => {
            let punct = as_token!(iter.next().unwrap(), TokenTree::Punct).unwrap();
            if punct.as_char() == '>' && punct.spacing() == Spacing::Alone {
                BranchType::Unconditional
            } else {
                unimplemented!()
            }
        }
        _ => unimplemented!(),
    }
}

#[derive(Debug, PartialEq, Eq)]
enum EdgeRuleType {
    ZeroOrMore, // *
    OneOrMore,  // +
    One,        // -
    Optional,   // ?
}

impl From<char> for EdgeRuleType {
    fn from(value: char) -> Self {
        match value {
            '*' => Self::ZeroOrMore,
            '+' => Self::OneOrMore,
            '-' => Self::One,
            '?' => Self::Optional,
            _ => unimplemented!(),
        }
    }
}

type Edge = (Node, Node, BranchType, Option<String>);
type EdgeRule = (Vec<Edge>, EdgeRuleType);

fn pattern_internal(input: TokenStream) -> TokenStream {
    let mut nodes = Vec::new();
    let mut successors = HashMap::<Node, Vec<Node>>::new();
    let mut predecessors = HashMap::<Node, Vec<Node>>::new();
    let mut edge_rules = Vec::<EdgeRule>::new();

    let mut iter = input.into_iter().peekable();
    while let Some(token) = iter.next() {
        let rule_type = as_token!(token, TokenTree::Punct).unwrap().as_char();
        match rule_type {
            '@' => {
                let node = as_token!(iter.next().unwrap(), TokenTree::Ident)
                    .unwrap()
                    .to_string();
                nodes.push(node)
            }
            '*' | '+' | '-' | '?' => {
                let mut edges = Vec::new();
                loop {
                    let source_node = parse_node(&mut iter);
                    let branch_type = parse_branch_type(&mut iter);
                    let dest_node = parse_node(&mut iter);
                    let binding = if let Some(token) = iter.peek()
                        && let Some(punct) = as_token!(token, TokenTree::Punct)
                        && punct.as_char() == '&'
                    {
                        iter.next();
                        Some(as_token!(iter.next().unwrap(), TokenTree::Ident).unwrap().to_string())
                    } else {
                        None
                    };
                    edges.push((source_node.clone(), dest_node.clone(), branch_type, binding));
                    successors
                        .entry(source_node.clone())
                        .or_default()
                        .push(dest_node.clone());
                    predecessors
                        .entry(dest_node.clone())
                        .or_default()
                        .push(source_node.clone());

                    if let Some(token) = iter.peek()
                        && let Some(punct) = as_token!(token, TokenTree::Punct)
                        && punct.as_char() == ','
                        && punct.spacing() == Spacing::Alone
                    {
                        iter.next();
                    } else {
                        break;
                    }
                }

                edge_rules.push((edges, rule_type.into()))
            }
            _ => unimplemented!(),
        }
    }

    let from_wildcard_edges = successors.get(&Node::Wildcard).unwrap();
    // TODO: support len == 0 and len > 1
    assert!(from_wildcard_edges.len() == 1);

    let _from_wildcard_edge = &from_wildcard_edges[0];

    quote!({
        use ::cfg::function::Function;
        use ::petgraph::stable_graph::NodeIndex;
        |function: &Function, root: NodeIndex| {
            let mut matches = Vec::new();

            matches
        }
    })
}

#[proc_macro]
pub fn pattern(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    pattern_internal(input.into()).into()
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_pattern() {
        pattern_internal(
            "
        @entry
        @then
        @else
        @exit
        * $ -> entry
        - entry then then
        - entry else else
        ? then -> exit, else -> exit
        * $ -> exit
        * exit -> $
        "
            .parse()
            .unwrap(),
        );
        todo!();
    }
}
