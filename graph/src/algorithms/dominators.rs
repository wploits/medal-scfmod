use array_tool::vec::Intersect;
use fxhash::{FxHashMap, FxHashSet};
use std::borrow::Cow;

use itertools::Itertools;

use crate::{Edge, Error, Graph, NodeId, Result};

use super::dfs_tree;

pub fn dominator_tree(graph: &Graph, root: NodeId, idoms: &FxHashMap<NodeId, NodeId>) -> Result<Graph> {
    let mut dom_tree = Graph::new();
    for &vertex in graph.nodes() {
        dom_tree.add_node_with_id(vertex)?;
    }

    for (&vertex, &idom) in idoms {
        dom_tree.add_edge(Edge::new(idom, vertex))?;
    }

    Ok(dom_tree)
}

pub fn dominators(graph: &Graph, root: NodeId) -> Result<FxHashMap<NodeId, Vec<NodeId>>> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let dom_tree = dominator_tree(graph, root, &compute_immediate_dominators(graph, root)?)?;
    let dom_tree_pre_oder = dom_tree.pre_order(root)?;

    let mut dominators = FxHashMap::default();

    for vertex in dom_tree_pre_oder {
        let mut doms = Vec::new();
        doms.push(vertex);
        for pred in dom_tree.predecessors(vertex) {
            doms.extend(&dominators[&pred])
        }
        dominators.insert(vertex, doms);
    }

    Ok(dominators)
}

pub fn common_dominator(
    dominators: &FxHashMap<NodeId, Vec<NodeId>>,
    nodes: Vec<NodeId>,
) -> Result<Option<NodeId>> {
    let mut nodes_dominators_iter = nodes.iter().map(|&node| dominators[&node].clone());
    let mut res = nodes_dominators_iter.next().unwrap();
    for node_dominators in nodes_dominators_iter {
        res = node_dominators.intersect(res);
    }
    Ok(res.first().cloned())
}

pub fn post_dominator_tree(graph: &Graph, root: NodeId) -> Result<Graph> {
    let dfs = dfs_tree(graph, root)?;
    let exits = graph
        .nodes()
        .iter()
        .cloned()
        .filter(|&node| graph.successors(node).next().is_none() && dfs.node_exists(node))
        .collect::<Vec<_>>();

    let mut reverse_graph = Graph::new();
    for &node in graph.nodes() {
        reverse_graph.add_node_with_id(node)?;
    }
    for &node in graph.nodes() {
        for successor in graph.successors(node) {
            reverse_graph.add_edge(Edge::new(successor, node))?;
        }
    }
    let single_exit_node = reverse_graph.add_node()?;
    for exit in exits {
        reverse_graph.add_edge(Edge::new(single_exit_node, exit))?;
    }

    let mut dom_tree = Graph::new();
    for &vertex in reverse_graph.nodes() {
        dom_tree.add_node_with_id(vertex)?;
    }

    let idoms = compute_immediate_dominators(&reverse_graph, single_exit_node)?;
    for (vertex, idom) in idoms {
        if idom != single_exit_node {
            dom_tree.add_edge(Edge::new(idom, vertex))?;
        }
    }

    Ok(dom_tree)
}

pub fn post_dominators(
    graph: &Graph,
    root: NodeId,
) -> Result<FxHashMap<NodeId, FxHashSet<NodeId>>> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let dom_tree = post_dominator_tree(graph, root)?;
    let dom_tree_pre_oder = dom_tree.pre_order(root)?;

    let mut dominators: FxHashMap<NodeId, FxHashSet<NodeId>> = FxHashMap::default();

    for vertex in dom_tree_pre_oder {
        let mut doms = FxHashSet::default();
        doms.insert(vertex);
        for pred in dom_tree.predecessors(vertex) {
            doms.extend(&dominators[&pred])
        }
        dominators.insert(vertex, doms);
    }

    Ok(dominators)
}

pub fn compute_dominance_frontiers(
    graph: &Graph,
    root: NodeId,
    immediate_dominators: Option<Cow<FxHashMap<NodeId, NodeId>>>,
) -> Result<FxHashMap<NodeId, FxHashSet<NodeId>>> {
    let immediate_dominators = match immediate_dominators {
        Some(immediate_dominators) => immediate_dominators,
        None => Cow::Owned(compute_immediate_dominators(graph, root)?),
    };

    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let dfs = dfs_tree(graph, root)?;

    let mut dominance_frontiers = FxHashMap::default();

    for &index in dfs.nodes() {
        dominance_frontiers.insert(index, FxHashSet::default());
    }

    let preds = dfs
        .nodes()
        .iter()
        .map(|&n| {
            (
                n,
                graph
                    .predecessors(n)
                    .filter(|&n| dfs.node_exists(n))
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<FxHashMap<_, _>>();

    for &index in dfs.nodes() {
        let preds = &preds[&index];
        if preds.len() > 1 {
            for &pred in preds {
                let mut runner = pred;
                let node_idom = *immediate_dominators.get(&index).unwrap();

                while runner != node_idom {
                    dominance_frontiers.get_mut(&runner).unwrap().insert(index);
                    runner = *immediate_dominators.get(&runner).unwrap();
                }
            }
        }
    }

    Ok(dominance_frontiers)
}

// Computes immediate dominators for all vertices in the graph
///
/// This implementation is based on the Semi-NCA algorithm described in:
/// Georgiadis, Loukas: Linear-Time Algorithms for Dominators and Related Problems (thesis)
/// <https://www.cs.princeton.edu/research/techreps/TR-737-05>
pub fn compute_immediate_dominators(
    graph: &Graph,
    root: NodeId,
) -> Result<FxHashMap<NodeId, NodeId>> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let dfs = super::dfs_tree(graph, root)?;
    let dfs_pre_order = dfs.pre_order(root)?;

    // filter out unreachable nodes
    let preds = |n: NodeId| graph.predecessors(n).filter(|&p| dfs.node_exists(p));

    let dfs_parent = |vertex| dfs.predecessors(vertex).next();

    // DFS-numbering and reverse numbering (starting from 0 instead of 1 as in the paper)
    let dfs_number: FxHashMap<NodeId, usize> = dfs_pre_order
        .iter()
        .enumerate()
        .map(|(number, vertex)| (*vertex, number))
        .collect();
    let graph_number = &dfs_pre_order;

    let mut ancestor: FxHashMap<NodeId, Option<NodeId>> = FxHashMap::default();
    let mut label: FxHashMap<NodeId, usize> = FxHashMap::default();
    for &vertex in graph.nodes() {
        if let Some(&vertex_label) = dfs_number.get(&vertex) {
            ancestor.insert(vertex, None);
            label.insert(vertex, vertex_label);
        }
    }

    // Compute semidominators in reverse preorder (without root)
    let mut semi = FxHashMap::<NodeId, usize>::default();
    for &vertex in dfs_pre_order.iter().skip(1).rev() {
        let mut min_semi = std::usize::MAX;

        for pred in preds(vertex) {
            if ancestor[&pred].is_some() {
                compress(&mut ancestor, &mut label, pred);
            }
            min_semi = std::cmp::min(min_semi, label[&pred]);
        }

        semi.insert(vertex, min_semi);
        label.insert(vertex, min_semi);

        ancestor.insert(vertex, dfs_parent(vertex));
    }
    let semi = semi;

    // https://github.com/rust-lang/rust/blob/master/compiler/rustc_data_structures/src/graph/dominators/mod.rs#L236
    // TODO: should we use smallvec like rust?
    // TODO: should we use IndexVec instead of FxHashMap? (see: https://docs.rs/index_vec/latest/index_vec/)
    fn compress(
        ancestor: &mut FxHashMap<NodeId, Option<NodeId>>,
        label: &mut FxHashMap<NodeId, usize>,
        v: NodeId,
    ) {
        let mut stack = Vec::new();
        let mut u = v;
        loop {
            stack.push(u);
            match ancestor[&u] {
                Some(v) => u = v,
                None => break,
            }
        }

        for &v in stack.iter().rev() {
            if let Some(u) = ancestor[&v] {
                if let Some(x) = ancestor[&u] {
                    if label[&u] < label[&v] {
                        label.insert(v, label[&u]);
                    }
                    ancestor.insert(v, Some(x));
                }
            }
        }
    }

    // Compute immediate dominators in preorder (without root)
    let mut idoms = FxHashMap::<usize, usize>::default();
    for &vertex in dfs_pre_order.iter().skip(1) {
        let mut idom = dfs_number[&dfs_parent(vertex).unwrap()];
        while idom > semi[&vertex] {
            idom = idoms[&idom];
        }
        idoms.insert(dfs_number[&vertex], idom);
    }
    let idoms = idoms;

    // Translate idoms from DFS-numbering back to graph indices
    let mut graph_idoms = FxHashMap::default();
    for (vertex, idom) in idoms {
        graph_idoms.insert(graph_number[vertex], graph_number[idom]);
    }
    Ok(graph_idoms)
}
