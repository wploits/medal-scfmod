use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::{Error, Graph, NodeId, Result};

pub fn compute_dominance_frontiers(
    graph: &Graph,
    root: NodeId,
    immediate_dominators: Option<Cow<HashMap<NodeId, NodeId>>>,
) -> Result<HashMap<NodeId, HashSet<NodeId>>> {
    let immediate_dominators = match immediate_dominators {
        Some(immediate_dominators) => immediate_dominators,
        None => Cow::Owned(compute_immediate_dominators(graph, root)?),
    };

    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let mut dominance_frontiers = HashMap::new();

    for &index in &graph.nodes {
        dominance_frontiers.insert(index, HashSet::new());
    }

    for &index in &graph.nodes {
        let preds = graph.predecessors(index).cloned().collect::<Vec<_>>();
        if preds.len() > 1 {
            for pred in preds {
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
) -> Result<HashMap<NodeId, NodeId>> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let dfs = super::dfs_tree(graph, root)?.0;
    let dfs_pre_order = dfs.pre_order(root)?;

    let preds = |n: NodeId| {
        graph.predecessors(n).cloned().filter(|&p| dfs.node_exists(p))
    };

    let dfs_parent = |vertex| dfs.predecessors(vertex).next().cloned();

    // DFS-numbering and reverse numbering (starting from 0 instead of 1 as in the paper)
    let dfs_number: HashMap<NodeId, usize> = dfs_pre_order
        .iter()
        .enumerate()
        .map(|(number, vertex)| (*vertex, number))
        .collect();
    let graph_number = &dfs_pre_order;

    let mut ancestor: HashMap<NodeId, Option<NodeId>> = HashMap::default();
    let mut label: HashMap<NodeId, usize> = HashMap::default();
    for &vertex in graph.nodes() {
        if let Some(&vertex_label) = dfs_number.get(&vertex) {
            ancestor.insert(vertex, None);
            label.insert(vertex, vertex_label);
        }
    }

    // Compute semidominators in reverse preorder (without root)
    let mut semi = HashMap::<NodeId, usize>::default();
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

    fn compress(
        ancestor: &mut HashMap<NodeId, Option<NodeId>>,
        label: &mut HashMap<NodeId, usize>,
        v: NodeId,
    ) {
        let u = ancestor[&v].unwrap();
        if ancestor[&u].is_some() {
            compress(ancestor, label, u);
            if label[&u] < label[&v] {
                label.insert(v, label[&u]);
            }
            ancestor.insert(v, ancestor[&u]);
        }
    }

    // Compute immediate dominators in preorder (without root)
    let mut idoms = HashMap::<usize, usize>::default();
    for &vertex in dfs_pre_order.iter().skip(1) {
        let mut idom = dfs_number[&dfs_parent(vertex).unwrap()];
        while idom > semi[&vertex] {
            idom = idoms[&idom];
        }
        idoms.insert(dfs_number[&vertex], idom);
    }
    let idoms = idoms;

    // Translate idoms from DFS-numbering back to graph indices
    let mut graph_idoms = HashMap::default();
    for (vertex, idom) in idoms {
        graph_idoms.insert(graph_number[vertex], graph_number[idom]);
    }
    Ok(graph_idoms)
}
