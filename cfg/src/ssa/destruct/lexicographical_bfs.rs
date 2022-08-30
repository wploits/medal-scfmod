use ast::RcLocal;
use fxhash::{FxHashMap, FxHashSet};
use graph::{Directed, Graph, NodeIndex};
// TODO: Miri detects undefined behavior in ritelinked, make an issue on the github repo.
// use ritelinked::LinkedHashSet;
use hashlink::LinkedHashSet;

use super::interference_graph::InterferenceGraph;

// https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/util/lexicographical_bfs.py
pub struct LexicographicalBFS<'a> {
    interference_graph: &'a mut InterferenceGraph,
    labeling_graph: Graph<Directed>,
    node_to_var_set: FxHashMap<NodeIndex, LinkedHashSet<RcLocal>>,
    // variable to labeling graph node containing the variable
    var_to_node: FxHashMap<RcLocal, NodeIndex>,
    head: Option<NodeIndex>,
}

impl<'a> LexicographicalBFS<'a> {
    pub fn new(interference_graph: &'a mut InterferenceGraph) -> Self {
        Self {
            interference_graph,
            labeling_graph: Graph::new(),
            node_to_var_set: FxHashMap::default(),
            var_to_node: FxHashMap::default(),
            head: None,
        }
    }

    pub fn reverse_lexicographic_bfs(&mut self) -> Vec<RcLocal> {
        let mut all_vars = LinkedHashSet::new();
        let mut isolated_vars = Vec::new();
        for node in self.interference_graph.graph.nodes().clone() {
            let var = self.interference_graph.node_to_variable[&node].clone();
            if self.interference_graph.graph.neighbors(node).is_empty() {
                isolated_vars.push(var);
            } else {
                all_vars.insert(var);
            }
        }
        all_vars.extend(isolated_vars);

        self.head = Some(self.labeling_graph.add_node());
        for var in &all_vars {
            self.var_to_node.insert(var.clone(), self.head.unwrap());
        }
        self.node_to_var_set.insert(self.head.unwrap(), all_vars);

        let mut res = Vec::new();
        while !self.labeling_graph.nodes().is_empty() {
            let curr_var = self.get_lexicographical_largest_var();
            self.var_to_node.remove(&curr_var);
            self.update_labeling_graph(&curr_var);
            res.push(curr_var);
        }
        res
    }

    fn get_lexicographical_largest_var(&mut self) -> RcLocal {
        let var_set = self.node_to_var_set.get_mut(&self.head.unwrap()).unwrap();
        let curr_var = var_set.pop_back().unwrap();
        if var_set.is_empty() {
            let prev_head = self.head.take().unwrap();
            if let Some(head) = self.labeling_graph.successors(prev_head).into_iter().next() {
                self.head = Some(head);
            }
            self.labeling_graph.remove_node(prev_head);
        }

        curr_var
    }

    fn update_labeling_graph(&mut self, curr_var: &RcLocal) {
        let mut modified_labeling_nodes = FxHashSet::default();
        for neighbor in self
            .interference_graph
            .graph
            .neighbors(self.interference_graph.local_to_node[curr_var])
        {
            let neighbor_var = self.interference_graph.node_to_variable[&neighbor].clone();
            if let Some(&neighbor_labeling_node) = self.var_to_node.get(&neighbor_var) {
                if modified_labeling_nodes.contains(&neighbor_labeling_node) {
                    let pred_labeling_node =
                        self.labeling_graph.predecessors(neighbor_labeling_node)[0];
                    self.node_to_var_set
                        .get_mut(&pred_labeling_node)
                        .unwrap()
                        .insert(neighbor_var.clone());
                    self.var_to_node
                        .insert(neighbor_var.clone(), pred_labeling_node);
                } else {
                    self.insert_new_node_for_var(&neighbor_var);
                    modified_labeling_nodes.insert(neighbor_labeling_node);
                }
                self.remove_var_from_prev_node(&neighbor_var, neighbor_labeling_node);
            }
        }
    }

    fn insert_new_node_for_var(&mut self, var: &RcLocal) {
        let curr_var_node = self.var_to_node.remove(var).unwrap();
        let new_var_node = self.labeling_graph.add_node();
        let mut new_var_set = LinkedHashSet::new();
        new_var_set.insert(var.clone());
        self.node_to_var_set.insert(new_var_node, new_var_set);
        self.var_to_node.insert(var.clone(), new_var_node);

        if self.head.map_or(false, |h| h == curr_var_node) {
            self.head = Some(new_var_node);
        } else {
            let prev_node = self.labeling_graph.predecessors(curr_var_node)[0];
            self.labeling_graph.remove_edge(&(prev_node, curr_var_node));
            self.labeling_graph.add_edge((prev_node, new_var_node))
        }
        self.labeling_graph.add_edge((new_var_node, curr_var_node));
    }

    // TODO: rename to `remove_var_from_node`
    fn remove_var_from_prev_node(&mut self, var: &RcLocal, prev_node: NodeIndex) {
        // TODO: use insertation order set (indexset doesnt preserve order after remove)
        let var_set = self.node_to_var_set.get_mut(&prev_node).unwrap();
        var_set.retain(|v| v != var);
        if var_set.is_empty() {
            if let Some(predecessor) = self
                .labeling_graph
                .predecessors(prev_node)
                .into_iter()
                .next()
            {
                if let Some(successor) =
                    self.labeling_graph.successors(prev_node).into_iter().next()
                {
                    self.labeling_graph.add_edge((predecessor, successor))
                }
            }
            self.labeling_graph.remove_node(prev_node);
        }
    }
}
