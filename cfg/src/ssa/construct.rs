use ast::LocalRw;
use fxhash::FxHashMap;
use graph::{algorithms::dfs_tree, Directed, Graph, NodeId};

use crate::{block::Terminator, function::Function, ssa_def_use};

struct SsaConstructor<'a> {
    function: &'a mut Function,
    dfs: Graph<Directed>,
    current_definition: FxHashMap<ast::RcLocal, FxHashMap<NodeId, ast::RcLocal>>,
}

// based on "Simple and Efficient Construction of Static Single Assignment Form" (https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf)
impl<'a> SsaConstructor<'a> {
    fn remove_unused_parameters(&mut self) {
        let def_use = ssa_def_use::SsaDefUse::new(self.function);

        let to_remove = def_use
            .parameters
            .into_iter()
            .filter(|(local, _)| !def_use.references.contains_key(local))
            .collect::<Vec<_>>();

        for (local, locations) in to_remove {
            for edge in locations {
                match self.function.block_mut(edge.0).unwrap().terminator.as_mut() {
                    Some(Terminator::Jump(edge)) => {
                        edge.arguments.retain(|target, _| target.0 != local);
                    }
                    Some(Terminator::Conditional(then_edge, else_edge)) => {
                        if then_edge.node == edge.1 {
                            then_edge.arguments.retain(|target, _| target.0 != local);
                        } else if else_edge.node == edge.1 {
                            else_edge.arguments.retain(|target, _| target.0 != local);
                        } else {
                            unreachable!();
                        }
                    }
                    None => {}
                }
            }
        }
    }

    fn find_local(&mut self, node: NodeId, local: &ast::RcLocal) -> ast::RcLocal {
        if let Some(new_local) = self.current_definition[local].get(&node) {
            // local to block
            new_local.clone()
        } else {
            // global
            let preds = self
                .function
                .graph()
                .predecessors(node)
                .into_iter()
                .filter(|&p| self.dfs.has_node(p))
                .collect::<Vec<_>>();
            if preds.len() == 1 {
                self.find_local(*preds.first().unwrap(), local)
            } else {
                let parameter_local = self.function.local_allocator.allocate();
                self.current_definition
                    .entry(local.clone())
                    .or_insert_with(FxHashMap::default)
                    .insert(node, parameter_local.clone());

                for pred in preds {
                    let argument_local = self.find_local(pred, local);
                    match self
                        .function
                        .block_mut(pred)
                        .unwrap()
                        .terminator
                        .as_mut()
                        .unwrap()
                    {
                        Terminator::Jump(edge) => {
                            assert!(edge.node == node);
                            edge.arguments
                                .insert(parameter_local.clone(), argument_local);
                        }
                        Terminator::Conditional(then_edge, else_edge) => {
                            if then_edge.node == node {
                                then_edge
                                    .arguments
                                    .insert(parameter_local.clone(), argument_local);
                            } else if else_edge.node == node {
                                else_edge
                                    .arguments
                                    .insert(parameter_local.clone(), argument_local);
                            } else {
                                unreachable!();
                            }
                        }
                    }
                }
                // todo: try remove trivial parameter

                parameter_local
            }
        }
    }

    fn construct(mut self) {
        for &node in self.dfs.nodes() {
            for stat_index in 0..self.function.block(node).unwrap().len() {
                let statement = self
                    .function
                    .block_mut(node)
                    .unwrap()
                    .get_mut(stat_index)
                    .unwrap();
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for local in written {
                    let new_local = self.function.local_allocator.allocate();
                    self.current_definition
                        .entry(local.clone())
                        .or_insert_with(FxHashMap::default)
                        .insert(node, new_local.clone());
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .get_mut(stat_index)
                        .unwrap();
                    statement
                        .as_assign_mut()
                        .unwrap()
                        .replace_values_written(&local, &new_local);
                }
            }
        }
        for node in self.dfs.nodes().clone() {
            for stat_index in 0..self.function.block(node).unwrap().len() {
                let statement = self
                    .function
                    .block_mut(node)
                    .unwrap()
                    .get_mut(stat_index)
                    .unwrap();
                let read = statement
                    .values_read()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for (local_index, local) in read.into_iter().enumerate() {
                    let new_local = self.find_local(node, &local);
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .get_mut(stat_index)
                        .unwrap();
                    *statement.values_read_mut()[local_index] = new_local;
                }
            }
        }

        self.remove_unused_parameters();
        //crate::dot::render_to(self.function, &mut std::io::stdout()).unwrap();
    }
}

pub fn construct(function: &mut Function) {
    SsaConstructor {
        dfs: dfs_tree(function.graph(), function.entry().unwrap()),
        function,
        current_definition: FxHashMap::default(),
    }
    .construct();
}
