use crate::{algorithms::dfs_tree, Graph, Result};

use super::BackEdges;

struct FlowRepresentation {}

impl FlowRepresentation {
    fn new(graph: Graph, back_edges: BackEdges) -> Self {
        let loop_headers = {
            let mut loop_headers = back_edges
                .into_iter()
                .map(|edge| edge.1)
                .collect::<Vec<_>>();
            loop_headers.sort();
            loop_headers.dedup();
            loop_headers
        };

        println!("loop_headers: {:?}", loop_headers);
        Self {}
    }
}

pub fn test(graph: &Graph) -> Result<()> {
    let (tree, back_edges) = dfs_tree(graph, graph.entry().unwrap())?;
    let flow_rep = FlowRepresentation::new(tree, back_edges);
    //dot::render(&tree, &mut std::io::stdout()).unwrap();
    Ok(())
}
