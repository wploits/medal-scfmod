use cfg_ir::{
    def_use::DefUse,
    function::Function,
    instruction::location::{InstructionLocation},
    value::ValueId,
};
use fxhash::FxHashMap;
use graph::{
    algorithms::dominators::{common_dominator, dominators},
    NodeId,
};

pub(super) fn local_declarations(
    function: &Function,
    root: NodeId,
    idoms: &FxHashMap<NodeId, NodeId>,
) -> FxHashMap<InstructionLocation, Vec<ValueId>> {
    let mut def_use = DefUse::new(function);
    def_use.remove_unused();

    let dominators = dominators(function.graph(), root, idoms).unwrap();

    let declaration_node = def_use.values().into_iter().map(|value| {
        let value_def_use = def_use.get(value).unwrap();
        (
            common_dominator(
                &dominators,
                value_def_use
                    .writes
                    .iter()
                    .map(|location| location.node)
                    .collect::<Vec<_>>(),
            )
            .unwrap(),
            value,
        )
    });

    let mut declaration_location = FxHashMap::default();

    for (node, value) in declaration_node {
        let mut writes = def_use
            .get(value)
            .unwrap()
            .writes
            .iter()
            .filter(|location| location.node == node)
            .collect::<Vec<_>>();
        writes.sort_by(|a, b| a.index.partial_cmp(&b.index).unwrap());
        declaration_location
            .entry(**writes.first().unwrap())
            .or_insert_with(Vec::new)
            .push(value);
    }

    declaration_location
}
