use cfg_ir::{
    def_use::DefUse,
    function::Function,
    instruction::location::{InstructionIndex, InstructionLocation},
    value::ValueId,
};
use fxhash::FxHashMap;
use graph::{
    algorithms::dominators::{common_dominator, dominators},
    NodeId,
};

#[derive(Debug, Clone)]
pub(super) enum Declaration {
    Forward(ValueId),
    Inline(ValueId),
}

pub(super) fn local_declarations(
    function: &Function,
    root: NodeId,
    idoms: &FxHashMap<NodeId, NodeId>,
) -> FxHashMap<InstructionLocation, Vec<Declaration>> {
    let mut def_use = DefUse::new(function);
    def_use.remove_unused();

    let dominators = dominators(function.graph(), root, idoms).unwrap();

    let declaration_node = def_use
        .values()
        .into_iter()
        .map(|value| {
            let value_def_use = def_use.get(value).unwrap();
            (
                value,
                common_dominator(
                    &dominators,
                    value_def_use
                        .reads
                        .iter()
                        .chain(value_def_use.writes.iter())
                        .map(|location| location.node)
                        .collect::<Vec<_>>(),
                )
                .unwrap(),
            )
        })
        .collect::<FxHashMap<_, _>>();

    let mut declaration_location = FxHashMap::default();

    for (value, node) in declaration_node {
        let value_def_use = def_use.get(value).unwrap();
        let mut usages = value_def_use
            .reads
            .iter()
            .map(|&v| (v, true))
            .chain(value_def_use.writes.iter().map(|&v| (v, false)))
            .filter(|(location, _)| location.node == node)
            .collect::<Vec<_>>();
        usages.sort_by(|(a, _), (b, _)| a.index.partial_cmp(&b.index).unwrap());
        let (location, declaration) = usages
            .first()
            .map(|&(location, forward)| {
                if forward {
                    (location, Declaration::Forward(value))
                } else {
                    (location, Declaration::Inline(value))
                }
            })
            .unwrap_or((
                InstructionLocation {
                    node,
                    index: InstructionIndex::Terminator,
                },
                Declaration::Forward(value),
            ));

        declaration_location
            .entry(location)
            .or_insert_with(Vec::new)
            .push(declaration);

        /*if let Some(&(location, forward_declare)) = usages.first() {
            declaration_location
                .entry(location)
                .or_insert_with(Vec::new)
                .push(LocalDeclaration {
                    value,
                    forward_declare,
                });
        } else {
            declaration_location
                .entry(InstructionLocation {
                    node,
                    index: InstructionIndex::Terminator,
                })
                .or_insert_with(Vec::new)
                .push(LocalDeclaration {
                    value,
                    forward_declare: true,
                });
        }*/
    }

    declaration_location
}
