use enum_dispatch::enum_dispatch;
use graph::NodeId;

// A trait implemented for branching instructions
#[enum_dispatch]
pub(crate) trait BranchInfo {
    // Returns the branches the instruction can take
    fn branches(&self) -> Vec<NodeId>;

    // Returns the branches the instruction can take
    fn branches_mut(&mut self) -> Vec<&mut NodeId>;

    // Replaces a branch to `old` with `new`
    // Caller is responsible for correctness!
    fn replace_branch(&mut self, old: NodeId, new: NodeId) {
        for value in self.branches_mut() {
            if *value == old {
                *value = new;
            }
        }
    }
}
