use crate::{LocalRw, SideEffects, Traverse};

#[derive(Debug, Clone, PartialEq)]
pub struct VarArg;

impl LocalRw for VarArg {}

impl SideEffects for VarArg {}

impl Traverse for VarArg {}
