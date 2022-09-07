use crate::{
    has_side_effects, type_system::Infer, LocalRw, RValue, RcLocal, Traverse, Type, TypeSystem,
};
use itertools::{Either, Itertools};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Table(pub Vec<(RValue, RValue)>);

/*impl Infer for Table {
    fn infer<'a: 'b, 'b>(&'a mut self, system: &mut TypeSystem<'b>) -> Type {
        let elements: BTreeSet<_> = self
            .0
            .iter_mut()
            .map(|(f, v)| (f.clone(), v.infer(system)))
            .collect();
        let elements: BTreeSet<_> = elements
            .iter()
            .filter(|(f, t)| {
                f.is_some() || !elements.iter().any(|(_, x)| t != x && t.is_subtype_of(x))
            })
            .cloned()
            .collect();
        let (elements, fields): (BTreeSet<_>, BTreeMap<_, _>) =
            elements.into_iter().partition_map(|(f, t)| match f {
                None => Either::Left(t),
                Some(f) => Either::Right((f, t)),
            });

        Type::Table {
            indexer: Box::new((
                Type::Any,
                if elements.len() > 1 {
                    Type::Union(elements)
                } else {
                    elements.into_iter().next().unwrap_or(Type::Any)
                },
            )),
            fields,
        }
    }
}*/

impl LocalRw for Table {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.0
            .iter()
            .flat_map(|(k, v)| k.values_read().into_iter().chain(v.values_read()))
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.0
            .iter_mut()
            .flat_map(|(k, v)| k.values_read_mut().into_iter().chain(v.values_read_mut()))
            .collect()
    }
}

impl Traverse for Table {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        self.0.iter_mut().flat_map(|(k, v)| vec![k, v]).collect()
    }

    fn rvalues(&self) -> Vec<&RValue> {
        self.0.iter().flat_map(|(k, v)| vec![k, v]).collect()
    }
}

// TODO
has_side_effects!(Table);

/*impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(key, value)| match key {
                    Some(key) => format!("{} = {}", key, value),
                    None => value.to_string(),
                })
                .join(", ")
        )
    }
}*/

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(k, v)| format!("[{}] = {}", k, v))
                .join(", ")
        )
    }
}
