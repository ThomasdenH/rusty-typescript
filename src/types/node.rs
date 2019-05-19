use super::text_range::TextRange;
use crate::types::node_array::NodeArray;
use crate::types::node_flags::NodeFlags;
use std::ops::Deref;

pub struct Node {
    text_range: TextRange,
    flags: NodeFlags,
    decorators: NodeArray<Decorator>,
}

pub struct Decorator {
    node: Node,
}

impl Deref for Decorator {
    type Target = Node;

    fn deref(&self) -> &Node {
        &self.node
    }
}
