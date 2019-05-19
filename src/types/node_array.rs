use crate::types::node::Node;
use crate::types::text_range::TextRange;
use std::ops::Deref;

pub struct NodeArray<T: Deref<Target = Node>> {
    nodes: Vec<T>,
    has_trailing_comma: bool,
    text_range: TextRange,
}
