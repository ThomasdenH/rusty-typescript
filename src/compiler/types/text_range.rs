use serde::{Deserialize, Serialize};

#[derive(Eq, PartialEq, Debug, Copy, Clone, Deserialize, Serialize)]
pub struct TextRange {
    pos: Option<usize>,
    end: Option<usize>,
}

impl TextRange {
    pub fn from_pos_and_end(pos: Option<usize>, end: Option<usize>) -> TextRange {
        if let (Some(pos), Some(end)) = (pos, end) {
            assert!(end >= pos);
        }
        TextRange { pos, end }
    }

    pub fn is_collapsed(&self) -> bool {
        if let (Some(pos), Some(end)) = (self.pos, self.end) {
            pos == end
        } else {
            false
        }
    }
}
