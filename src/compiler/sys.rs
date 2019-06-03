/// djb2 hashing algorithm
/// http://www.cse.yorku.ca/~oz/hash.html
#[derive(Clone, Copy)]
pub(crate) struct Djb2Hash {
    hash: u64,
}

impl Djb2Hash {
    pub(crate) fn new() -> Djb2Hash {
        Djb2Hash { hash: 5381 }
    }
}

impl std::hash::Hasher for Djb2Hash {
    fn write(&mut self, bytes: &[u8]) {
        self.hash = bytes
            .iter()
            .map(|c| u64::from(*c))
            .fold(self.hash, |hash, c| {
                (hash << 5).wrapping_add(hash).wrapping_add(c)
            })
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}
