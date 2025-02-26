use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopedName {
    components: Vec<String>,
}

impl ScopedName {
    pub fn new(components: Vec<String>) -> Self {
        assert!(!components.is_empty());
        Self { components }
    }

    // Turn a::b::c into (a, b::c)
    pub fn split_root(&self) -> (&str, ScopedName) {
        let (root, rest) = self.components.split_at(1);
        (
            root[0].as_str(),
            ScopedName::new(rest.iter().map(|s| s.clone()).collect()),
        )
    }

    pub fn last(&self) -> &str {
        self.components.last().unwrap()
    }

    pub fn leaf(&self) -> Option<&str> {
        if self.components.len() == 1 {
            Some(&self.components[0])
        } else {
            None
        }
    }
}

impl Display for ScopedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.components.join("::"))
    }
}
