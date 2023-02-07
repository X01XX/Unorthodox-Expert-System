//! The Truth enum.
//!
//! The allowable values are T (True), F (False), and M (Maybe, or More Samples Needed).
//!
//! Kind of like Boolean True/False, plus Maybe.

//use std::fmt;

//impl fmt::Display for Truth {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        let rc_str = self.formatted_string();

//        write!(f, "{rc_str}")
//    }
//}

#[derive(Debug, PartialEq, Eq)]
pub enum Truth {
    /// Boolean True.
    T,
    /// Boolean False.
    F,
    /// Non-Boolean "Maybe, or More Samples Needed".
    M,
}

impl Truth {
    /// Return a string for a Truth value.
    pub fn formatted_string(&self) -> &str {
        match self {
            Truth::T => "T",
            Truth::F => "F",
            Truth::M => "M",
        }
    }
}
