//! The Truth enum.
//!
//! The allowable values are T (True), F (False), and M (Maybe, or More Samples Needed).
//!
//! Kind of like Boolean True/False, plus Maybe.
//!
use std::fmt;

impl fmt::Display for Truth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = match self {
            Truth::T => String::from("T"),
            Truth::F => String::from("F"),
            Truth::M => String::from("M"),
        };

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub enum Truth {
    /// Boolean True.
    T,
    /// Boolean False.
    F,
    /// Non-Boolean "Maybe, or More Samples Needed".
    M,
}

impl PartialEq for Truth {
    /// Return true if two Truth enums are equal.
    fn eq(&self, other: &Self) -> bool {
        match self {
            Truth::T => match other {
                Truth::T => true,
                _ => false,
            },
            Truth::F => match other {
                Truth::F => true,
                _ => false,
            },
            Truth::M => match other {
                Truth::M => true,
                _ => false,
            },
        } // end match
    }
}
impl Eq for Truth {}

impl Truth {}
