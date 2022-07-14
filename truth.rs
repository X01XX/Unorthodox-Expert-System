//! The Truth enum.
//!
//! The allowable values are T (True), F (False), and M (Maybe, or More Samples Needed).
//!
//! Kind of like Boolean True/False, plus Maybe.

use std::cmp::Ordering;
use std::fmt;

impl fmt::Display for Truth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = self.formatted_string();

        write!(f, "{}", rc_str)
    }
}

impl PartialOrd for Truth {
    fn partial_cmp(&self, other: &Truth) -> Option<Ordering> {
        match self {
            Truth::T {} => match other {
                Truth::T => {
                    return Some(Ordering::Equal);
                }
                _ => {
                    return Some(Ordering::Greater);
                }
            },
            Truth::M {} => match other {
                Truth::T => {
                    return Some(Ordering::Less);
                }
                Truth::M => {
                    return Some(Ordering::Equal);
                }
                Truth::F => {
                    return Some(Ordering::Greater);
                }
            },
            Truth::F {} => match other {
                Truth::F => {
                    return Some(Ordering::Equal);
                }
                _ => {
                    return Some(Ordering::Less);
                }
            },
        }
    }
}

#[derive(Debug, PartialEq)]
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
    pub fn formatted_string(&self) -> String {
        match self {
            Truth::T => String::from("T"),
            Truth::F => String::from("F"),
            Truth::M => String::from("M"),
        }
    }
}
