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

impl Truth {
    /// Return a string for a Truth value.
    pub fn formatted_string(&self) -> String {
        let rc_str = match self {
            Truth::T => String::from("T"),
            Truth::F => String::from("F"),
            Truth::M => String::from("M"),
        };
        rc_str
    }
}

#[cfg(test)]
mod tests {
    use crate::truth::Truth;
    use std::cmp::Ordering;

    // Check ordering of Truth values.
    // Should be F < M < T
    #[test]
    fn truth_comparisons() -> Result<(), String> {
        assert!(Truth::T.partial_cmp(&Truth::T).unwrap() == Ordering::Equal);
        assert!(Truth::T.partial_cmp(&Truth::M).unwrap() == Ordering::Greater);
        assert!(Truth::T.partial_cmp(&Truth::F).unwrap() == Ordering::Greater);

        assert!(Truth::M.partial_cmp(&Truth::T).unwrap() == Ordering::Less);
        assert!(Truth::M.partial_cmp(&Truth::M).unwrap() == Ordering::Equal);
        assert!(Truth::M.partial_cmp(&Truth::F).unwrap() == Ordering::Greater);

        assert!(Truth::F.partial_cmp(&Truth::T).unwrap() == Ordering::Less);
        assert!(Truth::F.partial_cmp(&Truth::M).unwrap() == Ordering::Less);
        assert!(Truth::F.partial_cmp(&Truth::F).unwrap() == Ordering::Equal);

        Ok(())
    }
}
