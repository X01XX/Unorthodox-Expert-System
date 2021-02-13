//! The Combinable enum, representing the combinability of two SomeSquare structs.
//!
//! The allowable values are True, False, and MoreSamplesNeeded.
//!
//! Kind of like Boolean True/False, plus Maybe.
//!
use std::fmt;

impl fmt::Display for Combinable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = match self {
            Combinable::True => String::from("True"),
            Combinable::False => String::from("False"),
            Combinable::MoreSamplesNeeded => String::from("MoreSamplesNeeded"),
        };

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub enum Combinable {
    /// Boolean True.
    True,
    /// Boolean False.
    False,
    /// Non-Boolean "Maybe".
    MoreSamplesNeeded,
}

impl PartialEq for Combinable {
    /// Return true if two Combinable enums are equal.
    fn eq(&self, other: &Self) -> bool {
        match self {
            Combinable::True => match other {
                Combinable::True => true,
                _ => false,
            },
            Combinable::False => match other {
                Combinable::False => true,
                _ => false,
            },
            Combinable::MoreSamplesNeeded => match other {
                Combinable::MoreSamplesNeeded => true,
                _ => false,
            },
        } // end match
    }
}
impl Eq for Combinable {}

impl Combinable {}
