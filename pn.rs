//! The Pn enum. Pn stands for Pattern Number, with values of One, Two (order matters) or Unpredictable.
//!
//! Squares with the same Pn value may be combinable into a group.
//!
//! For Pn::One squares, their RuleStores should be compatible for a union.
//! There should be no squares inbetween that are not Pn::One.
//! Squares inbetween should have a rule that is a subset of the combined rules.
//!
//! For Pn::Two squares, their RuleStores should be compatible for a union.
//! There should be no squares inbetween that are Pn::Unpredictable.
//! There should be no squares inbetween that are Pn::One, with more than one sample,
//! as that indicates not-Pn::Two, since order matters.
//! Squares inbetween with one sample should have a rule that is a subset of one
//! of the combined rules.
//! Squares inbetween that are Pn::Two, must have rules that are a subset of the combined rules.
//!
//! For Pn::Unpredictable squares, there should be no squares
//! inbetween that have a pnc (pattern number confirmed) = true,
//! with a Pn other than Unpredictable.
//!
//! Repeated sampling can cause the Pn value to change back and forth, its contingent.
//! A ball drops when you let go of it, until is doesn't.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;

/// Implement the fmt::Display trait for a Pn.
impl fmt::Display for Pn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

/// Implement the PartialOrd trait for two Pn instances.
/// When getting more samples for a state, the Pn value may cycle,
/// at first from One, to Two, to Unpredictable.  So that order is
/// implemented here.
impl PartialOrd for Pn {
    fn partial_cmp(&self, other: &Pn) -> Option<Ordering> {
        match self {
            Pn::One {} => match other {
                Pn::One => Some(Ordering::Equal),
                _ => Some(Ordering::Less),
            },
            Pn::Two {} => match other {
                Pn::One => Some(Ordering::Greater),
                Pn::Two => Some(Ordering::Equal),
                Pn::Unpredictable => Some(Ordering::Less),
            },
            Pn::Unpredictable {} => match other {
                Pn::Unpredictable => Some(Ordering::Equal),
                _ => Some(Ordering::Greater),
            },
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Pn {
    /// Only one result for a state.
    One,
    /// Two predictable results (order matters) for a state.
    Two,
    /// No pattern number, unpredictable.
    Unpredictable,
}

impl Pn {
    /// Return a String representation of a Pn.
    pub fn formatted_string(&self) -> String {
        match self {
            Pn::One => String::from("1"),
            Pn::Two => String::from("2"),
            Pn::Unpredictable => String::from("U"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Check ordering of Pn values.
    // Should be One < Two < Unpredictable
    #[test]
    fn pn_comparisons() -> Result<(), String> {
        assert!(Pn::Unpredictable.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Equal);
        assert!(Pn::Unpredictable.partial_cmp(&Pn::Two).unwrap() == Ordering::Greater);
        assert!(Pn::Unpredictable.partial_cmp(&Pn::One).unwrap() == Ordering::Greater);

        assert!(Pn::Two.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Less);
        assert!(Pn::Two.partial_cmp(&Pn::Two).unwrap() == Ordering::Equal);
        assert!(Pn::Two.partial_cmp(&Pn::One).unwrap() == Ordering::Greater);

        assert!(Pn::One.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Less);
        assert!(Pn::One.partial_cmp(&Pn::Two).unwrap() == Ordering::Less);
        assert!(Pn::One.partial_cmp(&Pn::One).unwrap() == Ordering::Equal);

        Ok(())
    }
}
