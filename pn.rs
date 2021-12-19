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

impl fmt::Display for Pn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = match self {
            Pn::One => String::from("1"),
            Pn::Two => String::from("2"),
            Pn::Unpredictable => String::from("U"),
        };

        write!(f, "{}", rc_str)
    }
}

impl PartialOrd for Pn {
    fn partial_cmp(&self, other: &Pn) -> Option<Ordering> {
        match self {
            Pn::One {} => match other {
                Pn::One => {
                    return Some(Ordering::Equal);
                }
                _ => {
                    return Some(Ordering::Less);
                }
            },
            Pn::Two {} => match other {
                Pn::One => {
                    return Some(Ordering::Greater);
                }
                Pn::Two => {
                    return Some(Ordering::Equal);
                }
                Pn::Unpredictable => {
                    return Some(Ordering::Less);
                }
            },
            Pn::Unpredictable {} => match other {
                Pn::Unpredictable => {
                    return Some(Ordering::Equal);
                }
                _ => {
                    return Some(Ordering::Greater);
                }
            },
        }
    }
}

impl PartialEq for Pn {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pn::One, Pn::One) => { return true; }
            (Pn::Two, Pn::Two) => { return true; }
            (Pn::Unpredictable, Pn::Unpredictable) => { return true; }
            _ => { return false; }
        }
    }
}

#[derive(Serialize, Deserialize, Eq, Clone, Copy, Debug)]
pub enum Pn {
    /// Only one result for a state.
    One,
    /// Two predictable results (order matters) for a state.
    Two,
    /// Unpredicable results.
    Unpredictable,
}

impl Pn {}

#[cfg(test)]
mod tests {
    use crate::pn::Pn;
    use std::cmp::Ordering;
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
