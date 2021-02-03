// Implement a Pattern Number.
//
// Squares with the same Pn may be combinable into a group.
//
// For Pn::Unpredictable squares to form a group, there should be no squares
// inbetween that have filled the maximum results (4) and established a Pn other
// than Unpredictable.
//
// For Pn::One squares, their RuleStores should be compatible for a union.
// There should be no squares inbetween that are not Pn::One.
// Squares inbetween should have a rule that is a subset of the combined rules.
//
// For Pn::Two squares, their RuleStores should be compatible for a union.
// There should be no squares inbetween that are Pn::Unpredictable.
// There should be no squares inbetween that are Pn::One, with more than one sample,
// as that indicates not-Pn::Two, since order matters.
// Squares inbetween with one sample should have a rule that is a subset of one
// of the combined rules.
// Squares inbetween that are Pn::Two, must be a subset of the combined rules.
//
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
        match self {
            Pn::One {} => match other {
                Pn::One => {
                    return true;
                }
                _ => {}
            },
            Pn::Two {} => match other {
                Pn::Two => {
                    return true;
                }
                _ => {}
            },
            Pn::Unpredictable {} => match other {
                Pn::Unpredictable => {
                    return true;
                }
                _ => {}
            },
        }
        false
    }
}

#[derive(Serialize, Deserialize, Eq, Clone, Copy, Debug)]
pub enum Pn {
    One,           // Only one result for a state
    Two,           // Two predictable results/order for a state
    Unpredictable, // Unpredicable results
}

impl Pn {}
