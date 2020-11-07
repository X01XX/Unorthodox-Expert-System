// Implement a Pattern Number,

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

#[derive(Eq, Clone, Copy, Debug)]
pub enum Pn {
    One,           // Only one result for a state
    Two,           // Two predictable results/order for a state
    Unpredictable, // Unpredicable results
}

impl Pn {}
