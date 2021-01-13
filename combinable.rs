// Implement a Bool-like enum signifying the ability for two squares to be combined.
// Allows True, False, and two additional states

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
    True,
    False,
    MoreSamplesNeeded,
}

impl PartialEq for Combinable {
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
