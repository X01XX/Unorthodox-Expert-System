//! The AltRuleHint enum, used in the SomeStep struct.

use crate::rule::SomeRule;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub enum AltRuleHint {
    NoAlt {},
    AltNoChange {},
    AltRule { rule: SomeRule },
}

impl fmt::Display for AltRuleHint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::NoAlt {} => String::from("Alt: None"),
            Self::AltNoChange {} => String::from("Alt: NoChange"),
            Self::AltRule { rule } => format!("Alt: {rule}"),
        };
        write!(f, "{}", str)
    }
}

impl StrLen for AltRuleHint {
    fn strlen(&self) -> usize {
        match self {
            Self::NoAlt {} => 9,
            Self::AltNoChange {} => 13,
            Self::AltRule { rule } => 5 + rule.strlen(),
        }
    }
}

impl FromStr for AltRuleHint {
    type Err = String;
    /// Return a AltRuleHint instance, given a string representation.
    ///
    /// Like: Alt: None
    ///       Alt: No change
    ///       Alt: XX/01/10
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("AltRuleHint::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 5 || str_in2[0..5].to_lowercase() != *"alt: " {
            return Err(
                "AltRuleHint::from_str: string should be at least = Alt:<space>".to_string(),
            );
        }

        let mut token = &str_in2[5..];
        token = token.trim();
        if token.to_lowercase() == "none" {
            Ok(AltRuleHint::NoAlt {})
        } else if token.to_lowercase() == "nochange" {
            Ok(AltRuleHint::AltNoChange {})
        } else {
            Ok(AltRuleHint::AltRule {
                rule: SomeRule::from_str(token)?,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_hint1 = AltRuleHint::from_str("Alt: None")?;
        println!("tmp_hint1 {tmp_hint1}");
        assert!(tmp_hint1.strlen() == 9);

        let tmp_hint2 = AltRuleHint::from_str("Alt: NoChange")?;
        println!("tmp_hint2 {tmp_hint2}");
        assert!(tmp_hint2.strlen() == 13);

        let tmp_hint3 = AltRuleHint::from_str("Alt: XX/01/10")?;
        println!("tmp_hint3 {tmp_hint3}");
        assert!(tmp_hint3.strlen() == 13);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let tmp_hint1_str = "Alt: None";
        let tmp_hint1 = AltRuleHint::from_str(&tmp_hint1_str)?;
        println!("tmp_hint1 {tmp_hint1}");
        assert!(format!("{tmp_hint1}") == tmp_hint1_str);

        let tmp_hint2_str = "Alt: NoChange";
        let tmp_hint2 = AltRuleHint::from_str(&tmp_hint2_str)?;
        println!("tmp_hint2 {tmp_hint2}");
        assert!(format!("{tmp_hint2}") == tmp_hint2_str);

        let tmp_hint3_str = "Alt: XX/01/10";
        let tmp_hint3 = AltRuleHint::from_str(&tmp_hint3_str)?;
        println!("tmp_hint3 {tmp_hint3}");
        assert!(format!("{tmp_hint3}") == tmp_hint3_str);

        Ok(())
    }
}
