//! A Sample is a state, an applied action, and a result state.

use crate::rule::SomeRule;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use unicode_segmentation::UnicodeSegmentation;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SomeSample {
    // Initial state of the sample.
    pub initial: SomeState,
    // State after running an action.
    pub result: SomeState,
}

impl fmt::Display for SomeSample {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeSample {
    pub fn new(initial: SomeState, result: SomeState) -> Self {
        debug_assert_eq!(initial.num_bits(), result.num_bits());

        Self { initial, result }
    }

    /// Create a rule from a sample.
    pub fn rule(&self) -> SomeRule {
        SomeRule::new(self)
    }

    /// Return a string to represent a SomeSample instance.
    fn formatted_string(&self) -> String {
        format!("{} -> {}", self.initial, self.result)
    }

    /// Return the number bits used in sample elements.
    pub fn num_bits(&self) -> usize {
        self.initial.num_bits()
    }

    /// Return a sample from a string.
    /// Like "0101->1010".
    pub fn from(s_str: &str) -> Result<Self, String> {
        let mut initial_str = String::new();
        let mut result_str = String::new();

        let mut dash_flag = false;
        let mut arrow_flag = false;

        for chr in s_str.graphemes(true) {
            if chr == " " {
                continue;
            }
            if chr == "-" {
                if dash_flag || arrow_flag {
                    return Err(format!("SomeSample::from: Did not understand {s_str}"));
                }
                dash_flag = true;
                continue;
            }
            if chr == ">" {
                if !dash_flag || arrow_flag {
                    return Err(format!("SomeSample::from: Did not understand {s_str}"));
                }
                arrow_flag = true;
                continue;
            }
            if arrow_flag {
                result_str.push_str(chr);
            } else {
                initial_str.push_str(chr)
            }
        }

        if !dash_flag || !arrow_flag {
            return Err(format!("SomeSample::from: Did not understand {s_str}"));
        }

        if let Ok(initial_sta) = SomeState::from(&initial_str) {
            if let Ok(result_sta) = SomeState::from(&result_str) {
                if initial_sta.num_bits() == result_sta.num_bits() {
                    return Ok(Self::new(initial_sta, result_sta));
                } else {
                    return Err(format!("SomeSample::from: Number bits of initial and result do not match in {s_str}"));
                }
            } else {
                return Err(format!("SomeSample::from: Did not understand result state in {s_str}"));
            }
        }
        Err(format!("SomeSample::from: Did not understand initial state in {s_str}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from() -> Result<(), String> {
        match SomeSample::from("0101->1010") {
            Ok(asample) => {
                assert!(asample.initial == SomeState::from("0101")?);
                assert!(asample.result == SomeState::from("1010")?);
            }
            Err(errstr) => {
                return Err(errstr);
            }
        }

        match SomeSample::from("01010->1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Number bits of initial and result do not match in 01010->1010".to_string());
            }
        }

        match SomeSample::from("0101x->1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand initial state in 0101x->1010".to_string());
            }
        }

        match SomeSample::from("0101->1x10") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand result state in 0101->1x10".to_string());
            }
        }

        match SomeSample::from("0101-1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand 0101-1010".to_string());
            }
        }

        match SomeSample::from("0101-->1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand 0101-->1010".to_string());
            }
        }

        match SomeSample::from("0101>1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand 0101>1010".to_string());
            }
        }

        match SomeSample::from("0101>>1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand 0101>>1010".to_string());
            }
        }

        match SomeSample::from("0101>-1010") {
            Ok(_asample) => {
                return Err("SNH".to_string());
            }
            Err(errstr) => {
                println!("{errstr}");
                assert!(errstr == "SomeSample::from: Did not understand 0101>-1010".to_string());
            }
        }

        Ok(())
    }
}
