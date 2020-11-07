// Step struct for an Unorthodox Expert System

//use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
//use crate::group::SomeGroup;
use std::fmt;

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = self.str2();

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub struct SomeStep {
    pub initial: SomeRegion,
    pub act_num: usize,
    pub result: SomeRegion,
    pub rule: SomeRule,
    pub alt_rule: Option<SomeRule>,
}

impl SomeStep {
    // Return a new Step struct instance
    pub fn new(act_num: usize, rule: SomeRule, alt_rule: Option<SomeRule>) -> Self {
        let initial = rule.initial_region();
        let result = rule.result_region();
        Self {
            initial,
            act_num,
            result,
            rule,
            alt_rule,
        }
    }

    pub fn clone(&self) -> Self {
        if let Some(alt_rule) = &self.alt_rule {
            Self {
                initial: self.initial.clone(),
                act_num: self.act_num,
                result: self.result.clone(),
                rule: self.rule.clone(),
                alt_rule: Some(alt_rule.clone()),
            }
        } else {
            Self {
                initial: self.initial.clone(),
                act_num: self.act_num,
                result: self.result.clone(),
                rule: self.rule.clone(),
                alt_rule: None,
            }
        }
    }

    // Return a new step, by taking a given step and restricting the initial region
    // Keep the rule the same, so steps with the same rules can be identified.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        //let rulx = self.rule.restrict_initial_region(reg);

        let init_reg = self.initial.intersection(&reg);

        if let Some(alt_rule) = &self.alt_rule {
            Self {
                initial: init_reg.clone(),
                act_num: self.act_num,
                result: self.rule.result_from_initial(&init_reg),
                rule: self.rule.clone(),
                alt_rule: Some(alt_rule.clone()),
            }
        } else {
            Self {
                initial: init_reg.clone(),
                act_num: self.act_num,
                result: self.rule.result_from_initial(&init_reg),
                rule: self.rule.clone(),
                alt_rule: None,
            }
        }
    }

    // Return a new step, by taking a given step and restricting the result region
    // Keep the rule the same, so steps with the same rules can be identified.
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        //let rulx = self.rule.restrict_result_region(reg);

        let rslt_reg = self.result.intersection(&reg);

        if let Some(alt_rule) = &self.alt_rule {
            Self {
                initial: self.rule.initial_from_result(&rslt_reg),
                act_num: self.act_num,
                result: rslt_reg,
                rule: self.rule.clone(),
                alt_rule: Some(alt_rule.clone()),
            }
        } else {
            Self {
                initial: self.rule.initial_from_result(&rslt_reg),
                act_num: self.act_num,
                result: rslt_reg,
                rule: self.rule.clone(),
                alt_rule: None,
            }
        }
    }

    // Return a string representing a step
    fn str2(&self) -> String {
        let mut rcstr = String::from("[");
        rcstr.push_str(&format!("{}", self.initial));
        rcstr.push_str(&format!(" -{}> ", self.act_num));
        rcstr.push_str(&self.result.str_not_x(&self.rule.b01.m_or(&self.rule.b10)));
        rcstr.push_str("]");
        rcstr
    }

    // Return a terse string representing a step
    //    pub fn str_terse(&self) -> String {
    //        let mut rcstr = String::from(" ");
    //        rcstr.push_str(&format!("{}", self.initial));
    //        rcstr.push_str(&format!(" -{}> ", self.act_num));
    //        rcstr.push_str(&self.result.str_not_x(&self.rule.b01.m_or(&self.rule.b10)));
    //        rcstr.push(' ');
    //        rcstr
    //    }
}
