// Step struct for an Unorthodox Expert System

//use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use std::fmt;

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = self.str2();

        write!(f, "{}", rc_str)
    }
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug)]
pub struct SomeStep {
    pub initial: SomeRegion,
    pub act_num: usize,
    pub result: SomeRegion,
    pub hint: usize,
    pub rule: SomeRule,
    //    pub mark: usize,
}

//impl Copy for SomeStep {}

//impl Clone for SomeStep {
//    fn clone(&self) -> Self {
//        Self {
//            initial: self.initial,
//            act_num: self.act_num,
//            result: self.result,
//            hint: self.hint,
//            rule: self.rule,
//        }
//    }
//}

impl SomeStep {
    // Return a new Step struct instance
    pub fn new(act_num: usize, hint: usize, rule: SomeRule) -> Self {
        //let msk = rule.b01.m_or(&rule.b10);
        let initial = rule.initial_region();
        let result = rule.result_region();
        Self {
            initial,
            act_num,
            result,
            hint,
            rule,
            //mark: 0,
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            initial: self.initial.clone(),
            act_num: self.act_num,
            result: self.result.clone(),
            hint: self.hint,
            rule: self.rule.clone(),
        }
    }

    // Return a new step, by taking a given step and restricting the initial region
    // Keep the rule the same, so steps with the same rules can be identified.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        //let rulx = self.rule.restrict_initial_region(reg);

        let init_reg = self.initial.intersection(&reg);

        Self {
            initial: init_reg.clone(),
            act_num: self.act_num,
            result: self.rule.result_from_initial(&init_reg),
            hint: self.hint,
            rule: self.rule.clone(),
        }
    }

    // Return a new step, by taking a given step and restricting the result region
    // Keep the rule the same, so steps with the same rules can be identified.
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        //let rulx = self.rule.restrict_result_region(reg);

        let rslt_reg = self.result.intersection(&reg);

        Self {
            initial: self.rule.initial_from_result(&rslt_reg),
            act_num: self.act_num,
            result: rslt_reg,
            hint: self.hint,
            rule: self.rule.clone(),
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
