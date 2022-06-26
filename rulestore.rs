//! The RuleStore struct, a vector of SomeRule structs.
//!
//! The vector is sometimes empty, or has one or two rules.
//!
//! If two rules, they cannot be combined due to a 0->X or 1->X difference.
//!
//! If a square has two alternating results, since the square has a bit pattern with
//! no X positions, the different results must different by one, or more,
//! 1->X or 0->X bit positions.

use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::truth::Truth;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RuleStore {
    avec: Vec<SomeRule>,
}
impl fmt::Display for RuleStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for RuleStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        if self.len() == 0 {
            return true;
        }

        if self.len() == 1 {
            return self.avec[0] == other.avec[0];
        }

        // For two rules, order does not matter
        if self.len() == 2 {
            if let Some(rulesx) = self.intersection(&other) {
                if rulesx.initial_region() == self.initial_region() {
                    return true;
                }
            }

            return false;
        }

        panic!("Unsupported RuleStore length {}", self.len());
    }
}

impl Eq for RuleStore {}

impl RuleStore {
    /// Return a new RuleStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRule>::with_capacity(2),
        }
    }

    /// Return if a square result rulestore is valid
    pub fn is_valid(&self) -> bool {
        if self.len() == 0 {
            return true;
        }

        if self.len() == 1 {
            return self.avec[0].is_valid();
        } // single rule is valid, or not

        if self.len() > 2 {
            return false;
        }

        // Length must be 2.  The two rules are different and the initial regions are the same.
        self.avec[0].is_valid()
            && self.avec[1].is_valid()
            && self.avec[0] != self.avec[1]
            && self.avec[0].initial_region() == self.avec[1].initial_region()
    }

    /// Return the length of a RuleStore.
    /// Should be 0, 1 or 2.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a rule to a RuleStore.
    /// If there are two rules, they will have at least one incompatibility,
    /// 0->0/0->1 or 1->1/1->0, and have equal initial regions.
    pub fn push(&mut self, val: SomeRule) {
        assert!(self.avec.len() < 2);

        self.avec.push(val);
        assert!(self.is_valid());
    }

    /// Return true if one RuleStore is a subset of another.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.len() > other.len() {
            return false;
        }

        if self.initial_region().is_subset_of(&other.initial_region()) == false {
            return false;
        }

        if self.len() == 1 {
            if other.len() == 1 {
                return self[0].is_subset_of(&other[0]);
            } else if other.len() == 2 {
                return self[0].is_subset_of(&other[0]) || self[0].is_subset_of(&other[1]);
            }
        }

        if self.len() == 2 {
            if self[0].is_subset_of(&other[0]) && self[1].is_subset_of(&other[1]) {
                return true;
            }

            if self[0].is_subset_of(&other[1]) && self[1].is_subset_of(&other[0]) {
                return true;
            }

            return false;
        }

        panic!("unexpected rulestore length!");
    }

    /// Return true if a RuleStore is a superset of another.
    pub fn is_superset_of(&self, other: &RuleStore) -> bool {
        other.is_subset_of(self)
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        if self.len() == 1 {
            return self[0].is_superset_of(other);
        }

        if self.len() == 2 {
            return self[0].is_superset_of(other) || self[1].is_superset_of(other);
        }

        panic!("unexpected rulestore length!");
    }

    /// Return the union of two RuleStores.
    /// May return None, if the union is invalid.
    ///
    /// If the rules will are from two squares, the possible combinations
    /// for each bit position are:
    ///
    /// 0->0, 0->0 = 0->0
    /// 0->0, 0->1 = 0->X, disallowed
    /// 0->0, 1->1 = X->X
    /// 0->0, 1->0 = X->0
    ///
    /// 0->1, 0->1 = 0->1
    /// 0->1, 1->1 = X->1
    /// 0->1, 1->0 = X->x (X to x-not)
    ///
    /// 1->1, 1->1 = 1->1
    /// 1->1, 1->0 = 1->X, disallowed
    ///
    /// 1->0, 1->0 = 1->0
    ///
    /// For two result rules,
    /// They may not be compatible.
    /// They may be compatible in one way,
    /// They may appear to be compatible in two ways, which indicates a problem, so they will be
    /// considered to be incompatible.  Being compatible in two ways will produce either a
    /// (X->1 or X->0) in a bit position of one union, an a (X->x or X->X) in the corresponding position
    /// of another union.
    /// For (X->1, X->x), 1->?
    ///     (X->0, X->x), 0->?
    ///     (X->1, X->X), 0->?
    ///     (X->0, X->X), 1->?
    pub fn union(&self, other: &Self) -> Option<Self> {
        //println!("\nrulestore union {} and {}", &self, &other);
        if self.len() != other.len() {
            return None;
        }

        if self.len() == 0 {
            //return Some(Self::new());
            panic!("Unpredictable union not allowed");
        }

        let regx = self.initial_region().union(&other.initial_region());

        let mut ars = Self::new();

        if self.len() == 1 {
            let rulx = self.avec[0].union(&other.avec[0]);

            if rulx.is_valid_union() {
                if rulx.initial_region() == regx {
                    ars.push(rulx);
                    return Some(ars);
                }
            }
            return None;
        }

        if self.len() == 2 {
            //assert!(self.initial_region().x_mask().is_low());
            //assert!(other.initial_region().x_mask().is_low());

            let mut ordera = false;

            let rul0 = self.avec[0].union(&other.avec[0]);
            let rul1 = self.avec[1].union(&other.avec[1]);

            if rul0.is_valid_union()
                && rul1.is_valid_union()
                && rul0.initial_region() == rul1.initial_region()
            {
                ordera = true;
            }

            let mut orderb = false;

            let rul2 = self.avec[0].union(&other.avec[1]);
            let rul3 = self.avec[1].union(&other.avec[0]);

            if rul2.is_valid_union()
                && rul3.is_valid_union()
                && rul2.initial_region() == rul3.initial_region()
            {
                orderb = true;
            }

            // For any two-result state, there must be a 01 and 00, alternating change bit,
            // or a 10 and 11 alternating change bit.
            //
            // The predictive power is in the alternation, one sample after another for a specific bit position and value.
            //
            // Making a union of the two different bits (other bit positions being compatible) makes either (XX, Xx) or (X0, X1),
            // which presents problems for future unions, intersections and equality tests.
            //
            // You could choose only one of the two options, I prefer (XX, Xx), but if there are two such alternating positions,
            // you may have to decide between a wrong choice, and a wrong choice.
            //
            // Disallowing this kind of union can be seen as preventing an X initial bit in that position.
            if ordera && orderb {
                return None;
            }

            let mut ret_store = Self::new();
            if ordera {
                ret_store.push(rul0);
                ret_store.push(rul1);
                return Some(ret_store);
            }

            if orderb {
                ret_store.push(rul2);
                ret_store.push(rul3);
                return Some(ret_store);
            }

            return None;
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
    }

    /// Return Truth value of a possible union.
    pub fn can_form_union(&self, other: &Self) -> Truth {
        assert!(self.len() > 0);
        assert!(other.len() > 0);

        // Handle Pn1 vs. Pn2.
        // The Pn1 type should not have enough samples to be pnc.
        if self.len() < other.len() {
            for rulx in other.iter() {
                if rulx.union(&self[0]).is_valid_union() {
                    return Truth::M;
                }
            }
            return Truth::F;
        }

        if other.len() < self.len() {
            for rulx in self.iter() {
                if rulx.union(&other[0]).is_valid_union() {
                    return Truth::M;
                }
            }
            return Truth::F;
        }

        if let Some(_ruls) = self.union(other) {
            //println!("can_form_union: 1 returning T {}", &ruls);
            return Truth::T;
        }
        //println!("can_form_union: 2 returning F");
        Truth::F
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.avec.iter()
    }

    /// Return intersection of two RuleStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if self.len() != other.len() {
            panic!("rulestore lengths not eq!");
        }

        if self.len() == 1 {
            let mut ars = Self::new();

            let int1 = self[0].intersection(&other[0]);
            if int1.is_valid_intersection() {
                ars.push(int1);
                return Some(ars);
            }
            return None;
        }

        if self.len() == 2 {
            // Intersect by order1
            let mut order1 = true;

            let int00 = self[0].intersection(&other[0]);
            let int11 = self[1].intersection(&other[1]);

            if int00.is_valid_intersection() == false {
                order1 = false;
            } else {
                if int11.is_valid_intersection() == false {
                    order1 = false;
                } else if int00.initial_region() != int11.initial_region() {
                    order1 = false;
                }
            }

            // Intersect by order2
            let mut order2 = true;

            let int01 = self[0].intersection(&other[1]);
            let int10 = self[1].intersection(&other[0]);

            if int01.is_valid_intersection() == false {
                order2 = false;
            } else {
                if int10.is_valid_intersection() == false {
                    order2 = false;
                } else if int01.initial_region() != int10.initial_region() {
                    order2 = false;
                }
            }

            // Act on results of intersections
            if order1 && order2 {
                let mut ord1 = Self::new();
                ord1.push(int00);
                ord1.push(int11);

                let mut ord2 = Self::new();
                ord2.push(int01);
                ord2.push(int10);

                let ord12 = ord1.union(&ord2).unwrap();
                //println!("pn3 intersection of {} and {} is12 {}", self, other, ord2);
                return Some(ord12);
            }

            if order1 {
                let mut ord1 = Self::new();
                ord1.push(int00);
                ord1.push(int11);
                //println!("pn3 intersection of {} and {} is1 {}", self, other, ord1);
                return Some(ord1);
            }

            if order2 {
                let mut ord2 = Self::new();
                ord2.push(int01);
                ord2.push(int10);
                //println!("pn3 intersection of {} and {} is2 {}", self, other, ord2);
                return Some(ord2);
            }
            //println!("pn3 intersection of {} and {} failed", self, other);
            return None;
        }
        panic!("not ready for pn {}!", self.len());
    }

    /// Return the result of restricting the initial region of rules.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        assert!(self.len() > 0);
        assert!(regx.intersects(&self.initial_region()));
        let mut rcrs = Self::new();

        for rulx in self.avec.iter() {
            rcrs.push(rulx.restrict_initial_region(&regx));
        }
        rcrs
    }

    /// Return the initial region of the first rule in the store.
    pub fn initial_region(&self) -> SomeRegion {
        self.avec[0].initial_region()
    }

    /// Return the expected length of a string representing the store.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 3;

        if self.avec.len() > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing the store.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());

        rc_str.push_str("R[");
        for strx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &strx));
            flg = 1;
        }
        rc_str.push_str("]");

        rc_str
    }
} // end impl RuleStore

impl Index<usize> for RuleStore {
    type Output = SomeRule;
    fn index<'a>(&'a self, i: usize) -> &'a SomeRule {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test restrict_initial_region and initial_region
    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/11/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/11/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            println!("test_restrict_initial_region 1 union worked {}", &rul_str3);
            let regx = SomeRegion::new_from_string(1, "r101").unwrap();
            let rul_str4 = rul_str3.restrict_initial_region(&regx);
            println!("test_restrict_initial_region 2 worked is {}", &rul_str4);
            if rul_str4.initial_region() != regx {
                return Err(format!("test_restrict_initial_region 1 element 0 failed?"));
            }
            if rul_str4[1].initial_region() != regx {
                return Err(format!("test_restrict_initial_region 1 element 1 failed?"));
            }
        } else {
            return Err(format!("test_restrict_initial_region 1 union failed?"));
        }
        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Intersect two single-rule RuleStores, it should work.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            println!("test_intersection 1 worked = {}", &rul_str3);
            let mut rul_str4 = RuleStore::new();
            rul_str4.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());
            if rul_str3 != rul_str4 {
                return Err(format!(
                    "test_intersection 1 {} ne {}",
                    &rul_str3, &rul_str4
                ));
            }
        } else {
            return Err(format!("test_intersection 1 failed?"));
        }

        // Produce failure due to bit 3, Xx intersect 11 = null.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/Xx/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            return Err(format!("test_intersection 2 worked = {}?", &rul_str3));
        }

        // Intersect two two-rule RuleStores, it should work.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            println!("test_intersection 3 worked = {}", &rul_str3);
            let rulx = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();
            if rul_str3[0] != rulx && rul_str3[1] != rulx {
                return Err(format!("test_intersection 3 00/11/11/10/00 not in result?"));
            }
            let rulx = SomeRule::new_from_string(1, "01/11/11/10/00").unwrap();
            if rul_str3[0] != rulx && rul_str3[1] != rulx {
                return Err(format!("test_intersection 3 01/11/11/10/00 not in result?"));
            }
        } else {
            return Err(format!("test_intersection 3 failed?"));
        }

        // Intersect two two-rule RuleStores, it should not work, due to the left-most bit.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/11/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            return Err(format!("test_intersection 4 worked = {}", &rul_str3));
        }

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        // Compare one-rule RuleStores.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
            println!("test_is_subset_of 1 worked");
        } else {
            return Err(format!("test_is_subset_of 1 failed?"));
        }

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/10/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
            return Err(format!("test_is_subset_of 2 worked?"));
        }

        // Compare two two-rule RuleStores.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "01/11/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
        } else {
            return Err(format!("test_is_subset_of 3 failed?"));
        }

        if rul_str1.is_subset_of(&rul_str2) {
            return Err(format!("test_is_subset_of 4 worked?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of_rule() -> Result<(), String> {
        // Compare a rule to a one-rule RuleStore.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let rul2 = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
            println!("test_is_superset_of_rule 1 worked");
        } else {
            return Err(format!("test_is_superset_of_rule 1 failed?"));
        }

        let rul2 = SomeRule::new_from_string(1, "00/10/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
            return Err(format!("test_is_superset_of_rule 2 worked?"));
        }

        // Compare rule to a two-rule RuleStore.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let rul2 = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
        } else {
            return Err(format!("test_is_superset_of_rule 3 failed?"));
        }

        let rul2 = SomeRule::new_from_string(1, "00/00/11/10/00").unwrap();
        if rul_str1.is_superset_of_rule(&rul2) {
            return Err(format!("test_is_superset_of_rule 4 worked?"));
        }
        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Produce /X0/X1/XX/Xx/XX.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "10/11/11/10/xx").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            println!("test_union 1 worked = {}", &rul_str3);
        } else {
            return Err(format!("test_union 1 failed?"));
        }

        // Fail due to bit 1 being 0X.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "10/11/11/00/xx").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            println!("test_union 2 worked {}?", &rul_str3);
        }

        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/11/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/11/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            println!("test_union 3 worked {}", &rul_str3);
        } else {
            return Err(format!("union 3 failed?"));
        }

        // Fail due to bit 1 forming 0X.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/00/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            return Err(format!("test_union 4 worked {}?", &rul_str3));
        } else {
            println!("union 4 failed");
        }

        // Fail due to X1 and X0 forming (XX, Xx) and (X0, X1)
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/X1").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/00/X0").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/X0").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/X1").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            return Err(format!("test_union 5 worked {}?", &rul_str3));
        } else {
            println!("union 5 failed");
        }

        Ok(())
    }
}
