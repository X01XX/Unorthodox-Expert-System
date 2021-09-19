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

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

#[derive(Serialize, Deserialize, Debug)]
pub struct RuleStore {
    pub avec: Vec<SomeRule>,
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

        if self.len() == 1 {
            return self.avec[0] == other.avec[0];
        }

        // For two rules, order does not matter
        if self.len() == 2 {
            if self.avec[0] == other.avec[0] && self.avec[1] == other.avec[1] {
                return true;
            }

            if self.avec[0] == other.avec[1] && self.avec[1] == other.avec[0] {
                return true;
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

    /// Return the length of a RuleStore.
    /// Should be 0, 1 or 2.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a rule to a RuleStore.
    pub fn push(&mut self, val: SomeRule) {
        if self.len() > 1 {
            panic!("Rule already full??");
        }

        self.avec.push(val);
    }

    /// Return a reference to the first rule.
    pub fn first(&self) -> &SomeRule {
        &self.avec[0]
    }

    /// Return a reference to the second rule.
    pub fn second(&self) -> &SomeRule {
        &self.avec[1]
    }

    /// Return true if one RuleStore is a subset of another.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.initial_region().is_subset_of(&other.initial_region()) == false {
            return false;
        }

        if self.len() == 1 {
            if other.len() == 1 {
                return self.first().is_subset_of(&other.first());
            } else if other.len() == 2 {
                return self.first().is_subset_of(&other.first())
                    || self.first().is_subset_of(&other.second());
            } else {
                panic!("Unexpected rulestore length!");
            }
        }

        if self.len() == 2 {
            if other.len() == 1 {
                return false;
            }

            let reg_int = self.initial_region().intersection(&other.initial_region());

            let rules2 = other.restrict_initial_region(&reg_int);

            if let Some(_) = self.union(&rules2) {
                return true;
            }

            return false;
        }

        panic!("unexpected rulestore length!");
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        if self.len() == 1 {
            return other.is_subset_of(&self.first());
        }

        if self.len() == 2 {
            if other.is_subset_of(&self.first()) {
                return true;
            }

            return other.is_subset_of(&self.second());
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

        let mut ars = Self::new();

        if self.len() == 1 {
            let rulx = self.avec[0].union(&other.avec[0]);

            if rulx.is_valid_union() {
                ars.push(rulx);
                return Some(ars);
            } else {
                return None;
            }
        }

        if self.len() == 2 {
            let mut ordera = false;
            let rul0 = self.avec[0].union(&other.avec[0]);
            let rul1 = self.avec[1].union(&other.avec[1]);
            if rul0.is_valid_union() && rul1.is_valid_union() {
                ordera = true;
            }

            let mut orderb = false;
            let rul2 = self.avec[0].union(&other.avec[1]);
            let rul3 = self.avec[1].union(&other.avec[0]);
            if rul2.is_valid_union() && rul3.is_valid_union() {
                orderb = true;
            }

            if ordera && orderb {
                return None;
            }

            let mut ret_store = Self::new();
            if ordera {
                ret_store.push(rul0);
                ret_store.push(rul1);
                return Some(ret_store);
            } else if orderb {
                ret_store.push(rul2);
                ret_store.push(rul3);
                return Some(ret_store);
            }

            return None;
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
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

            if self.avec[0]
                .intersection(&other.avec[0])
                .is_valid_intersection()
            {
                ars.push(self.avec[0].intersection(&other.avec[0]));
                return Some(ars);
            }
            return None;
        } else if self.len() == 2 {
            let regx = self.initial_region().intersection(&other.initial_region());

            let rs1 = self.restrict_initial_region(&regx);
            let rs2 = other.restrict_initial_region(&regx);

            if let Some(rs12) = rs1.union(&rs2) {
                return Some(rs12);
            }

            return None;
        }
        panic!("not ready for pn {}!", self.len());
    }

    /// Return the result of restricting the initial region of rules.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        let mut rcrs = Self::new();

        for rulx in self.avec.iter() {
            rcrs.push(rulx.restrict_initial_region(&regx));
        }
        rcrs
    }

    /// Return the inital region of the first rule in the store.
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

impl Clone for RuleStore {
    fn clone(&self) -> Self {
        let mut rcrs = RuleStore::new();

        for rulx in self.iter() {
            rcrs.push(rulx.clone());
        }
        rcrs
    }
}
