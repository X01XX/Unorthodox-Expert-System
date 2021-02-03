// Implement a store for one or two rules, for an Unorthodox Expert System.

use crate::maskstore::MaskStore;
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

        if self.len() == 2 {
            if self.avec[0] == other.avec[0] && self.avec[0] == other.avec[0] {
                return true;
            }
            return self.avec[0] == other.avec[1] && self.avec[1] == other.avec[0];
        }

        panic!("Unsupported RuleStore length {}", self.len());
    }
}

impl Eq for RuleStore {}

impl RuleStore {
    // Return a new RuleStore
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRule>::with_capacity(2),
        }
    }

    // Return the length of a RuleStore
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    // Add a rule to a RuleStore
    pub fn push(&mut self, val: SomeRule) {
        if self.len() > 1 {
            panic!("Rule already full??");
        }

        self.avec.push(val);
    }

    pub fn first(&self) -> &SomeRule {
        &self.avec[0]
    }

    pub fn second(&self) -> &SomeRule {
        &self.avec[1]
    }

    // Return true if one RuleStore is a subset of another.
    // This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    // should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
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

            if self.first().is_subset_of(&other.first())
                && self.second().is_subset_of(&other.second())
                || self.first().is_subset_of(&other.second())
                    && self.second().is_subset_of(&other.first())
            {
                return true;
            } else {
                return false;
            }
        }

        panic!("unexpected rulestore length!");
    }

    // Return true if a RuleStore is a superset of a rule
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

    // Return the union of two RuleStores.
    // May return None, if the union is invalid.
    //
    // The rules will be from two squares, the possible combinations
    // for each bit position are:
    //
    // 0->0, 0->0 = 0->0
    // 0->0, 0->1 = 0->X, disallowed
    // 0->0, 1->1 = X->X
    // 0->0, 1->0 = X->0
    //
    // 0->1, 0->1 = 0->1
    // 0->1, 1->1 = X->1
    // 0->1, 1->0 = X->x (X to x-not)
    //
    // 1->1, 1->1 = 1->1
    // 1->1, 1->0 = 1->X, disallowed
    //
    // 1->0, 1->0 = 1->0
    //
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
            // Avoid a combination or rules that combines changes that
            // are not seen together in the source rules.

            // Store b01 change patterns
            let mut b01_cng_pats = MaskStore::new_with_capacity(4);
            b01_cng_pats.push(self.avec[0].b01.clone());
            b01_cng_pats.push(self.avec[1].b01.clone());
            b01_cng_pats.push(other.avec[0].b01.clone());
            b01_cng_pats.push(other.avec[1].b01.clone());

            //  Store b10 change patterns
            let mut b10_cng_pats = MaskStore::new_with_capacity(4);
            b10_cng_pats.push(self.avec[0].b10.clone());
            b10_cng_pats.push(self.avec[1].b10.clone());
            b10_cng_pats.push(other.avec[0].b10.clone());
            b10_cng_pats.push(other.avec[1].b10.clone());

            // Attempt self[0]/other[0], self[1]/other[1] combination
            let mut ars1 = Self::new();

            let rulx = self.avec[0].union(&other.avec[0]);

            if rulx.is_valid_union()
                && b01_cng_pats.contains(&rulx.b01)
                && b10_cng_pats.contains(&rulx.b10)
            {
                let ruly = self.avec[1].union(&other.avec[1]);

                if ruly.is_valid_union()
                    && b01_cng_pats.contains(&ruly.b01)
                    && b10_cng_pats.contains(&ruly.b10)
                {
                    if rulx.initial_region() == ruly.initial_region() {
                        ars1.push(rulx);
                        ars1.push(ruly);
                        return Some(ars1);
                    }
                }
            }

            // Attempt self[0]/other[1], self[1]/other[0] combination
            let mut ars2 = Self::new();

            let rulx = self.avec[0].union(&other.avec[1]);

            if rulx.is_valid_union()
                && b01_cng_pats.contains(&rulx.b01)
                && b10_cng_pats.contains(&rulx.b10)
            {
                let ruly = self.avec[1].union(&other.avec[0]);

                if ruly.is_valid_union()
                    && b01_cng_pats.contains(&ruly.b01)
                    && b10_cng_pats.contains(&ruly.b10)
                {
                    if rulx.initial_region() == ruly.initial_region() {
                        ars2.push(rulx);
                        ars2.push(ruly);
                        return Some(ars2);
                    }
                }
            }

            //println!("rulestore union returns None\n");
            return None;
        } // end if

        if self.len() == 0 {
            // needed?
            return Some(Self::new());
        }

        panic!("unexpected RuleStore length");
    }

    pub fn iter(&self) -> Iter<SomeRule> {
        self.avec.iter()
    }

    // Return intersection of two RuleStores
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if self.len() != other.len() {
            panic!("rulestore lengths not eq!");
        }

        let mut ars = Self::new();
        let mut ars2 = Self::new();

        if self.len() == 1 {
            if self.avec[0]
                .intersection(&other.avec[0])
                .is_valid_intersection()
            {
                ars.push(self.avec[0].intersection(&other.avec[0]));
                return Some(ars);
            } else {
                return None;
            }
        } else if self.len() == 2 {
            if self.avec[0]
                .intersection(&other.avec[0])
                .is_valid_intersection()
                && self.avec[1]
                    .intersection(&other.avec[1])
                    .is_valid_intersection()
            {
                ars.push(self.avec[0].intersection(&other.avec[0]));
                ars.push(self.avec[1].intersection(&other.avec[1]));
                // return Some(ars);
            }

            if self.avec[0]
                .intersection(&other.avec[1])
                .is_valid_intersection()
                && self.avec[1]
                    .intersection(&other.avec[0])
                    .is_valid_intersection()
            {
                ars2.push(self.avec[0].intersection(&other.avec[1]));
                ars2.push(self.avec[1].intersection(&other.avec[0]));
                // return Some(ars);
            }

            if ars.len() > 0 && ars2.len() > 0 {
                return ars.union(&ars2);
            }

            if ars.len() > 0 {
                return Some(ars);
            }

            if ars2.len() > 0 {
                return Some(ars2);
            }

            None
        } else {
            panic!("not ready for pn {}!", self.len());
        }
    }

    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        let mut rcrs = Self::new();

        for rulx in self.avec.iter() {
            rcrs.push(rulx.restrict_initial_region(&regx));
        }
        rcrs
    }

    // Return the union of two RuleStores, pruned or 1->X and 0->X, under initial X bits, if needed
    pub fn union_prune(&self, other: &Self) -> Option<Self> {
        if self.len() != other.len() {
            return None;
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
        } else if self.len() == 2 {
            let rulx = self.avec[0].union(&other.avec[0]);

            if rulx.is_valid_union() {
                let ruly = self.avec[1].union(&other.avec[1]);

                if ruly.is_valid_union() {
                    if rulx.initial_region() == ruly.initial_region() {
                        ars.push(rulx);
                        ars.push(ruly);
                        return Some(ars);
                    } else if rulx.initial_region().intersects(&ruly.initial_region()) {
                        let regz = rulx.initial_region().intersection(&ruly.initial_region());
                        ars.push(rulx.restrict_initial_region(&regz));
                        ars.push(ruly.restrict_initial_region(&regz));
                        return Some(ars);
                    }
                }
            }

            let rulx = self.avec[0].union(&other.avec[1]);

            if rulx.is_valid_union() {
                let ruly = self.avec[1].union(&other.avec[0]);

                if ruly.is_valid_union() {
                    if rulx.initial_region() == ruly.initial_region() {
                        ars.push(rulx);
                        ars.push(ruly);
                        return Some(ars);
                    } else if rulx.initial_region().intersects(&ruly.initial_region()) {
                        let regz = rulx.initial_region().intersection(&ruly.initial_region());
                        ars.push(rulx.restrict_initial_region(&regz));
                        ars.push(ruly.restrict_initial_region(&regz));
                        return Some(ars);
                    }
                }
            }
        } // end if

        None
    } // end union_prune

    pub fn initial_region(&self) -> SomeRegion {
        self.avec[0].initial_region()
    }

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
