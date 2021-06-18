//! The RuleStore struct, a vector of SomeRule structs.
//!
//! The vector is sometimes empty, or has one or two rules.
//!
//! If two rules, they cannot be combined due to a 0->X or 1->X difference.
//!
//! If a square has two alternating results, since the square has a bit pattern with
//! no X positions, the different results must different by one, or more,
//! 1->X or 0->X bit positions.

// use crate::maskstore::MaskStore;
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

        // A rulestore of [Xx, XX] is equal to [X0, X1] at the rule bit mask level.
        // Xx = 0->1, 1->0, XX = 0->0, 1->1
        // X1 = 0->1, 1->1, X0 = 0->0, 1->0
        if self.len() == 2 {
            if self.avec[0].b00 != other.avec[0].b00 && self.avec[0].b00 != other.avec[1].b00 {
                return false;
            }
            if self.avec[0].b01 != other.avec[0].b01 && self.avec[0].b01 != other.avec[1].b01 {
                return false;
            }

            if self.avec[0].b11 != other.avec[0].b11 && self.avec[0].b11 != other.avec[1].b11 {
                return false;
            }
            if self.avec[0].b10 != other.avec[0].b10 && self.avec[0].b10 != other.avec[1].b10 {
                return false;
            }

            return true;
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
    /// For two result rules, the rule order is not important.
    /// A rulestore of [Xx, XX] is equal to [X0, X1] at the rule bit mask level.
    /// Xx = 0->1, 1->0, XX = 0->0, 1->1
    /// X1 = 0->1, 1->1, X0 = 0->0, 1->0
    /// in the original rule will be returned.
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
            let mut opt0 = 2; // Invalid option

            let b00_opt0 = self.avec[0].b00.m_or(&other.avec[0].b00);
            let b01_opt0 = self.avec[0].b01.m_or(&other.avec[0].b01);
            let b00_opt1 = self.avec[1].b00.m_or(&other.avec[1].b00);
            let b01_opt1 = self.avec[1].b01.m_or(&other.avec[1].b01);
            if b00_opt0.m_and(&b01_opt0).is_low() && b00_opt1.m_and(&b01_opt1).is_low() {
                opt0 = 0; // For b00, b01, match rules self[0] and other[0], self[1] and other[1].
            }

            if opt0 == 2 {
                let b00_opt0 = self.avec[0].b00.m_or(&other.avec[1].b00);
                let b01_opt0 = self.avec[0].b01.m_or(&other.avec[1].b01);
                let b00_opt1 = self.avec[1].b00.m_or(&other.avec[0].b00);
                let b01_opt1 = self.avec[1].b01.m_or(&other.avec[0].b01);
                if b00_opt0.m_and(&b01_opt0).is_low() && b00_opt1.m_and(&b01_opt1).is_low() {
                    opt0 = 1; // For b00, b01, match rules self[0] and other[1], self[1] and other[0].
                }
            }

            if opt0 == 2 {
                return None;
            }

            let mut opt1 = 2; // Invalid option

            let b11_opt0 = self.avec[0].b11.m_or(&other.avec[0].b11);
            let b10_opt0 = self.avec[0].b10.m_or(&other.avec[0].b10);
            let b11_opt1 = self.avec[1].b11.m_or(&other.avec[1].b11);
            let b10_opt1 = self.avec[1].b10.m_or(&other.avec[1].b10);
            if b11_opt0.m_and(&b10_opt0).is_low() && b11_opt1.m_and(&b10_opt1).is_low() {
                opt1 = 0; // For b11, b10, match rules self[0] and other[0], self[1] and other[1].
            }

            if opt1 == 2 {
                let b11_opt0 = self.avec[0].b11.m_or(&other.avec[1].b11);
                let b10_opt0 = self.avec[0].b10.m_or(&other.avec[1].b10);
                let b11_opt1 = self.avec[1].b11.m_or(&other.avec[0].b11);
                let b10_opt1 = self.avec[1].b10.m_or(&other.avec[0].b10);
                if b11_opt0.m_and(&b10_opt0).is_low() && b11_opt1.m_and(&b10_opt1).is_low() {
                    opt1 = 1; // For b11, b10, match rules self[0] and other[1], self[1] and other[0].
                }
            }

            if opt1 == 2 {
                return None;
            }

            let mut ret_store = Self::new();

            if opt0 == 0 {
                let r0_b00 = self.avec[0].b00.m_or(&other.avec[0].b00);
                let r0_b01 = self.avec[0].b01.m_or(&other.avec[0].b01);
                let r1_b00 = self.avec[1].b00.m_or(&other.avec[1].b00);
                let r1_b01 = self.avec[1].b01.m_or(&other.avec[1].b01);

                if opt1 == 0 {
                    // opt0 == 0, opt1 == 0

                    let r0_b11 = self.avec[0].b11.m_or(&other.avec[0].b11);
                    let r0_b10 = self.avec[0].b10.m_or(&other.avec[0].b10);
                    let r1_b11 = self.avec[1].b11.m_or(&other.avec[1].b11);
                    let r1_b10 = self.avec[1].b10.m_or(&other.avec[1].b10);

                    ret_store.push(SomeRule {
                        b00: r0_b00,
                        b01: r0_b01,
                        b11: r0_b11,
                        b10: r0_b10,
                    });
                    ret_store.push(SomeRule {
                        b00: r1_b00,
                        b01: r1_b01,
                        b11: r1_b11,
                        b10: r1_b10,
                    });
                    return Some(ret_store);
                } else {
                    // opt1 == 1
                    // opt0 == 0, opt1 == 1
                    let r0_b11 = self.avec[0].b11.m_or(&other.avec[1].b11);
                    let r0_b10 = self.avec[0].b10.m_or(&other.avec[1].b10);
                    let r1_b11 = self.avec[1].b11.m_or(&other.avec[0].b11);
                    let r1_b10 = self.avec[1].b10.m_or(&other.avec[0].b10);

                    ret_store.push(SomeRule {
                        b00: r0_b00,
                        b01: r0_b01,
                        b11: r0_b11,
                        b10: r0_b10,
                    });
                    ret_store.push(SomeRule {
                        b00: r1_b00,
                        b01: r1_b01,
                        b11: r1_b11,
                        b10: r1_b10,
                    });
                    return Some(ret_store);
                }
            } else {
                // opt0 == 1

                let r0_b00 = self.avec[0].b00.m_or(&other.avec[1].b00);
                let r0_b01 = self.avec[0].b01.m_or(&other.avec[1].b01);
                let r1_b00 = self.avec[1].b00.m_or(&other.avec[0].b00);
                let r1_b01 = self.avec[1].b01.m_or(&other.avec[0].b01);

                if opt1 == 0 {
                    // opt0 == 1, opt1 == 0
                    let r0_b11 = self.avec[0].b11.m_or(&other.avec[0].b11);
                    let r0_b10 = self.avec[0].b10.m_or(&other.avec[0].b10);
                    let r1_b11 = self.avec[1].b11.m_or(&other.avec[1].b11);
                    let r1_b10 = self.avec[1].b10.m_or(&other.avec[1].b10);

                    ret_store.push(SomeRule {
                        b00: r0_b00,
                        b01: r0_b01,
                        b11: r0_b11,
                        b10: r0_b10,
                    });
                    ret_store.push(SomeRule {
                        b00: r1_b00,
                        b01: r1_b01,
                        b11: r1_b11,
                        b10: r1_b10,
                    });
                    return Some(ret_store);
                } else {
                    // opt1 == 1
                    // opt0 == 1, opt1 == 1
                    let r0_b11 = self.avec[0].b11.m_or(&other.avec[1].b11);
                    let r0_b10 = self.avec[0].b10.m_or(&other.avec[1].b10);
                    let r1_b11 = self.avec[1].b11.m_or(&other.avec[0].b11);
                    let r1_b10 = self.avec[1].b10.m_or(&other.avec[0].b10);

                    ret_store.push(SomeRule {
                        b00: r0_b00,
                        b01: r0_b01,
                        b11: r0_b11,
                        b10: r0_b10,
                    });
                    ret_store.push(SomeRule {
                        b00: r1_b00,
                        b01: r1_b01,
                        b11: r1_b11,
                        b10: r1_b10,
                    });
                    return Some(ret_store);
                }
            }
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
