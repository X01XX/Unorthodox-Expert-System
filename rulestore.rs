//! The RuleStore struct, a vector of SomeRule structs.
//!
//! The vector is sometimes empty, or has one or two rules.
//!
//! If two rules, they cannot be combined due to a 0->X or 1->X difference.
//!
//! If a square has two alternating results, since the square has a bit pattern with
//! no X positions, the different results must different by one, or more,
//! 1->X or 0->X bit positions.

use crate::bits::vec_same_num_bits;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RuleStore {
    items: Vec<SomeRule>,
}

impl fmt::Display for RuleStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

impl PartialEq for RuleStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for rulx in self.iter() {
            if !other.contains(rulx) {
                return false;
            }
        }

        true
    }
}

impl Eq for RuleStore {}

impl RuleStore {
    /// Return a new, empty, RuleStore.
    pub fn new(items: Vec<SomeRule>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        Self { items }
    }

    /// Return true if a RuleStore contains a rule.
    pub fn contains(&self, rul: &SomeRule) -> bool {
        debug_assert!(self.is_empty() || self[0].num_bits() == rul.num_bits());

        self.items.contains(rul)
    }

    /// Return the length of a RuleStore.
    /// Should be 0, 1 or 2.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a rule to a RuleStore.
    /// If there are two rules, they will have at least one incompatibility,
    /// 0->0/0->1 or 1->1/1->0, and have equal initial regions.
    pub fn push(&mut self, val: SomeRule) {
        debug_assert!(self.is_empty() || self.num_bits().expect("SNH") == val.num_bits());

        self.items.push(val);
    }

    /// Return the number of bits used in RuleStore items.
    pub fn num_bits(&self) -> Option<usize> {
        if self.is_empty() {
            return None;
        }
        Some(self.items[0].num_bits())
    }

    /// Return true if one non-empty RuleStore is a subset of another non-empty.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.len() > other.len() {
            return false;
        }

        if !self.initial_region().is_subset_of(&other.initial_region()) {
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
    pub fn is_superset_of(&self, other: &Self) -> bool {
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        other.is_subset_of(self)
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        assert!(!self.is_empty());
        debug_assert_eq!(self.num_bits().expect("SNH"), other.num_bits());

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
        //println!("\nrulestore union {} and {}", self, other);
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.len() != other.len() {
            //println!("\nrulestore union: returns None");
            return None;
        }

        if self.len() == 1 {
            let rulx = self.items[0].union(&other.items[0])?;
            return Some(Self::new(vec![rulx]));
        }

        if self.len() == 2 {
            let mut ordera = true;

            let rul0 = self.items[0].union(&other.items[0]);
            let rul1 = self.items[1].union(&other.items[1]);

            if rul0.is_none() || rul1.is_none() {
                ordera = false;
            }

            let mut orderb = true;

            let rul2 = self.items[0].union(&other.items[1]);
            let rul3 = self.items[1].union(&other.items[0]);

            if rul2.is_none() || rul3.is_none() {
                orderb = false;
            }

            // For any Pn::Two RuleStore, there must be at least one single-bit position of 0->1 and 0->0 alternating result,
            // or a 1->0 and 1->1 alternating result.
            //
            // To join two Pn::Two RuleStores in one of two possible sequences, there must be at least one matching initial single-bit position
            // with an alternating result.
            if ordera == orderb {
                // Both true or both false.
                return None;
            }

            if ordera {
                return Some(Self::new(vec![rul0.expect("SNH"), rul1.expect("SNH")]));
            }

            // Must be orderb == true.
            return Some(Self::new(vec![rul2.expect("SNH"), rul3.expect("SNH")]));
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
    }

    /// Return a valid union, if possible, by restricting
    /// the initial region as needed to get rid of 0/X and 1/X bit
    /// positions.
    pub fn parsed_union(&self, other: &Self) -> Option<Self> {
        //println!("\nrulestore parsed_union {} and {}", self, other);
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.len() != other.len() {
            //println!("\nrulestore union: returns None");
            return None;
        }

        if self.len() == 1 {
            let rulx = self.items[0].parsed_union(&other.items[0])?;
            return Some(Self::new(vec![rulx]));
        }

        if self.len() == 2 {
            let mut ordera = true;

            let rul0 = self.items[0].parsed_union(&other.items[0]);
            let rul1 = self.items[1].parsed_union(&other.items[1]);

            if let Some(ref rulx) = rul0 {
                if let Some(ref ruly) = rul1 {
                    if rulx.initial_region() != ruly.initial_region() {
                        ordera = false;
                    }
                } else {
                    ordera = false;
                }
            } else {
                ordera = false;
            }

            let mut orderb = true;

            let rul2 = self.items[0].parsed_union(&other.items[1]);
            let rul3 = self.items[1].parsed_union(&other.items[0]);

            if let Some(ref rulx) = rul2 {
                if let Some(ref ruly) = rul3 {
                    if rulx.initial_region() != ruly.initial_region() {
                        orderb = false;
                    }
                } else {
                    orderb = false;
                }
            } else {
                orderb = false;
            }

            // For any Pn::Two RuleStore, there must be at least one single-bit position of 0->1 and 0->0 alternating result,
            // or a 1->0 and 1->1 alternating result.
            //
            // To join two Pn::Two RuleStores in one of two possible sequences, there must be at least one matching initial single-bit position
            // with an alternating result.
            if ordera == orderb {
                // Both true or both false.
                return None;
            }

            if ordera {
                return Some(Self::new(vec![rul0.expect("SNH"), rul1.expect("SNH")]));
            }

            // Must be orderb == true.
            return Some(Self::new(vec![rul2.expect("SNH"), rul3.expect("SNH")]));
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
    }

    /// If a two-rule RuleStore could be compatible with
    /// a one-rule RuleStore, return the index of the compatible rule.
    ///
    /// This is mostly useful in finding that a single rule is not compatible
    /// to a two-rule store, so its not neccessary to get additional samples.
    pub fn subcompatible_index(&self, other: &RuleStore) -> Option<usize> {
        //println!("starting subcompatible_index");
        assert!(self.len() == 2);
        assert!(other.len() == 1);
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let zero = self[0].union(&other[0]).is_some();
        let one = self[1].union(&other[0]).is_some();

        if zero && one {
            return None;
        }
        if zero {
            return Some(0);
        }
        if one {
            return Some(1);
        }
        None
    }

    /// Return true if two one-rule RuleStores,
    /// or two two-rule RuleStores are compatible.
    pub fn compatible(&self, other: &RuleStore) -> bool {
        assert!(self.len() == other.len());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        assert!(!self
            .initial_region()
            .is_superset_of(&other.initial_region()));
        assert!(!other
            .initial_region()
            .is_superset_of(&self.initial_region()));

        if self.len() == 1 {
            return self[0].union(&other[0]).is_some();
        }

        // Must be two-rule RuleStores.
        (self[0].union(&other[0]).is_some() && self[1].union(&other[1]).is_some())
            ^ (self[0].union(&other[1]).is_some() && self[1].union(&other[0]).is_some())
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.items.iter()
    }

    /// Return the intersection of two RuleStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.len() != other.len() {
            panic!("rulestore lengths not eq! {} vs {}", self, other);
        }

        if self.len() == 1 {
            let int1 = self[0].intersection(&other[0])?;

            return Some(Self::new(vec![int1]));
        }

        if self.len() == 2 {
            // Intersect by order1
            if let Some(int00) = self[0].intersection(&other[0]) {
                if let Some(int11) = self[1].intersection(&other[1]) {
                    if int00.initial_region() == int11.initial_region() {
                        //println!("pn3 intersection of {} and {}", self, other);
                        return Some(Self::new(vec![int00, int11]));
                    }
                }
            }

            // Intersect by order2
            if let Some(int01) = self[0].intersection(&other[1]) {
                if let Some(int10) = self[1].intersection(&other[0]) {
                    if int01.initial_region() == int10.initial_region() {
                        //println!("pn3 intersection of {} and {}", self, other);
                        return Some(Self::new(vec![int01, int10]));
                    }
                }
            }

            //println!("pn3 intersection of {} and {} failed", self, other);
            return None;
        }
        panic!("not ready for pn {}!", self.len());
    }

    /// Return the result of restricting the initial region of rules in a RuleStore.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        debug_assert!(self.is_not_empty());
        debug_assert_eq!(self.num_bits().expect("SNH"), regx.num_bits());
        debug_assert!(regx.intersects(&self.initial_region()));

        Self::new(
            self.items
                .iter()
                .map(|rulx| rulx.restrict_initial_region(regx))
                .collect::<Vec<SomeRule>>(),
        )
    }

    /// Return the initial region of the first rule in the store.
    pub fn initial_region(&self) -> SomeRegion {
        debug_assert!(self.is_not_empty());

        self.items[0].initial_region()
    }

    /// Return true if a RuleStore's rules causes predictable change.
    pub fn causes_predictable_change(&self) -> bool {
        debug_assert!(self.is_not_empty());

        if self.len() > 1 {
            return true;
        }
        self[0].causes_predictable_change()
    }

    /// Return true if all rules in the RuleStore have the same initial region.
    pub fn rules_initial_region_eq(&self, aregion: &SomeRegion) -> bool {
        debug_assert!(self.is_not_empty());
        debug_assert!(self.num_bits().expect("SNH") == aregion.num_bits());

        for rulx in self.iter() {
            if rulx.initial_region() != *aregion {
                return false;
            }
        }
        true
    }

    /// Return rules massaged to be within a given region.
    pub fn within(&self, within: &SomeRegion) -> Self {
        debug_assert!(self.is_not_empty());
        debug_assert!(self.num_bits().expect("SNH") == within.num_bits());

        let mut ret = Self::new(vec![]);

        for rulx in self.iter() {
            if rulx.initial_region().intersects(within) {
                ret.push(rulx.restrict_initial_region(within));
            } else {
                return ret;
            }
        }
        ret
    }
} // end impl RuleStore

impl Index<usize> for RuleStore {
    type Output = SomeRule;
    fn index(&self, i: usize) -> &SomeRule {
        &self.items[i]
    }
}

impl IntoIterator for RuleStore {
    type Item = SomeRule;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test restrict_initial_region and initial_region
    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/11/11")?,
            SomeRule::new_from_string("00/11/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("11/00/10")?,
            SomeRule::new_from_string("11/00/11")?,
        ]);

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.union(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let regx = SomeRegion::new_from_string("r101")?;
        let rul_str4 = rul_str3.restrict_initial_region(&regx);
        println!("rul_str4: {rul_str4}");

        assert!(rul_str4.initial_region() == regx);
        assert!(rul_str4[1].initial_region() == regx);

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Intersect two single-rule RuleStores, it should work.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/X1/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("X0/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rul_str4 = RuleStore::new(vec![SomeRule::new_from_string("00/11/11/10/00")?]);
        println!("rul_str4: {rul_str4}");

        assert!(rul_str3 == rul_str4);

        // Produce failure due to bit 3, Xx intersect 11 = null.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/Xx/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("X0/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        // Intersect two two-rule RuleStores, it should work.
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/X1/XX/Xx/xx")?,
            SomeRule::new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("X1/11/11/10/00")?,
            SomeRule::new_from_string("X0/11/11/10/00")?,
        ]);

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rulx = SomeRule::new_from_string("00/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        let rulx = SomeRule::new_from_string("01/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        // Intersect two two-rule RuleStores, it should not work, due to the left-most bit.
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/X1/XX/Xx/xx")?,
            SomeRule::new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("X1/11/11/10/00")?,
            SomeRule::new_from_string("X1/11/11/11/00")?,
        ]);

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        // Compare one-rule RuleStores.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/X1/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("00/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));

        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("00/10/11/10/00")?]);
        println!("rul_str2: {rul_str2}");

        assert!(!rul_str2.is_subset_of(&rul_str1));

        // Compare two two-rule RuleStores.
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/X1/XX/Xx/xx")?,
            SomeRule::new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("00/11/11/10/00")?,
            SomeRule::new_from_string("01/11/11/10/00")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));
        assert!(!rul_str1.is_subset_of(&rul_str2));

        Ok(())
    }

    #[test]
    fn is_superset_of_rule() -> Result<(), String> {
        // Compare a rule to a one-rule RuleStore.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/X1/XX/Xx/xx")?]);
        let rul2 = SomeRule::new_from_string("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = SomeRule::new_from_string("00/10/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        // Compare rule to a two-rule RuleStore.
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/X1/XX/Xx/xx")?,
            SomeRule::new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul2 = SomeRule::new_from_string("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = SomeRule::new_from_string("00/00/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Produce /X0/X1/XX/Xx/XX.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/01/00/01/xx")?]);
        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("10/11/11/10/xx")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 being 0X.
        let rul_str1 = RuleStore::new(vec![SomeRule::new_from_string("00/01/00/01/xx")?]);
        let rul_str2 = RuleStore::new(vec![SomeRule::new_from_string("10/11/11/00/xx")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/11/11")?,
            SomeRule::new_from_string("00/11/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("11/00/10")?,
            SomeRule::new_from_string("11/00/11")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 forming 0X.
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/01/11")?,
            SomeRule::new_from_string("00/00/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("11/00/10")?,
            SomeRule::new_from_string("11/00/11")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        // Fail due to X1 and X0 forming (XX, Xx) and (X0, X1)
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("00/01/X1")?,
            SomeRule::new_from_string("00/00/X0")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("11/00/X0")?,
            SomeRule::new_from_string("11/00/X1")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        Ok(())
    }

    // Most conditions are covered by other tests, except one.
    // If the two-result RuleStores can match in both orders.
    //
    // One bit position has all four possible changes represented.
    // 00, 01, 11, 10.
    //
    // [11/01, 11/00] (Initial regions must match, here its 10)
    // [00/11, 00/10] (Initial region 01)
    // --------------
    //  XX/X1, XX/X0
    //
    // [11/01, 11/00]
    // [00/10, 00/11]
    // --------------
    //  XX/Xx, XX/XX
    #[test]
    fn compatible() -> Result<(), String> {
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("11/01")?,
            SomeRule::new_from_string("11/00")?,
        ]);
        println!("rul_str1 {rul_str1}");

        let rul_str2 = RuleStore::new(vec![
            SomeRule::new_from_string("00/10")?,
            SomeRule::new_from_string("00/11")?,
        ]);
        println!("rul_str2 {rul_str2}");

        let rslt = rul_str1.compatible(&rul_str2);
        println!("rslt {rslt}");

        assert!(rslt == false);

        Ok(())
    }

    #[test]
    fn subcompatible() -> Result<(), String> {
        let rul_str1 = RuleStore::new(vec![
            SomeRule::new_from_string("11/11/11/xx")?,
            SomeRule::new_from_string("11/11/10/xx")?,
        ]);

        // Test match with first rule.
        let rul_sub = RuleStore::new(vec![SomeRule::new_from_string("11/11/11/00")?]);
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} Ok"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b Some(0), not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} s/b Some(0), not None"),
        }

        // Test match with second rule.
        let rul_sub = RuleStore::new(vec![SomeRule::new_from_string("11/11/10/00")?]);
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b Some(1), not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} Ok"),
            _ => println!("{rul_str1} sub {rul_sub} s/b Some(0), not None"),
        }

        // Test match with no rules.
        let rul_sub = RuleStore::new(vec![SomeRule::new_from_string("10/11/11/00")?]);
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} Ok"),
        }

        // Test match with both rules.
        let rul_sub = RuleStore::new(vec![SomeRule::new_from_string("11/11/00/00")?]);
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} Ok"),
        }

        Ok(())
    }
}
