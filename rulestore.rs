//! The RuleStore struct, a vector of SomeRule structs.
//!
//! For squares, or groups, a Some(RuleStore) will contain
//! one, or two, rules. If two rules, each rule will have the same
//! initial square-state/group-region, and different results.
use crate::bits::vec_same_num_bits;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::str::FromStr;

use unicode_segmentation::UnicodeSegmentation;

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

/// RuleStores are equal if they contain the same rules, order does not matter.
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
        if items.len() > 1 {
            for inx in 1..items.len() {
                assert!(items[inx].num_bits() == items[0].num_bits());
                assert!(items[inx].initial_region() == items[0].initial_region());
            }
        }
        Self { items }
    }

    /// Return a new RuleStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeRule>::with_capacity(num),
        }
    }

    /// Return true if a RuleStore contains a rule.
    pub fn contains(&self, rul: &SomeRule) -> bool {
        assert!(self.is_empty() || rul.num_bits() == self[0].num_bits());

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
    pub fn push(&mut self, val: SomeRule) {
        debug_assert!(self.is_empty() || self[0].num_bits() == val.num_bits());
        debug_assert!(self.is_empty() || self[0].initial_region() == val.initial_region());

        self.items.push(val);
    }

    /// Return the number of bits used in the first RuleStore item.
    pub fn num_bits(&self) -> Option<usize> {
        if self.is_empty() {
            return None;
        }
        Some(self.items[0].num_bits())
    }

    /// Return true if one non-empty RuleStore is a subset of another non-empty.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert!(self[0].num_bits() == other[0].num_bits());

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

        if self.len() == 2 && other.len() == 2 {
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
        debug_assert!(self[0].num_bits() == other[0].num_bits());

        other.is_subset_of(self)
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        assert!(!self.is_empty());

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
        debug_assert!(self[0].num_bits() == other[0].num_bits());

        if self.len() == 1 {
            let rulx = self.items[0].union(&other.items[0]);
            if rulx.is_valid_union() {
                return Some(Self::new(vec![rulx]));
            } else {
                return None;
            }
        }

        if self.len() == 2 {
            let mut ordera = true;

            let rul0 = self.items[0].union(&other.items[0]);
            let rul1 = self.items[1].union(&other.items[1]);

            if !rul0.is_valid_union() || !rul1.is_valid_union() {
                ordera = false;
            }

            let mut orderb = true;

            let rul2 = self.items[0].union(&other.items[1]);
            let rul3 = self.items[1].union(&other.items[0]);

            if !rul2.is_valid_union() || !rul3.is_valid_union() {
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
                return Some(Self::new(vec![rul0, rul1]));
            }

            // Must be orderb == true.
            return Some(Self::new(vec![rul2, rul3]));
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
        debug_assert!(self[0].num_bits() == other[0].num_bits());

        let zero = self[0].union(&other[0]);
        let one = self[1].union(&other[0]);

        if !zero.is_valid_union() {
            if !one.is_valid_union() {
                None
            } else {
                Some(1)
            }
        } else if !one.is_valid_union() {
            Some(0)
        } else {
            None
        }
    }

    /// Return true if two one-rule RuleStores,
    /// or two two-rule RuleStores are compatible.
    pub fn compatible(&self, other: &RuleStore) -> bool {
        assert!(self.len() == other.len());
        debug_assert!(self[0].num_bits() == other[0].num_bits());

        assert!(!self
            .initial_region()
            .is_superset_of(&other.initial_region()));
        assert!(!other
            .initial_region()
            .is_superset_of(&self.initial_region()));

        if self.len() == 1 {
            return self[0].union(&other[0]).is_valid_union();
        }

        // Must be two-rule RuleStores.
        (self[0].union(&other[0]).is_valid_union() && self[1].union(&other[1]).is_valid_union())
            ^ (self[0].union(&other[1]).is_valid_union()
                && self[1].union(&other[0]).is_valid_union())
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.items.iter()
    }

    /// Return the intersection of two RuleStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        assert!(self.is_not_empty() && other.is_not_empty());
        debug_assert!(self[0].num_bits() == other[0].num_bits());

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
        debug_assert_eq!(self[0].num_bits(), regx.num_bits());
        debug_assert!(vec_same_num_bits(&self.items));
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
        debug_assert!(self[0].num_bits() == aregion.num_bits());

        for rulx in self.iter() {
            if rulx.initial_region() != *aregion {
                return false;
            }
        }
        true
    }

    /// Return rules massaged to be within a given region.
    /// Rule order is preserved.
    pub fn within(&self, within: &SomeRegion) -> Vec<Option<SomeRule>> {
        debug_assert!(self.is_not_empty());
        debug_assert!(self[0].num_bits() == within.num_bits());

        let mut ret = Vec::<Option<SomeRule>>::new();

        for rulx in self.iter() {
            let initial = rulx.initial_region();

            if within.is_superset_of(&initial) {
                let result = rulx.result_region();

                if within.is_superset_of(&result) {
                    ret.push(Some(rulx.clone()));
                } else if within.intersects(&result) {
                    ret.push(Some(rulx.restrict_result_region(within)));
                } else {
                    ret.push(None);
                }
            } else if within.intersects(&initial) {
                let ruly = rulx.restrict_initial_region(within);
                let result = ruly.result_region();

                if within.is_superset_of(&result) {
                    ret.push(Some(ruly.clone()));
                } else if within.intersects(&result) {
                    ret.push(Some(ruly.restrict_result_region(within)));
                } else {
                    ret.push(None);
                }
            } else {
                ret.push(None);
            }
        } // next rulx
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

impl FromStr for RuleStore {
    type Err = String;
    /// Return a rulestore, given a string representation.
    /// Like [], [X0/11/11/10/00] or [00/X1/XX/Xx/xx, 01/X1/XX/Xx/xx].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("rulestore::from_str: {str_in}");
        let src_str = str_in.trim();

        if src_str.is_empty() {
            return Err("RuleStore::from_str: Empty string?".to_string());
        }

        // Unwrap "[ ... ]", check that the brackets are balanced.
        let mut src_str2 = String::new();
        let mut left = 0;
        let mut right = 0;

        for (inx, chr) in src_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "[" {
                    left += 1;
                    continue;
                } else {
                    return Err(
                        "RuleStore::from_str: Invalid string, should start with [".to_string()
                    );
                }
            }
            if chr == "[" {
                left += 1;
            }
            if chr == "]" {
                right += 1;
                if right > left {
                    return Err("RuleStore::from_str: Brackets not balanced".to_string());
                }
            }

            src_str2.push_str(chr);
        }
        if left != right {
            return Err("RuleStore::from_str: Brackets not balanced".to_string());
        }

        // Remove last right-bracket, balancing first left bracket.
        src_str2.remove(src_str2.len() - 1);
        //println!("src_str2 {src_str2}");

        // Split string into <Rule> tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in src_str2.graphemes(true) {
            if chr == " " || chr == "," {
                if token.is_empty() {
                } else {
                    token_list.push(token);
                    token = String::new();
                }
                continue;
            }

            token.push_str(chr);
        }
        if token.is_empty() {
        } else {
            token_list.push(token);
        }
        //println!("token_list {:?}", token_list);

        // Tally up tokens.
        let mut ret_store = RuleStore::with_capacity(token_list.len());

        for tokenx in token_list.into_iter() {
            match SomeRule::from_str(&tokenx) {
                Ok(rulx) => ret_store.push(rulx),
                Err(errstr) => return Err(format!("RuleStore::from_str: {errstr}")),
            }
        }

        Ok(ret_store)
    }
}

/// Implement the trait StrLen for RuleStore.
impl StrLen for RuleStore {
    fn strlen(&self) -> usize {
        let mut rc_len = 2; // for "[]"

        if !self.is_empty() {
            rc_len += self[0].strlen() * self.len(); // Each rule length.
            rc_len += 2 * (self.len() - 1); // Each ", " separator.
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let rulstx = RuleStore::from_str("[00/11/11, 00/11/10]")?;

        let calced_len = rulstx.strlen();
        let str_len = format!("{rulstx}").len();

        println!("str_len {str_len} calced_len {calced_len}");

        assert!(str_len == calced_len);
        Ok(())
    }

    // Test restrict_initial_region and initial_region
    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let rul_str1 = RuleStore::from_str("[00/11/11, 00/11/10]")?;

        let rul_str2 = RuleStore::from_str("[11/00/10, 11/00/11]")?;

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.union(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let regx = SomeRegion::from_str("r101")?;
        let rul_str4 = rul_str3.restrict_initial_region(&regx);
        println!("rul_str4: {rul_str4}");

        assert!(rul_str4.initial_region() == regx);
        assert!(rul_str4[1].initial_region() == regx);

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Intersect two single-rule RuleStores, it should work.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx]")?;
        let rul_str2 = RuleStore::from_str("[X0/11/11/10/00]")?;
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rul_str4 = RuleStore::from_str("[00/11/11/10/00]")?;
        println!("rul_str4: {rul_str4}");

        assert!(rul_str3 == rul_str4);

        // Produce failure due to bit 3, Xx intersect 11 = null.
        let rul_str1 = RuleStore::from_str("[00/Xx/XX/Xx/xx]")?;
        let rul_str2 = RuleStore::from_str("[X0/11/11/10/00]")?;
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        // Intersect two two-rule RuleStores, it should work.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx, 01/X1/XX/Xx/xx]")?;

        let rul_str2 = RuleStore::from_str("[X1/11/11/10/00, X0/11/11/10/00]")?;

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rulx = SomeRule::from_str("00/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        let rulx = SomeRule::from_str("01/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        // Intersect two two-rule RuleStores, it should not work, due to the left-most bit.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx, 01/X1/XX/Xx/xx]")?;

        let rul_str2 = RuleStore::from_str("[X1/11/11/10/00, X1/11/11/11/00]")?;

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        // Compare one-rule RuleStores.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx]")?;
        let rul_str2 = RuleStore::from_str("[00/11/11/10/00]")?;
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));

        let rul_str2 = RuleStore::from_str("[00/10/11/10/00]")?;
        println!("rul_str2: {rul_str2}");

        assert!(!rul_str2.is_subset_of(&rul_str1));

        // Compare two two-rule RuleStores.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx, 01/X1/XX/Xx/xx]")?;

        let rul_str2 = RuleStore::from_str("[00/11/11/10/00, 01/11/11/10/00]")?;

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));
        assert!(!rul_str1.is_subset_of(&rul_str2));

        Ok(())
    }

    #[test]
    fn is_superset_of_rule() -> Result<(), String> {
        // Compare a rule to a one-rule RuleStore.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx]")?;
        let rul2 = SomeRule::from_str("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = SomeRule::from_str("00/10/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        // Compare rule to a two-rule RuleStore.
        let rul_str1 = RuleStore::from_str("[00/X1/XX/Xx/xx, 01/X1/XX/Xx/xx]")?;

        let rul2 = SomeRule::from_str("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = SomeRule::from_str("00/00/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Produce /X0/X1/XX/Xx/XX.
        let rul_str1 = RuleStore::from_str("[00/01/00/01/xx]")?;
        let rul_str2 = RuleStore::from_str("[10/11/11/10/xx]")?;
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 being 0X.
        let rul_str1 = RuleStore::from_str("[00/01/00/01/xx]")?;
        let rul_str2 = RuleStore::from_str("[10/11/11/00/xx]")?;
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        let rul_str1 = RuleStore::from_str("[00/11/11, 00/11/10]")?;

        let rul_str2 = RuleStore::from_str("[11/00/10, 11/00/11]")?;

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 forming 0X.
        let rul_str1 = RuleStore::from_str("[00/01/11, 00/00/10]")?;

        let rul_str2 = RuleStore::from_str("[11/00/10, 11/00/11]")?;

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        // Fail due to X1 and X0 forming (XX, Xx) and (X0, X1)
        let rul_str1 = RuleStore::from_str("[00/01/X1, 00/00/X0]")?;

        let rul_str2 = RuleStore::from_str("[11/00/X0, 11/00/X1]")?;

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
        let rul_str1 = RuleStore::from_str("[11/01, 11/00]")?;

        println!("rul_str1 {rul_str1}");

        let rul_str2 = RuleStore::from_str("[00/10, 00/11]")?;

        println!("rul_str2 {rul_str2}");

        let rslt = rul_str1.compatible(&rul_str2);
        println!("rslt {rslt}");

        assert!(rslt == false);

        Ok(())
    }

    #[test]
    fn subcompatible() -> Result<(), String> {
        let rul_str1 = RuleStore::from_str("[11/11/11/xx, 11/11/10/xx]")?;

        // Test match with first rule.
        let rul_sub = RuleStore::from_str("[11/11/11/00]")?;
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} Ok"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b Some(0), not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} s/b Some(0), not None"),
        }

        // Test match with second rule.
        let rul_sub = RuleStore::from_str("[11/11/10/00]")?;
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b Some(1), not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} Ok"),
            _ => println!("{rul_str1} sub {rul_sub} s/b Some(0), not None"),
        }

        // Test match with no rules.
        let rul_sub = RuleStore::from_str("[10/11/11/00]")?;
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} Ok"),
        }

        // Test match with both rules.
        let rul_sub = RuleStore::from_str("[11/11/00/00]")?;
        match rul_str1.subcompatible_index(&rul_sub) {
            Some(0) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(0)"),
            Some(1) => println!("{rul_str1} sub {rul_sub} s/b None, not Some(1)"),
            _ => println!("{rul_str1} sub {rul_sub} Ok"),
        }

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let rulst1 = RuleStore::from_str("[]")?;
        println!("rulst1 {rulst1}");
        assert!(format!("{rulst1}") == "[]");

        let rulst2 = RuleStore::from_str("[00/01/XX]")?;
        println!("rulst2 {rulst2}");
        assert!(format!("{rulst2}") == "[00/01/XX]");

        let rulst3_str = "[X0/11/10/X1, X1/10/11/X0]";
        let rulst3 = RuleStore::from_str(&rulst3_str)?;
        println!("rulst3 {rulst3}");
        assert!(format!("{rulst3}") == rulst3_str);

        Ok(())
    }
}
