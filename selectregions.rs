//! Implement a struct of Select RegionStores.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::state::SomeState;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.to_string();
        if self.pos > 0 || self.neg > 0 {
            str.push_str(&format!(", positive: {}, negative: {}", self.pos, self.neg));
            // 123456789012  345678901234
        }
        if self.times_visited > 0 {
            str.push_str(&format!(", times visited {}", self.times_visited));
            //     1234567890123456
        }
        write!(f, "{}", str)
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of two, or more, domains.
    pub regions: RegionStore,
    /// A value for being in the select state.
    /// A Positive value is, so far, given to a goal state.
    pub pos: usize,
    /// A negative value is, so far, given to a plan that passes through the regions,
    /// not counting the beginning and end state.
    pub neg: usize,
    /// A cond of the number of time a SelectRegion has been visited due to satisfying a need.
    pub times_visited: usize,
}

impl Index<usize> for SelectRegions {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.regions[i]
    }
}

impl SelectRegions {
    /// Return a new SelectRegions instance.
    pub fn new(regions: RegionStore, pos: usize, neg: usize) -> Self {
        Self {
            regions,
            pos,
            neg,
            times_visited: 0,
        }
    }

    /// Return the aggregate value of a n instance.
    pub fn value(&self) -> isize {
        self.pos as isize - self.neg as isize
    }

    /// Increment times visited.
    pub fn inc_times_visited(&mut self) {
        self.times_visited += 1;
    }

    /// Return true if there is an intersection of corresponding regions, of two SelectRegions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.intersects_corr(&other.regions)
    }

    /// Return the intersection of two SelectRegions.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());

        self.regions
            .intersection_corr(&other.regions)
            .map(|regs| Self::new(regs, self.pos + other.pos, self.neg + other.neg))
    }

    /// Return true if at least one corresponding pair in two SelectRegions is adjacent,
    /// while other corresponding pairs are adjacent or intersect.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.is_adjacent_corr(&other.regions)
    }

    /// Return the adjacent part of two SelectRegions.
    /// Presumably, at least one pair of corresponding regions will be adjacent, calc the adjacent part.
    /// If a pair of corresponding regions intersect, calc the intersection.
    pub fn adjacent_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(other));

        Self::new(self.regions.adjacent_part_corr(&other.regions), 0, 0)
    }

    /// Calculate the distance between a SelectRegions and the current state.
    pub fn distance_states(&self, stas: &[&SomeState]) -> usize {
        debug_assert!(self.len() == stas.len());

        self.regions.distance_states_corr(stas)
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Add a Region.
    pub fn push(&mut self, regx: SomeRegion) {
        self.regions.push(regx);
    }

    /// Return true if a SelectRegions is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.regions.is_subset_of_corr(&other.regions)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of_states(&self, stas: &[&SomeState]) -> bool {
        self.regions.is_superset_states_corr(stas)
    }

    /// Return true if two SelectRegions regions match.
    pub fn regions_eq(&self, other: &Self) -> bool {
        self.regions.eq_corr(&other.regions)
    }

    /// Subtract two SelectRegions.
    pub fn subtract(&self, subtrahend: &Self) -> Vec<Self> {
        let mut ret_vec = Vec::<Self>::new();

        if self.is_subset_of(subtrahend) {
            return ret_vec;
        }

        if !self.intersects(subtrahend) {
            ret_vec.push(self.clone());
            return ret_vec;
        }

        let subvec = self.regions.subtract_corr(&subtrahend.regions);

        for regstx in subvec {
            ret_vec.push(SelectRegions::new(regstx, self.pos, self.neg));
        }
        ret_vec
    }
}

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SelectRegions {
    fn strlen(&self) -> usize {
        // Regions
        let mut ret = self.regions.strlen();
        if self.pos > 0 || self.neg > 0 {
            ret += 24 + format!("{}", self.pos).len() + format!("{}", self.neg).len();
        }
        if self.times_visited > 0 {
            ret += 16 + format!("{}", self.times_visited).len();
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::tools;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let ur_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let srs = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            0,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());

        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            1,
            20,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            0,
        );
        srs.times_visited = 1;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            5,
        );
        srs.times_visited = 11;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn test_subtract() -> Result<(), String> {
        let ur_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let regstr1 = SelectRegions::new(
            RegionStore::new(vec![ur_reg.new_from_string("r0xx1").expect("SNH")]),
            0,
            0,
        );

        let regstr2 = SelectRegions::new(
            RegionStore::new(vec![ur_reg.new_from_string("r001x").expect("SNH")]),
            0,
            0,
        );

        let rslt = regstr1.subtract(&regstr2);
        println!("rslt {}", tools::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![ur_reg.new_from_string("r0x01").expect("SNH")]),
                0,
                0
            )
        ));
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![ur_reg.new_from_string("r01x1").expect("SNH")]),
                0,
                0
            )
        ));

        let rslt = regstr2.subtract(&regstr1);
        println!("rslt {}", tools::vec_string(&rslt));
        assert!(rslt.len() == 1);
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![ur_reg.new_from_string("r0010").expect("SNH")]),
                0,
                0
            )
        ));

        let regstr1 = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0101").expect("SNH"),
            ]),
            0,
            0,
        );

        let regstr2 = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg.new_from_string("r001x").expect("SNH"),
                ur_reg.new_from_string("r0xx1").expect("SNH"),
            ]),
            0,
            0,
        );

        let rslt = regstr1.subtract(&regstr2);
        println!("rslt {}", tools::vec_string(&rslt));
        assert!(rslt.len() == 0);

        let rslt = regstr2.subtract(&regstr1);
        println!("rslt {}", tools::vec_string(&rslt));
        assert!(rslt.len() == 3);
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![
                    ur_reg.new_from_string("r0010").expect("SNH"),
                    ur_reg.new_from_string("r0xx1").expect("SNH")
                ]),
                0,
                0
            )
        ));
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![
                    ur_reg.new_from_string("r001x").expect("SNH"),
                    ur_reg.new_from_string("r0x11").expect("SNH")
                ]),
                0,
                0
            )
        ));
        assert!(tools::vec_contains(
            &rslt,
            SelectRegions::regions_eq,
            &SelectRegions::new(
                RegionStore::new(vec![
                    ur_reg.new_from_string("r001x").expect("SNH"),
                    ur_reg.new_from_string("r00x1").expect("SNH")
                ]),
                0,
                0
            )
        ));

        Ok(())
    }
}
