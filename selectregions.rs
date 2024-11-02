//! Implement a struct of SelectRegions.
//! This struct contains regions corresponding, in order, to domains in a DomainStore instance.
//!
//! Each instance has a given value.
//!
//! The regions have a boolean And relationship.
//! If only one region is non-maximum, that singles out that domain.

use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::statescorr::StatesCorr;
use crate::tools::StrLen;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.to_string();
        str.push_str(&format!(", {:+}", self.net_value));
        write!(f, "SR[{}]", str)
    }
}

impl PartialEq for SelectRegions {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (regx, regy) in self.regions.iter().zip(other.regions.iter()) {
            if regx != regy {
                return false;
            }
        }
        if self.pos_value != other.pos_value {
            return false;
        }
        if self.neg_value != other.neg_value {
            return false;
        }
        if self.net_value != other.net_value {
            return false;
        }
        true
    }
}
impl Eq for SelectRegions {}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of domains where the corsponding region is not all X.
    pub regions: RegionsCorr,
    /// A positive value for being in the select state.
    pub pos_value: isize,
    /// A negative value for being in the select state.
    pub neg_value: isize,
    /// A net value for being in the select state.
    pub net_value: isize,
}

impl Index<usize> for SelectRegions {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.regions[i]
    }
}

impl SelectRegions {
    /// Return a new SelectRegions instance.
    pub fn new(regions: RegionsCorr, value: isize) -> Self {
        debug_assert!(!regions.is_empty());
        debug_assert!(value != 0);

        if value < 0 {
            Self {
                regions,
                pos_value: 0,
                neg_value: value,
                net_value: value,
            }
        } else {
            Self {
                regions,
                pos_value: value,
                neg_value: 0,
                net_value: value,
            }
        }
    }

    /// Return the intersection of two SelectRegions.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());

        if let Some(regions) = self.regions.intersection(&other.regions) {
            let pos_value = self.pos_value + other.pos_value;
            let neg_value = self.neg_value + other.neg_value;
            let net_value = pos_value + neg_value;

            Some(Self {
                regions,
                pos_value,
                neg_value,
                net_value,
            })
        } else {
            None
        }
    }

    /// Calculate the distance between a SelectRegions and a vector of states.
    pub fn distance_states(&self, stas: &StatesCorr) -> usize {
        debug_assert!(self.len() == stas.len());

        self.regions.distance_states(stas)
    }

    /// Return the number of regions in a SelectRegions instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Add a Region.
    pub fn push(&mut self, regx: SomeRegion) {
        self.regions.push(regx);
    }

    /// Return true if a SelectRegions is a subset of another.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.is_adjacent(&other.regions)
    }

    /// Return a bridge between two adjacent SelectRegions.
    pub fn bridge(&self, other: &Self) -> Self {
        debug_assert!(self.len() == other.len());

        debug_assert!(self.regions.is_adjacent(&other.regions));

        Self::new(self.regions.bridge(&other.regions), 0)
    }

    /// Return true if a SelectRegions is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.is_subset_of(&other.regions)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of_states(&self, stas: &StatesCorr) -> bool {
        debug_assert!(self.len() == stas.len());

        self.regions.is_superset_states(stas)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.is_superset_of(&other.regions)
    }

    /// Return true if there is an intersection of corresponding regions, of two SelectRegions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.intersects(&other.regions)
    }

    // Set the positive value.
    pub fn set_values(&mut self, pos_value: isize, neg_value: isize) {
        debug_assert!(pos_value != 0 || neg_value != 0);

        self.pos_value = pos_value;
        self.neg_value = neg_value;
        self.net_value = pos_value + neg_value;
    }

    /// Subtract a SelectRegions from another.
    /// Fragments, if any, retain the same value.
    pub fn subtract(&self, other: &Self) -> Vec<Self> {
        // println!("subtract {other} from {self}");
        debug_assert!(self.len() == other.len());

        let mut ret_vec = vec![];

        if other.is_superset_of(self) {
            return ret_vec;
        } else if other.intersects(self) {
            let regs = self.regions.subtract(&other.regions);
            for regsz in regs {
                ret_vec.push(Self {
                    regions: regsz,
                    pos_value: self.pos_value,
                    neg_value: self.neg_value,
                    net_value: self.net_value,
                });
            }
        } else {
            return vec![self.clone()];
        }
        //println!("subtract {other} from {self} = ");
        //for selx in ret_vec.iter() {
        //    print!(" {selx}");
        //}
        //println!(" ");
        ret_vec
    }

    /// Return a SelectRegions instance, given a string representation.
    /// Like SR[RC[r1010], 1], or SR[RC[r101, r100], -1].
    pub fn from(sr_str: &str) -> Result<Self, String> {
        //println!("selectregions::from: {sr_str}");

        // Unwrap inner tokens.
        let mut inner_tokens = String::new();

        let mut last_chr = String::new();

        for (inx, chr) in sr_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "S" {
                    continue;
                } else {
                    return Err(format!(
                        "SelectRegions::from: Invalid string, {sr_str} should start with SR["
                    ));
                }
            }
            if inx == 1 {
                if chr == "R" {
                    continue;
                } else {
                    return Err(format!(
                        "SelectRegions::from: Invalid string, {sr_str} should start with SR["
                    ));
                }
            }
            if inx == 2 {
                if chr == "[" {
                    continue;
                } else {
                    return Err(format!(
                        "SelectRegions::from: Invalid string, {sr_str} should start with SR["
                    ));
                }
            }

            // Accumulate token value.
            inner_tokens.push_str(chr);
            last_chr = chr.to_string();
        } // next inx, chr

        if last_chr != "]" {
            return Err(format!(
                "SelectRegions::from: Invalid string, {sr_str} should end with ]"
            ));
        }

        inner_tokens.remove(inner_tokens.len() - 1); // Remove trailing "]" character.

        // Process inner tokens, into a RegionsCorr and value token.

        // Find last comma.
        let mut comma = 0;
        for (inx, chr) in inner_tokens.chars().enumerate() {
            if chr == ',' {
                comma = inx;
            }
        }
        if comma == 0 {
            return Err(format!(
                "SelectRegions::from: Invalid string, {sr_str} no comma found"
            ));
        }

        let mut rc_token = String::new();
        let mut tmp_token = String::new();

        for (inx, chr) in inner_tokens.chars().enumerate() {
            if chr == ' ' {
                continue;
            }
            if inx == comma {
                if !rc_token.is_empty() {
                    return Err(format!(
                        "SelectRegions::from: Invalid string, {sr_str} rc_token {rc_token}"
                    ));
                }
                rc_token = tmp_token;
                tmp_token = String::new();
                continue;
            }

            tmp_token.push(chr);
        }
        let val_token = tmp_token;

        // Check that there are two tokens.
        if rc_token.is_empty() || val_token.is_empty() {
            return Err(format!("SelectRegions::from: Invalid string, {sr_str}"));
        }

        println!("rc {rc_token} val {val_token}");

        // Get the regionscorr token value.
        let rcx = match RegionsCorr::from(&rc_token) {
            Ok(rcx) => rcx,
            Err(errstr) => return Err(format!("SelectRegions::from: {errstr}")),
        };

        // Get the value token value.
        let val = match val_token.parse::<isize>() {
            Ok(val) => val,
            Err(errstr) => return Err(format!("SelectRegions::from: {errstr}")),
        };

        Ok(Self::new(rcx, val))
    }
}

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SelectRegions {
    fn strlen(&self) -> usize {
        let mut ret = 4;
        // Add Regions.
        ret += self.regions.strlen();
        // Add separator.
        ret += 2;
        // Add value len.
        ret += if self.net_value.abs() < 10 { 2 } else { 3 };
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::selectregionsstore::SelectRegionsStore;

    #[test]
    fn strlen() -> Result<(), String> {
        let srs = SelectRegions::from("SR[RC[r0xx1, r0x1x], 1]")?;
        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::from("SR[RC[r0xx1, r0x1x], -19]")?;
        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::from("SR[RC[r0xx1, r0x1x], 1]")?;
        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::from("SR[RC[r0xx1, r0x1x], -5]")?;
        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let srs1 = SelectRegions::from("SR[RC[r0xx1, r1x1x], 3]")?;
        println!("srs1 {srs1}");

        let srs2 = SelectRegions::from("SR[RC[r0x1x, r1xx1], -5]")?;
        println!("srs2 {srs2}");

        let srs3 = SelectRegions::from("SR[RC[r0x1x, r0x1x], -5]")?;
        println!("srs3 {srs3}");

        if let Some(srsint) = srs1.intersection(&srs2) {
            println!("srs1 int srs2 = {srsint}");
            assert!(srsint.regions == RegionsCorr::from("RC[r0x11, r1x11]")?);
            assert!(srsint.pos_value == 3);
            assert!(srsint.neg_value == -5);
            assert!(srsint.net_value == -2);
        } else {
            return Err("No intersection of srs1 and srs2".to_string());
        }

        if let Some(srsint) = srs1.intersection(&srs3) {
            return Err(format!("srs1 int? srs3 = {srsint}"));
        }

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let srs1 = SelectRegions::from("SR[RC[r0xx1, r1x1x], 3]")?;
        println!("srs1 {srs1}");

        let srs2 = SelectRegions::from("SR[RC[r0x1x, r1xx1], -5]")?;
        println!("srs2 {srs2}");

        let srs3 = SelectRegions::from("SR[RC[r0x1x, r0x1x], -5]")?;
        println!("srs3 {srs3}");

        let srssub = srs1.subtract(&srs2);
        println!("srs1 sub srs2:");
        for srsx in srssub.iter() {
            println!("    {}", srsx);
        }
        assert!(srssub.len() == 2);
        let srsx = SelectRegions {
            regions: RegionsCorr::from("RC[r0x01, r1x1x]")?,
            pos_value: 3,
            neg_value: 0,
            net_value: 3,
        };
        assert!(srssub.contains(&srsx));

        let srsy = SelectRegions {
            regions: RegionsCorr::from("RC[r0xx1, r1x10]")?,
            pos_value: 3,
            neg_value: 0,
            net_value: 3,
        };
        assert!(srssub.contains(&srsy));

        let srssub = srs1.subtract(&srs3);
        println!("srs1 sub srs3:");
        for srsx in srssub.iter() {
            println!("    {}", srsx);
        }
        assert!(srssub.len() == 1);
        assert!(srssub[0] == srs1);

        Ok(())
    }

    #[test]
    fn subtract2() -> Result<(), String> {
        let srs1 = SelectRegions::from("SR[RC[rXXXX], 3]")?;
        println!("srs1 {srs1}");

        let srs2 = SelectRegions::from("SR[RC[r0xx1], -5]")?;
        println!("srs2 {srs2}");

        let srs3 = SelectRegions::from("SR[RC[rxx0x], -5]")?;
        println!("srs3 {srs3}");

        let srssub12 = SelectRegionsStore::new(srs1.subtract(&srs2));
        println!("srs1 {srs1} - {srs2} = {srssub12}");

        let srssub13 = SelectRegionsStore::new(srs1.subtract(&srs3));
        println!("srs1 {srs1} - {srs3} = {srssub13}");

        Ok(())
    }
}
