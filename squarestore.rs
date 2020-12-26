// Implement a store for squares
//
use crate::combinable::Combinable;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

use std::collections::HashMap;
use std::fmt;

impl fmt::Display for SquareStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for (_key, sqrx) in &self.ahash {
            if flg == 1 {
                rc_str.push_str(",\n");
            }
            rc_str.push_str(&format!("{}", &sqrx));
            flg = 1;
        }

        write!(f, "{}", rc_str)
    }
}
pub struct SquareStore {
    ahash: HashMap<SomeState, SomeSquare>,
}

impl SquareStore {
    pub fn new() -> Self {
        Self {
            ahash: HashMap::new(),
        }
    }

    // Return a string for squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        let mut rc_store = StateStore::new();

        for (key, _) in &self.ahash {
            if areg.is_superset_of_state(&key) {
                rc_store.push(key.clone());
            }
        }
        rc_store
    }

    pub fn find_mut(&mut self, val: &SomeState) -> Option<&mut SomeSquare> {
        self.ahash.get_mut(val)
    }

    pub fn find(&self, val: &SomeState) -> Option<&SomeSquare> {
        self.ahash.get(val)
    }

    // Return rules after combining two squares, identified by their states.
    // May return an empty RuleStore, if the union is invalid
    pub fn rules(&self, sta1: &SomeState, sta2: &SomeState) -> Option<RuleStore> {
        let sqr1 = self.ahash.get(sta1).unwrap();
        let sqr2 = self.ahash.get(sta2).unwrap();
        return sqr1.rules.union(&sqr2.rules);
    }

    // Add a square that is not currently in the store.
    pub fn insert(&mut self, sqrx: SomeSquare) {
        self.ahash.insert(sqrx.state.clone(), sqrx);
    }

    // Given a region, look for possible two-square combinations that could
    // create the region.
    //
    // Return a StateStore with zero, or more, consecutive state pairs.
    pub fn possible_pairs_to_region(&self, regx: &SomeRegion) -> Option<StateStore> {
        let mut sta_pairs = StateStore::new();

        let stas = self.stas_in_reg(regx);

        let lenx = stas.len();

        for inx1 in 0..lenx {
            let sta1 = &stas[inx1];

            let sqrx = self.find(sta1).unwrap();
            for inx2 in (inx1 + 1)..lenx {
                let sta2 = &stas[inx2];

                let regy = SomeRegion::new(sta1, sta2);

                if regy == *regx {
                    let sqry = self.find(sta2).unwrap();
                    if sqry.pn() == Pn::Unpredictable {
                        let max_pnc = self.max_pn(&stas);
                        let min_pnc = self.min_pnc(&stas);

                        if max_pnc == min_pnc {
                            sta_pairs.push(sta1.clone());
                            sta_pairs.push(sta2.clone());
                        }
                    } else {
                        if let Some(rules_cmb) = sqrx.rules.union(&sqry.rules) {
                            if self.verify_combination(&regy, &rules_cmb, &sqrx.pn()) {
                                sta_pairs.push(sta1.clone());
                                sta_pairs.push(sta2.clone());
                            } else {
                                continue;
                            }
                        } else {
                            continue;
                        }
                    }
                }
            } // next inx2, sta2
        } // next inx1, sta1

        if sta_pairs.len() == 0 {
            return None;
        }

        Some(sta_pairs)
    }

    // Find squares whose rules can be combined with a given squares rules.
    // Check if any included squares invalidate a combination.
    // Remove subset combinations.
    // Return the regions resulting from successful combinations.
    pub fn possible_group_regions(&self, sqrx: &SomeSquare) -> RegionStore {
        let mut rsx = RegionStore::new();

        for (key, sqry) in &self.ahash {
            if *key == sqrx.state {
                continue;
            }

            if sqrx.pn() != sqry.pn() {
                continue;
            }

            let regx = SomeRegion::new(&sqrx.state, &sqry.state);

            match sqrx.pn() {
                Pn::Unpredictable => {
                    if self.verify_combination(&regx, &RuleStore::new(), &sqrx.pn()) {
                        rsx.push_nosubs(regx);
                    }
                }
                _ => {
                    if let Some(rules_cmb) = sqrx.rules.union(&sqry.rules) {
                        if self.verify_combination(&regx, &rules_cmb, &sqrx.pn()) {
                            //println!(
                            //    "Square {} can combine with {} giving {} {}",
                            //    sqrx.str_terse(),
                            //    sqry.str_terse(),
                            //    regx,
                            //    rules_cmb
                            //);
                            rsx.push_nosubs(regx);
                        } else {
                            //println!(
                            //    "Square {} cannot combine with {} due to other, included, squares",
                            //    sqrx.str_terse(),
                            //    sqry.str_terse()
                            //);
                        }
                    } else {
                        //println!(
                        //    "Square {} cannot combine with {}",
                        //    sqrx.str_terse(),
                        //    sqry.str_terse()
                        //);
                    }
                }
            } // end match
        } // end itoration

        for regx in rsx.iter() {
            let sqry = self.find(&regx.state2).unwrap();
            println!(
                "\nSquare {} can combine with\nSquare {}\ngiving {}\n",
                sqrx.str_terse(),
                sqry.str_terse(),
                regx,
            );
        }

        //println!("regions for new groups {}", rsx.str());
        rsx
    }

    // Verify if the squares in a region are compatible with rules
    // in  a RuleStore, made from two pn equal squares.
    //
    // If called for an Unpredictable region, the RuleStore will be empty.
    pub fn verify_combination(&self, regx: &SomeRegion, ruls: &RuleStore, pn: &Pn) -> bool {
        for (key, sqry) in &self.ahash {
            if regx.is_superset_of_state(&sqry.state) == false {
                continue;
            }

            if *key == regx.state1 || *key == regx.state2 {
                continue;
            }

            match pn {
                Pn::One => match sqry.pn() {
                    Pn::One => {
                        if sqry.rules.is_subset_of(ruls) == false {
                            return false;
                        }
                    }
                    Pn::Two => {
                        return false;
                    }
                    Pn::Unpredictable => {
                        return false;
                    }
                },
                Pn::Two => match sqry.pn() {
                    Pn::One => {
                        if sqry.pnc() || sqry.num_results() > 1 {
                            return false;
                        }
                        if sqry.rules.is_subset_of(ruls) == false {
                            return false;
                        }
                    }
                    Pn::Two => {
                        if sqry.rules.is_subset_of(ruls) == false {
                            return false;
                        }
                    }
                    Pn::Unpredictable => {
                        return false;
                    }
                },
                Pn::Unpredictable => match sqry.pn() {
                    Pn::One => {
                        if sqry.pnc() {
                            return false;
                        }
                    }
                    Pn::Two => {
                        if sqry.pnc() {
                            return false;
                        }
                    }
                    Pn::Unpredictable => {}
                },
            }
        } // next sqry

        true
    }

    pub fn not_in_regions(&self, regs: &RegionStore) -> StateStore {
        // TODO threads here?
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.any_superset_of_state(&key) == false {
                states.push(key.clone());
            }
        }

        states
    }

    pub fn states_in_1_region(&self, regs: &RegionStore) -> StateStore {
        // TODO threads here?
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.state_in_1_region(&key) {
                states.push(key.clone());
            }
        }

        states
    }

    // Return states that are adjacent to a given region.
    pub fn stas_adj_reg(&self, regx: &SomeRegion) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regx.diff_bits_state(&key).num_one_bits() == 1 {
                states.push(key.clone());
            }
        }

        states
    }

    // Return a StateStore of states of squares in a given region
    pub fn stas_in_regs(&self, regsx: &RegionStore) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            for regx in regsx.iter() {
                if regx.is_superset_of_state(&key) {
                    if states.contains(&key) {
                    } else {
                        states.push(key.clone());
                    }
                }
            }
        }

        states
    }

    // Given a list of square states, compare every pair of
    // squares and return the aggregate Combinable state.
    //    pub fn can_combine(&self, keys: &StateStore) -> Combinable {
    //        assert!(keys.len() > 0);
    //
    //        let mut msn_flg = false;
    //
    //        if keys.len() == 1 {
    //            return Combinable::True;
    //        }
    //
    //        for inx1 in 0..keys.len() {
    //            let key1 = keys[inx1];
    //
    //            if let Some(sqr1) = self.find(&key1) {
    //                for inx2 in (inx1 + 1)..keys.len() {
    //                    let key2 = keys[inx2];
    //
    //                    if let Some(sqr2) = self.find(&key2) {
    //                        match sqr1.can_combine(&sqr2) {
    //                            Combinable::False => {
    //                                return Combinable::False;
    //                            }
    //                            Combinable::MoreSamplesNeeded => {
    //                                msn_flg = true;
    //                            }
    //                            Combinable::True => {}
    //                        }
    //                    } else {
    //                        panic!("Square-2 {} not found?", &key2);
    //                    }
    //                }
    //            } else {
    //                panic!("Square-1 {} not found?", &key1);
    //            }
    //        }
    //
    //        if msn_flg {
    //            return Combinable::MoreSamplesNeeded;
    //        }
    //        Combinable::True
    //    }

    // Given a list of square states, compare every pair of
    // squares and return the aggregate Combinable state.
    pub fn no_incompatible_pairs(&self, keys: &StateStore) -> bool {
        assert!(keys.len() > 0);

        if keys.len() == 1 {
            return true;
        }

        for inx1 in 0..keys.len() {
            let key1 = &keys[inx1];

            let sqr1 = self.find(&key1).unwrap();
            for inx2 in (inx1 + 1)..keys.len() {
                let key2 = &keys[inx2];

                let sqr2 = self.find(&key2).unwrap();
                match sqr1.can_combine(&sqr2) {
                    Combinable::False => {
                        return false;
                    }
                    _ => {}
                }
            }
        }
        true
    }

    // Return the maximum Pn value of a set of squares,
    // identified by a list of their keys.
    pub fn max_pn(&self, keys: &StateStore) -> Pn {
        assert!(keys.len() > 0);

        let mut max_pn = Pn::One;

        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pn() > max_pn {
                max_pn = sqrx.pn();
            }
        }

        max_pn
    } // end max_pn

    // Return the maximum pnc pn value of a set of squares,
    // identified by a list of their keys.
    pub fn max_pnc(&self, keys: &StateStore) -> Pn {
        assert!(keys.len() > 0);

        let mut max_pn = Pn::One;
        let mut not_found = true;

        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pnc() {
                not_found = false;
                if sqrx.pn() > max_pn {
                    max_pn = sqrx.pn();
                }
            }
        }

        if not_found {
            panic!("No pnc square found in list!");
        }

        max_pn
    } // end max_pnc

    // Return the minimum Pn value of a set pnc squares,
    // identified by a list of their keys.
    pub fn min_pnc(&self, keys: &StateStore) -> Pn {
        assert!(keys.len() > 0);

        let mut min_pnc = Pn::Unpredictable;
        let mut not_found = true;

        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pnc() {
                not_found = false;
                if sqrx.pn() < min_pnc {
                    min_pnc = sqrx.pn();
                }
            }
        }

        if not_found {
            panic!("No pnc square found in list!");
        }
        min_pnc
    } // end min_pnc

    // Return first Pn of any square that has pnc set to true,
    // identified by a list of their keys.
    pub fn first_pnc_val(&self, keys: &StateStore) -> Option<Pn> {
        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pnc() {
                return Some(sqrx.pn());
            }
        }

        None
    }

    // Return true if any key value in a StateStore corresponds with
    // a square that has pnc set to true.
    pub fn any_pnc(&self, keys: &StateStore) -> bool {
        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pnc() {
                return true;
            }
        }

        false
    }

    pub fn rules_union(&self, keys: &StateStore, pn: Pn) -> Option<RuleStore> {
        let mut rcrs = RuleStore::new();

        for keyx in keys.iter() {
            let sqrx = self.find(&keyx).unwrap();
            if sqrx.pn() == pn {
                if rcrs.len() == 0 {
                    rcrs = sqrx.rules.clone();
                } else {
                    if let Some(rctmp) = rcrs.union(&sqrx.rules) {
                        rcrs = rctmp;
                    } else {
                        return None;
                    }
                }
            }
        }

        if rcrs.len() == 0 {
            return None;
        }
        Some(rcrs)
    }
}
