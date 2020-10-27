// Implement a store of groups

use crate::group::SomeGroup;
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter; // IterMut

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {
            if grpx.active {
                if flg == 1 {
                    rc_str.push_str(",\n              ");
                }
                rc_str.push_str(&format!("{}", &grpx));
                flg = 1;
            }
        }

        write!(f, "{}", rc_str)
    }
}

pub struct GroupStore {
    avec: Vec<SomeGroup>,
}

impl GroupStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeGroup>::with_capacity(5),
        }
    }

    // Check groups for a changed sqaure.
    // Return the number of active groups inactivated by a square
    pub fn check_square(&mut self, sqrx: &SomeSquare) -> RegionStore {
        let mut regs_invalid = RegionStore::new();

        for grpx in &mut self.avec {
            if grpx.active {
                if grpx.region.is_superset_of_state(&sqrx.state) {
                    if grpx.square_is_ok(&sqrx) == false {
                        println!("sqr {} invlidate group {}", sqrx, grpx.region);
                        regs_invalid.push(grpx.region.clone());
                        grpx.inactivate();
                    }
                }
            }
        }
        regs_invalid
    }

    // Return the number of active groups a state is in
    pub fn num_groups_state_in(&self, stax: &SomeState) -> usize {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.active {
                if grpx.region.is_superset_of_state(stax) {
                    num_grps += 1;
                }
            }
        }
        num_grps
    }

    // Return true if any group is a superset, or equal, to a region
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        for grpx in &self.avec {
            if grpx.active && reg.is_subset_of(&grpx.region) {
                return true;
            }
        }
        false
    }

    // Find and make inactive any subset groups
    fn inactivate_subsets_of(&mut self, reg: &SomeRegion) -> bool {
        let mut fnd = false;
        for grpx in &mut self.avec {
            if grpx.active && grpx.region.is_subset_of(&reg) {
                grpx.inactivate();
                // println!("Inactivating group {}", grpx.str_terse());
                fnd = true;
            }
        }

        fnd
    }

    // Find index to first inactive group, or return -1 is none found
    fn first_inactive_index(&mut self) -> i32 {
        let mut cnt = 0;
        for grpx in &mut self.avec {
            if grpx.active == false {
                return cnt;
            }
            cnt += 1;
        }

        -1
    }

    pub fn push(&mut self, grp: SomeGroup) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&grp.region) {
            println!("skipped adding group {}, a superset exists", grp.region);
            return false;
        }

        // Mark any subset groups as inactive
        self.inactivate_subsets_of(&grp.region);

        // Get index to the first inactive group
        let inx = self.first_inactive_index();

        // If no inactive group found, push the new group
        if inx < 0 {
            println!("Adding group  {}", grp);
            self.avec.push(grp);
        } else {
            // Replace the inactive group with the new group
            let inx = inx as usize;
            //println!("Deleting group {}", self.avec[inx].str_terse());
            println!("Adding group  {}", grp);
            self.avec[inx] = grp;
        }

        true
    }

    // Check groups
    // for a given sample.
    // Return the number of active groups that could be
    // inactivated by a square
    // A pn=One or Two group can be inactivated by a single sample,
    // A pn=Unpredictable cannot.  A sample for a step is usually not save, unless an
    // existing needed square has been saved.  A need for a contradictory intersection sample
    // can build up a pn=One or Two square that can invalidate a pn=Unpredictable group.
    pub fn check_sample(&mut self, init: &SomeState, rslt: &SomeState) -> usize {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.active {
                if grpx.region.is_superset_of_state(&init) {
                    if !grpx.sample_is_ok(&init, &rslt) {
                        num_grps += 1;
                    }
                }
            }
        }
        num_grps
    }

    pub fn regions(&self) -> RegionStore {
        let mut regs = RegionStore::new();

        for grpx in &self.avec {
            if grpx.active {
                regs.push(grpx.region.clone());
            }
        }
        regs
    }

    // Collect all group anchor states
    pub fn anchors(&self) -> StateStore {
        let mut stas = StateStore::new();

        for grpx in &self.avec {
            if grpx.active {
                if let Some(stax) = &grpx.anchor {
                    stas.push(stax.clone());
                }
            }
        }
        stas
    }

    // Return an iterator
    pub fn iter(&self) -> Iter<SomeGroup> {
        self.avec.iter()
    }

    //    pub fn iter_mut(&mut self) -> IterMut<SomeGroup> {
    //        self.avec.iter_mut()
    //    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn find_mut(&mut self, val: &SomeRegion) -> Option<&mut SomeGroup> {
        for grpx in &mut self.avec {
            if grpx.active && grpx.region == *val {
                return Some(grpx);
            }
        }
        None
    }

    pub fn find(&self, val: &SomeRegion) -> Option<&SomeGroup> {
        for grpx in &self.avec {
            if grpx.region == *val {
                return Some(grpx);
            }
        }
        None
    }

    // Return a steps that have any change intersection with a given aggregate rule.
    pub fn get_steps(&self, arule: &SomeRule, act_num: usize) -> StepStore {
        let mut stps = StepStore::new();

        let zero_chg = arule.b01.clone();
        let one_chg = arule.b10.clone();

        for grpx in &self.avec {
            if grpx.active == false {
                continue;
            }

            match grpx.pn {
                Pn::Unpredictable => {}
                _ => {
                    for rulx in grpx.rules.iter() {
                        // Find bit changes that are desired
                        let ones = rulx.b10.m_and(&one_chg);
                        let zeros = rulx.b01.m_and(&zero_chg);

                        if ones.is_not_low() || zeros.is_not_low() {
                            stps.push(SomeStep::new(act_num, 1, rulx.clone()));
                        }
                    } // end rule iter
                } // end _ match variant
            } // end match pn
        } // next grpx

        stps
    }

    // Inform each group of new X bits in the max_region
    pub fn new_x_bits(&mut self, bitsx: &SomeMask) {
        for grpx in &mut self.avec {
            grpx.new_x_bits(&bitsx);
        }
    }
}

impl Index<usize> for GroupStore {
    type Output = SomeGroup;
    fn index<'a>(&'a self, i: usize) -> &'a SomeGroup {
        &self.avec[i]
    }
}

impl IndexMut<usize> for GroupStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}
