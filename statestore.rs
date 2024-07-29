//! The StateStore struct. A vector of SomeState structs.
//! Duplicates are suppressed.

use crate::bits::vec_same_num_bits;
use crate::mask::SomeMask;
use crate::state::SomeState;
use crate::tools::{self, anyxofn};

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStore {
    /// A vector of states.
    items: Vec<SomeState>,
}

impl StateStore {
    /// Return a new, empty, StateStore instance.
    pub fn new(items: Vec<SomeState>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        let mut ret = Self {
            items: Vec::<SomeState>::with_capacity(items.len()),
        };
        for stax in items {
            ret.push(stax);
        }
        ret
    }

    /// Return a mask of aggregate differences between states.
    fn x_mask(&self) -> SomeMask {
        assert!(self.is_not_empty());

        let mut xmask = self[0].to_mask().new_low();

        for stax in self.items.iter().skip(1) {
            xmask = xmask.bitwise_or(&stax.bitwise_xor(&self[0]));
        }
        xmask
    }

    /// Given a list of states, minimize the number of states needed io form a region.
    pub fn minimize(&self) -> Self {
        //println!("minimize {self}");
        if self.len() < 3 {
            return self.clone();
        }

        let target_x_mask = self.x_mask();
        //println!("target x-mask {target_x_mask}");
        for x in 2..self.len() {
            let options = anyxofn(x, self.len());
            //println!("options: {:?}", options);

            for optx in options.iter() {
                let mut xmask_tmp = self[optx[0]].to_mask().new_low();

                for itmx in optx.iter().skip(1) {
                    xmask_tmp = xmask_tmp.bitwise_or(&self[*itmx].bitwise_xor(&self[optx[0]]));
                }
                //println!("opt {:?} xmask_tmp {xmask_tmp}", optx);
                if xmask_tmp == target_x_mask {
                    let mut states = Vec::<SomeState>::with_capacity(x);
                    for itmx in optx.iter() {
                        states.push(self[*itmx].clone());
                    }
                    return StateStore { items: states };
                }
            }
        }
        // No lesser minimum found.
        self.clone()
    }

    /// Add a state to a StateStore.
    /// Do not allow duplicates.
    pub fn push(&mut self, val: SomeState) {
        debug_assert!(self.is_empty() || val.num_bits() == self.items[0].num_bits());

        if !self.contains(&val) {
            self.items.push(val);
        }
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.items.iter()
    }

    /// Return true if a StateStore contains a state.
    pub fn contains(&self, stax: &SomeState) -> bool {
        debug_assert!(self.is_empty() || stax.num_bits() == self.items[0].num_bits());

        self.items.contains(stax)
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return a reference to the first state.
    pub fn first(&self) -> Option<&SomeState> {
        if self.items.is_empty() {
            None
        } else {
            Some(&self.items[0])
        }
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.items[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minimize() -> Result<(), String> {
        let sta1 = SomeState::new_from_string("0b0001")?;
        let sta2 = SomeState::new_from_string("0b0010")?;
        let sta4 = SomeState::new_from_string("0b0100")?;
        let sta5 = SomeState::new_from_string("0b0101")?;
        let sta7 = SomeState::new_from_string("0b0111")?;

        let store = StateStore {
            items: vec![sta1.clone(), sta2.clone(), sta5.clone(), sta7.clone()],
        };

        let min_store = store.minimize();
        println!("min_store {}", min_store);

        assert!(min_store.len() == 2);
        assert!(min_store.contains(&sta2));
        assert!(min_store.contains(&sta5));

        let store = StateStore {
            items: vec![sta1.clone(), sta2.clone(), sta4.clone(), sta7.clone()],
        };

        let min_store = store.minimize();
        println!("min_store {}", min_store);

        assert!(min_store.len() == 3);
        assert!(min_store.contains(&sta1));
        assert!(min_store.contains(&sta2));
        assert!(min_store.contains(&sta4));

        //assert!(1 == 2);

        Ok(())
    }
}
