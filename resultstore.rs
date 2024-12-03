//! The ResultStore struct.  Saves up to 4 results of an action for a single state.
//!
//! Possible sequences of two (there could be more in practice) different result values:
//!
//! First Sample
//!  Pn::One  ['X']
//!  Pn::One  ['Y']
//!
//! Second Sample
//!  Pn::One  ['X', 'X'] from Pn::One  ['X'] + X
//!  Pn::One  ['Y', 'Y'] from Pn::One  ['Y'] + Y
//!  Pn::Two  ['X', 'Y'] from Pn::One  ['X'] + Y
//!  Pn::Two  ['Y', 'X'] from Pn::One  ['Y'] + X
//!
//! Third Sample
//!  Pn::One  ['X', 'X', 'X'] from Pn::One  ['X', 'X'] + X
//!  Pn::One  ['Y', 'Y', 'Y'] from Pn::One  ['Y', 'Y'] + Y
//!  Pn::Two  ['X', 'Y', 'X'] from Pn::Two  ['X', 'Y'] + X
//!  Pn::Two  ['Y', 'X', 'Y'] from Pn::Two  ['Y', 'X'] + Y
//!  Pn::None ['X', 'X', 'Y'] from Pn::One  ['X', 'X'] + Y
//!  Pn::None ['X', 'Y', 'Y'] from Pn::Two  ['X', 'Y'] + Y
//!  Pn::None ['Y', 'X', 'X'] from Pn::Two  ['Y', 'X'] + X
//!  Pn::None ['Y', 'Y', 'X'] from Pn::One  ['Y', 'Y'] + X
//!most_recent_result
//! Fourth Sample
//!  Pn::One  ['X', 'X', 'X', 'X'] from Pn::One  ['X', 'X', 'X'] + X
//!  Pn::One  ['Y', 'Y', 'Y', 'Y'] from Pn::One  ['Y', 'Y', 'Y'] + Y
//!  Pn::Two  ['X', 'Y', 'X', 'Y'] from Pn::Two  ['X', 'Y', 'X'] + Y
//!  Pn::Two  ['Y', 'X', 'Y', 'X'] from Pn::Two  ['Y', 'X', 'Y'] + X
//!  Pn::None ['X', 'X', 'X', 'Y'] from Pn::One  ['X', 'X', 'X'] + Y
//!  Pn::None ['X', 'X', 'Y', 'X'] from Pn::None ['X', 'X', 'Y'] + X
//!  Pn::None ['X', 'X', 'Y', 'Y'] from Pn::None ['X', 'X', 'Y'] + Y
//!  Pn::None ['X', 'Y', 'X', 'X'] from Pn::Two  ['X', 'Y', 'X'] + X
//!  Pn::None ['X', 'Y', 'Y', 'X'] from Pn::None ['X', 'Y', 'Y'] + X
//!  Pn::None ['X', 'Y', 'Y', 'Y'] from Pn::None ['X', 'Y', 'Y'] + Y
//!  Pn::None ['Y', 'X', 'X', 'X'] from Pn::None ['Y', 'X', 'X'] + X
//!  Pn::None ['Y', 'X', 'X', 'Y'] from Pn::None ['Y', 'X', 'X'] + Y
//!  Pn::None ['Y', 'X', 'Y', 'Y'] from Pn::Two  ['Y', 'X', 'Y'] + Y
//!  Pn::None ['Y', 'Y', 'X', 'X'] from Pn::None ['Y', 'Y', 'X'] + X
//!  Pn::None ['Y', 'Y', 'X', 'Y'] from Pn::None ['Y', 'Y', 'X'] + Y
//!  Pn::None ['Y', 'Y', 'Y', 'X'] from Pn::One  ['Y', 'Y', 'Y'] + X
//!
//! Fifth Sample (the first sample is dropped)
//!  Pn::One  ['X', 'X', 'X', 'X'] from Pn::One  ['X', 'X', 'X', 'X'] + X
//!  Pn::One  ['Y', 'Y', 'Y', 'Y'] from Pn::None ['X', 'Y', 'Y', 'Y'] + Y
//!  Pn::One  ['X', 'X', 'X', 'X'] from Pn::None ['Y', 'X', 'X', 'X'] + X
//!  Pn::One  ['Y', 'Y', 'Y', 'Y'] from Pn::One  ['Y', 'Y', 'Y', 'Y'] + Y
//!  Pn::Two  ['X', 'Y', 'X', 'Y'] from Pn::None ['X', 'X', 'Y', 'X'] + Y
//!  Pn::Two  ['Y', 'X', 'Y', 'X'] from Pn::Two  ['X', 'Y', 'X', 'Y'] + X
//!  Pn::Two  ['X', 'Y', 'X', 'Y'] from Pn::Two  ['Y', 'X', 'Y', 'X'] + Y
//!  Pn::Two  ['Y', 'X', 'Y', 'X'] from Pn::None ['Y', 'Y', 'X', 'Y'] + X
//!  Pn::None ['X', 'X', 'X', 'Y'] from Pn::One  ['X', 'X', 'X', 'X'] + Y
//!  Pn::None ['X', 'X', 'Y', 'X'] from Pn::None ['X', 'X', 'X', 'Y'] + X
//!  Pn::None ['X', 'X', 'Y', 'Y'] from Pn::None ['X', 'X', 'X', 'Y'] + Y
//!  Pn::None ['X', 'Y', 'X', 'X'] from Pn::None ['X', 'X', 'Y', 'X'] + X
//!  Pn::None ['X', 'Y', 'Y', 'X'] from Pn::None ['X', 'X', 'Y', 'Y'] + X
//!  Pn::None ['X', 'Y', 'Y', 'Y'] from Pn::None ['X', 'X', 'Y', 'Y'] + Y
//!  Pn::None ['Y', 'X', 'X', 'X'] from Pn::None ['X', 'Y', 'X', 'X'] + X
//!  Pn::None ['Y', 'X', 'X', 'Y'] from Pn::None ['X', 'Y', 'X', 'X'] + Y
//!  Pn::None ['Y', 'X', 'Y', 'Y'] from Pn::Two  ['X', 'Y', 'X', 'Y'] + Y
//!  Pn::None ['Y', 'Y', 'X', 'X'] from Pn::None ['X', 'Y', 'Y', 'X'] + X
//!  Pn::None ['Y', 'Y', 'X', 'Y'] from Pn::None ['X', 'Y', 'Y', 'X'] + Y
//!  Pn::None ['Y', 'Y', 'Y', 'X'] from Pn::None ['X', 'Y', 'Y', 'Y'] + X
//!  Pn::None ['X', 'X', 'X', 'Y'] from Pn::None ['Y', 'X', 'X', 'X'] + Y
//!  Pn::None ['X', 'X', 'Y', 'X'] from Pn::None ['Y', 'X', 'X', 'Y'] + X
//!  Pn::None ['X', 'X', 'Y', 'Y'] from Pn::None ['Y', 'X', 'X', 'Y'] + Y
//!  Pn::None ['X', 'Y', 'X', 'X'] from Pn::Two  ['Y', 'X', 'Y', 'X'] + X
//!  Pn::None ['X', 'Y', 'Y', 'X'] from Pn::None ['Y', 'X', 'Y', 'Y'] + X
//!  Pn::None ['X', 'Y', 'Y', 'Y'] from Pn::None ['Y', 'X', 'Y', 'Y'] + Y
//!  Pn::None ['Y', 'X', 'X', 'X'] from Pn::None ['Y', 'Y', 'X', 'X'] + X
//!  Pn::None ['Y', 'X', 'X', 'Y'] from Pn::None ['Y', 'Y', 'X', 'X'] + Y
//!  Pn::None ['Y', 'X', 'Y', 'Y'] from Pn::None ['Y', 'Y', 'X', 'Y'] + Y
//!  Pn::None ['Y', 'Y', 'X', 'X'] from Pn::None ['Y', 'Y', 'Y', 'X'] + X
//!  Pn::None ['Y', 'Y', 'X', 'Y'] from Pn::None ['Y', 'Y', 'Y', 'X'] + Y
//!  Pn::None ['Y', 'Y', 'Y', 'X'] from Pn::One  ['Y', 'Y', 'Y', 'Y'] + X
//!
//! End

use crate::pn::Pn;
use crate::state::SomeState;
use crate::statestore::StateStore;

const MAX_RESULTS: usize = 4; // Results for a two-result square can be seen twice, changing pnc to true.
                              // If three-result squares are to be supported, a one-result square would need three results before pnc = true, instead of two.
                              // So you could tell the difference between (1, 1, 1) and (1, 1, 2, 1, 1, 2).
                              // Some assumptions for one-result squares would need to be changed in other code.
                              // Better not to go there unless there is a really good reason.

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for ResultStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.states)
    }
}

type Resultint = usize;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]
pub struct ResultStore {
    /// A vector to store sample results for one domain/action/state, that is a square.
    states: StateStore,
    /// Number results seen so far.
    num_results: Resultint,
}

impl ResultStore {
    /// Return a new ResultStore, with an initial result.
    pub fn new(st: SomeState) -> Self {
        let mut states = StateStore::with_capacity(MAX_RESULTS);
        states.push(st);

        Self {
            states,
            num_results: 1,
        }
    }

    /// Return the number of results in the store.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Add a result to a circular buffer.
    /// Return the pattern number enum.
    pub fn add_result(&mut self, st: SomeState) -> Pn {
        debug_assert_eq!(st.num_bits(), self.num_bits());

        if self.states.len() < MAX_RESULTS {
            self.states.push(st);
        } else {
            self.states[self.num_results % MAX_RESULTS] = st;
        }

        // Wrap around check.
        if self.num_results == Resultint::MAX {
            self.num_results = MAX_RESULTS;
        } else {
            self.num_results += 1;
        }

        self.calc_pn()
    }

    /// Return the first result.
    pub fn first(&self) -> &SomeState {
        &self.states[0]
    }

    /// Return the second result.
    pub fn second(&self) -> &SomeState {
        &self.states[1]
    }

    /// Return the most recent result.
    pub fn most_recent_result(&self) -> &SomeState {
        &self.states[(self.num_results - 1) % MAX_RESULTS]
    }

    /// Calculate the Pattern Number, after adding a result.
    /// Due to the way the function is used, the minimum number of results will be two.
    fn calc_pn(&self) -> Pn {
        // Check for Pn::One.
        let mut pn_one = true;
        for inx in 1..self.states.len() {
            if self.states[inx] != self.states[0] {
                pn_one = false;
            }
        }
        if pn_one {
            return Pn::One;
        }

        // Check for not Pn::Two
        if self.states.len() > 2 {
            if self.states[0] != self.states[2] {
                return Pn::Unpredictable;
            }
            if self.states.len() > 3 && self.states[1] != self.states[3] {
                return Pn::Unpredictable;
            }
        }

        Pn::Two
    }

    /// Return the number of results so far.
    pub fn num_results(&self) -> usize {
        self.num_results
    }

    /// Return the number of bits used by ResultStore states.
    pub fn num_bits(&self) -> usize {
        self.states[0].num_bits()
    }

    /// Increment the number of results.
    pub fn inc_num_results(&mut self) {
        self.num_results += 1;
    }
} // end impl ResultStore

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn most_recent_result() -> Result<(), String> {
        let sta1 = SomeState::from_str("s0001")?;
        let sta2 = SomeState::from_str("s0010")?;
        let sta3 = SomeState::from_str("s0011")?;
        let sta4 = SomeState::from_str("s0100")?;
        let sta5 = SomeState::from_str("s0101")?;
        let sta6 = SomeState::from_str("s0110")?;
        let sta7 = SomeState::from_str("s0111")?;
        let sta8 = SomeState::from_str("s1000")?;
        let sta9 = SomeState::from_str("s1001")?;

        let mut rslt_str = ResultStore::new(sta1.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta1: {sta1}");
        assert!(*mrr == sta1);

        rslt_str.add_result(sta2.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta2: {sta2}");
        assert!(*mrr == sta2);

        rslt_str.add_result(sta3.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta3: {sta3}");
        assert!(*mrr == sta3);

        rslt_str.add_result(sta4.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta4: {sta4}");
        assert!(*mrr == sta4);

        rslt_str.add_result(sta5.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta5: {sta5}");
        assert!(*mrr == sta5);

        rslt_str.add_result(sta6.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta6: {sta6}");
        assert!(*mrr == sta6);

        rslt_str.add_result(sta7.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta7: {sta7}");
        assert!(*mrr == sta7);

        rslt_str.add_result(sta8.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta8: {sta8}");
        assert!(*mrr == sta8);

        rslt_str.add_result(sta9.clone());
        let mrr = rslt_str.most_recent_result();
        println!("mrr: {mrr} sta9: {sta9}");
        assert!(*mrr == sta9);

        Ok(())
    }

    // Test ResultStore::add_result for Pn::One
    #[test]
    fn add_result_pn_one() -> Result<(), String> {
        let mut rslt_str = ResultStore::new(SomeState::from_str("s0101_0000_0101")?);

        let pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0101")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::One);

        // Test additional adds.
        for _ in 0..8 {
            let pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0101")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::One);
        }

        assert!(rslt_str.len() == MAX_RESULTS);

        Ok(())
    }

    // Test ResultStore::add_result for Pn::Two
    #[test]
    fn add_result_pn_two() -> Result<(), String> {
        let mut rslt_str = ResultStore::new(SomeState::from_str("s0101_0000_0101")?);
        let mut pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0101")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        // Test additional adds.
        for _ in 0..4 {
            pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0101")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::Two);

            pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::Two);
        }

        assert!(rslt_str.len() == MAX_RESULTS);

        Ok(())
    }

    // Test ResultStore::add_result for Pn::Unpredictable
    #[test]
    fn add_result_pn_unpredictable() -> Result<(), String> {
        // Test two different results but out of order.
        let mut rslt_str = ResultStore::new(SomeState::from_str("s0101_0000_0101")?);
        let mut pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);

        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?); // two results, but out of order
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Unpredictable);

        // Test three different results.
        rslt_str = ResultStore::new(SomeState::from_str("s0101_0000_0101")?);
        pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(SomeState::from_str("s0101_0000_0010")?); // two results, but out of order
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Unpredictable);

        Ok(())
    }

    // Test ResultStore::add_result functions first, second, most_recent.
    #[test]
    fn add_result_misc() -> Result<(), String> {
        let mut rslt_str = ResultStore::new(SomeState::from_str("s0101_0000_0000")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0001")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0010")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0011")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0100")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0101")?);
        rslt_str.add_result(SomeState::from_str("s0101_0000_0110")?);

        println!("results: {rslt_str}");
        assert!(*rslt_str.first() == SomeState::from_str("s0101_0000_0100")?);
        assert!(*rslt_str.second() == SomeState::from_str("s0101_0000_0101")?);
        assert!(*rslt_str.most_recent_result() == SomeState::from_str("s0101_0000_0110")?);

        Ok(())
    }
}
