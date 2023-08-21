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

const MAX_RESULTS: usize = 4; // Results for a two-result square can be seen twice, changing pnc to true.
                              // If three-result squares are to be supported, a one-result square would need three results before pnc = true, instead of two.
                              // So you could tell the difference between (1, 1, 1) and (1, 1, 2, 1, 1, 2).
                              // Some assumptions for one-result squares would need to be changed in other code.
                              // Better not to go there unless there is a really good reason.

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for ResultStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ResultStore {
    /// A vector to store sample results for one domain/action/state, that is a square.
    astore: Vec<SomeState>,
    /// Number results seen so far.
    num_results: usize,
}

impl ResultStore {
    /// Return a new ResultStore, with an initial result.
    pub fn new(mut st: Vec<SomeState>) -> Self {
        let mut ret = Self {
            astore: Vec::<SomeState>::with_capacity(MAX_RESULTS),
            num_results: st.len(),
        };
        ret.astore.append(&mut st);
        ret
    }

    /// Return the number of results in the store.
    pub fn len(&self) -> usize {
        self.astore.len()
    }

    /// Add a result to a circular buffer.
    /// Return the pattern number enum.
    pub fn add_result(&mut self, st: SomeState) -> Pn {
        if self.astore.len() < MAX_RESULTS {
            self.astore.push(st);
        } else {
            self.astore[self.num_results % MAX_RESULTS] = st;
        }

        self.num_results += 1;

        if self.num_results >= 100 {
            // increase, if needed, up to usize::MAX.
            panic!("To many samples of {self}");
        }

        self.calc_pn()
    }

    /// Return the first result.
    pub fn first(&self) -> &SomeState {
        &self.astore[0]
    }

    /// Return the second result.
    pub fn second(&self) -> &SomeState {
        &self.astore[1]
    }

    /// Return the most recent result.
    pub fn most_recent_result(&self) -> &SomeState {
        &self.astore[(self.num_results - 1) % MAX_RESULTS]
    }

    /// Calculate the Pattern Number, after adding a result.
    /// Due to the way the function is used, the minimum number of results will be two.
    fn calc_pn(&self) -> Pn {
        // Check for Pn::One.
        let mut pn_one = true;
        for inx in 1..self.astore.len() {
            if self.astore[inx] != self.astore[0] {
                pn_one = false;
            }
        }
        if pn_one {
            return Pn::One;
        }

        // Check for not Pn::Two
        if self.astore.len() > 2 {
            if self.astore[0] != self.astore[2] {
                return Pn::Unpredictable;
            }
            if self.astore.len() > 3 && self.astore[1] != self.astore[3] {
                return Pn::Unpredictable;
            }
        }

        Pn::Two
    }

    /// Return the expected length of a string to represent a ResultStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        rc_len += self.astore.len() * self.astore[0].formatted_string_length();
        if self.astore.len() > 1 {
            rc_len += (self.astore.len() - 1) * 2;
        }

        rc_len
    }

    /// Return a string to represent a ResultStore.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for (inx, rsltx) in self.astore.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &rsltx));
        }

        rc_str.push(']');

        rc_str
    }
    pub fn num_results(&self) -> usize {
        self.num_results
    }
} // end impl ResultStore

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    #[test]
    fn test_most_recent_result() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0, 0]));

        let sta1 = tmp_sta.new_from_string("s0x1")?;
        let sta2 = tmp_sta.new_from_string("s0x2")?;
        let sta3 = tmp_sta.new_from_string("s0x3")?;
        let sta4 = tmp_sta.new_from_string("s0x4")?;
        let sta5 = tmp_sta.new_from_string("s0x5")?;
        let sta6 = tmp_sta.new_from_string("s0x6")?;
        let sta7 = tmp_sta.new_from_string("s0x7")?;
        let sta8 = tmp_sta.new_from_string("s0x8")?;
        let sta9 = tmp_sta.new_from_string("s0x9")?;

        let mut rslt_str = ResultStore::new(vec![sta1.clone()]);
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
        let tmp_sta = SomeState::new(SomeBits::new(vec![0, 0]));

        let mut rslt_str = ResultStore::new(vec![tmp_sta.new_from_string("s0x505")?]);

        let pn = rslt_str.add_result(tmp_sta.new_from_string("s0x505")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::One);

        // Test additional adds.
        for _ in 0..8 {
            let pn = rslt_str.add_result(tmp_sta.new_from_string("s0x505")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::One);
        }

        assert!(rslt_str.len() == MAX_RESULTS);

        Ok(())
    }

    // Test ResultStore::add_result for Pn::Two
    #[test]
    fn add_result_pn_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0, 0]));

        let mut rslt_str = ResultStore::new(vec![tmp_sta.new_from_string("s0x505")?]);
        let mut pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(tmp_sta.new_from_string("s0x505")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        // Test additional adds.
        for _ in 0..4 {
            pn = rslt_str.add_result(tmp_sta.new_from_string("s0x505")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::Two);

            pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);
            println!("Pn: {pn} results: {rslt_str}");
            assert!(pn == Pn::Two);
        }

        assert!(rslt_str.len() == MAX_RESULTS);

        Ok(())
    }

    // Test ResultStore::add_result for Pn::Unpredictable
    #[test]
    fn add_result_pn_unpredictable() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0, 0]));

        // Test two different results but out of order.
        let mut rslt_str = ResultStore::new(vec![tmp_sta.new_from_string("s0x505")?]);
        let mut pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);

        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?); // two results, but out of order
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Unpredictable);

        // Test three different results.
        rslt_str = ResultStore::new(vec![tmp_sta.new_from_string("s0x505")?]);
        pn = rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Two);

        pn = rslt_str.add_result(tmp_sta.new_from_string("s0x502")?); // two results, but out of order
        println!("Pn: {pn} results: {rslt_str}");
        assert!(pn == Pn::Unpredictable);

        Ok(())
    }

    // Test ResultStore::add_result functions first, second, most_recent.
    #[test]
    fn add_result_misc() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0, 0]));

        let mut rslt_str = ResultStore::new(vec![tmp_sta.new_from_string("s0x500")?]);
        rslt_str.add_result(tmp_sta.new_from_string("s0x501")?);
        rslt_str.add_result(tmp_sta.new_from_string("s0x502")?);
        rslt_str.add_result(tmp_sta.new_from_string("s0x503")?);
        rslt_str.add_result(tmp_sta.new_from_string("s0x504")?);
        rslt_str.add_result(tmp_sta.new_from_string("s0x505")?);
        rslt_str.add_result(tmp_sta.new_from_string("s0x506")?);

        println!("results: {rslt_str}");
        assert!(*rslt_str.first() == tmp_sta.new_from_string("s0x504")?);
        assert!(*rslt_str.second() == tmp_sta.new_from_string("s0x505")?);
        assert!(*rslt_str.most_recent_result() == tmp_sta.new_from_string("s0x506")?);

        Ok(())
    }
}
