//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::pn::Pn;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&format!("{}", &self.state));
        rc_str.push_str(&format!(", pn: {}", &self.pn));
        if self.pnc {
            rc_str.push_str(", pnc: t");
        } else {
            rc_str.push_str(", pnc: f");
        }

        let ruls = if let Some(ruls_str) = &self.rules {
            format!("{}", ruls_str)
        } else {
            String::from("None")
        };
        rc_str.push_str(&format!(", {}", ruls));

        rc_str.push(']');

        write!(f, "{rc_str}")
    }
}

impl PartialEq for SomeSquare {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}
impl Eq for SomeSquare {}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]

/// A state, with the most recent results on a given action.
pub struct SomeSquare {
    pub state: SomeState,         // State that an action was taken on.
    pub results: ResultStore,     // Circular list of most recent results.
    pub rules: Option<RuleStore>, // Rules, None, 1 or 2 rules depending on pn
    pub pn: Pn,                   // Square Pattern number.
    pub pnc: bool,                // Pattern number confirmed.
}

impl SomeSquare {
    /// Return a new Square instance.
    pub fn new(state: SomeState, result_state: SomeState) -> Self {
        Self {
            state: state.clone(),
            results: ResultStore::new(vec![result_state.clone()]),
            rules: Some(RuleStore::new(vec![SomeRule::new(&state, &result_state)])),
            pn: Pn::One,
            pnc: false,
        }
    }

    /// Return a rate for a square.
    /// Squares that are pnc are equal to the minimum number of samples needed to reach pnc.
    /// Otherwise, the rate is the number of results stored.
    pub fn rate(&self) -> usize {
        if self.pnc {
            match self.pn {
                Pn::One => 2,
                Pn::Two => 4,
                Pn::Unpredictable => 3,
            }
        } else {
            self.len_results()
        }
    }

    /// Return a string representing a square.
    pub fn str_terse(&self) -> String {
        self.state.formatted_string()
    }

    /// Add a result to a square (4-item circular buffer).
    /// Return true if the addition changed the square, either the
    /// pn or pnc changed.  If there is a change, update the rules.
    pub fn add_result(&mut self, st: SomeState) -> bool {
        let mut str_info = String::from(&format!(
            "\n  Square {} adding result {} {}",
            self.str_terse(),
            self.results.num_results() + 1,
            &st
        ));

        let sav_pn = self.pn;
        let sav_pnc = self.pnc;

        self.pn = self.results.add_result(st);
        self.pnc = self.calc_pnc();

        let mut changed = false;
        if self.pn != sav_pn || self.pnc != sav_pnc {
            changed = true;

            match self.pn {
                Pn::One => {
                    self.rules = Some(RuleStore::new(vec![SomeRule::new(
                        &self.state,
                        self.results.first(),
                    )]));
                }
                Pn::Two => {
                    self.rules = Some(RuleStore::new(vec![
                        SomeRule::new(&self.state, self.results.first()),
                        SomeRule::new(&self.state, self.results.second()),
                    ]));
                }
                Pn::Unpredictable => {
                    self.rules = None;
                }
            }
        }

        if self.pn != sav_pn {
            str_info.push_str(&format!(", pn changed from {} to {}", &sav_pn, &self.pn));
        } else {
            str_info.push_str(&format!(", pn {}", &self.pn));
        }

        if self.pnc != sav_pnc {
            str_info.push_str(&format!(", pnc changed from {} to {}", &sav_pnc, &self.pnc));
        } else {
            str_info.push_str(&format!(", pnc {}", &self.pnc));
        }

        println!("{}", &str_info);
        changed
    }

    /// Return the number of results stored for the square.
    pub fn len_results(&self) -> usize {
        self.results.len()
    }

    /// Return the first result for the square.
    pub fn first_result(&self) -> &SomeState {
        self.results.first()
    }

    /// Return the second result for the square.
    pub fn second_result(&self) -> &SomeState {
        self.results.second()
    }

    pub fn num_results(&self) -> usize {
        self.results.num_results()
    }

    /// Return true if two squares are adjacent, that is they differ by exactly one bit.
    pub fn is_adjacent(&self, other: &SomeSquare) -> bool {
        self.state.is_adjacent(&other.state)
    }

    /// Return the most recent results
    pub fn most_recent_result(&self) -> &SomeState {
        self.results.most_recent_result()
    }

    /// Return a String representing a SomeSquare instance.
    pub fn formatted_string2(&self) -> String {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&format!("{}", &self.state));
        rc_str.push_str(&format!(", pn: {}", &self.pn));
        rc_str.push_str(&format!(", pnc: {}", &self.pnc));
        let ruls_str = if let Some(rules) = &self.rules {
            format!("{}", rules)
        } else {
            String::from("None")
        };
        rc_str.push_str(&format!(", {}", ruls_str));

        rc_str.push(']');
        rc_str
    }

    /// Check true if a square can be combined with some other, now.
    /// This does not check squares that may be between them.
    pub fn can_combine_now(&self, other: &SomeSquare) -> bool {
        //println!("running can_combine_now: {} {}", self.state, other.state);
        assert_ne!(self.state, other.state); // Combining a square with itself is probably an error in logic.
        if self.pn != other.pn {
            //println!("running can_combine: returns false (1)");
            return false;
        }

        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self
            .rules
            .as_ref()
            .expect("SNH")
            .can_combine(other.rules.as_ref().expect("SNH"))
        {
            //println!("running can_combine: returns calced {}", self.pn == Pn::One || self.pnc && other.pnc);
            return self.pn == Pn::One || self.pnc && other.pnc;
        }
        //println!("running can_combine: returns false (2)");
        false
    }

    /// Return true if a square and another, after failing the can_combine_now test,
    /// may combine after the other has more samples.
    /// This assumes no substantial future change to the method-initiating square.
    pub fn may_combine_later_to(&self, other: &SomeSquare) -> bool {
        //println!("running may_combine_later_to: {} {}", self.state, other.state);
        if other.pn > self.pn || other.pnc {
            return false;
        }

        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self
            .rules
            .as_ref()
            .expect("SNH")
            .can_form_union(other.rules.as_ref().expect("SNH"))
            != Some(false)
        {
            return true;
        }
        false
    }

    /// Return the Pattern Number Confirmed (pnc) value.
    fn calc_pnc(&self) -> bool {
        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self.pn == Pn::One && self.results.len() > 1 {
            return true;
        }

        if self.pn == Pn::Two && self.results.len() > 3 {
            return true;
        }
        false
    }

    /// Return true if a square is between two given squares, exclusive.
    pub fn is_between(&self, sqr1: &SomeSquare, sqr2: &SomeSquare) -> bool {
        self.state.is_between(&sqr1.state, &sqr2.state)
    }

    /// Return the distance (number of bits different) between two squares.
    pub fn distance(&self, other: &SomeSquare) -> usize {
        self.state.distance(&other.state)
    }

    /// Check if any square ref states  are between another two.
    /// So each item adds at least one X-bit position to a region formed by the states.
    pub fn vec_ref_check_for_unneeded(avec: &Vec<&SomeSquare>) -> bool {
        if avec.len() < 2 {
            return false;
        }
        for inx in 0..(avec.len() - 1) {
            for iny in (inx + 1)..avec.len() {
                for inz in 0..avec.len() {
                    if inz == inx || inz == iny {
                        continue;
                    }
                    let diff = (avec[inz].state.bitwise_xor(&avec[inx].state))
                        .to_mask()
                        .bitwise_and(&avec[inz].state.bitwise_xor(&avec[iny].state));
                    if diff.is_low() {
                        return true;
                    }
                    //println!("{} is not between {} and {}", avec[inz].state, avec[inx].state, avec[iny].state);
                } // next inz
            } // next iny
        } // next inx
        false
    }
} // end impl SomeSquare

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    // Test multiple additions to a square, cycle through all pn and pnc values.
    #[test]
    fn cycle_through_pn_pnc_values() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        let mut sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );
        assert_eq!(sqrx.pn, Pn::One);
        assert!(!sqrx.pnc);

        // Second result, same as the first.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Third result, same as the first two.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Fourth result, different from the first three, square becomes Unpredictable.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0100")?);
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Fifth result, same as the first, square remains Unpredictable.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Sixth result, same as the second most recent, square becomes Pn::Two.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0100")?);
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Seventh result, same as the second most recent, square stays Pn::Two.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Eighth result, same as the most recent, square becomes Pn::Unpredictable.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Nineth result, same as the most recent, square remains Pn::Unpredictable.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Tenth result, same as the most recent, square becomes Pn::One.
        let changed = sqrx.add_result(tmp_sta.new_from_string("s0b0101")?);
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        Ok(())
    }

    // Test can_combine, cannot compare the same square.
    #[test]
    #[should_panic(expected = "left != right")]
    fn can_combine_not_same() {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        let sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101").unwrap(),
            tmp_sta.new_from_string("s0b0101").unwrap(),
        );

        let sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101").unwrap(),
            tmp_sta.new_from_string("s0b0101").unwrap(),
        );

        sqr1.can_combine_now(&sqr2);

        ()
    }

    // Test can_combine Pn == One
    #[test]
    fn can_combine_pn_one_one() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Test two squares with only one sample each.
        // Allow a true result for bootstrapping.
        let sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        let sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );
        println!("sqr1: {sqr1}");
        println!("sqr2: {sqr2}");

        assert!(sqr1.can_combine_now(&sqr2));

        // Create a square incompatible to sqr1 to produce M result.
        let mut sqr3 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1100")?,
        );
        println!("sqr3: {sqr3}");

        assert!(!sqr1.can_combine_now(&sqr3));

        // Add to sqr3 to make it pnc.
        sqr3.add_result(tmp_sta.new_from_string("s0b1100")?);
        println!("sqr3: {sqr3}");

        assert!(!sqr1.can_combine_now(&sqr3));
        assert!(!sqr3.can_combine_now(&sqr1));

        Ok(())
    }

    // Test can_combine Pn == One and Pn == two
    #[test]
    fn can_combine_pn_one_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Create sqr1 pn 1 pnc f.
        let sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        // Create sqr2 pn 2 pnc f, one result compatible to sqr1.
        let mut sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );

        sqr2.add_result(tmp_sta.new_from_string("s0b1100")?);

        // Create sqr3 pn 1 pnc f, incompatible to any result in sqr2.
        let sqr3 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );

        println!("sqr1: {sqr1}");
        println!("sqr2: {sqr2}");
        println!("sqr3: {sqr3}");

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        assert!(sqr2.may_combine_later_to(&sqr1));

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc f.
        assert!(!sqr2.can_combine_now(&sqr3));

        // Create sqr4 pn 1 pnc t, compatible with one result in sqr2.
        let mut sqr4 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        sqr4.add_result(tmp_sta.new_from_string("s0b0101")?);
        println!("sqr4: {sqr4}");
        println!("sqr2: {sqr2}");

        // Test sqr4 pn 1 pnc t, sqr2 pn 2 pnc f.
        assert!(!sqr4.may_combine_later_to(&sqr2));

        // Exersize other branch of code.
        assert!(!sqr2.may_combine_later_to(&sqr4));

        // Add to sqr2 to make it pnc.
        sqr2.add_result(tmp_sta.new_from_string("s0b1101")?);
        sqr2.add_result(tmp_sta.new_from_string("s0b1100")?);

        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc t.
        assert!(sqr2.may_combine_later_to(&sqr1));

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc t.
        println!("sqr2 {}", sqr2);
        println!("sqr3 {}", sqr3);
        assert!(!sqr2.can_combine_now(&sqr3));

        // Exersize other branch of code.
        assert!(!sqr3.can_combine_now(&sqr2));

        Ok(())
    }

    // Test can_combine Pn == two
    #[test]
    fn can_combine_pn_two_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Create sqr1 pn 2 pnc f.
        let mut sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );

        sqr1.add_result(tmp_sta.new_from_string("s0b1100")?);

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        sqr2.add_result(tmp_sta.new_from_string("s0b0100")?);
        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);
        let cmb = sqr1.may_combine_later_to(&sqr2);
        println!("cmb {:?}", cmb);

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        assert!(cmb);

        // Create sqr3 pn 2 pnc f, not compatible with sqr1.
        let mut sqr3 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        sqr3.add_result(tmp_sta.new_from_string("s0b1101")?);
        println!("sqr1 {}", sqr1);
        println!("sqr3 {}", sqr3);
        let cmb = sqr1.can_combine_now(&sqr3);
        println!("cmb {:?}", cmb);

        // Test sqr1 pn 1 pnc f, sqr3 pn 2 pnc f.
        assert!(!cmb);

        // Make sqr1 pnc.
        sqr1.add_result(tmp_sta.new_from_string("s0b1101")?);
        sqr1.add_result(tmp_sta.new_from_string("s0b1100")?);

        // Test sqr1 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr1.can_combine_now(&sqr2) {
            return Err(String::from("Test 3 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_result(tmp_sta.new_from_string("s0b0101")?);
        sqr2.add_result(tmp_sta.new_from_string("s0b0100")?);

        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn 2 pnc t, sqr2 pn 2 pnc t.
        assert!(sqr1.can_combine_now(&sqr2));

        // Test sqr1 pn 2 pnc t, sqr3 pn 2 pnc f.
        assert!(!sqr1.can_combine_now(&sqr3));

        // Create sqr4, cause X0/X1 or Xx/XX combination error with sqr1.
        let mut sqr4 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );

        sqr4.add_result(tmp_sta.new_from_string("s0b0101")?);
        sqr4.add_result(tmp_sta.new_from_string("s0b0100")?);
        sqr4.add_result(tmp_sta.new_from_string("s0b0101")?);
        println!("sqr1 {}", sqr1);
        println!("sqr4 {}", sqr4);

        // Test sqr1 pn 2 pnc t, sqr4 pn 2 pnc t.
        assert!(!sqr1.can_combine_now(&sqr4));

        // Create sqr5, combinable with sqr1, but results in reverse order.
        let mut sqr5 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0100")?,
        );

        sqr5.add_result(tmp_sta.new_from_string("s0b0101")?);
        sqr5.add_result(tmp_sta.new_from_string("s0b0100")?);
        sqr5.add_result(tmp_sta.new_from_string("s0b0101")?);
        println!("sqr1 {}", sqr1);
        println!("sqr5 {}", sqr5);

        // Test sqr1 pn 2 pnc t, sqr5 pn 2 pnc t.
        assert!(sqr1.can_combine_now(&sqr5));

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 1
    #[test]
    fn can_combine_pn_u_one() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );

        sqr1.add_result(tmp_sta.new_from_string("s0b1100")?);
        sqr1.add_result(tmp_sta.new_from_string("s0b1000")?);

        // Create sqr2 pn 1 pnc f.
        let mut sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );
        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc f.
        assert!(!sqr1.can_combine_now(&sqr2));

        // Make sqr2 pnc == true.
        sqr2.add_result(tmp_sta.new_from_string("s0b0101")?);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc t.
        assert!(!sqr1.can_combine_now(&sqr2));

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 2
    #[test]
    fn can_combine_pn_u_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );

        sqr1.add_result(tmp_sta.new_from_string("s0b1100")?);
        sqr1.add_result(tmp_sta.new_from_string("s0b1000")?);

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        );
        sqr2.add_result(tmp_sta.new_from_string("s0b0100")?);
        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc f.
        let cmb = sqr1.can_combine_now(&sqr2);
        println!("cmb is {:?}", cmb);
        assert!(!sqr1.can_combine_now(&sqr2));

        // Make sqr2 pnc == true.
        sqr2.add_result(tmp_sta.new_from_string("s0b0101")?);
        sqr2.add_result(tmp_sta.new_from_string("s0b0100")?);
        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc t.
        assert!(!sqr1.can_combine_now(&sqr2));

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == Unpredictable
    #[test]
    fn can_combine_pn_u_u() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        );

        sqr1.add_result(tmp_sta.new_from_string("s0b1100")?);
        sqr1.add_result(tmp_sta.new_from_string("s0b1000")?);

        // Create sqr2 pn U pnc T.
        let mut sqr2 = SomeSquare::new(
            tmp_sta.new_from_string("s0b0001")?,
            tmp_sta.new_from_string("s0b0001")?,
        );

        sqr2.add_result(tmp_sta.new_from_string("s0b0010")?);
        sqr2.add_result(tmp_sta.new_from_string("s0b0100")?);
        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);

        // Test sqr1 pn U pnc t, sqr2 pn U pnc t.
        assert!(sqr1.can_combine_now(&sqr2));

        Ok(())
    }
}
