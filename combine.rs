use crate::compare::Compare;
use crate::pn::Pn;
use crate::squarestore::SquareStore;
use crate::truth::Truth;

/// Can two structure rules, that implement the Compare trait (SomeSquare, SomeGroup), be combined?
///
/// This is one of the most tricky, and crucial, functions.
///
/// At program start, you want a few easy wins by combining squares
/// with only one sample each, even if a few combinations turn out to be invalid.
///
/// Combinations allow plans to be formed to get desired samples,
/// to improve understanding of the logic.
///
/// Combinations will be invalidated if they produce an unexpected result
/// when used in a plan.
///
/// No more than the last four samples are considered.
///
/// The pattern number (pn) and the number of samples are
/// deciding factors.
///
/// With continuing samples, a square might cycle through different
/// patterns, although that should be rare.
///
/// This returns three possible results: True, False or MoreSamplesNeeded.
///
/// If this function returns True, the next question is: Are there any squares
/// between them that will invalidate the combination?
///
/// For combinations of (square square), (group group) or (group square).
///
///

pub fn can_combine<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
    //println!("can_combine {} {}", &arg1.get_region(), &arg2.get_region());

    // Check for a subset/superset relationship.
    if arg1.get_region().is_subset_of(&arg2.get_region()) {
        //println!("Test ref 1a");
        return can_combine_subset(arg1, arg2);
    } else if arg2.get_region().is_subset_of(&arg1.get_region()) {
        //println!("Test ref 1b");
        return can_combine_subset(arg2, arg1);
    }

    // Special check for single result bootstrapping.
    if *arg1.get_pn_ref() == Pn::One && *arg2.get_pn_ref() == Pn::One {
        if arg1.get_rules_ref().can_form_union(arg2.get_rules_ref()) == Truth::T {
            //println!("Test ref 2");
            return Truth::T;
        }
    }

    // When both are not pnc, more samples needed.
    if arg1.get_pnc() == false && arg2.get_pnc() == false {
        //println!("Test ref 3");
        return Truth::M;
    }

    // When both are pnc.
    if arg1.get_pnc() && arg2.get_pnc() {
        //println!("Test ref 4");
        if *arg1.get_pn_ref() != *arg2.get_pn_ref() {
            return Truth::F;
        }
        if *arg1.get_pn_ref() == Pn::Unpredictable {
            return Truth::T;
        }
        // Return the Truth of their possible union or rules.
        return arg1.get_rules_ref().can_form_union(arg2.get_rules_ref());
    }

    // When arg1 one is pnc.
    if arg1.get_pnc() {
        //println!("Test ref 5");
        if *arg1.get_pn_ref() == Pn::Unpredictable {
            return Truth::M;
        }
        if *arg2.get_pn_ref() > *arg1.get_pn_ref() {
            return Truth::F;
        }
        return arg1.get_rules_ref().can_form_union(arg2.get_rules_ref());
    }

    // arg2 must be pnc.
    //println!("Test ref 6");
    if *arg2.get_pn_ref() == Pn::Unpredictable {
        return Truth::M;
    }
    if *arg1.get_pn_ref() > *arg2.get_pn_ref() {
        return Truth::F;
    }
    arg1.get_rules_ref().can_form_union(arg2.get_rules_ref())
} // end can_combine

/// Return Truth enum if arg1 is a subset of arg2.
pub fn can_combine_subset<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
    assert!(arg1.get_region().is_subset_of(&arg2.get_region()));

    // Handle Unpredictable superset.
    if *arg2.get_pn_ref() == Pn::Unpredictable {
        //println!("Test ref 1");
        if *arg1.get_pn_ref() == Pn::Unpredictable {
            return Truth::T;
        }
        if arg1.get_pnc() {
            return Truth::F;
        }
        return Truth::M;
    }

    // Handle subset Pn GT superset PN.
    if arg1.get_pn_ref() > arg2.get_pn_ref() {
        //println!("Test ref 2");
        return Truth::F;
    }

    // Handle subset pnc and Pn LT superset PN.
    if arg1.get_pn_ref() < arg2.get_pn_ref() {
        if arg1.get_pnc() {
            //println!("Test ref 3");
            return Truth::F;
        }
    }

    // Check rules subset.
    //println!("Test ref 4");
    if arg1.get_rules_ref().is_subset_of(arg2.get_rules_ref()) {
        return Truth::T;
    }
    return Truth::F;
}

/// Return Truth enum for the combination of any two structs implementing the Compare trait,
/// and the squares between them.
pub fn can_combine_check_between<T: Compare, U: Compare>(
    arg1: &T,
    arg2: &U,
    squares: &SquareStore,
) -> Truth {
    //println!("can_combine_check_between {} {}", &arg1.get_region(), &arg2.get_region());
    assert!(arg1.get_region() != arg2.get_region());

    // Check the two structs
    assert!(can_combine(arg1, arg2) == Truth::T);

    let reg1 = arg1.get_region();
    let reg2 = arg2.get_region();

    // Get keys for all squares in the region formed by the
    // two given structs.
    let stas = squares.stas_in_reg(&arg1.get_region().union(&arg2.get_region()));

    // Handle Pn::Unpredictable squares
    if *arg1.get_pn_ref() == Pn::Unpredictable {
        //println!("Test ref 1");
        // Check each between square
        for stax in stas.iter() {
            if reg1.is_superset_of_state(stax) || reg2.is_superset_of_state(stax) {
                continue;
            }

            let sqrz = squares.find(stax).unwrap();
            if sqrz.results.pn == Pn::Unpredictable {
            } else {
                if sqrz.results.pnc {
                    return Truth::F;
                }
            }
        }
        return Truth::T;
    }

    // Get rules union
    let rulsx = arg1.get_rules_ref().union(arg2.get_rules_ref()).unwrap();

    // Check squares between for compatibility to the rules.
    for stax in stas.iter() {
        if reg1.is_superset_of_state(stax) || reg2.is_superset_of_state(stax) {
            continue;
        }

        let sqrz = squares.find(stax).unwrap();

        if sqrz.results.pn != *arg1.get_pn_ref() {
            if sqrz.results.pnc {
                //println!("Test ref 2");
                return Truth::F;
            }
            if sqrz.results.pn > *arg1.get_pn_ref() {
                //println!("Test ref 3");
                return Truth::F;
            }
        }

        if sqrz.rules.is_subset_of(&rulsx) == false {
            //println!("Test ref 4");
            return Truth::F;
        }
    } // next stax
      //println!("Test ref 5");
    Truth::T
} // end can_combine_check_between

// Run various tests.
// The tests exercise each chunk of code.
// If code is changed, testing requires setting up a print statement in the exercised code, to insure the expected code is entered,
// and an intentional failure at the bottom of the test code, to allow seeing the printed confirmation.
#[cfg(test)]
mod tests {
    use crate::combine::*;
    use crate::group::SomeGroup;
    use crate::region::SomeRegion;
    use crate::rulestore::RuleStore;
    use crate::square::SomeSquare;
    use crate::squarestore::SquareStore;
    use crate::state::SomeState;
    use crate::truth::Truth;

    #[test]
    fn can_combine_check_between_ref1() -> Result<(), String> {
        //Create a SquareStore, as an argument to can_combine_check_between.
        let mut squares = SquareStore::new();

        // Create a Pn::Unpredictable square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s1110").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s1100").unwrap());
        squares.insert(sqr1, 0, 0);

        // Create second Pn::Unpredictable square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s0001").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        squares.insert(sqr2, 0, 0);

        // Create pn1, non-pnc square
        let sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0001").unwrap(),
            SomeState::new_from_string(1, "s0001").unwrap(),
        );
        squares.insert(sqr3, 0, 0);

        let sqr1: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqr2: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqr1, sqr2, &squares);

        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        // Create pn1, pnc square
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr4.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        squares.insert(sqr4, 0, 0);

        let sqr1: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqr2: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqr1, sqr2, &squares);

        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_check_between_ref2() -> Result<(), String> {
        //Create a SquareStore, as an argument to can_combine_check_between.
        let mut squares = SquareStore::new();

        // Create a Pn::Two square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s1110").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s1111").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s1110").unwrap());
        squares.insert(sqr1, 0, 0);

        // Create second Pn::Two square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0001").unwrap(),
            SomeState::new_from_string(1, "s0001").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s0000").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0001").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Create a Pn::One pnc square, between.
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        squares.insert(sqr3, 0, 0);

        let sqr1: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqr2: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0001").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqr1, sqr2, &squares);

        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_check_between_ref3() -> Result<(), String> {
        //Create a SquareStore, as an argument to can_combine_check_between.
        let mut squares = SquareStore::new();

        // Create a Pn::One pnc square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s1111").unwrap());
        squares.insert(sqr1, 0, 0);

        // Create second Pn::One pnc square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Create Pn::Two non-pnc square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0001").unwrap(),
            SomeState::new_from_string(1, "s0001").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s0000").unwrap());
        squares.insert(sqr3, 0, 0);

        let sqr1: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqr2: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqr1, sqr2, &squares);

        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_check_between_ref4() -> Result<(), String> {
        //Create a SquareStore, as an argument to can_combine_check_between.
        let mut squares = SquareStore::new();

        // Create a Pn::One pnc square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s1111").unwrap());
        squares.insert(sqr1, 0, 0);

        // Create second Pn::One pnc square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Create Pn::One non-pnc square
        let sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0001").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        squares.insert(sqr3, 0, 0);

        let sqr1: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqr2: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqr1, sqr2, &squares);

        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_check_between_ref5() -> Result<(), String> {
        //Create a SquareStore, as an argument to can_combine_check_between.
        let mut squares = SquareStore::new();

        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        squares.insert(sqr1, 0, 0);

        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        squares.insert(sqr2, 0, 0);

        let sqrx: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqry: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqrx, sqry, &squares);

        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        // Compatible square between.
        let sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0001").unwrap(),
            SomeState::new_from_string(1, "s0001").unwrap(),
        );
        squares.insert(sqr3, 0, 0);

        let sqrx: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s1111").unwrap())
            .unwrap();
        let sqry: &SomeSquare = squares
            .find(&SomeState::new_from_string(1, "s0000").unwrap())
            .unwrap();

        let rslt = can_combine_check_between(sqrx, sqry, &squares);

        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref1() -> Result<(), String> {
        // Create Pn::One square
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );

        // Create second Pn::One square
        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );

        // Create a Pn::One group
        let grp1 = SomeGroup::new(
            SomeRegion::new(&sqr1.state, &sqr2.state),
            sqr1.rules.union(&sqr2.rules).unwrap(),
            true,
        );

        let rslt = can_combine(&sqr1, &grp1);
        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        let rslt = can_combine(&grp1, &sqr1);
        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref2() -> Result<(), String> {
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::T {
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref3() -> Result<(), String> {
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1000").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s1001").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::M {
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref4() -> Result<(), String> {
        // Create Pn::Two, pnc, square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create Pn::One pnc square.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1000").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s1000").unwrap());

        let ret = can_combine(&sqr1, &sqr2);
        if ret != Truth::F {
            return Err(format!("ret: {}", ret));
        }

        // Create Pn::Unpredictable square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr3.add_result(SomeState::new_from_string(1, "s111").unwrap());

        // Create second Pn::U square
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s0111").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr4.add_result(SomeState::new_from_string(1, "s1010").unwrap());
        sqr4.add_result(SomeState::new_from_string(1, "s111").unwrap());

        let ret = can_combine(&sqr3, &sqr4);
        if ret != Truth::T {
            return Err(format!("ret: {}", ret));
        }

        // Create second Pn::One square
        let mut sqr5 = SomeSquare::new(
            SomeState::new_from_string(1, "s1000").unwrap(),
            SomeState::new_from_string(1, "s1000").unwrap(),
        );
        sqr5.add_result(SomeState::new_from_string(1, "s1000").unwrap());

        let ret = can_combine(&sqr3, &sqr4);
        if ret != Truth::T {
            return Err(format!("ret: {}", ret));
        }

        // Create third Pn::One square
        let mut sqr6 = SomeSquare::new(
            SomeState::new_from_string(1, "s1011").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );
        sqr6.add_result(SomeState::new_from_string(1, "s1001").unwrap());

        let ret = can_combine(&sqr2, &sqr6);
        if ret != Truth::F {
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref5() -> Result<(), String> {
        // Create Pn::U pnc square.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create Pn::One, non-pnc square.
        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::M {
            return Err(format!("ret: {}", ret));
        }

        // Create Pn::One pnc square.
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s1001").unwrap());

        // Create Pn::Two, non-pnc square.
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s1011").unwrap(),
            SomeState::new_from_string(1, "s1011").unwrap(),
        );
        sqr4.add_result(SomeState::new_from_string(1, "s1010").unwrap());

        let ret = can_combine(&sqr3, &sqr4);

        if ret != Truth::F {
            return Err(format!("ret: {}", ret));
        }

        // Create Pn::Two, pnc, square
        let mut sqr5 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr5.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create Pn::One non-pnc square.
        let sqr6 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );

        let ret = can_combine(&sqr5, &sqr6);

        if ret != Truth::M {
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn can_combine_ref6() -> Result<(), String> {
        // Create Pn::U pnc square.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create Pn::One, non-pnc square.
        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );

        let ret = can_combine(&sqr2, &sqr1);

        if ret != Truth::M {
            return Err(format!("ret: {}", ret));
        }

        // Create Pn::One pnc square.
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s1001").unwrap(),
            SomeState::new_from_string(1, "s1001").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s1001").unwrap());

        // Create Pn::Two, non-pnc square.
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s1011").unwrap(),
            SomeState::new_from_string(1, "s1011").unwrap(),
        );
        sqr4.add_result(SomeState::new_from_string(1, "s1010").unwrap());

        let ret = can_combine(&sqr4, &sqr3);

        if ret != Truth::F {
            return Err(format!("ret: {}", ret));
        }

        // Create Pn::Two, pnc, square
        let mut sqr5 = SomeSquare::new(
            SomeState::new_from_string(1, "s0011").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr5.add_result(SomeState::new_from_string(1, "s0010").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0011").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create Pn::One non-pnc square.
        let sqr6 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );

        let ret = can_combine(&sqr6, &sqr5);

        if ret != Truth::M {
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn can_combine_subset_ref1() -> Result<(), String> {
        // No previous requirements.

        // Create Pn::Unpredictable square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0001").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        // Create second Pn::Unpredictable square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s1110").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s1101").unwrap());

        // Create a Pn::Unpredictable group
        let grp1 = SomeGroup::new(
            SomeRegion::new(&sqr1.state, &sqr2.state),
            RuleStore::new(),
            true,
        );

        // Create Pn::One square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0010").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );

        // Test Pn::U subset Pn::U
        let rslt = can_combine_subset(&sqr2, &grp1);
        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        // Test Pn::1 pnc=false subset Pn::U
        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::M {
            return Err(format!("rslt: {}", rslt));
        }

        // Make sqr3 pnc=true
        sqr3.add_result(SomeState::new_from_string(1, "s0011").unwrap());

        // Test Pn::1 pnc=true subset Pn::U
        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_subset_ref2() -> Result<(), String> {
        //
        // Create Pn::One square
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );

        // Create second Pn::One square
        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1111").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );

        // Create a Pn::One group
        let grp1 = SomeGroup::new(
            SomeRegion::new(&sqr1.state, &sqr2.state),
            sqr1.rules.union(&sqr2.rules).unwrap(),
            true,
        );

        // Create Pn::Two square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s0001").unwrap());

        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_subset_ref3() -> Result<(), String> {
        // Create Pn::Two square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0001").unwrap());

        // Create second Pn::Two square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1110").unwrap(),
            SomeState::new_from_string(1, "s1110").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s1111").unwrap());

        // Create a Pn::Two group
        let grp1 = SomeGroup::new(
            SomeRegion::new(&sqr1.state, &sqr2.state),
            sqr1.rules.union(&sqr2.rules).unwrap(),
            true,
        );

        // Create Pn::One, pnc square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0010").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );
        sqr3.add_result(SomeState::new_from_string(1, "s0011").unwrap());

        // Test Pn::One pnc subset Pn::Two
        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }

    #[test]
    fn can_combine_subset_ref4() -> Result<(), String> {
        // Create Pn::Two square
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0000").unwrap(),
            SomeState::new_from_string(1, "s0000").unwrap(),
        );
        sqr1.add_result(SomeState::new_from_string(1, "s0001").unwrap());

        // Create second Pn::Two square
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s1110").unwrap(),
            SomeState::new_from_string(1, "s1110").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s1111").unwrap());

        // Create a Pn::Two group
        let grp1 = SomeGroup::new(
            SomeRegion::new(&sqr1.state, &sqr2.state),
            sqr1.rules.union(&sqr2.rules).unwrap(),
            true,
        );

        // Create Pn::One square
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0010").unwrap(),
            SomeState::new_from_string(1, "s0011").unwrap(),
        );

        // Test Pn::One subset Pn::Two
        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        // Test Pn::Two subset Pn::Two
        sqr3.add_result(SomeState::new_from_string(1, "s0010").unwrap());

        let rslt = can_combine_subset(&sqr3, &grp1);
        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }

        // Create Pn::One square
        let sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s0010").unwrap(),
            SomeState::new_from_string(1, "s1111").unwrap(),
        );

        // Test Pn::One not compatable subset Pn::Two
        let rslt = can_combine_subset(&sqr4, &grp1);
        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }

        Ok(())
    }
} // End tests
