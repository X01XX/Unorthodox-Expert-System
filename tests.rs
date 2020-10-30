// tests for the ues
//
//use crate::action::SomeAction;
use crate::actions::action0;
//use crate::actionstore::ActionStore;
use crate::bits::SomeBits;
use crate::domain::SomeDomain;
use crate::region::SomeRegion;
use crate::state::SomeState;

//use std::io;
//use std::io::Write;
use std::process; // bring flush() into scope

pub fn _run_tests() {
    println!("Running Tests");

    println!(
        "\nTest1, form a group of two squares, pn=2, invalidate the group with pn=1 samples\n"
    );
    let rc = _test1();
    if rc == 0 {
        println!("Test1 OK");
    } else {
        println!("Test1 Failed at {}!", rc);
        process::exit(rc as i32);
    }

    println!(
        "\nTest2, form a group of two squares, pn=U, invalidate the group with pn=1 samples\n"
    );
    let rc = _test2();
    if rc == 0 {
        println!("Test2 OK");
    } else {
        println!("Test2 Failed at {}!", rc);
        process::exit(rc as i32);
    }

    println!("All tests are OK");
}

// Form a group, X1X1 from two squares that have alternating (pn=Two) results.
//
// Sample a square, 0111, in the group, once.  There should be no change.
//
// Sample the square a second time, with the same result, proving it cannot have an
// alternting result.
//
// Then group X1X1 should be invalidate and removed.
// **********************************************************************************
fn _test1() -> usize {
    let mut dmx = SomeDomain::new(SomeState::new(SomeBits::new_low(1)));
    dmx.add_action(action0);

    if let Ok(s5) = SomeState::new_from_string("s101", 1) {
        if let Ok(s4) = SomeState::new_from_string("s100", 1) {
            if let Ok(sf) = SomeState::new_from_string("s1111", 1) {
                if let Ok(se) = SomeState::new_from_string("s1110", 1) {
                    if let Ok(s7) = SomeState::new_from_string("s111", 1) {
                        if let Ok(rx1x1) = SomeRegion::new_from_string("rx1x1", 1) {
                            //println!(
                            //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
                            //    s5, s4, sf, se, rx1x1
                            //);
                            dmx.take_action_arbitrary(0, &s5, &s5);
                            dmx.take_action_arbitrary(0, &s5, &s4);
                            dmx.take_action_arbitrary(0, &sf, &se);
                            dmx.take_action_arbitrary(0, &sf, &sf);
                            if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                //println!("\nActs: {}", &dmx.actions[0]);
                                //println!("       Sqrs: ({})", dmx.actions[0].squares);
                                //println!("Group deleted too soon!");

                                //println!("sample-1 7");
                                dmx.take_action_arbitrary(0, &s7, &s7);

                                if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                    //println!("sample-2 7");
                                    dmx.take_action_arbitrary(0, &s7, &s7); // cause not-pn=2 condition
                                    if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                        //println!("\nActs: {}", &dmx.actions[0]);
                                        //println!(" {}", dmx.actions[0].squares);
                                        //println!(
                                        //     "Two samples for s7 failed to invalidate group xx1x1"
                                        // );
                                        return 1;
                                    } else {
                                        // println!("\nActs: {}", &dmx.actions[0]);
                                        // println!("       Sqrs: ({})", dmx.actions[0].squares);
                                        return 0;
                                    }
                                } else {
                                    //println!("\nActs: {}", &dmx.actions[0]);
                                    //println!("       Sqrs: ({})", dmx.actions[0].squares);
                                    //println!("Group deleted too soon!");
                                    return 2;
                                }
                            } else {
                                //  println!("\nActs: {}", &dmx.actions[0]);
                                //  println!("group rx1x1 was not formed by two squares!");
                                return 3;
                            }
                        }
                    }
                }
            }
        }
    }
    4
} // end test1

// Form a group, X1X1 from two squares that have unpredictable results.
//
// Sample a square, 0111, in the group, several times.  There should be no change until pnc (4 samples).
//
// Then group X1X1 should be invalidate and removed.
// **********************************************************************************
fn _test2() -> usize {
    let mut dmx = SomeDomain::new(SomeState::new(SomeBits::new_low(1)));
    dmx.add_action(action0);

    if let Ok(s5) = SomeState::new_from_string("s101", 1) {
        if let Ok(s4) = SomeState::new_from_string("s100", 1) {
            if let Ok(sf) = SomeState::new_from_string("s1111", 1) {
                if let Ok(se) = SomeState::new_from_string("s1110", 1) {
                    if let Ok(s7) = SomeState::new_from_string("s111", 1) {
                        if let Ok(rx1x1) = SomeRegion::new_from_string("rx1x1", 1) {
                            //println!(
                            //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
                            //    s5, s4, sf, se, rx1x1
                            //);
                            dmx.take_action_arbitrary(0, &s5, &s5);
                            dmx.take_action_arbitrary(0, &s5, &s4);
                            dmx.take_action_arbitrary(0, &s5, &se);

                            dmx.take_action_arbitrary(0, &sf, &se);
                            dmx.take_action_arbitrary(0, &sf, &sf);
                            dmx.take_action_arbitrary(0, &sf, &s4);

                            if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                println!("\nActs: {}", &dmx.actions[0]);
                                dmx.take_action_arbitrary(0, &s7, &s7);
                                dmx.take_action_arbitrary(0, &s7, &s7);
                                //dmx.take_action_arbitrary(0, &s7, &s7);
                                //println!("\nActs: {}", &dmx.actions[0]);
                                println!("Sqrs: {}", &dmx.actions[0].squares.stas_in_reg(&rx1x1));
                                if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                    dmx.take_action_arbitrary(0, &s7, &s7); // cause pnc = true
                                    if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                                        //println!("\nActs: {}", &dmx.actions[0]);
                                        //println!(" {}", dmx.actions[0].squares);
                                        //println!(
                                        //     "Two samples for s7 failed to invalidate group xx1x1"
                                        // );
                                        return 1;
                                    } else {
                                        // println!("\nActs: {}", &dmx.actions[0]);
                                        // println!("       Sqrs: ({})", dmx.actions[0].squares);
                                        return 0;
                                    }
                                } else {
                                    // println!("\nActs: {}", &dmx.actions[0]);
                                    // println!("       Sqrs: ({})", dmx.actions[0].squares);
                                    //  println!("Group deleted too soon!");
                                    return 2;
                                }
                            } else {
                                //  println!("\nActs: {}", &dmx.actions[0]);
                                //  println!("group rx1x1 was not formed by two squares!");
                                return 3;
                            }
                        }
                    }
                }
            }
        }
    }
    4
} // end test2
