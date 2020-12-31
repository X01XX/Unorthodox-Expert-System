// tests for the ues
//

#[cfg(test)]
mod tests {

    //    use crate::actions::dom0_act0;
    use crate::bits::SomeBits;
    use crate::domain::SomeDomain;
    use crate::mask::mask_from_string;
    use crate::region::region_from_string;
    use crate::rule::region_to_region;
    use crate::square::SomeSquare;
    use crate::state::{state_from_string, SomeState};

    // Form a group, X1X1 from two squares that have alternating (pn=Two) results.
    //
    // Sample a square, 0111, in the group, once.  There should be no change.
    //
    // Sample the square a second time, with the same result, proving it cannot have an
    // alternting result.
    //
    // Then group X1X1 should be invalidated and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_2_union_then_invalidation() -> Result<(), String> {
        let mut dmx = SomeDomain::new(1, "s1", "r1");
        dmx.add_action(0);

        let s5 = state_from_string(dmx.num_ints, "s101").unwrap();

        let s4 = state_from_string(dmx.num_ints, "s100").unwrap();

        let sf = state_from_string(dmx.num_ints, "s1111").unwrap();

        let se = state_from_string(dmx.num_ints, "s1110").unwrap();

        let s7 = state_from_string(dmx.num_ints, "s111").unwrap();

        let rx1x1 = region_from_string(dmx.num_ints, "rx1x1").unwrap();

        //println!(
        //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
        //    s5, s4, sf, se, rx1x1
        //);

        dmx.take_action_arbitrary(0, &s5, &s5);
        dmx.take_action_arbitrary(0, &s5, &s4);
        dmx.take_action_arbitrary(0, &sf, &se);
        dmx.take_action_arbitrary(0, &sf, &sf);

        if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
            dmx.take_action_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                dmx.take_action_arbitrary(0, &s7, &s7); // cause not-pn=2 condition

                if let Some(_) = dmx.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dmx.actions[0]);
                    //println!(" {}", dmx.actions[0].squares);
                    return Err(String::from("failed, rx1x1 should have been deleted"));
                } else {
                    // println!("\nActs: {}", &dmx.actions[0]);
                    // println!("       Sqrs: ({})", dmx.actions[0].squares);
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dmx.actions[0]);
                //println!("       Sqrs: ({})", dmx.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dmx.actions[0]);
            return Err(String::from(
                "failed, group rx1x1 was not formed by two squares",
            ));
        }
    } // end group_pn_2_union_then_invalidation

    // Form a group, X1X1 from two squares that have unpredictable results.
    //
    // Sample a square, 0111, in the group, several times.  There should be no change until pnc (4 samples).
    //
    // Then group X1X1 should be invalidate and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_u_union_then_invalidation() -> Result<(), String> {
        let mut dmx = SomeDomain::new(1, "s1", "r1");
        dmx.add_action(0);

        let s5 = state_from_string(dmx.num_ints, "s101").unwrap();

        let s4 = state_from_string(dmx.num_ints, "s100").unwrap();

        let sf = state_from_string(dmx.num_ints, "s1111").unwrap();

        let se = state_from_string(dmx.num_ints, "s1110").unwrap();

        let s7 = state_from_string(dmx.num_ints, "s111").unwrap();

        let rx1x1 = region_from_string(dmx.num_ints, "rx1x1").unwrap();

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
            //println!("\nActs: {}", &dmx.actions[0]);
            dmx.take_action_arbitrary(0, &s7, &s7);
            dmx.take_action_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                dmx.take_action_arbitrary(0, &s7, &s7); // cause pnc = true
                if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dmx.actions[0]);
                    //println!(" {}", dmx.actions[0].squares);
                    return Err(String::from(
                        "Two samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                // println!("\nActs: {}", &dmx.actions[0]);
                // println!("       Sqrs: ({})", dmx.actions[0].squares);
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dmx.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    // Test subtraction of two regions
    //#[test]
    //fn test3() {
    //    let mut dmx = SomeDomain::new(1, "s1");
    //    dmx.add_action(action0, 0);
    //
    //    if let Ok(rx1x1) = region_from_string(dmx.num_ints, "rx1x1") {
    //        if let Ok(r1x0x) = region_from_string(dmx.num_ints, "r1x0x") {
    //            let subs = RegionStore {
    //                avec: rx1x1.subtract(&r1x0x),
    //            }; // subtract -> Vec<SomeRegion>
    //
    //            println!("{} - {} = ", &rx1x1, &r1x0x);
    //            for subx in subs.iter() {
    //                println!(
    //                    "sub: {} adj part {} is {}",
    //                    &subx,
    //                    &r1x0x,
    //                    &r1x0x.adj_part(&subx)
    //                );
    //            }
    //        } else {
    //            process::exit(1);
    //        }
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    process::exit(0);
    //} // end test3

    // Test check if a region is fully surrounded by adjacent groups
    //#[test]
    //fn test4() {
    //    let mut dmx = SomeDomain::new(1, "s1");
    //    dmx.add_action(action0, 0);
    //
    //    let mut regs = RegionStore::new();
    //
    //    if let Ok(r00x1) = region_from_string(dmx.num_ints, "r00x1") {
    //        // adjacent to rx1x1
    //        regs.push(r00x1);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(r1x1x) = region_from_string(dmx.num_ints, "r1x1x") {
    //       // intersects rx1x1
    //        regs.push(r1x1x);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(r0010) = region_from_string(dmx.num_ints, "r0010") {
    //        // not adjacent to rx1x1
    //        regs.push(r0010);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(rx1x1) = region_from_string(dmx.num_ints, "rx1x1") {
    //        regs.push(rx1x1.clone());
    //        let empty_adjacent: RegionStore = regs.empty_adjacent(&rx1x1);
    //
    //        println!(
    //            "Parts of {}, compared with other regions {}, lacking adjacent region are {}",
    //            &rx1x1, &regs, &empty_adjacent
    //        );
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    process::exit(0);
    //} // end test4

    // Test the successful union of two two-result rulestores
    #[test]
    fn pn_2_rules_union() -> Result<(), String> {
        let mut dmx = SomeDomain::new(1, "s1", "r1");
        dmx.add_action(0);

        let sta_5 = SomeState {
            bts: SomeBits {
                ints: vec![5 as u8],
            },
        };
        let sta_4 = SomeState {
            bts: SomeBits {
                ints: vec![4 as u8],
            },
        };
        let sta_f = SomeState {
            bts: SomeBits {
                ints: vec![15 as u8],
            },
        };
        let sta_e = SomeState {
            bts: SomeBits {
                ints: vec![14 as u8],
            },
        };

        // println!("sta_5 {} sta_4 {} sta_f {} sta_e {}", &sta_5, &sta_4, &sta_f, &sta_e);

        let mut sqr_5 = SomeSquare::new(sta_5.clone(), sta_5.clone());
        sqr_5.add_result(sta_4.clone());

        let mut sqr_e = SomeSquare::new(sta_e.clone(), sta_e.clone());
        sqr_e.add_result(sta_f.clone());

        // println!("sqr_5: {}\nsqr_e: {}", &sqr_5, &sqr_e);

        // XX / Xx
        if let Some(rules_5e) = sqr_e.rules.union(&sqr_5.rules) {
            //println!("union of sqr_5 rules and sqr_e rules is {}", &rules_5e);

            let sta_8 = SomeState {
                bts: SomeBits {
                    ints: vec![8 as u8],
                },
            };
            let sta_9 = SomeState {
                bts: SomeBits {
                    ints: vec![9 as u8],
                },
            };

            let mut sqr_8 = SomeSquare::new(sta_8.clone(), sta_9.clone());
            sqr_8.add_result(sta_8.clone());

            if let Some(rules_58) = sqr_8.rules.union(&sqr_5.rules) {
                //println!("union of sqr_5 rules and sqr_8 rules is {}", &rules_58);

                if let Some(rules_int) = rules_5e.intersection(&rules_58) {
                    //println!("rules_int {}", &rules_int);
                    if let Ok(rx10x) = region_from_string(dmx.num_ints, "rx10x") {
                        if rx10x == rules_int.initial_region() {
                            return Ok(());
                        }
                    }
                } else {
                    return Err(String::from("rules intersection failed"));
                }
            } else {
                return Err(String::from("cannot get union of 5 and 8"));
            }
        }
        Err(String::from("failed, at end"))
    } // end pn_2_rules_union

    #[test]
    fn region_to_region_test() -> Result<(), String> {
        let reg1 = region_from_string(1, "r00x11x").unwrap();

        let reg2 = region_from_string(1, "r010101").unwrap();

        let b01 = mask_from_string(1, "m00010001").unwrap();

        let b10 = crate::mask::mask_from_string(1, "m00001010").unwrap();

        let ragg = region_to_region(&reg1, &reg2);

        if b10 != ragg.b10 {
            return Err(String::from("b10 NEQ rule.b10"));
        }

        if b01 != ragg.b01 {
            return Err(String::from("b01 NEQ rule.b01"));
        }

        Ok(())
    } // end region_to_region_test

    #[test]
    fn shift_left() -> Result<(), String> {
        let bts1 = SomeBits {
            ints: vec![0 as u8, 129 as u8],
        };
        let bts2 = bts1.shift_left();
        let bts3 = SomeBits {
            ints: vec![1 as u8, 2 as u8],
        };

        if bts2 != bts3 {
            return Err(String::from("bts2 NEQ bts3"));
        }
        Ok(())
    }
} // end mod tests
