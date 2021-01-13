// tests for the ues
//

#[cfg(test)]
mod tests {

    use crate::bits::SomeBits;
    use crate::bitsstore::BitsStore;
    use crate::domain::SomeDomain;
    use crate::mask::SomeMask;
    use crate::maskstore::MaskStore;
    use crate::region::region_from_string;
    use crate::regionstore::RegionStore;
    use crate::resultstore::ResultStore;
    use crate::rule::{region_to_region, SomeRule};
    use crate::rulestore::RuleStore;
    use crate::square::SomeSquare;
    use crate::state::{state_from_string, SomeState};
    use crate::statestore::StateStore;
    use crate::step::SomeStep;
    use crate::stepstore::StepStore;

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
            dmx.take_action_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                dmx.take_action_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dmx.actions[0].groups.find(&rx1x1) {
                    println!("\nActs: {}", &dmx.actions[0]);
                    println!(" {}", dmx.actions[0].squares);
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
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

        let b01 = SomeMask {
            bts: SomeBits {
                ints: vec![17 as u8],
            },
        };

        let b10 = SomeMask {
            bts: SomeBits {
                ints: vec![10 as u8],
            },
        };

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

    #[test]
    fn bits_string_length() -> Result<(), String> {
        let bts1 = SomeBits {
            ints: vec![129 as u8],
        };

        let n1 = bts1.formatted_string_length();
        let rs = bts1.formatted_string('b');
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeBits string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let bts2 = SomeBits {
            ints: vec![129 as u8, 18 as u8],
        };

        let n1 = bts2.formatted_string_length();
        let rs = bts2.formatted_string('b');
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeBits string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut btst = BitsStore { avec: vec![] };

        let n1 = btst.formatted_string_length();
        let rs = btst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty BitsStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        btst = BitsStore {
            avec: vec![SomeBits {
                ints: vec![12 as u8],
            }],
        };
        let n1 = btst.formatted_string_length();
        let rs = btst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of BitsStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        btst = BitsStore {
            avec: vec![
                SomeBits {
                    ints: vec![12 as u8],
                },
                SomeBits {
                    ints: vec![11 as u8],
                },
            ],
        };
        let n1 = btst.formatted_string_length();
        let rs = btst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of BitsStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn state_string_length() -> Result<(), String> {
        let sta1 = SomeState {
            bts: SomeBits {
                ints: vec![129 as u8],
            },
        };

        let n1 = sta1.formatted_string_length();
        let rs = sta1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeState string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let sta2 = SomeState {
            bts: SomeBits {
                ints: vec![129 as u8, 18 as u8],
            },
        };

        let n1 = sta2.formatted_string_length();
        let rs = sta2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeState string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut stst = StateStore::new();

        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty StateStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        stst.push(SomeState {
            bts: SomeBits {
                ints: vec![12 as u8],
            },
        });
        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        stst.push(SomeState {
            bts: SomeBits {
                ints: vec![1 as u8],
            },
        });
        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn mask_string_length() -> Result<(), String> {
        let msk1 = SomeMask {
            bts: SomeBits {
                ints: vec![129 as u8],
            },
        };

        let n1 = msk1.formatted_string_length();
        let rs = msk1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeMask string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let msk2 = SomeMask {
            bts: SomeBits {
                ints: vec![129 as u8, 18 as u8],
            },
        };

        let n1 = msk2.formatted_string_length();
        let rs = msk2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeMask string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut mkst = MaskStore { avec: vec![] };

        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty MaskStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        mkst = MaskStore {
            avec: vec![SomeMask {
                bts: SomeBits {
                    ints: vec![12 as u8],
                },
            }],
        };
        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of MaskStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        mkst = MaskStore {
            avec: vec![
                SomeMask {
                    bts: SomeBits {
                        ints: vec![12 as u8],
                    },
                },
                SomeMask {
                    bts: SomeBits {
                        ints: vec![12 as u8],
                    },
                },
            ],
        };
        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn region_string_length() -> Result<(), String> {
        let reg1 = region_from_string(1, "rx1x1").unwrap();

        let n1 = reg1.formatted_string_length();
        let rs = reg1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRegion string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let reg2 = region_from_string(2, "rx1x1").unwrap();

        let n1 = reg2.formatted_string_length();
        let rs = reg2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRegion string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut regt = RegionStore { avec: vec![] };

        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty RegionStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        regt = RegionStore {
            avec: vec![region_from_string(2, "rx1x1").unwrap()],
        };
        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RegionStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        regt = RegionStore {
            avec: vec![
                region_from_string(2, "rx1x1").unwrap(),
                region_from_string(2, "rx1x1").unwrap(),
            ],
        };
        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RegionStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn step_string_length() -> Result<(), String> {
        let stp1 = SomeStep {
            act_num: 0,
            initial: region_from_string(1, "r1011").unwrap(),
            result: region_from_string(1, "r1010").unwrap(),
            group_reg: region_from_string(1, "rx0x1").unwrap(),
            rule: SomeRule::new(
                &SomeState {
                    bts: SomeBits {
                        ints: vec![11 as u8],
                    },
                },
                &SomeState {
                    bts: SomeBits {
                        ints: vec![10 as u8],
                    },
                },
            ),
            alt_rule: false,
        };

        let n1 = stp1.formatted_string_length();
        let rs = stp1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeStep string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stp2 = SomeStep {
            act_num: 0,
            initial: region_from_string(2, "r1011").unwrap(),
            result: region_from_string(2, "r1010").unwrap(),
            group_reg: region_from_string(2, "rx0x1").unwrap(),
            rule: SomeRule::new(
                &SomeState {
                    bts: SomeBits {
                        ints: vec![0 as u8, 11 as u8],
                    },
                },
                &SomeState {
                    bts: SomeBits {
                        ints: vec![0 as u8, 10 as u8],
                    },
                },
            ),
            alt_rule: false,
        };

        let n1 = stp2.formatted_string_length();
        let rs = stp2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeStep string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore { avec: vec![] };

        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty StepStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore {
            avec: vec![stp2.clone()],
        };
        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StepStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore {
            avec: vec![stp2.clone(), stp2.clone()],
        };
        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StepStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn rule_string_length() -> Result<(), String> {
        let rul1 = SomeRule::new(
            &SomeState {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
            &SomeState {
                bts: SomeBits {
                    ints: vec![1 as u8],
                },
            },
        );

        let n1 = rul1.formatted_string_length();
        let rs = rul1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRule string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rul2 = SomeRule::new(
            &SomeState {
                bts: SomeBits {
                    ints: vec![0 as u8, 1 as u8],
                },
            },
            &SomeState {
                bts: SomeBits {
                    ints: vec![2 as u8, 3 as u8],
                },
            },
        );

        let n1 = rul2.formatted_string_length();
        let rs = rul2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeRule string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore { avec: vec![] };

        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty RuleStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore {
            avec: vec![rul2.clone()],
        };
        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RuleStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore {
            avec: vec![rul2.clone(), rul2.clone()],
        };
        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RuleStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn resultstore_string_length() -> Result<(), String> {
        let rsltst = ResultStore::new(SomeState {
            bts: SomeBits {
                ints: vec![0 as u8, 1 as u8],
            },
        });
        let n1 = rsltst.formatted_string_length();
        let rs = rsltst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int ResultStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    /// Given a square and a two-rule RuleStore,
    /// return the expected next result for the square.
    #[test]
    fn predict_next_result() -> Result<(), String> {
        let s5 = state_from_string(1, "s101").unwrap();

        let s4 = state_from_string(1, "s100").unwrap();

        let sf = state_from_string(1, "s1111").unwrap();

        let se = state_from_string(1, "s1110").unwrap();

        // Build 2-result square 1
        let mut sqr1 = SomeSquare::new(s5.clone(), s5.clone());
        sqr1.add_result(s4.clone());

        // Build 2-result square 2
        let mut sqr2 = SomeSquare::new(sf.clone(), se.clone());
        sqr2.add_result(sf.clone());

        // Build rules from sqr1 and sqr2
        let ruls = sqr1.rules.union(&sqr2.rules).unwrap();

        // Get the next result, really the previous-to-last result, of square 1 (fastest)
        let next_result = sqr1.next_result(&ruls);

        if next_result != s5 {
            return Err(format!(
                "ruls: {} sqr: {} next rslt: {}",
                &ruls, &sqr1, &next_result
            ));
        }

        // Build 1-result square 3
        let sqr3 = SomeSquare::new(s5.clone(), s5.clone());

        // Get next result of square 3 using the last(and only) result and the given rules.
        let next_result = sqr3.next_result(&ruls);

        if next_result != s4 {
            return Err(format!(
                "ruls: {} sqr: {} next rslt: {}",
                &ruls, &sqr3, &next_result
            ));
        }

        // Build 1-result square 4
        let sqr4 = SomeSquare::new(s5.clone(), s4.clone());

        // Get next result of square 4 using the last(and only) result and the given rules.
        let next_result = sqr4.next_result(&ruls);

        if next_result != s5 {
            return Err(format!(
                "ruls: {} sqr: {} next rslt: {}",
                &ruls, &sqr3, &next_result
            ));
        }

        Ok(())
    }

    // Check two-result squares combination does not infer two changes
    // but one or the other.
    #[test]
    fn two_result_rules_union() -> Result<(), String> {
        let mut dmx = SomeDomain::new(1, "s1", "r1");
        dmx.add_action(0);

        let s0 = state_from_string(dmx.num_ints, "s0").unwrap();
        let s2 = state_from_string(dmx.num_ints, "s10").unwrap();
        let s4 = state_from_string(dmx.num_ints, "s100").unwrap();

        dmx.take_action_arbitrary(0, &s0, &s2);
        dmx.take_action_arbitrary(0, &s0, &s4);
        dmx.take_action_arbitrary(0, &s0, &s2);
        dmx.take_action_arbitrary(0, &s0, &s4);

        let sf = state_from_string(dmx.num_ints, "s1111").unwrap();
        let sb = state_from_string(dmx.num_ints, "s1011").unwrap();
        let sd = state_from_string(dmx.num_ints, "s1101").unwrap();

        dmx.take_action_arbitrary(0, &sf, &sb);
        dmx.take_action_arbitrary(0, &sf, &sd);
        dmx.take_action_arbitrary(0, &sf, &sb);
        dmx.take_action_arbitrary(0, &sf, &sd);

        //println!("act: 0 actions: {}", &dmx.actions);

        //println!("sqrs: \n{}", &dmx.actions[0].squares);

        let rx10x = region_from_string(dmx.num_ints, "rx10x").unwrap();
        let rx01x = region_from_string(dmx.num_ints, "rx01x").unwrap();
        let rulx = region_to_region(&rx10x, &rx01x);

        println!("r-2-r rule: {}", &rulx);

        let stps = dmx.actions.get_steps(&rulx);

        if stps.len() != 2 {
            return Err(format!("Number steps NEQ 2 {}", stps));
        }

        Ok(())
    } // end two_result_region_rules
} // end mod tests
