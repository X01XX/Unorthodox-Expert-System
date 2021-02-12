//! Tests for an Unorthodox Expert System.
//!

#[cfg(test)]
mod tests {
    use crate::bits::SomeBits;
    //    use crate::bitsstore::BitsStore;
    use crate::change::SomeChange;
    use crate::domain::SomeDomain;
    use crate::mask::SomeMask;
    use crate::maskstore::MaskStore;
    //    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
    use crate::resultstore::ResultStore;
    use crate::rule::SomeRule;
    use crate::rulestore::RuleStore;
    use crate::square::SomeSquare;
    use crate::state::SomeState;
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
        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);

        if let Some(_regx) = dm1.actions[0].groups._find(&rx1x1) {
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups._find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause not-pn=2 condition

                if let Some(_) = dm1.actions[0].groups._find(&rx1x1) {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!(" {}", dm1.actions[0].squares);
                    return Err(String::from("failed, rx1x1 should have been deleted"));
                } else {
                    // println!("\nActs: {}", &dm1.actions[0]);
                    // println!("       Sqrs: ({})", dm1.actions[0].squares);
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm1.actions[0]);
                //println!("       Sqrs: ({})", dm1.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dm1.actions[0]);
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
        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        //println!(
        //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
        //    s5, s4, sf, se, rx1x1
        //);
        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &s5, &se);

        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);
        dm1.eval_sample_arbitrary(0, &sf, &s4);

        if let Some(_regx) = dm1.actions[0].groups._find(&rx1x1) {
            //println!("\nActs: {}", &dm1.actions[0]);
            dm1.eval_sample_arbitrary(0, &s7, &s7);
            dm1.eval_sample_arbitrary(0, &s7, &s7);
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups._find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dm1.actions[0].groups._find(&rx1x1) {
                    println!("\nActs: {}", &dm1.actions[0]);
                    println!(" {}", dm1.actions[0].squares);
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                // println!("\nActs: {}", &dm1.actions[0]);
                // println!("       Sqrs: ({})", dm1.actions[0].squares);
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    // Test subtraction of two regions
    //#[test]
    //fn test3() {
    //    let mut dm1 = SomeDomain::new(1, "s1");
    //    dm1.add_action(action0, 0);
    //
    //    if let Ok(rx1x1) = dm1.region_from_string("rx1x1") {
    //        if let Ok(r1x0x) = dm1.region_from_string("r1x0x") {
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
    //    let mut dm1 = SomeDomain::new(1, "s1");
    //    dm1.add_action(action0, 0);
    //
    //    let mut regs = RegionStore::new();
    //
    //    if let Ok(r00x1) = dm1.region_from_string("r00x1") {
    //        // adjacent to rx1x1
    //        regs.push(r00x1);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(r1x1x) = dm1.region_from_string("r1x1x") {
    //       // intersects rx1x1
    //        regs.push(r1x1x);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(r0010) = dm1.region_from_string("r0010") {
    //        // not adjacent to rx1x1
    //        regs.push(r0010);
    //    } else {
    //        process::exit(1);
    //    }
    //
    //    if let Ok(rx1x1) = dm1.region_from_string("rx1x1") {
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
        let dm1 = SomeDomain::new(1, "s1", "r1", 1);

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
                    if let Ok(rx10x) = dm1.region_from_string("rx10x") {
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

    //    #[test]
    //    fn region_to_region_test() -> Result<(), String> {
    //        let dm1 = SomeDomain::new(2, "s1", "r1", 0);
    //
    //        let reg1 = dm1.region_from_string("r000_111_xxx").unwrap();
    //
    //        let reg2 = dm1.region_from_string("r01x_01x_01x").unwrap();
    //
    //        let b00 = SomeMask {
    //            bts: SomeBits { ints: vec![255 as u8, 325 as u8] },
    //        };
    //
    //        let b01 = SomeMask {
    //            bts: SomeBits { ints: vec![0 as u8, 194 as u8] },
    //        };
    //
    //        let b11 = SomeMask {
    //            bts: SomeBits { ints: vec![0 as u8, 27 as u8] },
    //        };
    //
    //        let b10 = SomeMask {
    //            bts: SomeBits { ints: vec![0 as u8, 44 as u8] },
    //        };
    //
    //        let ragg = _region_to_region(&reg1, &reg2);
    //
    //        if b00 != ragg.b00 {
    //            return Err(format!(
    //                "b00 {} problem in {} s/b {}",
    //                &b00, &ragg, &ragg.b00
    //            ));
    //        }
    //
    //        if b01 != ragg.b01 {
    //            return Err(format!(
    //                "b01 {} problem in {} s/b {} r1 {} r2 {}",
    //                &b01, &ragg, &ragg.b01, &reg1, &reg2
    //            ));
    //        }
    //
    //        if b11 != ragg.b11 {
    //            return Err(format!(
    //                "b11 {} problem in {} s/b {}",
    //                &b11, &ragg, &ragg.b11
    //            ));
    //        }
    //
    //        if b10 != ragg.b10 {
    //            return Err(format!(
    //                "b10 {} problem in {} s/b {}",
    //                &b10, &ragg, &ragg.b10
    //            ));
    //        }
    //
    //        Ok(())
    //    } // end region_to_region_test

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

        //        let mut btst = BitsStore { avec: vec![] };
        //
        //        let n1 = btst.formatted_string_length();
        //        let rs = btst.formatted_string();
        //        let n2 = rs.len();
        //        if n1 != n2 {
        //            return Err(format!(
        //                "calculated len of empty BitsStore string {} NEQ real len {} {}",
        //                n1, n2, rs
        //            ));
        //        }

        //        btst = BitsStore {
        //            avec: vec![SomeBits { ints: vec![12 as u8] }],
        //        };
        //        let n1 = btst.formatted_string_length();
        //        let rs = btst.formatted_string();
        //        let n2 = rs.len();
        //        if n1 != n2 {
        //            return Err(format!(
        //                "calculated len of BitsStore len 1 string {} NEQ real len {} {}",
        //                n1, n2, rs
        //            ));
        //        }

        //        btst = BitsStore {
        //            avec: vec![SomeBits { ints: vec![12 as u8] }, SomeBits { ints: vec![11 as u8] }],
        //        };
        //        let n1 = btst.formatted_string_length();
        //        let rs = btst.formatted_string();
        //        let n2 = rs.len();
        //        if n1 != n2 {
        //            return Err(format!(
        //                "calculated len of BitsStore len 2 string {} NEQ real len {} {}",
        //                n1, n2, rs
        //            ));
        //        }

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
        let dm1 = SomeDomain::new(2, "s1", "r1", 0);

        let reg1 = dm1.region_from_string("rx1x1").unwrap();

        let n1 = reg1.formatted_string_length();
        let rs = reg1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRegion string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let reg2 = dm1.region_from_string("rx1x1").unwrap();

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
            avec: vec![dm1.region_from_string("rx1x1").unwrap()],
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
                dm1.region_from_string("rx1x1").unwrap(),
                dm1.region_from_string("rx1x1").unwrap(),
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
        let dm1 = SomeDomain::new(1, "s1", "r1", 0);

        let stp1 = SomeStep {
            act_num: 0,
            initial: dm1.region_from_string("r1011").unwrap(),
            result: dm1.region_from_string("r1010").unwrap(),
            group_reg: dm1.region_from_string("rx0x1").unwrap(),
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

        let dm2 = SomeDomain::new(2, "s1", "r1", 2);

        let stp2 = SomeStep {
            act_num: 0,
            initial: dm2.region_from_string("r1011").unwrap(),
            result: dm2.region_from_string("r1010").unwrap(),
            group_reg: dm2.region_from_string("rx0x1").unwrap(),
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

    //#[test]
    //    fn change_string_length() -> Result<(), String> {
    //        let from_reg = dm1.region_from_string("rx1x1").unwrap();
    //        let goal_reg = dm1.region_from_string("rx1x1").unwrap();
    //
    //        let cngtst = SomeChange::region_to_region(&from_reg, &goal_reg);
    //
    //        let n1 = cngtst.formatted_string_length();
    //        let rs = cngtst.formatted_string();
    //        let n2 = rs.len();
    //        if n1 != n2 {
    //            return Err(format!(
    //                "calculated len of one int SomeChange string {} NEQ real len {} {}",
    //                n1, n2, rs
    //            ));
    //        }
    //
    //        Ok(())
    //    }

    /// Given a square and a two-rule RuleStore,
    /// return the expected next result for the square.
    #[test]
    fn predict_next_result() -> Result<(), String> {
        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

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
        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);

        let s0 = dm1.state_from_string("s0").unwrap();
        let s2 = dm1.state_from_string("s10").unwrap();
        let s4 = dm1.state_from_string("s100").unwrap();

        dm1.eval_sample_arbitrary(0, &s0, &s2);
        dm1.eval_sample_arbitrary(0, &s0, &s4);
        dm1.eval_sample_arbitrary(0, &s0, &s2);
        dm1.eval_sample_arbitrary(0, &s0, &s4);

        let sf = dm1.state_from_string("s1111").unwrap();
        let sb = dm1.state_from_string("s1011").unwrap();
        let sd = dm1.state_from_string("s1101").unwrap();

        dm1.eval_sample_arbitrary(0, &sf, &sb);
        dm1.eval_sample_arbitrary(0, &sf, &sd);
        dm1.eval_sample_arbitrary(0, &sf, &sb);
        dm1.eval_sample_arbitrary(0, &sf, &sd);

        //println!("act: 0 actions: {}", &dm1.actions);

        //println!("sqrs: \n{}", &dm1.actions[0].squares);

        let rx10x = dm1.region_from_string("rx10x").unwrap();
        let rx01x = dm1.region_from_string("rx01x").unwrap();
        let cngx = SomeChange::region_to_region(&rx10x, &rx01x);

        println!("r-2-r rule: {}", &cngx);

        let stps = dm1.actions.get_steps(&cngx);

        if stps.len() != 1 {
            return Err(format!("Number steps NEQ 1 {}", stps));
        }

        Ok(())
    } // end two_result_region_rules

    #[test]
    fn change_region_to_region_test() -> Result<(), String> {
        let dm1 = SomeDomain::new(2, "s1", "r1", 0);

        let reg1 = dm1.region_from_string("r000_111_xxx").unwrap();

        let reg2 = dm1.region_from_string("r01x_01x_01x").unwrap();

        let b01 = SomeMask {
            bts: SomeBits {
                ints: vec![0 as u8, 194 as u8],
            },
        };

        let b10 = SomeMask {
            bts: SomeBits {
                ints: vec![0 as u8, 44 as u8],
            },
        };

        let cngx = SomeChange::region_to_region(&reg1, &reg2);

        if b01 != cngx.b01 {
            return Err(format!(
                "b01 {} problem in {} s/b {} r1 {} r2 {}",
                &b01, &cngx, &cngx.b01, &reg1, &reg2
            ));
        }

        if b10 != cngx.b10 {
            return Err(format!(
                "b10 {} problem in {} s/b {}",
                &b10, &cngx, &cngx.b10
            ));
        }

        Ok(())
    } // end region_to_region_test

    #[test]
    fn rule_pruning() -> Result<(), String> {
        // Test rule pruning to fit a change
        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);

        let s122 = dm1.state_from_string("s1111001").unwrap();

        let s69 = dm1.state_from_string("s1000101").unwrap();

        let s9 = dm1.state_from_string("s1001").unwrap();

        let s85 = dm1.state_from_string("s1010101").unwrap();

        dm1.eval_sample_arbitrary(0, &s122, &s122);
        dm1.eval_sample_arbitrary(0, &s122, &s69);
        dm1.eval_sample_arbitrary(0, &s9, &s85);
        dm1.eval_sample_arbitrary(0, &s9, &s9);

        //println!("dom {}", &dm1);
        //println!("acts: {}", &dm1.actions);

        // Sub test 1
        let cng1 = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![16 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
        };

        let stps = dm1.actions.get_steps(&cng1);
        //println!("steps for {} are {}", &cng1, &stps);
        if stps.len() == 1 {
            let r01001001 = dm1.region_from_string("r01001001").unwrap();
            if r01001001 != stps[0].initial {
                return Err(format!("r01001001 ne {}", stps[0].initial));
            }
            let r01010101 = dm1.region_from_string("r01010101").unwrap();
            if r01010101 != stps[0].result {
                return Err(format!("r01010101 ne {}", stps[0].result));
            }
        } else {
            panic!("steps len NE 1");
        }

        // Sub test 2
        let cng1 = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![32 as u8],
                },
            },
        };

        let stps = dm1.actions.get_steps(&cng1);
        //println!("steps for {} are {}", &cng1, &stps);
        if stps.len() == 1 {
            let r011x1001 = dm1.region_from_string("r011X1001").unwrap();
            if r011x1001 != stps[0].initial {
                return Err(format!("r011X1001 ne {}", stps[0].initial));
            }
            let r010x0101 = dm1.region_from_string("r010x0101").unwrap();
            if r010x0101 != stps[0].result {
                return Err(format!("r010x0101 ne {}", stps[0].result));
            }
        } else {
            panic!("steps len NE 1");
        }

        // Sub test 3
        let cng1 = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![64 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
        };

        let stps = dm1.actions.get_steps(&cng1);
        //println!("steps for {} are {}", &cng1, &stps);
        if stps.len() == 1 {
            let r00001001 = dm1.region_from_string("r00001001").unwrap();
            if r00001001 != stps[0].initial {
                return Err(format!("r00001001 ne {}", stps[0].initial));
            }
            let r01010101 = dm1.region_from_string("r01010101").unwrap();
            if r01010101 != stps[0].result {
                return Err(format!("r01010101 ne {}", stps[0].result));
            }
        } else {
            panic!("steps len NE 1");
        }

        Ok(())
    } // end rule_pruning

    #[test]
    fn sort_steps() -> Result<(), String> {
        // Test SomeDomain::sort_steps to filter out steps that have more
        // changes then needed, if possible.

        let mut dm1 = SomeDomain::new(1, "s1", "r1", 1);
        dm1.add_action(0);
        dm1.add_action(0);
        dm1.add_action(0);

        let s0 = dm1.state_from_string("s0").unwrap();

        let s3 = dm1.state_from_string("s11").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let s6 = dm1.state_from_string("s110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let s8 = dm1.state_from_string("s1000").unwrap();

        let s12 = dm1.state_from_string("s1100").unwrap();

        let s14 = dm1.state_from_string("s1110").unwrap();

        dm1.eval_sample_arbitrary(0, &s0, &s8);
        dm1.eval_sample_arbitrary(0, &s7, &s8);

        dm1.eval_sample_arbitrary(1, &s0, &s14);
        dm1.eval_sample_arbitrary(1, &s3, &s12);

        dm1.eval_sample_arbitrary(2, &s4, &s6);
        dm1.eval_sample_arbitrary(2, &s6, &s6);

        //println!("Acts: {}", &dm1.actions);

        let cngx = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![8 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
        };

        let stpsx = dm1.actions.get_steps(&cngx);

        //println!("cng {} steps: {}", &cngx, &stpsx);

        let stp_cngs = dm1.sort_steps(&stpsx, &cngx);
        //println!("stp_cngs (inx): {:?}", &stp_cngs);

        if stp_cngs.len() != 1 {
            return Err(format!("1 stp_cngs len NE 1"));
        }

        if stp_cngs[0].len() != 1 {
            return Err(format!("1 stp_cngs[0] len NE 1"));
        }

        if stpsx[stp_cngs[0][0]].act_num != 0 {
            return Err(format!("1 stpsx[stp_cngs[0][0]].act_num != 0"));
        }

        let cngx = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![2 as u8],
                },
            },
        };

        let stpsx = dm1.actions.get_steps(&cngx);

        //println!("cng {} steps: {}", &cngx, &stpsx);

        let stp_cngs = dm1.sort_steps(&stpsx, &cngx);
        //println!("stp_cngs (inx): {:?}", &stp_cngs);

        if stp_cngs.len() != 2 {
            return Err(format!("2 stp_cngs len NE 2"));
        }

        if stp_cngs[0].len() != 1 {
            return Err(format!("2 stp_cngs[0] len NE 1"));
        }

        if stpsx[stp_cngs[0][0]].act_num > 1 {
            return Err(format!("2 stpsx[stp_cngs[0][0]].act_num > 1"));
        }

        if stp_cngs[1].len() != 1 {
            return Err(format!("2 stp_cngs[1] len NE 1"));
        }

        if stpsx[stp_cngs[1][0]].act_num > 1 {
            return Err(format!("2 stpsx[stp_cngs[1][0]].act_num > 1"));
        }

        let cngx = SomeChange {
            b01: SomeMask {
                bts: SomeBits {
                    ints: vec![2 as u8],
                },
            },
            b10: SomeMask {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
        };

        let stpsx = dm1.actions.get_steps(&cngx);

        //println!("cng {} steps: {}", &cngx, &stpsx);

        let stp_cngs = dm1.sort_steps(&stpsx, &cngx);
        //println!("stp_cngs (inx): {:?}", &stp_cngs);

        if stp_cngs.len() != 1 {
            return Err(format!("3 stp_cngs len NE 1"));
        }

        if stp_cngs[0].len() != 1 {
            return Err(format!("3 stp_cngs[0] len NE 1"));
        }

        if stpsx[stp_cngs[0][0]].act_num != 2 {
            return Err(format!("3 stpsx[stp_cngs[0][0]].act_num != 2"));
        }

        Ok(())
    } // end sort_steps
} // end mod tests
