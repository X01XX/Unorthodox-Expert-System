/*
 * Unorthodox Expert System
 *
 * Copyright 2021 Bitflogger <earl.dukerschein@wisc.edu>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 *
 */

/*
 * For use outside of the license, contact the Wisconsin Alumni Research Foundation (WARF).
 */

use std::env;
mod action;
mod actionstore;
mod bits;
use crate::bits::SomeBits;
mod group;
mod groupstore;
mod mask;
mod need;
mod tools;
use need::SomeNeed;
mod region;
use region::SomeRegion;
mod change;
mod regionstore;
mod regionstorecorr;
mod resultstore;
mod rule;
mod rulestore;
use rulestore::RuleStore;
mod sample;
mod square;
mod squarestore;
mod state;
use sample::SomeSample;
use state::SomeState;
mod domain;
mod needstore;
mod plan;
use plan::SomePlan;
mod pn;
mod statestore;
mod statestorecorr;
use pn::Pn;
mod domainstore;
mod step;
mod stepstore;
use domainstore::{DomainStore, InxPlan};
use stepstore::StepStore;
mod actioninterface;
mod planstore;
mod selectregions;
use crate::selectregions::SelectRegions;
mod selectregionsstore;
mod target;
mod targetstore;
use crate::change::SomeChange;
use crate::regionstorecorr::RegionStoreCorr;
use crate::rule::SomeRule;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
use std::str::FromStr;
use std::time::{Duration, Instant};

/// Get and react to arguments given.
fn main() {
    // Get arguments, if any.
    let args: Vec<String> = env::args().collect();
    //println!("{:?}", args);

    if args.len() == 1 {
        run_step_by_step();
        process::exit(0);
    }

    if args.len() == 2 {
        if args[1] == "h" || args[1] == "help" {
            usage();
            process::exit(0);
        }
        if let Ok(runs) = args[1].parse::<usize>() {
            match runs {
                0 => {
                    usage();
                    eprintln!("Did not understand {:?}", args);
                    process::exit(1);
                }
                1 => {
                    let mut dmxs = domainstore_init();
                    run_to_end(&mut dmxs);
                }
                _ => {
                    let fails = run_number_times(runs);
                    if fails == 0 {
                        return;
                    }
                    process::exit(1);
                }
            }
        } else {
            // Run with arg[1] as file path.
            run_with_file(&args[1]);
        }
        process::exit(0);
    }

    usage();
    eprintln!("Did not understand {:?}", args);
    process::exit(1);
} // end main

/// Run with user input step by step.
fn run_step_by_step() {
    usage();
    // Generate needs, get can_do and cant_do need vectors.
    let mut dmxs = domainstore_init();

    dmxs.generate_and_display_needs();

    do_session(&mut dmxs);
}

/// Load data from a file, then run with user input, step by step.
fn run_with_file(file_path: &str) {
    usage();
    // Init DomainStore or read in from file.
    let mut dmxs = match load_data(file_path) {
        Ok(new_dmxs) => {
            println!("Data loaded");
            new_dmxs
        }
        Err(why) => {
            eprintln!("{why}");
            return;
        }
    };

    // Generate needs, get can_do and cant_do need vectors.
    dmxs.display_needs();

    do_session(&mut dmxs);
}

/// Run until no more needs can be done, then take user input.
fn run_to_end(dmxs: &mut DomainStore) {
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        dmxs.generate_and_display_needs();

        // Check for end.
        if dmxs.can_do.is_empty() {
            break;
        }

        do_any_need(dmxs);
    } // end loop

    do_session(dmxs);
}

/// Run a number of times without user input, generate aggregae data.
/// Return number failures, that is the number of seessions that ended with unsatisfied needs.
fn run_number_times(num_runs: usize) -> usize {
    let mut runs_left = num_runs;
    let mut cant_do = 0;
    let mut duration_vec = Vec::<Duration>::with_capacity(num_runs);
    let mut steps_vec = Vec::<usize>::with_capacity(num_runs);
    let num_groups_unexpected = 0;
    let mut num_groups_off = 0;

    while runs_left > 0 {
        runs_left -= 1;

        let start = Instant::now();
        match do_one_session() {
            Ok((steps, groups, expected)) => {
                if groups == expected {
                    let duration = start.elapsed();
                    println!("Steps {steps}, Time elapsed in do_session() is: {duration:?} groups: {groups:?}");
                    duration_vec.push(duration);
                    steps_vec.push(steps);
                } else {
                    let duration = start.elapsed();
                    println!("Steps {steps}, Time elapsed in do_session() is: {duration:?} groups: {groups:?}");

                    if groups != expected {
                        num_groups_off += 1
                    }
                }
            }
            Err(_) => {
                cant_do += 1;
            }
        }
    }

    if duration_vec.is_empty() {
        println!("Number with unsatisfied needs: {cant_do} Num groups off {num_groups_off}");
        return 1;
    }

    let mut duration_total = Duration::new(0, 0);
    let mut duration_high = duration_vec[0];
    let mut duration_low = duration_vec[0];
    for durx in duration_vec.iter() {
        duration_total += *durx;
        if *durx > duration_high {
            duration_high = *durx;
        }
        if *durx < duration_low {
            duration_low = *durx;
        }
    }

    let mut steps_total = 0;
    let mut steps_high = 0;
    let mut steps_low = usize::MAX;
    for stepsx in steps_vec.iter() {
        steps_total += *stepsx;
        if *stepsx > steps_high {
            steps_high = *stepsx;
        }
        if *stepsx < steps_low {
            steps_low = *stepsx;
        }
    }
    let average_time = duration_total / duration_vec.len() as u32;
    let average_steps = steps_total / steps_vec.len();

    println!("\nRuns {}, Average steps: {} high: {}, low: {}, Average time elapsed: {:.3?}, high: {:.3?}, low: {:.3?} Number Groups unexpected: {:3.3?} Number with unsatisfied needs {} Num groups off {}",
         num_runs, average_steps, steps_high, steps_low, average_time, duration_high, duration_low, num_groups_unexpected, cant_do, num_groups_off);
    cant_do
}

/// Initialize a Domain Store, with two domains and 11 actions.
fn domainstore_init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    dmxs.add_domain(SomeState::new(SomeBits::new(5)).new_random());
    dmxs.add_domain(SomeState::new(SomeBits::new(16)).new_random());

    // Add actions 0 through 9 to Domain 0;
    let ruls0: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_11/XX/00/Xx")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_00/XX/11/Xx")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_XX/11/XX/10")
            .expect("SNH")]),
        RuleStore::new(vec![
            dmxs[0].rule_from_string("XX_11/00/10/XX").expect("SNH"),
            dmxs[0].rule_from_string("XX_11/01/11/XX").expect("SNH"),
        ]),
        RuleStore::new(vec![
            dmxs[0].rule_from_string("XX_11/XX/10/00").expect("SNH"),
            dmxs[0].rule_from_string("XX_11/Xx/11/00").expect("SNH"),
        ]),
        RuleStore::new(vec![
            dmxs[0].rule_from_string("XX_00/00/00/Xx").expect("SNH"),
            dmxs[0].rule_from_string("XX_00/00/01/XX").expect("SNH"),
            dmxs[0].rule_from_string("XX_00/01/00/XX").expect("SNH"),
        ]),
        RuleStore::new(vec![
            dmxs[0].rule_from_string("XX_00/XX/00/01").expect("SNH"),
            dmxs[0].rule_from_string("XX_00/XX/01/00").expect("SNH"),
            dmxs[0].rule_from_string("XX_00/Xx/00/00").expect("SNH"),
        ]),
    ];
    dmxs[0].add_action(ruls0);

    let ruls1: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_XX/Xx/11/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_XX/XX/01/XX")
            .expect("SNH")]),
    ];
    dmxs[0].add_action(ruls1);

    let ruls2: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_XX/XX/10/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_XX/Xx/00/XX")
            .expect("SNH")]),
    ];
    dmxs[0].add_action(ruls2);

    let ruls3: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("Xx_11/XX/XX/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_01/XX/XX/XX")
            .expect("SNH")]),
    ];
    dmxs[0].add_action(ruls3);

    let ruls4: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX_10/XX/XX/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[0]
            .rule_from_string("Xx_00/XX/XX/XX")
            .expect("SNH")]),
    ];
    dmxs[0].add_action(ruls4);

    let ruls5: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[0]
        .rule_from_string("XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[0].add_action(ruls5);

    // Add actions 0 through 6 to domain 1.
    let ruls0: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/Xx/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls0);

    let ruls1: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/XX/Xx_XX/Xx/XX/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls1);

    let ruls2: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/XX/Xx_Xx/XX/XX/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls2);

    let ruls3: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/XX/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls3);

    let ruls4: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/Xx/XX_XX/XX/XX/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls4);

    let ruls5: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
        .rule_from_string("XX/XX/XX/XX_XX/XX/Xx/XX_XX/XX/XX/XX_XX/XX/XX/XX")
        .expect("SNH")])];
    dmxs[1].add_action(ruls5);

    let ruls6: Vec<RuleStore> = vec![
        RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/11/XX_XX/XX/XX/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/XX/XX/XX_XX/XX/Xx/XX_XX/11/00/XX_XX/XX/XX/XX")
            .expect("SNH")]),
        RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/XX/XX/XX_XX/Xx/XX/XX_XX/00/00/XX_XX/XX/XX/XX")
            .expect("SNH")]),
    ];
    dmxs[1].add_action(ruls6);

    // Load optimal regions
    let mut regstr1 = RegionStoreCorr::with_capacity(2);
    regstr1.push(dmxs[0].region_from_string_pad_x("r0x0x").expect("SNH"));
    regstr1.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XX1X_1XXX_XXXX")
            .expect("SNH"),
    );

    let mut regstr2 = RegionStoreCorr::with_capacity(2);
    regstr2.push(dmxs[0].region_from_string_pad_x("r0xx1").expect("SNH"));
    regstr2.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XXX1_1XXX_XXXX")
            .expect("SNH"),
    );

    let mut regstr3 = RegionStoreCorr::with_capacity(2);
    regstr3.push(dmxs[0].region_from_string_pad_x("rx1x1").expect("SNH"));
    regstr3.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XX00_0XXX_XXXX")
            .expect("SNH"),
    );

    let mut regstr4 = RegionStoreCorr::with_capacity(2);
    regstr4.push(dmxs[0].region_from_string_pad_x("r1110").expect("SNH"));
    regstr4.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XXX0_0XXX_XXXX")
            .expect("SNH"),
    );

    let mut regstr5 = RegionStoreCorr::with_capacity(2);
    regstr5.push(dmxs[0].region_from_string_pad_x("rXX00").expect("SNH"));
    regstr5.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XXx1_0xXX_XXXX")
            .expect("SNH"),
    );

    let mut regstr6 = RegionStoreCorr::with_capacity(2);
    regstr6.push(dmxs[0].region_from_string_pad_x("rX10X").expect("SNH"));
    regstr6.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXX_XX1x_x0XX_XXXX")
            .expect("SNH"),
    );

    // Add select regionstores.
    dmxs.add_select(SelectRegions::new(regstr1, 3));
    dmxs.add_select(SelectRegions::new(regstr2, 2));
    dmxs.add_select(SelectRegions::new(regstr3, 3));
    dmxs.add_select(SelectRegions::new(regstr4, 1));
    dmxs.add_select(SelectRegions::new(regstr5, -1));
    dmxs.add_select(SelectRegions::new(regstr6, -1));

    dmxs.calc_select();

    dmxs
}

/// Do one session to end.
/// Return the number steps taken get to the point where
/// there are no more needs.
/// Return error if no needs that can be done are available and
/// there are needs than cannot be done.
fn do_one_session() -> Result<(usize, usize, usize), String> {
    let mut dmxs = domainstore_init();
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        dmxs.generate_and_display_needs();

        // Check for end.
        if dmxs.can_do.is_empty() {
            if dmxs.cant_do.is_empty() {
                return Ok((
                    dmxs.step_num,
                    dmxs.number_groups(),
                    dmxs.number_groups_expected(),
                ));
            } else {
                // do_session(&mut dmxs);
                // process::exit(0);
                return Err("There are needs that cannot be done".to_string());
            }
        }

        do_any_need(&mut dmxs);
    } // end loop
}

/// Do a session, step by step, taking user commands.
pub fn do_session(dmxs: &mut DomainStore) {
    loop {
        command_loop(dmxs);

        // Generate needs, get can_do and cant_do need vectors.
        dmxs.generate_and_display_needs();
    } // end loop
} // end do_session

/// Do command loop.
fn command_loop(dmxs: &mut DomainStore) {
    //println!("start command loop");
    loop {
        let mut cmd = Vec::<&str>::with_capacity(10);

        let guess = pause_for_input("\nPress Enter or type a command: ");

        for word in guess.split_whitespace() {
            cmd.push(word);
        }

        // Default command, just press Enter
        if cmd.is_empty() {
            // Process needs
            if dmxs.can_do.is_empty() {
            } else {
                do_any_need(dmxs);
            }
            return;
        }

        // Do commands
        match cmd[0] {
            "aj" => {
                if cmd.len() == 2 {
                    match display_action_anchor_info(dmxs, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                        }
                    }
                } else if cmd.len() == 3 {
                    match display_group_anchor_info(dmxs, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                        }
                    }
                } else {
                    println!("Too many args for the aj command.");
                }
            }
            "cd" => match do_change_domain(dmxs, &cmd) {
                Ok(()) => {
                    dmxs.print_domain();
                    dmxs.display_needs();
                }
                Err(error) => {
                    println!("{error}");
                }
            },
            "cs" => match do_change_state_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "dcs" => {
                dmxs.print_domain();
                dmxs.print_can_do();
            }
            "dn" => match do_chosen_need(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "exit" | "q" | "quit" => {
                println!("Done");
                process::exit(0);
            }
            "fsd" => match store_data(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "gnds" => {
                let dom_id = dmxs.current_domain;
                dmxs[dom_id].get_needs();
                dmxs.print_domain();
                continue;
            }
            "gps" => match do_print_group_defining_squares_command(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "h" => usage(),
            "help" => usage(),
            "ppd" => match do_print_plan_details(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "ps" => match do_print_squares_command(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "psr" => match do_print_select_regions(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "run" => {
                if dmxs.can_do.is_empty() {
                } else {
                    run_to_end(dmxs);
                }
            }
            "ss" => match do_sample_state_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "to" => match do_to_region_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "rx" => {
                // Check for, and process an argument to the rx command.
                let mut goal: Option<SomeRegion> = None;
                let mut dif: Option<SomeChange> = None;
                let dmx = &dmxs.domains[dmxs.current_domain];
                let mut cur_state = dmx.cur_state.clone();

                if cmd.len() > 1 {
                    if let Ok(regx) = dmx.region_from_string(cmd[1]) {
                        if regx.is_superset_of(&cur_state) {
                            println!("Current state is already in the goal");
                            return;
                        } else {
                            goal = Some(regx);
                            println!("A *  after a rule indicates at least one needed change.");
                            println!("A *- after a rule indicates at least one additional uneeded change.");
                        }
                    } else {
                        println!("Unable to convert string {} into a region", cmd[1]);
                    }
                }
                let mut stack = Vec::<(SomeState, (usize, &SomeRule), SomeState)>::new();
                let ruls = dmx.all_rules();
                loop {
                    if let Some(ref regx) = goal {
                        let wanted_changes = SomeRule::rule_region_to_region(
                            &SomeRegion::new(vec![cur_state.clone()]),
                            regx,
                        )
                        .change();
                        println!(
                            "Goal {}, Change needed {} (number bits {})",
                            regx,
                            wanted_changes,
                            wanted_changes.number_changes()
                        );
                        dif = Some(wanted_changes);
                    }

                    // Print Stack.
                    if stack.is_empty() {
                        println!("Stack: empty\n");
                    } else {
                        println!("Stack:");
                        for (inx, (cur_sta, rulx, nxt_sta)) in stack.iter().enumerate() {
                            println!(
                                "  {:2} {} {} -{}-> {}",
                                inx, cur_sta, rulx.1, rulx.0, nxt_sta
                            );
                        }
                        println!(" ");
                    }

                    // Check if a goal is given and achieved.
                    let mut show_rules = true;
                    if let Some(ref regx) = goal {
                        if regx.is_superset_of(&cur_state) {
                            show_rules = false;
                        }
                    }

                    // A list of valid indicies for later display and validation.
                    let mut valid_indexes = Vec::<usize>::new();
                    if show_rules {
                        println!("{} rules", ruls.len());

                        // Get rules that make a needed change, but cannot be used with the current state.
                        // Save a list of valid indicies for later display.
                        let mut future_numbers = Vec::<usize>::new();
                        if let Some(ref wanted_changes) = &dif {
                            for (inx, (_act_id, rulx)) in ruls.iter().enumerate() {
                                if !rulx.initial_region().is_superset_of(&cur_state)
                                    && wanted_changes.intersection(*rulx).is_not_low()
                                {
                                    future_numbers.push(inx);
                                }
                            }
                        }

                        // Get rules that make a change to the current state.
                        for (inx, (_act_id, rulx)) in ruls.iter().enumerate() {
                            // If rule can be applied to the state..
                            if rulx.initial_region().is_superset_of(&cur_state) {
                                // Calc the result state is different from the current state.
                                let next_state = rulx.result_from_initial_state(&cur_state);
                                if next_state != cur_state {
                                    // Check for dup state in the stack.
                                    let mut not_dup = true;
                                    for (cur_sta, _rulx, nxt_sta) in stack.iter() {
                                        if next_state == *cur_sta || next_state == *nxt_sta {
                                            not_dup = false;
                                        }
                                    }
                                    // If no dup state found in the stack, add the index.
                                    if not_dup {
                                        valid_indexes.push(inx);
                                    }
                                }
                            }
                        }

                        // Display future options, if any.
                        if future_numbers.is_empty() {
                        } else {
                            println!("Possible future options, containing a needed change:");
                            let goal_reg = goal.as_ref().expect("SNH");
                            let edge_mask = goal_reg.edge_mask();

                            for inx in future_numbers.iter() {
                                let (act_id, rulx) = ruls[*inx];
                                let mut ruly = rulx.clone();

                                let mut suffix = String::from(" ");
                                if let Some(wanted_changes) = &dif {
                                    ruly = rulx
                                        .restrict_for_changes(wanted_changes, None)
                                        .expect("SNH");
                                    let to_rule = SomeRule::rule_region_to_region(
                                        &SomeRegion::new(vec![cur_state.clone()]),
                                        &ruly.initial_region(),
                                    );

                                    let agg_rule = to_rule.combine_pair(&ruly);

                                    let wanted_bit_changes = wanted_changes.intersection(&agg_rule);

                                    let unwanted_bit_changes = wanted_changes
                                        .bitwise_not()
                                        .intersection(&agg_rule)
                                        .bitwise_and(&edge_mask);

                                    if wanted_bit_changes.is_not_low() {
                                        suffix += &format!("wanted {}", wanted_bit_changes);
                                    }
                                    if unwanted_bit_changes.is_not_low() {
                                        suffix += &format!(" unwanted {}", unwanted_bit_changes);
                                    }
                                }
                                println!(
                                    "  {:2} {} {} -{}-> {} {suffix}",
                                    inx,
                                    ruly.initial_region(),
                                    ruly,
                                    act_id,
                                    ruly.result_region(),
                                );
                            }
                            println!(" ");
                        }

                        // Display current options.
                        if valid_indexes.is_empty() {
                            println!("Current options: None");
                        } else {
                            println!("Current options:");
                            for inx in valid_indexes.iter() {
                                let (act_id, rulx) = ruls[*inx];

                                // Calc result of applying the rule.
                                let result = rulx.result_from_initial_state(&cur_state);

                                let ruly = rulx.restrict_initial_region(&SomeRegion::new(vec![
                                    cur_state.clone(),
                                ]));

                                // If there is a change needed for a goal.
                                if let Some(ref wanted_changes) = dif {
                                    // Init string for display of links.
                                    let mut suffix = String::new();
                                    // Init vector for link indicies.
                                    let mut link_indexes = Vec::<usize>::new();
                                    // Look for possible links.
                                    for inf in future_numbers.iter() {
                                        // Get rule info.
                                        let (_act_idf, rulf) = ruls[*inf];

                                        // If possible future rule can be linked.
                                        if rulf.initial_region().is_superset_of(&result) {
                                            // Restrict rule to filter out changes that will not apply to the linked result state.
                                            let ruly = rulf.restrict_initial_region(
                                                &SomeRegion::new(vec![result.clone()]),
                                            );
                                            if wanted_changes.intersection(&ruly).is_low() {
                                            } else {
                                                // There is a change caused by the rule, calc next state.
                                                let next_state =
                                                    ruly.result_from_initial_state(&result);
                                                // Check for dup state, in stack.
                                                let mut not_dup = true;
                                                for (cur_sta, _rulx, nxt_sta) in stack.iter() {
                                                    if next_state == *cur_sta
                                                        || next_state == *nxt_sta
                                                    {
                                                        not_dup = false;
                                                    }
                                                }
                                                // If no dup state in stack, add possible link.
                                                if not_dup {
                                                    link_indexes.push(*inf);
                                                }
                                            }
                                        }
                                    }
                                    // Generate suffix for rule display, if needed.
                                    if link_indexes.is_empty() {
                                    } else {
                                        suffix = format!(" Links to {:?}, above", link_indexes);
                                    }

                                    // Check if a wanted change occurs.
                                    if wanted_changes.intersection(&ruly).is_low() {
                                        println!(
                                            "  {:2} {} -{}-> {} {}",
                                            inx, cur_state, act_id, result, suffix,
                                        );
                                    } else if wanted_changes.intersection(&ruly) != ruly.change() {
                                        // Check for uneeded change.
                                        println!(
                                            "  {:2} {} -{}-> {} *- {}",
                                            inx, cur_state, act_id, result, suffix,
                                        );
                                    } else {
                                        // There is a needed change.
                                        println!(
                                            "  {:2} {} -{}-> {} * {}",
                                            inx, cur_state, act_id, result, suffix,
                                        );
                                    }
                                } else {
                                    // No goal was given.
                                    println!("  {:2} {} -{}-> {}", inx, cur_state, act_id, result);
                                }
                            }
                        }
                    }

                    // Get user input.
                    let mut cmd2 = Vec::<&str>::with_capacity(10);
                    let guess2 = pause_for_input(
                        "\nType a number, from options, above, pop, clear, or q to quit: ",
                    );

                    // Process user input.
                    for word in guess2.split_whitespace() {
                        cmd2.push(word);
                    }
                    if cmd2.is_empty() {
                        continue;
                    }
                    if cmd2[0] == "q" {
                        return;
                    }
                    if cmd2[0] == "pop" {
                        if let Some((new_state, _rulx, _old_state)) = stack.pop() {
                            cur_state = new_state;
                        }
                        continue;
                    }
                    if cmd2[0] == "clear" {
                        while !stack.is_empty() {
                            if let Some((new_state, _rulx, _old_state)) = stack.pop() {
                                cur_state = new_state;
                            }
                        }
                        continue;
                    }
                    match usize::from_str(cmd2[0]) {
                        Ok(rul_num) => {
                            if !valid_indexes.contains(&rul_num) {
                                println!("Number not matched");
                            } else {
                                let new_state =
                                    ruls[rul_num].1.result_from_initial_state(&cur_state);

                                stack.push((cur_state.clone(), ruls[rul_num], new_state.clone()));
                                cur_state = new_state;
                            }
                        }
                        _ => println!("Did not understand number"),
                    }
                } // end loop
            }
            _ => {
                println!("\nDid not understand command: {cmd:?}");
            }
        };
    } // end loop
} // end command_loop

/// Change the domain to a number given by user.
fn do_change_domain(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one number argument is needed for the cd command.".to_string());
    }
    // Get domain number from string
    match dmxs.domain_id_from_string(cmd[1]) {
        Ok(d_id) => {
            dmxs.change_domain(d_id);
            Ok(())
        }
        Err(error) => Err(error),
    } // end match
}

/// Choose a need from a number of possibilities.
/// Attempt to satisfy the chosen need.
fn do_any_need(dmxs: &mut DomainStore) {
    let np_inx = dmxs.choose_a_need();

    if do_a_need(dmxs, dmxs.can_do[np_inx].clone()) {
        println!("Need satisfied");
    }
}

/// Print details of a given plan
fn do_print_plan_details(dmxs: &DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the ppd command.".to_string());
    }
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let ndx = &dmxs.needs[dmxs.can_do[n_num].inx];
                let pln = &dmxs.can_do[n_num].plans;

                println!("\n{} Need: {}", n_num, ndx);
                match ndx {
                    SomeNeed::ToSelectRegion { .. } => {
                        //println!("\n{}", pln.str2());
                        dmxs.print_plan_detail(pln);
                    }
                    SomeNeed::ExitSelectRegion { .. } => {
                        //println!("\n{}", pln.str2());
                        dmxs.print_plan_detail(pln);
                    }
                    _ => {
                        if ndx.satisfied_by(dmxs[ndx.dom_id()].get_current_state()) {
                            println!("\nPlan: current state satisfies need, just take the action");
                        } else {
                            //println!("\n{}", pln.str2());
                            //print_plan_detail(dmxs, pln);
                            dmxs.print_plan_detail(pln);
                        }
                    }
                }
                Ok(())
            }
        }
        Err(error) => Err(error.to_string()),
    }
}

/// Try to satisfy a need.
/// Return true if success.
fn do_a_need(dmxs: &mut DomainStore, inx_pln: InxPlan) -> bool {
    let dom_id = dmxs.current_domain;
    let nd_inx = inx_pln.inx;

    // Display Domain info, if needed.
    match dmxs.needs[nd_inx] {
        SomeNeed::ToSelectRegion { .. } => {
            //println!("\nNeed chosen: {} {}", ndx, plans.str_terse())
        }
        SomeNeed::ExitSelectRegion { .. } => {
            //println!("\nNeed chosen: {} {}", nd_inx, inx_pln.plans.str_terse())
        }
        _ => {
            let nd_dom = dmxs.needs[nd_inx].dom_id();
            if dom_id != nd_dom {
                // Show "before" state before running need.
                println!("\nAll domain states: {}", dmxs.all_current_states());
                dmxs.change_domain(nd_dom);
                dmxs.print_domain();
                //println!("\nNeed chosen: {} {}", ndx, plans.str_terse());
            }
        }
    }

    // Run the plan, allow for one failure.
    if !dmxs.run_plan_store(&inx_pln.plans) {
        print!("Run plan failed, ");
        if let Some(plans2) = dmxs.make_plans(&dmxs.needs[inx_pln.inx].target()) {
            println!("try again with {}", plans2);
            if !dmxs.run_plan_store(&plans2) {
                println!("Unexpected result, giving up.");
                return false;
            }
        } else {
            println!("unexpected result, new path to goal not found.");
        }
    }

    // Take action after the desired state is reached.
    match &dmxs.needs[nd_inx] {
        SomeNeed::ToSelectRegion { .. } => {
            if dmxs.needs[nd_inx]
                .target()
                .is_superset_of_states(&dmxs.all_current_states())
            {
                if dmxs.set_boredom_limit() {
                    dmxs.update_times_visited();
                }
                return true;
            }
        }
        SomeNeed::ExitSelectRegion { .. } => {
            if dmxs.needs[nd_inx]
                .target()
                .is_superset_of_states(&dmxs.all_current_states())
            {
                return true;
            }
        }
        _ => {
            if dmxs.needs[nd_inx].satisfied_by(dmxs.cur_state(dmxs.needs[nd_inx].dom_id())) {
                dmxs.take_action_need(nd_inx);
                return true;
            }
        }
    }
    false
}

/// Try to satisfy a need chosen by the user.
fn do_chosen_need(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the dn command.".to_string());
    }

    let dom_id = dmxs.current_domain;

    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let nd_inx = dmxs.can_do[n_num].inx;

                println!(
                    "\nNeed chosen: {:2} {} {}",
                    n_num,
                    dmxs.needs[nd_inx],
                    dmxs.can_do[n_num].plans.str_terse()
                );

                match dmxs.needs[nd_inx] {
                    SomeNeed::ToSelectRegion { .. } => (),
                    SomeNeed::ExitSelectRegion { .. } => (),
                    _ => {
                        if dom_id != dmxs.needs[nd_inx].dom_id() {
                            dmxs.change_domain(dmxs.needs[nd_inx].dom_id());
                            dmxs.print_domain();
                        }
                    }
                }

                if do_a_need(dmxs, dmxs.can_do[n_num].clone()) {
                    println!("Need satisfied");
                }

                Ok(())
            }
        }
        Err(error) => Err(format!("{error}")),
    }
}

/// Do a change-state command.
fn do_change_state_command(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one state argument is needed for the cs command.".to_string());
    }
    // Get state from string
    match dmxs.state_from_string(cmd[1]) {
        Ok(a_state) => {
            println!("Changed state to {a_state}");
            dmxs.set_state(&a_state);
            Ok(())
        }
        Err(error) => Err(format!("\nDid not understand state, {error}")),
    } // end match
}

/// Do to-region command.
fn do_to_region_command(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one region argument is needed for the to command.".to_string());
    }
    let dom_id = dmxs.current_domain;
    let dmx = &mut dmxs[dom_id];

    let cur_state = dmx.get_current_state();

    // Get region from string
    match dmx.region_from_string(cmd[1]) {
        Ok(goal_region) => {
            println!("\nChange Current_state {cur_state} to region {goal_region}");
            if goal_region.is_superset_of(cur_state) {
                println!(
                    "\nCurrent_state {} is already in region {}",
                    dmx.get_current_state(),
                    goal_region
                );
            } else if dmxs.seek_state_in_region(dom_id, &goal_region) {
                println!("\nChange to region succeeded");
            } else {
                let cur_region = SomeRegion::new(vec![dmxs.cur_state(dom_id).clone()]);
                println!("\nChange to region failed");
                let cng_rule = SomeRule::rule_region_to_region(&cur_region, &goal_region);
                println!("Rule needed: {cng_rule}");

                let required_change = cng_rule.change();

                let steps_str: StepStore = dmxs[dom_id].actions.get_steps(&required_change, None);
                if steps_str.is_not_empty() {
                    println!("Steps found {steps_str}");
                    let can_change = steps_str.aggregate_changes().expect("SNH");

                    if required_change.is_subset_of(&can_change) {
                        println!("All needed changes found in steps");
                        if steps_str.len() > 1 {
                            for inx in 0..(steps_str.len() - 1) {
                                for iny in (inx + 1)..steps_str.len() {
                                    if steps_str[inx]
                                        .mutually_exclusive(&steps_str[iny], &required_change)
                                    {
                                        println!(
                                            "Mutually exclusive steps {} {}",
                                            steps_str[inx], steps_str[iny]
                                        );
                                    }
                                }
                            }
                        }
                        for stepx in steps_str.iter() {
                            if let Some(plans2) =
                                dmxs[dom_id].make_plans2(&cur_region, &stepx.initial, None)
                            {
                                println!("Plan(s) to get from {cur_region} to {}", stepx.initial);
                                // delete dups.
                                let mut plans = Vec::<SomePlan>::new();
                                for planx in plans2 {
                                    if tools::vec_contains(&plans, SomePlan::eq, &planx) {
                                    } else {
                                        plans.push(planx);
                                    }
                                }
                                println!("{}", tools::vec_string(&plans));
                            } else {
                                println!("No plan to get from {cur_region} to {}", stepx.initial);
                            }
                        }
                    } else {
                        println!(
                            "Wanted changes {} are not a subset of possible changes {}",
                            required_change, can_change
                        );
                    }
                } else {
                    println!("No steps found");
                }
                pause_for_input("\nPress Enter to continue: ");
            }
            Ok(())
        }
        Err(error) => Err(error),
    } // end match region_r
}

/// Do sample-state command.
fn do_sample_state_command(dmxs: &mut DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &mut dmxs[dom_id];

    let cur_state = dmx.get_current_state();

    if cmd.len() == 1 {
        return Err("Action number is needed for the ss command.".to_string());
    }

    let act_id = match dmx.act_id_from_string(cmd[1]) {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 {
        println!("Act {act_id} sample State {cur_state}");
        dmx.take_action_arbitrary(act_id);
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get state from string
        let a_state = match dmx.state_from_string(cmd[2]) {
            Ok(a_state) => a_state,
            Err(error) => {
                return Err(error);
            }
        };

        println!("Act {act_id} sample State {a_state}");
        dmx.set_state(&a_state);
        dmx.take_action_arbitrary(act_id);
        return Ok(());
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state
        // This tends to break things for an action, unless all samples are arbitrary.
        // Useful for testing a wholly different series of samples/results.
        // Using the command: ss  action-number  initial-state  result-state
        // e.g. ss  0  s0b1010  s0b1111

        // Get i-state from string
        let i_state = match dmx.state_from_string(cmd[2]) {
            Ok(i_state) => i_state,
            Err(error) => {
                return Err(error);
            }
        };

        // Get r-state from string
        let r_state = match dmx.state_from_string(cmd[3]) {
            Ok(r_state) => r_state,
            Err(error) => {
                return Err(error);
            }
        };

        println!("Act {act_id} take sample {i_state} -> {r_state}");
        let smpl = SomeSample::new(i_state, r_state);
        dmx.eval_sample_arbitrary(act_id, &smpl);
        return Ok(());
    } // end command ss 4

    Err(format!("Did not understand {cmd:?}"))
}

/// Display anchors, rating, and adjacent squares, for an action.
/// For a group that has an anchor, and is limited, the number edges, that can be changed with actions,
/// should equal the sum of the first two number of the rating.
fn display_action_anchor_info(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &mut dmxs[dom_id];

    if cmd.len() == 1 {
        return Err("Need to supply an action number".to_string());
    }

    // Get action number
    let act_id = match dmx.act_id_from_string(cmd[1]) {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error);
        }
    };

    // Display the rates
    dmxs[dom_id].display_action_anchor_info(act_id)
}

/// Do print-squares command.
fn do_print_select_regions(dmxs: &DomainStore, cmd: &[&str]) -> Result<(), String> {
    if cmd.len() != 1 {
        return Err("No arguments needed for the psr command".to_string());
    }

    for selx in dmxs.select.iter() {
        println!("{}", selx);
    }
    Ok(())
}

/// Do print-squares command.
fn do_print_squares_command(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];

    if cmd.len() == 1 {
        return Err("Need to supply at least an action number".to_string());
    }

    // Get action number
    let act_id = match dmx.act_id_from_string(cmd[1]) {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 {
        println!(
            "Squares of Action {} are:\n{}\n",
            act_id, dmx.actions[act_id].squares
        );
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get region
        let aregion = match dmx.region_from_string(cmd[2]) {
            Ok(aregion) => aregion,
            Err(error) => {
                return Err(error);
            }
        };

        println!("Squares of Action {act_id} in region {aregion} are:\n");

        let sqrs = dmx.actions[act_id].squares.squares_in_reg(&aregion);
        if sqrs.is_empty() {
            println!("No squares in region {aregion}");
            return Ok(());
        }

        let mut max_pn = Pn::One;
        let mut min_pn = Pn::Unpredictable;
        let mut max_pn_reg: Option<SomeRegion> = None;

        for sqrx in sqrs.iter() {
            println!("    {sqrx}");

            if sqrx.pn < min_pn {
                min_pn = sqrx.pn;
            }

            if sqrx.pn > max_pn {
                max_pn = sqrx.pn;
                max_pn_reg = Some(SomeRegion::new(vec![sqrx.state.clone()]));
            } else if sqrx.pn == max_pn {
                if let Some(regx) = max_pn_reg {
                    max_pn_reg = Some(regx.union(&sqrx.state));
                } else {
                    max_pn_reg = Some(SomeRegion::new(vec![sqrx.state.clone()]));
                }
            }
        }

        // Get rule union, if any
        let mut rules: Option<RuleStore> = None;
        let mut non_pn_stas = Vec::<SomeState>::new();
        for sqrx in sqrs.iter() {
            if sqrx.pn == max_pn {
                if max_pn < Pn::Unpredictable {
                    if let Some(ruls) = rules {
                        if let Some(ruls2) = ruls.union(sqrx.rules.as_ref().expect("SNH")) {
                            rules = Some(ruls2);
                        } else {
                            rules = None;
                            break;
                        }
                    } else {
                        rules = Some(sqrx.rules.clone().expect("SNH"));
                    }
                }
            } else if let Some(ref regx) = max_pn_reg {
                if regx.is_superset_of(*sqrx) {
                    non_pn_stas.push(sqrx.state.clone());
                }
            }
        }

        // Check if max Pn squares can form a group.
        let mut form_group = true;
        let mut rules_str = String::from("None");
        if max_pn == Pn::Unpredictable {
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_id].squares.find(stax).unwrap();
                if sqrx.pnc {
                    form_group = false;
                }
            }
        } else if let Some(ruls) = rules {
            rules_str = ruls.to_string();
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_id].squares.find(stax).expect(
                    "States in the non_pn_stas StateStore should all reference existing squares",
                );
                if !sqrx.rules.as_ref().expect("SNH").is_subset_of(&ruls) {
                    form_group = false;
                }
            }
        } else {
            form_group = false;
        }

        if form_group {
            println!(
                "    Min Pn: {min_pn} Max Pn: {max_pn} Rules: {rules_str} Can form group: {form_group}");
        } else {
            println!("    Min Pn: {min_pn} Max Pn: {max_pn} Can form group: {form_group}");
        }
        return Ok(());
    }

    Err(format!("Did not understand {cmd:?}"))
}

/// Do adjacent-anchor command.
fn display_group_anchor_info(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];

    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_id = match dmx.act_id_from_string(cmd[1]) {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let aregion = match dmx.region_from_string(cmd[2]) {
        Ok(aregion) => aregion,
        Err(error) => {
            return Err(error);
        }
    };

    dmx.display_group_anchor_info(act_id, &aregion)
}

/// Do print-group-defining-squares command.
fn do_print_group_defining_squares_command(
    dmxs: &DomainStore,
    cmd: &Vec<&str>,
) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];
    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_id = match dmx.act_id_from_string(cmd[1]) {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let aregion = match dmx.region_from_string(cmd[2]) {
        Ok(aregion) => aregion,
        Err(error) => {
            return Err(error);
        }
    };

    if let Some(grpx) = dmx.actions[act_id].groups.find(&aregion) {
        for stax in grpx.region.states.iter() {
            if let Some(sqrx) = dmx.actions[act_id].squares.find(stax) {
                println!(" {}", sqrx);
            }
        }
    } else {
        return Err(format!("Region {} not found", aregion));
    }
    Ok(())
}

/// Display usage options.
fn usage() {
    println!("\nStartup Commands: <invoke> may be the command \"ues\" or \"cargo run\"");
    println!("\n    <invoke>                 - Run interactively, press Enter for each step.");
    println!(
        "\n    <invoke> 1               - Run non-interactively, stop when no needs can be done."
    );
    println!("\n    <invoke> <number times>  - Run a number, greater than 1, times. Exit with step and duration statistics.");
    println!(
        "\n    <invoke> <file path>     - Open a file previously stored with the fsd command."
    );
    println!("\n    <invoke> [h | help]      - Show this list.\n");
    println!("\nSession Commands:");
    println!("\n    h | help                 - Help list display (this list).");
    println!(
        "\n    cd <dom num>             - Change the Curently Displayed Domain (CDD) to the given domain number."
    );
    println!("\n    Press Enter (no command) - Satisfy one need that can be done, if any.");
    println!("\n    q | exit | quit          - Quit the program.");
    println!("\n\n    aj <act num>             - For an Action in the CDD, display all groups anchor, and adJacent, info.");
    println!("    aj <act num> <region>    - For an Action in the CDD, display group anchor, and adJacent, info.");
    println!("\n    cs <state>               - Change State, an arbitrary change, for the CDD.");
    println!("\n    dn <need number>         - Do a particular Need from the can-do need list.");
    println!("\n    dcs                      - Display Current State, and domain.  After a number of commands,");
    println!("                               the current state scrolls off screen, this might be useful.");
    println!("    fsd <path>               - File Store Data.");
    println!(
        "\n    gps <act num> <region>   - Group Print Squares that define the group region, of a given action, of the CDD."
    );
    println!("\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list.");
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!(
        "    ps <act num> <region>    - Print Squares in a given action and region, of the CDD."
    );
    println!("\n    psr                      - Print Select Regions.");
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!("\n    rx <region>              - Experiment with rules of the CDD and it's current state. Esp. when a need cannot be satisfied.");
    println!("\n    ss <act num>                        - Sample the current State, for a given action, for the CDD.");
    println!("    ss <act num> <state>                - Sample State for a given action and state, for the CDD.");
    println!(
        "    ss <act num> <state> <result-state> - Sample State, for a given action, state and arbitrary result, for the CDD."
    );
    println!("\n    to <region>              - Change the current state TO within a region, by calculating and executing a plan.");
    println!(
        "                               To find out more about why a need cannot be satisfied:"
    );
    println!("                               If needed, use the \"cd\" command, to display the need domain.");
    println!("                               Then use this command, with the need target.");
    println!("\n    A domain number is an integer, zero or greater, where such a domain exists. CDD means the Currently Displayed Domain.");
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("\n    A need number is an integer, zero or greater, where such a need exists.");
    println!("\n    A state starts with an 's0b' or 's0x', followed by zero, or more, digits.");
    println!("\n    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("    A trailing \"+\" indicates the region is formed by more than two states.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. Leading zeros can be omitted.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable.");
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("\n    If there is an select region for the CDD, when no more needs can be done, the program will seek to change the current state");
    println!("    to be in an select region.");
    println!("\n    If there is another select region the current state is not in, after a (3 * number-regions-in) steps, the program will get bored");
    println!("    and seek to move the current state to a different select region, or to an intersection of select regions.");
    println!(
        "\n    \"P:0[]\" means Plan: Domain 0. The current state can be used to satisfy the need."
    );
    println!("\n    \"P:1[2,3]/1/2/+5\" means Plan: Domain 1. Run action 2, then action 3, to change the current state to satisfy the need.");
    println!("    The number of desired bit changes is 1.");
    println!("    The number of bit changes in the plan that cause the desired bit changes is 2.");
    println!("    The regions the plan passes through have an aggregate rating of positive 5.");
    println!("\n    Once the current state is correct, most (but not all) needs require an additional action to get a sample.");
    println!("\n    Needs that cannot be done.  Lets say the current state is s00000000, there is a need for s10000000, and an action that changes");
    println!("    the left-most two bits.  From state s00.. the only option is state s11.. using that action.  Using the command \"cs s10<any 6 more bits>\"");
    println!("    will get things moving again.");
    println!("\n    After no more RUST_BACKTRACE=1needs can be done, select region seeking logic will be used.  If there is more than one select");
    println!("    region, repeatedly pressing enter will increase the boredom duration, after the value of the select regions");
    println!("    the current state is in, a different select region will be sought.");
}

///Pause for input from user.
pub fn pause_for_input(prompt: &str) -> String {
    // Print prompt without going to a new line
    print!("{prompt}");
    io::stdout()
        .flush()
        .expect("This system call should have worked");

    // Init and read in string
    let mut in_str = String::new();
    io::stdin()
        .read_line(&mut in_str)
        .expect("Failed to read line");

    in_str.trim().to_string()
}

/// Load data from a given path string.
fn load_data(path_str: &str) -> Result<DomainStore, String> {
    let path = Path::new(path_str);
    let display = path.display();

    // Open a file, returns `io::Result<File>`
    match File::open(path) {
        Err(why) => Err(format!("Couldn't open {display}: {why}")),
        Ok(mut afile) => {
            let mut serialized = String::new();
            match afile.read_to_string(&mut serialized) {
                Err(why) => Err(format!("Couldn't read {display}: {why}")),
                Ok(_) => {
                    let deserialized_r = serde_yaml::from_str(&serialized);
                    match deserialized_r {
                        Err(why) => Err(format!("Couldn't deserialize {display}: {why}")),
                        Ok(new_dmxs) => Ok(new_dmxs),
                    } // end match deserialized_r
                }
            }
        }
    } // end match open file
}

/// Store current data to a given path string.
fn store_data(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    if cmd.len() != 2 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let path_str = &cmd[1];
    let serialized_r = serde_yaml::to_string(&dmxs);

    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(&path_str);
            let display = path.display();

            // Open a file in write-only mode, returns `io::Result<File>`
            match File::create(path) {
                Err(why) => Err(format!("Couldn't create {display}: {why}")),
                Ok(mut file) => match file.write_all(serialized.as_bytes()) {
                    Err(why) => Err(format!("Couldn't write to {display}: {why}")),
                    Ok(_) => {
                        println!("Data written");
                        Ok(())
                    }
                },
            }
        }
        Err(error) => Err(format!("Couldn't serialize {path_str}: {error}")),
    } // end match serialized_r
} // end store_data
