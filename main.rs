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
mod statestore;
use domain::SomeDomain;
mod needstore;
mod plan;
mod pn;
use pn::Pn;
mod actions;
mod domainstore;
mod step;
mod stepstore;
use domainstore::{DomainStore, InxPlan};
mod actioninterface;
mod planstore;
use planstore::PlanStore;
mod randompick;
mod regionstorecorr;
mod removeunordered;
mod selectregionsstore;
mod target;
mod targetstore;
use crate::regionstorecorr::RegionStoreCorr;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
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
    let mut failures = 0;
    let mut duration_vec = Vec::<Duration>::with_capacity(runs_left);
    let mut steps_vec = Vec::<usize>::with_capacity(runs_left);

    while runs_left > 0 {
        runs_left -= 1;

        let start = Instant::now();
        match do_one_session() {
            Ok(steps) => {
                let duration = start.elapsed();
                println!("Steps {steps}, Time elapsed in do_session() is: {duration:?}");
                duration_vec.push(duration);
                steps_vec.push(steps);
            }
            Err(_) => {
                failures += 1;
            }
        }
    }

    if duration_vec.is_empty() {
        println!("Number with unsatisfied needs: {failures}");
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
    println!("\nRuns {}, Average steps: {} high: {}, low: {}, Average time elapsed: {:.3?}, high: {:.3?}, low: {:.3?} Number with unsatisfied needs {}",
         num_runs, average_steps, steps_high, steps_low, average_time, duration_high, duration_low, failures);
    failures
}

/// Initialize a Domain Store, with two domains and 11 actions.
fn domainstore_init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(2)]);

    // Add actions 0 through 9 to Domain 0;
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();
    dmxs[0].add_action();

    // Add actions 0 through 6 to domain 1.
    dmxs[1].add_action();
    dmxs[1].add_action();
    dmxs[1].add_action();
    dmxs[1].add_action();
    dmxs[1].add_action();
    dmxs[1].add_action();
    dmxs[1].add_action();

    // Load optimal regions
    let mut regstr1 = RegionStoreCorr::with_capacity(2);
    regstr1.push(
        dmxs[0]
            .region_from_string_pad_x("r0x0x")
            .expect("String should be formatted correctly"),
    );
    regstr1.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXX1X_1XXX_XXXX")
            .expect("String should be formatted correctly"),
    );

    let mut regstr2 = RegionStoreCorr::with_capacity(2);
    regstr2.push(
        dmxs[0]
            .region_from_string_pad_x("r0xx1")
            .expect("String should be formatted correctly"),
    );
    regstr2.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXXX1_1XXX_XXXX")
            .expect("String should be formatted correctly"),
    );

    let mut regstr3 = RegionStoreCorr::with_capacity(2);
    regstr3.push(
        dmxs[0]
            .region_from_string_pad_x("rx1x1")
            .expect("String should be formatted correctly"),
    );
    regstr3.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXX00_0XXX_XXXX")
            .expect("String should be formatted correctly"),
    );

    let mut regstr4 = RegionStoreCorr::with_capacity(2);
    regstr4.push(
        dmxs[0]
            .region_from_string_pad_x("r1110")
            .expect("String should be formatted correctly"),
    );
    regstr4.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXXX0_0XXX_XXXX")
            .expect("String should be formatted correctly"),
    );

    let mut regstr5 = RegionStoreCorr::with_capacity(2);
    regstr5.push(
        dmxs[0]
            .region_from_string_pad_x("rXX00")
            .expect("String should be formatted correctly"),
    );
    regstr5.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXXx1_0xXX_XXXX")
            .expect("String should be formatted correctly"),
    );

    let mut regstr6 = RegionStoreCorr::with_capacity(2);
    regstr6.push(
        dmxs[0]
            .region_from_string_pad_x("rX10X")
            .expect("String should be formatted correctly"),
    );
    regstr6.push(
        dmxs[1]
            .region_from_string_pad_x("rXXXXXX1x_x0XX_XXXX")
            .expect("String should be formatted correctly"),
    );

    // Add select regionstores.
    dmxs.add_select(regstr1, 3);
    dmxs.add_select(regstr2, 2);
    dmxs.add_select(regstr3, 3);
    dmxs.add_select(regstr4, 1);
    dmxs.add_select(regstr5, -1);
    dmxs.add_select(regstr6, -1);
    dmxs.calc_select();

    dmxs
}

/// Do one session to end.
/// Return the number steps taken get to the point where
/// there are no more needs.
/// Return error if no needs that can be done are available and
/// there are needs than cannot be done.
fn do_one_session() -> Result<usize, String> {
    let mut dmxs = domainstore_init();
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        dmxs.generate_and_display_needs();

        // Check for end.
        if dmxs.can_do.is_empty() {
            if dmxs.cant_do.is_empty() {
                return Ok(dmxs.step_num);
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
            if !dmxs.can_do.is_empty() {
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
                }
            }
            "cd" => match do_change_domain(dmxs, &cmd) {
                Ok(()) => {
                    dmxs.print_domain();
                    dmxs.print_can_do();
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
                if !dmxs.can_do.is_empty() {
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
            _ => {
                println!("\nDid not understand command: {cmd:?}");
            }
        };
    } // end loop
} // end command_loop

/// Change the domain to a number given by user.
fn do_change_domain(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Get domain number from string
    match dmxs.domain_num_from_string(cmd[1]) {
        Ok(d_num) => {
            dmxs.change_domain(d_num);
            Ok(())
        }
        Err(error) => Err(error),
    } // end match
}

/// Choose a need from a number of possibilities.
/// Attempt to satisfy the chosen need.
fn do_any_need(dmxs: &mut DomainStore) {
    let np_inx = dmxs.choose_need();

    if do_a_need(dmxs, dmxs.can_do[np_inx].clone()) {
        println!("Need satisfied");
    }
}

/// Print details of a given plan
fn do_print_plan_details(dmxs: &DomainStore, cmd: &[&str]) -> Result<(), String> {
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let ndx = &dmxs.needs[dmxs.can_do[n_num].inx];
                let pln = &dmxs.can_do[n_num]
                    .plans
                    .as_ref()
                    .expect("Any need in the can_do vector should have a non-None plan");

                println!("\n{} Need: {}", &n_num, &ndx);
                match ndx {
                    SomeNeed::ToSelectRegion { .. } => {
                        //println!("\n{}", &pln.str2());
                        print_plan_detail(dmxs, pln);
                    }
                    _ => {
                        if ndx.satisfied_by(dmxs[ndx.dom_num()].get_current_state()) {
                            println!("\nPlan: current state satisfies need, just take the action");
                        } else {
                            //println!("\n{}", &pln.str2());
                            print_plan_detail(dmxs, pln);
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
    let dom_num = dmxs.current_domain;
    let nd_inx = inx_pln.inx;

    // Display Domain info, if needed.
    match dmxs.needs[nd_inx] {
        SomeNeed::ToSelectRegion { .. } => {
            //println!("\nNeed chosen: {} {}", &ndx, &plans.str_terse())
        }
        SomeNeed::ExitSelectRegion { .. } => {
            //println!("\nNeed chosen: {} {}", &ndx, &plans.str_terse())
        }
        _ => {
            let nd_dom = dmxs.needs[nd_inx].dom_num();
            if dom_num != nd_dom {
                // Show "before" state before running need.
                println!(
                    "\nAll domain states: {}",
                    SomeState::vec_ref_string(&dmxs.all_current_states())
                );
                dmxs.change_domain(nd_dom);
                dmxs.print_domain();
                //println!("\nNeed chosen: {} {}", &ndx, &plans.str_terse());
            }
        }
    }

    // Run the plan, allow for one failure.
    if let Some(plans) = &inx_pln.plans {
        if !dmxs.run_plan_store(plans) {
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
    }

    // Take action after the desired state is reached.
    match dmxs.needs[nd_inx] {
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
                dmxs.set_boredom_limit();
                return true;
            }
        }
        _ => {
            if dmxs.needs[nd_inx].satisfied_by(dmxs.cur_state(dmxs.needs[nd_inx].dom_num())) {
                dmxs.take_action_need(nd_inx);
                return true;
            }
        }
    }
    false
}

/// Try to satisfy a need chosen by the user.
fn do_chosen_need(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    let dom_num = dmxs.current_domain;

    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let nd_inx = dmxs.can_do[n_num].inx;

                let plans = &dmxs.can_do[n_num]
                    .plans
                    .as_ref()
                    .expect("Any need in the can_do vector should have a non-None plan");

                println!(
                    "\nNeed chosen: {:2} {} {}",
                    n_num,
                    &dmxs.needs[nd_inx],
                    &plans.str_terse()
                );

                match dmxs.needs[nd_inx] {
                    SomeNeed::ToSelectRegion { .. } => (),
                    SomeNeed::ExitSelectRegion { .. } => (),
                    _ => {
                        if dom_num != dmxs.needs[nd_inx].dom_num() {
                            dmxs.change_domain(dmxs.needs[nd_inx].dom_num());
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
    let dom_num = dmxs.current_domain;
    let dmx = &mut dmxs[dom_num];

    let cur_state = dmx.get_current_state();

    // Get region from string
    match dmx.region_from_string(cmd[1]) {
        Ok(goal_region) => {
            println!("\nChange Current_state {cur_state} to region {goal_region}");
            if goal_region.is_superset_of_state(cur_state) {
                println!(
                    "\nCurrent_state {} is already in region {}",
                    dmx.get_current_state(),
                    goal_region
                );
            } else if dmxs.seek_state_in_region(dom_num, &goal_region) {
                println!("\nChange to region succeeded");
            } else {
                println!("\nChange to region failed");
            }
            Ok(())
        }
        Err(error) => Err(error),
    } // end match region_r
}

/// Do sample-state command.
fn do_sample_state_command(dmxs: &mut DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_num = dmxs.current_domain;
    let dmx = &mut dmxs[dom_num];

    let cur_state = dmx.get_current_state();

    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 {
        println!("Act {act_num} sample State {cur_state}");
        dmx.take_action_arbitrary(act_num);
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

        println!("Act {act_num} sample State {a_state}");
        dmx.set_state(&a_state);
        dmx.take_action_arbitrary(act_num);
        return Ok(());
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state

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

        println!("Act {act_num} take sample {i_state} -> {r_state}");
        let smpl = SomeSample::new(i_state, act_num, r_state);
        dmx.eval_sample_arbitrary(&smpl);
        return Ok(());
    } // end command ss 4

    Err(format!("Did not understand {cmd:?}"))
}

/// Display anchors, rating, and adjacent squares, for an action.
/// For a group that has an anchor, and is limited, the number edges, that can be changed with actions,
/// should equal the sum of the first two number of the rating.
fn display_action_anchor_info(dmxs: &mut DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_num = dmxs.current_domain;
    let dmx = &mut dmxs[dom_num];

    if cmd.len() == 1 {
        return Err("Need to supply an action number".to_string());
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            return Err(error);
        }
    };

    // Display the rates
    dmxs[dom_num].display_action_anchor_info(act_num)
}

fn print_plan_detail(dom_str: &DomainStore, plan_str: &PlanStore) {
    let mut cur_states = dom_str.all_current_states();

    for planx in plan_str.iter() {
        if planx.is_empty() {
            continue;
        }

        println!("\nDomain: {}, Plan:", planx.dom_num);

        for (inx, stepx) in planx.iter().enumerate() {
            let df = stepx.initial.diff_mask(&stepx.result);
            print!(
                "{} Action {:02} Group {} ",
                &stepx.initial, &stepx.act_num, &stepx.group_reg
            );
            if inx > 0 {
                for sel_regx in dom_str.select.iter() {
                    if sel_regx.regions.is_superset_states(&cur_states) && sel_regx.value < 0 {
                        print!(" in {:+}", sel_regx);
                    }
                }
            }
            println!("\n{}", df.str2());
            cur_states[planx.dom_num] = stepx.result.state1();
        } // next steps
        println!("{}", planx.result_region());
    } // next planx
}

/// Do print-squares command.
fn do_print_select_regions(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
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
    let dom_num = dmxs.current_domain;
    let dmx = &dmxs[dom_num];

    if cmd.len() == 1 {
        return Err("Need to supply at least an action number".to_string());
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            return Err(error);
        }
    };

    if cmd.len() == 2 {
        println!(
            "Squares of Action {} are:\n{}\n",
            &act_num, &dmx.actions[act_num].squares
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

        println!("Squares of Action {act_num} in region {aregion} are:\n");

        let sqrs = dmx.actions[act_num].squares.squares_in_reg(&aregion);
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
                    max_pn_reg = Some(regx.union_state(&sqrx.state));
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
                if regx.is_superset_of_state(&sqrx.state) {
                    non_pn_stas.push(sqrx.state.clone());
                }
            }
        }

        // Check if max Pn squares can form a group.
        let mut form_group = true;
        let mut rules_str = String::from("None");
        if max_pn == Pn::Unpredictable {
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                if sqrx.pnc {
                    form_group = false;
                }
            }
        } else if let Some(ruls) = rules {
            rules_str = ruls.formatted_string();
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_num].squares.find(stax).expect(
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
    let dom_num = dmxs.current_domain;
    let dmx = &dmxs[dom_num];

    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
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

    dmx.display_group_anchor_info(act_num, &aregion)
}

/// Do print-group-defining-squares command.
fn do_print_group_defining_squares_command(
    dmxs: &DomainStore,
    cmd: &Vec<&str>,
) -> Result<(), String> {
    let dom_num = dmxs.current_domain;
    let dmx = &dmxs[dom_num];
    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
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

    if let Some(grpx) = dmx.actions[act_num].groups.find(&aregion) {
        for stax in grpx.region.states.iter() {
            if let Some(sqrx) = dmx.actions[act_num].squares.find(stax) {
                println!(" {}", &sqrx);
            } else {
                println!("far state {} not found", stax);
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
    println!("\n    ss <act num>                        - Sample the current State, for a given action, for the CDD.");
    println!("    ss <act num> <state>                - Sample State for a given action and state, for the CDD.");
    println!(
        "    ss <act num> <state> <result-state> - Sample State, for a given action, state and arbitrary result, for the CDD."
    );
    println!("\n    to <region>              - Change the current state TO within a region, by calculating and executing a plan.");
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
    println!("    \"P:1[2,3]/+5\" means Plan: Domain 1. Run action 2, then action 3, to change the current state to satisfy the need.");
    println!("    Regions the plan passes through have an aggregate rating of positive 5.");
    println!("\n    Once the current state is correct, most (but not all) needs require an additional action to get a sample.");
    println!("\n    Needs that cannot be done.  Lets say the current state is s00000000, there is a need for s10000000, and an action that changes");
    println!("    the left-most two bits.  From state s00.. the only option is state s11.. using that action.  Using the command \"cs s10<any 6 more bits>\"");
    println!("    will get things moving again.");
    println!("\n    After no more needs can be done, select region seeking logic will be used.  If there is more than one select");
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
