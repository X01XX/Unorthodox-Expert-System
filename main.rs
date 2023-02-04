// Main function for an Unorthodox Expert System

//#![allow(
//dead_code,
//unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
//)]

use std::env;
mod action;
mod actionstore;
mod bits;
mod group;
mod groupstore;
mod mask;
mod need;
use need::SomeNeed;
mod region;
use region::SomeRegion;
mod change;
mod regionstore;
use regionstore::RegionStore;
mod resultstore;
mod rule;
mod rulestore;
use rulestore::RuleStore;
mod square;
mod squarestore;
mod state;
use state::somestate_ref_vec_string;
mod statestore;
use statestore::StateStore;
mod domain;
pub use domain::SomeDomain;
mod needstore;
pub use needstore::NeedStore;
mod plan;
mod pn;
use pn::Pn;
mod actions;
mod domainstore;
mod step;
mod stepstore;
pub use domainstore::{DomainStore, InxPlan};
mod actioninterface;
mod optimalregionsstore;
mod planstore;
use planstore::PlanStore;
mod randompick;
mod removeunordered;
mod target;
mod targetstore;
mod truth;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
use std::time::{Duration, Instant};

/// Initialize a Domain Store, with two domains and 11 actions.
fn init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    // Create domain 0.
    let dom_num_ints: usize = 1;

    // Add domain to the DomainStore.
    let inx0 = dmxs.push(SomeDomain::new(dom_num_ints));

    // Add actions 0 through 8;
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();
    dmxs[inx0].add_action();

    // Create domain 1.
    let dom_num_ints: usize = 2;

    // Add a domain to the DomainStore.
    let inx1 = dmxs.push(SomeDomain::new(dom_num_ints));

    // Add actions 0 through 5.
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();
    dmxs[inx1].add_action();

    // Load optimal regions
    let mut regstr1 = RegionStore::with_capacity(2);
    regstr1.push(dmxs[inx0].region_from_string_pad_x("r0x0x").unwrap());
    regstr1.push(
        dmxs[inx1]
            .region_from_string_pad_x("rXXXXXX1X_1XXX_XXXX")
            .unwrap(),
    );

    let mut regstr2 = RegionStore::with_capacity(2);
    regstr2.push(dmxs[inx0].region_from_string_pad_x("r0xx1").unwrap());
    regstr2.push(
        dmxs[inx1]
            .region_from_string_pad_x("rXXXXXXX1_1XXX_XXXX")
            .unwrap(),
    );

    let mut regstr3 = RegionStore::with_capacity(2);
    regstr3.push(dmxs[inx0].region_from_string_pad_x("rx1x1").unwrap());
    regstr3.push(
        dmxs[inx1]
            .region_from_string_pad_x("rXXXXXX00_0XXX_XXXX")
            .unwrap(),
    );

    let mut regstr4 = RegionStore::with_capacity(2);
    regstr4.push(dmxs[inx0].region_from_string_pad_x("r1110").unwrap());
    regstr4.push(
        dmxs[inx1]
            .region_from_string_pad_x("rXXXXXXX0_0XXX_XXXX")
            .unwrap(),
    );

    // Add optimal regionstores.
    dmxs.add_optimal(regstr1);
    dmxs.add_optimal(regstr2);
    dmxs.add_optimal(regstr3);
    dmxs.add_optimal(regstr4);

    //println!("optimal and ints: {}", dmxs.optimal_and_ints.formatted_string());
    dmxs
}

/// The User Interface.
fn main() {
    // Start a DomainStore, add a Domain
    let args: Vec<String> = env::args().collect();
    //println!("{:?}", args);

    let mut run_to_end = false;
    let mut run_left = 1;

    if args.len() > 1 {
        if args[0] == "h" || args[0] == "help" {
            usage();
            return;
        }
        run_left = args[1].parse::<usize>().unwrap_or_else(|err| {
            println!("String to Number conversion error: {err}");
            0
        });
        if run_left == 0 {
            usage();
            return;
        }
        run_to_end = true;
    }

    usage();

    let run_count = 1;
    let run_max = run_left;

    if run_left > 1 {
        let mut runs = 0;
        let mut duration_vec = Vec::<Duration>::with_capacity(run_left);
        let mut steps_vec = Vec::<usize>::with_capacity(run_left);

        while run_left > 0 {
            run_left -= 1;
            runs += 1;

            let start = Instant::now();
            let steps = do_session(run_to_end, run_count, run_max);
            if steps == 0 {
                return;
            }
            let duration = start.elapsed();
            println!("Steps {steps}, Time elapsed in do_session() is: {duration:?}");
            duration_vec.push(duration);
            steps_vec.push(steps);
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
            steps_total += stepsx;
            if *stepsx > steps_high {
                steps_high = *stepsx;
            }
            if *stepsx < steps_low {
                steps_low = *stepsx;
            }
        }
        println!("\nRuns {}, Average steps: {} high: {}, low: {}, Average time elapsed: {:?}, high: {:?}, low: {:?}",
         runs, steps_total / runs, steps_high, steps_low, duration_total / runs as u32, duration_high, duration_low);
        return;
    }

    // Run do_session until quit.
    // In do_session, the command "so" (start over) reruns do_session.
    loop {
        do_session(run_to_end, run_count, run_max);
    } // end while
} // end main

/// Do one session of finding and using rules.
/// Return 0 to start over, or number of steps.
pub fn do_session(run_to_end: bool, run_count: usize, run_max: usize) -> usize {
    let mut to_end = run_to_end;
    let mut dmxs = init();
    let mut dom_num = 0;
    let mut stepx = 0; // Current step number of the session.

    loop {
        stepx += 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();

        let need_plans = dmxs.evaluate_needs(&nds);

        println!(
            "\nStep {} All domain states: {}",
            stepx,
            somestate_ref_vec_string(&dmxs.all_current_states())
        );
        assert!(stepx < 800); // Remove for continuous use

        print_domain(&dmxs, dom_num);

        //println!("session loop 3");

        // Vector for position = display index, val = need_plans index
        let mut need_can = Vec::<usize>::with_capacity(nds.len());

        let mut can_do = 0;
        let mut cant_do = 0;

        for ndx in need_plans.iter() {
            if ndx.plans.is_some() {
                can_do += 1;
            } else {
                cant_do += 1;
            }
        }

        if !nds.is_empty() {
            // Print needs that cannot be done.
            if cant_do == 0 {
                println!("\nNeeds that cannot be done: None");
            } else {
                println!("\nNeeds that cannot be done:");

                for ndplnx in need_plans.iter() {
                    if ndplnx.plans.is_some() {
                        continue;
                    }
                    println!("   {}", nds[ndplnx.inx]);
                }
            }

            // Print needs that can be done.
            if can_do == 0 {
                println!("\nNeeds that can be done: None");
                dmxs.print_optimal();
            } else {
                println!("\nNeeds that can be done:");

                for (inx, ndplnx) in need_plans.iter().enumerate() {
                    if let Some(plans) = &ndplnx.plans {
                        println!(
                            "{:2} {} {}",
                            need_can.len(),
                            &nds[ndplnx.inx],
                            plans.str_terse()
                        );
                        need_can.push(inx);
                    }
                } // next ndplnx
            }

            // Stop running for this condition
            if cant_do > 0 && can_do == 0 {
                if run_count != run_max || run_max > 1 {
                    println!("\nrun_count {run_count} of {run_max}");
                }
                to_end = false;
            }
        } else {
            if run_max == 1 {
                println!("\nAction needs: None");
            } else {
                println!("\nAction needs: None, run_count {run_count} of {run_max}");
            }
            dmxs.print_optimal();
            if to_end {
                if run_count < run_max {
                    return stepx;
                }
                to_end = false;
            }
        }

        if !to_end || (cant_do > 0 && can_do == 0) {
            // Start command loop
            // In the loop,
            // The break command will end the loop, display the domain and needs, without incrementing the step number.
            // The continue will prompt for another command.
            // step_inc will be changed to zero if a command should not change the step number.
            loop {
                to_end = false;

                //println!("start command loop");
                let mut cmd = Vec::<&str>::with_capacity(10);

                let guess = pause_for_input("\nPress Enter or type a command: ");

                for word in guess.split_whitespace() {
                    cmd.push(word);
                }

                // Default command, just press Enter
                if cmd.is_empty() {
                    // Process needs
                    if can_do > 0 {
                        //println!("\nAction needs: {}", nds);

                        dom_num = do_any_need(&mut dmxs, dom_num, &nds, &need_plans, &need_can);

                        break;
                    } // end-if can_do > 0

                    break;
                } // end if cmd.len() == 0

                // Do other commands
                if cmd.len() == 1 {
                    match cmd[0] {
                        "q" | "exit" | "quit" => {
                            println!("Done");
                            process::exit(1);
                        }
                        "so" => {
                            return 0;
                        }
                        "run" => {
                            to_end = true;
                            stepx -= 1;
                            break;
                        }
                        "dcs" => {
                            stepx -= 1;
                            break;
                        }
                        _ => (),
                    }
                }

                if cmd.len() == 2 && cmd[0] == "fld" {
                    match load_data(cmd[1]) {
                        Ok(new_dmxs) => {
                            println!("Data loaded");
                            (stepx, dmxs) = new_dmxs;
                            stepx -= 1;
                            break;
                        }
                        Err(why) => {
                            println!("couldn't read {}: {}", &cmd[1], why);
                        }
                    } // end match load_data
                    continue;
                }

                // Do other commands
                match cmd[0] {
                    "h" => usage(),
                    "help" => usage(),
                    "cs" => do_change_state_command(&mut dmxs[dom_num], &cmd),
                    "to" => do_to_region_command(&mut dmxs[dom_num], &cmd),
                    "ss" => do_sample_state_command(&mut dmxs[dom_num], &cmd),
                    "ps" => do_print_squares_command(&mut dmxs[dom_num], &cmd),
                    "aj" => do_adjacent_anchor_command(&mut dmxs[dom_num], &cmd),
                    "gps" => do_print_group_defining_squares_command(&mut dmxs[dom_num], &cmd),
                    "fsd" => store_data(&dmxs, stepx, &cmd),
                    "ppd" => do_print_plan_details(&mut dmxs, &cmd, &nds, &need_plans, &need_can),
                    "cd" => {
                        dom_num = do_change_domain(&dmxs, dom_num, &cmd);
                        stepx -= 1;
                        break;
                    }
                    "dn" => {
                        dom_num =
                            do_chosen_need(&mut dmxs, dom_num, &cmd, &nds, &need_plans, &need_can);
                        break;
                    }
                    _ => {
                        println!("\nDid not understand command: {cmd:?}");
                    }
                };
            } // end command loop
        } else if can_do > 0 {
            dom_num = do_any_need(&mut dmxs, dom_num, &nds, &need_plans, &need_can);
        }
    } // end loop
} // end do_session

/// Change the domain to a number given by user.
fn do_change_domain(dmxs: &DomainStore, dom_num: usize, cmd: &[&str]) -> usize {
    // Get domain number from string
    match dmxs.domain_num_from_string(cmd[1]) {
        Ok(d_num) => {
            return d_num;
        }
        Err(error) => {
            println!("\n{error}");
        }
    } // end match
    dom_num
}

/// Choose a need from a number of possibilities.
/// Attempt to satisfy the chosen need.
fn do_any_need(
    dmxs: &mut DomainStore,
    dom_num: usize,
    nds: &NeedStore,
    need_plans: &[InxPlan],
    need_can: &Vec<usize>,
) -> usize {
    let np_inx = dmxs.choose_need(nds, need_plans, need_can);

    let nd_inx = need_plans[np_inx].inx;
    let ndx = &nds[nd_inx];
    let plans = &need_plans[np_inx].plans.as_ref().unwrap();

    if do_a_need(dmxs, dom_num, ndx, plans) {
        println!("Need satisfied");
    }

    match ndx {
        SomeNeed::ToOptimalRegion { .. } => dom_num,
        SomeNeed::ToRegion { .. } => dom_num,
        _ => ndx.dom_num(),
    }
}

/// Print details of a given plan
fn do_print_plan_details(
    dmxs: &mut DomainStore,
    cmd: &[&str],
    nds: &NeedStore,
    need_plans: &[InxPlan],
    need_can: &Vec<usize>,
) {
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= need_can.len() {
                println!("Invalid Need Number: {}", cmd[1]);
            } else {
                let ndx = &nds[need_plans[need_can[n_num]].inx];
                let pln = &need_plans[need_can[n_num]].plans.as_ref().unwrap();

                println!("\n{} Need: {}", &n_num, &ndx);
                match ndx {
                    SomeNeed::ToOptimalRegion { .. } => {
                        println!("\n{}", &pln.str2());
                    }
                    _ => {
                        if ndx.satisfied_by(dmxs[ndx.dom_num()].get_current_state()) {
                            println!("\nPlan: current state satisfies need, just take the action");
                        } else {
                            println!("\n{}", &pln.str2());
                        }
                    }
                }
            }
        }
        Err(error) => {
            println!("\n{error}");
        }
    }
}

/// Try to satisfy a need.
/// Return true if success.
fn do_a_need(dmxs: &mut DomainStore, dom_num: usize, ndx: &SomeNeed, plans: &PlanStore) -> bool {
    match ndx {
        SomeNeed::ToOptimalRegion { .. } => {
            println!("\nNeed chosen: {} {}", &ndx, &plans.str_terse())
        }
        SomeNeed::ToRegion { .. } => println!("\nNeed chosen:{} {}", &ndx, &plans.str_terse()),
        _ => {
            if dom_num != ndx.dom_num() {
                // Show "before" state before running need.
                println!(
                    "\nAll domain states: {}",
                    somestate_ref_vec_string(&dmxs.all_current_states())
                );
                print_domain(dmxs, ndx.dom_num());
                println!("\nNeed chosen: {} {}", &ndx, &plans.str_terse());
            }
        }
    }

    if !plans.is_empty() {
        dmxs.run_plans(plans);
    }

    match ndx {
        SomeNeed::ToOptimalRegion { .. } => {
            if ndx
                .target()
                .is_superset_of_states(&dmxs.all_current_states())
            {
                dmxs.set_boredom_limit();
                return true;
            }
        }
        SomeNeed::ToRegion { .. } => {
            if ndx
                .target()
                .is_superset_of_state(dmxs.cur_state(ndx.dom_num()))
            {
                return true;
            }
        }
        _ => {
            if ndx.satisfied_by(dmxs.cur_state(ndx.dom_num())) {
                dmxs.take_action_need(ndx.dom_num(), ndx);
                return true;
            }
        }
    }
    false
}

/// Try to satisfy a need chosen by the user.
fn do_chosen_need(
    dmxs: &mut DomainStore,
    dom_num: usize,
    cmd: &[&str],
    nds: &NeedStore,
    need_plans: &[InxPlan],
    need_can: &Vec<usize>,
) -> usize {
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= need_can.len() {
                println!("Invalid Need Number: {}", cmd[1]);
                dom_num
            } else {
                let ndx = &nds[need_plans[need_can[n_num]].inx];

                let plans = &need_plans[need_can[n_num]].plans.as_ref().unwrap();

                if do_a_need(dmxs, dom_num, ndx, plans) {
                    println!("Need satisfied");
                }

                match ndx {
                    SomeNeed::ToOptimalRegion { .. } => dom_num,
                    SomeNeed::ToRegion { .. } => dom_num,
                    _ => ndx.dom_num(),
                }
            }
        }
        Err(error) => {
            println!("\n{error}");
            dom_num
        }
    }
}

/// Do a change-state command.
/// Return 1 is Ok, 0 if not.
fn do_change_state_command(dmx: &mut SomeDomain, cmd: &[&str]) {
    // Get state from string
    match dmx.state_from_string(cmd[1]) {
        Ok(a_state) => {
            println!("Changed state to {a_state}");
            dmx.set_state(&a_state);
        }
        Err(error) => {
            println!("\nDid not understand state, {error}");
        }
    } // end match
}

/// Do to-region command.
/// Return 1 is Ok, 0 if not.
fn do_to_region_command(dmx: &mut SomeDomain, cmd: &[&str]) {
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
            } else if dmx.to_region(&goal_region) {
                println!("\nChange to region succeeded");
            } else {
                println!("\nChange to region failed");
            }
        }
        Err(error) => {
            println!("\n{error}");
        }
    } // end match region_r
}

/// Do sample-state command.
/// Return 1 is Ok, 0 if not.
fn do_sample_state_command(dmx: &mut SomeDomain, cmd: &Vec<&str>) {
    let cur_state = dmx.get_current_state();

    if cmd.len() == 1 {
        println!("Did not understand {cmd:?}");
        return;
    }

    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if cmd.len() == 2 {
        println!("Act {act_num} sample State {cur_state}");
        dmx.take_action_arbitrary(act_num);
        return;
    }

    if cmd.len() == 3 {
        // Get state from string
        let a_state = match dmx.state_from_string(cmd[2]) {
            Ok(a_state) => a_state,
            Err(error) => {
                println!("\n{error}");
                return;
            }
        };

        println!("Act {act_num} sample State {a_state}");
        dmx.set_state(&a_state);
        dmx.take_action_arbitrary(act_num);
        return;
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state

        // Get i-state from string
        let i_state = match dmx.state_from_string(cmd[2]) {
            Ok(i_state) => i_state,
            Err(error) => {
                println!("\n{error}");
                return;
            }
        };

        // Get r-state from string
        let r_state = match dmx.state_from_string(cmd[3]) {
            Ok(r_state) => r_state,
            Err(error) => {
                println!("\n{error}");
                return;
            }
        };

        println!("Act {act_num} take sample {i_state} -> {r_state}");
        dmx.eval_sample_arbitrary(act_num, &i_state, &r_state);
        return;
    } // end command ss 4

    println!("Did not understand {cmd:?}");
}

/// Do print-squares command.
/// Return 1 is Ok, 0 if not.
fn do_print_squares_command(dmx: &mut SomeDomain, cmd: &Vec<&str>) {
    if cmd.len() == 1 {
        return;
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if cmd.len() == 2 {
        println!(
            "Squares of Action {} are:\n{}\n",
            &act_num, &dmx.actions[act_num].squares
        );
        return;
    }

    if cmd.len() == 3 {
        // Get region
        let aregion = match dmx.region_from_string(cmd[2]) {
            Ok(aregion) => aregion,
            Err(error) => {
                println!("\n{error}");
                return;
            }
        };

        println!("Squares of Action {act_num} in region {aregion} are:\n");

        let stas = dmx.actions[act_num].squares.stas_in_reg(&aregion);
        if stas.is_empty() {
            println!("No squares in region {aregion}");
            return;
        }

        let mut max_pn = Pn::One;
        let mut min_pn = Pn::Unpredictable;
        let mut max_pn_reg: Option<SomeRegion> = None;

        for stax in stas.iter() {
            let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
            println!("    {sqrx}");

            if sqrx.pn < min_pn {
                min_pn = sqrx.pn;
            }

            if sqrx.pn > max_pn {
                max_pn = sqrx.pn;
                max_pn_reg = Some(SomeRegion::new(sqrx.state.clone(), sqrx.state.clone()));
            } else if sqrx.pn == max_pn {
                if let Some(regx) = max_pn_reg {
                    max_pn_reg = Some(regx.union_state(&sqrx.state));
                } else {
                    max_pn_reg = Some(SomeRegion::new(sqrx.state.clone(), sqrx.state.clone()));
                }
            }
        }

        // Get rule union, if any
        let mut rules: Option<RuleStore> = None;
        let mut non_pn_stas = StateStore::new();
        for stax in stas.iter() {
            let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
            if sqrx.pn == max_pn {
                if max_pn < Pn::Unpredictable {
                    if let Some(ruls) = rules {
                        if let Some(ruls2) = ruls.union(&sqrx.rules) {
                            rules = Some(ruls2);
                        } else {
                            rules = None;
                            break;
                        }
                    } else {
                        rules = Some(sqrx.rules.clone());
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
                let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                if !sqrx.rules.is_subset_of(&ruls) {
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
        return;
    }

    println!("Did not understand {cmd:?}");
}

/// Do adjacent-anchor command.
/// Return 1 is Ok, 0 if not.
fn do_adjacent_anchor_command(dmx: &mut SomeDomain, cmd: &Vec<&str>) {
    if cmd.len() == 1 {
        println!("Did not understand {cmd:?}");
        return;
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        println!("Did not understand {cmd:?}");
        return;
    }

    let aregion = match dmx.region_from_string(cmd[2]) {
        Ok(aregion) => aregion,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if let Some(grpx) = dmx.actions[act_num].groups.find(&aregion) {
        if let Some(anchor) = &grpx.anchor {
            println!("\n  {aregion}");
            let stas_adj = dmx.actions[act_num].squares.stas_adj_reg(&grpx.region);
            for stax in stas_adj.iter() {
                if stax.is_adjacent(anchor) {
                    let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                    println!("{sqrx}");
                }
            }
        } else {
            println!("\nGroup {aregion} does not have an anchor defined");
        }
    } else {
        println!("\nGroup with region {aregion} not found");
    }
}

/// Do print-group-defining-squares command.
/// Return 1 is Ok, 0 if not.
fn do_print_group_defining_squares_command(dmx: &mut SomeDomain, cmd: &Vec<&str>) {
    if cmd.len() == 1 {
        println!("Did not understand {cmd:?}");
        return;
    }

    // Get action number
    let act_num = match dmx.act_num_from_string(cmd[1]) {
        Ok(act_num) => act_num,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        println!("Did not understand {cmd:?}");
        return;
    }

    let aregion = match dmx.region_from_string(cmd[2]) {
        Ok(aregion) => aregion,
        Err(error) => {
            println!("\n{error}");
            return;
        }
    };

    if let Some(grpx) = dmx.actions[act_num].groups.find(&aregion) {
        if let Some(sqrx) = dmx.actions[act_num].squares.find(&grpx.region.state1) {
            println!("state1   {}", &sqrx);
        } else {
            println!("state1   {} not found?", &grpx.region.state1);
            return;
        }

        if grpx.region.state1 == grpx.region.state2 {
        } else {
            if let Some(sqrx) = dmx.actions[act_num].squares.find(&grpx.region.state2) {
                println!("state2   {}", &sqrx);
                return;
            }
            println!("state2   {} not found?", &grpx.region.state1);
        }
    } else {
        println!("\nGroup with region {} not found", &aregion);
    }
}

/// Print a domain.
fn print_domain(dmxs: &DomainStore, dom_num: usize) {
    print!("\nCurrent Domain: {} of {}", dom_num, dmxs.len(),);

    println!("\nActs: {}", &dmxs[dom_num].actions);

    let cur_state = &dmxs[dom_num].get_current_state();

    println!("\nDom: {} Current State: {}", dom_num, &cur_state);
}

/// Display usage options.
fn usage() {
    println!("\nStartup Commands: <invoke> may be the command \"ues\" or \"cargo run\"");
    println!("\n    <invoke>                 - Run interactively, press Enter for each step.");
    println!(
        "\n    <invoke> 1               - Run non-interactively, stop when no needs can be done."
    );
    println!("\n    <invoke> <number times>  - Run a number, greater than 1, times. Exit with step and duration statistics.");
    println!("\n                               Or drop into interactive mode if there are needs, but none can be done.");
    println!("\n    <invoke> [h | help]      - Show this list.\n");

    println!("\nSession Commands:");
    println!("\n    h | help                 - Help list display (this list).");
    println!(
        "\n    cd <dom num>             - Change the Curently Displayed Domain (CDD) to the given domain number."
    );
    println!("\n    Press Enter (no command) - Satisfy one need that can be done, if any.");
    println!("\n    q | exit | quit          - Quit the program.");
    println!("\n\n    aj <act num> <group-region>    - For an Action in the CDD, print adJacent squares to the groups anchor");

    println!("\n    cs <state>               - Change State, an arbitrary change, for the CDD.");
    println!("\n    dn <need number>         - Do a particular Need from the can-do need list.");
    println!("\n    dcs                      - Display Current State, and domain.  After a number of commands,");
    println!("                               the current state scrolls off screen, this might be useful.");
    println!("\n    fld <path>               - File Load Data.");
    println!("    fsd <path>               - File Store Data.");
    println!(
        "\n    gps <act num> <region>   - Group Print Squares that define the group region, of a given action, of the CDD."
    );
    println!("\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list.");
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!(
        "    ps <act num> <region>    - Print Squares in a given action and region, of the CDD."
    );
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!("\n    so                       - Start Over.");
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
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. Leading zeros can be omitted.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable.");
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("\n    If there is an optimal region for the CDD, when no more needs can be done, the program will seek to change the current state");
    println!("    to be in an optimal region.");
    println!("\n    If there is another optimal region the current state is not in, after a (3 * number-regions-in) steps, the program will get bored");
    println!("    and seek to move the current state to a different optimal region, or to an intersection of optimal regions.");
    println!(
        "\n    \"P:0[]\" means Plan: Domain 0. The current state can be used to satisfy the need."
    );
    println!("    \"P:1[2,3]\" means Plan: Domain 1. Run action 2, then action 3, to change the current state to satisfy the need.");
    println!("    Once the current state is correct, most (but not all) needs require an additional action to get a sample.");
    println!("\n    Needs that cannot be done.  Lets say the current state is s00000000, there is a need for s10000000, and an action that changes");
    println!("    the left-most two bits.  From state s00.. the only option is state s11.. using that action.  Using the command \"cs s10<any 6 more bits>\"");
    println!("    will get things moving again.");
    println!("\n    After no more needs can be done, optimal region seeking logic will be used.  If there are more than one optimal");
    println!("    regions, repeatedly pressing enter will increase the boredom duration, and after 3 times the number of optimal regions");
    println!("    the current state is in, a different optimal region will be sought.");
}

///Pause for input from user.
pub fn pause_for_input(prompt: &str) -> String {
    // Print prompt without going to a new line
    print!("{prompt}");
    io::stdout().flush().unwrap();

    // Init and read in string
    let mut in_str = String::new();
    io::stdin()
        .read_line(&mut in_str)
        .expect("Failed to read line");

    in_str.trim().to_string()
}

/// Load data from a given path string.
fn load_data(path_str: &str) -> Result<(usize, DomainStore), String> {
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
fn store_data(dmxs: &DomainStore, stepx: usize, cmd: &Vec<&str>) {
    if cmd.len() != 2 {
        println!("Did not understand {cmd:?}");
        return;
    }

    let path_str = &cmd[1];
    let serialized_r = serde_yaml::to_string(&(stepx, &dmxs));

    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(&path_str);
            let display = path.display();

            // Open a file in write-only mode, returns `io::Result<File>`
            match File::create(path) {
                Err(why) => println!("Couldn't create {display}: {why}"),
                Ok(mut file) => match file.write_all(serialized.as_bytes()) {
                    Err(why) => println!("Couldn't write to {display}: {why}"),
                    Ok(_) => {
                        println!("Data written");
                    }
                },
            }
        }
        Err(error) => println!("Couldn't serialize {path_str}: {error}"),
    } // end match serialized_r
} // end store_data
