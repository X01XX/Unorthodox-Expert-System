// Main function for an Unorthodox Expert System

#![allow(
dead_code,
//unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
)]

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
use state::SomeState;
mod statestore;
use statestore::StateStore;
mod domain;
pub use domain::SomeDomain;
mod needstore;
mod plan;
mod pn;
use pn::Pn;
mod actions;
mod domainstore;
mod step;
mod stepstore;
pub use domainstore::DomainStore;
mod actioninterface;
mod randompick;
mod removeunordered;
mod truth;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;

/// Initialize a Domain Store, with two domains and 11 actions.
fn init() -> DomainStore {

    // Load optimal regions
    let mut optimal = Vec::<RegionStore>::new();

    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
    regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
    optimal.push(regstr);

    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
    regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
    optimal.push(regstr);

    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
    regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
    optimal.push(regstr);

    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
    regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
    optimal.push(regstr);

    // Start a DomainStore
    let mut dmxs = DomainStore::new(optimal);

    // Initialize a domain, with number of integers = 1, initial state, optimal region.

    let num_ints = 1;
    let init_state = SomeState::new_random(num_ints);

    // Set up optimal regions.
    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(num_ints, "r0x0x").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "r0xx1").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "rx1x1").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "r1110").unwrap());
    // Intersections, 0x01, 01x1.
    // Intersections of intersections, 0101.

    // Create domain 0.
    let mut dom0 = SomeDomain::new(dmxs.len(), init_state, regstr);

    // Add actions 0 through 8;
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();
    dom0.add_action();

    // Add the domain to the DomainStore.
    dmxs.push(dom0);

    // Initialize a domain, with number of integers = 2, initial state, optimal region.

    let num_ints = 2;
    let init_state = SomeState::new_random(num_ints);

    // Set up optimal region.
    let mut regstr = RegionStore::with_capacity(1);
    regstr.push(SomeRegion::new_from_string(num_ints, "rXXXXXX10_1XXX_XXXX").unwrap());

    // Create domain 1.
    let mut dom1 = SomeDomain::new(dmxs.len(), init_state, regstr);

    // Add actions 0 through 4.
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();

    // Add the domain to the DomainStore.
    dmxs.push(dom1);

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
        if args[1] == "h" || args[1] == "help" {
            usage();
            return;
        }
        run_left = args[1].parse::<usize>().unwrap_or_else(|err| {
            println!("String to Number conversion error: {}", err);
            0
        });
        if run_left == 0 {
            usage();
            return;
        }
        run_to_end = true;
    }

    usage();

    let mut run_count = 1;
    let run_max = run_left;

    while run_left > 0 {
        //println!("run_left = {}", run_left);
        if 1 == do_session(run_to_end, run_count, run_max) {
            run_count = 1;
            run_left = run_max;
        } else {
            run_left -= 1;
            run_count += 1;
        }
    } // end while
} // end main

/// Do one session of finding and using rules.
pub fn do_session(run_to_end: bool, run_count: usize, run_max: usize) -> usize {
    let mut to_end = run_to_end;
    let mut dmxs = init();
    let mut dom_num = 0;
    let mut step_inc = 1; // amount to increment the step in the next loop

    loop {
        //println!("start session loop");
        dmxs.set_step(dmxs.step + step_inc);
        step_inc = 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();
        //println!("main {} needs {}", nds.len(), &nds);
        let mut need_plans = dmxs.evaluate_needs(&nds);

        // Boredom processing if no needs, or no needs can be done.
        if need_plans.len() == 0 {
            need_plans = dmxs.evaluate_needs(&nds);
        }

        // Check if all needs are for the same domain, change domain number if needed
        if nds.len() > 0 {
            let mut need_domain = nds[0].dom_num();
            let mut same_domain = true;

            if need_plans.len() > 0 {
                need_domain = nds[need_plans[0].inx].dom_num();
                for ndx in need_plans.iter() {
                    if nds[ndx.inx].dom_num() != need_domain {
                        same_domain = false;
                        break;
                    }
                }
            } else {
                for ndx in nds.iter() {
                    if ndx.dom_num() != need_domain {
                        same_domain = false;
                        break;
                    }
                }
            }

            if same_domain {
                if dom_num != need_domain {
                    //println!("changing domain from {} to {}", &dom_num, &need_domain);
                    dom_num = need_domain;
                }
            }
        } // endif nds.len() > 0

        // See if any need can be done, for print_domain call.
        let mut can_do_flag = false;
        for ndplnx in need_plans.iter() {
            if let Some(_) = ndplnx.pln {
                if nds[ndplnx.inx].type_string() != "ToRegion" {
                    can_do_flag = true;
                    break;
                }
            }
        }

        println!("\nAll domain states: {}", dmxs.all_current_states());

        print_domain(&dmxs, dom_num, can_do_flag);
        //println!("session loop 3");

        // Vector for position = display index, val = need_plans index
        let mut need_can = Vec::<usize>::with_capacity(nds.len());

        let mut can_do = 0;
        let mut cant_do = 0;

        if nds.len() > 0 {
            // Check if any needs (maybe a subset of the orginal needs have been checked) have a plan
            if need_plans.len() == 0 {
                cant_do = nds.len();

                println!("\nNeeds that cannot be done:");
                for ndx in nds.iter() {
                    println!("   {}", ndx);
                }

                println!("\nNeeds that can be done: None");
            } else {
                // Get count of needs that can, and cannot, be done.
                for ndplnx in need_plans.iter() {
                    if let Some(_) = ndplnx.pln {
                        can_do += 1;
                    } else {
                        cant_do += 1;
                    }
                }

                // Print needs that cannot be done.
                if cant_do == 0 {
                    println!("\nNeeds that cannot be done: None");
                } else {
                    println!("\nNeeds that cannot be done:");
                    for ndplnx in need_plans.iter() {
                        if let Some(_) = ndplnx.pln {
                        } else {
                            println!("   {}", nds[ndplnx.inx]);
                        }
                    }
                }

                // Print needs that can be done.

                if can_do == 0 {
                    println!("\nNeeds that can be done: None");
                } else {
                    println!("\nNeeds that can be done:");
                    let mut inx = 0;
                    let mut disp = 0;
                    for ndplnx in need_plans.iter() {
                        if let Some(plnx) = &ndplnx.pln {
                            if plnx.len() > 0 {
                                println!("{:2} {} {}", &disp, &nds[ndplnx.inx], &plnx.str_terse());
                            } else {
                                println!("{:2} {} {}", &disp, &nds[ndplnx.inx], &plnx);
                            }
                            need_can.push(inx);
                            disp += 1;
                        }
                        inx += 1;
                    } // next ndplnx
                }
            } // end  if need_plans.len() == 0 {} else

            // Stop running for this condition
            if cant_do > 0 && can_do == 0 {
                if run_count != run_max || run_max > 1 {
                    println!("\nrun_count {} of {}", run_count, run_max);
                }
                //return 0;
                to_end = false;
            }
        } else {
            if run_max == 1 {
                println!("\nAction needs: None");
            } else {
                println!(
                    "\nAction needs: None, run_count {} of {}",
                    run_count, run_max
                );
            }
            if to_end {
                if run_count < run_max {
                    return 0;
                }
                to_end = false;
            }
        }

        // Start command loop
        loop {
            //println!("start command loop");
            let mut cmd = Vec::<String>::with_capacity(10);

            if to_end == false || (cant_do > 0 && can_do == 0) {
                let guess = pause_for_input("\nPress Enter or type a command: ");

                for word in guess.split_whitespace() {
                    //println!("word: {} is {}", word_count, word);
                    cmd.push(String::from(word));
                }
            }

            // Default command, just press Enter
            if cmd.len() == 0 {
                // Process needs
                if can_do > 0 {
                    //println!("\nAction needs: {}", nds);

                    let np_inx = dmxs.choose_need(&nds, &need_plans, &need_can);

                    let nd_inx = need_plans[np_inx].inx;
                    let ndx = &nds[nd_inx];
                    dom_num = ndx.dom_num();

                    let pln = &need_plans[np_inx].pln.as_ref().unwrap();

                    //println!("need {}, plan {}", &ndx, &pln);

                    if pln.len() > 0 {
                        //println!("doing dmx.run_plan");
                        dmxs.run_plan(dom_num, &pln);
                    } else {
                        //println!("NOT doing dmx.run_plan");
                    }

                    if ndx.satisfied_by(&dmxs.cur_state(dom_num)) {
                        // println!("doing dmx.take_action_need");

                        match ndx {
                            SomeNeed::ToRegion { .. } => (),
                            _ => {
                                dmxs.take_action_need(dom_num, &ndx);
                            } // Add new needs here
                        }
                    } else {
                        // println!("NOT doing dmx.take_action_need");
                    }
                    break;
                } // end-if can_do > 0

                break;
            } // end if cmd.len() == 0

            if cmd.len() == 1 {
                // Quit with q , exit, quit
                if cmd[0] == "q" || cmd[0] == "exit" || cmd[0] == "quit" {
                    println!("Done");
                    process::exit(1);
                } else if cmd[0] == "run" {
                    to_end = true;
                    step_inc = 0;
                    continue;
                } else if cmd[0] == "so" {
                    if run_count != run_max {
                        return 0;
                    } else {
                        return 1;
                    }
                } else if cmd[0] == "dcs" {
                    step_inc = 0;
                    break;
                    //                } else if cmd[0] == "left" {
                    //                    println!("left-overs: {}", &dmxs[0].actions[0].left_overs());
                    //                    continue;
                }
            }

            if cmd.len() == 2 {
                if cmd[0] == "cd" {
                    step_inc = 0;

                    // Get domain number from string
                    match dmxs.domain_num_from_string(&cmd[1]) {
                        Ok(d_num) => {
                            dom_num = d_num;
                            break;
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match
                    continue;
                } else if cmd[0] == "ta" {
                    // Take an arbirary action with the current state

                    // Get act number from string
                    match dmxs[dom_num].act_num_from_string(&cmd[1]) {
                        Ok(a_num) => {
                            dmxs.take_action(dom_num, a_num);
                            break;
                        }
                        Err(error) => {
                            println!("\n{}", error);
                            step_inc = 0;
                        }
                    } // end match
                    continue;
                } else if cmd[0] == "ppd" {
                    step_inc = 0;
                    match cmd[1].parse::<usize>() {
                        Ok(n_num) => {
                            if n_num >= need_can.len() {
                                println!("Invalid Need Number: {}", cmd[1]);
                            } else {
                                let ndx = &nds[need_plans[need_can[n_num]].inx];

                                // Change the displayed Domain, if needed
                                if dom_num != ndx.dom_num() {
                                    dom_num = ndx.dom_num();
                                }

                                //print_domain(&dmxs, dom_num);

                                let pln = need_plans[need_can[n_num]].pln.as_ref().unwrap();

                                println!("\n{} Need: {}", &n_num, &ndx);

                                if ndx.satisfied_by(&dmxs[dom_num].get_current_state()) {
                                    println!("\nPlan: current state satisfies need, just take the action");
                                } else {
                                    println!("\nPlan: \n{}", &pln.str2());
                                }
                            }
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    }
                    continue;
                } else if cmd[0] == "dn" {
                    match cmd[1].parse::<usize>() {
                        Ok(n_num) => {
                            if n_num >= need_can.len() {
                                println!("Invalid Need Number: {}", cmd[1]);
                                step_inc = 0;
                            } else {
                                let ndx = &nds[need_plans[need_can[n_num]].inx];

                                let inxpln = &need_plans[need_can[n_num]];

                                let pln = inxpln.pln.as_ref().unwrap();

                                println!("\nNeed chosen: {} {} {}", &n_num, &ndx, &pln.str_terse());

                                dom_num = ndx.dom_num();

                                if pln.len() > 0 {
                                    dmxs.run_plan(dom_num, &pln);
                                }

                                if ndx.satisfied_by(&dmxs.cur_state(dom_num)) {
                                    dmxs.take_action_need(dom_num, &ndx);
                                }
                                break;
                            }
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    }
                    continue;
                } else if cmd[0] == "fld" {
                    match load_data(&cmd[1]) {
                        Err(why) => {
                            println!("couldn't read {}: {}", &cmd[1], why);
                        }
                        Ok(new_dmxs) => {
                            print!("Data loaded");
                            dmxs = new_dmxs;
                            break;
                        }
                    } // end match load_data
                    step_inc = 0;
                    continue;
                } else if cmd[0] == "fsd" {
                    match store_data(&dmxs, &cmd[1]) {
                        Err(why) => {
                            println!("couldn't write {}: {}", &cmd[1], why);
                        }
                        Ok(_) => {
                            print!("Data written");
                        }
                    }
                    step_inc = 0;
                    continue;
                } // end command sd
            } // end if cmd.len() == 2

            // Do other commands
            step_inc = do_command(&mut dmxs[dom_num], &cmd);
            if step_inc == 1 {
                break;
            }
        } // end command loop
          //println!("end command loop");
    } // end loop
} // end do_session

/// Do most commands entered by the user.
/// Return a zero or one, to indicate how the step number should change.
fn do_command(dmx: &mut SomeDomain, cmd: &Vec<String>) -> usize {
    let cur_state = dmx.get_current_state();

    // Handle one-word commands
    if cmd.len() == 1 {
        if cmd[0] == "h" || cmd[0] == "help" {
            usage();
            return 0;
        }
    } // end one-word commands

    // Handle two-word commands
    if cmd.len() == 2 {
        if cmd[0] == "oa" {
            match dmx.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    let val = dmx.add_optimal(goal_region.clone());
                    println!("Add Optimal region {} result {}", goal_region, val);
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                }
            } // end match
            return 0;
        } //end command oa

        if cmd[0] == "od" {
            match dmx.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    let val = dmx.delete_optimal(&goal_region);
                    println!("Delete Optimal region {} result {}", goal_region, val);
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                }
            } // end match
            return 0;
        } //end command od

        // Arbitrary change state
        if cmd[0] == "cs" {
            // Get state from string
            match dmx.state_from_string(&cmd[1]) {
                Ok(a_state) => {
                    println!("Changed state to {}", a_state);
                    dmx.set_state(&a_state);
                    return 1;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return 0;
                }
            } // end match
        } // end command cs

        if cmd[0] == "to" {
            let mut step_inc = 0;
            // Get region from string
            match dmx.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    println!(
                        "\nChange Current_state {} to region {}",
                        cur_state, goal_region
                    );
                    if goal_region.is_superset_of_state(&cur_state) {
                        println!(
                            "\nCurrent_state {} is already in region {}",
                            dmx.get_current_state(),
                            goal_region
                        );
                    } else {
                        if dmx.to_region(&goal_region) {
                            println!("\nChange to region succeeded");
                            step_inc = 1;
                        } else {
                            println!("\nChange to region failed");
                        }
                    }
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match region_r
            return step_inc;
        } //end command to

        if cmd[0] == "ss" {
            let mut step_inc = 0;
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!("Act {} sample State {}", act_num, cur_state);
                    dmx.take_action_need(&SomeNeed::StateNotInGroup {
                        dom_num: dmx.num,
                        act_num: act_num,
                        targ_state: dmx.get_current_state(),
                    });
                    step_inc = 1;
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            return step_inc;
        }

        if cmd[0] == "ps" {
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!(
                        "Squares of Action {} are:\n{}\n",
                        &act_num, &dmx.actions[act_num].squares
                    );
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            return 0;
        }
    } // end two-word commands

    // Handle three-word commands
    if cmd.len() == 3 {
        if cmd[0] == "ss" {
            let mut step_inc = 0;
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get state from string
                    match dmx.state_from_string(&cmd[2]) {
                        Ok(a_state) => {
                            println!("Act {} sample State {}", act_num, a_state);
                            dmx.set_state(&a_state);
                            dmx.take_action_need(&SomeNeed::StateNotInGroup {
                                dom_num: dmx.num,
                                act_num: act_num,
                                targ_state: a_state,
                            });
                            step_inc = 1;
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            return step_inc;
        }

        if cmd[0] == "ps" {
            // Get act_num
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region
                    match dmx.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            let mut psstr = String::from(format!(
                                "Squares of Action {} in region {} are:\n",
                                &act_num, &aregion
                            ));

                            let stas = dmx.actions[act_num].squares.stas_in_reg(&aregion);
                            if stas.len() == 0 {
                                println!("No squares in region {}", &aregion);
                                return 0;
                            }

                            let mut nl_flg = 0;
                            let mut max_pn = Pn::One;
                            let mut min_pn = Pn::Unpredictable;
                            let mut max_pn_reg: Option<SomeRegion> = None;

                            for stax in stas.iter() {
                                if nl_flg == 1 {
                                    psstr.push_str(",\n");
                                }

                                let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                                psstr.push_str(&format!("    {}", &sqrx));

                                if sqrx.pn < min_pn {
                                    min_pn = sqrx.pn;
                                }

                                if sqrx.pn > max_pn {
                                    max_pn = sqrx.pn;
                                    max_pn_reg = Some(SomeRegion::new(&sqrx.state, &sqrx.state));
                                } else if sqrx.pn == max_pn {
                                    if let Some(regx) = max_pn_reg {
                                        max_pn_reg = Some(regx.union_state(&sqrx.state));
                                    } else {
                                        max_pn_reg =
                                            Some(SomeRegion::new(&sqrx.state, &sqrx.state));
                                    }
                                }

                                nl_flg = 1;
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
                                } else {
                                    if let Some(ref regx) = max_pn_reg {
                                        if regx.is_superset_of_state(&sqrx.state) {
                                            non_pn_stas.push(sqrx.state.clone());
                                        }
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
                            } else {
                                if let Some(ruls) = rules {
                                    rules_str = ruls.formatted_string();
                                    for stax in non_pn_stas.iter() {
                                        let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                                        if sqrx.rules.is_subset_of(&ruls) == false {
                                            form_group = false;
                                        }
                                    }
                                } else {
                                    form_group = false;
                                }
                            }

                            psstr.push_str(&format!("\n    Min Pn: {} Max Pn: {} Max Pn Reg {} Rules: {} Can form group: {}",
                                &min_pn, &max_pn, &max_pn_reg.unwrap(), &rules_str, &form_group));
                            println!("{}", psstr);
                        }
                        Err(error) => {
                            println!("{}", &error);
                        }
                    } // end match region
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match act_num

            return 0;
        }

        if cmd[0] == "aj" {
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region
                    match dmx.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            if let Some(grpx) = dmx.actions[act_num].groups.find(&aregion) {
                                println!("\n  {}", &aregion);
                                let stas_adj =
                                    dmx.actions[act_num].squares.stas_adj_reg(&grpx.region);
                                for stax in stas_adj.iter() {
                                    let sqrx = dmx.actions[act_num].squares.find(stax).unwrap();
                                    println!("{}", sqrx);
                                }
                            } else {
                                println!("\nGroup with region {} not found", &aregion);
                            }
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match region
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match act num
            return 0;
        }

        if cmd[0] == "rps" {
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region from string
                    match dmx.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            let sta_1s = dmx.actions[act_num]
                                .squares
                                .states_in_1_region(&dmx.actions[act_num].groups.regions());

                            println!(
                                "Squares in one region, in {} are {}",
                                &aregion,
                                aregion.states_in(&sta_1s)
                            );
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match region
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match act num
            return 0;
        }

        if cmd[0] == "gps" {
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region from string
                    match dmx.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            // Find group
                            if let Some(grpx) = dmx.actions[act_num].groups.find(&aregion) {
                                if let Some(astate) = &grpx.anchor {
                                    if let Some(sqrx) = dmx.actions[act_num].squares.find(&astate) {
                                        println!("anchor   {}", &sqrx);
                                    } else {
                                        println!("anchor   {} not found?", &grpx.region.state1);
                                    }

                                    let bit_masks = grpx.region.x_mask().m_not().split();
                                    for mskx in bit_masks.iter() {
                                        let adj = SomeState::new(astate.bts.b_xor(&mskx.bts));
                                        if let Some(adjsqr) =
                                            dmx.actions[act_num].squares.find(&adj)
                                        {
                                            println!("adjacent {}", adjsqr);
                                        }
                                    }

                                    if grpx.region.state1 != grpx.region.state2 {
                                        let sta_far = grpx.region.far_state(&astate);
                                        if let Some(sqrx) =
                                            dmx.actions[act_num].squares.find(&sta_far)
                                        {
                                            println!("far      {}", &sqrx);
                                        } else {
                                            println!("far      {} not found?", &grpx.region.state2);
                                        }
                                    }
                                } else {
                                    if let Some(sqrx) =
                                        dmx.actions[act_num].squares.find(&grpx.region.state1)
                                    {
                                        println!("state1   {}", &sqrx);
                                    } else {
                                        println!("state1   {} not found?", &grpx.region.state1);
                                    }

                                    if grpx.region.state1 != grpx.region.state2 {
                                        if let Some(sqrx) =
                                            dmx.actions[act_num].squares.find(&grpx.region.state2)
                                        {
                                            println!("state2   {}", &sqrx);
                                        } else {
                                            println!("state2   {} not found?", &grpx.region.state2);
                                        }
                                    }
                                }
                            } else {
                                println!("Group {} not found!", &aregion);
                            }
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match region
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match act num
            return 0;
        }
    } // end 3-word commands

    // Handle four-word commands
    if cmd.len() == 4 {
        let mut step_inc = 0;
        // Take Sample (ts) with <action num> <initial-state> <result-state>, don't update current state
        if cmd[0] == "ss" {
            // Get act number from string
            match dmx.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get i-state from string
                    match dmx.state_from_string(&cmd[2]) {
                        Ok(i_state) => {
                            // Get r-state from string
                            match dmx.state_from_string(&cmd[3]) {
                                Ok(r_state) => {
                                    println!(
                                        "Act {} take sample {} -> {}",
                                        act_num, &i_state, &r_state
                                    );

                                    dmx.eval_sample_arbitrary(act_num, &i_state, &r_state);
                                    step_inc = 1;
                                }
                                Err(error) => {
                                    println!("\n{}", error);
                                }
                            } // end match r_state
                        }
                        Err(error) => {
                            println!("\n{}", error);
                        }
                    } // end match i_state
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            return step_inc;
        } // end command ss
    } // end 4-word commands

    println!("\nDid not understand command: {:?}", cmd);
    0
} // end do_command

/// Print a domain.
fn print_domain(dmxs: &DomainStore, dom_num: usize, can_do_flag: bool) {
    if dmxs[dom_num].boredom > 0 && can_do_flag == false {
        print!("\nCurrent Domain: {} of {}", dom_num, dmxs.num_domains(),);
    } else {
        print!("\nCurrent Domain: {} of {}", dom_num, dmxs.num_domains());
    }
    println!("\nActs: {}", &dmxs[dom_num].actions);

    let cur_state = &dmxs[dom_num].get_current_state();

    if dmxs[dom_num].optimal.len() > 0 && can_do_flag == false {
        let mut optstr = dmxs[dom_num].optimal.formatted_string();

        let opt_regs = dmxs[dom_num].optimal.supersets_of_state(&cur_state);

        if opt_regs.len() > 0 {
            optstr = opt_regs.formatted_string();
            if opt_regs.len() != dmxs[dom_num].optimal.len() {
                let notin = dmxs[dom_num]
                    .optimal
                    .not_supersets_of_state(&dmxs[dom_num].get_current_state());
                if dmxs[dom_num].boredom > dmxs[dom_num].boredom_limit() {
                    println!(
                        "\nStep: {} Dom: {} Current State: {} in Optimal Regions: {} not in {}  Bored! (or satiated)",
                        &dmxs.step, dom_num, &cur_state, optstr, &notin
                    );
                } else {
                    println!(
                        "\nStep: {} Dom: {} Current State: {} in Optimal Regions: {} not in {}  Boredom duration {} limit {}",
                        &dmxs.step, dom_num, &cur_state, optstr, &notin, dmxs[dom_num].boredom, dmxs[dom_num].boredom_limit()
                    );
                }
            } else {
                println!(
                    "\nStep: {} Dom: {} Current State: {} in Optimal Regions: {}",
                    &dmxs.step, dom_num, &cur_state, optstr
                );
            }
        } else {
            println!(
                "\nStep: {} Dom: {} Current State: {} Not in Optimal Regions: {}",
                &dmxs.step, dom_num, &cur_state, optstr
            );
        }
    } else {
        println!(
            "\nStep: {} Dom: {} Current State: {}",
            &dmxs.step, dom_num, &cur_state
        );
    }

    assert!(dmxs.step < 500); // Remove for continuous use
}

/// Display usage options.
fn usage() {
    println!("\nStartup Commands: <invoke> may be the command \"ues\" or \"cargo run\"");
    println!("\n    <invoke>                 - Run interactively, press enter for each step.");
    println!("    <invoke> <number times>  - Run a number of times, stop at last run, or when no needs can be done and one, or more, cannot.");
    println!("    <invoke> [h | help]      - Show this list.\n");

    println!("\nSession Commands:");
    println!("\n    h | help                 - Help list display (this list).");
    println!(
        "\n    cd <dom num>             - Change the Curently Displayed Domain (CDD) to the given domain number."
    );
    println!("\n    Press Enter (no command) - Satisfy one need that can be done, if any.");
    println!("\n    q | exit | quit          - Quit the program.");
    println!("\n\n    aj <act num> <region>    - For an Action in the CDD, print adJacent squares to the groups anchor");

    println!("\n    cs <state>               - Change State, an arbitrary change, for the CDD.");
    println!("\n    dn <need number>         - Do a particular Need from the can-do need list.");
    println!("\n    dcs                      - Display Current State, and domain.  After a number of commands,");
    println!("                               the current state scrolls off screen, this might be useful.");
    println!("\n    fld <path>               - File Load Data.");
    println!("    fsd <path>               - File Store Data.");
    println!(
        "\n    gps <act num> <region>   - Group Print Squares that define the group region, of a given action, of the CDD."
    );
    println!("\n    oa <region>              - Optimal regions Add the given region, of the CDD.");
    println!("                             - This will fail if the region is a subset of one of the displayed regions.");
    println!(
        "\n    od <region>              - Optimal regions Delete the given region, of the CDD."
    );
    println!("                             - This will fail if the region is not found or is a displayed intersection.");
    println!("\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list.");
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!(
        "    ps <act num> <region>    - Print Squares in a given action and region, of the CDD."
    );
    println!(
        "\n    rps <act num> <region>    - Region, Print Squares that are in the given action and region of the CDD."
    );
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!("\n    so                       - Start Over.");
    println!("\n    ss <act num>                        - Sample the current State, for a given action, for the CDD.");
    println!("    ss <act num> <state>                - Sample State for a given action and state, for the CDD.");
    println!(
        "    ss <act num> <state> <result-state> - Sample State, for a given action, state and arbitrary result, for the CDD."
    );
    println!("\n    ta <act-num>             - Take Action with the current state.");
    println!("\n    to <region>              - Change the current state TO within a region, by calculating and executing a plan.");
    println!("\n    A domain number is an integer, zero or greater, where such a domain exists. CDD means the Currently Displayed Domain.");
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("\n    A need number is an integer, zero or greater, where such a need exists.");
    println!("\n    A state starts with an 's' character, followed by zero, or more, zero and one characters.");
    println!("\n    A state can be specified in hexadecimal, like s0xa5.");
    println!("\n    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. Leading zeros can be omitted.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable.");
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("\n    If there is an optimal region for the CDD, when no more needs can be done, the program will seek to change the current state");
    println!("    to be in an optimal region.");
    println!("\n    If there is another optimal region the current state is not in, after a (3 * number-regions-in) steps, the program will get bored");
    println!("    and seek to move the current state to a different optimal region, or to an intersection of optimal regions.");
    println!("\n    \"P[]\" means Plan: No extra actions need to be run, using the current state, run the need action to satisfy the need.");
    println!("    \"P[1,2]\" means Plan: Run action 1, then action 2, to change the current state, then run the need action to satisfy the need.");
    println!("    You start to see these after step 50-60, using previously generated rules, to limit or extend rules, or test contradictory intersections.");
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
    print!("{}", prompt);
    io::stdout().flush().unwrap();

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
    match File::open(&path) {
        Err(why) => {
            return Err(format!("couldn't open {}: {}", display, why));
        }
        Ok(mut afile) => {
            let mut serialized = String::new();
            match afile.read_to_string(&mut serialized) {
                Err(why) => {
                    return Err(format!("couldn't read {}: {}", display, why));
                }
                Ok(_) => {
                    let deserialized_r = serde_yaml::from_str(&serialized);
                    match deserialized_r {
                        Err(why) => {
                            return Err(format!("couldn't deserialize {}: {}", display, why));
                        }
                        Ok(new_dmxs) => {
                            return Ok(new_dmxs);
                        }
                    } // end match deserialized_r
                }
            }
        }
    } // end match open file
}

/// Store current data to a given path string.
fn store_data(dmxs: &DomainStore, path_str: &str) -> Result<bool, String> {
    let serialized_r = serde_yaml::to_string(&dmxs);
    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(&path_str);
            let display = path.display();

            // Open a file in write-only mode, returns `io::Result<File>`
            match File::create(&path) {
                Err(why) => Err(format!("couldn't create {}: {}", display, why)),
                Ok(mut file) => match file.write_all(serialized.as_bytes()) {
                    Err(why) => Err(format!("couldn't write to {}: {}", display, why)),
                    Ok(_) => Ok(true),
                },
            }
        }
        Err(error) => Err(format!("{}", error)),
    } // end match serialized_r
} // end store_data
