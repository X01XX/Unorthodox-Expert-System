// Main function for an Unorthodox Expert System

#![allow(
dead_code,
//  unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
)]

use std::env;
mod action;
//use crate::action::SomeAction;
mod actionstore;
mod bits;
//use bits::SomeBits;
mod group;
mod groupstore;
mod mask;
//use mask::SomeMask;
mod maskstore;
//use maskstore::MaskStore;
mod need;
mod region;
use region::SomeRegion;
mod change;
//use change::SomeChange;
mod regionstore;
use regionstore::RegionStore;
mod resultstore;
mod rule;
//use rule::SomeRule;
mod rulestore;
use crate::rulestore::RuleStore;
mod compare;
mod square;
mod squarestore;
mod state;
//use state::SomeState;
mod statestore;
use crate::statestore::StateStore;
use need::SomeNeed;
mod domain;
mod needstore;
//use needstore::NeedStore;
mod plan;
//use crate::plan::SomePlan;
mod pn;
use crate::pn::Pn;
mod step;
mod stepstore;
use domain::SomeDomain;
mod actions;
mod domainstore;
use domainstore::DomainStore;
mod combine;
mod truth;
//use crate::truth::Truth;
mod randompick;
mod removeunordered;
mod actioninterface;

use std::io;
use std::io::{Read, Write};
//use std::process;
extern crate rand;
use rand::Rng;
use std::fs::File;
use std::path::Path;
//use std::cmp::Ordering;

/// Initialize a Domain Store, with two domains and 11 actions.
fn init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    // Initialize a domain, with number of integers, initial state, optimal region.

    // Generate a one u8 integer random starting state string.
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(6));
    let inx_str = &format!("s{:b}", inx);

    let num_ints = 1;
    let mut regstr = RegionStore::with_capacity(2);
    regstr.push(SomeRegion::new_from_string(num_ints, "r0x0x").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "r0xx1").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "rx1x1").unwrap());
    regstr.push(SomeRegion::new_from_string(num_ints, "r1110").unwrap());
    // Intersections, 0x01, 01x1.
    // Intersections of intersections, 0101.
    let mut dom1 = SomeDomain::new(dmxs.len(), num_ints, inx_str, regstr);

    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();
    dom1.add_action();

    dmxs.push(dom1);

    // Generate a two u8 integer random starting state string,
    // spanning the two integers.
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(4));
    let inx_str = &format!("s{:b}000000", inx);

    let num_ints = 2;
    let mut regstr = RegionStore::with_capacity(1);
    regstr.push(SomeRegion::new_from_string(num_ints, "r10_1X00_0000").unwrap());
    let mut dom2 = SomeDomain::new(dmxs.len(), num_ints, inx_str, regstr);

    dom2.add_action();
    dom2.add_action();
    dom2.add_action();
    dom2.add_action();
    dom2.add_action();

    dmxs.push(dom2);

    dmxs
}

/// The User Interface.
fn main() {

//    let stint1 = StateInterface::new(2);

//    if 1 == 1 {
//        assert!(1 == 2);
//    }

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
        println!("String to Number conversion error: {}", err); 0 });
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
        run_left += do_session(run_to_end, run_count, run_max);

        run_left -= 1;
        run_count += 1;

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
        //println!("session loop 1");
        let mut need_plans = dmxs.evaluate_needs(&nds);

        // Boredom processing if no needs, or no needs can be done.
        if need_plans.len() == 0 {
            need_plans = dmxs.evaluate_needs(&nds);
        }
        //println!("session loop 2");

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

        print_domain(&dmxs, dom_num);
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
                    //let domx = ndx.dom_num();
                    //let req_chg = SomeChange::region_to_region(&SomeRegion::new(&dmxs[domx].cur_state, &dmxs[domx].cur_state), &ndx.target());
                    
                    //let stps = dmxs[domx].actions.get_steps(&req_chg);
                    //println!("steps returned: {}", &stps);
                    //let mut avail_chgs = SomeChange::new_low(dmxs[domx].num_ints);
                    //if stps.len() > 0 {
                    //    avail_chgs = stps.aggregate_changes();
                    //}
                    //let lack_chg = req_chg.c_and(&avail_chgs.c_not());
                    //print!("\n    changes needed {} \n          avail {} \n        not found: {}", req_chg, avail_chgs, lack_chg);
                    //println!(" ");
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
                            //let domx = nds[ndplnx.inx].dom_num();
                            //let req_chg = SomeChange::region_to_region(&SomeRegion::new(&dmxs[domx].cur_state, &dmxs[domx].cur_state), &nds[ndplnx.inx].target());
                    
                            //let stps = dmxs[domx].actions.get_steps(&req_chg);
                            //println!("steps returned: {}", &stps);
                            //let mut avail_chgs = SomeChange::new_low(dmxs[domx].num_ints);
                            //if stps.len() > 0 {
                            //    avail_chgs = stps.aggregate_changes();
                            //}
                            //let lack_chg = req_chg.c_and(&avail_chgs.c_not());
                            //print!("\n    changes needed {} \n             avail {} \n        not found: {}", req_chg, avail_chgs, lack_chg);
                            //println!(" ");
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
                if run_count != run_max {
                    println!("\nrun_count {} of {}", run_count, run_max);
                }
                //return 0;
                to_end = false;
            }
        } else {
            if run_max == 1 {
                println!("\nAction needs: None");
            } else {
                println!("\nAction needs: None, run_count {} of {}", run_count, run_max);
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
                            SomeNeed::ToRegion {
                                ..
                                } => {},
                            _ => {
                                dmxs.take_action_need(dom_num, &ndx);
                                },
                                // Add new needs here
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
                    return 0;
                } else if cmd[0] == "run" {
                    to_end = true;
                    step_inc = 0;
                    continue;
                } else if cmd[0] == "so" {
                    return 1;
                } else if cmd[0] == "dcs" {
                    step_inc = 0;
                    break;
                } else if cmd[0] == "left" {
                    println!("left-overs: {}", &dmxs[0].actions[0].left_overs());
                    continue;
                } else if cmd[0] == "vert" {
                    dmxs[0].actions[0].vertices();
                    continue;
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

                } else if cmd[0] == "ta" {  // Take an arbirary action with the current state
                    
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
                                    break;
                                }
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
fn do_command(dm1: &mut SomeDomain, cmd: &Vec<String>) -> usize {

    let cur_state = dm1.get_current_state();

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
            match dm1.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    let val = dm1.add_optimal(goal_region.clone());
                    println!("Add Optimal region {} result {}", goal_region, val);
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                }
            } // end match
            return 0
        } //end command oa

        if cmd[0] == "od" {
            match dm1.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    let val = dm1.delete_optimal(&goal_region);
                    println!("Delete Optimal region {} result {}", goal_region, val);
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                }
            } // end match
            return 0
        } //end command od

        // Arbitrary change state
        if cmd[0] == "cs" {
            // Get state from string
            match dm1.state_from_string(&cmd[1]) {
                Ok(a_state) => {
                    println!("Changed state to {}", a_state);
                    dm1.set_state(&a_state);
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
            match dm1.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    println!(
                        "\nChange Current_state {} to region {}",
                        cur_state, goal_region
                    );
                    if goal_region.is_superset_of_state(&cur_state) {
                        println!(
                            "\nCurrent_state {} is already in region {}",
                            dm1.get_current_state(), goal_region
                        );
                    } else {
                        if dm1.to_region(&goal_region) {
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

        if cmd[0] == "ibn" {
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    let ndx = &dm1.actions[act_num].seek_edge_needs1();

                    if ndx.len() > 0 {
                        println!("Seek Edge Needs1 are {}", &ndx);
                    } else {

                        let ndx = &dm1.actions[act_num].seek_edge_needs2();
                    
                        if ndx.len() > 0 {
                            println!("Seek Edge Needs2 are {}", &ndx);
                        } else {
                            println!("No needs found");
                        }
                    }
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            return 0;
        }

        if cmd[0] == "ss" {
            let mut step_inc = 0;
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!("Act {} sample State {}", act_num, cur_state);
                    dm1.take_action_need(&SomeNeed::StateNotInGroup {
                        dom_num: dm1.num,
                        act_num: act_num,
                        targ_state: dm1.get_current_state(),
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!(
                        "Squares of Action {} are:\n{}\n",
                        &act_num, &dm1.actions[act_num].squares
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get state from string
                    match dm1.state_from_string(&cmd[2]) {
                        Ok(a_state) => {
                            println!("Act {} sample State {}", act_num, a_state);
                            dm1.set_state(&a_state);
                            dm1.take_action_need(&SomeNeed::StateNotInGroup {
                                dom_num: dm1.num,
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {

                    // Get region
                    match dm1.region_from_string(&cmd[2]) {
                
                        Ok(aregion) => {
                            let mut psstr = String::from(format!(
                                "Squares of Action {} in region {} are:\n",
                                &act_num, &aregion
                            ));

                            let stas = dm1.actions[act_num].squares.stas_in_reg(&aregion);
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

                                let sqrx = dm1.actions[act_num].squares.find(stax).unwrap();
                                psstr.push_str(&format!("    {}", &sqrx));

                                if sqrx.results.pn < min_pn {
                                    min_pn = sqrx.results.pn;
                                }

                                if sqrx.results.pn > max_pn {
                                    max_pn = sqrx.results.pn;
                                    max_pn_reg = Some(SomeRegion::new(&sqrx.state, &sqrx.state));
                                } else if sqrx.results.pn == max_pn {
                                        if let Some(regx) = max_pn_reg {
                                            max_pn_reg = Some(regx.union_state(&sqrx.state));
                                        } else {
                                            max_pn_reg = Some(SomeRegion::new(&sqrx.state, &sqrx.state));
                                        }
                                }

                                nl_flg = 1;
                            }

                            // Get rule union, if any
                            let mut rules: Option<RuleStore> = None;
                            let mut non_pn_stas = StateStore::new();
                            for stax in stas.iter() {
                                let sqrx = dm1.actions[act_num].squares.find(stax).unwrap();
                                if sqrx.results.pn == max_pn {
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
                            if max_pn == Pn::Unpredictable{
                                for stax in non_pn_stas.iter() {
                                    let sqrx = dm1.actions[act_num].squares.find(stax).unwrap();
                                    if sqrx.results.pnc {
                                        form_group = false;
                                    }
                                }
                            } else {
                                if let Some(ruls) = rules {
                                    rules_str = ruls.formatted_string();
                                    for stax in non_pn_stas.iter() {
                                        let sqrx = dm1.actions[act_num].squares.find(stax).unwrap();
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region
                    match dm1.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            if let Some(grpx) = dm1.actions[act_num].groups.find(&aregion) {
                                if let Some(anchor) = &grpx.anchor {
                                    let sqrx = dm1.actions[act_num].squares.find(anchor).unwrap();
                                    println!("\nGroup:      {}", &aregion);
                                    println!("Anchor:   {}", sqrx.formatted_string2());
                                    let stas = dm1.actions[act_num].squares.stas_adj_reg(&SomeRegion::new(&anchor, &anchor));
                                    for stax in stas.iter() {
                                        if aregion.is_superset_of_state(stax) {
                                        } else {
                                            println!("Adjacent: {}", &dm1.actions[act_num].squares.find(stax).unwrap().formatted_string2());
                                        }
                                    }
                                } else {
                                    println!("\nGroup {} is not limited by a square only in one region", &aregion);
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region from string
                    match dm1.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            let sta_1s = dm1.actions[act_num]
                                .squares
                                .states_in_1_region(&dm1.actions[act_num].groups.regions());

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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region from string
                    match dm1.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            // Find group
                            if let Some(grpx) = dm1.actions[act_num].groups.find(&aregion) {
                                if let Some(sqrx) = dm1.actions[act_num].squares.find(&grpx.region.state1) {
                                    println!("{}", &sqrx);
                                } else {
                                    println!("{} samples needed.", &grpx.region.state1);
                                }

                                if grpx.region.state1 != grpx.region.state2 {
                                    if let Some(sqrx) = dm1.actions[act_num].squares.find(&grpx.region.state2) {
                                        println!("{}", &sqrx);
                                    } else {
                                        println!("{} samples needed.", &grpx.region.state2);
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
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get i-state from string
                    match dm1.state_from_string(&cmd[2]) {
                        Ok(i_state) => {
                            // Get r-state from string
                            match dm1.state_from_string(&cmd[3]) {
                                Ok(r_state) => {
                                    println!("Act {} take sample {} -> {}", act_num, &i_state, &r_state);

                                    dm1.eval_sample_arbitrary(act_num, &i_state, &r_state);
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
fn print_domain(dmxs: &DomainStore, dom_num: usize) {
    if dmxs[dom_num].boredom > 0 {
        print!("\nCurrent Domain: {} of {} Boredom level {}", dom_num, dmxs.num_domains(), dmxs[dom_num].boredom);
    } else {
        print!("\nCurrent Domain: {} of {}", dom_num, dmxs.num_domains());
    }
    println!("\nActs: {}", &dmxs[dom_num].actions);

    let cur_state = &dmxs[dom_num].get_current_state();

    if dmxs[dom_num].optimal_and_ints.len() > 0 {

        let mut optstr = dmxs[dom_num].optimal_and_ints.formatted_string();

        let opt_regs = dmxs[dom_num].optimal_and_ints.supersets_of_state(&cur_state);

        if opt_regs.len() > 0 {
            optstr = opt_regs.formatted_string();
            if opt_regs.len() != dmxs[dom_num].optimal_and_ints.len() {
                let notin = dmxs[dom_num].optimal_and_ints.not_supersets_of_state(&dmxs[dom_num].get_current_state());
                println!("\nStep: {} Dom: {} Current State: {} in Optimal Regions: {} not in {}", &dmxs.step, dom_num, &cur_state, optstr, &notin);
            } else {
                println!("\nStep: {} Dom: {} Current State: {} in Optimal Regions: {}", &dmxs.step, dom_num, &cur_state, optstr);
            }
        } else {
                println!("\nStep: {} Dom: {} Current State: {} Not in Optimal Regions: {}", &dmxs.step, dom_num, &cur_state, optstr);
        }
    } else {
        println!("\nStep: {} Dom: {} Current State: {}", &dmxs.step, dom_num, &cur_state);
    }

    if dmxs.step > 500 {
        assert!(1 == 2);
    }
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
        "\n    cd <dom num>             - Change the curently displayed Domain (CDD) to the given domain number."
    );
    println!(
        "\n    Press Enter (no command) - Satisfy one need that can be done, if any."
    );
    println!("\n    aj <act num> <region>    - For an Action in the CDD, and a limited group, print adJacent squares to the groups anchor");


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
    println!("\n    od <region>              - Optimal regions Delete the given region, of the CDD.");
    println!("                             - This will fail if the region is not found or is a displayed intersection.");
    println!("\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list.");
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!("    ps <act num> <region>    - Print Squares in a given action and region, of the CDD.");
    
    println!("\n    q | exit | quit          - Quit program.");
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
}

///Pause for input from user.
pub fn pause_for_input(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut in_str = String::new();
    io::stdin()
        .read_line(&mut in_str)
        .expect("Failed to read line");

    in_str
}

/// Load data from a given path string.
fn load_data(path_str: &String) -> Result<DomainStore, String> {
    let path = Path::new(path_str);
    let display = path.display();

    // Open a file, returns `io::Result<File>`
    match File::open(&path) {
        Err(why) => {
            return Err(format!("couldn't read {}: {}", display, why));
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
fn store_data(dmxs: &DomainStore, path_str: &String) -> Result<bool, String> {
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
