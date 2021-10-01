// Main function for an Unorthodox Expert System

#![allow(
// dead_code,
//  unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
)]

use std::env;
mod action;
use crate::action::SomeAction;
mod actionstore;
mod bits;
mod group;
mod groupstore;
mod mask;
mod maskstore;
mod need;
mod region;
use region::SomeRegion;
mod change;
mod regionstore;
mod resultstore;
mod rule;
mod rulestore;
mod square;
mod squarestore;
mod state;
mod statestore;
use need::SomeNeed;
mod domain;
mod needstore;
mod plan;
//use crate::plan::SomePlan;
mod pn;
mod step;
mod stepstore;
use domain::SomeDomain;
mod actions;
mod domainstore;
mod tests;
use domainstore::DomainStore;
mod inxplan;
//use crate::inxplan::InxPlan;
mod truth;
use std::io;
use std::io::{Read, Write};
//use std::process;
extern crate rand;
use rand::Rng;
use std::fs::File;
use std::path::Path;

/// Initialize a Domain Store, with two domains and 11 actions.
fn init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    // Initialize a domain, with number of integers, initial state, optimal region.

    // Generate a one u8 integer random starting state string.
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(6));
    let inx_str = &format!("s{:b}", inx);

    let num_ints = 1;
    let mut dom1 = SomeDomain::new(num_ints, inx_str, Some(SomeRegion::from_string(num_ints, "r101X").unwrap()));

    dom1.push(SomeAction::new(num_ints), 6);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);
    dom1.push(SomeAction::new(num_ints), 0);

    dmxs.push(dom1);

    // Generate a two u8 integer random starting state string,
    // spanning the two integers.
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(4));
    let inx_str = &format!("s{:b}000000", inx);

    let num_ints = 2;
    let mut dom2 = SomeDomain::new(num_ints, inx_str, Some(SomeRegion::from_string(num_ints, "r10_1X00_0000").unwrap()));

    dom2.push(SomeAction::new(num_ints), 0);
    dom2.push(SomeAction::new(num_ints), 0);
    dom2.push(SomeAction::new(num_ints), 0);
    dom2.push(SomeAction::new(num_ints), 0);
    dom2.push(SomeAction::new(num_ints), 0);

    dmxs.push(dom2);

    dmxs
}

/// User Interface
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
        println!("String to Number conversion error: {}", err); 0 });
        if run_left == 0 {
            usage();
            return;
        }
        run_to_end = true;
    }

    usage();
    
    //let mut run_left = 1;
    let mut run_count = 1;
    let run_max = run_left;

    while run_left > 0 {
        do_session(run_to_end, run_count, run_max);

        run_left -= 1;
        run_count += 1;

    } // end while
} // end main

/// DO one session of finding and using rules
pub fn do_session(run_to_end: bool, run_count: usize, run_max: usize) {

    let mut to_end = run_to_end;
    let mut dmxs = init();
    let mut dom_num = 0;
    let mut step_inc = 1; // amount to increment the step in the next loop
    
    loop {
        dmxs.set_step(dmxs.get_step() + step_inc);
        step_inc = 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();

        print_domain(&dmxs, dom_num);

        // Vector for position = display index, val = need_plans index
        let mut need_can = Vec::<usize>::with_capacity(nds.len());

        //let mut need_plans = Vec::<InxPlan>::with_capacity(1);

        let mut can_do = 0;
        let mut cant_do = 0;

       //if nds.len() > 0 {
            // Check if each need can be done
        let need_plans = dmxs.evaluate_needs(&nds);

        if need_plans.len() > 0 {
            // Get count of needs that can, and cannot, be done.

            for ndplnx in need_plans.iter() {
                if let Some(_) = ndplnx.pln {
                    can_do += 1;
                } else {
                    cant_do += 1;
                }
            }

            println!(" ");
            
            // Print needs that cannot be done.
            if cant_do == 0 {
                println!("Needs that cannot be done: None");
            } else {
                println!("Needs that cannot be done:");
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

            // Stop running for this condition
            if cant_do > 1 && can_do == 0 {
               to_end = false;
            }
            
        } else {
            println!("\nAction needs: None, run_count {} of {}", run_count, run_max);
            if to_end {
                if run_count < run_max {
                    return;
                }
                to_end = false;
            }
        }

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
                            dom_num: _,
                            act_num: _,
                            goal_reg: _,
                            } => {},
                        _ => {
                            dmxs.take_action_need(dom_num, &ndx);
                            },
                            // Add new needs here
                        }
                } else {
                    // println!("NOT doing dmx.take_action_need");
                }
                continue;
            }

//            if can_do == 0 {
//                if cant_do > 0 {
//                }
//            }
            continue;
        } // end if cmd.len() == 0

        if cmd.len() == 1 {
            // Quit with q , exit, quit
            if cmd[0] == "q" || cmd[0] == "exit" || cmd[0] == "quit" {
                println!("Done");
                return;
            } else if cmd[0] == "run" {
                to_end = true;
                step_inc = 0;
                continue;
            } else if cmd[0] == "so" {
                dmxs = init();
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
                    }
                    Err(error) => {
                        println!("\n{}", error);
                        pause_for_input("\nPress Enter to continue: ");
                    }
                } // end match
                continue;

            } else if cmd[0] == "ta" {  // Take an arbirary action with the current state
                
                // Get act number from string
                match dmxs[dom_num].act_num_from_string(&cmd[1]) {
                    Ok(a_num) => {
                        dmxs.take_action(dom_num, a_num); 
                        pause_for_input("\nPress Enter to continue: ");
                    }
                    Err(error) => {
                        println!("\n{}", error);
                        pause_for_input("\nPress Enter to continue: ");
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
                        
                            let ndx = &nds[need_can[n_num]];

                            // Change the displayed Domain, if needed
                            if dom_num != ndx.dom_num() {
                                dom_num = ndx.dom_num();
                            }

                            print_domain(&dmxs, dom_num);

                            let pln = need_plans[need_can[n_num]].pln.as_ref().unwrap();

                            println!("\nNeed: {}", &ndx);

                            println!("\nPlan: \n{}", &pln.str2());
                        }

                    }
                    Err(error) => {
                        println!("\n{}", error);
                    }
                }
                pause_for_input("\nPress Enter to continue: ");
                continue;
            } else if cmd[0] == "dn" {
                
                match cmd[1].parse::<usize>() {
                    Ok(n_num) => {
                        
                        if n_num >= need_can.len() {
                            println!("Invalid Need Number: {}", cmd[1]);
                            step_inc = 0;
                        } else {

                            let ndx = &nds[need_can[n_num]];

                            let inxpln = &need_plans[need_can[n_num]];
                            
                            let pln = inxpln.pln.as_ref().unwrap();

                            println!("\nNeed chosen: {} {} {}\n", &n_num, &ndx, &pln.str_terse());

                            dom_num = ndx.dom_num();

                            //println!("need {}, plan {}", &ndx, &pln);

                            if pln.len() > 0 {
                                //println!("doing dmx.run_plan");
                                dmxs.run_plan(dom_num, &pln);
                            }

                            if ndx.satisfied_by(&dmxs.cur_state(dom_num)) {
                                // println!("doing dmx.take_action_need");
                                dmxs.take_action_need(dom_num, &ndx);
                            } else {
                                // println!("NOT doing dmx.take_action_need");
                            }
                        }
                    }
                    Err(error) => {
                        println!("\n{}", error);
                    }
                }
                pause_for_input("\nPress Enter to continue: ");
                continue;
            } else if cmd[0] == "ld" {
                match load_data(&cmd[1]) {
                    Err(why) => {
                        println!("couldn't read {}: {}", &cmd[1], why);
                    }
                    Ok(new_dmxs) => {
                        print!("Data loaded");
                        dmxs = new_dmxs;
                    }
                } // end match load_data
                pause_for_input("\nPress Enter to continue: ");
                step_inc = 0;
                continue;
            } else if cmd[0] == "sd" {
                match store_data(&dmxs, &cmd[1]) {
                    Err(why) => {
                        println!("couldn't write {}: {}", &cmd[1], why);
                    }
                Ok(_) => {
                        print!("Data written");
                    }
                }
                pause_for_input("\nPress Enter to continue: ");
                step_inc = 0;
                continue;
            } // end command sd
        } // end if cmd.len() == 2

        // Do other commands
        step_inc = do_command(&mut dmxs[dom_num], &cmd);

    } // end loop
} // end do_session


/// Do most commands entered by the user.
/// Return a zero or one, to indicate how the step number should change
fn do_command(dm1: &mut SomeDomain, cmd: &Vec<String>) -> usize {

    // Handle one-word commands
    if cmd.len() == 1 {

        if cmd[0] == "h" || cmd[0] == "help" {
            usage();
            pause_for_input("\nPress Enter to continue: ");
            return 0;
        }

        if cmd[0] == "co" {
            if let Some(areg) = &dm1.get_optimal() {
                println!("Change Optimal region from {} to None", areg);
            } else {
                println!("Change Optimal region from None to None");
            }
            dm1.set_optimal(None);
            pause_for_input("\nPress Enter to continue: ");
            return 0;
        } //end command co
    } // end one-word commands

    // Handle two-word commands
    if cmd.len() == 2 {
        if cmd[0] == "co" {
            match dm1.region_from_string(&cmd[1]) {
                Ok(goal_region) => {
                    if let Some(areg) = &dm1.get_optimal() {
                        println!("Change Optimal region from {} to {}", areg, goal_region);
                    } else {
                        println!("Change Optimal region from None to {}", goal_region);
                    }
                    dm1.set_optimal(Some(goal_region));
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                }
            } // end match
            pause_for_input("\nPress Enter to continue: ");
            return 0
        } //end command co

        // Arbitrary change state
        if cmd[0] == "cs" {
            // Get state from string
            match dm1.state_from_string(&cmd[1]) {
                Ok(a_state) => {
                    println!("Changed state to {}", a_state);
                    dm1.set_cur_state(&a_state);
                    pause_for_input("\nPress Enter to continue: ");
                    return 1;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    pause_for_input("\nPress Enter to continue: ");
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
                        dm1.get_cur_state(), goal_region
                    );
                    if goal_region.is_superset_of_state(&dm1.get_cur_state()) {
                        println!(
                            "\nCurrent_state {} is already in region {}",
                            dm1.get_cur_state(), goal_region
                        );
                    } else {
                        step_inc = 1;
                        if dm1.to_region(&goal_region) {
                            println!("\nChange to region succeeded");
                        } else {
                            println!("\nChange to region failed");
                        }
                    }
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match region_r
            pause_for_input("\nPress Enter to continue: ");
            return step_inc;
        } //end command to

        if cmd[0] == "ibn" {
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    let ndx = &dm1.get_action(act_num).seek_edge_needs1();

                    if ndx.len() > 0 {
                        println!("Seek Edge Needs1 are {}", &ndx);
                    } else {

                        let ndx = &dm1.get_action(act_num).seek_edge_needs2();
                    
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
            pause_for_input("\nPress Enter to continue: ");
            return 0;
        }

        if cmd[0] == "ss" {
            let mut step_inc = 0;
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!("Act {} sample State {}", act_num, dm1.get_cur_state());
                    dm1.take_action_need(&SomeNeed::StateNotInGroup {
                        dom_num: dm1.get_num(),
                        act_num: act_num,
                        targ_state: dm1.get_cur_state().clone(),
                    });
                    step_inc = 1;
                }
                Err(error) => {
                    println!("\n{}", error);

                }
            } // end match
            pause_for_input("\nPress Enter to continue: ");
            return step_inc;
        }

        if cmd[0] == "g1" {
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    let sqrs = dm1.get_action(act_num)
                        .get_squares()
                        .states_in_1_region(&dm1.get_action(act_num).get_groups().regions());

                    println!("Act {} State in one group {}", act_num, &sqrs);
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            pause_for_input("\nPress Enter to continue: ");
            return 0;
        }

        if cmd[0] == "ps" {
            
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    println!(
                        "Squares of Action {} are:\n{}\n",
                        &act_num, &dm1.get_action(act_num).get_squares()
                    );
                }
                Err(error) => {
                    println!("\n{}", error);
                }
            } // end match
            pause_for_input("\nPress Enter to continue: ");
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
                            dm1.set_cur_state(&a_state);
                            dm1.take_action_need(&SomeNeed::StateNotInGroup {
                                dom_num: dm1.get_num(),
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
            pause_for_input("\nPress Enter to continue: ");
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
                            let stas = dm1.get_action(act_num).get_squares().stas_in_reg(&aregion);

                            let mut flg = 0;

                            for stax in stas.iter() {
                                if flg == 1 {
                                    psstr.push_str(",\n");
                                }

                                if let Some(sqrx) = dm1.get_action(act_num).get_squares().find(stax) {
                                    psstr.push_str(&format!("    {}", sqrx));
                                } else {
                                    println!("Square {} not found??", stax);
                                }

                                flg = 1;
                            }
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

            pause_for_input("\nPress Enter to continue: ");
            return 0;
        }

        if cmd[0] == "aj" {
            // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region
                    match dm1.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            let stas = dm1.get_action(act_num).get_squares().stas_adj_reg(&aregion);
                            println!("Squares adj to {} are {}", &aregion, &stas);
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
            pause_for_input("\nPress Enter to continue: ");
            return 0;
        }

        if cmd[0] == "g1" {
           // Get act number from string
            match dm1.act_num_from_string(&cmd[1]) {
                Ok(act_num) => {
                    // Get region from string
                    match dm1.region_from_string(&cmd[2]) {
                        Ok(aregion) => {
                            let sta_1s = dm1.get_action(act_num)
                                .get_squares()
                                .states_in_1_region(&dm1.get_action(act_num).get_groups().regions());

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
            pause_for_input("\nPress Enter to continue: ");
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
                            if let Some(grpx) = dm1.get_action(act_num).get_groups().find(&aregion) {
                                let sqr1 = dm1.get_action(act_num)
                                    .get_squares()
                                    .find(&grpx.get_region().get_state1())
                                    .unwrap();
                                println!("Square 1: {}", &sqr1);
                                let sqr2 = dm1.get_action(act_num)
                                    .get_squares()
                                    .find(&grpx.get_region().get_state2())
                                    .unwrap();
                                println!("Square 2: {}", &sqr2);
                            } else {
                                println!("Region {} not found!", &aregion);
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
            pause_for_input("\nPress Enter to continue: ");
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
            pause_for_input("\nPress Enter to continue: ");
            return step_inc;
        } // end command ss
    } // end 4-word commands

    println!("\nDid not understand command: {:?}", cmd);
    0
} // end do_command

/// Print a domain
fn print_domain(dmxs: &DomainStore, dom_num: usize) {
    print!("\nCurrent Domain: {} of {}", dom_num, dmxs.num_domains());
    println!("\nActs: {}", &dmxs[dom_num].get_actions());

    if let Some(areg) = &dmxs[dom_num].get_optimal() {
        let mut in_opt = "Not in";
        let optstr = areg.formatted_string();
        if areg.is_superset_of_state(&dmxs[dom_num].get_cur_state()) {
            in_opt = "in";
        }
        println!("\nStep: {} Dom: {} Current State: {} {} Optimal Region: {}", &dmxs.get_step(), dom_num, &dmxs[dom_num].get_cur_state(), &in_opt, optstr);
    } else {
        println!("\nStep: {} Dom: {} Current State: {}",&dmxs.get_step(), dom_num, &dmxs[dom_num].get_cur_state());
    }
    if dmxs.get_step() > 300 {
        assert!(1 == 2);
    }
}

/// Display usage options.
fn usage() {
    println!("\nStartup Commands: <invoke> may be the command \"ues\" or \"cargo run\"");
    println!("\n    <invoke>                 - Run once, stop when no needs can be done.");
    println!("    <invoke> <number times>  - Run a number of times, stop at last run, or when no needs can be done and one, or more, cannot.");
    println!("    <invoke> [h | help]      - Show this list.\n");

    println!("\nSession Commands:");

    println!(
        "    Press Enter (no command) - Satisfy one need that can be done, if any."
    );
    println!("\n    aj <act num> <region>    - For an Action, print Adjacent Squares to a region.");
    println!(
        "\n    cd <dom num>             - Change the displayed Domain to the given Domain number."
    );
    println!("\n    co                       - Change the current domain optimal region to None.");
    println!("    co <region>              - Change the current domain optimal region to the given region.");
    println!("\n    cs <state>               - Arbitrary Change State.");
    println!("\n    dn <need number>         - Run a particular need from the need list.");
    println!(
        "\n    g1 <act num>             - For an Action, print squares that are only in one group."
    );
    println!(
        "    g1 <act num> <region>    - For an Action and region, print squares that are only in the given region."
    );
    println!(
        "\n    gps <act num> <region>    - For an Action and region, print squares that define the group region."
    );
    println!("\n    h | help                 - Show this list.");
    println!("\n    ld <path>                - Load data from a file.");
    println!("\n    ppd <need number>        - Print the Plan Details for a given need.");
    println!("\n    ps <act num>             - For an Action, Print all Squares.");
    println!("    ps <act num> <region>    - For an Action, Print Squares in a region.");
    
    println!("\n    q | exit | quit          - Quit program.");
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!("\n    sd <path>                - Store data to a file.\n");
    println!("\n    so                       - Start Over.");
    println!("\n    ss <act num>                        - Action to Sample the current State.");
    println!("    ss <act num> <state>                - Action to Sample a given State.");
    println!(
        "    ss <act num> <state> <result-state> - Action to take an arbitrary State Sample.\n"
    );
    println!("\n    ta <act-num>             - Take an arbirary action with the current state");
    println!("\n    to <region>              - Change the current state to within a region, by calculating and executing a plan.");
    println!("\n    A domain number is an integer, zero or greater, where such a domain exists.");
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("\n    A need number is an integer, zero or greater, where such a need exists.\n");
    println!("    A state starts with an 's' character, followed by zero, or more, zero and one characters.\n");
    println!("    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. Leading zeros can be omitted.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");


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
