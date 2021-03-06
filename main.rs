// Main function for an Unorthodox Expert System

#![allow(
// dead_code,
//  unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
)]

mod action;
use crate::action::SomeAction;
mod actionstore;
mod bits;
//use crate::bits::SomeBits;
//mod bitsstore;
mod group;
mod groupstore;
mod mask;
//use crate::mask::SomeMask;
mod maskstore;
mod need;
mod region;
//use crate::region::SomeRegion;
mod change;
//use crate::change::SomeChange;
mod regionstore;
mod resultstore;
mod rule;
mod rulestore;
mod square;
mod squarestore;
mod state;
//use crate::state::SomeState;
mod statestore;
use need::SomeNeed;
//mod combinable;
mod domain;
mod needstore;
//use crate::needstore::NeedStore;
mod plan;
mod pn;
mod step;
mod stepstore;
use domain::SomeDomain;
mod actions;
mod domainstore;
mod tests;
use domainstore::DomainStore;
mod inxplan;
use crate::inxplan::InxPlan;
mod randompick;
mod truth;
// use crate::randompick::RandomPick;

use std::io;
use std::io::{Read, Write};
use std::process;
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
    let mut dom1 = SomeDomain::new(num_ints, inx_str, "r101X");

    dom1.push(SomeAction::new(num_ints), 6);
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
    let mut dom2 = SomeDomain::new(num_ints, inx_str, "r10_1X00_0000");

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

    let mut dmxs = init();

    usage();

    let mut dom_num = 0;
    let mut run = 0;

    loop {
        dmxs.step += 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();

        //let mut dmx = &mut dmxs[dom_num];

        print_domain(&dmxs, dom_num);

        //        print!("\nCurrent Domain: {}", dom_num,);

        //        print!(
        //            " Predictable changes: {}",
        //            &dmxs[dom_num].x_mask,
        //        );

        //        println!("\nActs: {}", &dmxs[dom_num].actions);

        // Vector for position = display index, val = need_plans index
        let mut need_can = Vec::<usize>::with_capacity(nds.len());

        let mut need_plans = Vec::<InxPlan>::with_capacity(1);

        let mut can_do = 0;
        let mut cant_do = 0;

        if nds.len() > 0 {
            // Check if each need can be done
            need_plans = dmxs.evaluate_needs(&nds);

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
                run = 0;
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
                }
            }
        } else {
            println!("\nAction needs: None");
        }

        let mut cmd = Vec::<String>::with_capacity(10);

        if run > 0 {
        } else {
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
                    dmxs.take_action_need(dom_num, &ndx);
                } else {
                    // println!("NOT doing dmx.take_action_need");
                }
            } else {
                dmxs[dom_num].check_async();
                // If no needs, change the state to an optimal state if needed
                if dmxs[dom_num]
                    .optimal
                    .is_superset_of_state(&dmxs[dom_num].cur_state)
                {
                } else {
                    let optimal = dmxs[dom_num].optimal.clone();
                    if dmxs[dom_num].to_region(&optimal) {
                        println!("\nChange to optimal region succeeded");
                    } else {
                        println!("\nChange to optimal region failed");
                    }
                }
            }

            if can_do == 0 {
                if run > 0 {
                    run -= 1;
                    if run > 0 {
                        dmxs = init();
                    }
                }
            }
            continue;
        } else {
            if cmd.len() == 1 {
                if cmd[0] == "run" {
                    run = 1;
                    if nds.len() == 0 {
                        dmxs = init();
                    }
                    continue;
                } else if cmd[0] == "so" {
                    dmxs = init();
                    continue;
                }
            } else if cmd.len() == 2 {
                if cmd[0] == "run" {
                    let r_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Run Number: {}", err);
                        0
                    });
                    run = r_num;
                } else if cmd[0] == "cd" {
                    let d_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Domain Number: {}", err);
                        999
                    });
                    if d_num == 999 {
                        continue;
                    }
                    if d_num >= dmxs.num_domains() {
                        println!("\nInvalid Domain number");
                    } else {
                        dom_num = d_num;
                    }
                    continue;
                } else if cmd[0] == "ppd" {
                    let n_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Need Number: {}", err);
                        999
                    });
                    if n_num == 999 {
                        continue;
                    }
                    if n_num >= need_can.len() {
                        println!("Invalid Need Number: {}", cmd[1]);
                        continue;
                    }

                    let ndx = &nds[need_can[n_num]];

                    // Chnage the displayed Domian, if needed
                    if dom_num != ndx.dom_num() {
                        dom_num = ndx.dom_num();
                    }

                    print_domain(&dmxs, dom_num);

                    let pln = need_plans[need_can[n_num]].pln.as_ref().unwrap();

                    println!("\nNeed: {}", &ndx);

                    println!("\nPlan: \n{}", &pln.str2());

                    pause_for_input("\nPress Enter to continue: ");

                    continue;
                } else if cmd[0] == "dn" {
                    let n_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Need Number: {}", err);
                        999
                    });
                    if n_num == 999 {
                        continue;
                    }
                    if n_num >= need_can.len() {
                        println!("Invalid Need Number: {}", cmd[1]);
                        continue;
                    }

                    let ndx = &nds[need_can[n_num]];

                    let pln = need_plans[need_can[n_num]].pln.as_ref().unwrap();

                    println!("\nNeed chosen: {} {} {}\n", &n_num, &ndx, &pln.str_terse());

                    dom_num = ndx.dom_num();

                    //println!("need {}, plan {}", &ndx, &pln);

                    if pln.len() > 0 {
                        //println!("doing dmx.run_plan");
                        dmxs.run_plan(dom_num, &pln);
                    } else {
                        //println!("NOT doing dmx.run_plan");
                    }

                    if ndx.satisfied_by(&dmxs.cur_state(dom_num)) {
                        // println!("doing dmx.take_action_need");
                        dmxs.take_action_need(dom_num, &ndx);
                    } else {
                        // println!("NOT doing dmx.take_action_need");
                    }

                    continue;
                } else if cmd[0] == "ld" {
                    match load_data(&cmd[1]) {
                        Err(why) => {
                            println!("couldn't read {}: {}", &cmd[1], why);
                        }
                        Ok(new_dmxs) => {
                            print!("Data loaded");
                            dmxs = new_dmxs;
                            dmxs.step -= 1;
                        }
                    } // end match load_data

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

                    continue;
                } // end command sd
            }
            do_command(&mut dmxs[dom_num], &cmd);
        }
    } // end loop
} // end main

/// Do most commands entered by the user.
fn do_command(dm1: &mut SomeDomain, cmd: &Vec<String>) -> bool {
    // Handle one-word commands
    if cmd.len() == 1 {
        // Quit with q , exit, quit
        if cmd[0] == "q" || cmd[0] == "exit" || cmd[0] == "quit" {
            println!("Done");
            process::exit(0);
        }

        if cmd[0] == "h" || cmd[0] == "help" {
            usage();
            return true;
        }

        if cmd[0] == "pa" {
            //println!("\nActs: {}", dm1.actions);
            return true;
        }

        if cmd[0] == "gn" {
            println!("\nActs: {}", dm1.get_needs());
            return true;
        }
    } // end one-word commands

    // Handle two-word commands
    if cmd.len() == 2 {
        if cmd[0] == "co" {
            let region_r = dm1.region_from_string(&cmd[1]);
            match region_r {
                Ok(goal_region) => {
                    println!(
                        "Change Optimal regionfrom {} to {}",
                        dm1.optimal, goal_region
                    );
                    dm1.optimal = goal_region;
                    return true;
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                    return false;
                }
            } // end match region_r
        } //end command co

        // Arbitrary change state
        if cmd[0] == "cs" {
            let state_r = dm1.state_from_string(&cmd[1]);
            match state_r {
                Ok(a_state) => {
                    println!("Change state to {}", a_state);
                    dm1.cur_state = a_state.clone();
                    return true;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match
        } // end command cs

        if cmd[0] == "to" {
            let region_r = dm1.region_from_string(&cmd[1]);
            match region_r {
                Ok(goal_region) => {
                    println!(
                        "\nChange Current_state {} to region {}",
                        dm1.cur_state, goal_region
                    );
                    if goal_region.is_superset_of_state(&dm1.cur_state) {
                        println!(
                            "\nCurrent_state {} is already in region {}",
                            dm1.cur_state, goal_region
                        );
                        return false;
                    } else {
                        if dm1.to_region(&goal_region) {
                            println!("\nChange to region succeeded");
                            return true;
                        } else {
                            println!("\nChange to region failed");
                            return false;
                        }
                    }
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                    return false;
                }
            } // end match region_r
        } //end command to

        if cmd[0] == "ibn" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            let ndx = &dm1.actions[act_num].seek_edge_needs1();

            if ndx.len() > 0 {
                println!("Seek Edge Needs are {}", &ndx);
                return true;
            }

            let ndx = &dm1.actions[act_num].seek_edge_needs2();

            println!("Seek Edge Needs are {}", &ndx);
            return true;
        }

        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            println!("Act {} sample State {}", act_num, dm1.cur_state);
            dm1.take_action_need(&SomeNeed::StateNotInGroup {
                dom_num: dm1.num,
                act_num: act_num,
                targ_state: dm1.cur_state.clone(),
            });
            return true;
        }

        if cmd[0] == "pa" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            println!("{}", &dm1.actions[act_num]);
            return true;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            let sqrs = dm1.actions[act_num]
                .squares
                .states_in_1_region(&dm1.actions[act_num].groups.regions());

            println!("Act {} State in one group {}", act_num, &sqrs);
            return true;
        }

        if cmd[0] == "ps" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            println!(
                "Squares of Action {} are:\n{}\n",
                &act_num, &dm1.actions[act_num].squares
            );
            return true;
        }
    } // end two-word commands

    // Handle three-word commands
    if cmd.len() == 3 {
        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let state_r = dm1.state_from_string(&cmd[2]);
            match state_r {
                Ok(a_state) => {
                    println!("Act {} sample State {}", act_num, a_state);
                    dm1.cur_state = a_state.clone();
                    dm1.take_action_need(&SomeNeed::StateNotInGroup {
                        dom_num: dm1.num,
                        act_num: act_num,
                        targ_state: a_state,
                    });
                    return true;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match state_r
        }

        if cmd[0] == "ps" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                let mut psstr = String::from(format!(
                    "Squares of Action {} in region {} are:\n",
                    &act_num, &aregion
                ));
                let stas = dm1.actions[act_num].squares.stas_in_reg(&aregion);

                let mut flg = 0;

                for stax in stas.iter() {
                    if flg == 1 {
                        psstr.push_str(",\n");
                    }

                    if let Some(sqrx) = dm1.actions[act_num].squares.find(stax) {
                        psstr.push_str(&format!("{}", sqrx));
                    } else {
                        println!("Square {} not found??", stax);
                    }

                    flg = 1;
                }

                println!("{}", &psstr);
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "aj" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                let stas = dm1.actions[act_num].squares.stas_adj_reg(&aregion);
                println!("Squares adj to {} are {}", &aregion, &stas);
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                let sta_1s = dm1.actions[act_num]
                    .squares
                    .states_in_1_region(&dm1.actions[act_num].groups.regions());

                println!(
                    "Squares in one region, in {} are {}",
                    &aregion,
                    aregion.states_in(&sta_1s)
                );
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "gps" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                if let Some(grpx) = dm1.actions[act_num].groups.find(&aregion) {
                    let sqr1 = dm1.actions[act_num]
                        .squares
                        .find(&grpx.region.state1)
                        .unwrap();
                    println!("Square 1: {}", &sqr1);
                    let sqr2 = dm1.actions[act_num]
                        .squares
                        .find(&grpx.region.state2)
                        .unwrap();
                    println!("Square 2: {}", &sqr2);
                } else {
                }
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }
    } // end 3-word commands

    // Handle four-word commands
    if cmd.len() == 4 {
        // Take Sample (ts) with <action num> <initial-state> <result-state>, don't update current state
        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let i_state_rslt = dm1.state_from_string(&cmd[2]);
            match i_state_rslt {
                Ok(i_state) => {
                    let r_state_rslt = dm1.state_from_string(&cmd[3]);
                    match r_state_rslt {
                        Ok(r_state) => {
                            println!("Act {} take sample {} -> {}", act_num, &i_state, &r_state);

                            dm1.eval_sample_arbitrary(act_num, &i_state, &r_state);
                            return true;
                        }
                        Err(error) => {
                            println!("\nDid not understand state, {}", error);
                            return false;
                        }
                    } // end match r_state_rslt
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match i_state_rslt
        }
    } // end 4-word commands

    println!("\nDid not understand command: {:?}", cmd);
    false
} // end do_command

fn print_domain(dmxs: &DomainStore, dom_num: usize) {
    print!("\nCurrent Domain: {}", dom_num,);
    println!("\nActs: {}", &dmxs[dom_num].actions);

    let mut in_opt = "Not in";

    if dmxs[dom_num]
        .optimal
        .is_superset_of_state(&dmxs[dom_num].cur_state)
    {
        in_opt = "in";
    }

    println!(
        "\nStep: {} Dom: {} Current State: {} {} Optimal Region: {}",
        &dmxs.step, dom_num, &dmxs[dom_num].cur_state, &in_opt, &dmxs[dom_num].optimal
    );
}

/// Display usage options.
fn usage() {
    println!("\nCommands:");
    println!("    h | help                 - Show this list.\n");

    println!(
        "    Press Enter (no command) - Check for any Action needs, satisfy one need if possible."
    );
    println!("                               If no needs can be done, try to change the current state to the optimal region.\n");
    println!("    aj <act num> <region>    - For an Action, print Adjacent Squares to a region.\n");
    println!(
        "    cd <dom num>             - Change the displayed Domain to the given Domain number.\n"
    );

    println!("    co <region>              - Change the Optimal region to the given region.\n");
    println!("    cs <state>               - Arbitrary Change State.\n");
    println!("    dn <need number>         - Run a particular need from the need list.\n");
    println!(
        "    g1 <act num>             - For an Action, print squares that are only in one group."
    );
    println!(
        "    g1 <act num> <region>    - For an Action and region, print squares that are only in the given region.\n"
    );
    println!(
        "    gps <act num> <region>    - For an Action and region, print squares that define the group region.\n"
    );
    println!("    pa                       - Print all Actions of the current Domain.");
    println!("    pa <act num>             - Print an Action of the current Domain.");
    println!("\n    ppd <need number>        - Print the Plan Details for a given need.");
    println!("\n    ps <act num>             - For an Action, Print all Squares.");
    println!("    ps <act num> <region>    - For an Action, Print Squares in a region.\n");

    println!("    ss <act num>                        - Action to Sample the current State.");
    println!("    ss <act num> <state>                - Action to Sample a given State.");
    println!(
        "    ss <act num> <state> <result-state> - Action to take an arbitrary State Sample.\n"
    );

    println!("    to <region>              - Change the current state to within a region, by calculating and executing a plan.");
    println!("\n    A domain number is an integer, zero or greater, where such a domain exists.");
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("\n    A need number is an integer, zero or greater, where such a need exists.\n");
    println!("    A state starts with an 's' character, followed by zero, or more, zero and one characters.\n");
    println!("    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. Leading zeros can be omitted.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    ld <path>                - Load data from a file.");
    println!("\n    sd <path>                - Store data to a file.\n");

    println!("    run                      - Run until no needs left.");
    println!("    run <number times>       - Run a number of times.");

    println!("\n    q | exit | quit          - Quit program.");
    println!("\n    so                       - Start Over.");
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
