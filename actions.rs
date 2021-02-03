// Actions to be taken by the UES
//
use crate::change::SomeChange;
use crate::state::SomeState;

// The number of bits is set by the line of code that creates
// the first current state, with 1, or more, integers.

pub fn take_action(dom_num: usize, act_num: usize, cur_state: &SomeState, hv: usize) -> SomeState {
    if dom_num == 0 {
        if act_num == 0 {
            return dom0_act0(cur_state, hv);
        } else if act_num == 1 {
            return dom0_act1(cur_state, hv);
        } else if act_num == 2 {
            return dom0_act2(cur_state, hv);
        } else if act_num == 3 {
            return dom0_act3(cur_state, hv);
        } else if act_num == 4 {
            return dom0_act4(cur_state, hv);
        } else if act_num == 5 {
            return dom0_act5(cur_state, hv);
        } else if act_num == 6 {
            return dom0_act6(cur_state, hv);
        } else {
            panic!("Dom: 0, Uknown Action number {}", act_num);
        }
    } else if dom_num == 1 {
        if act_num == 0 {
            return dom1_act0(cur_state, hv);
        } else if act_num == 1 {
            return dom1_act1(cur_state, hv);
        } else if act_num == 2 {
            return dom1_act2(cur_state, hv);
        } else if act_num == 3 {
            return dom1_act3(cur_state, hv);
        } else {
            panic!("Dom: 1, Uknown Action number {}", act_num);
        }
    } else {
        panic!("Unknown Domain number {}", dom_num);
    }
}

// Domain 0 actions

pub fn dom0_act0(cur: &SomeState, hv: usize) -> SomeState {
    if cur.is_bit_set(3) && cur.is_bit_set(1) == false     // ...1X0X
        || cur.is_bit_set(3) == false && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1

        let new_state = cur.toggle_bits(vec![0]);
        println!(
            "Dom 0 Act 0 {} -> {} change: {}\n",
            cur,
            new_state,
            SomeChange::new(&cur, &new_state)
        );
        return new_state;
    }

    // When the action is created, changing the hv parm (0,1,2), in the
    // main file will have the effect of changing these groups to have
    // 1, 2 or 3 results.

    if cur.is_bit_set(1) {
        // ...101x, 1x10, given that the above is not true

        if hv % 2 == 0 {
            let new_state = cur.toggle_bits(vec![1]);
            println!(
                "Dom 0 Act 0  {} -> {} change: {}\n",
                cur,
                new_state,
                SomeChange::new(&cur, &new_state)
            );
            return new_state;
        } else {
            let new_state = cur.toggle_bits(vec![2]);
            println!(
                "Dom 0 Act 0  {} -> {} change: {}\n",
                cur,
                new_state,
                SomeChange::new(&cur, &new_state)
            );
            return new_state;
        }
    } else {
        // ...000x, 0x00, given that the above is not true

        if hv % 3 == 0 {
            let new_state = cur.toggle_bits(vec![1]);
            println!(
                "Dom 0 Act 0  {} -> {} change: {}\n",
                cur,
                new_state,
                SomeChange::new(&cur, &new_state)
            );
            return new_state;
        } else if hv % 3 == 1 {
            let new_state = cur.toggle_bits(vec![2]);
            println!(
                "Dom 0 Act 0  {} -> {} change: {}\n",
                cur,
                new_state,
                SomeChange::new(&cur, &new_state)
            );
            return new_state;
        } else {
            let new_state = cur.toggle_bits(vec![3]);
            println!(
                "Dom 0 Act 0  {} -> {} change: {}\n",
                cur,
                new_state,
                SomeChange::new(&cur, &new_state)
            );
            return new_state;
        }
    }
} // end action0

pub fn dom0_act1(cur: &SomeState, _hv: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![1]);
    println!(
        "Dom 0 Act 1  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom0_act2(cur: &SomeState, _hv: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![2]);
    println!(
        "Dom 0 Act 2  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom0_act3(cur: &SomeState, _hv: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![3]);
    println!(
        "Dom 0 Act 3  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom0_act4(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![4]);
    println!(
        "Dom 0 Act 4  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom0_act5(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![5]);
    println!(
        "Dom 0 Act 5  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom0_act6(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 6;
    let new_state = cur.toggle_bits(vec![2, 3]);
    println!(
        "Dom 0 Act {}  {} -> {} change: {}\n",
        num,
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

// Domain 1 actions

pub fn dom1_act0(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![6]);
    println!(
        "Dom 1 Act 0  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom1_act1(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![7]);
    println!(
        "Dom 1 Act 1  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom1_act2(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![8]);
    println!(
        "Dom 1 Act 2  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}

pub fn dom1_act3(cur: &SomeState, _num_seen: usize) -> SomeState {
    let new_state = cur.toggle_bits(vec![9]);
    println!(
        "Dom 1 Act 3  {} -> {} change: {}\n",
        cur,
        new_state,
        SomeChange::new(&cur, &new_state)
    );
    return new_state;
}
