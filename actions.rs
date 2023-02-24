//! Actions to be taken, creating samples for the UES.
//!
//! These actions are for testing and learning, they should eventually be replaced
//! by actions that make changes outside the program.
//!
//! Storing a function pointer in the SomeAction runs into problems with the parallel crate
//! and the serialization crate.

use crate::rule::SomeRule;
use crate::state::SomeState;

/// Take an action given the domain number, action number, current_state and last change mask (if any).
pub fn take_action(
    dom_num: usize,
    act_num: usize,
    cur_state: &SomeState,
    anum: usize,
) -> SomeState {
    if dom_num == 0 {
        if act_num == 0 {
            dom0_act0(cur_state, anum)
        } else if act_num == 1 {
            dom0_act1(cur_state)
        } else if act_num == 2 {
            dom0_act2(cur_state)
        } else if act_num == 3 {
            dom0_act3(cur_state)
        } else if act_num == 4 {
            dom0_act4(cur_state)
        } else if act_num == 5 {
            dom0_act5(cur_state)
        } else if act_num == 6 {
            dom0_act6(cur_state)
        } else if act_num == 7 {
            dom0_act7(cur_state)
        } else if act_num == 8 {
            dom0_act8(cur_state)
        } else if act_num == 9 {
            dom0_act9(cur_state)
        } else {
            panic!("Dom 0, Uknown Action number {act_num}");
        }
    } else if dom_num == 1 {
        if act_num == 0 {
            dom1_act0(cur_state)
        } else if act_num == 1 {
            dom1_act1(cur_state)
        } else if act_num == 2 {
            dom1_act2(cur_state)
        } else if act_num == 3 {
            dom1_act3(cur_state)
        } else if act_num == 4 {
            dom1_act4(cur_state)
        } else if act_num == 5 {
            dom1_act5(cur_state)
        } else if act_num == 6 {
            dom1_act6(cur_state)
        } else {
            panic!("Dom 1, Uknown Action number {act_num}");
        }
    } else {
        panic!("Unknown Domain number {dom_num}");
    }
}

/// Domain 0, act 0, actions, given the current state.
/// The SomeMask argument is a kludge to support multiple result actions.
pub fn dom0_act0(cur: &SomeState, anum: usize) -> SomeState {
    let new_state = if cur.is_bit_set(3) && !cur.is_bit_set(1)     // ...1X0X
        || !cur.is_bit_set(3) && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1
        cur.change_bit(0)
    } else if cur.is_bit_set(1) {
        // ...101x, 1x10, alternate between two changes.
        let sample_num = anum % 2;

        if sample_num == 0 {
            cur.change_bit(1)
        } else {
            cur.change_bit(2)
        }
    } else {
        // ...000x, 0x00, alternate between 4 changes.
        if anum == 0 {
            cur.change_bit(0)
        } else if anum == 1 {
            cur.change_bit(1)
        } else if anum == 2 {
            cur.change_bit(2)
        } else {
            cur.change_bit(3)
        }
    };

    println!(
        "\nDom 0 Act 0 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
} // end action0

/// Domain 0, act 1, actions, given the current state.
/// Toggle bit 1. ....../Xx/.
pub fn dom0_act1(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(1) {
        cur.change_bit(2)
    } else {
        cur.change_bit(1)
    };
    println!(
        "\nDom 0 Act 1 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 2, actions, given the current state.
/// Toggle bit 2. ...../Xx/..
pub fn dom0_act2(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(1) {
        cur.change_bit(1)
    } else {
        cur.change_bit(2)
    };
    println!(
        "\nDom 0 Act 2 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 3, actions, given the current state.
/// Toggle bit 3. ..../Xx/...
pub fn dom0_act3(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(3) {
        cur.change_bit(4)
    } else {
        cur.change_bit(3)
    };
    println!(
        "\nDom 0 Act 3 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 4, actions, given the current state.
/// Toggle bit 4. .../Xx/...
pub fn dom0_act4(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(3) {
        cur.change_bit(3)
    } else {
        cur.change_bit(4)
    };
    println!(
        "\nDom 0 Act 4 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 5, actions, given the current state.
/// Toggle bit 5. ../Xx/.....
pub fn dom0_act5(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(5);
    println!(
        "\nDom 0 Act 5 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 6, actions, given the current state.
/// Toggle bit 2,3 ..../Xx/Xx/..
pub fn dom0_act6(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(2).change_bit(3);
    println!(
        "\nDom 0 Act 6 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 7, actions, given the current state.
/// Toggle bit 6 and 7, Xx/Xx/......
pub fn dom0_act7(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(6).change_bit(7);
    println!(
        "\nDom 0 Act 7 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 8, actions, given the current state.
/// Toggle bit 6 ./Xx/......
/// Will act 8 be used in combination with act 7 to effect a change of bit 7
/// without affecting bit 6?
/// /../Xx/, /../01/, /../10/
/// /Xx/Xx/, /Xx/Xx/, /Xx/Xx/
/// /Xx/XX/, /Xx/00/, /Xx/11/
pub fn dom0_act8(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(6);
    println!(
        "\nDom 0 Act 8 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Form a rule using X->0 and X->1.
pub fn dom0_act9(cur: &SomeState) -> SomeState {
    let new_state = cur.set_bit_to_0(0).set_bit_to_1(1);
    println!(
        "\nDom 0 Act 9 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
// Domain 1 actions

/// Domain 1, act 0, actions, given the current state.
/// Toggle bit 6.
pub fn dom1_act0(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(5);
    println!(
        "\nDom 1 Act 0 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 1, actions, given the current state.
/// Toggle bit 7.
pub fn dom1_act1(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(6);
    println!(
        "\nDom 1 Act 1 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 2, actions, given the current state.
/// Toggle bit 8.
pub fn dom1_act2(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(7);
    println!(
        "\nDom 1 Act 2 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 3, actions, given the current state.
/// Toggle bit 9.
pub fn dom1_act3(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(8);
    println!(
        "\nDom 1 Act 3 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 4, actions, given the current state.
pub fn dom1_act4(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(9);
    println!(
        "\nDom 1 Act 4 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 5, actions, given the current state.
pub fn dom1_act5(cur: &SomeState) -> SomeState {
    let new_state = cur.change_bit(10);
    println!(
        "\nDom 1 Act 5 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 6, actions, given the current state.
pub fn dom1_act6(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(5) {
        cur.change_bit(8)
    } else if cur.is_bit_set(6) {
        cur.change_bit(9)
    } else {
        cur.change_bit(10)
    };
    println!(
        "\nDom 1 Act 6 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
