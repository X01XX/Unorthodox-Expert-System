//! Actions to be taken, creating samples for the UES.
//!
//! These actions are for testing and learning, they should eventually be replaced
//! by actions that make changes outside the program.
//!
//! Storing a function pointer in the SomeAction runs into problems with the parallel crate
//! and the serialization crate.

use crate::rule::SomeRule;
use crate::sample::SomeSample;
use crate::state::SomeState;

/// Array of action function pointers for domain 0.
const DOM0_ACTIONS: [for<'a> fn(&SomeState, usize) -> SomeState; 10] = [
    dom0_act0, dom0_act1, dom0_act2, dom0_act3, dom0_act4, dom0_act5, dom0_act6, dom0_act7,
    dom0_act8, dom0_act9,
];

/// Array of action function pointers for domain 1.
const DOM1_ACTIONS: [for<'a> fn(&SomeState, usize) -> SomeState; 7] = [
    dom1_act0, dom1_act1, dom1_act2, dom1_act3, dom1_act4, dom1_act5, dom1_act6,
];

/// Take an action given the domain number, action number, current_state and last change mask (if any).
pub fn take_action(
    dom_id: usize,
    act_id: usize,
    cur_state: &SomeState,
    sample_hint: usize,
) -> SomeState {
    let new_state = if dom_id == 0 {
        DOM0_ACTIONS[act_id](cur_state, sample_hint)
    } else {
        DOM1_ACTIONS[act_id](cur_state, sample_hint)
    };
    println!(
        "\nDom {} {} -{}-> {} R[{}]",
        dom_id,
        cur_state,
        act_id,
        new_state,
        SomeRule::new(&SomeSample::new(cur_state.clone(), new_state.clone()))
    );
    new_state
}

/// Domain 0, act 0, actions, given the current state.
/// The SomeMask argument is a kludge to support multiple result actions.
pub fn dom0_act0(cur: &SomeState, sample_hint: usize) -> SomeState {
    if cur.is_bit_set(3) && !cur.is_bit_set(1)     // ...1X0X
        || !cur.is_bit_set(3) && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1
        cur.change_bit(0)
    } else if cur.is_bit_set(1) {
        // ...101x, 1x10, alternate between two changes.
        let sample_num = sample_hint % 2;

        if sample_num == 0 {
            cur.change_bit(1)
        } else {
            cur.change_bit(2)
        }
    } else {
        // ...000x, 0x00, alternate between 4 changes.
        if sample_hint == 0 {
            cur.change_bit(0)
        } else if sample_hint == 1 {
            cur.change_bit(1)
        } else if sample_hint == 2 {
            cur.change_bit(2)
        } else {
            cur.change_bit(3)
        }
    }
} // end action0

/// Domain 0, act 1, actions, given the current state.
/// Toggle bit 1. ....../Xx/.
pub fn dom0_act1(cur: &SomeState, _sample_hint: usize) -> SomeState {
    if cur.is_bit_set(1) {
        cur.change_bit(2)
    } else {
        cur.change_bit(1)
    }
}

/// Domain 0, act 2, actions, given the current state.
/// Toggle bit 2. ...../Xx/..
pub fn dom0_act2(cur: &SomeState, _sample_hint: usize) -> SomeState {
    if cur.is_bit_set(1) {
        cur.change_bit(1)
    } else {
        cur.change_bit(2)
    }
}

/// Domain 0, act 3, actions, given the current state.
/// Toggle bit 3. ..../Xx/...
pub fn dom0_act3(cur: &SomeState, _sample_hint: usize) -> SomeState {
    if cur.is_bit_set(3) {
        cur.change_bit(4)
    } else {
        cur.change_bit(3)
    }
}

/// Domain 0, act 4, actions, given the current state.
/// Toggle bit 4. .../Xx/...
pub fn dom0_act4(cur: &SomeState, _sample_hint: usize) -> SomeState {
    if cur.is_bit_set(3) {
        cur.change_bit(3)
    } else {
        cur.change_bit(4)
    }
}

/// Domain 0, act 5, actions, given the current state.
/// Toggle bit 5. ../Xx/.....
pub fn dom0_act5(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(5)
}

/// Domain 0, act 6, actions, given the current state.
/// Toggle bit 2,3 ..../Xx/Xx/..
pub fn dom0_act6(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(2).change_bit(3)
}

/// Domain 0, act 7, actions, given the current state.
/// Toggle bit 6 and 7, Xx/Xx/......
pub fn dom0_act7(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(6).change_bit(7)
}

/// Domain 0, act 8, actions, given the current state.
/// Toggle bit 6 ./Xx/......
/// Will act 8 be used in combination with act 7 to effect a change of bit 7
/// without affecting bit 6?
/// /../Xx/, /../01/, /../10/
/// /Xx/Xx/, /Xx/Xx/, /Xx/Xx/
/// /Xx/XX/, /Xx/00/, /Xx/11/
pub fn dom0_act8(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(6)
}

/// Form a rule using X->0 and X->1.
pub fn dom0_act9(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.set_bit_to_0(0).set_bit_to_1(1)
}
// Domain 1 actions

/// Domain 1, act 0, actions, given the current state.
/// Toggle bit 6.
pub fn dom1_act0(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(5)
}

/// Domain 1, act 1, actions, given the current state.
/// Toggle bit 7.
pub fn dom1_act1(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(6)
}

/// Domain 1, act 2, actions, given the current state.
/// Toggle bit 8.
pub fn dom1_act2(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(7)
}

/// Domain 1, act 3, actions, given the current state.
/// Toggle bit 9.
pub fn dom1_act3(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(8)
}

/// Domain 1, act 4, actions, given the current state.
pub fn dom1_act4(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(9)
}

/// Domain 1, act 5, actions, given the current state.
pub fn dom1_act5(cur: &SomeState, _sample_hint: usize) -> SomeState {
    cur.change_bit(10)
}

/// Domain 1, act 6, actions, given the current state.
pub fn dom1_act6(cur: &SomeState, _sample_hint: usize) -> SomeState {
    if cur.is_bit_set(5) {
        cur.change_bit(8)
    } else if cur.is_bit_set(6) {
        cur.change_bit(9)
    } else {
        cur.change_bit(10)
    }
}
