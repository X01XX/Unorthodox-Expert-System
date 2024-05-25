use crate::bits::SomeBits;
use crate::state::SomeState;
use serde::{Deserialize, Serialize};

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct DomainInterface {
    cur_state: SomeState,
}

impl DomainInterface {
    pub fn new(num_bits: usize) -> Self {
        Self {
            cur_state: SomeState::new(SomeBits::new(num_bits).new_random()),
        }
    }
}
