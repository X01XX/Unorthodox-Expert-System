// Implement a store for masks

use crate::mask::SomeMask;
use std::fmt;
use std::ops::Index;

impl fmt::Display for MaskStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

        for mskx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &mskx));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

pub struct MaskStore {
    avec: Vec<SomeMask>,
}

impl MaskStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeMask>::with_capacity(5),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn add(&mut self, val: SomeMask) {
        self.avec.push(val);
    }
}

impl Index<usize> for MaskStore {
    type Output = SomeMask;
    fn index<'a>(&'a self, i: usize) -> &'a SomeMask {
        &self.avec[i]
    }
}
