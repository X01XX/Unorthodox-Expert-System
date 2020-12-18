// Implement a store for Domains

use crate::domain::SomeDomain;
use std::fmt;
use std::ops::Index;
//use std::slice::Iter;
use std::slice::IterMut;

impl fmt::Display for DomainStore {
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

pub struct DomainStore {
    pub avec: Vec<SomeDomain>,
}

impl DomainStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
        }
    }

    pub fn _len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, mut val: SomeDomain) {
        val.num = self.avec.len();
        self.avec.push(val);
    }

    pub fn iter_mut(&mut self) -> IterMut<SomeDomain> {
        self.avec.iter_mut()
    }
}

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index<'a>(&'a self, i: usize) -> &'a SomeDomain {
        &self.avec[i]
    }
}
