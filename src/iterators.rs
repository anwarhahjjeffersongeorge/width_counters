//! Iterator support
use core::sync::atomic::AtomicBool;

use crate::*;

/// An iterator for a counter 
pub struct CounterIterator<I: IsCounter> {
  inner_counter: I,
  has_reached_bound: AtomicBool,
}
impl <I: IsCounter> Iterator for CounterIterator<I> {
  type Item = <I as IsCounter>::Unit;
  fn next(&mut self) -> Option<Self::Item> {
    let Self { inner_counter, has_reached_bound } = self;
    let ordering = inner_counter.get_ordering_ref();
    let reached_bound = has_reached_bound.load(*ordering);
    (!reached_bound).then(|| inner_counter.get_current())
    .and_then(|current|{
      if inner_counter.get_behavior_ref().contains(CountingBehavior::Increment) {
        inner_counter.inc_one();
      } else {
        inner_counter.dec_one();
      }
      let new = inner_counter.get_current();
      if new == current {
        has_reached_bound.store(true, *ordering);
      }
      Some(current)
    })
  }
}
impl <I:IsCounter> CounterIterator<I> {
  /// Instantiate 
  pub (crate) fn new(inner_counter: I) -> Self {
    Self {
      inner_counter, 
      has_reached_bound: AtomicBool::new(false),
    }
  } 
}
#[cfg(test)]
mod iterator_tests {
  use super::*;
  use enumflags2::make_bitflags;
  extern crate alloc;
  use alloc::vec::Vec;
  #[test]
  fn iterates_well() {
    let c = CounterU8::new_with_counting_behavior(
      make_bitflags!(CountingBehavior::{Acyclic | Increment | Monotonic})
    );
    let i = c.into_iter();
    assert_eq!(
      i.take(CounterU8::MAX as usize * 2).collect::<Vec<_>>(),
      (0..=CounterU8::MAX).collect::<Vec<_>>(),
      "Counter iterator within bounds must produce same as range, ceasing to yield when exhausted"
    )
  }
}