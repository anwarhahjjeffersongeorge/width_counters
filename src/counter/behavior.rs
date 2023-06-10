//! Counter behavioral elements
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use strum::{EnumCount, IntoEnumIterator};
use enumflags2::{bitflags, BitFlags};


/// Counting behaviors
#[bitflags(default = Nonmonotonic | Increment | Acyclic)]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum CountingBehavior {
  /// Non monotonic (default)
  Nonmonotonic = 1 << 0,
  /// In monotonic mode, values of the counter have consistent 
  /// such the differences between any two values (taken in 
  /// the order they were produced) will not change sign 
  Monotonic = 1 << 1,
  /// Increases unless order is specified (default)
  Increment = 1 << 2, 
  /// Decreases unless order is specified
  Decrement = 1 << 3,
  /// Stops at overflow limit (default) 
  Acyclic = 1 << 4,
  /// Wraps around on overflow
  Cyclic = 1 << 5,
}
impl CountingBehavior {
  /// Do these counting behaviors conflict?
  pub fn conflicts(&self, rhs: &Self) -> CountingBehaviorConflict {
    match (self, rhs) {
      (Self::Increment, Self::Decrement) 
      | (Self::Decrement, Self::Increment) => CountingBehaviorConflict::Always,
      (Self::Monotonic, Self::Nonmonotonic) 
      | (Self::Nonmonotonic, Self::Monotonic) => CountingBehaviorConflict::Always,
      (Self::Cyclic, Self::Acyclic) 
      | (Self::Acyclic, Self::Cyclic) => CountingBehaviorConflict::Overflowing,
      _ => CountingBehaviorConflict::None
    }
  }
}
impl HasCountingBehavior for BitFlags<CountingBehavior> {
  fn get_behavior_ref(&self) -> &BitFlags<CountingBehavior> { self }
}

#[derive(
  Copy, Clone, Debug, PartialEq, 
  Eq, PartialOrd, Ord, Hash, 
  strum::EnumCount, strum::EnumIter
)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
/// A conflict between counting behaviors
pub enum CountingBehaviorConflict {
  /// No conflict
  None, 
  /// Conflict for any operation
  Always,
  /// Conflict only at overflow operations
  Overflowing,
}
impl CountingBehaviorConflict {
  const CONFLICTING_VARIANTS: usize = Self::COUNT - 1;
}
pub type AllCountingBehaviorConflicts = [
  CountingBehaviorConflict; 
  CountingBehaviorConflict::CONFLICTING_VARIANTS
];

/// Something that has counting behaviors 
pub trait HasCountingBehavior{
  fn get_behavior_ref(&self) -> &BitFlags<CountingBehavior>;
  fn get_behavior_conflicts(&self) -> AllCountingBehaviorConflicts {
    let mut r = [
      CountingBehaviorConflict::None; 
      CountingBehaviorConflict::CONFLICTING_VARIANTS
    ];
    let ii = self.get_behavior_ref().iter();
    let jj = ii.clone();
    CountingBehaviorConflict::iter()
    .filter(|counting_behavior_conflict| counting_behavior_conflict.ne(&CountingBehaviorConflict::None))
      .enumerate()
      .for_each(|(x, counting_behavior_conflict)| {
        if ii.clone().any(|i| {
          jj.clone().any(|j| {
            i.conflicts(&j).eq(&counting_behavior_conflict)
          })
        }) {
          r[x] = counting_behavior_conflict;
        }
      });
    r
  }
}

/// Something that can counts up or down
pub trait CountsNonmotonically {
  /// Can increment 
  fn can_inc(&self) -> bool;
  /// Increment by one if possible
  fn inc_one(&self);
  /// Is it incrementable based on internal counting behavior settings?
  fn is_incrementable(&self) -> bool;
  // Is it at the increment bound?
  fn is_at_increment_bound(&self) -> bool;
  // Is it within the increment bound?
  fn is_within_increment_bound(&self) -> bool;
  /// Can decrement 
  fn can_dec(&self) -> bool;
  /// Decrement by one if possible
  fn dec_one(&self);
  /// Is it incrementable based on internal counting behavior settings?
  fn is_decrementable(&self) -> bool;
  // Is it at the decrement bound?
  fn is_at_decrement_bound(&self) -> bool;
  // Is it within the decrement bound?
  fn is_within_decrement_bound(&self) -> bool;
}

/// Something that is a counter 
pub trait IsCounter: HasCountingBehavior + CountsNonmotonically {
  type Unit: Sized + PartialEq + PartialOrd + Clone + core::fmt::Debug + Copy + core::hash::Hash;
  /// Get the ordering 
  fn get_ordering_ref(&self) -> &core::sync::atomic::Ordering;
  /// Get the current value with the preset ordering 
  fn get_current(&self) -> Self::Unit;
}