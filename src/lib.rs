//! Atomic, monotonically increasing counters of differing integer widths
//!
//! ### Comes with
//! These counters support
//! - Incrementing by default (1) or jumping by specified amounts,  
//! - Using per-operation [atomic orderings](::core::sync::atomic::Ordering), ([see this also](https://en.cppreference.com/w/c/atomic/memory_order))  
//! - Const instantiation with default offset (0) and default atomic ordering ( sequentially consistent ),  
//! - Const instantiation with custom offset, custom ordering or both, and  
//! - [PartialEq], [Eq], [Hash](core::hash::Hash), [PartialOrd], [Ord], [Clone], [Debug](core::fmt::Debug), [Display](core::fmt::Display)  
//!
//! ### Optional features
//! - `serde`: Enable de/serialization
#![no_std]

mod counter;
pub use counter::*;
