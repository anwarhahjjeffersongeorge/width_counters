//! Atomic, thread-safe counters of differing integer widths
//!
//! ### Comes with
//! These counters support
//! - Incrementing/decrementing by default (1) or specified amounts,  
//! - Using per-operation [atomic orderings](::core::sync::atomic::Ordering), ([see this also](https://en.cppreference.com/w/c/atomic/memory_order))  
//! - Instantiation with default offset (0) and default atomic ordering ( sequentially consistent ),  
//! - Instantiation with custom offset, custom ordering or both, 
//! - Instantiation with custom enum-flag-based counting behaviors (monotonic, nonmonotonic, incrementing, decrementing, cyclic, acyclic)  
//! - [PartialEq], [Eq], [Hash](core::hash::Hash), [PartialOrd], [Ord], [Clone], [Debug](core::fmt::Debug), [Display](core::fmt::Display)  
//! - [Send], [Sync]
//! ### Optional features
//! - `serde`: Enable de/serialization


#![no_std]

mod counter;
pub use counter::*;
