//! Counters

use core::{
    cmp, fmt,
    hash::{Hash, Hasher},
    i16, i32, i64, i8, isize,
    ops,
    sync::atomic::*,
    u16, u32, u64, u8, usize,
};
use paste::paste;
#[cfg(feature = "serde")]
use serde::{ser::SerializeStruct, Deserialize, Serialize};
use strum::{EnumCount, IntoEnumIterator};
use enumflags2::{bitflags, BitFlags, _internal::RawBitFlags};

mod private {
    use super::*;
    /// An AsRef/AsMut trait that shouldn't be used outside this crate  
    pub trait As<T> {
        fn as_ref(&self) -> &T;
        fn as_mut(&mut self) -> &mut T;
    }

    /// An ordering definition for serde
    #[derive(strum::AsRefStr)]
    #[cfg_attr(feature = "serde", derive(Deserialize))]
    #[non_exhaustive]
    pub enum O {
        AcqRel,
        Acquire,
        Relaxed,
        Release,
        SeqCst,
    }
    impl From<Ordering> for O {
        fn from(value: Ordering) -> Self {
            match value {
                Ordering::AcqRel => Self::AcqRel,
                Ordering::Acquire => Self::Acquire,
                Ordering::Relaxed => Self::Relaxed,
                Ordering::Release => Self::Release,
                Ordering::SeqCst => Self::SeqCst,
                _ => Self::SeqCst,
            }
        }
    }
    impl From<O> for Ordering {
        fn from(value: O) -> Self {
            #[allow(unreachable_patterns)]
            match value {
                O::AcqRel => Self::AcqRel,
                O::Acquire => Self::Acquire,
                O::Relaxed => Self::Relaxed,
                O::Release => Self::Release,
                O::SeqCst => Self::SeqCst,
                _ => Self::SeqCst,
            }
        }
    }
    #[cfg(feature = "serde")]
    impl Serialize for O {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            macro_rules! serialize {
        (@it $V:ident) => {{ serializer.serialize_unit_variant("O", Self::$V as u32, Self::$V.as_ref()) }};
        ($($V:ident,)+) => {{
          #[allow(unreachable_patterns)]
          match self {
            $(Self::$V => serialize!(@it $V),)+
            _ => serialize!(@it SeqCst),
          }
        }}
      }
            serialize! { AcqRel, Acquire, Relaxed, Release, SeqCst, }
        }
    }
}
use private::*;

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


/// Implements a counter or multiple counters
macro_rules! make_counter {
  (@Main $Prefix:ident => $Unit:ident | $Atomic:ident ) => {
    paste!{
      #[doc = r#"An atomic counter using ["# $Atomic r#"] / ([core::"# $Unit r#"])."#]
      #[doc = ""]
      #[doc = r"### Behavior  "]
      #[doc = "1. The default ordering is [Sequentially Consistent](Ordering::SeqCst).  "]
      #[doc = "2. The ordering used for atomic operations is customizable for operations ending in `with_ordering`.  "]
      #[doc = "3. The choice of ordering *intentionally* impacts **ALMOST EVERYTHING** about how this counter works, including de/serialization, incrementing, decrementing, equality comparisons, partial ordering comparisons, etc.  "]
      #[doc = "4. Total (non-partial) ordering comparisons always use the default ordering.  "]
      #[doc = "5. Unlike the underlying [" $Atomic "], this will not wrap on overflow unless the cyclic behavior mode is set. "]
      #[doc = "6. The default behavior is non-monotonic, so the counter can increment and decrement. "]
      #[doc = r"### Ordering  "]
      #[doc = "- PartialEq is implemented such that counters with differing orderings are never equal.  "]
      #[doc = "- PartialOrd is implemented such that counters with differing [(atomic) orderings](Ordering) produce no [(comparison) ordering](core::cmp::Ordering).  "]
      #[doc = "- **(Saturating) arithmetic operations are implemented such that differing atomic orderings between the operands are ignored!**"]
      #[doc = r"### Miscellaneous"]
      #[doc = r"You can use the [to_x](Self::to_x) method to convert to any type that implements From\<" $Unit r"\>"]
      #[derive(Debug)]
      pub struct [<$Prefix $Unit:camel>] {
        /// The underlying counter
        inner: $Atomic,
        /// The ordering used for all operations
        ordering: Ordering,
        /// A bitmask of counting modes
        counting_behavior: BitFlags<CountingBehavior>,
        /// Conflicts 
        conflicts: AllCountingBehaviorConflicts,
      }

      #[cfg(feature = "serde")]
      #[allow(non_snake_case)]
      mod [<serde_impls_ $Prefix $Unit:camel >] {
        use super::*;
        impl Serialize for [<$Prefix $Unit:camel>] {
          fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
              S: serde::Serializer
          {
            let name_str = stringify!([<$Prefix $Unit:camel>]);
            let mut counter = serializer.serialize_struct( name_str, 2 )?;
            counter.serialize_field("ordering", &O::from(self.ordering))?;
            counter.serialize_field("inner", &self.get())?;
            counter.serialize_field("counting_behavior", &self.counting_behavior)?;
            counter.end()
          }
        }

        impl<'de> Deserialize<'de> for [<$Prefix $Unit:camel>] {
          fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
              where
                  D: serde::Deserializer<'de>
          {
            let name_str = stringify!([<$Prefix $Unit:camel>]);

            #[derive(Deserialize)]
            #[serde(field_identifier, rename_all = "lowercase")]
            enum Field { Inner, Ordering, CountingBehavior }

            struct [<$Prefix $Unit:camel Visitor>];
            impl<'de> serde::de::Visitor<'de> for [<$Prefix $Unit:camel Visitor>] {
              type Value = [<$Prefix $Unit:camel >];

              fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                let expecting_str = stringify!(struct [<$Prefix $Unit:camel>]);
                formatter.write_str(expecting_str)
              }

              fn visit_map<V>(self, mut map: V) -> Result< [<$Prefix $Unit:camel >], V::Error>
                where V: serde::de::MapAccess<'de>
              {
                let mut inner = None;
                let mut ordering = None;
                let mut counting_behavior = None;
                while let Some(key) = map.next_key()? {
                  match key {
                    Field::Inner => {
                      if inner.is_some() {
                        return Err(serde::de::Error::duplicate_field("inner"));
                      }
                      let val: $Unit = map.next_value()?;
                      inner = Some(val.into())
                    },
                    Field::Ordering => {
                      if ordering.is_some() {
                        return Err(serde::de::Error::duplicate_field("ordering"));
                      }
                      let val: O = map.next_value()?;
                      ordering = Some(val.into())
                    },
                    Field::CountingBehavior => {
                      if counting_behavior.is_some() {
                        return Err(serde::de::Error::duplicate_field("counting_behavior"));
                      }
                      let val: BitFlags<CountingBehavior> = map.next_value()?;
                      counting_behavior = Some(val.into())
                    },
                  }
                }
                let ordering = ordering.ok_or_else(|| serde::de::Error::missing_field("ordering"))?;
                let inner = inner.ok_or_else(|| serde::de::Error::missing_field("inner"))?;
                let counting_behavior: BitFlags<CountingBehavior> = counting_behavior
                  .ok_or_else(|| serde::de::Error::missing_field("inner"))?;
                let conflicts = counting_behavior.get_behavior_conflicts();
                Ok([<$Prefix $Unit:camel >]{
                  ordering, 
                  inner, 
                  counting_behavior,
                  conflicts
                })
              }
            }
            const FIELDS: &'static [&'static str] = &["ordering", "inner"];
            deserializer.deserialize_struct(name_str, FIELDS, [<$Prefix $Unit:camel Visitor>])
          }
        }

        #[cfg(test)]
        mod test_serde{
          use super::*;
          #[test]
          fn serialize_and_deserialize() {
            use [<$Prefix $Unit:camel >] as C;
            // use $Unit as U;
            let c = C::new_from_offset(21);
            let ron_c = ron::to_string(&c).expect(stringify!(Must serialize [<$Prefix $Unit:camel >]));
            let d = ron::from_str(&ron_c).expect(stringify!(Must deserialize [<$Prefix $Unit:camel >]));
            assert_eq!(c, d, "Counter deserialization equals original serialized counter");
            // assert_eq!(c.get(), d.get(), "Counter deserialization equals original serialized counter");
          }
        }
      }

      #[doc = "PartialOrd only produces [cmp ordering](cmp::Ordering) when [atomic orderings](Ordering) are equal"]
      #[doc = r"```"]
      #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
      #[doc = "use core::sync::atomic::Ordering;"]
      #[doc = r#"let a = C::new_from_offset_with_ordering(32, Ordering::Relaxed);"# ]
      #[doc = r#"let b = C::new_from_offset_with_ordering(33, Ordering::Relaxed);"# ]
      #[doc = r#"assert!(a < b, "same-cmp::ordering counters must be ordered by when counts");"# ]
      #[doc = r#"assert!(b > a, "same-cmp::ordering counters must be ordered by when counts");"# ]
      #[doc = r#"let m = 20;"# ]
      #[doc = r#"(0..m).for_each(|_| { a.inc_one(); b.inc_one(); });"# ]
      #[doc = r#"assert!(a < b, "cmp::ordering preserved after counting same amount");"# ]
      #[doc = r#"assert!(b > a, "cmp::ordering preserved after counting same amount");"# ]
      impl cmp::PartialOrd for [<$Prefix $Unit:camel >] {
        fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
          self.ordering
            .eq(&other.ordering)
            .then(|| ())
            .and_then(|()| self.get().partial_cmp(&other.get()))
        }
      }

      impl cmp::Ord for [<$Prefix $Unit:camel >] {
        fn cmp(&self, other:&Self) -> cmp::Ordering {
          self.get_with_ordering(Self::DEFAULT_ORDERING)
            .cmp(&other.get_with_ordering(Self::DEFAULT_ORDERING))
        }
      }

      impl fmt::Display for [<$Prefix $Unit:camel>] {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
          write!(f, "{}", self.get())
        }
      }

      impl As<$Atomic> for [<$Prefix $Unit:camel>] {
        fn as_ref(&self) -> &$Atomic { &self.inner}
        fn as_mut(&mut self) -> &mut $Atomic { &mut self.inner}
      }
      impl As<Ordering> for [<$Prefix $Unit:camel>] {
        fn as_ref(&self) -> &Ordering { &self.ordering}
        fn as_mut(&mut self) -> &mut Ordering { &mut self.ordering}
      }
      impl AsRef<Ordering> for [<$Prefix $Unit:camel>] {
        fn as_ref(&self) -> &Ordering { <Self as As<Ordering>>::as_ref(self) }
      }

      impl Clone for [<$Prefix $Unit:camel>] {
        fn clone(&self) -> Self {
          Self::new_from_offset_with_ordering(self.get(), self.ordering)
        }
      }

      impl Default for [<$Prefix $Unit:camel>] {
        fn default() -> Self { Self::new() }
      }

      #[allow(non_snake_case)]
      mod [<eq_partial_eq_hash_ $Prefix $Unit:camel >] {
        use super::*;
        impl Eq for [<$Prefix $Unit:camel>] {}
        #[doc = "PartialEq is only equal when orderings and counting behaviors are equal"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use core::sync::atomic::Ordering;"]
        #[doc = r#"let a = C::new_from_offset_with_ordering(33, Ordering::Relaxed);"# ]
        #[doc = r#"let b = C::new_from_offset_with_ordering(33, Ordering::Relaxed);"# ]
        #[doc = r#"assert_eq!(a, b, "counters must be equal when counts and orderings are equal");"# ]
        #[doc = r#"assert_eq!(a, b, "counters must be equal when counts and orderings are equal");"# ]
        #[doc = r#"let m = 20;"# ]
        #[doc = r#"(0..m).for_each(|_| { a.inc_one(); b.inc_one(); });"# ]
        #[doc = r#"assert_eq!(a, b, "counters must be equal after counting same amount");"# ]
        #[doc = r#"assert_eq!(a, b, "counters must be equal after counting same amount");"# ]
        #[doc = r#"a.inc_one();"# ]
        #[doc = r#"assert_ne!(a, b, "counters must not be equal after counting different amounts");"# ]
        #[doc = r#"let c = C::new_from_offset_with_ordering(44, Ordering::Relaxed);"# ]
        #[doc = r#"let d = C::new_from_offset_with_ordering(44, Ordering::Release);"# ]
        #[doc = r#"assert_ne!(c, d, "ordering-inequal counters must not be equal with same count");"# ]
        impl PartialEq for [<$Prefix $Unit:camel>] {
          fn eq(&self, rhs: &Self) -> bool {
            self.ordering.eq(&rhs.ordering)
            && self.counting_behavior.eq(&rhs.counting_behavior)
            && self.get().eq(&rhs.get())
          }
        }

        impl Hash for [<$Prefix $Unit:camel >] {
          fn hash<H: Hasher>(&self, state: &mut H) {
            self.ordering.hash(state);
            self.counting_behavior.hash(state);
            self.get().hash(state);
          }
        }

        #[cfg(test)]
        mod hash_and_eq {
          extern crate std;
          use super::*;
          use std::collections::hash_map::DefaultHasher;
          #[test]
          fn hash_and_eq_property() {
            use [<$Prefix $Unit:camel >] as C;
            // use $Unit as U;
            let c = C::new_from_offset(21);
            let d = C::new_from_offset(21);
            assert_eq!(c, d, "Test counters must equal");
            let hasher_c = &mut DefaultHasher::new();
            c.hash(hasher_c);
            let hash_c = hasher_c.finish();
            let hasher_d = &mut DefaultHasher::new();
            d.hash(hasher_d);
            let hash_d = hasher_d.finish();
            assert_eq!(hash_c, hash_d, "When impelementing Hash and Eq, the property k1 == k2 -> hash(k1) == hash(k2) must hold");
          }
        }
      }

      impl From<$Unit> for [<$Prefix $Unit:camel>] {
        fn from(x: $Unit) -> Self {
          let counting_behavior = Self::DEFAULT_COUNTING_BEHAVIOR;
          let conflicts = counting_behavior.get_behavior_conflicts(); 
          Self{ 
            inner: $Atomic::new(x), 
            ordering: Self::DEFAULT_ORDERING,
            counting_behavior,
            conflicts 
          }  
        }
      }

      impl From<&[<$Prefix $Unit:camel>]> for $Unit {
        fn from(counter: &[<$Prefix $Unit:camel>]) -> Self { counter.get() }
      }
      
      impl HasCountingBehavior for [<$Prefix $Unit:camel>] {
        fn get_behavior_ref(&self) -> &BitFlags<CountingBehavior> { &self.counting_behavior }
        fn get_behavior_conflicts(&self) -> AllCountingBehaviorConflicts { self.conflicts }
      }

      impl [<$Prefix $Unit:camel>] {
        #[doc = "Largest [representable value](" $Unit "::MAX)"]
        pub const MAX: $Unit = $Unit::MAX;
        #[doc = "Smallest [representable value](" $Unit "::MIN)"]
        pub const MIN: $Unit = $Unit::MIN;
        /// Default [Atomic ordering](Ordering)
        pub const DEFAULT_ORDERING: Ordering = Ordering::SeqCst;
        /// Default [counting behavior](CountingBehavior) 
        pub const DEFAULT_COUNTING_BEHAVIOR: BitFlags<CountingBehavior> = BitFlags::<CountingBehavior>::from_bits_truncate_c(
          CountingBehavior::DEFAULT,
          BitFlags::CONST_TOKEN
        );
        /// Instantiate
        pub fn new() -> Self { 
          let counting_behavior = Self::DEFAULT_COUNTING_BEHAVIOR;
          let conflicts = counting_behavior.get_behavior_conflicts();
          Self { 
            inner: $Atomic::new(0), 
            ordering: Self::DEFAULT_ORDERING,
            counting_behavior,
            conflicts
          }
        }
        /// Instantiate with ordering
        pub fn new_with_ordering(ordering: Ordering) -> Self {
          let mut s = Self::new();
          s.ordering = ordering;
          s
        }
        /// Instantiate with offset value
        pub fn new_from_offset(offset: $Unit) -> Self {
          let mut s = Self::new();
          s.inner = $Atomic::new(offset);
          s
        }
        /// Instantiate with offset value and ordering
        pub fn new_from_offset_with_ordering(offset: $Unit, ordering: Ordering) -> Self {
          let mut s = Self::new_from_offset(offset);
          s.ordering = ordering;
          s
        }
        /// Set counting behavior 
        pub fn set_counting_behavior<B: Into<BitFlags<CountingBehavior>>>(
          &mut self, 
          counting_behavior: B
        ) {
          self.counting_behavior = counting_behavior.into();
          self.conflicts = self.counting_behavior.get_behavior_conflicts();
        }
        /// Instantiate with counting behaviors 
        pub fn new_with_counting_behavior<B: Into<BitFlags<CountingBehavior>>>(
          counting_behavior: B
        ) -> Self {
          let mut s = Self::new();
          s.set_counting_behavior(counting_behavior);
          s
        } 
        /// Instantiate with offset value and counting behavior
        pub fn new_from_offset_with_counting_behavior<B: Into<BitFlags<CountingBehavior>>>(
          offset: $Unit, 
          counting_behavior: B
        ) -> Self {
          let mut s = Self::new_from_offset(offset);
          s.set_counting_behavior(counting_behavior);
          s
        }


        /// Advance (by one) according to the current counting behavior
        ///
        /// - Returns the new value if applicable
        /// - Returns none if the counting behaviors [conflict](CountingBehavior::conflicts)
        /// - Returns none if no new value would be produced under current behavior   
        pub fn try_next(&self) -> Option<$Unit> {
          let current: $Unit = self.get();
          let new = if self.counting_behavior.contains(CountingBehavior::Increment) {
            self.inc_one();
            self.get()
          } else {
            self.dec_one();
            self.get()
          };
          (new != current).then(|| new)
        } 

        #[doc = "Get current value with the default [ordering](Ordering)"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use " $Unit " as U;"]
        #[doc = r#"let c = C::new();"# ]
        #[doc = r#"assert_eq!(c.get(), 0, "get returns initial value");"# ]
        #[doc = r#"c.inc_one();"# ]
        #[doc = r#"c.inc_one();"# ]
        #[doc = r#"c.inc_one();"# ]
        #[doc = r#"assert_eq!(c.get(), 3, "get returns post-increment value");"# ]
        pub fn get(&self) -> $Unit { self.inner.load(self.ordering) }
        /// Get current value with a specific [ordering](Ordering)
        pub fn get_with_ordering(&self, ordering: Ordering) -> $Unit { self.inner.load(ordering) }
        /// Convenience method for getting the current value as i128 
        pub fn get_i128(&self) -> i128 { self.get() as i128 }  
        #[doc = r#"Convert to some type that impls [From] "# $Unit r#"."#]
        pub fn to_x<X: From<$Unit>> (&self) -> X { X::from(self.get()) } 
      }

    }
  };
  (@Counting $Prefix:ident => 
    $Unit:ident | $Atomic:ident => 
    $CountingDesc:ident ($pro_op:ident/$anti_op:ident) 
    as $counting_prefix:ident using $counting_op:ident until $test_limit:ident
    or $cyclic_counting_op:ident
  ) => {
    paste!{
      impl [<$Prefix $Unit:camel>] {
        #[doc = $CountingDesc " by one "]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use core::ops::*; "]
        #[doc = "use enumflags2::{make_bitflags};"]
        #[doc = "use " $Unit " as U;"]
        #[doc = r#"let offset = U::MAX/2;"# ]
        #[doc = r#"let c = C::new_from_offset(offset);"# ]
        #[doc = r#"let m = 20;"# ]
        #[doc = r#"(0..m).for_each(|_| { c."# [<$counting_prefix _one>] r#"(); });"# ]
        #[doc = r#"assert_eq!(c.get(), (offset)."# $pro_op r#"(20), "counter must "# $CountingDesc r#"/"# $pro_op r#" number of times given as per sequential ordering");"# ]
        #[doc = r#"let d = C::new_from_offset(U::"# $test_limit r#");"# ]
        #[doc = r#"d."# [<$counting_prefix _one>] r#"();"# ]
        #[doc = r#"d."# [<$counting_prefix _one>] r#"();"# ]
        #[doc = r#"assert_eq!(d.get(), U::"# $test_limit r#", "counter must stop at "# $test_limit r#" ");"# ]
        pub fn [<$counting_prefix _one>](&self) { self.[<$counting_prefix _by_with_ordering>](1, self.ordering) }
     
        #[doc = $CountingDesc " by one with ordering"]
        pub fn [<$counting_prefix _one_with_ordering>](&self, ordering: Ordering) { self.[<$counting_prefix _by_with_ordering>](1, ordering) }
    
        #[doc = $CountingDesc " by specified amount"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use core::ops::*; "]
        #[doc = "use " $Unit " as U;"]
        #[doc = r#"let offset = U::MAX/2;"# ]
        #[doc = r#"let c = C::new_from_offset(offset);"# ]
        #[doc = r#"let m = 20;"# ]
        #[doc = r#"(0..m).for_each(|_| { c."# [<$counting_prefix _by>] r#"(2); });"# ]
        #[doc = r#"assert_eq!((c.get() as i128)."# $anti_op r#"((20*2) as i128), ((offset) as i128), "counter must "# $CountingDesc r#" by specified amount");"# ]
        pub fn [<$counting_prefix _by>](&self, amount: $Unit) { self.[<$counting_prefix _by_with_ordering>](amount, self.ordering); }
     
        #[doc = $CountingDesc " by specified amount with ordering"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use " $Unit " as U;"]
        #[doc = "use core::ops::*; "]
        #[doc = r#"let m = 3"# $Unit r#";"# ]
        #[doc = r#"let d = C::new_from_offset(U::"# $test_limit r#"."# $anti_op r#"(m * 2));"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"assert_eq!(d.get(), U::"# $test_limit r#", "counter must stop at "# $test_limit r#"");"# ]
        pub fn [<$counting_prefix _by_with_ordering>](&self, amount: $Unit, ordering: Ordering) {
          if self.[<is $counting_prefix _rementable>]() {
            let current = self.get_with_ordering(ordering);
            if self.counting_behavior.contains(CountingBehavior::Cyclic) {
              let _ = self.inner.swap(current.$cyclic_counting_op(amount), ordering);
            } else {
              let _ = self.inner.swap(current.$counting_op(amount), ordering);
            }
          }
        }
      
        #[doc = "Can the counter " $CountingDesc:lower " any further?"]
        #[doc = ""]
        #[doc = "- It halts " $CountingDesc:lower " ing at Self::"$test_limit ]
        #[doc = ""]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C, CountingBehavior as B };"]
        #[doc = "use " $Unit " as U;"]
        #[doc = "use core::ops::*; "]
        #[doc = r#"let m = 3"# $Unit r#";"# ]
        #[doc = r#"let offset = C::MAX/2;"# ]
        #[doc = r#"let d = C::new_from_offset_with_counting_behavior(offset, B::"# $CountingDesc r#");"# ]
        #[doc = r#"assert_eq!(d."# [< can_ $counting_prefix >] r#"(), true, "counter must detect when it can "# $CountingDesc r#"");"# ]
        #[doc = r#"let offset = C::"# $test_limit r#";"# ]
        #[doc = r#"let d = C::new_from_offset_with_counting_behavior(offset, B::"# $CountingDesc r#");"# ]
        #[doc = r#"assert_eq!(d."# [< can_ $counting_prefix >] r#"(), false, "counter must detect when it can no longer "# $CountingDesc r#"");"# ]
        pub fn [< can_ $counting_prefix >](&self) -> bool { 
          self.[<is $counting_prefix _rementable>]() 
          && self.[<is_within_ $counting_prefix _bound >]()     
        }

        #[doc = "Is the current index advanceable with the given operation type"]
        pub fn [<is $counting_prefix _rementable>](&self) -> bool {
          !self.counting_behavior.is_empty()
          && !self.conflicts.contains(&CountingBehaviorConflict::Always) 
          && ( 
            self.counting_behavior.contains(CountingBehavior::$CountingDesc)
            || self.counting_behavior.contains(CountingBehavior::Nonmonotonic)
          ) 
        } 

        #[doc = "Is it halted at the " $CountingDesc:lower " bound"]
        #[doc = ""]
        #[doc = "- **The bound is:** [Self::"$test_limit "]"]
        #[doc = "- The bound never applies in:** [acyclic mode](CountingBehavior)" ]
        pub fn [<is_within_ $counting_prefix _bound >](&self) -> bool {
          self.counting_behavior.contains(CountingBehavior::Acyclic)
          || self.get() != Self::$test_limit
        } 

        #[doc = "Combine the " $CountingDesc:lower " (by one) and get operations" ]
        #[doc = ""]
        #[doc = "Returns the value **before** the " $CountingDesc:lower " operation"]
        pub fn [<get_and_ $counting_prefix _one>](&self,) -> $Unit {
          self.[<get_and_ $counting_prefix _by>](1)
        }
        
        #[doc = "Combine the " $CountingDesc:lower " (by the given amount) and get operations" ]
        #[doc = ""]
        #[doc = "Returns the value **before** the " $CountingDesc:lower "operation"]
        pub fn [<get_and_ $counting_prefix _by>](&self, amount: $Unit) -> $Unit {
          self.[<get_and_ $counting_prefix _by_with_ordering>](amount, self.ordering)
        }
        
        #[doc = "Combine the " $CountingDesc:lower " (by the given amount) and get operations" ]
        #[doc = ""]
        #[doc = "Returns the value **before** the " $CountingDesc:lower " operation"]
        pub fn [<get_and_ $counting_prefix _by_with_ordering>](&self, amount: $Unit, ordering: Ordering) -> $Unit {
          let u = self.get();
          self.[<$counting_prefix _by_with_ordering>](amount, ordering);
          u
        }
      }
    }
  };
  (@Op $Prefix:ident => $Unit:ident op $Op:ty : $op_fn:ident ) => {
    paste!{
      impl $Op for [<$Prefix $Unit:camel>] {
        type Output = Self; 
        #[doc = r" - This operation is implemented with saturating arithmetic"]
        #[doc = r" - **This operation IGNORES dissimilar [atomic orderings](Ordering)!**"]
        fn $op_fn(self, rhs: Self) -> Self::Output {
          Self::new_from_offset(self.get().[<saturating_ $op_fn>](rhs.get()))
        }
      }
    }
  };
  (@Ops $Prefix:ident => $Unit:ident => [
    $($Op:ty : $op_fn:ident;)+
  ] ) => {
    $(make_counter!{@Op $Prefix => $Unit op $Op : $op_fn})+
  };
  ($Prefix:ident => [$($Unit:ident | $Atomic:ident, )+]) => {
    $(make_counter!{@Main $Prefix => $Unit | $Atomic})+
    $(make_counter!{@Counting $Prefix => $Unit | $Atomic => Increment (add/sub) as inc using saturating_add until MAX or wrapping_add })+
    $(make_counter!{@Counting $Prefix => $Unit | $Atomic => Decrement (sub/add) as dec using saturating_sub until MIN or wrapping_sub })+
    $(make_counter!{@Ops $Prefix => $Unit => [
      ops::Add : add;
      ops::Sub : sub;
      ops::Mul : mul;
      ops::Div : div;
    ]})+
  }
}

make_counter! {Counter => [
  u8 | AtomicU8,
  u16 | AtomicU16,
  u32 | AtomicU32,
  u64 | AtomicU64,
  // u128 | AtomicU128,
  usize | AtomicUsize,

  i8 | AtomicI8,
  i16 | AtomicI16,
  i32 | AtomicI32,
  i64 | AtomicI64,
  // i128 | AtomicI128,
  isize | AtomicIsize,
]}


#[cfg(test)]
mod general{
  use super::*;
  use enumflags2::make_bitflags;

  #[test]
  fn nonmonotonic() {
    // The same as the doc tests but more readable and only for one type
    let c = CounterI8::new_with_counting_behavior(make_bitflags!(CountingBehavior::{Nonmonotonic}));
    (0..100).for_each(|_| c.inc_one() );
    assert_eq!(c.get(), 100);
    (0..100).for_each(|_| c.dec_one() );
    assert_eq!(c.get(), 0);
    (0..100).for_each(|_| c.inc_by(2) );
    assert_eq!(c.get(), i8::MAX);
    (0..100).for_each(|_| c.dec_by(5) );
    assert_eq!(c.get(), i8::MIN); 
  }

  #[test]
  fn get_and() {
    let c = CounterI32::new_from_offset_with_counting_behavior(33, make_bitflags!(CountingBehavior::{Decrement}));
    assert_eq!(c.get_and_dec_by(34), 33, "get_and...method must return starting value");
    assert_eq!(c.get(), -1, "counter must be set to new value following get_and... method")
  }

  #[test]
  fn conflicting_behaviors() {
    let c0 = CounterI16::new_with_counting_behavior(
      make_bitflags!(CountingBehavior::{Monotonic | Nonmonotonic})
    );
    assert!(
      c0.get_behavior_conflicts().contains(&CountingBehaviorConflict::Always), 
      "Must detect conflicting monotonic/nonmonotonic counting behaviors"
    );
    assert_eq!(c0.try_next(), None, "must not advance with conflicting counting behaviors");
    let c1 = CounterI16::new_with_counting_behavior(make_bitflags!(CountingBehavior::{Increment | Decrement}));
    assert!(
      c1.get_behavior_conflicts().contains(&CountingBehaviorConflict::Always), 
      "Must detect conflicting counting increment/decrement behaviors"
    );

    let c2 = CounterI16::new_with_counting_behavior(
      make_bitflags!(CountingBehavior::{Acyclic | Cyclic})
    );
    assert!(
      c2.get_behavior_conflicts().contains(&CountingBehaviorConflict::Overflowing), 
      "Must detect conflicting counting acyclic/cyclic behaviors"
    );
   
    let c3 = CounterI16::new_with_counting_behavior(make_bitflags!(CountingBehavior::{Monotonic | Increment}));
    assert!(
      !c3.get_behavior_conflicts().contains(&CountingBehaviorConflict::Always), 
      "Must ignore nonconflicting counting behaviors"
    );
  }


  #[test]
  fn cyclic() {
    // The same as the doc tests but more readable and only for one type
    let c = CounterU8::new_with_counting_behavior(
      make_bitflags!(
        CountingBehavior::{Monotonic | Increment | Cyclic}
      )
    );
    let m = 100u8;
    (0..m).for_each(|_| c.inc_one() );
    assert_eq!(c.get(), m, "must increment specified amount");
    (0..m).for_each(|_| c.dec_one() );
    assert_eq!(c.get(), m, "must remain monotonic non-decreasing");
    (0..(u8::MAX-m)).for_each(|_| c.inc_by(1) );
    assert_eq!(c.get(), u8::MAX, "must increment to maximum");
    (0..(m+1)).for_each(|_| c.inc_one() );
    assert_eq!(c.get(), m, "must cycle around"); 
  }

}