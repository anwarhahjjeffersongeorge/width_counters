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
      #[doc = "5. Unlike the underlying [" $Atomic "], this will not wrap on overflow.  "]
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
            enum Field { Inner, Ordering }

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
                  }
                }
                let ordering = ordering.ok_or_else(|| serde::de::Error::missing_field("ordering"))?;
                let inner = inner.ok_or_else(|| serde::de::Error::missing_field("inner"))?;
                Ok([<$Prefix $Unit:camel >]{ordering, inner})
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
        #[doc = "PartialEq is only equal when orderings are equal"]
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
            && self.get().eq(&rhs.get())
          }
        }

                impl Hash for [<$Prefix $Unit:camel >] {
          fn hash<H: Hasher>(&self, state: &mut H) {
            self.ordering.hash(state);
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
        fn from(x: $Unit) -> Self { Self{ inner: $Atomic::new(x), ordering: Self::DEFAULT_ORDERING }  }
      }

      impl From<&[<$Prefix $Unit:camel>]> for $Unit {
        fn from(counter: &[<$Prefix $Unit:camel>]) -> Self { counter.get() }
      }
      
      impl [<$Prefix $Unit:camel>] {
        /// New inner
        #[allow(dead_code)]
        const fn new_inner(u: $Unit) -> $Atomic { $Atomic::new(u) }
        #[doc = "Largest [representable value](" $Unit "::MAX)"]
        pub const MAX: $Unit = $Unit::MAX;
        #[doc = "Smallest [representable value](" $Unit "::MIN)"]
        pub const MIN: $Unit = $Unit::MIN;
        /// Default [Atomic ordering](Ordering)
        pub const DEFAULT_ORDERING: Ordering = Ordering::SeqCst;
        /// Instantiate
        pub const fn new() -> Self { Self { inner: $Atomic::new(0), ordering: Self::DEFAULT_ORDERING }}
        /// Instantiate with ordering
        pub const fn new_with_ordering(ordering: Ordering) -> Self {
          let mut s = Self::new();
          s.ordering = ordering;
          s
        }
        /// Instantiate with offset value
        pub const fn new_from_offset(offset: $Unit) -> Self {
          let mut s = Self::new();
          s.inner = $Atomic::new(offset);
          s
        }
        /// Instantiate with offset value and ordering
        pub const fn new_from_offset_with_ordering(offset: $Unit, ordering: Ordering) -> Self {
          let mut s = Self::new_from_offset(offset);
          s.ordering = ordering;
          s
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
  (@Counting $Prefix:ident => $Unit:ident | $Atomic:ident => $counting_desc:literal($pro_op:ident/$anti_op:ident) as $counting_prefix:ident using $counting_op:ident until $test_limit:ident) => {
    paste!{
      impl [<$Prefix $Unit:camel>] {
        #[doc = $counting_desc " by one "]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use core::ops::*; "]
        #[doc = "use " $Unit " as U;"]
        #[doc = r#"let offset = U::MAX/2;"# ]
        #[doc = r#"let c = C::new_from_offset(offset);"# ]
        #[doc = r#"let m = 20;"# ]
        #[doc = r#"(0..m).for_each(|_| { c."# [<$counting_prefix _one>] r#"(); });"# ]
        #[doc = r#"assert_eq!(c.get(), (offset)."# $pro_op r#"(20), "counter must "# $counting_desc r#"/"# $pro_op r#" number of times given as per sequential ordering");"# ]
        #[doc = r#"let d = C::new_from_offset(U::"# $test_limit r#");"# ]
        #[doc = r#"d."# [<$counting_prefix _one>] r#"();"# ]
        #[doc = r#"d."# [<$counting_prefix _one>] r#"();"# ]
        #[doc = r#"assert_eq!(d.get(), U::"# $test_limit r#", "counter must stop at "# $test_limit r#" ");"# ]
        pub fn [<$counting_prefix _one>](&self) { self.[<$counting_prefix _by_with_ordering>](1, self.ordering) }
        #[doc = $counting_desc " by one with ordering"]
        pub fn [<$counting_prefix _one_with_ordering>](&self, ordering: Ordering) { self.[<$counting_prefix _by_with_ordering>](1, ordering) }
        #[doc = $counting_desc " by specified amount"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use core::ops::*; "]
        #[doc = "use " $Unit " as U;"]
        #[doc = r#"let offset = U::MAX/2;"# ]
        #[doc = r#"let c = C::new_from_offset(offset);"# ]
        #[doc = r#"let m = 20;"# ]
        #[doc = r#"(0..m).for_each(|_| { c."# [<$counting_prefix _by>] r#"(2); });"# ]
        #[doc = r#"assert_eq!((c.get() as i128)."# $anti_op r#"((20*2) as i128), ((offset) as i128), "counter must "# $counting_desc r#" by specified amount");"# ]
        pub fn [<$counting_prefix _by>](&self, amount: $Unit) { self.[<$counting_prefix _by_with_ordering>](amount, self.ordering); }
        #[doc = $counting_desc " by specified amount with ordering"]
        #[doc = r"```"]
        #[doc = "use width_counters::{" [<$Prefix $Unit:camel>] " as C };"]
        #[doc = "use " $Unit " as U;"]
        #[doc = "use core::ops::*; "]
        #[doc = r#"let m = 3"# $Unit r#";"# ]
        #[doc = r#"let offset = U::MAX/2;"# ]
        #[doc = r#"let d = C::new_from_offset(U::"# $test_limit r#"."# $anti_op r#"(m * 2));"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"d."# [<$counting_prefix _by>] r#"(m);"# ]
        #[doc = r#"assert_eq!(d.get(), U::"# $test_limit r#", "counter must stop at "# $test_limit r#"");"# ]
        pub fn [<$counting_prefix _by_with_ordering>](&self, amount: $Unit, ordering: Ordering) {
          let current = self.get_with_ordering(ordering);
          let _ = self.inner.swap(current.$counting_op(amount), ordering);
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
    $(make_counter!{@Counting $Prefix => $Unit | $Atomic => "Increment"(add/sub) as inc using saturating_add until MAX})+
    $(make_counter!{@Counting $Prefix => $Unit | $Atomic => "Decrement"(sub/add) as dec using saturating_sub until MIN})+
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
mod clarity{
  use crate::CounterI8;

use super::*;
  #[test]
  fn all_behaviours() {
    // The same as the doc tests but more readable and only for one type
    let c = CounterI8::new();
    (0..100).for_each(|_| c.inc_one() );
    assert_eq!(c.get(), 100);
    (0..100).for_each(|_| c.dec_one() );
    assert_eq!(c.get(), 0);
    (0..100).for_each(|_| c.inc_by(2) );
    assert_eq!(c.get(), i8::MAX);
    (0..100).for_each(|_| c.dec_by(5) );
    assert_eq!(c.get(), i8::MIN);
    
  }
}