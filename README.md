<!-- cargo-sync-rdme title [[ -->
# width_counters
<!-- cargo-sync-rdme ]] -->

#### Usability:
<!-- cargo-sync-rdme badge:usability [[ -->
![License: MIT OR Apache-2.0](https://img.shields.io/crates/l/width_counters.svg?style=for-the-badge)
[![crates.io](https://img.shields.io/crates/v/width_counters.svg?logo=rust&style=for-the-badge)](https://crates.io/crates/width_counters)
[![docs.rs](https://img.shields.io/docsrs/width_counters.svg?logo=docs.rs&style=for-the-badge)](https://docs.rs/width_counters)
<!-- cargo-sync-rdme ]] -->
#### Development:
<!-- cargo-sync-rdme badge:development [[ -->
[![Maintenance: actively-developed](https://img.shields.io/badge/maintenance-actively--developed-brightgreen.svg?style=for-the-badge)](https://doc.rust-lang.org/cargo/reference/manifest.html#the-badges-section)
[![GitHub Actions: CI](https://img.shields.io/github/actions/workflow/status/anwarhahjjeffersongeorge/width_counters/ci.yml.svg?label=CI&logo=github&style=for-the-badge)](https://github.com/anwarhahjjeffersongeorge/width_counters/actions/workflows/ci.yml)
<!-- cargo-sync-rdme ]] -->

---
## Overview
<!-- cargo-sync-rdme rustdoc [[ -->
Atomic, thread-safe counters of differing integer widths

#### Comes with

These counters support

* Incrementing/decrementing by default (1) or specified amounts,
* Using per-operation [atomic orderings](https://doc.rust-lang.org/nightly/core/sync/atomic/enum.Ordering.html), ([see this also](https://en.cppreference.com/w/c/atomic/memory_order))
* Const instantiation with default offset (0) and default atomic ordering ( sequentially consistent ),
* Const instantiation with custom offset, custom ordering or both, and
* [PartialEq](https://doc.rust-lang.org/nightly/core/cmp/trait.PartialEq.html), [Eq](https://doc.rust-lang.org/nightly/core/cmp/trait.Eq.html), [Hash](https://doc.rust-lang.org/nightly/core/hash/trait.Hash.html), [PartialOrd](https://doc.rust-lang.org/nightly/core/cmp/trait.PartialOrd.html), [Ord](https://doc.rust-lang.org/nightly/core/cmp/trait.Ord.html), [Clone](https://doc.rust-lang.org/nightly/core/clone/trait.Clone.html), [Debug](https://doc.rust-lang.org/nightly/core/fmt/trait.Debug.html), [Display](https://doc.rust-lang.org/nightly/core/fmt/trait.Display.html)
* [Send](https://doc.rust-lang.org/nightly/core/marker/trait.Send.html), [Sync](https://doc.rust-lang.org/nightly/core/marker/trait.Sync.html)

#### Optional features

* `serde`: Enable de/serialization
<!-- cargo-sync-rdme ]] -->
