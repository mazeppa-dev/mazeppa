# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

## 0.3.4 - 2024-10-12

### Changed

 - Reuse predefined thunks for variables standing as constructor arguments.
   - This improves output code size and compilation times when C code generation is used.

## 0.3.3 - 2024-10-03

### Fixed

  - Tweak the dependencies in `dune-project` after [trying to publish] the package.

[trying to publish]: https://github.com/ocaml/opam-repository/pull/26657

## 0.3.2 - 2024-10-02

### Added

 - Expose the `Gensym` module in the public API (previously private).

## 0.3.1 - 2024-09-23

### Added

 - The Mazeppa-to-C translator (with GNU11 extensions) via the new `translate` CLI command (issue https://github.com/mazeppa-dev/mazeppa/issues/1).
 - The `translate_to_c` and `mazeppa_h` API functions.

## 0.3.0 - 2024-08-16

### Added

 - The `--print-gc-stats` flag for `run` and `eval` to observe total memory usage.
 - Expose the `check` function in the public API.

### Changed

 - Optimize homeomorphic embedding by maintaining a local result cache (issue https://github.com/mazeppa-dev/mazeppa/issues/20).
 - Other homeomorphic embedding performance tweaks (issue https://github.com/mazeppa-dev/mazeppa/issues/17).
   - In particular, utilize `Weak` hash consing while building function bodies.
 - Rename the functions 1) `Symbol.kind` to `op_kind`, 2) `Symbol.is_lazy` to `is_lazy_op` for clarity [**BC**].
 - Hide the middles of long reduction paths with `(N more...)` (issue https://github.com/mazeppa-dev/mazeppa/issues/21).

### Fixed

 - Evaluation of built-in panics raising an exception (issue https://github.com/mazeppa-dev/mazeppa/issues/22).

### Removed

 - The function `Raw_term.is_immediate` is no longer public [**BC**].

## 0.2.0 - 2024-08-03

### Added

 - Propagate positive information for inequality (`!=`) tests (issue https://github.com/mazeppa-dev/mazeppa/issues/7).
 - More simplification rules:
   - _op(op(t)) -> op(t)_, where _op_ is one of `u8`, `u16`, `u32`, `u64`, `u128`, `i8`, `i16`, `i32`, `i64`, `i128`, `string`.
   - _/(0, t) -> 0_
   - _|(t, all ones), |(all ones, t) -> all ones_
   - _&(t, all ones), &(all ones, t) -> t_
 - Add the `check` command to the CLI for checking program well-formedness (issue https://github.com/mazeppa-dev/mazeppa/issues/18).
 - Expose the `Symbol.list` function in the public API.

### Changed

 - Only whistle on terms with equal redex signatures (issues https://github.com/mazeppa-dev/mazeppa/issues/9, https://github.com/mazeppa-dev/mazeppa/issues/11).
 - Only whistle on equal integers (issue https://github.com/mazeppa-dev/mazeppa/issues/12).
 - Rebuild as less terms as possible when substituting contractions (issue https://github.com/mazeppa-dev/mazeppa/issues/14).
 - Optimize homeomorphic embedding by storing term sizes (issue https://github.com/mazeppa-dev/mazeppa/issues/17).

### Fixed

 - Internal compiler errors when analyzing `T`/`F`-patterns during driving. Instead, show proper error messages (issue https://github.com/mazeppa-dev/mazeppa/issues/6).
 - Incorrect exception backtraces (issue https://github.com/mazeppa-dev/mazeppa/issues/8).
 - Require constructors to be used consistently [**BC**] (issue https://github.com/mazeppa-dev/mazeppa/issues/10).

## 0.1.2 - 2024-07-20

### Added

 - Function productivity analysis for preventing unneeded specializations (issue https://github.com/mazeppa-dev/mazeppa/issues/2).
 - `pattern_to_string` and `pattern_verbatim` (`Raw_term`).

### Changed

 - Simplify horizontal configuration analysis by only checking for renamings instead of matching arbitrary safe instances.
 - Do not emit 1) linear let-bindings for redexes, 2) unused let-bindings for immediate terms.

## 0.1.1 - 2024-07-15

### Added

 - `mazeppa eval`: A built-in evaluator for the Mazeppa language (issue https://github.com/mazeppa-dev/mazeppa/issues/5).
 - Expose the `Const` module in the public API.

### Changed

 - The following simplification rules have been upgraded to handle arbitrary values instead of variables:
   - _*(x, 0), *(0, x) -> 0_
   - _&(x, 0), &(0, x) -> 0_
   - _%(x, 1) -> 0_
   - _/(x, 0), %(x, 0) -> out of range_
   - _|(x, x), &(x, x) -> x_
   - _=(x, x), >=(x, x), <=(x, x) -> T()_
   - _!=(x, x), >(x, x), <(x, x) -> F()_

## 0.1.0 - 2024-07-11

### Added

 - The Mazeppa supercompiler.
