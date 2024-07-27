# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Added

 - Propagate positive information for inequality (`!=`) tests ([issue #7](https://github.com/mazeppa-dev/mazeppa/issues/7)).
 - Simplify _string(string(t))_ into _string(t)_.

### Fixed

 - Internal compiler errors when analyzing `T`/`F`-patterns during driving. Instead, show proper error messages ([issue #6](https://github.com/mazeppa-dev/mazeppa/issues/6)).
 - Incorrect exception backtraces ([issue #8](https://github.com/mazeppa-dev/mazeppa/issues/8)).
 - Require constructor symbols to be called and matched with the same number of arguments ([issue #10](https://github.com/mazeppa-dev/mazeppa/issues/10)).

### Changed

 - Avoid some over-generalizations by whistling on terms with the same redex operator ([issue #9](https://github.com/mazeppa-dev/mazeppa/issues/9)).

## 0.1.2 - 2024-07-20

### Added

 - Function productivity analysis for preventing unneeded specializations ([issue #2](https://github.com/mazeppa-dev/mazeppa/issues/2)).
 - `pattern_to_string` and `pattern_verbatim` (`Raw_term`).

### Changed

 - Simplify horizontal configuration analysis by only checking for renamings instead of matching arbitrary safe instances.
 - Do not emit 1) linear let-bindings for redexes, 2) unused let-bindings for immediate terms.

## 0.1.1 - 2024-07-15

### Added

 - `mazeppa eval`: A built-in evaluator for the Mazeppa language ([issue #5](https://github.com/mazeppa-dev/mazeppa/issues/5)).
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
