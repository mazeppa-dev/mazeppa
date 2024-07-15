# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

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
