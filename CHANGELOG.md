# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Does not fail when a script value evaluates to `OtherScript` ([PR\#121](https://github.com/gfngfn/SATySFi/pull/121)).

## [0.0.3] - 2018-10-09
### Fixed
- Does NOT insert spacing between different scripts when the [line breaking class](http://unicode.org/reports/tr14/) of the posterior charcter is CL, CP, QU, NS, JLCP, JLNS, JLCM, or JLFS.

### Added
- Supports the application of math commands to optional arguments.
- Provides primitives `set-space-ratio-between-scripts` and `get-space-ratio-between-scripts`.

## [0.0.2] - 2018-08-09
### Fixed
- Make type inference algorithm firmer (mainly about records and optional arguments).

### Changed
- Conforms to a new operational semantics as to optional arguments.
- Starts reporting error for duplicated fields in a record expression.
- Improve type error report as to application of non-function expressions.

## 0.0.1 - 2018-08-05
### Added
- Initial version of SATySFi


  [Unreleased]: https://github.com/gfngfn/SATySFi/compare/v0.0.3...HEAD
  [0.0.3]: https://github.com/gfngfn/SATySFi/compare/v0.0.2...v0.0.3
  [0.0.2]: https://github.com/gfngfn/SATySFi/compare/v0.0.1...v0.0.2
