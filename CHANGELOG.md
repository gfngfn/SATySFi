# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Rename math command `\centerdot` to `\cdot` ([PR\#114](https://github.com/gfngfn/SATySFi/pull/114) by `nekketsuuu`).
- Does not fail when a script value evaluates to `OtherScript` ([PR\#121](https://github.com/gfngfn/SATySFi/pull/121) by `na4zagin3`).
- Handles `CR`+`LF` correctly as one line ending ([PR\#122](https://github.com/gfngfn/SATySFi/pull/122) by `matsud224`).
- Fix how to generate ToUnicodeCMap as to ligatures ([PR\#140](https://github.com/gfngfn/SATySFi/pull/140) by `matsud224`).
- Fix bug of `\ref` in `stdjareport` ([PR\#135](https://github.com/gfngfn/SATySFi/pull/135) by `matsud224`).
- Fix bug of signature matching in the type checker ([PR\#143](https://github.com/gfngfn/SATySFi/pull/143) and [PR\#144](https://github.com/gfngfn/SATySFi/pull/144) by `elpinal`).
- Fix bug of type inference about records ([PR\#148](https://github.com/gfngfn/SATySFi/pull/148) by `elpinal`).

## Added
- Begins to support Markdown input.
- Supports PDF hyperlinks ([PR\#113](https://github.com/gfngfn/SATySFi/pull/113) by `matsud224`).
- Supports fixed-length list patterns ([PR\#123](https://github.com/gfngfn/SATySFi/pull/123) by `nekketsuuu`).
- Supports PDF outlines ([PR\#134](https://github.com/gfngfn/SATySFi/pull/134) by `matsud224`).
- Reports detailed error messages when a given image file is invalid ([PR\#138](https://github.com/gfngfn/SATySFi/pull/138) by `matsud224`).
- Provides new primitives `get-leftmost-script : inline-boxes -> script option`, `get-rightmost-script : inline-boxes -> script option`, and `script-guard-both : script -> script -> inline-boxes -> inline-boxes`.
- Regards `<LIBROOT>/local/packages/` as a place for user-defined packages.
- Uses `<LIBROOT>/local/hash/fonts.satysfi-hash` (as well as `<LIBROOT>/dist/fonts.satysfi-hash`) for a font fash file (the same holds for `<LIBROOT>/local/hash/mathfonts.satysfi-hash`).
- Supports (non-extensible) record updates, i.e., `(| <record> with <label> = <new-value> |)`.

## Deprecated

- The use of `"src-dist":` entries in font hash files is deprecated; use `"src":` instead, which requires a font file’s path relative to `<LIBROOT>`. An entry of the form `"src-dist": "<path/to/font-file>"` is now treated equivalently to `"src": "dist/fonts/<path/to/font-file>"`.

## [0.0.3] - 2018-10-09
### Fixed
- Does NOT insert spacing between different scripts when the [line breaking class](http://unicode.org/reports/tr14/) of the posterior charcter is CL, CP, QU, NS, JLCP (= [JLreq cl-02](https://www.w3.org/TR/jlreq/ja/#cl-02)), JLFS (= [JLreq cl-06](https://www.w3.org/TR/jlreq/ja/#cl-06)), or JLCM (= [JLreq cl-07](https://www.w3.org/TR/jlreq/ja/#cl-07)).

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
