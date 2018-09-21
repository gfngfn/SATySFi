# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Does NOT insert spacing between different scripts when the line breaking class of the posterior charcter is CL, CP, QU, NS, JLCP, JLNS, JLCM, or JLFS

# Changed
- Supports the application of math commands to optional arguments

## [0.0.2] - 2018-08-09
### Fixed
- Make type inference algorithm firmer (mainly about records and optional arguments).

### Changed
- Conforms to a new operational semantics as to optional arguments.
- Starts reporting error for duplicated fields in a record expression.
- Improve type error report as to application of non-function expressions.

## [0.0.1] - 2018-08-05
### Added
- Initial version of SATySFi
