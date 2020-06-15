# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
 - Introduce `container_kind` in model
 - `pack`, `unpack` functions

### Changed
 - Replace `collection` by `subset`
 - Replace `list` by `set` container for asset container fields (Subset | Partition).
 - Rename `action` by `entry`

## [0.1.14] - 2020-04-25
### Added
 - Crypto functions
 - `slice`, `concat` and `length` functions
 - `isnone`, `issome`, `getopt` option functions
 - `floor` and `ceil` functions
 - add percent literal

### Update
 - Remove key asset for execution generation
 - Mlw support for new view type

## [0.1.13] - 2020-03-05
### Added
 - Contract calls
 - Handle rational type
 - Handle date and duration type
 - Add list container
 - Add print-type-contract command (-ptc)
 - Add `addupdate` method for asset
 - Add `bytes` type

### Changed
 - Add guard condition in `Add` and `UpdateAdd` api storage
 - Syntax : add identifier on signature of contract argument
 - Refactor and update syntax for transition
 - Verification api for asset
 - Syntax : extension arguments are enclosed by parentheses

## [0.1.12] - 2020-01-09
### Added
 - Add Scaml output

### Changed
 - Syntax : add ':' between identifier and type in declaration `id : type` (instead of `id1 type1`)
 - Syntax : add ',' between arguments in funciton `(id1 : type1, id2 : type2)` (instead of `(id1 : type1) (id2 : type2)`)
 - Accept transfer is enable by default (use `refuse transfer` to disable it)

## [0.1.11] - 2019-12-12
### Added
 - Invariants on constants and variables
 - Date and duration support
 - Shadow fields in asset

### Changed
 - Improve loop generation in ligo output (issue #107)
 - Refactoring storage in model module
 - Enchance ligo output printer
 - Remove keys list of assets in order to decrease storage size


## [0.1.10] - 2019-11-02
### Fixed
 - Fix why3 generation


## [0.1.8] - 2019-10-25
### Added
 - Add variable declaration `var id = val`
 - Typing for transition entry

### Changed
 - Update syntax for for and iter loop `for : loop i in col do ... done`

### Removed
 - Remove namespace syntax

### Fixed
 - Improve LIGO generation


## [0.1.6] - 2019-10-15
### Added
 - Add focus property command for vscode extension

### Changed
 - Update syntax for security predicate arguments


## [0.1.5] - 2019-09-28
### Added
 - Create a share directory for contracts and extensions

### Changed
 - Update of the archetype syntax
 - Fix mlw generation

## [0.1.4] - 2019-09-21
### Added
 - Initial release of Archetype compiler.
 - The main example is miles_with_expiration.arl contract.
 - For more informations: https://docs.archetype-lang.org
