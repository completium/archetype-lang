# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
 - Contract calls
 - Handle rational type
 - Handle date and duration type
 - Add list container
 - Add print-type-contract command (-ptc)

### Changed
 - Add guard condition in `Add` and `UpdateAdd` api storage
 - Syntax : add identifier on signature of contract argument
 - Refactor transition

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
