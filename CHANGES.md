# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Added
  - edo features:
    * add new cryptographic hash functions: `sha3` and `keccak`
    * add `total_voting_power` constant and `voting_power` operation
    * add `level` constant
    * add `ticket` type and associated instructions (`create_ticket`, `read_ticket`, `split_ticket`, `join_tickets`)
    * add sapling types (`sapling_state` and `sapling_transaction`) and operations (`sapling_empty_state` and `sapling_verify_update`)
    * add bls curves types (`bls12_381_fr`, `bls12_381_g1` and `bls12_381_g2`), arthmetic (`+`, `*`, unary `-`) and operation (`pairing_check`)

## Unreleased
### Added
  - `contract_address` (contract -> address) and `key_address` (key -> address)
  - `require_some`, same as `opt_get` with an extra arg for custom failed
  - `sub_nat` substraction which returns `nat`
  - `require_entrypoint`, same as `require_some(entrypoint<...>(...))`
  - michelson optimizations
### Changed
  - entrypoints are generated with a complete binary tree.

## [1.2.7] - 2021-08-11
### Added
  - npm package for archetype compiler (https://www.npmjs.com/package/@completium/archetype)
  - support `tez` type for `sum` method
### Changed
  - default call contract (`transfer 0tz to addr call default<nat>(2)`)
  - change type of string in mlw archetype library

## [1.2.6] - 2021-06-24
### Added
  - test mode

## [1.2.5] - 2021-05-08
### Added
  - `sourced by`, same as `called by` with source
### Changed
  - minor fixes

## [1.2.4] - 2021-05-03
### Added
  - `update` function for map (like `UPDATE` in michelson)
  - `state is` section in entry
  - `called by` supports asset with a key typed address
  - failure id in fails specification section (i.e. `... f1 with InvalidCaller(msg : string): ...`)
  - metadata can be defined in the source file (`with metadata`)
  - instruction for container (`my_map.update(key, some(value))`)

### Changed
  - `slice` returns `option<byte|string>`
  - fail message and expression (see documentation)
  - syntax `enum` declaration with args (`enum my_enum = | A <nat> | B <string>`)

### Removed
  - `role` type

## [1.2.3] - 2021-04-12
### Added
  - `date_from_timestamp` function (convert a timestamp typed int to date)
  - `const` for parameter, which processes like a constant instead of a variable by default
### Changed
  - Improve js ouptut

## [1.2.2] - 2021-03-09
### Added
 - records shaping with `as`
 - add `parameter`
 - add left and right shift operators (`<<|` and `|>>`)
 - add divmod and three-way comparison operators (resp. EDIV and COMPARE in michelson)
 - add `Unit` literal

### Changed
 - handle decimal for percent and currency literals
 - getter send `transferred` (AMOUNT) instead of 0tz

### Removed
 - Execution language target (LIGO, SmartPy and Scaml)
 - `head_tail` (replace by `match ... with ... end`)

## [1.2.1] - 2020-10-01
### Added
 - Add contract metadata tzip-16 (https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md)

## [1.2.0] - 2020-09-15
### Added
 - Michelson backend
 - specification declaration for asset, variable, entry and function
 - `fails` section in specification declaration
 - `xor` operator
 - `head_tail` and `reverse` builtin functions for list
 - `getter` entry

### Changed
 - remove `set_` and `map_` prefix for container builtins
 - replace `entrysig` by `contract`

## [1.1.2] - 2020-08-13
### Changed
 - Refactoring of whyml generation

## [1.1.1] - 2020-08-11
### Added
 - in `require` and `failif` section, add custom failed with respectivelly `otherwise` and `with`
 - `to_string` convert argument to a string (only avalaible for `nat` type for now)

### Changed
 - `fail` can take any type argument
 - `dorequire` and `dofailif` take two arguments, the second is for `fail`

## [1.1.0] - 2020-08-05
### Added
 - `chain_id` constant
 - `union` `inter` and `diff` operator for view in formula
 - multi-keys asset : `identified by` can take several field name
 - `nat` type (comparison and arithmetic operations)
 - support for entrypoints with `entrysig` type, `entrypoint` function and `transfer` with `entrysig` and `self`
 - `map` and `set` containers
 - `record` structure

### Removed
 - `contract` declaration

### Changed
 - syntax of composite type (i.e. `list<string>` instead of `string list`)
 - `rational` are mapped to `int * nat`

### Fixed
 - `caller` as asset key in `initialized by` section

## [1.0.0] - 2020-06-27
### Added
 - Introduce `container_kind` in model
 - `pack`, `unpack` functions
 - `removeif` for collection, aggregate and partition

### Changed
 - Replace `collection` by `aggregate`
 - Replace `list` by `set` container for asset container fields (Aggregate | Partition).
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
