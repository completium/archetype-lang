# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.5.1] - 2024-01-15
### Added
  - `sandbox_exec` instruction and `make_sandbox_exec_operation` expression
  - asset can be initialized with constant parameter

## [1.5.0] - 2023-11-23
### Added
  - debugger output
  - `match_detach` instruction
  - multiple variable declaration
  - `open_chest` update
### Fixed
  - `nat_to_string` bug (since 1.4.3 version)

## [1.4.3] - 2023-06-02
### Added
  - Side effect in function
  - Function as instruction
  - Native michelson code
  - `exp_horner` function
  - `self` for internal call view
### Changed
  - BREAKING: bls curves literals

## [1.4.2] - 2023-05-03
### Added
  - Global constants
  - Add `is_implicit_address` function
  - Add `create_contract` function from arl source
  - Add `import` declaration from arl and tz source

### Removed
  - BREAKING: Formal verification
  - `tx_rollup_l2_address` type

## [1.4.1] - 2023-02-01
### Added
  - Ticket support (beta)
  - Add `simplify_rational`, `get_numerator` and `get_denominator` builtin functions
  - Add `bytes_to_nat` and `nat_to_bytes` util functions
  - Add `head` and `tail` functions for list
### Removed
  - Comment of michelson storage in michelson output
### Changed
  - Output of michelson
  - Improve michelson generation code
### Fixed
  - Complex type of contract interface for bindings

## [1.4.0] - 2022-12-06
### Added
  - Add `min_block_time` constant
### Changed
  - Improve type-checking (e.g. can now use named composite types in contract parameter)
  - `pair` is now n-ary (instead of binary right comb)

## [1.3.6] - 2022-11-24
### Added
  - add `tx_rollup_l2_address` type
### Changed
  - Improve contract interface

## [1.3.5] - 2022-09-22
### Changed
  - `emit` instruction generates Kathmandu event
### Fixed
  - Tuple and record operator assigment

## [1.3.4] - 2022-09-09
### Changed
  - Improve contract interface

## [1.3.3] - 2022-08-18
### Added
  - Import for `tz` files
  - Offchain views support (`-t offchain-views`)
  - `make_event` builtin
  - Contract interface for binding generation service
### Fixed
  - Generate error message for side effect instructions in functions and views
### Changed
  - BREAKING: `emit` instruction generates `EMIT` michelson instruction instead of a call to event well contrat

## [1.3.2] - 2022-07-07
### Added
  - `asset_container` type
  - `make_asset`, `create_contract` builtin, `update` on set type
  - `to_container`, `put_remove` asset method
### Changed
  - BREAKING: constant `caller` (and any other constant) are no longer available as initial storage value.

## [1.3.1] - 2022-06-22
### Added
  - `constant` entry section
  - `+=` and `-=` on `map`/`set` for `update`/`add_update`
### Changed
  - Jakarta protocol: update `sapling_verify_update` signature
  - `const` parameter behavior
### Fixed
  - Instructions for `operations` and `metadata`

## [1.3.0] - 2022-06-16
### Added
  - Asset access `my_asset[]`
  - Type `asset_value`
  - Add `otherwise` in `no transfer`, `called by`, `source by` and `state is` section
  - Arithmetic operations `div` and `mod` between `tez`
  - Ternary operator with `bool`, `option`
  - `otherwise` for entry section
### Changed
  - Short-circuit evaluation for boolean condition (`and` and `or`)
  - Optimize simple `add_update`
  - Optimize `reverse` for list
  - BREAKING: some identifiers and fail messages have been renamed, for more informations https://archetype-lang.org/blog/v13#renamings
### Fixed
  - Multiplication between rational and tez with big value

## [1.2.16] - 2022-05-17
### Added
  - New high-level type: `iterable_big_map`
  - Add `const` declaration in code block
### Changed
  - Asset flagged `big_map` with one field generates a `big_map` instead of `set`

## [1.2.15] - 2022-04-22
### Changed
  - BREAKING: `transfer` and `emit` instructions are executed in the declaration order
  - replace `view` type by `asset_view`

## [1.2.14] - 2022-04-01
### Added
  - Ithaca support: `sub_mutez`, `map` for `option` type
  - builtins: `isnat`, `to_nat`
  - `not` operator for `int` and `nat`
### Changed
  - Add type in event

## [1.2.13] - 2022-03-10
### Added
  - Event support (event type and emit instruction)
### Fixed
  - Arithmetic for bls curves
  - Unit literal as simple expression

## [1.2.12] - 2022-03-02
### Added
  - Sapling and timelock support
  - Show entry command
  - Output option in cli
### Fixed
  - Empty bytes literal

## [1.2.11] - 2021-12-14
### Added
  - emptylist<t> as literal of empty list
### Fixed
  - views are now present in javascript output
  - match ... with nested in lambda body

## [1.2.10] - 2021-11-10
### Added
  - on-chain view
### Changed
  - improve michelson output
### Fixed
  - `addupdate` when there is default value in asset

## [1.2.9] - 2021-10-03
### Added
  - `callview`, call on-chain view
### Fixed
  - `addupdate` on partition

## [1.2.8] - 2021-09-07
### Added
  - `contract_address` (contract -> address) and `key_address` (key -> address)
  - `require_some`, same as `opt_get` with an extra arg for custom failed
  - `sub_nat` substraction which returns `nat`
  - `require_entrypoint`, same as `require_some(entrypoint<...>(...))`
  - `mutez_to_nat`, convert `tez` to `nat` in mutez
  - michelson optimizations
### Changed
  - entrypoints are generated with a complete binary tree.
### Removed
  - Breaking : remove implicit cast from `tez` to `nat` (c.f. `mutez_to_nat`)

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
