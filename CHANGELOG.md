# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0-alpha] - 2024-01-16

### Changed

- Enormous changes since last version; just getting it into open-sourceable
place
- Add disclaimer to readme
- Add license

## [0.2.0-alpha] - 2023-09-15

### Added

- `dist` handles desk whitelisting
- `groups` integration
- `%cli` agent interface
- `Sink.elm` frontend
- `nix` devops scripts

### Changed

- `vapors` is now a labelled namespace
- `peek` will now operate within the context of a namespace
- inline threads

Also refactors and extends the data structures in sur/vapor. The changes include:

- Removal of the old molds and comments related to the previous implementation of the vapor-make pattern.
- Introduction of new molds to represent a version of software distribution that packages multiple urbit desk-related entities. These include:
  - `vapors`: An association structure for bond-related permissions.
  - `owners`: Holds addresses and ownership types.
  - `versioned-state`: Represents the state of the software distribution.
  - `state-0`: Initial state of the software distribution.
  - `link`, `moss`, `urth`, `mars`: Various structures to representassociations, earth or problem, unreliable earth part, and a dynamicpiece of mars respectively.
  - Introduction of new molds to represent Ethereum output, distribution types, ownership data, and other related entities. These include:
    - `earf`: Ethereum output.
    - `tech`: Distribution types.
    - `pack`: Represents external data, internal namespace, distributiondata, and distributing ship.
    - `pods`: Owned ships.
    - `arks`: Ownership data.
    - Refactoring of the `make` and `ware` cores to accommodate the new data structures and their related actions and updates.

### Removed

- All old `ware` frontends. These live in the `old` branch now and should be moved to a new repo.

## [0.1.0-alpha] - 2023-03-01

### Added

- Full Ware and Make demo UIs
- Bolton Demos
- Opensea integration in hoon for improved ware loading experience
- Opensea integration in client for improved ware make experience
- Glob

### Fixed

- Issue with large file uploads
- Issue with processing non-text file blobs

### Changed

- Enhanced Azimal experience

## [0.0.1-alpha]

### Added

- vapor-make hoon
- vapor-ware hoon
- initial prototyping UI
- azimal index.html

(future records will be more granular and feature-based)

## [0.0.0]

### Added

- Initial add of VERSION and CHANGELOG.md
