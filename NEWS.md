# rcvr 0.1.0

First release usable by the `cvrs` pipeline. `rcvr` is now the only place raw
CVRs are parsed and the only place regex-based cleaning runs.

## Breaking changes

* `gen_metadata()` gains required `election`, `state`, and `county` arguments
  and emits the full store key, so its output is directly appendable to the
  metadata store.
* `gen_metadata()` no longer sets `magnitude` from `rank`. `rank` is the
  voter's ranking position and is carried as its own column; `magnitude` is
  the number of seats to be filled.
* `clean_cvr()` left-joins metadata and aborts with condition class
  `rcvr_unmatched_pairs` when a raw pair has no store row. It previously used
  `inner_join()` and dropped such rows silently.
* Supplying `metadata =` now hard-disables `generate_metadata`.
* Every reader returns the same pairs frame: `cvr_id`, `precinct`, `contest`,
  `raw_candidate`, `raw_party`, `rank`.

## New features

* `clean_cvr(type =)` takes the registry-declared type and uses
  `classify_cvr()` only to resolve the sub-case, so a directory holding more
  than one file type no longer aborts.
* XML CVRs are supported (`read_xml_cvr()`), covering 27 counties that
  previously fell through the dispatch with `clean` undefined.
* JSON CVRs produce a valid store key. `dominionCVR::extract_cvr()` returns
  only integer ids, so the JSON branch previously emitted no `raw_candidate`
  and its join could never match. Manifest resolution now happens in the
  reader, and `district`/`magnitude` are seeded from `ContestManifest`'s
  `VoteFor`.
* Counties whose format fits no general parser are handled by
  `rcvr_SPECIAL_READERS`, a registry keyed on `(state, county)`. All eleven
  special counties are now registered: New Jersey Cumberland, Texas Denton,
  Florida Walton, Florida Lee/Manatee/Marion/Santa Rosa/Sarasota (one shared
  reader), Pennsylvania Allegheny, Texas Montgomery, and California Los
  Angeles.
* `nonpartisan` is seeded alongside `party_detailed`; a nonpartisan row
  carries both.

## Bug fixes

* `make.unique()`'s dedup suffix no longer leaks into `raw_party` or
  `raw_candidate` (values like `"REP_1"`).
* `dplyr::na_if()` is `::`-qualified; it was previously called bare in a
  package that imports nothing.

## Known limitations

* The Texas Montgomery and California Los Angeles special readers are
  unverified against real data. `cvrs`'s own `process_special()` has no
  reading branch for either county — it only ever reads their files to list
  distinct contests — so there is no reference implementation to port. Their
  ballot-file column names and row shapes were inferred from what little the
  source code confirms; the reasoning is written out in comments in
  `R/read_special.R` above each reader. Check both against real ballot
  exports before trusting them in production.
* The fixtures for the five multi-file Florida special counties (Lee,
  Manatee, Marion, Santa Rosa, Sarasota) are `.xlsx` stand-ins for the real
  `.xls` format those counties actually ship.
