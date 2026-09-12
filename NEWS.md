# rcvr 0.1.0

First release usable by the `cvrs` pipeline. `rcvr` is now the only place raw
CVRs are parsed and the only place regex-based cleaning runs.

## Condition classes

Every abort a caller might need to catch and continue past (recording a
failed county rather than stopping the whole run) is a classed condition:

* `rcvr_no_files` — a directory holds no files of the expected format
  (`read_delim_multi_cvr()`, `read_json_cvr()`, `read_xml_cvr()`,
  `read_special_pa_allegheny()`, `resolve_reader()`).
* `rcvr_bad_type` — the declared `type` is not one of `"delim"`, `"json"`,
  `"xml"`, `"special"` (`resolve_reader()`); or `classify_cvr()`/
  `clean_cvr()` could not resolve a type at all (a directory whose files
  carry no recognised CVR extension).
* `rcvr_bad_pairs` — a reader returned a pairs frame missing a required
  column, with the wrong column type, or with zero rows (`assert_pairs()`).
* `rcvr_missing_manifest` — a Dominion JSON export directory is missing one
  of its manifest files (`read_dominion_manifests()`).
* `rcvr_unresolved_ids` — a manifest id in a JSON or California Los Angeles
  CVR could not be resolved against its lookup table.
* `rcvr_no_special_reader` — no reader is registered in
  `rcvr_SPECIAL_READERS` for the requested `(state, county)`.
* `rcvr_missing_county` — `state`/`county` were not supplied for a `"special"`
  CVR, which `read_special_cvr()` requires to look up the reader.
* `rcvr_bad_metadata_args` — `election`/`state`/`county` passed to
  `gen_metadata()` are not single non-`NA` strings.
* `rcvr_no_metadata` — `join_metadata()`/`clean_cvr()` was asked to clean
  without a metadata store and without `generate_metadata`/`metadata_only`.
* `rcvr_unmatched_pairs` — a raw `(contest, raw_candidate)` pair has no row
  in the supplied metadata store slice.
* `rcvr_duplicate_metadata` — the supplied metadata store slice has more
  than one row for the same `(contest, raw_candidate)` pair, which would fan
  a left join out and silently inflate ballot counts.
* `rcvr_unnamed_option` — a marked XML `<Options>` sibling has neither a
  `<Name>` nor write-in text, so its cast vote cannot be identified
  (`read_xml_cvr()`).
* `rcvr_bad_path` — `clean_cvr(path =)` is not a single non-`NA` string.

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
* A redacted ballot cell (`rcvr_REDACT_NAMES`) no longer aborts its whole
  county. `pairs_from_delim()` now maps a redacted cell to the literal
  sentinel `"redacted"` instead of `NA_character_`, and `rcvr_UNSEEDED_RE`
  exempts it, so `gen_metadata()`'s seed filter and `join_metadata()`'s
  anti-join exemption agree by construction. A genuine `NA` `raw_candidate`
  remains an error.
* `join_metadata()` now aborts with `rcvr_duplicate_metadata` before joining
  if the supplied metadata store has more than one row for the same
  `(contest, raw_candidate)` pair, instead of silently fanning ballot rows
  out across the left join.
* `assert_pairs()` now rejects a zero-row pairs frame; previously
  `all(is.na(x))` is vacuously `TRUE` on an empty vector, so a reader whose
  filters removed every row passed every check and produced a silent,
  successful-looking loss of the whole county.
* `classify_cvr()` aborts with `rcvr_bad_type` (instead of falling off the
  end and returning `NULL`) when a directory holds files but none with a
  recognised CVR extension; `clean_cvr()` now validates the resolved type
  before dispatch instead of dying with an unclassed
  "missing value where TRUE/FALSE needed" error.
* `read_delim_cvr()`'s unreadable-extension abort is now reachable: the
  extension check runs before `is_header()`, which previously died first
  with an unclassed `object 'd' not found` for any file not matched by
  `is_header()`'s own CSV/Excel detection.
* `gen_patterns()`'s metacharacter-escaping regex is now built with
  `perl = TRUE`; the previous TRE-engine bracket expression silently failed
  to escape a party abbreviation containing a regex metacharacter.
* A marked XML `<Options>` sibling with neither a `<Name>` nor write-in text
  no longer silently collapses to `"undervote"` (a vote was cast; it's just
  unidentified). `xml_ballot()` now keeps it distinct from a genuinely
  unmarked option, and `read_xml_cvr()` aborts with `rcvr_unnamed_option`
  instead of the trailing `filter(!is.na(raw_candidate))` quietly dropping
  the row.
* `clean_cvr(path =)` now aborts with `rcvr_bad_path` on a missing or
  non-string `path` instead of an unclassed error surfacing from deeper in
  the call stack (`fs::path_real()`/`classify_cvr()`).

## Dependencies

* `dominionCVR` moved from `Imports` to `Suggests`, with a new
  `Remotes: github::kuriwaki/dominionCVR` entry. It is not on CRAN, so as a
  hard `Imports:` it made `remotes::install_github()` unresolvable for
  anyone but the author; every call site already guarded it with
  `rlang::check_installed()` and the JSON tests already
  `skip_if_not_installed()` it.

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
