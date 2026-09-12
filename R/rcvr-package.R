# These are all data-frame column names referenced via NSE (dplyr/tidyr
# verbs) inside the package's own functions, not undeclared globals — but
# `R CMD check`'s static analysis can't tell the difference, so they must be
# declared here to keep `checking R code for possible problems` clean.
utils::globalVariables(c(
  "CVRNumber", "Candidate", "Code", "Contest", "Freq", "IsVote", "Precinct",
  "PrecinctPortion", "Race", "Var1", "ballot_order", "ballot_style",
  "ballot_style2", "batchId", "candidate", "candidateId", "cardId", "cid",
  "code", "contest", "contestId", "cvrNumber", "cvr_id", "district",
  "district_id", "id", "isCurrent", "is_vote", "magnitude", "magnitude2",
  "office", "p", "partyId", "party_detailed", "precinct",
  "precinctPortionId", "raw_candidate", "raw_party", "recordId", "tabulatorId"
))
