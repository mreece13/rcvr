devtools::load_all()

# path = "~/Dropbox (MIT)/Research/cvr_panel/data/raw/CO_Boulder/2024-Boulder-County-General-Redacted-Cast-Vote-Record.xlsx"
# path = "~/Dropbox (MIT)/Research/cvr_panel/data/raw/Maryland/2024 General/Caroline/06PG24_Prov_cvr_SBE.xlsx"
path = "~/Dropbox (MIT)/Research/cvr_panel/data/raw/Maryland/2024 General/Allegany"

# classify_cvr(path)
d = clean_cvr(path, generate_metadata = TRUE, metadata_only = TRUE)

md24 = tibble(
  election = "2024 General",
  dir = list.dirs("~/Dropbox (MIT)/Research/cvr_panel/data/raw/Maryland/2024 General", recursive = FALSE),
  county = list.dirs("~/Dropbox (MIT)/Research/cvr_panel/data/raw/Maryland/2024 General", recursive = FALSE, full.names = FALSE)
) |>
  mutate(
    metadata = map(dir, \(x) clean_cvr(x, generate_metadata = TRUE, metadata_only = TRUE))
  ) |>
  unnest(cols = metadata)

md24 |>
  arrange(county, ballot_order) |>
  select(-dir, -ballot_order) |>
  mutate(state = "Maryland", .before=county) |>
  mutate(
    office = NA,
    district = NA,
    .after = raw_party
  ) |>
  left_join(t, join_by(candidate), relationship="many-to-many") |>
  mutate(party = coalesce(raw_party.x, raw_party.y)) |>
  rename(raw_party = raw_party.x) |>
  select(-raw_party.y) |>
  clipr::write_clip()

library(rvest)
library(httr2)

get_party <- function(val){

  read_html(glue::glue("https://elections.maryland.gov/elections/2024/general_results/gen_results_2024_by_county_{val}.html")) |>
    html_table(convert=FALSE) |>
    bind_rows() |>
    select(Name, Party) |>
    filter(!str_detect(Name, "Write-In|Totals")) |>
    mutate(Name = str_remove_all(Name, '"')) |>
    mutate(Name = str_remove(Name, "\\(.*\\)") |> str_squish()) |>
    select(candidate = Name, raw_party = Party)
}

# get parties from Maryland websites
t = map(1:24, get_party) |> bind_rows() |> distinct()

clipr::read_clip_tbl() |>
  left_join(t, join_by(candidate == Name), relationship="many-to-many") |>
  mutate(party = coalesce(Party.x, Party.y)) |>
  select(-Party.x, -Party.y) |>
  clipr::write_clip()

metadata = googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Pq9sNcCfLVi-qeXfBy7xEi5lPxpMJYy3LVUHQn_uEFI/edit?gid=1550619837#gid=1550619837",
  "metadata",
  col_types = "c"
) |>
  mutate(
    district = str_replace(district, "county_name", county),
    across(office:prop_text, str_to_lower),
    magnitude = as.integer(magnitude)
  ) |>
  group_by(raw_candidate) |>
  fill(party) |>
  ungroup() |>
  group_by(candidate, party) |>
  fill(office, district, .direction = "updown") |>
  ungroup()
