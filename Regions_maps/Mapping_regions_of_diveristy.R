
# install.packages("stringr") # if needed
library(stringr)
library(dplyr)
library(readxl)

### Regions review file processing:
veglist1_regions <- read_excel("C:/Users/sarah/Downloads/REVIEW_veglist1_regions_assigned_2025_12_04.xlsx")

# make region_final field
# if "SG PTFTW regions reassigned" is not NA, fill in "region_final"; and if NA then fill in "region_final" with PTFTW_region_assigned
veglist1_regions <- veglist1_regions %>%
  mutate(
    # ensure character (avoid factor/type issues)
    `SG PTFTW regions reassigned` = as.character(`SG PTFTW regions reassigned`),
    PTFTW_region_assigned = as.character(PTFTW_region_assigned),
    # treat empty string as NA if desired:
    `SG PTFTW regions reassigned` = na_if(`SG PTFTW regions reassigned`, ""),
    PTFTW_region_assigned = na_if(PTFTW_region_assigned, ""),
    # final column: first non-NA value
    regions_final = coalesce(`SG PTFTW regions reassigned`, PTFTW_region_assigned)
  )

# canonical list (same as before)
canonical <- c(
  "Africa_Central",
  "Africa_East",
  "Africa_IndianOceanIslands",
  "Africa_Southern",
  "Africa_West",
  "America_Caribbean",
  "America_Central_and_Mexico",
  "America_North",
  "America_South_andean",
  "America_South_temperate",
  "America_South_tropical",
  "Asia_Central",
  "Asia_East",
  "Asia_South",
  "Asia_Southeast",
  "Asia_West",
  "Europe_Eastern_north",
  "Europe_Eastern_south",
  "Europe_Western_north",
  "Europe_Western_south",
  "Mediterranean_SouthandEast",
  "Pacific Region- Australia and New Zealand",
  "Pacific_Region_tropical"
)

# Helper to normalize strings: replace underscores/hyphens with space, collapse whitespaces, lower
norm_str <- function(x) {
  if (is.na(x)) return(NA_character_)
  x2 <- tolower(gsub("[_\\-]+", " ", as.character(x)))     # turn "_" and "-" into spaces
  x2 <- gsub("[^a-z0-9\\s]+", " ", x2, perl = TRUE)        # remove other punctuation -> spaces
  x2 <- gsub("\\s+", " ", x2, perl = TRUE)                 # collapse multiple spaces
  trimws(x2)
}

# Build stricter regex patterns from canonical names (match whole words in order)
patterns <- vapply(canonical, function(x) {
  nx <- norm_str(x)
  words <- strsplit(nx, "\\s+", perl = TRUE)[[1]]
  # require each word as a whole word, separated by one or more whitespace
  pat <- paste0("\\b", paste(words, collapse = "\\s+"), "\\b")
  paste0("(?i)", pat)  # case-insensitive
}, FUN.VALUE = character(1), USE.NAMES = FALSE)
names(patterns) <- canonical

standardize_regions <- function(cell) {
  if (is.na(cell) || str_trim(cell) == "") return(NA_character_)
  s <- as.character(cell)
  s_norm <- norm_str(s)
  
  # first try global full-text detection (handles multi-region strings)
  matches <- canonical[vapply(patterns, function(p) {
    grepl(p, s_norm, perl = TRUE)
  }, logical(1))]
  
  # fallback: split on common separators and match each token against normalized canonical forms
  if (length(matches) == 0) {
    toks <- unlist(strsplit(s, "[,;/|\\n\\t\\+&]", perl = TRUE))
    toks <- trimws(toks)
    toks <- toks[toks != ""]
    matches_found <- character(0)
    # precompute normalized canonicals for substring/approx matches
    norm_canon <- vapply(canonical, norm_str, FUN.VALUE = character(1), USE.NAMES = FALSE)
    for (t in toks) {
      nt <- norm_str(t)
      # exact normalized equality or substring (prefer exact)
      exact <- canonical[which(nt == norm_canon)]
      if (length(exact) > 0) {
        matches_found <- c(matches_found, exact[1]); next
      }
      # substring within a canonical normalized form (e.g., token "southeast" => matches "asia southeast")
      subfound <- canonical[vapply(norm_canon, function(nc) grepl(paste0("\\b", nt, "\\b"), nc, perl = TRUE), logical(1))]
      if (length(subfound) > 0) {
        matches_found <- c(matches_found, subfound[1]); next
      }
      # approximate match as last resort
      approx <- agrep(nt, canonical, ignore.case = TRUE, max.distance = 0.15, value = TRUE)
      if (length(approx) > 0) matches_found <- c(matches_found, approx[1])
    }
    matches <- unique(matches_found)
  }
  
  # if still nothing matched, return trimmed original so you can inspect (same as before)
  if (length(matches) == 0) return(trimws(s))
  
  # keep canonical order and unique
  matches_ordered <- canonical[canonical %in% matches]
  paste(matches_ordered, collapse = ", ")
}

# Apply to your dataframe (after you already created veglist1_regions$regions_final)
veglist1_regions$regions_final_clean <- vapply(
  veglist1_regions$regions_final,
  standardize_regions,
  FUN.VALUE = character(1)
)

# Quick check for problematic mappings (where we changed or over-expanded)
head(subset(veglist1_regions, regions_final != regions_final_clean),
     n = 20)[, c("regions_final", "regions_final_clean")]



# Count veglist 1 species that no PTFTW region assigned = 375
count_missing_region <- veglist1_regions %>%
  filter(is.na(regions_final_clean)) %>%
  summarise(count = n()) %>%
  pull(count)
count_missing_region

# Count priority species that have no PTFTW region assigned = 0 
count_missing_region <- veglist1_regions %>%
  filter(priority_species == "Y" & is.na(regions_final_clean)) %>%
  summarise(count = n()) %>%
  pull(count)
count_missing_region

# Count PTFTW species that have no PTFTW region assigned =0 
count_missing_region <- veglist1_regions %>%
  filter(ptftw_species == "Y" & is.na(regions_final_clean)) %>%
  summarise(count = n()) %>%
  pull(count)
count_missing_region


# drop field
veglist1_regions <- veglist1_regions %>% select(-regions_final)
veglist1_regions <- veglist1_regions %>% rename(regions_final = regions_final_clean)



################## Full workflow  — updated 2025_12_15
# Mapping
# Change: country lists in region -> country aggregates now use semicolon ("; ") as the separator
# so that official country names that include commas (e.g. "Congo, Democratic Republic of the")
# are not split when those lists are later processed. The greedy fallback now splits on semicolons.
#
# Other behavior unchanged: normalized full-string country match before splitting, region->country expansion,
# Country_code first, greedy fallback.
#
# Inputs required in environment:
# - veglist1_regions (data frame with ptftw_species, Taxon, regions_final)
# - countries_in_regions Excel at guide_path (with PlantsThatFeedTheWorld_Region_new, Country_fullname, Country_code)
#
# Outputs written (same as before).
#
# Usage: source() this file or paste into your R session.

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(readxl)
library(countrycode)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(viridis)
library(readr)
library(writexl)

# ---------------------------
# 0) Filter veglist1_regions to 80 PTFTW species
if (!exists("veglist1_regions")) stop("veglist1_regions not found in the environment")
veglist1 <- veglist1_regions %>% filter(ptftw_species == "Y")


# ---------------------------
# 0) Filter veglist1_regions to 141 Priority species
if (!exists("veglist1_regions")) stop("veglist1_regions not found in the environment")
veglist1 <- veglist1_regions %>% filter(priority_species == "Y")

# ---------------------------
# Helper: normalization function (strip diacritics, lower-case, trim, collapse spaces)
norm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("\\s*\\(.*?\\)\\s*", " ", x)
  x <- gsub("[^a-z0-9\\s,\\-]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# ---------------------------
# Paths: adjust if needed
# COUNTRIES IN REGIONS GUIDE FILE:
# UPDATED 2025_12_15
guide_path <- "C:/Users/sarah/Downloads/countries_in_regions_corrected2025_12_15.xlsx"

# ---------------------------
# Read the guide Excel file
countries_in_regions <- readxl::read_excel(guide_path)

# ---------------------------
# Preprocess guide: use Country_fullname (ignore 'corrected'), split multi-region cells into single rows
countries_in_regions_expanded <- countries_in_regions %>%
  mutate(
    PlantsThatFeedTheWorld_Region_new = as.character(PlantsThatFeedTheWorld_Region_new),
    Country_fullname = as.character(Country_fullname),
    Country_code = as.character(Country_code)
  ) %>%
  separate_rows(PlantsThatFeedTheWorld_Region_new, sep = "[,;|]+") %>%
  mutate(region_country = trimws(PlantsThatFeedTheWorld_Region_new),
         country_name = trimws(Country_fullname)) %>%
  filter(!is.na(region_country) & region_country != "" & !is.na(country_name) & country_name != "") %>%
  distinct(region_country, country_name, Country_code)

# Build lookups we will use
region_to_country_df <- countries_in_regions_expanded %>%
  distinct(region_country, country_name, Country_code)

# IMPORTANT CHANGE:
# Use semicolon as the collapse separator so comma-containing official names are preserved.
regions_countries_single <- region_to_country_df %>%
  group_by(region_country) %>%
  summarize(country = paste(unique(country_name), collapse = "; "), .groups = "drop") %>%
  mutate(region_norm = norm(region_country))

countries_lookup <- countries_in_regions_expanded %>%
  distinct(country_name, Country_code) %>%
  filter(!is.na(country_name) & country_name != "")

# Precompute normalized country names for robust matching
countries_lookup <- countries_lookup %>%
  mutate(country_norm = norm(country_name))

# Precompute normalized region names for robust matching
region_to_country_df <- region_to_country_df %>% mutate(region_norm = norm(region_country))

# Quick sanity: warn if any region_country still contains separators
bad_region_names <- regions_countries_single %>% filter(str_detect(region_country, "[,;|]"))
if (nrow(bad_region_names) > 0) {
  warning("Some region_country entries still contain separators; inspect bad_region_names:")
  print(bad_region_names)
}

# ---------------------------
# Optional: manual region recodes (keys normalized)
manual_region_recodes_raw <- c(
  "Ascension and Tristan da Cunha" = "Saint Helena, Ascension and Tristan da Cunha",
  "Belgium-Luxembourg" = "Belgium, Luxembourg",
  "Pacific Islands Trust Territory" = "Palau, Marshall Islands, Micronesia (Federated States of), Northern Mariana Islands",
  "RÃ©union" = "Reunion",
  "RÉUNION" = "Reunion",
  "Serbia and Montenegro" = "Serbia, Montenegro",
  "Yugoslav SFR" = "Serbia, Croatia, Bosnia and Herzegovina, Slovenia, North Macedonia, Montenegro, Kosovo"
)
manual_region_recodes <- setNames(manual_region_recodes_raw, norm(names(manual_region_recodes_raw)))

# ---------------------------
# Build veg_countries intermediate: region -> semicolon-list country (country may be a semicolon-list)
veg_countries <- veglist1 %>%
  mutate(record_id = row_number(),
         crop = as.character(Taxon),
         region_original = as.character(regions_final)) %>%
  select(record_id, crop, region_original) %>%
  filter(!is.na(region_original) & region_original != "") %>%
  separate_rows(region_original, sep = "[,;|]+") %>%
  mutate(region_token = trimws(region_original)) %>%
  filter(region_token != "") %>%
  mutate(region_norm = norm(region_token),
         region_expanded = ifelse(!is.na(region_norm) & region_norm %in% names(manual_region_recodes),
                                  manual_region_recodes[region_norm],
                                  region_token)) %>%
  separate_rows(region_expanded, sep = "[,;|]+") %>%
  mutate(region_expanded = trimws(region_expanded)) %>%
  filter(region_expanded != "") %>%
  mutate(region_expanded_norm = norm(region_expanded)) %>%
  left_join(regions_countries_single %>% select(region_norm, country),
            by = c("region_expanded_norm" = "region_norm")) %>%
  filter(!is.na(country) & country != "") %>%
  distinct(record_id, crop, country)

# PTFTW: Save intermediate file for debugging (country field is semicolon-separated)
#writexl::write_xlsx(veg_countries %>% dplyr::select(record_id, crop, country), "Regions/PTFTW/2025_12_15/veg_countries_long_veglist3_ptftw_2025_12_15.xlsx")

# Priority: Save intermediate file for debugging (country field is semicolon-separated)
writexl::write_xlsx(veg_countries %>% dplyr::select(record_id, crop, country), "Regions/Priority/2025_12_15/veg_countries_long_veglist2_priority_2025_12_15.xlsx")


# ---------------------------
# Report region tokens that failed to map to countries (so you can refine region recodes or the guide)
failed_region_rows <- veglist1 %>%
  mutate(record_id = row_number(), region_original = as.character(regions_final)) %>%
  select(record_id, region_original) %>%
  filter(!is.na(region_original) & region_original != "") %>%
  separate_rows(region_original, sep = "[,;|]+") %>%
  mutate(region_token = trimws(region_original)) %>%
  filter(region_token != "") %>%
  mutate(region_norm = norm(region_token),
         region_mapped_or_expanded = ifelse(region_norm %in% names(manual_region_recodes),
                                            manual_region_recodes[region_norm],
                                            region_token)) %>%
  separate_rows(region_mapped_or_expanded, sep = "[,;|]+") %>%
  mutate(region_mapped_or_expanded = trimws(region_mapped_or_expanded),
         region_mapped_norm = norm(region_mapped_or_expanded),
         mapped = region_mapped_norm %in% regions_countries_single$region_norm) %>%
  filter(!mapped) %>%
  distinct(region_token, region_mapped_or_expanded, region_mapped_norm)

if (nrow(failed_region_rows) > 0) {
  message("Region tokens (after applying manual recodes) that did NOT map to countries. Review and update the guide or manual_region_recodes:")
  print(failed_region_rows)
} else {
  message("All region tokens mapped to countries (after manual recodes).")
}

# ---------------------------
# Improved resolver v2: uses normalized full-cell match before splitting,
# and greedy longest-contiguous matching as fallback.
# NOTE: changed fallback splitting to semicolon (';') so comma-containing official names are preserved.
resolve_cell_to_rows <- function(cell_value,
                                 region_to_country_df = NULL,
                                 countries_lookup = NULL,
                                 region_names = NULL) {
  # lookups default to environment if not supplied
  if (is.null(region_to_country_df)) region_to_country_df <- get("region_to_country_df", envir = parent.frame())
  if (is.null(countries_lookup)) countries_lookup <- get("countries_lookup", envir = parent.frame())
  if (is.null(region_names)) region_names <- unique(region_to_country_df$region_country)
  
  cell_trim <- trimws(as.character(cell_value))
  if (is.na(cell_trim) || cell_trim == "") return(tibble(country = character(0), Country_code = character(0)))
  
  # 0a) normalized full-string country match using countries_lookup$country_norm
  cell_norm <- norm(cell_trim)
  if (!is.na(cell_norm) && cell_norm %in% countries_lookup$country_norm) {
    matched_name <- countries_lookup$country_name[match(cell_norm, countries_lookup$country_norm)]
    matched_code <- countries_lookup$Country_code[match(cell_norm, countries_lookup$country_norm)]
    return(tibble(country = matched_name, Country_code = matched_code))
  }
  
  # 0b) normalized full-string region match (in case region naming variants exist)
  if (!is.na(cell_norm) && cell_norm %in% region_to_country_df$region_norm) {
    rows <- region_to_country_df %>%
      filter(region_norm == cell_norm) %>%
      transmute(country = country_name, Country_code = Country_code)
    return(rows)
  }
  
  # 1) exact full-string region match (raw)
  if (cell_trim %in% region_names) {
    rows <- region_to_country_df %>%
      filter(region_country == cell_trim) %>%
      transmute(country = country_name, Country_code = Country_code)
    return(rows)
  }
  
  # 2) exact full-string guide country match (raw)
  if (cell_trim %in% countries_lookup$country_name) {
    code <- countries_lookup$Country_code[match(cell_trim, countries_lookup$country_name)]
    return(tibble(country = cell_trim, Country_code = code))
  }
  
  # 3) full-string countrycode resolution (official names may include commas)
  iso_full <- countrycode(cell_trim, origin = "country.name", destination = "iso3c", warn = FALSE)
  if (!is.na(iso_full)) {
    canonical <- countrycode(iso_full, origin = "iso3c", destination = "country.name", warn = FALSE)
    country_name <- ifelse(is.na(canonical) | canonical == "", cell_trim, canonical)
    return(tibble(country = country_name, Country_code = iso_full))
  }
  
  # 4) fallback: split into parts and perform greedy longest-contiguous matching
  # split on semicolon (preferred) or pipe ; do NOT split on commas to preserve official names with commas
  parts <- unlist(strsplit(cell_trim, ";\\s*|\\|\\s*"))
  parts <- trimws(parts)
  n <- length(parts)
  if (n == 0) return(tibble(country = character(0), Country_code = character(0)))
  
  resolved <- list()
  pos <- 1
  while (pos <= n) {
    matched <- FALSE
    for (len in seq(n - pos + 1, 1)) {
      j <- pos + len - 1
      candidate <- paste(parts[pos:j], collapse = "; ")
      candidate_trim <- trimws(candidate)
      # (a) normalized country match for candidate
      cand_norm <- norm(candidate_trim)
      if (!is.na(cand_norm) && cand_norm %in% countries_lookup$country_norm) {
        code <- countries_lookup$Country_code[match(cand_norm, countries_lookup$country_norm)]
        name <- countries_lookup$country_name[match(cand_norm, countries_lookup$country_norm)]
        resolved[[length(resolved) + 1]] <- tibble(country = name, Country_code = code)
        pos <- j + 1
        matched <- TRUE
        break
      }
      # (b) exact guide country match for candidate
      if (candidate_trim %in% countries_lookup$country_name) {
        code <- countries_lookup$Country_code[match(candidate_trim, countries_lookup$country_name)]
        resolved[[length(resolved) + 1]] <- tibble(country = candidate_trim, Country_code = code)
        pos <- j + 1
        matched <- TRUE
        break
      }
      # (c) countrycode match on candidate
      iso_cand <- countrycode(candidate_trim, origin = "country.name", destination = "iso3c", warn = FALSE)
      if (!is.na(iso_cand)) {
        canonical <- countrycode(iso_cand, origin = "iso3c", destination = "country.name", warn = FALSE)
        country_name <- ifelse(is.na(canonical) | canonical == "", candidate_trim, canonical)
        resolved[[length(resolved) + 1]] <- tibble(country = country_name, Country_code = iso_cand)
        pos <- j + 1
        matched <- TRUE
        break
      }
      # (d) region match on candidate (normalized)
      if (!is.na(cand_norm) && cand_norm %in% region_to_country_df$region_norm) {
        rows <- region_to_country_df %>% filter(region_norm == cand_norm) %>%
          transmute(country = country_name, Country_code = Country_code)
        if (nrow(rows) > 0) {
          resolved[[length(resolved) + 1]] <- rows
          pos <- j + 1
          matched <- TRUE
          break
        }
      }
    }
    if (!matched) {
      # single-part fallback
      part <- parts[pos]
      part_norm <- norm(part)
      if (!is.na(part_norm) && part_norm %in% countries_lookup$country_norm) {
        code <- countries_lookup$Country_code[match(part_norm, countries_lookup$country_norm)]
        name <- countries_lookup$country_name[match(part_norm, countries_lookup$country_norm)]
        resolved[[length(resolved) + 1]] <- tibble(country = name, Country_code = code)
      } else {
        iso_part <- countrycode(part, origin = "country.name", destination = "iso3c", warn = FALSE)
        if (!is.na(iso_part)) {
          canonical <- countrycode(iso_part, origin = "iso3c", destination = "country.name", warn = FALSE)
          country_name <- ifelse(is.na(canonical) | canonical == "", part, canonical)
          resolved[[length(resolved) + 1]] <- tibble(country = country_name, Country_code = iso_part)
        } else {
          resolved[[length(resolved) + 1]] <- tibble(country = part, Country_code = NA_character_)
        }
      }
      pos <- pos + 1
    }
  }
  
  bind_rows(resolved) %>% distinct()
}

# ---------------------------
# Expand veg_countries into atomic country rows and attach Country_code (ISO3) first
region_names <- unique(region_to_country_df$region_country)
countries_guide_names <- countries_lookup$country_name

t0_all <- Sys.time()
message("Starting fast expansion of ", nrow(veg_countries), " rows...")

# memoized countrycode cache (to avoid repeated lookups)
cc_cache <- new.env(parent = emptyenv())
memo_countrycode_single <- function(x) {
  k <- as.character(x)
  if (exists(k, envir = cc_cache, inherits = FALSE)) return(get(k, envir = cc_cache))
  res <- countrycode(k, origin = "country.name", destination = "iso3c", warn = FALSE)
  assign(k, res, envir = cc_cache)
  res
}
memo_countrycode_vec <- function(vec) {
  sapply(as.character(vec), memo_countrycode_single, USE.NAMES = FALSE)
}

# ensure helper lookups exist
if (!exists("region_to_country_df")) stop("region_to_country_df not found")
if (!exists("countries_lookup")) stop("countries_lookup not found")
if (!"country_norm" %in% names(countries_lookup)) {
  countries_lookup <- countries_lookup %>% mutate(country_norm = ifelse(!is.na(country_name), tolower(gsub("[^a-z0-9]+", " ", country_name)), NA_character_))
}

# prepare veg_countries (ensure character)
# Note: veg_countries$country is now a semicolon-separated list of countries for each region token
veg_c <- veg_countries %>% mutate(country = as.character(country), country_norm = norm(country))

# STEP 1: exact full-cell region matches -> expand (vectorized join)
t0 <- Sys.time()
rows_region <- veg_c %>%
  filter(country %in% region_names) %>%
  left_join(region_to_country_df, by = c("country" = "region_country")) %>%
  transmute(record_id, crop, Country_code, country = country_name)

t1 <- Sys.time()
message("Region-expansion done: ", nrow(rows_region), " rows (", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "s)")

# STEP 2: exact full-cell guide country matches (vectorized)
t0 <- Sys.time()
rows_guide_exact <- veg_c %>%
  filter(country %in% countries_guide_names) %>%
  left_join(countries_lookup %>% select(country_name, Country_code), by = c("country" = "country_name")) %>%
  transmute(record_id, crop, Country_code, country)

t1 <- Sys.time()
message("Guide exact matches done: ", nrow(rows_guide_exact), " rows (", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "s)")

# STEP 3: normalized full-cell guide matches (catch variants)
t0 <- Sys.time()
rows_guide_norm <- veg_c %>%
  filter(!country %in% c(region_names, countries_guide_names) & country_norm %in% unique(countries_lookup$country_norm)) %>%
  left_join(countries_lookup %>% select(country_name, Country_code, country_norm),
            by = c("country_norm" = "country_norm")) %>%
  transmute(record_id, crop, Country_code, country = country_name)

t1 <- Sys.time()
message("Guide normalized matches done: ", nrow(rows_guide_norm), " rows (", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "s)")

# Collect handled record_ids
handled_ids <- unique(c(rows_region$record_id, rows_guide_exact$record_id, rows_guide_norm$record_id))
remaining <- veg_c %>% filter(!record_id %in% handled_ids)

message("Remaining rows to process with countrycode/greedy resolver: ", nrow(remaining))

# STEP 4: try bulk/cached countrycode on the whole remaining cell values
t0 <- Sys.time()
if (nrow(remaining) > 0) {
  rem_tokens <- unique(remaining$country)
  message("Resolving ", length(rem_tokens), " unique remaining whole-cell tokens with countrycode (cached)...")
  iso_map <- memo_countrycode_vec(rem_tokens)
  iso_map_df <- tibble(token = rem_tokens, Country_code = as.character(iso_map))
  remaining <- remaining %>% left_join(iso_map_df, by = c("country" = "token"))
  rows_resolved_full <- remaining %>% filter(!is.na(Country_code)) %>%
    mutate(country = countrycode(Country_code, origin = "iso3c", destination = "country.name", warn = FALSE)) %>%
    transmute(record_id, crop, Country_code, country)
  remaining2 <- remaining %>% filter(is.na(Country_code)) %>% select(record_id, crop, country)
} else {
  rows_resolved_full <- tibble(record_id = integer(0), crop = character(0), Country_code = character(0), country = character(0))
  remaining2 <- tibble(record_id = integer(0), crop = character(0), country = character(0))
}
t1 <- Sys.time()
message("Bulk countrycode pass done: resolved ", nrow(rows_resolved_full), " rows (", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "s)")

# STEP 5: run greedy resolver only on unique unresolved tokens (should be small)
t0 <- Sys.time()
rows_from_greedy <- tibble(record_id = integer(0), crop = character(0), Country_code = character(0), country = character(0))
if (nrow(remaining2) > 0) {
  unresolved_tokens <- unique(remaining2$country)
  message("Running greedy resolver on ", length(unresolved_tokens), " unique unresolved tokens (this may take some time).")
  use_pb <- requireNamespace("pbapply", quietly = TRUE)
  if (use_pb) {
    resolved_list <- pbapply::pblapply(unresolved_tokens, function(tok) {
      res <- resolve_cell_to_rows(tok)
      if (nrow(res) == 0) return(NULL)
      res %>% mutate(source_token = tok)
    })
  } else {
    resolved_list <- lapply(unresolved_tokens, function(tok) {
      res <- resolve_cell_to_rows(tok)
      if (nrow(res) == 0) return(NULL)
      res %>% mutate(source_token = tok)
    })
  }
  resolved_map_df <- bind_rows(resolved_list)
  if (nrow(resolved_map_df) == 0) {
    message("Greedy resolver returned 0 rows.")
  } else {
    resolved_map_df <- resolved_map_df %>% rename(resolved_country = country)
    rows_from_greedy <- remaining2 %>% left_join(resolved_map_df, by = c("country" = "source_token")) %>%
      mutate(Country_code = Country_code,
             country = ifelse(!is.na(Country_code) & Country_code != "",
                              countrycode(Country_code, origin = "iso3c", destination = "country.name", warn = FALSE),
                              resolved_country)) %>%
      transmute(record_id, crop, Country_code, country) %>%
      distinct()
  }
}
t1 <- Sys.time()
message("Greedy resolver finished (", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "s)")

# Combine all resolved parts
combined <- bind_rows(rows_region, rows_guide_exact, rows_guide_norm, rows_resolved_full, rows_from_greedy) %>%
  distinct(record_id, crop, Country_code, country)

t_final <- Sys.time()
message("Expansion complete: produced ", nrow(combined), " rows in ", round(as.numeric(difftime(t_final, t0_all, units = "secs")), 2), "s total.")

# Final: ensure Country_code column exists
if (!"Country_code" %in% names(combined)) combined$Country_code <- NA_character_

# expose final object with Country_code first
veg_countries_expanded <- combined %>% select(record_id, crop, Country_code, country)

# PTFTW: write excel file
#writexl::write_xlsx(veg_countries_expanded, "Regions/PTFTW/2025_12_15/veg_countries_long_with_iso3_veglist3_ptftw_2025_12_15.xlsx")

# Priority: write excel file
writexl::write_xlsx(veg_countries_expanded, "Regions/Priority/2025_12_15/veg_countries_long_with_iso3_veglist2_priority_2025_12_15.xlsx")


# Print a quick sample and any unresolved country tokens
message("Sample of expanded rows:")
print(head(veg_countries_expanded, 10))

remaining_problems <- veg_countries_expanded %>% filter(is.na(Country_code) | Country_code == "") %>% distinct(country) %>% arrange(country)
if (nrow(remaining_problems) > 0) {
  message("Remaining unresolved country tokens (no ISO):")
  print(remaining_problems)
} else {
  message("All tokens resolved to ISO where possible.")
}

# expose veg_countries_expanded in the environment
assign("veg_countries_expanded", veg_countries_expanded, envir = .GlobalEnv)


# ---------------------------
# Compute richness FROM THE GUIDE (canonicalize to ISO3 using guide Country_code where available)
#
# Steps as before: expand regions per crop, join to guide mapping, prefer guide Country_code, count distinct iso3-crop pairs.
regions_per_crop_expanded <- veglist1 %>%
  mutate(crop = as.character(Taxon),
         region_original = as.character(regions_final)) %>%
  filter(!is.na(region_original) & region_original != "") %>%
  separate_rows(region_original, sep = "[,;|]+") %>%
  mutate(region = trimws(region_original)) %>%
  filter(region != "") %>%
  distinct(crop, region) %>%
  mutate(region_norm = norm(region))

guide_region_country <- region_to_country_df %>%
  select(region_country, country_name, Country_code, region_norm) %>%
  distinct()

crop_country_guide <- regions_per_crop_expanded %>%
  left_join(guide_region_country, by = "region_norm") %>%
  filter(!is.na(country_name) & country_name != "") %>%
  mutate(
    iso3 = ifelse(!is.na(Country_code) & Country_code != "", Country_code,
                  countrycode(country_name, origin = "country.name", destination = "iso3c", warn = FALSE))
  ) %>%
  select(crop, region = region_country, country_name, Country_code, iso3) %>%
  distinct()

richness_from_guide <- crop_country_guide %>%
  filter(!is.na(iso3) & iso3 != "") %>%
  distinct(iso3, crop) %>%
  group_by(iso3) %>%
  summarise(richness = n(), .groups = "drop") %>%
  rename(iso3 = iso3)

#PTFTW: save richness
#writexl::write_xlsx(richness_from_guide, "Regions/PTFTW/2025_12_15/country_richness_veglist3_ptftw_2025_12_15.xlsx")

#Priority: save richness
writexl::write_xlsx(richness_from_guide, "Regions/Priority/2025_12_15/country_richness_veglist2_priority_2025_12_15.xlsx")


assign("richness_from_guide", richness_from_guide, envir = .GlobalEnv)
richness <- richness_from_guide

# ---------------------------
# Map and plot (FIXED: normalize iso_a3 and fill -99 using countrycode + manual mappings)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, geometry, name_long = name_long, name = name)

# Fix common "-99" placeholders: infer from name_long (countrycode), special-case Kosovo, then manual map a few territories.
world_fixed <- world %>%
  mutate(
    iso_a3 = as.character(iso_a3),
    iso_a3_fixed = ifelse(iso_a3 == "-99" | is.na(iso_a3) | iso_a3 == "",
                          countrycode(name_long, origin = "country.name", destination = "iso3c", warn = FALSE),
                          iso_a3),
    # Kosovo often needs explicit handling
    iso_a3_fixed = ifelse(name_long == "Kosovo" & (is.na(iso_a3_fixed) | iso_a3_fixed == ""), "XKX", iso_a3_fixed),
    iso_a3_fixed = toupper(trimws(iso_a3_fixed))
  )

# Manual mapping for a few special features that countrycode can't resolve
manual_map <- data.frame(
  name_long = c("Somaliland", "Ashmore and Cartier Islands", "Indian Ocean Territories", "Siachen Glacier"),
  iso_manual = c("SOM", "AUS", "AUS", "IND"),
  stringsAsFactors = FALSE
)

world_fixed <- world_fixed %>%
  left_join(manual_map, by = "name_long") %>%
  mutate(
    iso_a3_fixed = ifelse(is.na(iso_a3_fixed) | iso_a3_fixed == "", iso_manual, iso_a3_fixed),
    iso_a3_fixed = toupper(trimws(iso_a3_fixed))
  ) %>%
  select(-iso_manual)

# Normalize richness iso3 column before joining
richness2 <- richness %>% mutate(iso3 = toupper(trimws(as.character(iso3))))

# Join using the fixed ISO3 field
map_data <- world_fixed %>% left_join(richness2, by = c("iso_a3_fixed" = "iso3"))

# PTFTW MAP:
# Make sure the plot has explicit white backgrounds so transparent areas are not rendered black by viewers
p <- ggplot(map_data) +
  geom_sf(aes(fill = richness), color = "gray40", size = 0.15) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "gray95", name = "Richness\n(# veg species)") +
  labs(title = "Region of diversity richness for 80 Plants That Feed the World vegetable species") +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "transparent"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Priority MAP:
# Make sure the plot has explicit white backgrounds so transparent areas are not rendered black by viewers
p <- ggplot(map_data) +
  geom_sf(aes(fill = richness), color = "gray40", size = 0.15) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "gray95", name = "Richness\n(# veg species)") +
  labs(title = "Region of diversity richness for 141 priority vegetable species") +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "transparent"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# PTFTW: Save PNG with ragg (high-quality)
ragg::agg_png(
  filename = "Regions/PTFTW/2025_12_15/Region_richness_map_veglist3_ptftw_2025_12_15.png",
  width = 12, height = 7, units = "in", res = 300
)
print(p)
dev.off()

# Priority: Save PNG with ragg (high-quality)
ragg::agg_png(
  filename = "Regions/Priority/2025_12_15/Region_richness_map_veglist2_priority_2025_12_15.png",
  width = 12, height = 7, units = "in", res = 300
)
print(p)
dev.off()

# ---------------------------
# Per-crop summary (GUIDE-DRIVEN)
countries_per_crop_from_guide <- crop_country_guide %>%
  mutate(dedup_key = ifelse(!is.na(iso3) & iso3 != "", iso3, country_name)) %>%
  group_by(crop) %>%
  summarise(
    countries_in_regions = paste(sort(unique(country_name)), collapse = "; "),
    country_count = n_distinct(dedup_key),
    .groups = "drop"
  )

regions_per_crop <- regions_per_crop_expanded %>%
  group_by(crop) %>%
  summarise(
    regions = paste(sort(unique(region)), collapse = "; "),
    region_count = n(),
    .groups = "drop"
  )

per_crop_regions_countries_summary <- regions_per_crop %>%
  full_join(countries_per_crop_from_guide, by = "crop") %>%
  mutate(
    regions = ifelse(is.na(regions), "", regions),
    region_count = ifelse(is.na(region_count), 0L, region_count),
    countries_in_regions = ifelse(is.na(countries_in_regions), "", countries_in_regions),
    country_count = ifelse(is.na(country_count), 0L, country_count)
  ) %>%
  arrange(crop)

assign("per_crop_regions_countries_summary", per_crop_regions_countries_summary, envir = .GlobalEnv)

print(head(per_crop_regions_countries_summary, 10))

#PTFTW: save summary
#writexl::write_xlsx(per_crop_regions_countries_summary, "Regions/PTFTW/2025_12_15/per_crop_regions_countries_summary_veglist3_ptftw_2025_12_15.xlsx")


#Priority: save summary
writexl::write_xlsx(per_crop_regions_countries_summary, "Regions/Priority/2025_12_15/per_crop_regions_countries_summary_veglist2_priority_2025_12_15.xlsx")


# End of script
