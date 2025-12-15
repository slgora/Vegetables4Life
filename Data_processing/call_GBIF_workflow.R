# -----------------------------------------------------------------------------
# Install and load required packages
packages <- c("pbapply", "readxl", "dplyr", "httr", "jsonlite", "tidyverse")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# -----------------------------------------------------------------------------
# Load GBIF taxa from the veg list that we dont have yet

#### Taxa list to pull from GBIF ###
# 152 taxa from first list
#GBIF_veglist_pull <- read_excel("C:/Users/sarah/Downloads/GBIF_taxa_veglist_pull.xlsx")


# veglists 1-4
veglist1 <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist1_extensive_vegetable_species_list_standardized_2025-11-03_v2.xlsx")
veglist2_priority <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist2_priority_vegetable_species_list_standardized_2025-11-03_v2.xlsx")
veglist3_ptftw <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03_v2.xlsx")
veglist4_cwr <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-03_v2.xlsx")


### combine all unique taxa into a species list ###

# safety checks
stopifnot("genus_species" %in% names(veglist1))
stopifnot("genus_species" %in% names(veglist2_priority))
stopifnot("genus_species" %in% names(veglist3_ptftw))
stopifnot("norm_taxa" %in% names(veglist4_cwr))

# extract, clean, combine
v1 <- as.character(veglist1$genus_species)
v2 <- as.character(veglist2_priority$genus_species)
v3 <- as.character(veglist3_ptftw$genus_species)
v4 <- as.character(veglist4_cwr$norm_taxa)    # note: use norm_taxa for veglist4 per your request

all_taxa <- c(v1, v2, v3, v4) %>%
  na.omit() %>%                        # remove NA
  as.character() %>%
  str_squish() %>%                     # trim and collapse internal whitespace
  .[. != ""] %>%                       # remove empty strings
  unique() %>%                         # deduplicate
  sort()                               # optional: sort alphabetically

taxa_df <- tibble::tibble(taxa = all_taxa)

taxa_df <- taxa_df %>%
  mutate(taxa = sub("^(\\s*)(\\S)", "\\1\\U\\2", taxa, perl = TRUE))

# view result
View(taxa_df)
# save
write_csv(taxa_df, "combined_veg_list_species.csv")


# Read in plant list from complementarity project
plant_list <- read_tsv("Outputs/WCFP_plantlist_standardized_2025-08-01.csv",
                     col_names = TRUE,
                     locale = locale(encoding = "UTF-8"))
# Fill blank/"" or NA Standardized_taxa with taxa
plant_list <- plant_list %>%
  mutate(
    # normalize whitespace and convert empty -> NA
    Standardized_taxa = na_if(str_squish(as.character(Standardized_taxa)), ""),
    taxa = as.character(taxa),
    # use taxa where Standardized_taxa is NA
    Standardized_taxa = coalesce(Standardized_taxa, taxa)
  )

# Make a list of all species that in all veg lists that are NOT in plant list
library(dplyr)
library(stringr)
library(purrr)

plant_list <- plant_list %>%
  mutate(Standardized_taxa = as.character(Standardized_taxa) %>% str_squish())

taxa_df <- taxa_df %>%
  mutate(taxa = as.character(taxa) %>% str_squish())

# unique, non-NA standardized taxa to speed matching
stds <- plant_list$Standardized_taxa %>% na.omit() %>% unique()

# for each taxa, check if it appears anywhere in any Standardized_taxa (case-insensitive)
is_found <- map_lgl(taxa_df$taxa, ~ any(str_detect(stds, fixed(.x, ignore_case = TRUE))))

missing_taxa_df <- taxa_df[!is_found, , drop = FALSE]

# save
View(missing_taxa_df)
write.csv(missing_taxa_df, "missing_taxa_in_veglists_not_in_plantlist.csv", row.names = FALSE)



# call it GBIF taxa pull
# 211 species
GBIF_veglist_pull <- missing_taxa_df

# -----------------------------------------------------------------------------
# install stringdist if you don't have it:
install.packages("stringdist")
library(stringdist)

# Normalization helper (defensive)
norm_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_squish(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")    # remove accents
  x <- tolower(x)
  x <- str_remove_all(x, "\\s*\\(.*?\\)\\s*")           # remove parentheses and their contents
  x <- str_replace_all(x, "[[:punct:]]", " ")           # convert punctuation to space
  x <- str_replace_all(x, "\\b(var|subsp|ssp|f|forma|cv)\\b.*", "")  # drop infraspecific markers and anything after
  x <- str_replace_all(x, "\\b(auth|ex)\\b.*", "")      # attempt to drop author fragments (best-effort)
  x <- str_squish(x)
  x
}

# Helper to pick the likely candidate column from a veglist dataframe
collect_candidates <- function(df, source_name) {
  candidate_cols <- c("Standardized_taxa", "genus_species", "norm_taxa", "taxa", "scientificName", "species")
  pick <- intersect(candidate_cols, names(df))
  col <- if (length(pick) > 0) pick[1] else names(df)[1]
  tibble(source = source_name, raw = as.character(df[[col]]))
}

# Build lookup table from veglists only (plant_list removed)
lookup <- bind_rows(
  collect_candidates(veglist1, "veglist1"),
  collect_candidates(veglist2_priority, "veglist2"),
  collect_candidates(veglist3_ptftw, "veglist3"),
  collect_candidates(veglist4_cwr, "veglist4")
) %>%
  filter(!is.na(raw), raw != "") %>%
  distinct() %>%
  mutate(norm = norm_name(raw)) %>%
  filter(norm != "") %>%
  # precompute tokens once
  mutate(tokens = map(norm, ~ if (.x == "") character(0) else str_split(.x, "\\s+")[[1]]))

# Ensure missing_taxa_df has normalized names
missing_taxa_df <- missing_taxa_df %>%
  mutate(taxa = as.character(taxa),
         norm = norm_name(taxa))

# Matching function accepts a normalized taxon string
match_taxon_candidates <- function(tax_norm, lookup_df, top_n = 3, sim_threshold = 0.82) {
  # defensive checks
  if (is.null(tax_norm) || length(tax_norm) == 0) {
    return(tibble(candidate = rep(NA_character_, top_n),
                  source = rep(NA_character_, top_n),
                  method = rep(NA_character_, top_n),
                  score = rep(NA_real_, top_n)))
  }

  tax_norm <- as.character(tax_norm)
  if (is.na(tax_norm) || tax_norm == "") {
    return(tibble(candidate = rep(NA_character_, top_n),
                  source = rep(NA_character_, top_n),
                  method = rep(NA_character_, top_n),
                  score = rep(NA_real_, top_n)))
  }

  # 1) exact normalized match
  exact <- lookup_df %>% filter(norm == tax_norm) %>% mutate(method = "exact", score = 1)

  # 2) token / Jaccard overlap
  tax_tokens <- if (tax_norm == "") character(0) else str_split(tax_norm, "\\s+")[[1]]
  token_scores <- lookup_df %>%
    mutate(jaccard = map_dbl(tokens, ~ {
      u <- union(.x, tax_tokens)
      if (length(u) == 0) return(0)
      length(intersect(.x, tax_tokens)) / length(u)
    })) %>%
    filter(jaccard > 0) %>%
    mutate(method = "token", score = jaccard)

  # 3) substring checks (lookup contains taxon OR taxon contains lookup)
  substr1 <- lookup_df %>% filter(str_detect(norm, fixed(tax_norm))) %>% mutate(method = "substr_lookup", score = 0.90)
  substr2 <- lookup_df %>% filter(str_detect(tax_norm, fixed(norm))) %>% mutate(method = "substr_taxon", score = 0.90)

  # 4) fuzzy similarity (Jaro-Winkler via stringsim) -> similarity in [0,1]
  sims <- stringsim(tax_norm, lookup_df$norm, method = "jw")
  fuzzy <- lookup_df %>% mutate(sim = sims) %>% filter(sim >= sim_threshold) %>% mutate(method = "jw", score = sim)

  # combine and pick top_n distinct candidates by norm+source
  combined <- bind_rows(exact, token_scores, substr1, substr2, fuzzy) %>%
    arrange(desc(score), method) %>%
    distinct(norm, source, .keep_all = TRUE)

  if (nrow(combined) == 0) {
    return(tibble(candidate = rep(NA_character_, top_n),
                  source = rep(NA_character_, top_n),
                  method = rep(NA_character_, top_n),
                  score = rep(NA_real_, top_n)))
  }

  out <- combined %>%
    slice_head(n = top_n) %>%
    transmute(candidate = raw, source, method, score)

  # pad if fewer than top_n
  if (nrow(out) < top_n) {
    out <- bind_rows(out, tibble(candidate = rep(NA_character_, top_n - nrow(out)),
                                 source = rep(NA_character_, top_n - nrow(out)),
                                 method = rep(NA_character_, top_n - nrow(out)),
                                 score = rep(NA_real_, top_n - nrow(out))))
  }

  out
}

# Run for all missing taxa (one-row-per-missing-taxon with candidate1..candidateN columns)
top_n <- 3
sim_threshold <- 0.82   # tune: higher -> stricter

matches_df <- map_dfr(seq_len(nrow(missing_taxa_df)), function(i) {
  tax_row <- missing_taxa_df[i, ]
  cand <- match_taxon_candidates(tax_row$norm, lookup, top_n = top_n, sim_threshold = sim_threshold)
  tibble(
    taxa = tax_row$taxa,
    candidate1 = cand$candidate[1], source1 = cand$source[1], method1 = cand$method[1], score1 = cand$score[1],
    candidate2 = cand$candidate[2], source2 = cand$source[2], method2 = cand$method[2], score2 = cand$score[2],
    candidate3 = cand$candidate[3], source3 = cand$source[3], method3 = cand$method[3], score3 = cand$score[3]
  )
})

# Save and inspect
write_csv(matches_df, "missing_taxa_candidate_matches_veglists_only.csv")

matched_df <- matches_df %>%
  select(taxa, candidate1) %>%
  rename(
    taxon_name_submitted = taxa,
    Standardized_taxa = candidate1
  )



# -----------------------------------------------------------------------------

### Full extract across all taxa ###
gbif_data_list <- pbapply::pblapply(GBIF_veglist_pull$taxa, get_gbif_data)

# Filter out NULLs and combine
gbif_data_clean <- gbif_data_list[!sapply(gbif_data_list, is.null)]
gbif_data_all <- bind_rows(gbif_data_clean)

# View results and check success count
View(gbif_data_all)
unique(gbif_data_all$lookup_name) #207 successfully pull taxa (all taxa not found in complementarity gbif data pull)

# Save results
# 63,300 rows
write_csv(gbif_data_all, "Agrobio_veg_list/Data/GBIF_pull/veglists_taxa_gbif_pull_data_2025-11-05.csv")



gbif_data_all <- read_csv("Agrobio_veg_list/Data/GBIF_pull/veglists_taxa_gbif_pull_data_2025-11-05.csv")




###### Clean GBIF data ######
# Filter out GBIF geo data
# Align to WFO naming
# Basis of Record” - delete FOSSIL_SPECIMEN


# Select fields and align to WFO naming
gbif_data_veglist_taxa <- gbif_data_all %>%
  select(
    taxon_name_submitted = lookup_name,
    basis_of_record       = basisOfRecord,
    latitude              = decimalLatitude,
    longitude             = decimalLongitude,
    country_code          = countryCode,
    country               = country,
    state_province        = stateProvince,
    locality              = locality,
    inst_code             = institutionCode,
    collection_code       = collectionCode )

# Remove FOSSIL_SPECIMEN
# result: 63,211 rows, 89 rows dropped
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  filter(basis_of_record != "FOSSIL_SPECIMEN")


##############################################################################
# Fill in Standardized_taxa by veg_list (matched_df)
library(dplyr)
library(stringr)
library(readr)

# Defensive checks
stopifnot("taxon_name_submitted" %in% names(matched_df))
stopifnot("Standardized_taxa" %in% names(matched_df))
stopifnot("taxon_name_submitted" %in% names(gbif_data_veglist_taxa))

# Ensure character and normalized whitespace for reliable joins
matched_lookup <- matched_df %>%
  mutate(
    taxon_name_submitted = as.character(taxon_name_submitted) %>% str_squish(),
    Standardized_taxa = as.character(Standardized_taxa)
  ) %>%
  distinct(taxon_name_submitted, .keep_all = TRUE) %>%
  select(taxon_name_submitted, Standardized_taxa)

gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  mutate(taxon_name_submitted = as.character(taxon_name_submitted) %>% str_squish(),
         # ensure column exists (create if missing)
         Standardized_taxa = if ("Standardized_taxa" %in% names(.) ) as.character(Standardized_taxa) else NA_character_)

# How many are missing before, 63,211
n_missing_before <- sum(is.na(gbif_data_veglist_taxa$Standardized_taxa) | gbif_data_veglist_taxa$Standardized_taxa == "")

# Left join the mapped Standardized_taxa (rename to avoid clobbering)
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  left_join(matched_lookup %>% rename(mapped_Standardized_taxa = Standardized_taxa),
            by = "taxon_name_submitted") %>%
  mutate(
    # prefer existing Standardized_taxa; otherwise use mapped_Standardized_taxa; convert empty -> NA then coalesce
    Standardized_taxa = na_if(str_squish(as.character(Standardized_taxa)), ""),
    mapped_Standardized_taxa = na_if(str_squish(as.character(mapped_Standardized_taxa)), ""),
    Standardized_taxa = coalesce(Standardized_taxa, mapped_Standardized_taxa)
  ) %>%
  select(-mapped_Standardized_taxa)

# How many were filled by this operation
n_missing_after <- sum(is.na(gbif_data_veglist_taxa$Standardized_taxa) | gbif_data_veglist_taxa$Standardized_taxa == "")
n_filled <- n_missing_before - n_missing_after

#view
View(gbif_data_veglist_taxa)


# Add a normalized genus_species field
# helper function to extract and normalize genus + species from standardized taxa
extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("(?i)(^|\\s)[×x](?=\\s|$)", "\\1") %>%   # remove standalone x or × robustly
    str_squish() %>%
    str_extract("^\\S+\\s+\\S+")
}
# Add genus_species field
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Add data_source and inst_type fields
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  mutate(
    data_source = "GBIF",
    inst_type = "Botanic garden")

# Save results
write_csv(gbif_data_veglist_taxa, "Agrobio_veg_list/Data/GBIF_pull/veglist_taxa_gbif_pull_data_2025-11-05.csv")



###### end script #########



