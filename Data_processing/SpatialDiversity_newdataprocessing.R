# Spatial Diversity

library(httr)
library(jsonlite)
install.packages("tidyr")
library(tidyr)
library(purrr)


# process new veglist 1
veglist1_not_stand <- read_excel("C:/Users/sarah/Downloads/Table S3_Extensive list of vegetable species_111125 (2).xlsx")

# proess legumes veglist 6
veglist6_legumes_not_stand <- read_csv("C:/Users/sarah/Downloads/Crop_legume_names (1).csv")

# load functions
source('Functions/query_taxa_resolver.R')   # import function query_taxa_resolver
source('Functions/extract_best_result.R') # import function extract_best_result

############### veg list 1 ################

# make a new field- fullTaxa with all data possible
veglist1_not_stand <- veglist1_not_stand %>%
  unite(fullTaxa, Genus, `Species epithet`, Authority, sep = " ", na.rm = TRUE, remove = FALSE)
df <- veglist1_not_stand

############### veg list 6 ################

# make a new field- fullTaxa with all data possible
veglist6_legumes_not_stand <- veglist6_legumes_not_stand %>%
  unite(fullTaxa, Taxon, Authority, sep = " ", na.rm = TRUE, remove = FALSE)
df <- veglist6_legumes_not_stand

# taxa list to be standardised
taxa_list <- unique(trimws(na.omit(df$fullTaxa))) #update to name of taxon column

#query API WFO
result_queries_WFO <- map(taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)

taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))

colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
taxa_standardized_df_WFO <- apply(taxa_standardized_df_WFO, 2, as.character)
#write.csv(taxa_standardized_df_WFO, 'Agrobio_veg_list/Outputs/veglist1_updated_vegetable_species_list_taxa_table_2025-11-12.csv', row.names = FALSE)
write.csv(taxa_standardized_df_WFO, 'Agrobio_veg_list/Outputs/veglist6_legume_species_list_taxa_table_2025-11-25.csv', row.names = FALSE)

taxa_standardized_df_WFO <- as.data.frame(taxa_standardized_df_WFO, stringsAsFactors = FALSE)
taxa_standardized_df_WFO <- taxa_standardized_df_WFO %>%
  rename(fullTaxa = input_name)
matched_names <- taxa_standardized_df_WFO %>%
  select(fullTaxa, output_name_WFO)
matched_names$fullTaxa <- as.character(matched_names$fullTaxa)

df_joined <- df %>%
  left_join(matched_names, by = "fullTaxa")

df_joined2 <- df_joined %>%
  rename(Standardized_taxa= output_name_WFO)


#save standardized veglist 1
df_joined2 <- apply(df_joined2, 2, as.character)
#write.csv(df_joined2, 'Agrobio_veg_list/Outputs/veglist1_updated_standardized_2025-11-12.csv', row.names = FALSE)

#save standardized veglist 6
df_joined2 <- apply(df_joined2, 2, as.character)
write.csv(df_joined2, 'Agrobio_veg_list/Outputs/veglist6_legumes_standardized_2025-11-25.csv', row.names = FALSE)





# Extract the New Species from veglist 1 to pull from GBIF
veglist1_updated <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist1_updated_extensive_vegetable_species_list_standardized_2025-11-12.xlsx")
new_species <- subset(veglist1_updated, !is.na(Comment) & Comment == "New species")

# Veglist 6 standardized to pull from GBIF:
# 156 species
veglist6_legumes <- read_csv('Agrobio_veg_list/Outputs/veglist6_legumes_standardized_2025-11-25.csv')

new_species <- veglist6_legumes

# -----------------------------------------------------------------------------
# Install and load required packages
packages <- c("pbapply", "readxl", "dplyr", "httr", "jsonlite", "tidyverse")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# -----------------------------------------------------------------------------

# Load veg list 1 updated
veglist1_updated <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist1_updated_extensive_vegetable_species_list_standardized_2025-11-12.xlsx")
#Extract the New Species to pull from GBIF
new_species <- subset(veglist1_updated, !is.na(Comment) & Comment == "New species")


# Full run across all taxa
gbif_data_list <- pbapply::pblapply(new_species$Taxon, get_gbif_data)

# Filter out NULLs and combine
gbif_data_clean <- gbif_data_list[!sapply(gbif_data_list, is.null)]
gbif_data_all <- bind_rows(gbif_data_clean)


# Save results
# 2,700 rows
#write_csv(gbif_data_all, "Agrobio_veg_list/Data/GBIF_pull/veglist_newspecies_gbif_pull_data_2025-11-25.csv")



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
# veglist 1 new speices result: 2,694 rows, 6 rows dropped
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  filter(basis_of_record != "FOSSIL_SPECIMEN")

# add Standardized_taxa field

# initialize as NA (character)
gbif_data_veglist_taxa$Standardized_taxa <- NA_character_

# set mapping for the specific name
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Erythrina edulis"] <- "Erythrina edulis Triana ex Micheli"
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Lupinus mutabilis"] <- "Lupinus mutabilis Sweet"
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Porophyllum linaria"] <- "Porophyllum linaria (Cav.) DC."
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Spilanthes oleracea"] <- "Spilanthes oleracea L."
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Tagetes lucida"] <- "Tagetes lucida Cav."
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Physalis peruviana"] <- "Physalis peruviana L."
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Salvia hispanica"] <- "Salvia hispanica L."
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Mesosphaerum suaveolens"] <- "Mesosphaerum suaveolens (L.) Kuntze"
gbif_data_veglist_taxa$Standardized_taxa[
  gbif_data_veglist_taxa$taxon_name_submitted == "Cestrum latifolium"] <- "Cestrum latifolium Lam."


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
write_csv(gbif_data_veglist_taxa, "Agrobio_veg_list/Data/GBIF_pull/veglist_newtaxa_gbif_pull_data_2025-11-25.csv")












# Remove FOSSIL_SPECIMEN
# veglist 6 new species result: 46,647 rows, 52 rows dropped
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  filter(basis_of_record != "FOSSIL_SPECIMEN")


# add Standardized_taxa field
veglist6_legumes <- read_csv('Agrobio_veg_list/Outputs/veglist6_legumes_standardized_2025-11-25.csv')

veglist6_legumes <- veglist6_legumes %>%
  select(Taxon, Standardized_taxa)

# join Standardized_taxa from veglist6 by Taxon
gbif_data_veglist_taxa <- gbif_data_veglist_taxa %>%
  left_join(
    veglist6_legumes %>% select(Taxon, Standardized_taxa),
    by = c("taxon_name_submitted" = "Taxon")
  )

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
write_csv(gbif_data_veglist_taxa, "Agrobio_veg_list/Data/GBIF_pull/veglist6_legumes_gbif_pull_data_2025-11-25.csv")


###### end script #########
