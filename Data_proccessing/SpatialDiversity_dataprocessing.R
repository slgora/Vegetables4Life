#### VEG LIST project for MAARTEN ###########
### started 2025_10_23

# Load required libraries
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(future.apply)
library(data.table)
library(purrr)
library(stringr)
library(tidyr)


## Get data from GBIF, genebanks (Genesys), botanic gardens
## - from GCCFP complementarity project.

#-----------------------------------------#
#--- Helper functions  ---#
#-----------------------------------------#

# helper function to extract and normalize genus + species from standardized taxa
extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("(?i)(^|\\s)[×x](?=\\s|$)", "\\1") %>%   # remove standalone x or × robustly
    str_squish() %>%
    str_extract("^\\S+\\s+\\S+")
}

# helper function to create a norm_taxa field
clean_taxon <- function(x) {
  re <- "(?xi)^\\s*([A-Z][^\\s]+)\\s+([^\\s]+)(?:\\s+(subsp\\.?|ssp\\.?|var\\.?|f\\.?|forma|subvar\\.?|cv\\.?|sect\\.?|subsect\\.?)(?:\\s+([^\\s]+))?)?"
  m <- stri_match_first_regex(x, re)
  out <- ifelse(is.na(m[,1]), NA_character_,
                paste0(m[,2], " ", m[,3],
                       ifelse(!is.na(m[,4]) & m[,4] != "", paste0(" ", m[,4],
                                                                  ifelse(!is.na(m[,5]) & m[,5] != "", paste0(" ", m[,5]), "")), "")))
  stri_trim_both(out)
}

#-----------------------------------------#
#--- Veg list and guide files read in  ---#
#-----------------------------------------#

### 1. Full veg list (~1,485 species)
### 2. Prioritized veg list (141 species)
### 3. PTFTW veg list (80 species)
### 4. CWRs veg list  (325 species) ??????
### 5. Americas veg list (59 species)

# note: 211 taxa NOT in plant list (complementarity) that ARE in the veg lists (1,4)
#veg_only_taxa <- read_csv("missing_taxa_in_veglists_not_in_plantlist.csv")



### 1. VEGLIST 1: extensive veg list (standardized)
# Revised table of all recorded vegetable taxa, standardized
#veglist1 <- read_csv("C:/Users/sarah/Downloads/veglist1_extensive_vegetable_species_list_standardized_2025-11-03.csv")
# updated, veglist1 = 1,485 species
veglist1 <-  read_csv('Agrobio_veg_list/Outputs/veglist1_updated_standardized_2025-11-12.csv')

# dont need to do this step with updated list
# drop duplicates with zero value Priority score, advised by Maarten
#veglist1 <- veglist1[-c(1054,  #Limnocharis flava duplicate
#                        #303,   #Ormocarpum cochinchinense duplicate, dont remove as advised by Maarten
#                        #1387,  #Terumnus labialis duplicate, dont remove as advised by Maarten
#                        1393), #Toona sinensis dupliate
#                     ]

#add genus_species field from Standardized_taxa for filter and match
veglist1 <- veglist1 %>% mutate(genus_species = extract_genus_species(Standardized_taxa))

# save new
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist1_updated_extensive_vegetable_species_list_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist1, path = out_file)

#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist1_updated_extensive_vegetable_species_list_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist1, path = out_file)

### 2. VEGLIST 2: Prioritized veg list (standardized)
# subset of veg list 1 (already standardized with genus_species field)
# - prioritized by at least two authority lists
# 147 taxa with priority score equal to 2 or above
# 141 taxa in updated list
veglist2_priority <- veglist1 %>%
  filter(`Priority score` >= 2)

# save new
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist2_priority_updated_vegetable_species_list_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist2_priority, path = out_file)

#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist2_priority_updated_vegetable_species_list_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist2_priority, path = out_file)



### 3. VEGLIST 3: 80 PTFTW veg list (standardized)
veglist3_PTFTW_standardized <- read_csv("C:/Users/sarah/Downloads/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03.csv")

#add genus_species field from Standardized_taxa for filter and match
veglist3_ptftw <- veglist3_PTFTW_standardized %>% mutate(genus_species = extract_genus_species(Standardized_taxa))

#save new
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03.xlsx"
writexl::write_xlsx(veglist3_ptftw, path = out_file)

#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03.xlsx"
writexl::write_xlsx(veglist3_ptftw, path = out_file)



### 4. VEGLIST 4: CWRs of 80 PTFTW veg list
veglist_ptftw_cwr_all <- read_csv("Agrobio_veg_list/Outputs/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-07.csv")

#add genus_species field from Standardized_taxa for filter and match
veglist_ptftw_cwr_all <- veglist_ptftw_cwr_all %>% mutate(genus_species = extract_genus_species(Standardized_taxa))

# filter for 80 PTFTW CWRs (veglist3) based on genus_species match
veglist4_cwr <- semi_join(veglist_ptftw_cwr_all, veglist3_ptftw, by = c("genus_species"))

# add a norm_taxa field for intraspecific taxa names (cwrs)
setDT(veglist4_cwr)
veglist4_cwr[, norm_taxa := clean_taxon(Standardized_taxa)]

#save new
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-07.xlsx"
writexl::write_xlsx(veglist4_cwr, path = out_file)

#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-07.xlsx"
writexl::write_xlsx(veglist4_cwr, path = out_file)



### 5. Americas veg list
# veglist5= 59 species
veglist5_latinamerica <- veglist1 %>%
  filter(!is.na(`Latin American road map`))

#save new
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist5_latin_america_vegetable_species_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist5_latinamerica, path = out_file)

#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist5_latin_america_vegetable_species_standardized_2025-11-12.xlsx"
writexl::write_xlsx(veglist5_latinamerica, path = out_file)



### 6. Legumes veglist
# veglist6 = 156 species
veglist6_legumes <- read_csv('Agrobio_veg_list/Outputs/veglist6_legumes_standardized_2025-11-25.csv')
#save
out_file <- "Agrobio_veg_list/Outputs/Final_veg_lists/Updated/veglist6_legume_vegetable_species_standardized_2025-11-25.xlsx"
writexl::write_xlsx(veglist6_legumes, path = out_file)







#### need to filter by the new veglists 1-6; note, 2025_11_25

#### NOTES: DONT KNOW IF WE NEED VEGLIST6 GBIF DATA
#####       MAY NEED TO REMOVE THE VEGLIST6 GBIF DATA FROM COMBING STEP
####        ALREADY HAVE OCCURENCE DATA IN DRIVE ANYWAY










#----------------------#
#--- Data read in ---#
#----------------------#

## Get data from GBIF, genebanks (Genesys), botanic gardens
## from GCCFP complementarity project.
### read in raw files (unfiltered) with taxon name standardized to WFO


##### GENESYS DATA #####
# 4,863,459 rows
WCFP_Genesys_data_all <- read_csv("WCFP_Genesys_data_all_standardized_2025_08_01.csv")


##### BGCI DATA #####
# 614,248 rows
WCFP_BGCI_data_all <- read_csv("WCFP_BGCI-PlantSearch_data_standardized_2025_08_01.csv")


##### CANO DATA #####
# 386,313 rows
#WCFP_Cano_data_all <- read_csv("Data_processed/WCFP_Cano_data_processed_2025-08-15.csv")
WCFP_Cano_data_all <- read_csv("Data_processed/WCFP_Cano_data_processed_2025-10-31.csv")


###### GBIF DATA #####
## Combine complementarity gbif data pull (plant list) with gbif data pull
#  of veglist taxa explicitly NOT found in complementarity data pull and filter

# GBIF WCFP observations data (extracted based on plant list standardized names to WFO)
# not all data has coordinates, filter out later
# 7,493,620 rows
WCFP_GBIF_data_all_standardized <- read_csv("Agrobio_veg_list/Data/WCFP_GBIF_data_all_2025-11-05.csv")

### GBIF veglist data pull for other taxa on veg list (NOT found in complementarity data pull)
# not all data has coordinates, filter out later
# 63,211 rows
veglist_GBIF_data_standardized <- read_csv("Agrobio_veg_list/Data/GBIF_pull/veglist_taxa_gbif_pull_data_2025-11-05.csv")

### GBIF veglist NEW SPECIES (Nov 2025)
# not all data has coordinates, filter out later
# 2,694 rows
veglist_newspecies_GBIF_data_standardized <- read_csv("Agrobio_veg_list/Data/GBIF_pull/veglist_newtaxa_gbif_pull_data_2025-11-25.csv")

### Dont add?? already in GBFIF data above I think
### GBIF veglist 6 legume species (NOV 2025)
# not all data has coordinates, filter out later
# 46,647 rows
#veglist_legumes_GBIF_data_standardized <- read_csv("Agrobio_veg_list/Data/GBIF_pull/veglist6_legumes_gbif_pull_data_2025-11-25.csv")

## combine
# 7,606,172 total rows
#WCFP_GBIF_data_all <- rbind(WCFP_GBIF_data_all_standardized, veglist_GBIF_data_standardized, veglist_newspecies_GBIF_data_standardized, veglist_legumes_GBIF_data_standardized)

# combine
# 7,559,525 total rows
WCFP_GBIF_data_all <- rbind(WCFP_GBIF_data_all_standardized, veglist_GBIF_data_standardized, veglist_newspecies_GBIF_data_standardized)



## Rename dataset names for easier processing below:
genesys_df_all <- WCFP_Genesys_data_all
bgci_df_all <- WCFP_BGCI_data_all
cano_df_all <- WCFP_Cano_data_all
gbif_df_all <- WCFP_GBIF_data_all





#----------------------#
#--- Data processing ---#
#----------------------#

## Remove the weird parenthesis from Genesys field
genesys_df_all <- genesys_df_all %>%
  mutate(taxon_name_submitted = if_else(
    is.na(taxon_name_submitted),
    NA_character_,
    str_replace_all(
      taxon_name_submitted,
      "([[:alpha:]'’`-]+\\s+[[:alpha:]'’`-]+)\\(",
      "\\1 ("
    )
  ))

# If Standardized_taxa field has no_match, then replace with original name
# Genesys
genesys_df_all$Standardized_taxa <- ifelse(tolower(genesys_df_all$Standardized_taxa) == "no_match",
                                           genesys_df_all$taxon_name_submitted,
                                           genesys_df_all$Standardized_taxa)
# BGCI
bgci_df_all$Standardized_taxa <- ifelse(tolower(bgci_df_all$Standardized_taxa) == "no_match",
                                           bgci_df_all$taxon_name_submitted,
                                           bgci_df_all$Standardized_taxa)

# Cano
cano_df_all$Standardized_taxa <- ifelse(tolower(cano_df_all$Standardized_taxa) == "no_match",
                                        cano_df_all$taxa,
                                        cano_df_all$taxa)


## Add institutions info (based on what institution data is avail per dataset)
#WIEWS_instIDs <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")
WIEWS_instIDs <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")
WIEWS_instIDs_dedup <- WIEWS_instIDs %>% group_by(ORGNAME_L) %>% slice(1) %>% ungroup()

bgci_df_all <- bgci_df_all %>%
  left_join(WIEWS_instIDs_dedup, by = c("ex_situ_site_name" = "ORGNAME_L")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_code = WIEWS_INSTCODE)

genesys_df_all <- genesys_df_all %>%
  left_join(WIEWS_instIDs, by = c("inst_code" = "WIEWS_INSTCODE")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_name = ORGNAME_L)

## Assign institution type for selected institutions
bgci_df_all <- bgci_df_all %>%
  mutate(inst_type = ifelse(ex_situ_site_name %in% c("U.S. National Plant Germplasm System", "Seeds of Success (SOS)"),
                            "Genebank", "Botanic garden"))
genesys_df_all <- genesys_df_all %>%
  mutate(inst_type = ifelse(inst_code %in% c("GBR004", "USA151", "DEU502", "NLD020", "BEL014", "DEU022", "DEU515", "CHE100", "POL001", "DEU156", "CHE006", "LTU010", "ESP218", "ARM010", "POL022", "DEU078", "LVA019", "GEO002"),
                            "Botanic garden", "Genebank"))

## Remove duplicates
bgci_df_all <- bgci_df_all[!bgci_df_all$ex_situ_site_name %in% c("U.S. National Plant Germplasm System", "Seeds of Success (SOS)", "Millennium Seed Bank", "United States National Arboretum"), ]
genesys_df_all <- genesys_df_all[genesys_df_all$inst_code != "YUG001", ]

# Rename LC to inst_code
cano_df_all <- cano_df_all %>% rename(inst_code = LC)



###### Fill inst_name if NA
# Read in guide files
inst_names_long_SG <- read_excel("Data/inst_names_long_SG.xlsx")
WIEWS_inst_names <- read_excel("G:/.shortcut-targets-by-id/1s1xfW_iLmsvnSftrb38wREf8b0QSuicc/GCCFP/Data and analyses/Food plant distributions/FAO_WIEWS_organizations_PG.xlsx") %>%
  select(inst_code = `WIEWS instcode`, inst_name_fill = `Name of organization`)

# Genesys: fill inst_name based on inst_code
genesys_df_all <- genesys_df_all %>%
  left_join(WIEWS_inst_names, by = c("inst_code")) %>%
  mutate(
    inst_name = ifelse(is.na(inst_name) | inst_name == "", inst_name_fill, inst_name)
  ) %>%
  select(-inst_name_fill)

# BGCI: rename ex_situ_site_name to inst_name
bgci_df_all <- bgci_df_all %>%
  rename(inst_name = ex_situ_site_name)

# Cano: fill in inst_name based on inst_code from guide file
cano_df_all$inst_name <- rep(NA_character_, nrow(cano_df_all))
cano_df_all <- cano_df_all %>%
  left_join(
    inst_names_long_SG %>% select(inst_code, inst_name_ref = inst_name),
    by = "inst_code"
  ) %>%
  mutate(
    inst_name = ifelse(is.na(inst_name), inst_name_ref, inst_name)
  ) %>%
  select(-inst_name_ref)

# GBIF : fill inst_name from WIEWS and then from inst_names_long_SG
gbif_df_all$inst_name <- rep(NA_character_, nrow(gbif_df_all))
gbif_df_all <- gbif_df_all %>%
  left_join(WIEWS_inst_names %>% rename(inst_name = inst_name_fill), by = "inst_code") %>%
  mutate(
    inst_name = ifelse(is.na(inst_name.x) | inst_name.x == "", inst_name.y, inst_name.x)
  ) %>%
  select(-inst_name.x, -inst_name.y)

gbif_df_all <- gbif_df_all %>%
  left_join(
    inst_names_long_SG %>% select(inst_code, inst_name_ref = inst_name),
    by = "inst_code"
  ) %>%
  mutate(
    inst_name = ifelse(is.na(inst_name), inst_name_ref, inst_name)
  ) %>%
  select(-inst_name_ref)


## Assign institution status by guide file
# only for Genesys
internationalgenebanks_list <- read_excel("Data/internationalgenebanks_list.xlsx")
international_codes <- internationalgenebanks_list$instCode
genesys_df_all <- genesys_df_all %>%
  mutate(inst_status = ifelse(inst_code %in% international_codes, "International", "National"))

## Assign storage type
# only for BGCI
bgci_df_all <- bgci_df_all %>%
  mutate(storage_type = paste(
    ifelse(germplasm_plant == 1, "plant", NA),
    ifelse(germplasm_seed == 1, "seed", NA),
    ifelse(germplasm_pollen == 1, "pollen", NA),
    ifelse(germplasm_explant == 1, "explant", NA),
    sep = "; "
  )) %>%
  mutate(storage_type = gsub("(^NA; |; NA$|; NA; |NA)", "", storage_type)) %>%
  mutate(storage_type = ifelse(storage_type == "", NA, storage_type))



###### Add normalized genus_species fields from Standardized_taxa

# Genesys
genesys_df_all <- genesys_df_all %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# BGCI
bgci_df_all <- bgci_df_all %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Cano
cano_df_all <- cano_df_all %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# GBIF, already has this
#gbif_df_all <- gbif_df_all %>%
#  mutate(genus_species = extract_genus_species(Standardized_taxa))


### Decode encrypted genesys geo data

# Read original raw Genesys file
WCFP_Genesys_data_all_raw <- read.csv("Data/colin_dataset_from_Christelle_2025-03-25.csv", sep = ";")
# Replace lat/lon by uuid in genesys (lat/long encrypted)
genesys_df_all <- genesys_df_all  %>%
  left_join(
    WCFP_Genesys_data_all_raw %>%
      select(UUID, DECLATITUDE, DECLONGITUDE),
    by = c("uuid" = "UUID")
  ) %>%
  select(-latitude, -longitude) %>%
  rename(
    latitude = DECLATITUDE,
    longitude = DECLONGITUDE
  ) %>%
  mutate(
    latitude = as.numeric(str_replace(latitude, ",", ".")),
    longitude = as.numeric(str_replace(longitude, ",", "."))
  )


##### Add record_type field (G or H)

# Genesys ex-situ (G):
genesys_df_all <- genesys_df_all %>%
  mutate(record_type = "G")

# BGCI ex-situ (G):
bgci_df_all <- bgci_df_all %>%    #### is this correct?
  mutate(record_type = "G")

# Cano ex-situ (G):
cano_df_all <- cano_df_all %>%
  mutate(record_type = "G")

# GBIF observations (H):
# remove basis_of_record= fossil and living specimen, call gbif_filtered_observations_df
gbif_df_all_observations <- gbif_df_all %>%
  filter(!basis_of_record %in% c("FOSSIL_RECORD", "LIVING_SPECIMEN")) %>%
  mutate(record_type = "H")

# GBIF ex-situ (G):
# select only basis_of_record = living specimen, call gbif_filtered_botanicgarden_df
gbif_df_all_botanicgardens <- gbif_df_all %>%
  filter(basis_of_record %in% c("LIVING_SPECIMEN")) %>%
  mutate(record_type = "G")


# add inst_
# Add data_source and inst_type fields
cano_df_all <- cano_df_all %>%
  mutate(
    data_source = "Cano",
    inst_type = "Botanic garden")


# save NEW 2025_12_11
write.csv(genesys_df_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Genesys_data_all.csv', row.names = FALSE)
write.csv(bgci_df_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_BGCI_data_all.csv', row.names = FALSE)
write.csv(cano_df_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Cano_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_observations, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_observations_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_botanicgardens, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv', row.names = FALSE)


## View semi-processed NEW data, unfiltered for veg list
nrow(genesys_df_all) # 4,857,984 rows
nrow(bgci_df_all) # 594,926 rows
nrow(cano_df_all) # 386,313 rows
nrow(gbif_df_all_observations) # 7,431,218 rows, data not yet filtered to drop NAs in lat/long
nrow(gbif_df_all_botanicgardens) # 128,307 rows, data not yet filtered to drop NAs in lat/long

# add a norm_taxa field to new data for intraspecific taxa names (cwrs)
genesys_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Genesys_data_all.csv")
setDT(genesys_df_all)
genesys_df_all[, norm_taxa := clean_taxon(Standardized_taxa)]

cano_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Cano_data_all.csv")
setDT(cano_df_all)
cano_df_all[, norm_taxa := clean_taxon(Standardized_taxa)]

gbif_df_all_observations <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_observations_data_all.csv")
setDT(gbif_df_all_observations)
gbif_df_all_observations[, norm_taxa := clean_taxon(Standardized_taxa)]

gbif_df_all_botanicgardens <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv")
setDT(gbif_df_all_botanicgardens)
gbif_df_all_botanicgardens[, norm_taxa := clean_taxon(Standardized_taxa)]

# save NEW with norm_taxa field 2025_12_11
write.csv(genesys_df_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_Genesys_data_all.csv', row.names = FALSE)
write.csv(cano_df_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_Cano_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_observations, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_GBIF_observations_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_botanicgardens, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_GBIF_botanicgardens_data_all.csv', row.names = FALSE)




# save old data
write.csv(genesys_df_all, 'Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Genesys_data_all.csv', row.names = FALSE)
write.csv(bgci_df_all, 'Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_BGCI_data_all.csv', row.names = FALSE)
write.csv(cano_df_all, 'Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Cano_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_observations, 'Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_observations_data_all.csv', row.names = FALSE)
write.csv(gbif_df_all_botanicgardens, 'Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv', row.names = FALSE)

## View semi-processed old data, unfiltered for veg list
nrow(genesys_df_all) # 4,857,984 rows
nrow(bgci_df_all) # 594,926 rows
nrow(cano_df_all) # 386,313 rows
nrow(gbif_df_all_observations) # 7,428,674 rows, data not yet filtered to drop NAs in lat/long
nrow(gbif_df_all_botanicgardens) # 128,157 rows, data not yet filtered to drop NAs in lat/long



#---------------------------#
#--- Filter for veg list ---#
#---------------------------#

# Note: filter for veg list based on genus + species match of standardized taxa

####### Normalize genus_species field in veg_list for matching #######
# Create normalized genus_species column from Standardized_taxa field

# read in veg lists 1,2,3,5 NEW

veglist1 <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist1_updated_extensive_vegetable_species_list_standardized_2025-11-12.xlsx")
veglist2 <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist2_priority_updated_vegetable_species_list_standardized_2025-11-12.xlsx")
veglist3 <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03.xlsx")
veglist5 <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist5_latin_america_vegetable_species_standardized_2025-11-12.xlsx")

# read in veg lists 1-3 old
#veglist1 <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist1_extensive_vegetable_species_list_standardized_2025-11-03_v2.xlsx")
#veglist2_priority <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist2_priority_vegetable_species_list_standardized_2025-11-03_v2.xlsx")
#veglist3_ptftw <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist3_80vegetable_species_plants_that_feed_the_world_list_standardized_2025-11-03_v2.xlsx")

# set veg_list for processing below
#veg_list <- veglist1
#veg_list <- veglist2
#veg_list <- veglist3
veg_list <- veglist5


##########################################################
####### Filtering Genesys for veg list taxa 1-3, 5 #######
##########################################################

# Step 1: Read in pre-processed Genesys data
#new data
genesys_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Genesys_data_all.csv")
#old data
#genesys_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Genesys_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(genus_species) %>%
  pivot_longer(everything(), values_to = "genus_species") %>%
  filter(!is.na(genus_species)) %>%
  distinct()

# Filter exact matches
genesys_df_all_filtered_exact <- genesys_df_all %>%
  semi_join(exact_terms, by = "genus_species")

# Get remaining unmatched rows
remaining <- anti_join(genesys_df_all, exact_terms, by = "genus_species")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["genus_species"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
genesys_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
genesys_df_filtered <- bind_rows(genesys_df_all_filtered_exact, genesys_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
genesys_df_filtered <- genesys_df_filtered %>%
  filter(!genus_species %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- genesys_df_filtered %>%
  pull(genus_species) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))


############ NEW ##############

### SAVE FILTER BY VEGLIST 1 ###
# filtered Genesys dataset has 1,210,547 rows
#write.csv(genesys_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist1/WCFP_Genesys_data_filtered_veglist1_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 2 ###
# filtered Genesys dataset has 635,353 rows
#write.csv(genesys_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist2/WCFP_Genesys_data_filtered_veglist2_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 3 ###
# filtered Genesys dataset has 256,285 rows
#write.csv(genesys_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist3/WCFP_Genesys_data_filtered_veglist3_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 5 ###
# filtered Genesys dataset has 373,267 rows
#write.csv(genesys_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist5/WCFP_Genesys_data_filtered_veglist5_2025-12-11.csv', row.names = FALSE)



############ OLD ##############
### SAVE FILTER BY VEGLIST 1 ###
# filtered Genesys dataset has 1,208,666 rows
#write.csv(genesys_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_Genesys_data_filtered_veglist1_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 2 ###
# filtered Genesys dataset has 635,370 rows
#write.csv(genesys_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_Genesys_data_filtered_veglist2_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 3 ###
# filtered Genesys dataset has 256,285 rows
#write.csv(genesys_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_Genesys_data_filtered_veglist3_2025-11-04.csv', row.names = FALSE)




#######################################################
####### Filtering BGCI for veg list taxa 1-3 ##########
#######################################################

# Step 1: Read in pre-processed BGCI data
bgci_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_BGCI_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 3: Filter all matching rows
bgci_df_filtered <- bgci_df_all %>%
  filter(str_detect(genus_species, regex(pattern)))

# Step 4: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
bgci_df_filtered <- bgci_df_filtered %>%
  filter(!genus_species %in% exclude)

# Step 5. Check matched + unmatched taxa = number of taxa in veg list
# veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- bgci_df_filtered %>%
  pull(genus_species) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

### SAVE FILTER BY VEGLIST 1 ###
# filtered BGCI dataset has 70,383 rows
#write.csv(bgci_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_BGCI_data_filtered_veglist1_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 2 ###
# filtered BGCI dataset has 11,073 rows
#write.csv(bgci_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_BGCI_data_filtered_veglist2_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 3 ###
# filtered BGCI dataset has 8,768 rows
#write.csv(bgci_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_BGCI_data_filtered_veglist3_2025-11-04.csv', row.names = FALSE)





##############################################################
####### Filtering Cano for veg list taxa 1-3,5 #################
##############################################################

# Step 1: Read in pre-processed Cano data
# new
cano_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_Cano_data_all.csv")
#old
#cano_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Cano_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 3: Filter all matching rows
cano_df_filtered <- cano_df_all %>%
  filter(str_detect(genus_species, regex(pattern)))

# Step 4: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" was found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
cano_df_filtered <- cano_df_filtered %>%
  filter(!genus_species %in% exclude)

# Step 5. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
matched_taxa <- cano_df_filtered %>%
  pull(genus_species) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

################ NEW###########
### SAVE FILTER BY VEGLIST 1 ###
# filtered Cano dataset has 29,106 rows
#write.csv(cano_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist1/WCFP_Cano_data_filtered_veglist1_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 2 ###
# filtered Cano dataset has 3,055 rows
#write.csv(cano_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist2/WCFP_Cano_data_filtered_veglist2_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 3 ###
# filtered Cano dataset has 2,563 rows
#write.csv(cano_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist3/WCFP_Cano_data_filtered_veglist3_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 5 ###
# filtered Cano dataset has 1,673 rows
#write.csv(cano_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist5/WCFP_Cano_data_filtered_veglist5_2025-12-11.csv', row.names = FALSE)




################ OLD ###########
### SAVE FILTER BY VEGLIST 1 ###
# filtered Cano dataset has 29,059 rows
#write.csv(cano_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_Cano_data_filtered_veglist1_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 2 ###
# filtered Cano dataset has 3,027 rows
#write.csv(cano_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_Cano_data_filtered_veglist2_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 3 ###
# filtered Cano dataset has 2,563 rows
#write.csv(cano_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_Cano_data_filtered_veglist3_2025-11-04.csv', row.names = FALSE)






####################################################################
####### Filtering GBIF Observations for veg list taxa 1-3,5 ##########
####################################################################

# Step 1: Read in pre-processed GBIF observations data
#new
gbif_df_all_observations <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_observations_data_all.csv")
#old
#gbif_df_all_observations <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_observations_data_all.csv")

# Step 2: Prepare genus-species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(genus_species) %>%
  pivot_longer(everything(), values_to = "genus_species") %>%
  filter(!is.na(genus_species)) %>%
  distinct()

# Filter exact matches
gbif_df_filtered_exact <- gbif_df_all_observations %>%
  semi_join(exact_terms, by = "genus_species")

# Get remaining unmatched rows
remaining <- anti_join(gbif_df_all_observations, exact_terms, by = "genus_species")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["genus_species"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
gbif_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
gbif_df_filtered <- bind_rows(gbif_df_filtered_exact, gbif_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
gbif_df_filtered <- gbif_df_filtered %>%
  filter(!genus_species %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
matched_taxa <- gbif_df_filtered %>%
  pull(genus_species) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
#View(data.frame(unmatched_taxa = unmatched_taxa))


############## NEW ####################

### SAVE FILTER BY VEGLIST 1 ###
# filtered GBIF data is 458,501  rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_observations_data_filtered_veglist1_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 2 ###
# filtered GBIF data is 57,258 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_observations_data_filtered_veglist2_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 3 ###
# filtered GBIF data is 40,910 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_observations_data_filtered_veglist3_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 5 ###
# filtered GBIF data is 22,889 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist5/WCFP_GBIF_observations_data_filtered_veglist5_2025-12-11.csv', row.names = FALSE)




############## OLD ####################
### SAVE FILTER BY VEGLIST 1 ###
# filtered GBIF data is 454,344 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_observations_data_filtered_veglist1_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 2 ###
# filtered GBIF data is 59,075 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_observations_data_filtered_veglist2_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 3 ###
# filtered GBIF data is 40,910 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_observations_data_filtered_veglist3_2025-11-04.csv', row.names = FALSE)





######################################################################
####### Filtering GBIF Botanic Gardens for veg list taxa 1-3,5 ##########
#######################################################################

# Step 1: Read in pre-processed GBIF observations data
#new
gbif_df_all_botanicgardens <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv")
#old
#gbif_df_all_botanicgardens <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv")

# Step 2: Prepare genus-species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(genus_species) %>%
  pivot_longer(everything(), values_to = "genus_species") %>%
  filter(!is.na(genus_species)) %>%
  distinct()

# Filter exact matches
gbif_df_filtered_exact <- gbif_df_all_botanicgardens %>%
  semi_join(exact_terms, by = "genus_species")

# Get remaining unmatched rows
remaining <- anti_join(gbif_df_all_botanicgardens, exact_terms, by = "genus_species")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["genus_species"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
gbif_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
gbif_df_filtered <- bind_rows(gbif_df_filtered_exact, gbif_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides")
gbif_df_filtered <- gbif_df_filtered %>%
  filter(!genus_species %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
matched_taxa <- gbif_df_filtered %>%
  pull(genus_species) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

################ NEW ##############
### SAVE FILTER BY VEGLIST 1 ###
# filtered GBIF data is 14,184 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_botanicgardens_data_filtered_veglist1_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 2 ###
# filtered GBIF data is 3,903 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_botanicgardens_data_filtered_veglist2_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 3 ###
# filtered GBIF data is 2,873 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_botanicgardens_data_filtered_veglist3_2025-12-11.csv', row.names = FALSE)

### SAVE FILTER BY VEGLIST 5 ###
# filtered GBIF data is 1,993 rows
#write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist5/WCFP_GBIF_botanicgardens_data_filtered_veglist5_2025-12-11.csv', row.names = FALSE)




################ OLD ##############
### SAVE FILTER BY VEGLIST 1 ###
# filtered GBIF data is 13,852 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_botanicgardens_data_filtered_veglist1_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 2 ###
# filtered GBIF data is 3,885 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_botanicgardens_data_filtered_veglist2_2025-11-04.csv', row.names = FALSE)
### SAVE FILTER BY VEGLIST 3 ###
# filtered GBIF data is 2,873 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_botanicgardens_data_filtered_veglist3_2025-11-04.csv', row.names = FALSE)







################################################################################
########################## Veg list 4 cwrs #####################################
################################################################################

# read in veg list 4
#new
veglist4_cwr <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_lists/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-07.xlsx")
#old
#veglist4_cwr <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-03_v2.xlsx")
#veglist4_cwr <- read_excel("Agrobio_veg_list/Outputs/Final_veg_lists/veglist4_80vegetable_species_plants_that_feed_the_world_CWRs_list_standardized_2025-11-07_v3.xlsx")
veg_list <- veglist4_cwr
# note: filter by norm_taxa for cwrs, matched Standardized_taxa

######################################################
####### Filtering Genesys for veg list 4 taxa  #######
######################################################

# Step 1: Read in pre-processed Genesys data (has norm_tax field)
#new
genesys_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_Genesys_data_all.csv")
#old
#genesys_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Genesys_data_all2.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$norm_taxa) %>%  #or Standardized_taxa
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(norm_taxa) %>%  #or Standardized_taxa
  pivot_longer(everything(), values_to = "norm_taxa") %>% #or Standardized_taxa
  filter(!is.na(norm_taxa)) %>%
  distinct()

# Filter exact matches
genesys_df_all_filtered_exact <- genesys_df_all %>%
  semi_join(exact_terms, by = "norm_taxa")    #or Standardized_taxa

# Get remaining unmatched rows
remaining <- anti_join(genesys_df_all, exact_terms, by = "norm_taxa")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["norm_taxa"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
genesys_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
genesys_df_filtered <- bind_rows(genesys_df_all_filtered_exact, genesys_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
exclude <- c("Capsicum baccatum var. umbilicatum")
genesys_df_filtered <- genesys_df_filtered %>%
  filter(!norm_taxa %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- genesys_df_filtered %>%
  pull(norm_taxa) %>% #or Standardized_taxa
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))


########## NEW ###########
### SAVE FILTER BY VEGLIST 4 ###
# filtered Genesys dataset has 251,070 rows
#write.csv(genesys_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist4/WCFP_Genesys_data_filtered_veglist4_2025-12-11.csv', row.names = FALSE)


########## OLD ###########
### SAVE FILTER BY VEGLIST 4 ###
# filtered Genesys dataset has 254,248 rows
#write.csv(genesys_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_Genesys_data_filtered_veglist4_2025-11-04.csv', row.names = FALSE)







###################################################
####### Filtering BGCI for veg list 4 taxa ##########
###################################################

# Step 1: Read in pre-processed BGCI data
bgci_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_BGCI_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$norm_taxa) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 3: Filter all matching rows
bgci_df_filtered <- bgci_df_all %>%
  filter(str_detect(norm_taxa, regex(pattern)))

# Step 4: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
#exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
#bgci_df_filtered <- bgci_df_filtered %>%
#  filter(!Standardized_taxa %in% exclude)

# Step 5. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- bgci_df_filtered %>%
  pull(Standardized_taxa) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
View(data.frame(matched_taxa = matched_taxa))
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

### SAVE FILTER BY VEGLIST 4 ###
# filtered BGCI dataset has 8,139 rows
write.csv(bgci_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_BGCI_data_filtered_veglist4_2025-11-04.csv', row.names = FALSE)









##########################################################
####### Filtering Cano for veg list 4 taxa #################
##########################################################

# Step 1: Read in pre-processed Cano data
#new
cano_df_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed//norm_taxa/WCFP_Cano_data_all.csv")
#old
#cano_df_all <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_Cano_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$norm_taxa) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 3: Filter all matching rows
cano_df_filtered <- cano_df_all %>%
  filter(str_detect(norm_taxa, regex(pattern)))

# Step 4: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" was found WITHIN "mussaenda glabrata" (not in veg_list) and matched
#exclude <- c("gnetum gnemonoides", "mussaenda glabrata", "persicaria hydropiperoides", "cajanus cajanifolius")
#cano_df_filtered <- cano_df_filtered %>%
#  filter(!genus_species %in% exclude)

# Step 5. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
matched_taxa <- cano_df_filtered %>%
  pull(norm_taxa) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
View(data.frame(matched_taxa = matched_taxa))
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

########## NEW ########
### SAVE FILTER BY VEGLIST 4 ###
# filtered Cano dataset has 2,437 rows
write.csv(cano_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist4/WCFP_Cano_data_filtered_veglist4_2025-12-11.csv', row.names = FALSE)


########## OLD ########
### SAVE FILTER BY VEGLIST 4 ###
# filtered Cano dataset has 2,268 rows
#write.csv(cano_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_Cano_data_filtered_veglist4_2025-11-04.csv', row.names = FALSE)






################################################################
####### Filtering GBIF Observations for veg list 4 taxa ##########
################################################################

# Step 1: Read in pre-processed GBIF observations data
#new
gbif_df_all_observations <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_GBIF_observations_data_all.csv")
#old
#gbif_df_all_observations <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_observations_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$norm_taxa) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(norm_taxa) %>%
  pivot_longer(everything(), values_to = "norm_taxa") %>%
  filter(!is.na(norm_taxa)) %>%
  distinct()

# Filter exact matches
gbif_df_all_filtered_exact <- gbif_df_all_observations %>%
  semi_join(exact_terms, by = "norm_taxa")

# Get remaining unmatched rows
remaining <- anti_join(gbif_df_all_observations, exact_terms, by = "norm_taxa")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["norm_taxa"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
gbif_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
gbif_df_filtered <- bind_rows(gbif_df_all_filtered_exact, gbif_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
#exclude <- c("Brassica rapa L. subsp. pekinensis x b. oleracea l. (Lour.) Hanelt", "Beta vulgaris L. subsp. maritima x beta vulgaris l. subsp. vulgaris (L.) Arcang.", "Solanum lycopersicum L. lycopersicum x cerasiforme")
#gbif_df_filtered <- gbif_df_filtered %>%
#  filter(!Standardized_taxa %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- gbif_df_filtered %>%
  pull(norm_taxa) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
View(data.frame(matched_taxa = matched_taxa))
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

############# NEW #############
### SAVE FILTER BY VEGLIST 4 ###
# filtered GBIF data is 40,014 rows
write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_observations_data_filtered_veglist4_2025-12-11.csv', row.names = FALSE)


############ OLD #############
### SAVE FILTER BY VEGLIST 4 ###
# filtered GBIF data is 40,066 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_observations_data_filtered_veglist4_2025-11-04.csv', row.names = FALSE)







################################################################
####### Filtering GBIF Botanic Gardens for veg list 4 taxa ##########
################################################################

# Step 1: Read in pre-processed GBIF observations data
#new
gbif_df_all_botanicgardens <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_preprocessed/norm_taxa/WCFP_GBIF_botanicgardens_data_all.csv")
#old
#gbif_df_all_botanicgardens <- read_csv("Agrobio_veg_list/Outputs/Data_preprocessed/WCFP_GBIF_botanicgardens_data_all.csv")

# Step 2: Prepare genus_species terms from veg list
pattern_terms <- c(veg_list$norm_taxa) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized filtering for x-large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(norm_taxa) %>%
  pivot_longer(everything(), values_to = "norm_taxa") %>%
  filter(!is.na(norm_taxa)) %>%
  distinct()

# Filter exact matches
gbif_df_all_filtered_exact <- gbif_df_all_botanicgardens %>%
  semi_join(exact_terms, by = "norm_taxa")

# Get remaining unmatched rows
remaining <- anti_join(gbif_df_all_botanicgardens, exact_terms, by = "norm_taxa")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))
# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["norm_taxa"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Step 4: Combine all filtered results
gbif_df_filtered_regex <- rbindlist(filtered_list)
# Combine with exact matches
gbif_df_filtered <- bind_rows(gbif_df_all_filtered_exact, gbif_df_filtered_regex)

# Step 5: Check and remove taxa that slipped through pattern
# ei: veg_list "mussaenda glabra" found WITHIN "mussaenda glabrata" (not in veg_list) and matched
#exclude <- c("Brassica rapa L. subsp. pekinensis x b. oleracea l. (Lour.) Hanelt", "Beta vulgaris L. subsp. maritima x beta vulgaris l. subsp. vulgaris (L.) Arcang.", "Solanum lycopersicum L. lycopersicum x cerasiforme")
#gbif_df_filtered <- gbif_df_filtered %>%
#  filter(!Standardized_taxa %in% exclude)

# Step 6. Check matched + unmatched taxa = number of taxa in veg list
# NEW: veglist1= 1,485; veglist2= 141; veglist3= 80; veglist4= 325; veglist5=59
# OLD: veglist1= 1,476; veglist2= 147; veglist3= 80; veglist4= 315
# matched taxa present in the filtered output
matched_taxa <- gbif_df_filtered %>%
  pull(norm_taxa) %>%
  discard(is.na) %>%
  unique()
length(matched_taxa)
# compute unmatched taxa (taxa in veg_list that did NOT appear in the filtered Genesys results)
unmatched_taxa <- setdiff(pattern_terms, matched_taxa)
length(unmatched_taxa)
View(data.frame(unmatched_taxa = unmatched_taxa))

### SAVE FILTER BY VEGLIST 4 ###
# filtered GBIF data is 2,869 rows
write.csv(gbif_df_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_botanicgardens_data_filtered_veglist4_2025-12-11.csv', row.names = FALSE)


### SAVE FILTER BY VEGLIST 4 ###
# filtered GBIF data is 2,820 rows
#write.csv(gbif_df_filtered, 'Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_botanicgardens_data_filtered_veglist4_2025-11-04.csv', row.names = FALSE)








#####################################################################################
##### Show what taxa from veglist are NOT found in each dataset #####################

# required packages
library(dplyr)
library(purrr)
library(stringr)
library(writexl)

# ---- helper: normalize/extract a character vector from a column (handles list-columns) ----
norm_chr <- function(x) {
  # x can be character/factor or list-column
  if (is.list(x)) {
    # take first element of each list entry (change collapse rule if you prefer)
    v <- map_chr(x, ~ {
      if (is.null(.x) || length(.x) == 0) return(NA_character_)
      as.character(.x)[1]
    })
  } else {
    v <- as.character(x)
  }
  # trim whitespace and convert empty strings to NA, then lowercase for case-insensitive matching
  v <- trimws(v)
  v[v == ""] <- NA_character_
  v_lower <- tolower(v)
  v_lower
}

# ---- prepare unique taxa from veg_list ----
# keep an original-display version (trimmed, first-appearance) and a normalized version for matching
veg_raw <- if (is.list(veg_list$genus_species)) {
  purrr::map_chr(veg_list$genus_species, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x)[1])
} else {
  as.character(veg_list$genus_species)
}
veg_raw <- trimws(veg_raw)
veg_raw[veg_raw == ""] <- NA_character_

# unique display taxa (preserve original casing)
display_taxa <- unique(na.omit(veg_raw))

# normalized taxa for matching
veg_norm <- tolower(display_taxa)

# ---- list the datasets you want to check (replace these names with your actual data.frame names) ----
datasets <- list(
  Genesys = genesys_df_filtered,
  BGCI = bgci_df_filtered,
  GBIF = gbif_df_filtered,
  Cano = cano_df_filtered
)

# ---- build presence/absence columns ----
result <- tibble(taxa = display_taxa)

for (ds_name in names(datasets)) {
  df <- datasets[[ds_name]]
  if (!"genus_species" %in% names(df)) stop(glue::glue("Dataset {ds_name} has no column 'genus_species'"))
  present_set <- unique(na.omit(norm_chr(df$genus_species)))   # normalized present taxa in dataset
  present_logical <- veg_norm %in% present_set
  # map to "Y"/"N"
  result[[ds_name]] <- ifelse(present_logical, "Y", "N")
}

# ---- optionally: filter to only taxa NOT found in ANY dataset ----
# set this flag to TRUE if you want to output only taxa that are absent from all datasets
only_not_found_in_any <- FALSE

if (only_not_found_in_any) {
  # find rows where all dataset columns are "N"
  dataset_cols <- names(datasets)
  result <- result %>% filter(if_all(all_of(dataset_cols), ~ .x == "N"))
}


########### NEW ############
# ---- write to Excel ----
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veg_list_presence_by_dataset.xlsx"
writexl::write_xlsx(result, path = out_file)




########### OLD ############
# ---- write to Excel ----
#out_file <- "veg_list_presence_by_dataset4.xlsx"
#writexl::write_xlsx(result, path = out_file)





# ---- optionally: filter to only taxa NOT found in ANY dataset ----
# set this flag to TRUE if you want to output only taxa that are absent from all datasets
only_not_found_in_any <- TRUE

if (only_not_found_in_any) {
  # find rows where all dataset columns are "N"
  dataset_cols <- names(datasets)
  result <- result %>% filter(if_all(all_of(dataset_cols), ~ .x == "N"))
}

########## NEW ###########
# ---- write to Excel ----
out_file <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Data_filtered_by_veg_lists/veg_list_presence_by_dataset.xlsx"
writexl::write_xlsx(result, path = out_file)



########## OLD ###########
# ---- write to Excel ----
#out_file <- "veg_list_presence_by_dataset3.xlsx"
#writexl::write_xlsx(result, path = out_file)













###################### data exploration, counts ##################
# dataset names
dataset_names <- c(
  "genesys_df_filtered",
  "bgci_df_filtered",
  "gbif_df_filtered",
  "cano_df_filtered"
)

# helper: normalize/extract a character vector from a column (handles list-columns)
norm_col <- function(x) {
  if (is.list(x)) {
    v <- purrr::map_chr(x, ~ {
      if (is.null(.x) || length(.x) == 0) return(NA_character_)
      as.character(.x)[1]
    })
  } else {
    v <- as.character(x)
  }
  v <- trimws(v)
  v[v == ""] <- NA_character_
  v_lower <- tolower(v)
  v_lower
}

# compute counts and collect unique taxa per dataset
results <- map_dfr(dataset_names, function(obj_name) {
  if (!exists(obj_name, envir = .GlobalEnv)) {
    return(tibble(dataset = obj_name, rows = NA_integer_, unique_taxa = NA_integer_, taxa_col = NA_character_, note = "object not found"))
  }
  df <- get(obj_name, envir = .GlobalEnv)
  rows <- if (is.data.frame(df) || is.matrix(df)) nrow(df) else NA_integer_
  taxa_col <- if ("genus_species" %in% names(df)) "genus_species" else NA_character_
  unique_taxa <- NA_integer_
  note <- ""
  if (!is.na(taxa_col)) {
    vec <- norm_col(df[[taxa_col]])
    unique_taxa <- length(unique(na.omit(vec)))
  } else {
    note <- "no genus_species column"
  }
  tibble(dataset = obj_name, rows = rows, unique_taxa = unique_taxa, taxa_col = taxa_col, note = note)
})

# create a list of data.frames to write to Excel:
# first sheet: counts; following sheets: unique taxa for each dataset
sheets <- list(Counts = results)

for (ds in dataset_names) {
  if (!exists(ds, envir = .GlobalEnv)) {
    sheets[[ds]] <- tibble(message = paste("Dataset", ds, "not found in Global Environment"))
  } else {
    df <- get(ds, envir = .GlobalEnv)
    if (!"genus_species" %in% names(df)) {
      sheets[[ds]] <- tibble(message = paste("No genus_species column in", ds))
    } else {
      taxa_vec <- norm_col(df$genus_species)
      taxa_unique <- sort(unique(na.omit(taxa_vec)))
      # restore display column name and keep original-cased values? here we save normalized lower-case
      sheets[[ds]] <- tibble(genus_species = taxa_unique)
    }
  }
}

# sanitize and make sheet names unique and <= 31 chars
clean_and_unique_sheet_names <- function(names_vec) {
  badchars <- "[\\\\/:?*\\[\\]]"
  cleaned <- gsub(badchars, "", names_vec)
  cleaned <- trimws(cleaned)
  cleaned[cleaned == ""] <- "Sheet"
  res <- character(length(cleaned))
  used <- character()
  for (i in seq_along(cleaned)) {
    base_raw <- cleaned[i]
    # initial truncate to 31
    base <- substr(base_raw, 1, 31)
    candidate <- base
    j <- 1
    while (candidate %in% used) {
      suffix <- paste0("_", j)
      max_base_len <- 31 - nchar(suffix)
      if (max_base_len < 1) max_base_len <- 1
      candidate <- paste0(substr(base, 1, max_base_len), suffix)
      j <- j + 1
    }
    res[i] <- candidate
    used <- c(used, candidate)
  }
  res
}

# assume `sheets` and `out_file` exist as in your prior code
orig_names <- names(sheets)
new_names <- clean_and_unique_sheet_names(orig_names)

# add a mapping sheet so you can see original -> new
mapping_df <- data.frame(original_name = orig_names, sheet_name = new_names, stringsAsFactors = FALSE)

# apply new names and append mapping sheet
names(sheets) <- new_names
sheets[["Sheet_Name_Map"]] <- mapping_df

# write workbook
openxlsx::write.xlsx(sheets, file = out_file, asTable = TRUE)

cat("Wrote", out_file, "with sanitized sheet names. Mapping sheet: Sheet_Name_Map\n")




