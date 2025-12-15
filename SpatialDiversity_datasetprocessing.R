
library(readr)
library(dplyr)

### Combine filtered data into datasets:
#  1. genebank_accessionlevel_df
#  2. botanicgarden_accessionlevel_df
#  3. botanicgarden_specieslevel_df   <<<<< dont need for occurrences data
#  4. gbif_observations_df

# Then combine into occurrences datset


################################################################################
########################### veglist1 ###########################################
################################################################################

## 1. GENEBANK ACCESSION-LEVEL DATASET
# Genesys df, inst_type=Genebank
# 1,200,126 rows
genebank_accessionlevel_df1 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_Genesys_data_filtered_veglist1_2025-11-04.csv"
) %>%
  filter(inst_type == "Genebank") %>%
  select("Standardized_taxa", "genus_species",
                  "inst_code","inst_name","inst_type",
                  "latitude","longitude","record_type",
                  "data_source")


## 2. BOTANIC GARDEN ACCESSION-LEVEL DATASET
# Cano df (all)
# 29,059 rows
cano_1 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_Cano_data_filtered_veglist1_2025-11-04.csv")
# forgot to add this
cano_1 <- cano_1 %>%
  mutate(
    data_source = "Cano",
    inst_type   = "Botanic garden",
    latitude    = NA_real_,
    longitude   = NA_real_
  )
# GBIF botanic gardens df (all)
# 13,852 rows
gbif_bg1 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_botanicgardens_data_filtered_veglist1_2025-11-04.csv")
# Genesys, inst_type = Botanic garden
# 8,540 rows
genesys_bg1 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_Genesys_data_filtered_veglist1_2025-11-04.csv"
) %>%
  filter(inst_type == "Botanic garden")

#view common columns
common_cols <- Reduce(intersect, list(colnames(cano_1), colnames(gbif_bg1), colnames(genesys_bg1)))
#combine by common columns
botanicgarden_accessionlevel_df1 <- bind_rows(
  select(cano_1, any_of(common_cols)),
  select(gbif_bg1, any_of(common_cols)),
  select(genesys_bg1, any_of(common_cols)))
# combined botanic garden accession level dataset
# 51,451 rows
botanicgarden_accessionlevel_df1 <- botanicgarden_accessionlevel_df1 %>%
  select(any_of(c("Standardized_taxa", "genus_species",
                  "inst_code","inst_name","inst_type",    # re-order
                  "latitude","longitude","record_type",
                  "data_source")), everything())


## 3. BOTANIC GARDEN SPECIES-LEVEL DATASET
# Bgci df (all)
# 70,383 rows
#botanicgarden_specieslevel_df1 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_BGCI_data_filtered_veglist1_2025-11-04.csv") %>%
#  select("Standardized_taxa", "genus_species",
#         "inst_code","inst_name","inst_type",
#         "latitude","longitude","record_type",
#         "data_source")


## 4. GBIF OBSERVATIONS DATASET
# GBIF observations df (all)
# 454,344 rows
gbif_observations_df1 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist1/WCFP_GBIF_observations_data_filtered_veglist1_2025-11-04.csv") %>%
  select("Standardized_taxa", "genus_species",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")



## ------------------------------------------- ##
## ------ OCCURENCES DATA for veglist 1 ------ ##
## ------------------------------------------- ##

# Combine genebank accession-level, bg accession-level, GBIF observations
#    = occurrences
# filter for only real lat/long data
occurrences_data1 <- bind_rows(
  genebank_accessionlevel_df1,
  botanicgarden_accessionlevel_df1,
  gbif_observations_df1) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%  #dropped 1,063,906 rows with no lat/long data
  filter(between(latitude, -90, 90), between(longitude, -180, 180)) #dropped 32 rows with invalid lat/long

# occurrences_data for veglist1 = 641,983 rows
# save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_veglist1.xlsx"
writexl::write_xlsx(occurrences_data1, path = out_file)


# check the bad coords
#occurrences_data_bad1 <- bind_rows(
#  genebank_accessionlevel_df1,
#  botanicgarden_accessionlevel_df1,
#  gbif_observations_df1) %>%
#  filter(!is.na(latitude), !is.na(longitude))

#bad_coords1 <- occurrences_data_bad1 %>%
#  filter(latitude < -90 | latitude > 90 |
#           longitude < -180 | longitude > 180)

#View(bad_coords1)


# count the number of occurrences/records per taxa for veglist1
occ_summary_veglist1 <- occurrences_data1 %>%
  group_by(genus_species) %>%
  summarise(
    total_occurrence_count = n(),
    G_occurrence_count   = sum(record_type == "G", na.rm = TRUE),
    H_occurrence_count   = sum(record_type == "H", na.rm = TRUE)
  ) %>%
  ungroup()

occ_summary_veglist1 <- occ_summary_veglist1 %>%
  rename(taxon = genus_species)

# view
View(occ_summary_veglist1)

#save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_summary_veglist1.xlsx"
writexl::write_xlsx(occ_summary_veglist1, path = out_file)


##### end script ###







################################################################################
########################### veglist2 ###########################################
################################################################################

## 1. GENEBANK ACCESSION-LEVEL DATASET
# Genesys df, inst_type=Genebank
# 632,599 rows
genebank_accessionlevel_df2 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_Genesys_data_filtered_veglist2_2025-11-04.csv"
) %>%
  filter(inst_type == "Genebank") %>%
  select("Standardized_taxa", "genus_species",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## 2. BOTANIC GARDEN ACCESSION-LEVEL DATASET
# Cano df (all)
# 3,027 rows
cano_2 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_Cano_data_filtered_veglist2_2025-11-04.csv")
# forgot to add this
cano_2 <- cano_2 %>%
  mutate(
    data_source = "Cano",
    inst_type   = "Botanic garden",
    latitude    = NA_real_,
    longitude   = NA_real_
  )
# GBIF botanic gardens df (all)
# 3,885 rows
gbif_bg2 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_botanicgardens_data_filtered_veglist2_2025-11-04.csv")
# Genesys, inst_type = Botanic garden
# 2,771 rows
genesys_bg2 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_Genesys_data_filtered_veglist2_2025-11-04.csv"
) %>%
  filter(inst_type == "Botanic garden")

#view common columns
common_cols <- Reduce(intersect, list(colnames(cano_2), colnames(gbif_bg2), colnames(genesys_bg2)))
#combine by common columns
botanicgarden_accessionlevel_df2 <- bind_rows(
  select(cano_2, any_of(common_cols)),
  select(gbif_bg2, any_of(common_cols)),
  select(genesys_bg2, any_of(common_cols)))
# combined botanic garden accession level dataset
# 9,683 rows
botanicgarden_accessionlevel_df2 <- botanicgarden_accessionlevel_df2 %>%
  select(any_of(c("Standardized_taxa", "genus_species",
                  "inst_code","inst_name","inst_type",    # re-order
                  "latitude","longitude","record_type",
                  "data_source")), everything())


## 3. BOTANIC GARDEN SPECIES-LEVEL DATASET
# Bgci df (all)
# ? rows
#botanicgarden_specieslevel_df2 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_BGCI_data_filtered_veglist2_2025-11-04.csv") %>%
#  select("Standardized_taxa", "genus_species",
#         "inst_code","inst_name","inst_type",
#         "latitude","longitude","record_type",
#         "data_source")


## 4. GBIF OBSERVATIONS DATASET
# GBIF observations df (all)
# 59,075 rows
gbif_observations_df2 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist2/WCFP_GBIF_observations_data_filtered_veglist2_2025-11-04.csv") %>%
  select("Standardized_taxa", "genus_species",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## ------------------------------------------- ##
## ------ OCCURENCES DATA for veglist 2 ------ ##
## ------------------------------------------- ##

# Combine genebank accession-level, bg accession-level, GBIF observations
#    = occurrences
# filter for only real lat/long data
occurrences_data2 <- bind_rows(
  genebank_accessionlevel_df2,
  botanicgarden_accessionlevel_df2,
  gbif_observations_df2) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%  #dropped ? rows with no lat/long data
  filter(between(latitude, -90, 90), between(longitude, -180, 180)) #dropped ? rows with invalid lat/long

# occurrences_data for veglist2 = 227,509 rows
# save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_veglist2.xlsx"
writexl::write_xlsx(occurrences_data2, path = out_file)


# check the bad coords
#occurrences_data_bad2 <- bind_rows(
#  genebank_accessionlevel_df2,
#  botanicgarden_accessionlevel_df2,
#  gbif_observations_df2) %>%
#  filter(!is.na(latitude), !is.na(longitude))

#bad_coords2 <- occurrences_data_bad2 %>%
#  filter(latitude < -90 | latitude > 90 |
#           longitude < -180 | longitude > 180)

#View(bad_coords2)


# count the number of occurrences/records per taxa for veglist2
occ_summary_veglist2 <- occurrences_data2 %>%
  group_by(genus_species) %>%
  summarise(
    total_occurrence_count = n(),
    G_occurrence_count   = sum(record_type == "G", na.rm = TRUE),
    H_occurrence_count   = sum(record_type == "H", na.rm = TRUE)
  ) %>%
  ungroup()

occ_summary_veglist2 <- occ_summary_veglist2 %>%
  rename(taxon = genus_species)

# view
View(occ_summary_veglist2)

#save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_summary_veglist2.xlsx"
writexl::write_xlsx(occ_summary_veglist2, path = out_file)







################################################################################
########################### veglist3 ###########################################
################################################################################

## 1. GENEBANK ACCESSION-LEVEL DATASET
# Genesys df, inst_type=Genebank
# 254,947 rows
genebank_accessionlevel_df3 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_Genesys_data_filtered_veglist3_2025-11-04.csv"
) %>%
  filter(inst_type == "Genebank") %>%
  select("Standardized_taxa", "genus_species",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## 2. BOTANIC GARDEN ACCESSION-LEVEL DATASET
# Cano df (all)
# 2,563 rows
cano_3 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_Cano_data_filtered_veglist3_2025-11-04.csv")
# forgot to add this
cano_3 <- cano_3 %>%
  mutate(
    data_source = "Cano",
    inst_type   = "Botanic garden",
    latitude    = NA_real_,
    longitude   = NA_real_
  )
# GBIF botanic gardens df (all)
# 2,873 rows
gbif_bg3 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_botanicgardens_data_filtered_veglist3_2025-11-04.csv")
# Genesys, inst_type = Botanic garden
# 1,338 rows
genesys_bg3 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_Genesys_data_filtered_veglist3_2025-11-04.csv"
) %>%
  filter(inst_type == "Botanic garden")

#view common columns
common_cols <- Reduce(intersect, list(colnames(cano_3), colnames(gbif_bg3), colnames(genesys_bg3)))
#combine by common columns
botanicgarden_accessionlevel_df3 <- bind_rows(
  select(cano_3, any_of(common_cols)),
  select(gbif_bg3, any_of(common_cols)),
  select(genesys_bg3, any_of(common_cols)))
# combined botanic garden accession level dataset
# 6,774 rows
botanicgarden_accessionlevel_df3 <- botanicgarden_accessionlevel_df3 %>%
  select(any_of(c("Standardized_taxa", "genus_species",
                  "inst_code","inst_name","inst_type",    # re-order
                  "latitude","longitude","record_type",
                  "data_source")), everything())


## 3. BOTANIC GARDEN SPECIES-LEVEL DATASET
# Bgci df (all)
# ? rows
#botanicgarden_specieslevel_df3 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_BGCI_data_filtered_veglist3_2025-11-04.csv") %>%
#  select("Standardized_taxa", "genus_species",
#         "inst_code","inst_name","inst_type",
#         "latitude","longitude","record_type",
#         "data_source")


## 4. GBIF OBSERVATIONS DATASET
# GBIF observations df (all)
# 40,910 rows
gbif_observations_df3 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist3/WCFP_GBIF_observations_data_filtered_veglist3_2025-11-04.csv") %>%
  select("Standardized_taxa", "genus_species",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## ------------------------------------------- ##
## ------ OCCURENCES DATA for veglist 3 ------ ##
## ------------------------------------------- ##

# Combine genebank accession-level, bg accession-level, GBIF observations
#    = occurrences
# filter for only real lat/long data
occurrences_data3 <- bind_rows(
  genebank_accessionlevel_df3,
  botanicgarden_accessionlevel_df3,
  gbif_observations_df3) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%  #dropped ? rows with no lat/long data
  filter(between(latitude, -90, 90), between(longitude, -180, 180)) #dropped ? rows with invalid lat/long

# occurrences_data for veglist3 = 86,301 rows
# save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_veglist3.xlsx"
writexl::write_xlsx(occurrences_data3, path = out_file)


# check the bad coords
#occurrences_data_bad3 <- bind_rows(
#  genebank_accessionlevel_df3,
#  botanicgarden_accessionlevel_df3,
#  gbif_observations_df3) %>%
#  filter(!is.na(latitude), !is.na(longitude))

#bad_coords3 <- occurrences_data_bad3 %>%
#  filter(latitude < -90 | latitude > 90 |
#           longitude < -180 | longitude > 180)

#View(bad_coords3)


# count the number of occurrences/records per taxa for veglist3
occ_summary_veglist3 <- occurrences_data3 %>%
  group_by(genus_species) %>%
  summarise(
    total_occurrence_count = n(),
    G_occurrence_count   = sum(record_type == "G", na.rm = TRUE),
    H_occurrence_count   = sum(record_type == "H", na.rm = TRUE)
  ) %>%
  ungroup()

occ_summary_veglist3 <- occ_summary_veglist3 %>%
  rename(taxon = genus_species)

# view
View(occ_summary_veglist3)

#save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_summary_veglist3.xlsx"
writexl::write_xlsx(occ_summary_veglist3, path = out_file)






################################################################################
########################### veglist 4 RERUN ###########################################
################################################################################


# select norm_taxa instead of genus species


## 1. GENEBANK ACCESSION-LEVEL DATASET
# Genesys df, inst_type=Genebank
#  15,236 rows
genebank_accessionlevel_df4 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4_rerun/WCFP_Genesys_data_filtered_veglist4_2025-11-07.csv"
) %>%
  filter(inst_type == "Genebank") %>%
  select("Standardized_taxa", "genus_species","norm_taxa",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## 2. BOTANIC GARDEN ACCESSION-LEVEL DATASET
# Cano df (all)
# 1,191 rows
cano_4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4_rerun/WCFP_Cano_data_filtered_veglist4_2025-11-07.csv")
# forgot to add this
cano_4 <- cano_4 %>%
  mutate(
    data_source = "Cano",
    inst_type   = "Botanic garden",
    latitude    = NA_real_,
    longitude   = NA_real_
  )
# GBIF botanic gardens df (all)
# 1,162 rows
gbif_bg4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4_rerun/WCFP_GBIF_botanicgardens_data_filtered_veglist4_2025-11-07.csv")
# Genesys, inst_type = Botanic garden
# 0 rows
genesys_bg4 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4_rerun/WCFP_Genesys_data_filtered_veglist4_2025-11-07.csv"
) %>%
  filter(inst_type == "Botanic garden")

#view common columns
common_cols <- Reduce(intersect, list(colnames(cano_4), colnames(gbif_bg4), colnames(genesys_bg4)))
#combine by common columns
botanicgarden_accessionlevel_df4 <- bind_rows(
  select(cano_4, any_of(common_cols)),
  select(gbif_bg4, any_of(common_cols)),
  select(genesys_bg4, any_of(common_cols)))
# combined botanic garden accession level dataset
# 2,353 rows
botanicgarden_accessionlevel_df4 <- botanicgarden_accessionlevel_df4 %>%
  select(any_of(c("Standardized_taxa", "genus_species","norm_taxa",
                  "inst_code","inst_name","inst_type",    # re-order
                  "latitude","longitude","record_type",
                  "data_source")), everything())


## 3. BOTANIC GARDEN SPECIES-LEVEL DATASET
# Bgci df (all)
# ? rows
#botanicgarden_specieslevel_df4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_BGCI_data_filtered_veglist4_2025-11-04.csv") %>%
#  select("Standardized_taxa", "genus_species",
#         "inst_code","inst_name","inst_type",
#         "latitude","longitude","record_type",
#         "data_source")


## 4. GBIF OBSERVATIONS DATASET
# GBIF observations df (all)
# 13,524 rows
gbif_observations_df4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4_rerun/WCFP_GBIF_observations_data_filtered_veglist4_2025-11-07.csv") %>%
  select("Standardized_taxa", "genus_species", "norm_taxa",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## ------------------------------------------- ##
## ------ OCCURENCES DATA for veglist 4 ------ ##
## ------------------------------------------- ##

# Combine genebank accession-level, bg accession-level, GBIF observations
#  = occurrences
# filter for only real lat/long data
occurrences_data4 <- bind_rows(
  genebank_accessionlevel_df4,
  botanicgarden_accessionlevel_df4,
  gbif_observations_df4) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%  #dropped 1 rows with no lat/long data
  filter(between(latitude, -90, 90), between(longitude, -180, 180)) #dropped 0 rows with invalid lat/long

# occurrences_data for veglist4 = 14,560 rows
# save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_veglist4_rerun.xlsx"
writexl::write_xlsx(occurrences_data4, path = out_file)


# check the bad coords
#occurrences_data_bad4 <- bind_rows(
#  genebank_accessionlevel_df4,
#  botanicgarden_accessionlevel_df4,
#  gbif_observations_df4) %>%
#  filter(!is.na(latitude), !is.na(longitude))

#bad_coords4 <- occurrences_data_bad4 %>%
#  filter(latitude < -90 | latitude > 90 |
#           longitude < -180 | longitude > 180)

#View(bad_coords4)


# count the number of occurrences/records per taxa for veglist4
occ_summary_veglist4 <- occurrences_data4 %>%
  group_by(norm_taxa) %>%
  summarise(
    total_occurrence_count = n(),
    G_occurrence_count   = sum(record_type == "G", na.rm = TRUE),
    H_occurrence_count   = sum(record_type == "H", na.rm = TRUE)
  ) %>%
  ungroup()

occ_summary_veglist4 <- occ_summary_veglist4 %>%
  rename(taxon = norm_taxa)

# view
View(occ_summary_veglist4)

#save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_summary_veglist4_rerun.xlsx"
writexl::write_xlsx(occ_summary_veglist4, path = out_file)











################################################################################
########################### veglist 4 old ###########################################
################################################################################


# select Standardized_taxa ? instead of genus species?
# norm_taxa


## 1. GENEBANK ACCESSION-LEVEL DATASET
# Genesys df, inst_type=Genebank
#  rows
genebank_accessionlevel_df4 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_Genesys_data_filtered_veglist4_2025-11-04_2.csv"
) %>%
  filter(inst_type == "Genebank") %>%
  select("Standardized_taxa", "genus_species","norm_taxa",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## 2. BOTANIC GARDEN ACCESSION-LEVEL DATASET
# Cano df (all)
# ? rows
cano_4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_Cano_data_filtered_veglist4_2025-11-04_2.csv")
# forgot to add this
cano_4 <- cano_4 %>%
  mutate(
    data_source = "Cano",
    inst_type   = "Botanic garden",
    latitude    = NA_real_,
    longitude   = NA_real_
  )
# GBIF botanic gardens df (all)
# ? rows
gbif_bg4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_botanicgardens_data_filtered_veglist4_2025-11-04_2.csv")
# Genesys, inst_type = Botanic garden
# 0 rows
genesys_bg4 <- read_csv(
  "Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_Genesys_data_filtered_veglist4_2025-11-04_2.csv"
) %>%
  filter(inst_type == "Botanic garden")

#view common columns
common_cols <- Reduce(intersect, list(colnames(cano_4), colnames(gbif_bg4), colnames(genesys_bg4)))
#combine by common columns
botanicgarden_accessionlevel_df4 <- bind_rows(
  select(cano_4, any_of(common_cols)),
  select(gbif_bg4, any_of(common_cols)),
  select(genesys_bg4, any_of(common_cols)))
# combined botanic garden accession level dataset
# ? rows
botanicgarden_accessionlevel_df4 <- botanicgarden_accessionlevel_df4 %>%
  select(any_of(c("Standardized_taxa", "genus_species","norm_taxa",
                  "inst_code","inst_name","inst_type",    # re-order
                  "latitude","longitude","record_type",
                  "data_source")), everything())


## 3. BOTANIC GARDEN SPECIES-LEVEL DATASET
# Bgci df (all)
# ? rows
#botanicgarden_specieslevel_df4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_BGCI_data_filtered_veglist4_2025-11-04.csv") %>%
#  select("Standardized_taxa", "genus_species",
#         "inst_code","inst_name","inst_type",
#         "latitude","longitude","record_type",
#         "data_source")


## 4. GBIF OBSERVATIONS DATASET
# GBIF observations df (all)
# ? rows
gbif_observations_df4 <- read_csv("Agrobio_veg_list/Outputs/Data_filtered_by_veg_lists/veglist4/WCFP_GBIF_observations_data_filtered_veglist4_2025-11-04_2.csv") %>%
  select("Standardized_taxa", "genus_species", "norm_taxa",
         "inst_code","inst_name","inst_type",
         "latitude","longitude","record_type",
         "data_source")


## ------------------------------------------- ##
## ------ OCCURENCES DATA for veglist 4 ------ ##
## ------------------------------------------- ##

# Combine genebank accession-level, bg accession-level, GBIF observations
#    = occurrences
# filter for only real lat/long data
occurrences_data4 <- bind_rows(
  genebank_accessionlevel_df4,
  botanicgarden_accessionlevel_df4,
  gbif_observations_df4) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%  #dropped ? rows with no lat/long data
  filter(between(latitude, -90, 90), between(longitude, -180, 180)) #dropped ? rows with invalid lat/long

# occurrences_data for veglist4 = 13,671 rows
# save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_veglist4_2.xlsx"
writexl::write_xlsx(occurrences_data4, path = out_file)


# check the bad coords
#occurrences_data_bad4 <- bind_rows(
#  genebank_accessionlevel_df4,
#  botanicgarden_accessionlevel_df4,
#  gbif_observations_df4) %>%
#  filter(!is.na(latitude), !is.na(longitude))

#bad_coords4 <- occurrences_data_bad4 %>%
#  filter(latitude < -90 | latitude > 90 |
#           longitude < -180 | longitude > 180)

#View(bad_coords4)


# count the number of occurrences/records per taxa for veglist4
occ_summary_veglist4 <- occurrences_data4 %>%
  group_by(norm_taxa) %>%
  summarise(
    total_occurrence_count = n(),
    G_occurrence_count   = sum(record_type == "G", na.rm = TRUE),
    H_occurrence_count   = sum(record_type == "H", na.rm = TRUE)
  ) %>%
  ungroup()

occ_summary_veglist4 <- occ_summary_veglist4 %>%
  rename(taxon = norm_taxa)

# view
View(occ_summary_veglist4)

#save
out_file <- "Agrobio_veg_list/Outputs/Occurrence_data/occurrences_data_summary_veglist4_2.xlsx"
writexl::write_xlsx(occ_summary_veglist4, path = out_file)


####### end script #####


