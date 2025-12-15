######### Spatial thinning ################
###########################################

# ---- Required packages ----
pkg_check_install <- function(pkgs) {
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
pkg_check_install(c("sf","dplyr","ggplot2","rnaturalearth","rnaturalearthdata","progress","ragg","scales","readxl","rlang","terra","spThin"))

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(progress)
library(scales)
library(readxl)
library(rlang)
library(terra)
library(spThin)

# ---- User input: load occurrences (replace as needed) ----

######### NEW DATA 2025_12_12 ############

# new veg list 1
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist1/occurrences_data_veglist1.xlsx")

# new veglist 2
occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist2/occurrences_data_veglist2.xlsx")

# veglist 3 shouldnt change
# veglist 4 shouldnt change

# new veglist 5
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist5/occurrences_data_veglist5.xlsx")


# ---- Optional: spatial thinning with spThin to reduce sampling bias ----
# Toggle and parameters
THIN_APPLY <- TRUE         # set FALSE to skip thinning and use raw occurrences_data
thin_km <- 20              # thinning distance in kilometers (try 20 for grid-scale thinning; 50 for neighborhood-scale)
reps <- 100                # number of stochastic replicates (50-200 common)
seed_val <- 42             # reproducible seed
keep_exact_dups <- FALSE   # if FALSE remove exact duplicate coords per species before thinning
thin_output_csv <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist2/occurrences_20kmthinned_veglist2.csv"

if (THIN_APPLY) {
  message("SPTHIN: Starting spatial thinning (thin_km = ", thin_km, " km; reps = ", reps, ")")
  set.seed(seed_val)
  
  # Optionally remove exact duplicate coordinates per species (keeps first)
  occ_prethin <- occurrences_data
  if (!keep_exact_dups) {
    occ_prethin <- occ_prethin %>%
      dplyr::group_by(genus_species, longitude, latitude) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  
  # Prepare container
  thinned_list <- vector("list", length = length(unique(occ_prethin$genus_species)))
  names(thinned_list) <- unique(occ_prethin$genus_species)
  
  species_vec <- sort(unique(occ_prethin$genus_species))
  for (sp in species_vec) {
    message("SPTHIN: Thinning species: ", sp)
    dfsp <- occ_prethin %>% dplyr::filter(genus_species == sp)
    
    # If too few records, keep them
    if (nrow(dfsp) <= 1) {
      thinned_list[[sp]] <- dfsp
      next
    }
    
    # spThin requires a data.frame with lat/long columns
    tmp <- dfsp %>% dplyr::select(longitude, latitude, genus_species)
    
    thin_out <- tryCatch(
      spThin::thin(loc.data = tmp,
                   lat.col = "latitude",
                   long.col = "longitude",
                   spec.col = "genus_species",
                   thin.par = thin_km,
                   reps = reps,
                   locs.thinned.list.return = TRUE,
                   write.files = FALSE),
      error = function(e) {
        warning("spThin failed for species ", sp, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    
    if (is.null(thin_out)) {
      thinned_list[[sp]] <- dfsp
      next
    }
    
    # Choose the replicate that retains the most records
    replicate_lengths <- sapply(thin_out, nrow)
    best_idx <- which.max(replicate_lengths)
    best_rep <- thin_out[[best_idx]]
    
    best_rep_df <- as.data.frame(best_rep)
    # Normalize column names that may differ by versions
    names(best_rep_df)[names(best_rep_df) %in% c("Longitude","longitude","LONG","lon")] <- "longitude"
    names(best_rep_df)[names(best_rep_df) %in% c("Latitude","latitude","LAT","lat")] <- "latitude"
    if (!"genus_species" %in% names(best_rep_df)) best_rep_df$genus_species <- sp
    
    # Join back to original dfsp to keep all metadata columns
    kept <- dplyr::inner_join(best_rep_df, dfsp, by = c("longitude","latitude","genus_species"))
    kept <- kept %>% dplyr::distinct(longitude, latitude, genus_species, .keep_all = TRUE)
    
    thinned_list[[sp]] <- kept
  }
  
  # Combine and save
  thinned_occurrences <- dplyr::bind_rows(thinned_list)
  message("SPTHIN: Original records: ", nrow(occurrences_data), "; Thinned records: ", nrow(thinned_occurrences))
  write.csv(thinned_occurrences, thin_output_csv, row.names = FALSE)
  message("SPTHIN: Saved thinned records to ", thin_output_csv)
  
  # Use the thinned occurrences going forward
  occurrences_data <- thinned_occurrences
} else {
  message("SPTHIN: Skipped thinning; using original occurrences_data")
}

