########################## GCCFP Spatial Diversity Veggies Maps ##########################
# Project: Global Vegetable Diversity Distributions and Hot Spots
# Veg list occurrence density and species richness maps
# 20km & 50km resolution, medium grey color
# Author: Sarah Gora
# Date created: 2025_11_01
# Modified: 2025_11_14 - run only 20km and 50km, removed "resampled" from map titles, updated density title,
#               added Map 3 (occurrence density - Latin America), Map 4 (species richness - Latin America)
# Modified: 2025_12_13 - produce only Latin America maps

# ---- Required packages ----
pkg_check_install <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }
}
pkg_check_install(c("sf", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "progress", "ragg", "scales", "readxl", "gridExtra"))

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(progress)
library(scales)
library(readxl)
library(gridExtra)


# --- Data Import ---

# Read in Occurrences data
# only usable lat/long data (NAs and invalid lat/long removed)

#veglist1
#occurrences_data <- read_excel("new_veg_list_data/Occurrence_data/veglist1/occurrences_data_veglist1.xlsx")

#veglist2
occurrences_data <- read_excel("new_veg_list_data/Occurrence_data/veglist2/occurrences_data_veglist2.xlsx")


#veglist5
#occurrences_data <- read_excel("new_veg_list_data/Occurrence_data/veglist5/occurrences_data_veglist5.xlsx")


############################################
###### Veg List 1-3: MEDIUM GREY ###########
############################################


# --- Spatial Data Preparation  ---

# 1. Load world land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# 2. Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# 3. Filter to land only
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# 4. Define helper function for grid/heatmap creation
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)
  grid <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)
  # Occurrence density
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")
  # Species richness
  richness_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(richness = n_distinct(genus_species))
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0
  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0
  list(grid_sf = grid_sf,
       grid_with_counts = grid_with_counts,
       grid_with_richness = grid_with_richness)
}


# 5. Define grid sizes and crs
crs_equalarea <- 8857 # Equal Earth
# Run only 20km and 50km
grid_sizes <- c("20km" = 20000, "50km" = 50000)


# 6. Map generation loop (Latin America only)
if (!require(progress)) install.packages("progress")
library(progress)

# Ensure the output directory exists
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "LatinAm", "2025_12_12", "all_plus_latin2")
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "LatinAm", "2025_12_12", "veglist5")
output_dir <- file.path("new_veg_list_data", "Veggie_maps", "LatinAm", "2025_12_15", "veglist2")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

n_steps <- length(grid_sizes)
pb <- progress_bar$new(
  format = "  Processing grid size :current/:total [:bar] :percent eta: :eta",
  total = n_steps, clear = FALSE, width = 60
)

for (sz in names(grid_sizes)) {
  pb$tick()
  if (!(sz %in% c("20km", "50km"))) {
    message(sprintf("Skipping grid size %s (only 20km and 50km outputs requested).", sz))
    next
  }
  
  cell_size <- grid_sizes[[sz]]
  message(sprintf("\nSTARTING: grid size %s (%s meters per cell)", sz, cell_size))
  t_start <- Sys.time()
  
  gc()
  
  # Create grids and tables
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))
  
  # Mask grid to land
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)
  grid_land <- st_filter(grid_with_counts, world_proj)
  grid_richness_land <- st_filter(grid_with_richness, world_proj)
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))
  
  # --- Prepare Latin America extent (bbox in lon/lat) ---
  latin_bbox_4326 <- st_as_sfc(st_bbox(c(xmin = -120, xmax = -30, ymin = -60, ymax = 30), crs = 4326))
  latin_bbox_proj <- st_transform(latin_bbox_4326, st_crs(grid_land))
  
  # Filter grids and world for Latin America
  grid_land_la <- st_filter(grid_land, latin_bbox_proj)
  grid_richness_land_la <- st_filter(grid_richness_land, latin_bbox_proj)
  world_plot_la <- st_intersection(world_plot, st_transform(latin_bbox_4326, st_crs(world_plot)))
  
  # --- Map 3: Occurrence density - Latin America ---
  message(sprintf("[%s] Generating Latin America density plot (MAP 3)...", format(Sys.time(), "%H:%M:%S")))
  density_plot_la <- ggplot() +
    geom_sf(data = world_plot_la, fill = "#F2F2F2", color = NA) +
    geom_sf(data = dplyr::filter(grid_land_la, count > 0), aes(fill = count), color = NA) +
    geom_sf(data = world_plot_la, fill = NA, color = "#D8D8D8", size = 0.2) +
    scale_fill_gradient(
      low = "yellow", high = "red", trans = "log10", name = sprintf("Occurrences (%s)", sz)
    ) +
    labs(
      title = sprintf("Latin America occurrence density, %s", sz),
      fill = "Occurrences"
    ) +
    theme_minimal()
  
  out_pdf3 <- file.path(output_dir, sprintf("map03_occurrence_density_LatinAmerica_%s.pdf", sz))
  out_png3 <- file.path(output_dir, sprintf("map03_occurrence_density_LatinAmerica_%s.png", sz))
  ggsave(out_pdf3, plot = density_plot_la, width = 10, height = 10, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png3, plot = density_plot_la, width = 10, height = 10, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf3), basename(out_png3)))
  
  # --- Map 4: Species richness - Latin America ---
  message(sprintf("[%s] Generating Latin America richness plot (MAP 4)...", format(Sys.time(), "%H:%M:%S")))
  richness_plot_la <- ggplot() +
    geom_sf(data = world_plot_la, fill = "#F2F2F2", color = NA) +
    geom_sf(data = dplyr::filter(grid_richness_land_la, richness > 0), aes(fill = richness), color = NA) +
    geom_sf(data = world_plot_la, fill = NA, color = "#D8D8D8", size = 0.2) +
    scale_fill_gradient(
      low = "lightgreen", high = "darkgreen", trans = "log10", name = sprintf("Species Richness (%s)", sz)
    ) +
    labs(
      title = sprintf("Latin America species richness, %s", sz),
      fill = "Richness"
    ) +
    theme_minimal()
  
  out_pdf4 <- file.path(output_dir, sprintf("map04_species_richness_LatinAmerica_%s.pdf", sz))
  out_png4 <- file.path(output_dir, sprintf("map04_species_richness_LatinAmerica_%s.png", sz))
  ggsave(out_pdf4, plot = richness_plot_la, width = 10, height = 10, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png4, plot = richness_plot_la, width = 10, height = 10, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf4), basename(out_png4)))
  
  t_end <- Sys.time()
  message(sprintf("Finished grid size %s in %s", sz, format(difftime(t_end, t_start, units = "mins"))))
}

############################################
###### Veg List 4: MEDIUM GREY ###########
############################################


# --- Spatial Data Preparation  ---

# 1. Load world land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# 2. Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# 3. Filter to land only
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# 4. Define helper function for grid/heatmap creation
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)
  grid <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)
  # Occurrence density
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")
  # Species richness
  richness_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(richness = n_distinct(norm_taxa))
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0
  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0
  list(grid_sf = grid_sf,
       grid_with_counts = grid_with_counts,
       grid_with_richness = grid_with_richness)
}


# 5. Define grid sizes and crs
crs_equalarea <- 8857 # Equal Earth
# Run only 20km and 50km
grid_sizes <- c("20km" = 20000, "50km" = 50000)


# 6. Map generation loop (Latin America only)
if (!require(progress)) install.packages("progress")
library(progress)

# Ensure the output directory exists
output_dir <- file.path("veggies_maps", "veglist4", "2025_11_14", "all_plus_latin")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

n_steps <- length(grid_sizes)
pb <- progress_bar$new(
  format = "  Processing grid size :current/:total [:bar] :percent eta: :eta",
  total = n_steps, clear = FALSE, width = 60
)

for (sz in names(grid_sizes)) {
  pb$tick()
  if (!(sz %in% c("20km", "50km"))) {
    message(sprintf("Skipping grid size %s (only 20km and 50km outputs requested).", sz))
    next
  }
  
  cell_size <- grid_sizes[[sz]]
  message(sprintf("\nSTARTING: grid size %s (%s meters per cell)", sz, cell_size))
  t_start <- Sys.time()
  
  gc()
  
  # Create grids and tables
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))
  
  # Mask grid to land
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)
  grid_land <- st_filter(grid_with_counts, world_proj)
  grid_richness_land <- st_filter(grid_with_richness, world_proj)
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))
  
  # --- Prepare Latin America extent (bbox in lon/lat) ---
  latin_bbox_4326 <- st_as_sfc(st_bbox(c(xmin = -120, xmax = -30, ymin = -60, ymax = 30), crs = 4326))
  latin_bbox_proj <- st_transform(latin_bbox_4326, st_crs(grid_land))
  
  # Filter grids and world for Latin America
  grid_land_la <- st_filter(grid_land, latin_bbox_proj)
  grid_richness_land_la <- st_filter(grid_richness_land, latin_bbox_proj)
  world_plot_la <- st_intersection(world_plot, st_transform(latin_bbox_4326, st_crs(world_plot)))
  
  # --- Map 3: Occurrence density - Latin America ---
  message(sprintf("[%s] Generating Latin America density plot (MAP 3)...", format(Sys.time(), "%H:%M:%S")))
  density_plot_la <- ggplot() +
    geom_sf(data = world_plot_la, fill = "#F2F2F2", color = NA) +
    geom_sf(data = dplyr::filter(grid_land_la, count > 0), aes(fill = count), color = NA) +
    geom_sf(data = world_plot_la, fill = NA, color = "#D8D8D8", size = 0.2) +
    scale_fill_gradient(
      low = "yellow", high = "red", trans = "log10", name = sprintf("Occurrences (%s)", sz)
    ) +
    labs(
      title = sprintf("Latin America occurrence density, %s", sz),
      fill = "Occurrences"
    ) +
    theme_minimal()
  
  out_pdf3 <- file.path(output_dir, sprintf("map03_occurrence_density_LatinAmerica_%s.pdf", sz))
  out_png3 <- file.path(output_dir, sprintf("map03_occurrence_density_LatinAmerica_%s.png", sz))
  ggsave(out_pdf3, plot = density_plot_la, width = 10, height = 10, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png3, plot = density_plot_la, width = 10, height = 10, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf3), basename(out_png3)))
  
  # --- Map 4: Species richness - Latin America ---
  message(sprintf("[%s] Generating Latin America richness plot (MAP 4)...", format(Sys.time(), "%H:%M:%S")))
  richness_plot_la <- ggplot() +
    geom_sf(data = world_plot_la, fill = "#F2F2F2", color = NA) +
    geom_sf(data = dplyr::filter(grid_richness_land_la, richness > 0), aes(fill = richness), color = NA) +
    geom_sf(data = world_plot_la, fill = NA, color = "#D8D8D08", size = 0.2) +
    scale_fill_gradient(
      low = "lightgreen", high = "darkgreen", trans = "log10", name = sprintf("Species Richness (%s)", sz)
    ) +
    labs(
      title = sprintf("Latin America species richness, %s", sz),
      fill = "Richness"
    ) +
    theme_minimal()
  
  out_pdf4 <- file.path(output_dir, sprintf("map04_species_richness_LatinAmerica_%s.pdf", sz))
  out_png4 <- file.path(output_dir, sprintf("map04_species_richness_LatinAmerica_%s.png", sz))
  ggsave(out_pdf4, plot = richness_plot_la, width = 10, height = 10, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png4, plot = richness_plot_la, width = 10, height = 10, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf4), basename(out_png4)))
  
  t_end <- Sys.time()
  message(sprintf("Finished grid size %s in %s", sz, format(difftime(t_end, t_start, units = "mins"))))
}

##### end script #####