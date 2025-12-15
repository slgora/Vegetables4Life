########################## GCCFP Spatial Diversity Veggies Maps ##########################
# Project: Global Vegetable Diversity Distributions and Hot Spots
# Veg list occurrence density and species richness maps
# 20km resolution, medium grey color
# Author: Sarah Gora
# Date created: 2025_12_12


# ---- Required packages ----
pkg_check_install <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }
}
pkg_check_install(c("sf", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "progress", "ragg", "scales"))

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(progress)
library(scales)


# --- Data Import ---

# Read in Occurrences data
# only usable lat/long data (NAs and invalid lat/long removed)

######### NEW DATA 2025_12_12 ############

# new veg list 1
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist1/occurrences_data_veglist1.xlsx")

# new veglist 2
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist2/occurrences_data_veglist2.xlsx")

# veglist 3 shouldnt change
# veglist 4 shouldnt change

# new veglist 5
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist5/occurrences_data_veglist5.xlsx")



############################################
###### Veg List 1-3,5: MEDIUM GREY ###########
############################################

#occurrences_data <- read_csv("veglist1_thinned_occurrences_spThin.csv")
#occurrences_data <- read_csv("veglist2_thinned_occurrences_spThin.csv")
#occurrences_data <- read_csv("veglist5_thinned_occurrences_spThin.csv")

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

# 5. Filter occurrence points by record_type (G or H)
# record_type: "H" = observation records, "G" = ex situ collection records
h_points <- occurrences_land %>% filter(record_type == "H")
g_points <- occurrences_land %>% filter(record_type == "G")
both_points <- occurrences_land %>% filter(record_type %in% c("G", "H"))

# Label mapping for potential legends/overlays
record_type_labels <- c("H" = "H (observations)", "G" = "G (ex situ collections)")

# 6. Define grid sizes and crs
crs_equalarea <- 8857 # Equal Earth
grid_sizes <- c("10km" = 10000, "20km" = 20000, "50km" = 50000)


# 7. Map generation loop
# --- Map Generation Loop with Progress Bar, Progress Tracking, and Output Directory ---

if (!require(progress)) install.packages("progress")
library(progress)

# Ensure the output directory exists
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist1", "2025_12_12")
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist2", "2025_12_12")
output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist5", "2025_12_12")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

grid_sizes <- c("10km" = 10000, "20km" = 20000, "50km" = 50000)
n_steps <- length(grid_sizes)
pb <- progress_bar$new(
  format = "  Processing grid size :current/:total [:bar] :percent eta: :eta",
  total = n_steps, clear = FALSE, width = 60
)

for (sz in names(grid_sizes)) {
  pb$tick()
  # (no skipping — process all sizes: 10km, 20km, 50km)

  cell_size <- grid_sizes[[sz]]
  message(sprintf("\nSTARTING: grid size %s (%s meters per cell)", sz, cell_size))
  t_start <- Sys.time()

  # Remove unnecessary objects and run garbage collector
  gc()

  # 1. Grid/table creation
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))

  # 2. Mask grid to land
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)
  grid_land <- st_filter(grid_with_counts, world_proj)
  grid_richness_land <- st_filter(grid_with_richness, world_proj)
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))

  # --- MAP 1: Heat map of occurrence density (per grid size) ---
  message(sprintf("[%s] Generating density plot (MAP 1)...", format(Sys.time(), "%H:%M:%S")))
  density_plot <- ggplot() +
    geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) + # slightly darker land fill
    geom_sf(data = dplyr::filter(grid_land, count > 0), aes(fill = count), color = NA) +
    geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) + # slightly darker soft borders
    scale_fill_gradient(
      low = "yellow", high = "red", trans = "log10", name = sprintf("Occurrences (%s)", sz)
    ) +
    labs(
      title = "Global occurrence richness",
      fill = "Occurrences"
    ) +
    theme_minimal()

  # Save MAP 1 as PDF and PNG
  out_pdf1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.pdf", sz))
  out_png1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.png", sz))
  ggsave(out_pdf1, plot = density_plot, width = 14, height = 7, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png1, plot = density_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf1), basename(out_png1)))

  # --- MAP 2: Heat map of species richness (per grid size) ---
  message(sprintf("[%s] Generating richness plot (MAP 2)...", format(Sys.time(), "%H:%M:%S")))
  richness_plot <- ggplot() +
    geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) + # slightly darker land fill
    geom_sf(data = dplyr::filter(grid_richness_land, richness > 0), aes(fill = richness), color = NA) +
    geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) + # slightly darker soft borders
    scale_fill_gradient(
      low = "lightgreen", high = "darkgreen", trans = "log10", name = sprintf("Species Richness (%s)", sz)
    ) +
    labs(
      title = sprintf("Global species richness, %s", sz),
      fill = "Richness"
    ) +
    theme_minimal()

  # Save MAP 2 as PDF and PNG
  out_pdf2 <- file.path(output_dir, sprintf("map02_species_richness_%s.pdf", sz))
  out_png2 <- file.path(output_dir, sprintf("map02_species_richness_%s.png", sz))
  ggsave(out_pdf2, plot = richness_plot, width = 14, height = 7, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png2, plot = richness_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf2), basename(out_png2)))

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
    summarise(richness = n_distinct(Standardized_taxa))
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0
  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0
  list(grid_sf = grid_sf,
       grid_with_counts = grid_with_counts,
       grid_with_richness = grid_with_richness)
}

# 5. Filter occurrence points by record_type (G or H)
# record_type: "H" = observation records, "G" = ex situ collection records
h_points <- occurrences_land %>% filter(record_type == "H")
g_points <- occurrences_land %>% filter(record_type == "G")
both_points <- occurrences_land %>% filter(record_type %in% c("G", "H"))

# Label mapping for potential legends/overlays
record_type_labels <- c("H" = "H (observations)", "G" = "G (ex situ collections)")

# 6. Define grid sizes and crs
crs_equalarea <- 8857 # Equal Earth
grid_sizes <- c("10km" = 10000, "20km" = 20000)


# 7. Map generation loop
# --- Map Generation Loop with Progress Bar, Progress Tracking, and Output Directory ---
if (!require(progress)) install.packages("progress")
library(progress)

# Ensure the output directory exists
output_dir <- file.path("veggies_maps", "veglist4", "rerun2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

grid_sizes <- c("10km" = 10000, "20km" = 20000)
n_steps <- length(grid_sizes)
pb <- progress_bar$new(
  format = "  Processing grid size :current/:total [:bar] :percent eta: :eta",
  total = n_steps, clear = FALSE, width = 60
)

for (sz in names(grid_sizes)) {
  pb$tick()
  # Only generate outputs for 20km as requested
  if (sz != "20km") {
    message(sprintf("Skipping grid size %s (only 20km outputs requested).", sz))
    next
  }

  cell_size <- grid_sizes[[sz]]
  message(sprintf("\nSTARTING: grid size %s (%s meters per cell)", sz, cell_size))
  t_start <- Sys.time()

  # Remove unnecessary objects and run garbage collector
  gc()

  # 1. Grid/table creation
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))

  # 2. Mask grid to land
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)
  grid_land <- st_filter(grid_with_counts, world_proj)
  grid_richness_land <- st_filter(grid_with_richness, world_proj)
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))

  # --- MAP 1: Heat map of occurrence density (20km only) ---
  message(sprintf("[%s] Generating density plot (MAP 1)...", format(Sys.time(), "%H:%M:%S")))
  density_plot <- ggplot() +
    geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) + # slightly darker land fill
    geom_sf(data = dplyr::filter(grid_land, count > 0), aes(fill = count), color = NA) +
    geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) + # slightly darker soft borders
    scale_fill_gradient(
      low = "yellow", high = "red", trans = "log10", name = sprintf("Occurrences (%s)", sz)
    ) +
    labs(
      title = "Global occurrence richness",
      fill = "Occurrences"
    ) +
    theme_minimal()

  # Save MAP 1 as PDF and PNG
  out_pdf1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.pdf", sz))
  out_png1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.png", sz))
  ggsave(out_pdf1, plot = density_plot, width = 14, height = 7, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png1, plot = density_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf1), basename(out_png1)))

  # --- MAP 2: Heat map of species richness (20km only) ---
  message(sprintf("[%s] Generating richness plot (MAP 2)...", format(Sys.time(), "%H:%M:%S")))
  richness_plot <- ggplot() +
    geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) + # slightly darker land fill
    geom_sf(data = dplyr::filter(grid_richness_land, richness > 0), aes(fill = richness), color = NA) +
    geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) + # slightly darker soft borders
    scale_fill_gradient(
      low = "lightgreen", high = "darkgreen", trans = "log10", name = sprintf("Species Richness (%s)", sz)
    ) +
    labs(
      title = sprintf("Global species richness, %s", sz),
      fill = "Richness"
    ) +
    theme_minimal()

  # Save MAP 2 as PDF and PNG
  out_pdf2 <- file.path(output_dir, sprintf("map02_species_richness_%s.pdf", sz))
  out_png2 <- file.path(output_dir, sprintf("map02_species_richness_%s.png", sz))
  ggsave(out_pdf2, plot = richness_plot, width = 14, height = 7, units = "in", device = cairo_pdf, bg = "white")
  ggsave(out_png2, plot = richness_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf2), basename(out_png2)))

  t_end <- Sys.time()
  message(sprintf("Finished grid size %s in %s", sz, format(difftime(t_end, t_start, units = "mins"))))
}

####### End script ########






#################################################################
########### delete below, expand if want to view extra script ############
# --- Spatial Data Preparation  ---

# 1. Load world land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# set the map land fill color to match your old example
bg_land_fill <- "#6e6e6e"  # medium grey like the example

# 2. Convert occurrences to sf
# occurrences_data must already exist and contain: longitude, latitude, genus_species, record_type
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# 3. Filter to land only
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# 4. Define helper function for grid/heatmap creation (also computes per-record_type tables)
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)

  # Make grid in projected CRS (meters)
  grid <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = seq_along(grid), geometry = grid)

  # spatial join points -> grid cells
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)

  # Overall occurrence density (per grid cell)
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")

  # Overall species richness (distinct genus_species per grid cell)
  richness_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(richness = n_distinct(genus_species), .groups = "drop")

  # Counts / richness by record_type (G / H)
  density_by_type <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id, record_type) %>%
    tally(name = "count") %>%
    ungroup()

  richness_by_type <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id, record_type) %>%
    summarise(richness = n_distinct(genus_species), .groups = "drop")

  # Join overall tables to full grid (ensuring zeros for empty cells)
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0

  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0

  # Create full grid x record_type combinations so we have zeros where absent
  expected_types <- c("H", "G")
  grid_rt <- expand.grid(grid_id = grid_sf$grid_id, record_type = expected_types, stringsAsFactors = FALSE)
  # Attach geometry by grid_id
  grid_rt_sf <- left_join(grid_rt, grid_sf, by = "grid_id")
  grid_rt_sf <- st_as_sf(grid_rt_sf)

  # Join counts by type and set missing -> 0
  grid_counts_by_type <- left_join(grid_rt_sf, density_by_type, by = c("grid_id", "record_type"))
  grid_counts_by_type$count[is.na(grid_counts_by_type$count)] <- 0

  # Join richness by type and set missing -> 0
  grid_richness_by_type <- left_join(grid_rt_sf, richness_by_type, by = c("grid_id", "record_type"))
  grid_richness_by_type$richness[is.na(grid_richness_by_type$richness)] <- 0

  list(
    grid_sf = grid_sf,
    grid_with_counts = grid_with_counts,
    grid_with_richness = grid_with_richness,
    grid_counts_by_type = grid_counts_by_type,
    grid_richness_by_type = grid_richness_by_type
  )
}

# 5. Filter occurrence points by record_type (G or H) for potential overlays/use
h_points <- occurrences_land %>% filter(record_type == "H")
g_points <- occurrences_land %>% filter(record_type == "G")
both_points <- occurrences_land %>% filter(record_type %in% c("G", "H"))

# Label mapping for legend/labels
record_type_labels <- c("H" = "H (observations)", "G" = "G (ex situ collections)")

# 6. Define grid sizes and crs (only 20km required)
crs_equalarea <- 8857 # Equal Earth
grid_sizes <- c("20km" = 20000)

# 7. Map generation loop
# Ensure the output directory exists
output_dir <- file.path("veggies_maps", "veglist1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Legend / colorbar settings to match the example
legend_breaks <- c(1, 10, 100, 1000)
legend_guide <- guide_colorbar(
  barwidth = unit(0.5, "cm"),
  barheight = unit(8, "cm"),
  title.position = "right",
  ticks = TRUE,
  draw.ulim = TRUE,
  draw.llim = TRUE
)

# Theme adjustments (replace existing shared_theme definition)
shared_theme <- theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.25),  # use linewidth instead of size
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "grey30")
  )

n_steps <- length(grid_sizes)
pb <- progress_bar$new(
  format = "  Processing grid size :current/:total [:bar] :percent eta: :eta",
  total = n_steps, clear = FALSE, width = 60
)

for (sz in names(grid_sizes)) {
  pb$tick()
  cell_size <- grid_sizes[[sz]]
  message(sprintf("\nSTARTING: grid size %s (%s meters per cell)", sz, cell_size))
  t_start <- Sys.time()

  gc()

  # Create grids and tables
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  grid_counts_by_type <- grid_tables$grid_counts_by_type
  grid_richness_by_type <- grid_tables$grid_richness_by_type
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))

  # Mask grid to land (project world to same CRS first)
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)

  # grid_with_counts / grid_with_richness are full grid objects; mask them to land
  grid_land <- st_filter(grid_with_counts, world_proj)
  grid_richness_land <- st_filter(grid_with_richness, world_proj)

  # per-record_type grids (grid_rt_sf with record_type column) masked to land
  grid_land_by_type <- st_filter(grid_counts_by_type, world_proj)
  grid_richness_land_by_type <- st_filter(grid_richness_by_type, world_proj)

  # Transform world to plotting CRS (match grid geometry)
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))

  # --- MAP 1: Heat map of occurrence density (20km only) ---
  # only draw grid cells with count > 0 so empty cells don't cover the land background
  message(sprintf("[%s] Generating MAP 1: Global occurrence richness...", format(Sys.time(), "%H:%M:%S")))
  grid_land_nonzero <- grid_land %>% filter(count > 0)

  density_plot <- ggplot() +
    # land fill in the medium-grey used in the old example
    geom_sf(data = world_plot, fill = bg_land_fill, color = NA, alpha = 1) +
    # draw only non-zero cells
    geom_sf(data = grid_land_nonzero, aes(fill = count), color = NA, alpha = 1) +
    # country borders ON TOP; use white thin borders like the example
    geom_sf(data = world_plot, fill = NA, color = "white", size = 0.25, alpha = 1) +
    scale_fill_gradient(
      low = "yellow", high = "red",
      trans = scales::pseudo_log_trans(base = 10),
      name = sprintf("Occurrences (%s)", sz),
      breaks = legend_breaks,
      labels = as.character(legend_breaks),
      guide = legend_guide
    ) +
    labs(title = "Global occurrence richness", fill = "Occurrences") +
    shared_theme +
    coord_sf(expand = FALSE)

  out_pdf1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.pdf", sz))
  out_png1 <- file.path(output_dir, sprintf("map01_occurrence_density_%s.png", sz))
  ggsave(out_pdf1, plot = density_plot, width = 14, height = 7, units = "in",
         device = cairo_pdf, bg = "white")
  ggsave(out_png1, plot = density_plot, width = 14, height = 7, units = "in",
         dpi = 300, device = ragg::agg_png, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf1), basename(out_png1)))

  # --- MAP 2: Heat map of species richness (20km only) ---
  message(sprintf("[%s] Generating MAP 2: Global species richness...", format(Sys.time(), "%H:%M:%S")))
  grid_richness_nonzero <- grid_richness_land %>% filter(richness > 0)

  richness_plot <- ggplot() +
    geom_sf(data = world_plot, fill = bg_land_fill, color = NA, alpha = 1) +
    geom_sf(data = grid_richness_nonzero, aes(fill = richness), color = NA, alpha = 1) +
    geom_sf(data = world_plot, fill = NA, color = "white", size = 0.25, alpha = 1) +
    scale_fill_gradient(
      low = "lightgreen", high = "darkgreen",
      trans = scales::pseudo_log_trans(base = 10),
      name = sprintf("Species Richness (%s)", sz),
      breaks = legend_breaks,
      labels = as.character(legend_breaks),
      guide = legend_guide
    ) +
    labs(title = sprintf("Global species richness, %s", sz), fill = "Richness") +
    shared_theme +
    coord_sf(expand = FALSE)

  out_pdf2 <- file.path(output_dir, sprintf("map02_species_richness_%s.pdf", sz))
  out_png2 <- file.path(output_dir, sprintf("map02_species_richness_%s.png", sz))
  ggsave(out_pdf2, plot = richness_plot, width = 14, height = 7, units = "in",
         device = cairo_pdf, bg = "white")
  ggsave(out_png2, plot = richness_plot, width = 14, height = 7, units = "in",
         dpi = 300, device = ragg::agg_png, bg = "white")
  message(sprintf("[%s] Saved %s and %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf2), basename(out_png2)))

  # MAP 3 & MAP 4 remain hashed out in this version.

  t_end <- Sys.time()
  message(sprintf("Finished grid size %s in %s", sz, format(difftime(t_end, t_start, units = "mins"))))
}

# End of script





