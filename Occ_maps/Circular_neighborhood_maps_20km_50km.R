# Robusted workflow: produce circular neighborhood maps (20km & 50km) computed on 20km base grid
# - Fixes the "no simple features geometry column present" error when building buffers_sf
# - Keeps original pipeline behavior otherwise (data loading, CRS handling, grid creation, masking)
#
# Requirements: sf, dplyr, ggplot2, rnaturalearth, progress, ragg

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(progress)
library(ragg)

######### NEW DATA 2025_12_12 ############

# new veg list 1
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist1/occurrences_data_veglist1.xlsx")

# new veglist 2
#occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist2/occurrences_data_veglist2.xlsx")

# veglist 3 shouldnt change
# veglist 4 shouldnt change

# new veglist 5
occurrences_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/new_veg_list_data/Occurrence_data/veglist5/occurrences_data_veglist5.xlsx")

# Produce circular-neighborhood maps:
# - 20 km circular neighborhood summaries computed on the 20 km base grid
# - 50 km circular neighborhood summaries computed on the 50 km base grid
#
# Robust handling of centroids/buffers to avoid "no simple features geometry column present"
# Outputs PNG and PDF maps into output_dir (create if missing)
#
# Requirements: sf, dplyr, ggplot2, rnaturalearth, progress, ragg

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(progress)
library(ragg)

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

# 4. Helper: create grid and summary tables (counts and richness)
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)
  grid <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")
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

# 5. CRS and grid sizes (process only 20km and 50km base grids)
crs_equalarea <- 8857 # Equal Earth
grid_sizes <- c("20km" = 20000, "50km" = 50000)

# Pre-project occurrences once for neighborhood intersection tests
occurrences_proj_global <- st_transform(occurrences_land, crs = crs_equalarea)

# Ensure the output directory exists
# Please adjust output_dir to the desired path or replace "veglist" with your chosen subfolder.
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist1", "2025_12_12")
#output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist2", "2025_12_12")
output_dir <- file.path("new_veg_list_data", "Veggie_maps", "veglist5", "2025_12_12")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Progress bar
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
  
  # Create grid and summary tables
  message(sprintf("[%s] Running make_grid_and_tables...", format(Sys.time(), "%H:%M:%S")))
  grid_tables <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
  grid_with_counts <- grid_tables$grid_with_counts
  grid_with_richness <- grid_tables$grid_with_richness
  message(sprintf("[%s] Finished make_grid_and_tables.", format(Sys.time(), "%H:%M:%S")))
  
  # Mask grid to land
  message(sprintf("[%s] Masking grid to land...", format(Sys.time(), "%H:%M:%S")))
  world_proj <- st_transform(world_4326, crs = crs_equalarea)
  grid_land <- st_filter(grid_with_counts, world_proj)            # equal-area CRS
  grid_richness_land <- st_filter(grid_with_richness, world_proj) # equal-area CRS
  world_plot <- st_transform(world_4326, crs = st_crs(grid_land))
  message(sprintf("[%s] Finished masking grid to land.", format(Sys.time(), "%H:%M:%S")))
  
  # Compute circular neighborhood only for the matching radius:
  # - for 20km base grid compute 20km circular neighborhood
  # - for 50km base grid compute 50km circular neighborhood
  if (sz %in% c("20km", "50km")) {
    # map radius name -> numeric distance (meters)
    radius_map <- list("20km" = 20000, "50km" = 50000)
    # pick the matching radius for this base grid (same as sz)
    rname <- sz
    buffer_dist <- radius_map[[rname]]
    
    message(sprintf("[%s] Computing %s circular neighborhood summaries on %s base grid...", format(Sys.time(), "%H:%M:%S"), rname, sz))
    
    # sanity checks
    if (!"grid_id" %in% names(grid_land)) stop("grid_land does not contain grid_id column")
    if (!"grid_id" %in% names(grid_richness_land)) stop("grid_richness_land does not contain grid_id column")
    
    # Compute centroids as an sfc geometry vector (avoid returning an sf)
    centroids_sfc <- st_geometry(st_centroid(grid_land))
    
    # Build sfc buffers (sfc vector equal length to centroids_sfc)
    buffers_sfc <- st_buffer(centroids_sfc, dist = buffer_dist)
    
    # Sanity check: ensure number of buffers equals number of grid rows
    if (length(buffers_sfc) != nrow(grid_land)) {
      stop(sprintf("Length mismatch: length(buffers) = %d, nrow(grid_land) = %d",
                   length(buffers_sfc), nrow(grid_land)))
    }
    
    # Build a proper sf with explicit geometry column and same CRS as grid_land
    buffers_sf <- st_sf(grid_id = grid_land$grid_id, geometry = buffers_sfc)
    st_crs(buffers_sf) <- st_crs(grid_land)
    
    # Compute intersections with pre-projected occurrences
    ints <- st_intersects(buffers_sf, occurrences_proj_global)
    
    # Occurrence counts within neighborhood
    nb_counts <- lengths(ints)
    # Species richness within neighborhood
    nb_richness <- sapply(ints, function(idx) {
      if (length(idx) == 0) return(0L)
      length(unique(occurrences_proj_global$genus_species[idx]))
    })
    
    # Name by grid_id for robust alignment
    names(nb_counts) <- as.character(buffers_sf$grid_id)
    names(nb_richness) <- as.character(buffers_sf$grid_id)
    
    # Attach to grid polygons
    grid_nb_counts <- grid_land
    grid_nb_counts$nb_count <- nb_counts[as.character(grid_land$grid_id)]
    grid_nb_counts$nb_count[is.na(grid_nb_counts$nb_count)] <- 0
    
    grid_nb_richness <- grid_richness_land
    grid_nb_richness$nb_richness <- nb_richness[as.character(grid_richness_land$grid_id)]
    grid_nb_richness$nb_richness[is.na(grid_nb_richness$nb_richness)] <- 0
    
    # --- MAP: neighborhood occurrence counts ---
    message(sprintf("[%s] Generating neighborhood density plot (%s base grid with %s neighborhood)...", format(Sys.time(), "%H:%M:%S"), sz, rname))
    neigh_density_plot <- ggplot() +
      geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) +
      geom_sf(data = dplyr::filter(grid_nb_counts, nb_count > 0), aes(fill = nb_count), color = NA) +
      geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) +
      scale_fill_gradient(low = "yellow", high = "red", trans = "log10", name = sprintf("Occurrences (%s grid, %s nb)", sz, rname)) +
      labs(title = sprintf("Occurrence density (%s grid, %s circular neighborhood)", sz, rname), fill = "Occurrences") +
      theme_minimal()
    
    out_png_density <- file.path(output_dir, sprintf("map_occurrence_density_%s_%sNb.png", sz, rname))
    ggsave(out_png_density, plot = neigh_density_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white", device = ragg::agg_png)
    message(sprintf("[%s] Saved %s", format(Sys.time(), "%H:%M:%S"), basename(out_png_density)))
    # Save PDF as well (same base filename, .pdf extension)
    out_pdf_density <- file.path(output_dir, sprintf("map_occurrence_density_%s_%sNb.pdf", sz, rname))
    ggsave(out_pdf_density, plot = neigh_density_plot, width = 14, height = 7, units = "in", device = "pdf", bg = "white")
    message(sprintf("[%s] Saved %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf_density)))
    
    # --- MAP: neighborhood species richness ---
    message(sprintf("[%s] Generating neighborhood richness plot (%s base grid with %s neighborhood)...", format(Sys.time(), "%H:%M:%S"), sz, rname))
    neigh_richness_plot <- ggplot() +
      geom_sf(data = world_plot, fill = "#F2F2F2", color = NA) +
      geom_sf(data = dplyr::filter(grid_nb_richness, nb_richness > 0), aes(fill = nb_richness), color = NA) +
      geom_sf(data = world_plot, fill = NA, color = "#D8D8D8", size = 0.2) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen", trans = "log10", name = sprintf("Species Richness (%s grid, %s nb)", sz, rname)) +
      labs(title = sprintf("Species richness (%s grid, %s circular neighborhood)", sz, rname), fill = "Richness") +
      theme_minimal()
    
    out_png_richness <- file.path(output_dir, sprintf("map_species_richness_%s_%sNb.png", sz, rname))
    ggsave(out_png_richness, plot = neigh_richness_plot, width = 14, height = 7, units = "in", dpi = 300, bg = "white", device = ragg::agg_png)
    message(sprintf("[%s] Saved %s", format(Sys.time(), "%H:%M:%S"), basename(out_png_richness)))
    # Save PDF as well (same base filename, .pdf extension)
    out_pdf_richness <- file.path(output_dir, sprintf("map_species_richness_%s_%sNb.pdf", sz, rname))
    ggsave(out_pdf_richness, plot = neigh_richness_plot, width = 14, height = 7, units = "in", device = "pdf", bg = "white")
    message(sprintf("[%s] Saved %s", format(Sys.time(), "%H:%M:%S"), basename(out_pdf_richness)))
  } else {
    message(sprintf("[%s] Skipping neighborhood computation for grid size %s", format(Sys.time(), "%H:%M:%S"), sz))
  }
  
  t_end <- Sys.time()
  message(sprintf("Finished grid size %s in %s", sz, format(difftime(t_end, t_start, units = "mins"))))
}
