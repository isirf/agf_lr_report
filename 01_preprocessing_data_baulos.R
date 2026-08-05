# =============================================================================
# preprocessing_baulos.R
#
# Spatially matches infopoints to Baulos construction clusters via polygon
# intersection. Run this script once (or whenever the GeoJSON or infopoints
# CSV changes). Output: baulos_lookup.csv in input_path.
#
# Output columns:
#   External ID  — infopoint identifier (joins to infopoints CSV)
#   Planned by   — project name (joins to infopoints / project_list)
#   baulos_name  — Baulos cluster Name from GeoJSON
#
# Infopoints in overlapping polygons appear once per polygon they fall in.
# Infopoints outside all polygons are excluded (no row in the output).
# =============================================================================

library(tidyverse)
library(readxl)
library(sf)

# ── CONFIG ────────────────────────────────────────────────────────────────────
# Update these paths/filenames to match your current exports.

base_path  <- "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/rimo_infopoints/"
input_path <- paste0(base_path, "input_files/")

infopoints_file <- "Customized_InfoLocations_privates_LR_2026_08_03.csv"
excel_file      <- "LR_Report_Gemeinden_2027_07_27.xlsx"

#baulos_path <- "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/baulos_construction-clusters_at_magenta_2026-05-06.geojson"
baulos_path <- "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/baulos_construction-clusters_at_magenta_2026-08-05.geojson"


output_file <- paste0(input_path, "baulos_lookup_2026_08_05.csv")

# ── FAIL FAST ─────────────────────────────────────────────────────────────────
required_files <- c(
  paste0(input_path, infopoints_file),
  paste0(input_path, excel_file),
  baulos_path
)
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  stop("File(s) not found — check CONFIG paths:\n  ", paste(missing, collapse = "\n  "))
}

# ── LOAD INFOPOINTS ───────────────────────────────────────────────────────────
cat("Loading infopoints...\n")
infopoints_raw <- read_csv2(
  paste0(input_path, infopoints_file),
  name_repair = "minimal",
  show_col_types = FALSE
) |>
  select(-last_col())

# Keep only rows with valid coordinates
infopoints_coords <- infopoints_raw |>
  select(`External ID`, `Planned by`, Latitude, Longitude) |>
  filter(!is.na(Latitude) & !is.na(Longitude) &
           Latitude != "" & Longitude != "") |>
  mutate(
    Latitude  = as.numeric(str_replace(Latitude,  ",", ".")),
    Longitude = as.numeric(str_replace(Longitude, ",", "."))
  ) |>
  filter(!is.na(Latitude) & !is.na(Longitude))

cat(sprintf("  %d infopoints with valid coordinates (of %d total)\n",
            nrow(infopoints_coords), nrow(infopoints_raw)))

# ── LOAD PROJECT MATCHING TABLE ───────────────────────────────────────────────
cat("Loading project matching table...\n")
proj_id_lookup <- read_excel(
  paste0(input_path, excel_file),
  sheet = "Gemeinde PB Zuordnung"
) |>
  rename(`Planned by` = Gemeinden) |>
  select(`Planned by`, project_id_ods) |>
  filter(!is.na(project_id_ods)) |>
  distinct() |>
  mutate(project_id_ods = as.character(project_id_ods))

cat(sprintf("  %d projects with a project_id_ods\n", nrow(proj_id_lookup)))

# ── LOAD BAULOS GEOJSON ───────────────────────────────────────────────────────
cat("Loading Baulos GeoJSON...\n")
# Use planar geometry (s2 = FALSE) — the GeoJSON has some degenerate polygons
# with duplicate vertices that S2's strict validator rejects. st_make_valid()
# repairs them before the spatial join.
sf::sf_use_s2(FALSE)
baulos_sf <- sf::st_read(baulos_path, quiet = TRUE) |>
  sf::st_make_valid() |>
  select(baulos_name = Name, ProjectId) |>
  mutate(ProjectId = as.character(ProjectId)) |>
  filter(!sf::st_is_empty(geometry))

cat(sprintf("  %d Baulos polygons loaded\n", nrow(baulos_sf)))

# ── CONVERT INFOPOINTS TO SF ──────────────────────────────────────────────────
cat("Converting infopoints to spatial points...\n")
infopoints_sf <- infopoints_coords |>
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs    = 4326,
    remove = FALSE
  )

# ── SPATIAL JOIN ──────────────────────────────────────────────────────────────
cat("Running spatial intersection (this may take a moment)...\n")
joined <- sf::st_join(infopoints_sf, baulos_sf, join = sf::st_within) |>
  sf::st_drop_geometry() |>
  filter(!is.na(baulos_name))

cat(sprintf("  %d infopoint × Baulos matches before project filter\n", nrow(joined)))

# ── FILTER: KEEP ONLY SAME-PROJECT MATCHES ────────────────────────────────────
# Guards against infopoints being matched to polygons from a different project
# when polygons overlap across project boundaries.
cat("Filtering to same-project matches...\n")
baulos_lookup <- joined |>
  left_join(proj_id_lookup, by = "Planned by") |>
  filter(!is.na(project_id_ods) & ProjectId == project_id_ods) |>
  select(`External ID`, `Planned by`, baulos_name) |>
  distinct() |>
  arrange(`Planned by`, baulos_name, `External ID`)

n_infopoints_matched <- n_distinct(baulos_lookup$`External ID`)
n_projects_matched   <- n_distinct(baulos_lookup$`Planned by`)
n_baulose            <- n_distinct(baulos_lookup$baulos_name)

cat(sprintf("  %d matches kept after project filter\n", nrow(baulos_lookup)))
cat(sprintf("  %d unique infopoints matched\n", n_infopoints_matched))
cat(sprintf("  %d projects with at least one Baulos match\n", n_projects_matched))
cat(sprintf("  %d distinct Baulos names\n", n_baulose))

# ── WRITE OUTPUT ──────────────────────────────────────────────────────────────
cat(sprintf("Writing output to %s...\n", output_file))
write_csv(baulos_lookup, output_file)
cat("Done.\n\n")

cat("Summary of matches per project:\n")
baulos_lookup |>
  group_by(`Planned by`) |>
  summarise(
    Baulose    = n_distinct(baulos_name),
    Infopoints = n_distinct(`External ID`),
    .groups    = "drop"
  ) |>
  arrange(desc(Infopoints)) |>
  print(n = 30)

# =============================================================================
# FttX Locations — Baulos spatial join
# =============================================================================
# Output: baulos_lookup_fttx_<date>.csv in input_path
# Columns: External ID, POP cluster (= Planned by), baulos_name
# =============================================================================

fttx_file <- "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/Queries_FttxLocations_2026_08_04.csv"
fttx_output_file <- paste0(input_path, "baulos_lookup_fttx_2026_08_04.csv")

if (!file.exists(fttx_file)) {
  warning("FttX file not found — skipping FttX Baulos join:\n  ", fttx_file)
} else {
  
  cat("\n── FttX Locations Baulos join ────────────────────────────────────────────\n")
  cat("Loading FttX locations...\n")
  
  fttx_raw <- read_csv2(fttx_file, name_repair = "minimal", show_col_types = FALSE) |>
    select(-last_col()) |>           # drop trailing empty column
    select(-any_of("Planned by"))    # drop existing Planned by to avoid duplicate on rename
  
  # Keep only rows with valid coordinates and a POP cluster
  fttx_coords <- fttx_raw |>
    select(`External ID`, `POP cluster`, Latitude, Longitude) |>
    filter(!is.na(`POP cluster`) & `POP cluster` != "") |>
    filter(!is.na(Latitude) & !is.na(Longitude) &
             Latitude != "" & Longitude != "") |>
    mutate(
      Latitude  = as.numeric(str_replace(as.character(Latitude),  ",", ".")),
      Longitude = as.numeric(str_replace(as.character(Longitude), ",", "."))
    ) |>
    filter(!is.na(Latitude) & !is.na(Longitude))
  
  cat(sprintf("  %d FttX locations with valid coordinates (of %d total)\n",
              nrow(fttx_coords), nrow(fttx_raw)))
  
  # Build POP cluster → project_id_ods lookup
  # POP cluster matches Planned by (= Gemeinden) directly
  fttx_proj_lookup <- proj_id_lookup |>
    rename(`POP cluster` = `Planned by`)
  
  # Convert to sf
  cat("Converting FttX locations to spatial points...\n")
  fttx_sf <- fttx_coords |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  # Spatial join against the already-loaded baulos_sf
  cat("Running spatial intersection for FttX...\n")
  fttx_joined <- sf::st_join(fttx_sf, baulos_sf, join = sf::st_within) |>
    sf::st_drop_geometry() |>
    filter(!is.na(baulos_name))
  
  cat(sprintf("  %d FttX × Baulos matches before project filter\n", nrow(fttx_joined)))
  
  # Filter: keep only same-project matches
  cat("Filtering to same-project matches...\n")
  baulos_lookup_fttx <- fttx_joined |>
    left_join(fttx_proj_lookup, by = "POP cluster") |>
    filter(!is.na(project_id_ods) & ProjectId == project_id_ods) |>
    select(`External ID`, `POP cluster`, baulos_name) |>
    distinct() |>
    arrange(`POP cluster`, baulos_name, `External ID`)
  
  cat(sprintf("  %d matches kept after project filter\n", nrow(baulos_lookup_fttx)))
  cat(sprintf("  %d unique FttX locations matched\n", n_distinct(baulos_lookup_fttx$`External ID`)))
  cat(sprintf("  %d projects with at least one Baulos match\n", n_distinct(baulos_lookup_fttx$`POP cluster`)))
  
  cat(sprintf("Writing FttX Baulos lookup to %s...\n", fttx_output_file))
  write_csv(baulos_lookup_fttx, fttx_output_file)
  cat("Done.\n\n")
  
  cat("Summary of FttX matches per project:\n")
  baulos_lookup_fttx |>
    group_by(`POP cluster`) |>
    summarise(
      Baulose   = n_distinct(baulos_name),
      Locations = n_distinct(`External ID`),
      .groups   = "drop"
    ) |>
    arrange(desc(Locations)) |>
    print(n = 30)
}
