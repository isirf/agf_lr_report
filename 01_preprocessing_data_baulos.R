### baulos spatial join — infopoints, fttx, mdu & sn locations

### spatially match infopoints to baulos construction clusters via polygon intersection
### spatially match fttx locations to baulos construction clusters via polygon intersection
### spatially match mdu durchleitung locations to baulos construction clusters
### spatially match sondernutzung locations to baulos construction clusters

# set-up ---------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, readxl, sf)


# load data ------------------------------------------------------------------------------------------------------------

base_path  = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/rimo_infopoints/"
input_path = file.path(base_path, "input_files/")

infopoints_file = "Customized_InfoLocations_privates_LR_2026_09_04.csv"
excel_file      = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/AGF EPS - Documents/General/Planung/Privates Leitungsrecht & Sondernutzungen/Reporting_LR_SN/Projektzuordnung PB PCM Status_aktuell.xlsx"
baulos_path     = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/construction-clusters_at_magenta_2026-08-31.geojson"
fttx_file       = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/Queries_FttxLocations_2026_09_04.csv"
mdu_file        = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export_rimo/Customized_InfoLocations_MDU_Durchleitungen_2026_09_04.csv"
sn_file         = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export_rimo/Customized_InfoLocations_SN_2026_09_04.csv"

output_file          = file.path(input_path, "baulos_lookup_2026_09_04.csv")
fttx_output_file     = file.path(input_path, "baulos_lookup_fttx_2026_09_04.csv")
mdu_output_file      = file.path(input_path, "baulos_lookup_mdu_2026_09_04.csv")
sn_output_file       = file.path(input_path, "baulos_lookup_sn_2026_09_04.csv")

# fail fast ---------------------------------------------------------------------------------------------------------------

required_files = c(paste0(input_path, infopoints_file), excel_file, baulos_path)
missing = required_files[!file.exists(required_files)]
if (length(missing) > 0) stop("file(s) not found — check paths:\n  ", paste(missing, collapse = "\n  "))

cat("loading project matching table...\n")
proj_id_lookup = read_excel(excel_file, sheet = "Gemeinde PB Zuordnung") %>%
  rename(`Planned by` = Gemeinden) %>%
  select(`Planned by`, project_id_ods) %>%
  filter(!is.na(project_id_ods)) %>%
  distinct() %>%
  mutate(project_id_ods = as.character(project_id_ods))

cat("loading baulos geojson...\n")
# use planar geometry (s2 = FALSE) — the geojson has degenerate polygons with duplicate
# vertices that s2's strict validator rejects; st_make_valid() repairs them before spatial join
sf::sf_use_s2(FALSE)
baulos_sf = sf::st_read(baulos_path, quiet = TRUE) %>%
  sf::st_make_valid() %>%
  select(baulos_name = Name, ProjectId) %>%
  mutate(ProjectId = as.character(ProjectId)) %>%
  filter(!sf::st_is_empty(geometry))

cat(sprintf("  %d baulos polygons loaded\n", nrow(baulos_sf)))


# helper: run spatial join for any infopoints-style file --------------------------------------------------------------

# takes a raw csv path, returns a baulos lookup tibble
# col_planned_by: column name for "Planned by" (differs for fttx which uses "POP cluster")
run_baulos_join = function(file_path, col_planned_by = "Planned by", label = "locations") {
  
  if (!file.exists(file_path)) {
    warning(sprintf("%s file not found — skipping: %s", label, file_path))
    return(NULL)
  }
  
  cat(sprintf("\nloading %s...\n", label))
  raw = read_csv2(file_path, name_repair = "minimal", show_col_types = FALSE) %>%
    select(-last_col())
  
  # rename pop cluster to planned by for uniform handling
  if (col_planned_by != "Planned by" && col_planned_by %in% names(raw)) {
    raw = raw %>% rename(`Planned by` = all_of(col_planned_by))
  }
  
  coords = raw %>%
    select(`External ID`, `Planned by`, Latitude, Longitude) %>%
    filter(!is.na(`Planned by`) & `Planned by` != "") %>%
    filter(!is.na(Latitude) & !is.na(Longitude) & Latitude != "" & Longitude != "") %>%
    mutate(
      Latitude  = as.numeric(str_replace(as.character(Latitude),  ",", ".")),
      Longitude = as.numeric(str_replace(as.character(Longitude), ",", "."))
    ) %>%
    filter(!is.na(Latitude) & !is.na(Longitude))
  
  cat(sprintf("  %d %s with valid coordinates (of %d total)\n",
              nrow(coords), label, nrow(raw)))
  
  cat(sprintf("converting %s to spatial points...\n", label))
  pts_sf = coords %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  cat(sprintf("running spatial intersection for %s...\n", label))
  joined = sf::st_join(pts_sf, baulos_sf, join = sf::st_within) %>%
    sf::st_drop_geometry() %>%
    filter(!is.na(baulos_name))
  
  cat(sprintf("  %d %s x baulos matches before project filter\n", nrow(joined), label))
  
  cat("filtering to same-project matches...\n")
  lookup = joined %>%
    left_join(proj_id_lookup, by = "Planned by") %>%
    filter(!is.na(project_id_ods) & ProjectId == project_id_ods) %>%
    select(`External ID`, `Planned by`, baulos_name) %>%
    distinct() %>%
    arrange(`Planned by`, baulos_name, `External ID`)
  
  cat(sprintf("  %d matches kept after project filter\n",       nrow(lookup)))
  cat(sprintf("  %d unique locations matched\n",                n_distinct(lookup$`External ID`)))
  cat(sprintf("  %d projects with at least one baulos match\n", n_distinct(lookup$`Planned by`)))
  cat(sprintf("  %d distinct baulos names\n",                   n_distinct(lookup$baulos_name)))
  
  lookup
}


# infopoints baulos join -----------------------------------------------------------------------------------------------

cat("\n=== privates LR infopoints ===\n")
baulos_lookup = run_baulos_join(
  paste0(input_path, infopoints_file),
  label = "infopoints"
)

if (!is.null(baulos_lookup)) {
  cat(sprintf("writing output to %s...\n", output_file))
  write_csv(baulos_lookup, output_file)
  cat("done.\n\n")
  
  cat("summary of matches per project:\n")
  baulos_lookup %>%
    group_by(`Planned by`) %>%
    summarise(baulose = n_distinct(baulos_name), infopoints = n_distinct(`External ID`), .groups = "drop") %>%
    arrange(desc(infopoints)) %>%
    print(n = 30)
}


# fttx locations baulos join -------------------------------------------------------------------------------------------

if (!file.exists(fttx_file)) {
  warning("fttx file not found — skipping fttx baulos join:\n  ", fttx_file)
} else {
  cat("\n=== fttx locations ===\n")
  
  fttx_raw = read_csv2(fttx_file, name_repair = "minimal", show_col_types = FALSE) %>%
    select(-last_col()) %>%
    select(-any_of("Planned by"))
  
  fttx_coords = fttx_raw %>%
    select(`External ID`, `POP cluster`, Latitude, Longitude) %>%
    filter(!is.na(`POP cluster`) & `POP cluster` != "") %>%
    filter(!is.na(Latitude) & !is.na(Longitude) & Latitude != "" & Longitude != "") %>%
    mutate(
      Latitude  = as.numeric(str_replace(as.character(Latitude),  ",", ".")),
      Longitude = as.numeric(str_replace(as.character(Longitude), ",", "."))
    ) %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    rename(`Planned by` = `POP cluster`)
  
  cat(sprintf("  %d fttx locations with valid coordinates (of %d total)\n", nrow(fttx_coords), nrow(fttx_raw)))
  
  fttx_sf = fttx_coords %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  fttx_joined = sf::st_join(fttx_sf, baulos_sf, join = sf::st_within) %>%
    sf::st_drop_geometry() %>%
    filter(!is.na(baulos_name))
  
  cat(sprintf("  %d fttx x baulos matches before project filter\n", nrow(fttx_joined)))
  
  baulos_lookup_fttx = fttx_joined %>%
    left_join(proj_id_lookup, by = "Planned by") %>%
    filter(!is.na(project_id_ods) & ProjectId == project_id_ods) %>%
    select(`External ID`, `Planned by`, baulos_name) %>%
    distinct() %>%
    arrange(`Planned by`, baulos_name, `External ID`)
  
  cat(sprintf("  %d matches kept after project filter\n",       nrow(baulos_lookup_fttx)))
  cat(sprintf("  %d unique fttx locations matched\n",           n_distinct(baulos_lookup_fttx$`External ID`)))
  cat(sprintf("  %d projects with at least one baulos match\n", n_distinct(baulos_lookup_fttx$`Planned by`)))
  
  # restore POP cluster name for compatibility with qmd
  baulos_lookup_fttx = baulos_lookup_fttx %>%
    rename(`POP cluster` = `Planned by`)
  
  cat(sprintf("writing fttx baulos lookup to %s...\n", fttx_output_file))
  write_csv(baulos_lookup_fttx, fttx_output_file)
  cat("done.\n\n")
  
  cat("summary of fttx matches per project:\n")
  baulos_lookup_fttx %>%
    group_by(`POP cluster`) %>%
    summarise(baulose = n_distinct(baulos_name), locations = n_distinct(`External ID`), .groups = "drop") %>%
    arrange(desc(locations)) %>%
    print(n = 30)
}


# mdu durchleitung baulos join -----------------------------------------------------------------------------------------

cat("\n=== MDU Durchleitung ===\n")
baulos_lookup_mdu = run_baulos_join(mdu_file, label = "MDU locations")

if (!is.null(baulos_lookup_mdu)) {
  cat(sprintf("writing MDU baulos lookup to %s...\n", mdu_output_file))
  write_csv(baulos_lookup_mdu, mdu_output_file)
  cat("done.\n\n")
  
  cat("summary of MDU matches per project:\n")
  baulos_lookup_mdu %>%
    group_by(`Planned by`) %>%
    summarise(baulose = n_distinct(baulos_name), locations = n_distinct(`External ID`), .groups = "drop") %>%
    arrange(desc(locations)) %>%
    print(n = 30)
}


# sondernutzung baulos join --------------------------------------------------------------------------------------------

cat("\n=== Sondernutzung ===\n")
baulos_lookup_sn = run_baulos_join(sn_file, label = "SN locations")

if (!is.null(baulos_lookup_sn)) {
  cat(sprintf("writing SN baulos lookup to %s...\n", sn_output_file))
  write_csv(baulos_lookup_sn, sn_output_file)
  cat("done.\n\n")
  
  cat("summary of SN matches per project:\n")
  baulos_lookup_sn %>%
    group_by(`Planned by`) %>%
    summarise(baulose = n_distinct(baulos_name), locations = n_distinct(`External ID`), .groups = "drop") %>%
    arrange(desc(locations)) %>%
    print(n = 30)
}


