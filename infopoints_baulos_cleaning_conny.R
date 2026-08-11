### get list of baulose from construciton clusters lists
### make list (csv) for Conny with project, baulos name and execution state



# set-up ---------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, readxl, sf, writexl)


# load data ------------------------------------------------------------------------------------------------------------

construction_cluster_sf = read_sf("/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/baulos_construction-clusters_at_magenta_2026-08-05.geojson")

project_list = read_excel("/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/rimo_infopoints/input_files/LR_Report_Gemeinden_2027_07_27.xlsx",
                          sheet = "Gemeinde PB Zuordnung")


# data wrangling -------------------------------------------------------------------------------------------------------

# reduce project list to needed list
project_list_cleaned = project_list %>%
  select(project_id_ods, Gemeinden, `Bau/Planung`)


# make df out of sf
construction_cluster = construction_cluster_sf %>%
  st_drop_geometry()


# remove not needed columns
construction_cluster_cleaned = construction_cluster %>%
  select(ProjectId, Name, ClusterNumber, ExecutionState, Id)


### merge both datasets so we can get RIMO project names into the dataset
construction_cluster_cleaned_rimo_projects = merge(construction_cluster_cleaned, project_list_cleaned,
                                                   by.x = "ProjectId",
                                                   by.y = "project_id_ods")


# reorder the columns and remore ProjectId
construction_cluster_cleaned_rimo_projects_active_projects = construction_cluster_cleaned_rimo_projects %>%
  select(-ProjectId, Gemeinden, Name, ClusterNumber, ExecutionState, `Bau/Planung`, Id) %>%
  filter(`Bau/Planung` != "on hold") %>% 
  mutate(baulos_relevant_conny = "")


# data export ----------------------------------------------------------------------------------------------------------

writexl::write_xlsx(construction_cluster_cleaned_rimo_projects_active_projects, "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/agf_data_export/baulos_construction-clusters_at_magenta_2026-08-05_cleaned.xlsx")










