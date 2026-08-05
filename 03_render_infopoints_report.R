### render infopoints data quality report to onedrive output folder

# set-up ---------------------------------------------------------------------------------------------------------------
pacman::p_load(quarto)


# render ---------------------------------------------------------------------------------------------------------------

output_dir  = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/rimo_infopoints/lr_report_weekly"
input_file  = "/Users/irf/GitHub/agf_lr_report/02_infopoints_dashboard_long_2026_08_03_baulos.qmd"

# TODO - change date to respective monday
output_file = file.path(output_dir, "LR_infopoints_report_2026_08_03.html")

# create output directory if it doesn't exist yet
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

quarto_render(
  input       = input_file,
  output_file = output_file
)

cat(sprintf("report written to:\n  %s\n", output_file))