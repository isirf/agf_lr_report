### render infopoints data quality report to onedrive output folder

# set-up ---------------------------------------------------------------------------------------------------------------
pacman::p_load(quarto)


# render ---------------------------------------------------------------------------------------------------------------

output_dir  = "/Users/irf/Library/CloudStorage/OneDrive-AlpenGlasfaserGmbH/rimo_infopoints/lr_report_weekly"
input_file  = "/Users/irf/GitHub/agf_lr_report/02_infopoints_dashboard_long_2026_08_03_baulos.qmd"

# TODO - change date to respective monday
output_name = "LR_infopoints_report_2026_08_03.html"

# create output directory if it doesn't exist yet
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

system2(
  command = "quarto",
  args    = c("render", shQuote(input_file),
              "--output-dir", shQuote(output_dir),
              "--output",     output_name)
)

cat(sprintf("report written to:\n  %s\n", file.path(output_dir, output_name)))


