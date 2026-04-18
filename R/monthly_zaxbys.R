# Monthly performance and Followers by platform #
monthly_zaxbys <- function(monthyear) {
  # ensure proper folder structure
  setup_project_dirs()

  # call font
  base_family <- setup_fonts()

  # Read in Google Sheets data
  options(gargle_oauth_email = "gspanalytics21@gmail.com")
  ga_id <- get_config()$ga_id

  all_sheets <- read_all_sheets(ga_id)

  # monthly followers per platform
  followers <- monthly_followers(sheet_data = all_sheets, monthyear)

  # monthly performance by platform
  performance <- monthly_performance(monthyear)

  # Overall
  overall <- dplyr::select(all_sheets$Overall, month:avg_views) |>
    dplyr::bind_rows(performance$month_overall) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      mom_views = round(
        (total_views - dplyr::lag(total_views)) /
          dplyr::lag(total_views),
        3
      ),
      mom_er = round((er - dplyr::lag(er)) / dplyr::lag(er), 5),
      diff_views = total_views - dplyr::lag(total_views)
    )

  # WRITE DATA TO SHEETS  --------------------------------------------------
  write_monthly_outputs(ga_id, followers, overall, performance)
  # now that everything has run and pushed to sheets, pull it back and plot monthly
  run_plots(base_family = base_family)
  archive_monthly_files(c(followers$files, performance$files), monthyear)
}
