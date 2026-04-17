# Monthly performance and Followers by platform #
monthly_zaxbys <- function(monthyear) {
  # packages
  pacman::p_load(tidyverse, janitor, here, glue, googlesheets4, googledrive)

  # helper functions
  source(here::here("R", "helpers", "helpers.R"))
  source(here::here("R", "run_plots.R"))

  # Read in Google Sheets data
  options(gargle_oauth_email = "gspanalytics21@gmail.com")
  ga_id <- "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic"

  sheet_names <- googlesheets4::sheet_names(ga_id)
  # read in all sheets
  all_sheets <- purrr::map(
    sheet_names,
    ~ googlesheets4::read_sheet(ss = ga_id, sheet = .x) |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of("month"), as.Date)
      )
  )
  names(all_sheets) <- sheet_names

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
  googlesheets4::sheet_write(overall, ss = ga_id, sheet = "Overall")

  # Followers
  googlesheets4::sheet_write(followers, ss = ga_id, sheet = "Followers")

  # Facebook
  googlesheets4::sheet_append(
    performance$perf_fb,
    ss = ga_id,
    sheet = "Facebook"
  )
  # IG Post
  googlesheets4::sheet_append(
    rbind(
      performance$perf_igp,
      performance$perf_igp_organic,
      performance$perf_igp_boosted,
      performance$perf_igp_copost
    ),
    ss = ga_id,
    sheet = "IG Post"
  )

  # IG Stories
  googlesheets4::sheet_append(
    perf_igs,
    ss = ga_id,
    sheet = "IG Stories"
  )

  # TikTok
  googlesheets4::sheet_append(
    rbind(
      performance$perf_tt,
      performance$perf_tt_organic,
      performance$perf_tt_boosted,
      performance$perf_tt_copost
    ),
    ss = ga_id,
    sheet = "TikTok"
  )

  # X
  googlesheets4::sheet_append(
    performance$perf_x,
    ss = ga_id,
    sheet = "X"
  )

  # now that everything has run and pushed to sheets, pull it back and plot monthly
  run_plots()
}
