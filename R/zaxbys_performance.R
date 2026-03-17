# Monthly performance and Followers by platform #

monthly_zaxbys <- function(month) {
  # packages
  pacman::p_load(tidyverse, janitor, here, glue, googlesheets4, googledrive)

  # helper functions
  source(here::here("R", "helpers", "monthly_followers.R"))
  source(here::here("R", "helpers", "monthly_performance.R"))
  # Read in Google Sheets data
  options(gargle_oauth_email = "gspanalytics21@gmail.com")
  ga_id <- "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic"
  sheet_names <- sheet_names(ga_id)
  # read in all sheets
  all_sheets <- map(
    sheet_names,
    ~ read_sheet(ss = ga_id, sheet = .x) |>
      clean_names() |>
      mutate(
        across(any_of("month"), as.Date)
      )
  )
  names(all_sheets) <- sheet_names

  # monthly followers per platform
  followers <- monthly_followers(all_sheets, month)

  # monthly performance by platform
  performance <- monthly_performance(all_sheets, month)

  # TO DO: need to bring in monthly organic and estimate MoM changes
  # bring in historical data, bind, estimate month over months
  overall <- sheet_data$Overall |>
    bind_rows(month_overall) |>
    mutate(
      mom_views = (views - lag(views)) / lag(views),
      mom_er = (er - lag(er)) / lag(er)
    )
}
