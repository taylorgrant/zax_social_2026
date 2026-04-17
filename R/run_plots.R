# Function for plotting
run_plots <- function() {
  source(here::here("R", "helpers", "plot_helpers.R"))
  # bring in the updated google sheets data
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

  # now plot

  # Start with Boosted
  boosted_plot(data = all_sheets, "TikTok")
  boosted_plot(data = all_sheets, "IG Post")

  # Now Followers
  follower_plot(data = all_sheets)

  # Organic
  organic_plot(data = all_sheets, "Facebook")
  organic_plot(data = all_sheets, "IG Post")
  organic_plot(data = all_sheets, "IG Stories")
  organic_plot(data = all_sheets, "TikTok")
  organic_plot(data = all_sheets, "X")

  # Overall
  overall_plot(data = all_sheets, "Facebook")
  overall_plot(data = all_sheets, "IG Post")
  overall_plot(data = all_sheets, "IG Stories")
  overall_plot(data = all_sheets, "TikTok")
  overall_plot(data = all_sheets, "X")
}
