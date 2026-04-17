# Function for plotting
run_plots <- function(base_family = "sans") {
  source(here::here("R", "helpers", "plot_helpers.R"))
  # bring in the updated google sheets data
  ga_id <- get_config()$ga_id

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
  boosted_plot(data = all_sheets, "TikTok", base_family = base_family)
  boosted_plot(data = all_sheets, "IG Post", base_family = base_family)

  # Now Followers
  follower_plot(data = all_sheets, base_family = base_family)

  # Organic
  organic_plot(data = all_sheets, "Facebook", base_family = base_family)
  organic_plot(data = all_sheets, "IG Post", base_family = base_family)
  organic_plot(data = all_sheets, "IG Stories", base_family = base_family)
  organic_plot(data = all_sheets, "TikTok", base_family = base_family)
  organic_plot(data = all_sheets, "X", base_family = base_family)

  # Overall
  overall_plot(data = all_sheets, "Facebook", base_family = base_family)
  overall_plot(data = all_sheets, "IG Post", base_family = base_family)
  overall_plot(data = all_sheets, "IG Stories", base_family = base_family)
  overall_plot(data = all_sheets, "TikTok", base_family = base_family)
  overall_plot(data = all_sheets, "X", base_family = base_family)
}
