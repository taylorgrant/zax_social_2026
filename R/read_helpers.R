read_follower_files <- function(f, sheet_data) {
  fb <- sum(
    readr::read_csv(f["fb"], skip = 1, show_col_types = FALSE)$Primary
  ) +
    dplyr::last(sheet_data$Followers$facebook)

  ig <- sum(
    readr::read_csv(f["ig"], skip = 1, show_col_types = FALSE)$Primary
  ) +
    dplyr::last(sheet_data$Followers$instagram)

  tt <- dplyr::last(readr::read_csv(f["tt"], show_col_types = FALSE)$Followers)

  x <- dplyr::last(readr::read_csv(f["x"], show_col_types = FALSE)$Followers)

  c(fb = fb, ig = ig, tt = tt, x = x)
}

read_fb_posts <- function(file) {
  fb_base <- readr::read_csv(file["fb"], show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(
        publish_time,
        orders = c("mdy HM", "mdy")
      ))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = reactions_comments_and_shares
    )
}

read_ig_posts <- function(file) {
  igp_base <- readr::read_csv(file["igp"], show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(
        publish_time,
        orders = c("mdy HM", "mdy")
      )),
      boosted = tidyr::replace_na(boosted, 0)
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + shares + comments + saves,
    )
}

read_ig_stories <- function(file) {
  igs_base <- readr::read_csv(file["igs"], show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(
        publish_time,
        orders = c("mdy HM", "mdy")
      ))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + shares + replies + sticker_taps,
      engagements = tidyr::replace_na(engagements, 0)
    )
}

read_tt_posts <- function(file) {
  tt_base <- readr::read_csv(file["tt"], show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(
        post_time,
        orders = c("mdy HM", "mdy")
      )),
      dplyr::across(co_posted:boosted, ~ tidyr::replace_na(., 0))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      views = video_views,
      engagements = likes + comments + shares + add_to_favorites,
    )
}

read_x_posts <- function(file) {
  x_base <- readr::read_csv(file["x"], show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(
        date,
        orders = c("mdy HM", "mdy")
      ))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + comments + shares + saves
    )
}

read_monthly_performance_files <- function(monthyear) {
  files <- performance_file_paths(monthyear)

  list(
    files = files,
    base = list(
      fb = read_fb_posts(files["fb"]),
      igp = read_ig_posts(files["igp"]),
      igs = read_ig_stories(files["igs"]),
      tt = read_tt_posts(files["tt"]),
      x = read_x_posts(files["x"])
    )
  )
}

read_all_sheets <- function(ga_id) {
  sheet_names <- googlesheets4::sheet_names(ga_id)

  purrr::set_names(
    purrr::map(
      sheet_names,
      ~ googlesheets4::read_sheet(ss = ga_id, sheet = .x) |>
        janitor::clean_names() |>
        dplyr::mutate(
          dplyr::across(dplyr::any_of("month"), as.Date)
        )
    ),
    sheet_names
  )
}
