# Pull in monthly followers from data
monthly_performance <- function(sheet_data, month) {
  platforms <- c("fb", "ig", "tiktok", "x")

  current_month <- as.Date(paste0(month, "-01"), format = "%b%y-%d")

  # FILE LOCATIONS ---------------------------------------------------------
  src_fb <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_posts_{month}.csv")
  )
  src_igp <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_posts_{month}.csv")
  )
  src_igs <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_stories_{month}.csv")
  )
  src_tt <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_posts_{month}.csv")
  )
  src_x <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_posts_{month}.csv")
  )

  # OVERALL PERFORMANCE PER MONTH ------------------------------------------
  fb_base <- readr::read_csv(src_fb, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
    ) |>
    mutate(
      month = floor_date(date, "month")
    )

  fb_overall <- fb_base |>
    group_by(month) |>
    summarise(
      engagements = sum(reactions_comments_and_shares),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "Facebook")

  igp_base <- readr::read_csv(src_igp, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(publish_time, format = "%m/%d/%y %H:%M"),
      boosted = tidyr::replace_na(boosted, 0)
    ) |>
    mutate(
      month = floor_date(date, "month"),
      engagements = likes + shares + comments + shares,
    )

  igp_overall <- igp_base |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "IG Posts")

  igp_organic <- igp_base |>
    filter(
      account_username == "realzaxbys",
      boosted != 1
    ) |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "IG Posts")

  igs_base <- readr::read_csv(src_igs, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
    ) |>
    mutate(
      month = floor_date(date, "month"),
      engagements = likes + shares + sticker_taps,
      engagements = replace_na(engagements, 0)
    )

  igs_overall <- igs_base |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "IG Stories")

  tt_base <- readr::read_csv(src_tt, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(post_time, format = "%m/%d/%y %H:%M"),
      across(co_posted:boosted, ~ replace_na(., 0))
    ) |>
    mutate(
      month = floor_date(date, "month"),
      views = video_views,
      engagements = likes + comments + shares,
    )

  tt_overall <- tt_base |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "TikTok")

  tt_organic <- tt_base |>
    filter(
      boosted != 1,
      co_posted != 1,
    ) |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "TikTok")

  x_base <- readr::read_csv(src_x, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(date, format = "%m/%d/%y")
    ) |>
    mutate(
      month = floor_date(date, "month"),
      engagements = likes + comments + shares + saves
    )

  x_overall <- x_base |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      posts = n()
    ) |>
    mutate(platform = "X")

  month_overall <- bind_rows(
    fb_overall,
    igp_overall,
    igs_overall,
    tt_overall,
    x_overall
  ) |>
    group_by(month) |>
    summarise(
      engagements = sum(engagements),
      views = sum(views),
      er = engagements / views,
      posts = sum(posts)
    )

  # FILE CLEANUP -----------------------------------------------------------

  fs::file_move(
    src_fb,
    here::here("data", "monthly_processed", fs::path_file(src_fb))
  )
  fs::file_move(
    src_igp,
    here::here("data", "monthly_processed", fs::path_file(src_igp))
  )
  fs::file_move(
    src_igs,
    here::here("data", "monthly_processed", fs::path_file(src_igs))
  )
  fs::file_move(
    src_tt,
    here::here("data", "monthly_processed", fs::path_file(src_tt))
  )
  fs::file_move(
    src_x,
    here::here("data", "monthly_processed", fs::path_file(src_x))
  )

  # OUTPUT (FB, IGS, X are always organic)
  list(
    month_overall = month_overall,
    fb_overall = fb_overall,
    igp_organic = igp_organic,
    igs_overall = igs_overall,
    tt_organic = tt_organic,
    x_overall = x_overall
  )
}
