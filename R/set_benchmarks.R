# Setting Organic Benchmarks by Platform #

pacman::p_load(tidyverse, janitor, here, glue)

# READ IN PLATFORM DATA --------------------------------------------------

ig_post <- readr::read_csv(here(
  "data",
  "bm_data",
  "ig_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M"),
    boosted = tidyr::replace_na(boosted, 0)
  )

ig_stories <- readr::read_csv(here(
  "data",
  "bm_data",
  "ig_stories_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
  )

tiktok <- readr::read_csv(here(
  "data",
  "bm_data",
  "tiktok_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(post_time, format = "%m/%d/%y %H:%M"),
    across(co_posted:boosted, ~ replace_na(., 0))
  )

facebook <- readr::read_csv(here(
  "data",
  "bm_data",
  "fb_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
  )

twitter <- readr::read_csv(here(
  "data",
  "bm_data",
  "x_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(date, format = "%m/%d/%y")
  )

# BENCHMARKS -------------------------------------------------------------

# working off of thresholds: 40, 65, 95
p <- c(.4, .65, .95)
p_funs <- map(
  p,
  ~ function(x) {
    # x <- x[x <= quantile(x, .95, na.rm = TRUE)] # trim top 5%
    quantile(x, probs = .x, na.rm = TRUE)
  }
) %>%
  set_names(paste0("p_", p * 100))

# IG Post Benchmarks
thresh_ig_post <- ig_post |>
  filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2026-12-01",
    boosted != 1
  ) |>
  mutate(
    engagements = likes + shares + comments + shares,
    er = engagements / views
  ) |>
  summarise(across(c(views, er), tibble::lst(!!!p_funs))) |>
  mutate(platform = "IG Posts")

# IG Stories Benchmarks
thresh_ig_story <- ig_stories |>
  filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2026-12-01",
  ) |>
  mutate(
    engagements = likes + shares + sticker_taps,
    er = engagements / views
  ) |>
  summarise(across(c(views, er), tibble::lst(!!!p_funs))) |>
  mutate(platform = "IG Stories")

thresh_tt <- tiktok |>
  filter(
    boosted != 1,
    co_posted != 1,
    date >= "2025-06-01",
    date < "2026-12-01"
  ) |>
  mutate(
    views = video_views,
    engagements = likes + comments + shares,
    er = engagements / views
  ) |>
  summarise(across(c(views, er), tibble::lst(!!!p_funs))) |>
  mutate(platform = "TikTok")

thresh_fb <- facebook |>
  filter(
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  mutate(
    er = reactions_comments_and_shares / views
  ) |>
  summarise(across(c(views, er), tibble::lst(!!!p_funs))) |>
  mutate(platform = "Facebook")

thresh_tw <- twitter |>
  filter(
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  mutate(
    er = (likes + comments + shares + saves) / views
  ) |>
  summarise(across(c(views, er), tibble::lst(!!!p_funs))) |>
  mutate(platform = "X")

bind_rows(thresh_fb, thresh_ig_post, thresh_ig_story, thresh_tt, thresh_tw)
