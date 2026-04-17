# Setting Organic Benchmarks by Platform #

pacman::p_load(tidyverse, janitor, here, glue)

# READ IN PLATFORM DATA --------------------------------------------------

ig_post <- readxl::read_excel(here::here(
  "data",
  "bm_data",
  "ig_posts_jan25_dec25.xlsx"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time),
    boosted = tidyr::replace_na(boosted, 0)
  )

ig_stories <- readr::read_csv(here(
  "data",
  "bm_data",
  "ig_stories_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(lubridate::parse_date_time(
      publish_time,
      orders = c("mdy HM", "mdy")
    ))
  )

tiktok <- readr::read_csv(here(
  "data",
  "bm_data",
  "tiktok_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(lubridate::parse_date_time(
      post_time,
      orders = c("mdy HM", "mdy")
    )),
    across(co_posted:boosted, ~ tidyr::replace_na(., 0))
  )

facebook <- readr::read_csv(here(
  "data",
  "bm_data",
  "fb_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(lubridate::parse_date_time(
      publish_time,
      orders = c("mdy HM", "mdy")
    ))
  )

twitter <- readr::read_csv(here(
  "data",
  "bm_data",
  "x_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(lubridate::parse_date_time(
      date,
      orders = c("mdy HM", "mdy")
    ))
  )

# BENCHMARKS -------------------------------------------------------------

# setting up quantiles (log, trim outliers, and quantile)
robust_quantile <- function(x, prob, trim = 0.97) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  x_log <- log1p(x)
  cap <- quantile(x_log, probs = trim, na.rm = TRUE)
  x_log <- pmin(x_log, cap)

  exp(quantile(x_log, probs = prob, na.rm = TRUE)) - 1
}

# working off of thresholds: 40, 65, 95
p <- c(.4, .5, .65, .95)
p_funs <- map(
  p,
  ~ function(x) robust_quantile(x, prob = .x, trim = 0.97)
) |>
  set_names(paste0("p_", p * 100))

# IG Post Benchmarks
thresh_ig_post <- ig_post |>
  dplyr::filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
    boosted != 1
  ) |>
  dplyr::mutate(
    engagements = likes + shares + comments + saves,
    er = engagements / views
  ) |>
  dplyr::summarise(dplyr::across(c(views, er), tibble::lst(!!!p_funs))) |>
  dplyr::mutate(platform = "IG Post")

# IG Stories Benchmarks
thresh_ig_story <- ig_stories |>
  dplyr::filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
  ) |>
  dplyr::mutate(
    engagements = likes + shares + replies + sticker_taps,
    er = engagements / views
  ) |>
  dplyr::summarise(dplyr::across(c(views, er), tibble::lst(!!!p_funs))) |>
  dplyr::mutate(platform = "IG Stories")

thresh_tt <- tiktok |>
  dplyr::filter(
    boosted != 1,
    co_posted != 1,
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    views = video_views,
    engagements = likes + comments + shares + add_to_favorites,
    er = engagements / views
  ) |>
  dplyr::summarise(dplyr::across(c(views, er), tibble::lst(!!!p_funs))) |>
  dplyr::mutate(platform = "TikTok")

thresh_fb <- facebook |>
  dplyr::filter(
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    er = reactions_comments_and_shares / views
  ) |>
  dplyr::summarise(dplyr::across(c(views, er), tibble::lst(!!!p_funs))) |>
  dplyr::mutate(platform = "Facebook")

thresh_tw <- twitter |>
  dplyr::filter(
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    er = (likes + comments + shares + saves) / views
  ) |>
  dplyr::summarise(dplyr::across(c(views, er), tibble::lst(!!!p_funs))) |>
  dplyr::mutate(platform = "X")

bm_2025 <- dplyr::bind_rows(
  thresh_fb,
  thresh_ig_post,
  thresh_ig_story,
  thresh_tt,
  thresh_tw
)

saveRDS(
  bm_2025,
  here("data", "bm_data", "benchmarks.rds")
)
