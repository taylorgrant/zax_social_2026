library(googledrive)
options(gargle_oauth_email = "gspanalytics21@gmail.com")
ga_id <- "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic"

# read in platform data --------------------------------------------------
ig_post <- readxl::read_excel(here::here(
  "data",
  "bm_data",
  "ig_posts_jan25_dec25.xlsx"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M"),
    boosted = tidyr::replace_na(boosted, 0)
  )

ig_stories <- readr::read_csv(here::here(
  "data",
  "bm_data",
  "ig_stories_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
  )

tiktok <- readr::read_csv(here::here(
  "data",
  "bm_data",
  "tiktok_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(post_time, format = "%m/%d/%y %H:%M"),
    dplyr::across(co_posted:boosted, ~ tidyr::replace_na(., 0))
  )

facebook <- readr::read_csv(here::here(
  "data",
  "bm_data",
  "fb_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(publish_time, format = "%m/%d/%y %H:%M")
  )

x <- readr::read_csv(here::here(
  "data",
  "bm_data",
  "x_posts_jan25_dec25.csv"
)) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(date, format = "%m/%d/%y")
  )


# estimate views and engagement rate -------------------------------------

# facebook
perf_fb <- facebook |>
  dplyr::filter(
    date >= as.Date("2025-06-01"),
    date < as.Date("2025-12-01")
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month")
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(reactions_comments_and_shares),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n(),
  ) |>
  dplyr::mutate(platform = "Facebook")
googlesheets4::sheet_write(perf_fb, ss = ga_id, sheet = "Facebook")

# IG posts - OVERALL (includes bosted and co-posted)
perf_igp <- ig_post |>
  dplyr::filter(
    date >= "2025-06-01",
    date < "2025-12-01",
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    engagements = likes + shares + comments + saves,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "IG Post Overall")

# IG posts - ORGANIC
perf_igp_organic <- ig_post |>
  dplyr::filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
    boosted != 1
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    engagements = likes + shares + comments + saves,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "IG Post Organic")

# IG posts - BOOSTED
perf_igp_boosted <- ig_post |>
  dplyr::filter(
    # account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
    boosted == 1
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    engagements = likes + shares + comments + saves,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "IG Post Boosted")

# IG posts - COPOSTED
perf_igp_copost <- ig_post |>
  dplyr::filter(
    account_username != "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
    boosted != 1
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    engagements = likes + shares + comments + saves,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "IG Post Coposted")

# write to IG Posts
googlesheets4::sheet_write(
  arrange(
    rbind(perf_igp, perf_igp_organic, perf_igp_boosted, perf_igp_copost),
    month
  ),
  ss = ga_id,
  sheet = "IG Post"
)

perf_igs <- ig_stories |>
  dplyr::filter(
    account_username == "realzaxbys",
    date >= "2025-06-01",
    date < "2025-12-01",
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    engagements = likes + shares + replies + sticker_taps,
    engagements = tidyr::replace_na(engagements, 0)
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "IG Stories")
googlesheets4::sheet_write(
  perf_igs,
  ss = ga_id,
  sheet = "IG Stories"
)

# TikTok - OVERALL (includes boosted and co-posted)
perf_tt <- tiktok |>
  dplyr::filter(
    # boosted != 1,
    # co_posted != 1,
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    views = video_views,
    engagements = likes + comments + shares + add_to_favorites,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "TikTok Overall")

# TikTok - ORGANIC
perf_tt_organic <- tiktok |>
  dplyr::filter(
    boosted != 1,
    co_posted != 1,
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    views = video_views,
    engagements = likes + comments + shares + add_to_favorites,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "TikTok Organic")

# TikTok - boosted
perf_tt_boosted <- tiktok |>
  dplyr::filter(
    boosted == 1,
    # co_posted != 1,
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    views = video_views,
    engagements = likes + comments + shares + add_to_favorites,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "TikTok Boosted")

# TikTok - boosted
perf_tt_copost <- tiktok |>
  dplyr::filter(
    # boosted == 1,
    co_posted == 1,
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    views = video_views,
    engagements = likes + comments + shares + add_to_favorites,
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = dplyr::n()
  ) |>
  dplyr::mutate(platform = "TikTok Coposted")

# write to TikTok
googlesheets4::sheet_write(
  arrange(
    rbind(perf_tt, perf_tt_organic, perf_tt_boosted, perf_tt_copost),
    month
  ),
  ss = ga_id,
  sheet = "TikTok"
)

perf_x <- x |>
  filter(
    date >= "2025-06-01",
    date < "2025-12-01"
  ) |>
  mutate(
    month = floor_date(date, "month"),
    engagements = likes + comments + shares + saves
  ) |>
  group_by(month) |>
  summarise(
    engagements = sum(engagements),
    total_views = sum(views),
    avg_views = round(mean(views), 0),
    er = round(engagements / total_views, 5),
    posts = n()
  ) |>
  mutate(platform = "X")
googlesheets4::sheet_write(
  perf_x,
  ss = ga_id,
  sheet = "X"
)

overall <- dplyr::bind_rows(perf_fb, perf_igp, perf_igs, perf_tt, perf_x) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    engagements = sum(engagements),
    total_views = sum(total_views),
    er = engagements / total_views,
    posts = sum(posts),
    avg_views = round(total_views / posts, 0)
  ) |>
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

# write the base levels to google sheets

googlesheets4::sheet_write(overall, ss = ga_id, sheet = "Overall")
