# Pull in monthly followers from data
monthly_followers <- function(sheet_data, monthyear) {
  platforms <- c("fb", "ig", "tiktok", "x")

  current_month <- as.Date(paste0(monthyear, "-01"), format = "%b%y-%d")

  src_fb <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_followers_{monthyear}.csv")
  )
  src_ig <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_followers_{monthyear}.csv")
  )
  src_tt <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_followers_{monthyear}.csv")
  )
  src_x <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_followers_{monthyear}.csv")
  )

  fb <- sum(readr::read_csv(src_fb, skip = 1, show_col_types = FALSE)$Primary) +
    dplyr::last(sheet_data$Followers$facebook)

  ig <- sum(readr::read_csv(src_ig, skip = 1, show_col_types = FALSE)$Primary) +
    dplyr::last(sheet_data$Followers$instagram)

  tt <- dplyr::last(readr::read_csv(src_tt, show_col_types = FALSE)$Followers)

  x <- dplyr::last(readr::read_csv(src_x, show_col_types = FALSE)$Followers)

  # move files after processing
  dest_dir <- here::here("data", "monthly_processed", monthyear)
  fs::dir_create(dest_dir)

  fs::file_move(
    src_fb,
    fs::path(dest_dir, fs::path_file(src_fb))
  )
  fs::file_move(
    src_ig,
    fs::path(dest_dir, fs::path_file(src_ig))
  )
  fs::file_move(
    src_tt,
    fs::path(dest_dir, fs::path_file(src_tt))
  )
  fs::file_move(
    src_x,
    fs::path(dest_dir, fs::path_file(src_x))
  )

  rbind(
    dplyr::select(sheet_data$Followers, month:x),
    tibble::tibble(
      month = current_month,
      facebook = fb,
      instagram = ig,
      tiktok = tt,
      x = x
    )
  ) |>
    dplyr::mutate(
      total = facebook + instagram + tiktok + x,
      diff_total = total - dplyr::lag(total),
      mom_total = round((total - dplyr::lag(total)) / dplyr::lag(total), 5)
    )
}


monthly_performance <- function(monthyear) {
  platforms <- c("fb", "ig", "tiktok", "x")

  current_month <- as.Date(paste0(monthyear, "-01"), format = "%b%y-%d")

  # FILE LOCATIONS ---------------------------------------------------------
  src_fb <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_posts_{monthyear}.csv")
  )
  src_igp <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_posts_{monthyear}.csv")
  )
  src_igs <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_stories_{monthyear}.csv")
  )
  src_tt <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_posts_{monthyear}.csv")
  )
  src_x <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_posts_{monthyear}.csv")
  )

  # OVERALL PERFORMANCE PER MONTH ------------------------------------------

  ## Facebook
  fb_base <- readr::read_csv(src_fb, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(parse_date_time(publish_time, orders = c("mdy HM", "mdy")))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month")
    )

  perf_fb <- fb_base |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(reactions_comments_and_shares),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n(),
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "Facebook")

  ## IG Posts
  igp_base <- readr::read_csv(src_igp, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(parse_date_time(
        publish_time,
        orders = c("mdy HM", "mdy")
      )),
      boosted = tidyr::replace_na(boosted, 0)
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + shares + comments + saves,
    )

  # IG Posts - Overall
  perf_igp <- igp_base |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Overall")

  # IG Posts - Organic
  perf_igp_organic <- igp_base |>
    dplyr::filter(
      account_username == "realzaxbys",
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Organic")

  # IG Posts - Boosted
  perf_igp_boosted <- igp_base |>
    dplyr::filter(
      boosted == 1
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Boosted")

  # IG Posts - Co-Posted
  perf_igp_copost <- igp_base |>
    dplyr::filter(
      account_username != "realzaxbys",
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Coposted")

  ## IG Stories
  igs_base <- readr::read_csv(src_igs, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(parse_date_time(publish_time, orders = c("mdy HM", "mdy")))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + shares + replies + sticker_taps,
      engagements = tidyr::replace_na(engagements, 0)
    )

  perf_igs <- igs_base |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Stories")

  # TikTok
  tt_base <- readr::read_csv(src_tt, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(parse_date_time(post_time, orders = c("mdy HM", "mdy"))),
      dplyr::across(co_posted:boosted, ~ tidyr::replace_na(., 0))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      views = video_views,
      engagements = likes + comments + shares + add_to_favorites,
    )

  # TikTok - Overall
  perf_tt <- tt_base |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Overall")

  # TikTok - Organic
  perf_tt_organic <- tt_base |>
    dplyr::filter(
      boosted != 1,
      co_posted != 1,
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Organic")

  # TikTok - Boosted
  perf_tt_boosted <- tt_base |>
    dplyr::filter(
      boosted == 1
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Boosted")

  # TikTok - Co-Posted
  perf_tt_copost <- tt_base |>
    dplyr::filter(
      co_posted == 1,
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Coposted")

  # X
  x_base <- readr::read_csv(src_x, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = as.Date(parse_date_time(date, orders = c("mdy HM", "mdy")))
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month"),
      engagements = likes + comments + shares + saves
    )

  perf_x <- x_base |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(views),
      avg_views = round(mean(views), 0),
      er = round(engagements / total_views, 5),
      posts = dplyr::n()
    ) |>
    dplyr::mutate(platform = "X")

  # Bind Overall for In-Feed view
  month_overall <- dplyr::bind_rows(
    perf_fb,
    perf_igp,
    perf_igs,
    perf_tt,
    perf_x
  ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(total_views),
      er = engagements / total_views,
      posts = sum(posts),
      avg_views = round(total_views / posts, 0)
    )

  # FILE CLEANUP -----------------------------------------------------------
  dest_dir <- here::here("data", "monthly_processed", monthyear)
  fs::file_move(
    src_fb,
    fs::path(dest_dir, fs::path_file(src_fb))
  )
  fs::file_move(
    src_igp,
    fs::path(dest_dir, fs::path_file(src_igp))
  )
  fs::file_move(
    src_igs,
    fs::path(dest_dir, fs::path_file(src_igs))
  )
  fs::file_move(
    src_tt,
    fs::path(dest_dir, fs::path_file(src_tt))
  )
  fs::file_move(
    src_x,
    fs::path(dest_dir, fs::path_file(src_x))
  )

  # Post-level data
  bases <- list(
    fb_base = fb_base,
    igp_base = igp_base,
    igs_base = igs_base,
    tt_base = tt_base,
    x_base = x_base
  )

  # bucket posts by thresholds, plot, save to gdrive
  monthly_buckets(bases)

  # OUTPUT (FB, IGS, X are always organic)
  list(
    month_overall = month_overall,
    perf_fb = perf_fb,
    perf_igp = perf_igp,
    perf_igp_organic = perf_igp_organic,
    perf_igp_boosted = perf_igp_boosted,
    perf_igp_copost = perf_igp_copost,
    perf_igs = perf_igs,
    perf_tt = perf_tt,
    perf_tt_organic = perf_tt_organic,
    perf_tt_boosted = perf_tt_boosted,
    perf_tt_copost = perf_tt_copost,
    perf_x = perf_x
  )
}

monthly_buckets <- function(data) {
  # Setup
  options(gargle_oauth_email = "gspanalytics21@gmail.com")
  ga_id <- "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic"
  benchmarks <- readRDS(here::here("data", "bm_data", "benchmarks.rds"))

  # Set up post-level data
  fb <- data$fb_base |>
    dplyr::mutate(
      engagements = reactions_comments_and_shares,
    ) |>
    dplyr::select(month, engagements, views) |>
    dplyr::mutate(er = engagements / views, platform = "Facebook")

  igp <- data$igp_base |>
    dplyr::filter(
      account_username == "realzaxbys",
      boosted != 1
    ) |>
    dplyr::select(month, engagements, views) |>
    dplyr::mutate(er = engagements / views, platform = "IG Post")

  igs <- data$igs_base |>
    dplyr::select(month, engagements, views) |>
    dplyr::mutate(er = engagements / views, platform = "IG Stories")

  tt <- data$tt_base |>
    dplyr::select(month, engagements, views) |>
    dplyr::mutate(er = engagements / views, platform = "TikTok")

  x <- data$x_base |>
    dplyr::select(month, engagements, views) |>
    dplyr::mutate(er = engagements / views, platform = "X")

  # estimate benchmark thresholds
  platform_thresholds <- dplyr::bind_rows(fb, igp, igs, tt, x) |>
    dplyr::left_join(benchmarks, by = "platform") |>
    dplyr::mutate(
      views_below = ifelse(views < views_p_40, 1, 0),
      views_t1 = ifelse(views > views_p_40 & views < views_p_65, 1, 0),
      views_t2 = ifelse(views > views_p_65 & views < views_p_95, 1, 0),
      views_t3 = ifelse(views > views_p_95, 1, 0),
      er_below = ifelse(er < er_p_40, 1, 0),
      er_t1 = ifelse(er > er_p_40 & er < er_p_65, 1, 0),
      er_t2 = ifelse(er > er_p_65 & er < er_p_95, 1, 0),
      er_t3 = ifelse(er > er_p_95, 1, 0)
    ) |>
    dplyr::group_by(month, platform) |>
    dplyr::summarise(dplyr::across(views_below:er_t3, sum)) %>%
    dplyr::mutate(post_total = rowSums(.[, 3:ncol(.)]) / 2) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric) & !c(post_total),
        ~ .x / post_total,
        .names = "{.col}_pct"
      )
    )

  # get data into long form and merge
  bucket_counts <- platform_thresholds |>
    select(month:er_t3) |>
    pivot_longer(cols = -c(month, platform), values_to = "count")

  bucket_pct <- platform_thresholds |>
    select(month, platform, views_below_pct:er_t3_pct) |>
    pivot_longer(cols = -c(month, platform), values_to = "percent") |>
    mutate(name = str_remove_all(name, "_pct"))

  bucket_full <- bucket_pct |>
    left_join(bucket_counts) |>
    mutate(
      name = factor(
        name,
        levels = c(
          "views_below",
          "views_t1",
          "views_t2",
          "views_t3",
          "er_below",
          "er_t1",
          "er_t2",
          "er_t3"
        )
      )
    ) |>
    group_by(platform) |>
    mutate(
      total_post = sum(count) / 2,
      platform = glue::glue("{platform}\n{total_post} posts"),
      percent = ifelse(percent == 0, NA, percent)
    )

  # Plots
  if (
    requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)
  ) {
    sysfonts::font_add_google("Barlow", "Barlow")
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 300)

    base_family <- "Barlow"
  } else {
    base_family <- "sans"
  }
  # source the theme for the plots
  devtools::source_gist(
    "https://gist.github.com/taylorgrant/1a486bccbde092d3b333a496e60049d5"
  )

  view_bucket_plot <- ggplot(
    data = filter(bucket_full, str_detect(name, "^views")),
    aes(
      x = platform,
      y = percent,
      label = scales::percent(percent, accuracy = 1),
      group = name,
      fill = name
    )
  ) +
    ggchicklet::geom_chicklet(width = .8) +
    coord_flip() +
    geom_text(
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = "white"
    ) +
    scale_fill_manual(
      values = c("#b2182b", "#e97451", "#67a9cf", "#2166ac"),
      name = NULL,
      labels = c("Below", "Tier 1", "Tier 2", "Tier 3")
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_zax() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 11, family = base_family),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Posts by Benchmark Tiers - Views",
      caption = glue::glue(
        "For the month of {format(unique(bucket_full$month), '%B %Y')}"
      )
    )

  er_bucket_plot <- ggplot(
    data = filter(bucket_full, str_detect(name, "^er")),
    aes(
      x = platform,
      y = percent,
      label = scales::percent(percent, accuracy = 1),
      group = name,
      fill = name
    )
  ) +
    ggchicklet::geom_chicklet(width = .8) +
    coord_flip() +
    geom_text(
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = "white"
    ) +
    scale_fill_manual(
      values = c("#b2182b", "#e97451", "#67a9cf", "#2166ac"),
      name = NULL,
      labels = c("Below", "Tier 1", "Tier 2", "Tier 3")
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_zax() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 11, family = base_family),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Posts by Benchmark Tiers - Engagement Rate",
      caption = glue::glue(
        "For the month of {format(unique(bucket_full$month), '%B %Y')}"
      )
    )

  # save to figure folder
  ggsave(
    glue::glue(here::here(
      "figures",
      "buckets",
      "view_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png"
    )),
    view_bucket_plot,
    width = 6.4,
    height = 4.21
  )

  ggsave(
    glue::glue(here::here(
      "figures",
      "buckets",
      "er_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png"
    )),
    er_bucket_plot,
    width = 6.4,
    height = 4.21
  )

  # upload to google drive
  folder_id <- "1fCsk4BfyBaNcgIsnYdEtg2P394l9B7NA"
  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "buckets",
      "view_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png"
    )),
    path = googledrive::as_id(folder_id)
  )
  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "buckets",
      "er_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png"
    )),
    path = googledrive::as_id(folder_id)
  )

  # write thresholds to sheets
  googlesheets4::sheet_write(
    platform_thresholds,
    ss = ga_id,
    sheet = "Platform Thresholds"
  )
}
