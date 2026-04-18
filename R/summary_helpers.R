build_follower_table <- function(data, sheet_data, monthyear) {
  current_month <- as.Date(paste0(monthyear, "-01"), format = "%b%y-%d")

  out <- rbind(
    dplyr::select(sheet_data$Followers, month:x),
    tibble::tibble(
      month = current_month,
      facebook = data["fb"],
      instagram = data["ig"],
      tiktok = data["tt"],
      x = data["x"]
    )
  ) |>
    dplyr::mutate(
      total = facebook + instagram + tiktok + x,
      diff_total = total - dplyr::lag(total),
      mom_total = round((total - dplyr::lag(total)) / dplyr::lag(total), 5)
    )
}

monthly_followers <- function(sheet_data, monthyear) {
  files <- follower_file_paths(monthyear)
  raw <- read_follower_files(files, sheet_data)
  out <- build_follower_table(raw, sheet_data, monthyear)

  list(
    data = out,
    files = files
  )
}

summarise_performance <- function(tbl) {
  tbl |>
    dplyr::summarise(
      engagements = sum(engagements, na.rm = TRUE),
      total_views = sum(views, na.rm = TRUE),
      avg_views = round(mean(views, na.rm = TRUE), 0),
      er = dplyr::if_else(
        total_views > 0,
        round(engagements / total_views, 5),
        NA_real_
      ),
      posts = dplyr::n()
    )
}

summarise_performance_files <- function(rawdata, monthyear) {
  current_month <- as.Date(paste0(monthyear, "-01"), format = "%b%y-%d")
  perf_fb <- rawdata$base$fb |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "Facebook")

  perf_igp <- rawdata$base$igp |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Overall")

  perf_igp_organic <- rawdata$base$igp |>
    dplyr::filter(
      account_username == "realzaxbys",
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Organic")

  perf_igp_boosted <- rawdata$base$igp |>
    dplyr::filter(
      boosted == 1
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Boosted")

  perf_igp_copost <- rawdata$base$igp |>
    dplyr::filter(
      account_username != "realzaxbys",
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Post Coposted")

  perf_igs <- rawdata$base$igs |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "IG Stories")

  perf_tt <- rawdata$base$tt |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Overall")

  perf_tt_organic <- rawdata$base$tt |>
    dplyr::filter(
      boosted != 1,
      co_posted != 1,
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Organic")

  perf_tt_boosted <- rawdata$base$tt |>
    dplyr::filter(
      boosted == 1
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Boosted")

  # TikTok - Co-Posted
  perf_tt_copost <- rawdata$base$tt |>
    dplyr::filter(
      co_posted == 1,
      boosted != 1
    ) |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "TikTok Coposted")

  perf_x <- rawdata$base$x |>
    dplyr::group_by(month) |>
    summarise_performance() |>
    dplyr::filter(month == current_month) |>
    dplyr::mutate(platform = "X")

  list(
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

build_month_overall <- function(data) {
  dplyr::bind_rows(
    data$perf_fb,
    data$perf_igp,
    data$perf_igs,
    data$perf_tt,
    data$perf_x
  ) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      engagements = sum(engagements),
      total_views = sum(total_views),
      er = engagements / total_views,
      posts = sum(posts),
      avg_views = round(total_views / posts, 0)
    )
}

performance_v_benchmark <- function(rawdata) {
  bases <- list(
    fb_base = rawdata$base$fb,
    igp_base = rawdata$base$igp,
    igs_base = rawdata$base$igs,
    tt_base = rawdata$base$tt,
    x_base = rawdata$base$x
  )
  monthly_buckets(bases)
}

monthly_performance <- function(monthyear) {
  mpf <- read_monthly_performance_files(monthyear)
  performance <- summarise_performance_files(mpf, monthyear)
  month_overall <- build_month_overall(performance)
  performance_v_benchmark(mpf)
  out <- list(
    month_overall = month_overall,
    perf_fb = performance$perf_fb,
    perf_igp = performance$perf_igp,
    perf_igp_organic = performance$perf_igp_organic,
    perf_igp_boosted = performance$perf_igp_boosted,
    perf_igp_copost = performance$perf_igp_copost,
    perf_igs = performance$perf_igs,
    perf_tt = performance$perf_tt,
    perf_tt_boosted = performance$perf_tt_boosted,
    perf_tt_copost = performance$perf_tt_copost,
    perf_tt_organic = performance$perf_tt_organic,
    perf_x = performance$perf_x,
    files = mpf$files
  )
  return(out)
}

monthly_performance <- function(monthyear) {
  mpf <- read_monthly_performance_files(monthyear)
  performance <- summarise_performance_files(mpf, monthyear)
  month_overall <- build_month_overall(performance)
  performance_v_benchmark(mpf)
  out <- list(
    month_overall = month_overall,
    perf_fb = performance$perf_fb,
    perf_igp = performance$perf_igp,
    perf_igp_organic = performance$perf_igp_organic,
    perf_igp_boosted = performance$perf_igp_boosted,
    perf_igp_copost = performance$perf_igp_copost,
    perf_igs = performance$perf_igs,
    perf_tt = performance$perf_tt,
    perf_tt_boosted = performance$perf_tt_boosted,
    perf_tt_copost = performance$perf_tt_copost,
    perf_tt_organic = performance$perf_tt_organic,
    perf_x = performance$perf_x,
    files = mpf$files
  )
  return(out)
}

monthly_buckets <- function(data) {
  # Setup
  ga_id <- get_config()$ga_id
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
  # call font
  base_family <- setup_fonts()

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
      "{format(unique(bucket_full$month), '%b-%Y')}-view_bucket.png"
    )),
    view_bucket_plot,
    width = 6.4,
    height = 4.21
  )

  ggsave(
    glue::glue(here::here(
      "figures",
      "buckets",
      "{format(unique(bucket_full$month), '%b-%Y')}-er_bucket.png"
    )),
    er_bucket_plot,
    width = 6.4,
    height = 4.21
  )

  # upload to google drive
  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "buckets",
      "{format(unique(bucket_full$month), '%b-%Y')}-view_bucket.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$buckets),
    overwrite = TRUE
  )
  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "buckets",
      "{format(unique(bucket_full$month), '%b-%Y')}-er_bucket.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$buckets),
    overwrite = TRUE
  )

  # write thresholds to sheets
  googlesheets4::sheet_write(
    platform_thresholds,
    ss = ga_id,
    sheet = "Platform Thresholds"
  )
}
