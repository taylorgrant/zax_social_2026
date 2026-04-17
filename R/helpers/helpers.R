get_config <- function() {
  list(
    ga_id = "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic",
    drive_folders = list(
      boosted = "1AS5XMk9EF89nKSKd-n6lMY1dA8f7MLKk",
      buckets = "1fCsk4BfyBaNcgIsnYdEtg2P394l9B7NA",
      followers = "1h3OheIsk-hHxFRmKmGvOM9uWp07GJSCB",
      organic = "1ZgmTESuDO8AW6ilaVZgrztmiyI1j665r",
      overall = "1a0vqb_5Wj03BamAPC53d65ww-RmH1z3E"
    )
  )
}

follower_file_paths <- function(monthyear) {
  platforms <- c("fb", "ig", "tiktok", "x")
  fb_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_followers_{monthyear}.csv")
  )
  ig_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_followers_{monthyear}.csv")
  )
  tt_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_followers_{monthyear}.csv")
  )
  x_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_followers_{monthyear}.csv")
  )
  files <- c(
    fb = fb_path,
    ig = ig_path,
    tt = tt_path,
    x = x_path
  )
}

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

# Pull in monthly followers from data
monthly_followers <- function(sheet_data, monthyear) {
  files <- follower_file_paths(monthyear)
  raw <- read_follower_files(files, sheet_data)
  out <- build_follower_table(raw, sheet_data, monthyear)

  list(
    data = out,
    files = files
  )
}

archive_monthly_files <- function(files, monthyear) {
  dest_dir <- here::here("data", "monthly_processed", monthyear)
  fs::dir_create(dest_dir)
  purrr::walk(files, ~ fs::file_move(.x, fs::path(dest_dir, fs::path_file(.x))))
}

performance_file_paths <- function(monthyear) {
  platforms <- c("fb", "ig", "tiktok", "x")
  fb_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_posts_{monthyear}.csv")
  )
  igp_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_posts_{monthyear}.csv")
  )
  igs_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_stories_{monthyear}.csv")
  )
  tt_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_posts_{monthyear}.csv")
  )
  x_path <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_posts_{monthyear}.csv")
  )
  files <- c(
    fb = fb_path,
    igp = igp_path,
    igs = igs_path,
    tt = tt_path,
    x = x_path
  )
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

# plot theme
theme_zax <- function(
  base_family = "Barlow",
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_size = 18,
  plot_title_face = "plain",
  plot_title_margin = 4,
  subtitle_family = "Barlow",
  subtitle_size = 12,
  subtitle_face = "plain",
  subtitle_margin = 15,
  strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  caption_family = "Barlow",
  caption_size = 9,
  caption_face = "italic",
  caption_margin = 10,
  axis_title_family = base_family,
  axis_title_size = 13,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = ggplot2::margin(10, 10, 10, 10),
  panel_spacing = ggplot2::unit(0.5, "lines"),
  grid_col = "#cccccc",
  grid = TRUE,
  axis_col = "#cccccc",
  axis = FALSE,
  ticks = FALSE
) {
  ret <- ggplot2::theme_minimal(
    base_family = base_family,
    base_size = base_size
  )

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret +
      ggplot2::theme(
        panel.grid = ggplot2::element_line(color = grid_col, linewidth = 0.10)
      )
    ret <- ret +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          color = grid_col,
          linewidth = 0.1
        )
      )
    ret <- ret +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_line(
          color = grid_col,
          linewidth = 0.1
        )
      )

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) {
        ret <- ret +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      }
      if (regexpr("Y", grid)[1] < 0) {
        ret <- ret +
          ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      }
      if (regexpr("x", grid)[1] < 0) {
        ret <- ret +
          ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      }
      if (regexpr("y", grid)[1] < 0) {
        ret <- ret +
          ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
      }
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = "#2b2b2b", linewidth = 0.15)
      )
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(
              color = axis_col,
              linewidth = 0.15
            )
          )
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret +
          ggplot2::theme(
            axis.line.y = ggplot2::element_line(
              color = axis_col,
              linewidth = 0.15
            )
          )
      }
    } else {
      ret <- ret +
        ggplot2::theme(
          axis.line.x = ggplot2::element_line(
            color = axis_col,
            linewidth = 0.15
          )
        )
      ret <- ret +
        ggplot2::theme(
          axis.line.y = ggplot2::element_line(
            color = axis_col,
            linewidth = 0.15
          )
        )
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret +
      ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth = 0.15))
    ret <- ret +
      ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth = 0.15))
    ret <- ret +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(
    tolower(substr(axis_title_just, 1, 1)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )
  yj <- switch(
    tolower(substr(axis_title_just, 2, 2)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )

  ret <- ret +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0))
    )
  ret <- ret +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0))
    )
  ret <- ret +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = axis_title_size,
        family = axis_title_family
      )
    )
  ret <- ret +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        hjust = xj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  ret <- ret +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        hjust = yj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  ret <- ret +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        hjust = 0,
        size = strip_text_size,
        face = strip_text_face,
        family = strip_text_family
      )
    )
  ret <- ret + ggplot2::theme(panel.spacing.x = grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(panel.spacing.y = grid::unit(.5, "lines"))
  ret <- ret +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = plot_title_size,
        margin = ggplot2::margin(b = plot_title_margin),
        family = plot_title_family,
        face = plot_title_face
      )
    )
  ret <- ret +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = subtitle_size,
        margin = ggplot2::margin(b = subtitle_margin),
        family = subtitle_family,
        face = subtitle_face
      )
    )
  ret <- ret +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = caption_size,
        margin = ggplot2::margin(t = caption_margin),
        family = caption_family,
        face = caption_face
      )
    )
  ret <- ret + ggplot2::theme(plot.margin = plot_margin)

  ret <- ret + ggplot2::theme(panel.spacing = panel_spacing)

  ret
}

# fonts
setup_fonts <- function() {
  if (
    requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)
  ) {
    sysfonts::font_add_google("Barlow", "Barlow")
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 300)

    return("Barlow")
  }

  return("sans")
}
