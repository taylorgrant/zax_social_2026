boosted_plot <- function(data, platform, base_family = "sans") {
  tmpdat <- if (platform == "IG Post") {
    data[[platform]][
      data[[platform]]$platform == "IG Post Boosted",
    ]
  } else if (platform == "TikTok") {
    data[[platform]][
      data[[platform]]$platform == "TikTok Boosted",
    ]
  }

  # Setting up a filter so that only 13 months are kept in plots
  max_month <- max(tmpdat$month, na.rm = TRUE)
  data_filtered <- tmpdat |>
    dplyr::filter(month >= (max_month %m-% months(12)))

  # function to format a number
  format_number <- function(x) {
    dplyr::case_when(
      abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"), # Billions
      abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"), # Millions
      abs(x) >= 1e3 ~ paste0(round(x / 1e3, 1), "k"), # Thousands
      TRUE ~ as.character(x) # No truncation
    )
  }

  # avg views plot
  p1 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = month, y = avg_views)) +
    ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_number(avg_views)),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_date(
      breaks = seq(
        min(data_filtered$month),
        max(data_filtered$month),
        by = "1 month"
      ),
      # date_breaks = "1 month",
      date_labels = "%b\n%y",
      # limits = c(x_min, x_max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    theme_zax(grid = FALSE, ticks = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{platform} - Boosted Posts"),
      subtitle = "Avg. Views",
      caption = "Bars capture monthly avg views for boosted posts"
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  # avg ER plot
  p2 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = month, y = er)) +
    ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(er, accuracy = .1)),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_date(
      breaks = seq(
        min(data_filtered$month),
        max(data_filtered$month),
        by = "1 month"
      ),
      # date_breaks = "1 month",
      date_labels = "%b\n%y",
      # limits = c(x_min, x_max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    theme_zax(grid = FALSE, ticks = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{platform} - Boosted Posts"),
      subtitle = " Avg. Engagement Rate",
      caption = "Bars capture monthly engagement rate for boosted posts"
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  # save to local folders
  ggplot2::ggsave(
    plot = p1,
    filename = glue::glue(here::here(
      "figures",
      "boosted",
      "{format(last(tmpdat$month), '%b-%Y')}-{platform}-BOOSTED-VIEWS.png"
    )),
    width = 6,
    height = 5
  )

  ggplot2::ggsave(
    plot = p2,
    filename = glue::glue(here::here(
      "figures",
      "boosted",
      "{format(last(tmpdat$month), '%b-%Y')}-{platform}-BOOSTED-ER.png"
    )),
    width = 6,
    height = 5
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "boosted",
      "{format(last(tmpdat$month), '%b-%Y')}-{platform}-BOOSTED-VIEWS.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$boosted),
    overwrite = TRUE
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "boosted",
      "{format(last(tmpdat$month), '%b-%Y')}-{platform}-BOOSTED-ER.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$boosted),
    overwrite = TRUE
  )
}


follower_plot <- function(data, base_family = "sans") {
  follower_data <- data$Followers |>
    tidyr::pivot_longer(cols = facebook:x) |>
    mutate(
      name = case_when(
        name == "facebook" ~ "Facebook",
        name == "instagram" ~ "Instagram",
        name == "tiktok" ~ "TikTok",
        name == "x" ~ "X"
      )
    )

  # filter down the number of months in each plot
  max_month <- max(follower_data$month, na.rm = TRUE)
  data_filtered <- follower_data |>
    dplyr::filter(month >= (max_month %m-% months(12)))

  make_plot <- function(data, platform) {
    # function to format a number
    format_number <- function(x) {
      dplyr::case_when(
        abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"), # Billions
        abs(x) >= 1e6 ~ paste0(round(x / 1e6, 2), "M"), # Millions
        abs(x) >= 1e3 ~ paste0(round(x / 1e3, 2), "k"), # Thousands
        TRUE ~ as.character(x) # No truncation
      )
    }

    p1 <- ggplot2::ggplot(
      dplyr::filter(data, name == platform),
      ggplot2::aes(x = month, y = value)
    ) +
      ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
      ggplot2::geom_text(
        ggplot2::aes(label = format_number(value)),
        vjust = -.2,
        hjust = .5,
        na.rm = TRUE,
        size = 4,
        family = base_family,
        color = "black",
        angle = 0
      ) +
      ggplot2::scale_x_date(
        breaks = seq(min(data$month), max(data$month), by = "1 month"),
        # date_breaks = "1 month",
        date_labels = "%b\n%y",
        # limits = c(x_min, x_max),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.2))
      ) +
      theme_zax(grid = FALSE, ticks = TRUE) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = glue::glue("{platform} - Total Followers"),
        caption = "Bars capture month-end follower counts"
      ) +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.background = ggplot2::element_rect(
          fill = "transparent",
          color = NA
        ),
        panel.background = ggplot2::element_rect(
          fill = "transparent",
          color = NA
        ),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )

    # save to local folders
    ggplot2::ggsave(
      plot = p1,
      filename = glue::glue(here::here(
        "figures",
        "followers",
        "{format(last(data_filtered$month), '%b-%Y')}-{platform}-FOLLOWERS.png"
      )),
      width = 6,
      height = 5
    )

    googledrive::drive_upload(
      glue::glue(here::here(
        "figures",
        "followers",
        "{format(last(data_filtered$month), '%b-%Y')}-{platform}-FOLLOWERS.png"
      )),
      path = googledrive::as_id(get_config()$drive_folders$followers),
      overwrite = TRUE
    )
  }

  make_plot(data_filtered, "Facebook")
  make_plot(data_filtered, "Instagram")
  make_plot(data_filtered, "TikTok")
  make_plot(data_filtered, "X")
}

organic_plot <- function(data, platform, base_family = "sans") {
  # pull in benchmarks
  benchmarks <- readRDS(here::here("data", "bm_data", "benchmarks.rds"))

  # function to format a number
  format_number <- function(x) {
    dplyr::case_when(
      abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"), # Billions
      abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"), # Millions
      abs(x) >= 1e3 ~ paste0(round(x / 1e3, 1), "k"), # Thousands
      TRUE ~ as.character(x) # No truncation
    )
  }

  # identify the data
  tmpdat <- if (platform == "IG Post") {
    data[[platform]][
      data[[platform]]$platform == "IG Post Organic",
    ]
  } else if (platform == "TikTok") {
    data[[platform]][
      data[[platform]]$platform == "TikTok Organic",
    ]
  } else {
    data[[platform]]
  }

  # filter down the number of months in each plot
  max_month <- max(tmpdat$month, na.rm = TRUE)
  data_filtered <- tmpdat |>
    dplyr::filter(month >= (max_month %m-% months(12)))

  # plot 1 - average views per post
  i_label <- paste0(
    format_number(data_filtered$avg_views),
    "\n",
    ifelse(
      data_filtered$avg_views >
        benchmarks[benchmarks$platform == platform, ]$views_p_50,
      paste0(
        "(+",
        scales::percent(
          (data_filtered$avg_views /
            benchmarks[benchmarks$platform == platform, ]$views_p_50) -
            1,
          accuracy = 1
        ),
        ")"
      ),
      paste0(
        "(",
        scales::percent(
          (data_filtered$avg_views /
            benchmarks[benchmarks$platform == platform, ]$views_p_50) -
            1,
          accuracy = 1
        ),
        ")"
      )
    )
  )
  i_labels <- ifelse(seq_along(i_label) == length(i_label), i_label, NA)
  p1 <- ggplot2::ggplot(
    data_filtered,
    ggplot2::aes(x = month, y = avg_views)
  ) +
    ggchicklet::geom_chicklet(fill = "#4284f3") +
    ggplot2::geom_text(
      ggplot2::aes(label = i_labels),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_datetime(date_breaks = "months", date_labels = "%b %y") +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::geom_hline(
      yintercept = benchmarks[benchmarks$platform == platform, ]$views_p_50,
      # yintercept = dplyr::filter(benchmarks, platform == platform)$views_p_50,
      linetype = "dashed",
      color = '#E4012A'
    ) +
    theme_zax(grid = "Y") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::labs(
      x = "Month",
      y = NULL,
      title = "Avg. Views",
      caption = glue::glue(
        "Benchmark for {platform}: {scales::comma(benchmarks[benchmarks$platform == platform, ]$views_p_50)}"
      )
    )

  # plot 2 - average engagement rate per post
  e_label <- paste0(
    scales::percent(data_filtered$er, accuracy = .01),
    "\n",
    ifelse(
      data_filtered$er > benchmarks[benchmarks$platform == platform, ]$er_p_50,
      paste0(
        "(+",
        scales::percent(
          (data_filtered$er /
            benchmarks[benchmarks$platform == platform, ]$er_p_50) -
            1,
          accuracy = 1
        ),
        ")"
      ),
      paste0(
        "(",
        scales::percent(
          (data_filtered$er /
            benchmarks[benchmarks$platform == platform, ]$er_p_50) -
            1,
          accuracy = 1
        ),
        ")"
      )
    )
  )
  e_labels <- ifelse(seq_along(e_label) == length(e_label), e_label, NA)
  p2 <- ggplot2::ggplot(
    data_filtered,
    ggplot2::aes(x = month, y = er)
  ) +
    ggchicklet::geom_chicklet(fill = "#4284f3") +
    ggplot2::geom_text(
      ggplot2::aes(label = e_labels),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_datetime(date_breaks = "months", date_labels = "%b %y") +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::geom_hline(
      # yintercept = filter(benchmarks$bm_er, network == platform)$bm_er,
      yintercept = benchmarks[benchmarks$platform == platform, ]$er_p_50,
      linetype = "dashed",
      color = '#E4012A'
    ) +
    theme_zax(grid = "Y") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::labs(
      x = "Month",
      y = NULL,
      title = "Engagement Rate",
      caption = glue::glue(
        "Benchmark for {platform}: {scales::percent(benchmarks[benchmarks$platform == platform, ]$er_p_50, accuracy = .1)}"
      )
    )

  # save to local folders
  ggplot2::ggsave(
    plot = p1,
    filename = glue::glue(here::here(
      "figures",
      "organic",
      "{format(last(data[[platform]]$month), '%b-%Y')}-{platform}-VIEWS.png"
    )),
    width = 6,
    height = 5
  )

  ggplot2::ggsave(
    plot = p2,
    filename = glue::glue(here::here(
      "figures",
      "organic",
      "{format(last(data[[platform]]$month), '%b-%Y')}-{platform}-ER.png"
    )),
    width = 6,
    height = 5
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "organic",
      "{format(last(data[[platform]]$month), '%b-%Y')}-{platform}-VIEWS.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$organic),
    overwrite = TRUE
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "organic",
      "{format(last(data[[platform]]$month), '%b-%Y')}-{platform}-ER.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$organic),
    overwrite = TRUE
  )
}

overall_plot <- function(data, platform, base_family = "sans") {
  tmpdat <- if (platform == "IG Post") {
    data[[platform]][
      data[[platform]]$platform == "IG Post Overall",
    ]
  } else if (platform == "TikTok") {
    data[[platform]][
      data[[platform]]$platform == "TikTok Overall",
    ]
  } else {
    data[[platform]]
  }

  # function to format a number
  format_number <- function(x) {
    dplyr::case_when(
      abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"), # Billions
      abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"), # Millions
      abs(x) >= 1e3 ~ paste0(round(x / 1e3, 1), "k"), # Thousands
      TRUE ~ as.character(x) # No truncation
    )
  }

  # filter down the number of months in each plot
  max_month <- max(tmpdat$month, na.rm = TRUE)
  data_filtered <- tmpdat |>
    dplyr::filter(month >= (max_month %m-% months(12)))

  # avg views plot
  p1 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = month, y = avg_views)) +
    ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_number(avg_views)),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_date(
      breaks = seq(
        min(data_filtered$month),
        max(data_filtered$month),
        by = "1 month"
      ),
      # date_breaks = "1 month",
      date_labels = "%b\n%y",
      # limits = c(x_min, x_max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    theme_zax(grid = FALSE, ticks = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{platform} - In-Feed Posts"),
      subtitle = "Avg. Views",
      caption = "Bars capture monthly avg views for all in-feed posts"
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  # avg ER plot
  p2 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = month, y = er)) +
    ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(er, accuracy = .1)),
      vjust = -.2,
      hjust = .5,
      na.rm = TRUE,
      size = 4,
      family = base_family,
      color = "black",
      angle = 0
    ) +
    ggplot2::scale_x_date(
      breaks = seq(
        min(data_filtered$month),
        max(data_filtered$month),
        by = "1 month"
      ),
      # date_breaks = "1 month",
      date_labels = "%b\n%y",
      # limits = c(x_min, x_max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    theme_zax(grid = FALSE, ticks = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{platform} - In-Feed Posts"),
      subtitle = "Avg. Engagement Rate",
      caption = "Bars capture monthly engagement rate for all in-feed posts"
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  # save to local folders
  ggplot2::ggsave(
    plot = p1,
    filename = glue::glue(here::here(
      "figures",
      "overall",
      "{format(last(data_filtered$month), '%b-%Y')}-{platform}-OVERALL-VIEWS.png"
    )),
    width = 6,
    height = 5
  )

  ggplot2::ggsave(
    plot = p2,
    filename = glue::glue(here::here(
      "figures",
      "overall",
      "{format(last(data_filtered$month), '%b-%Y')}-{platform}-OVERALL-ER.png"
    )),
    width = 6,
    height = 5
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "overall",
      "{format(last(data_filtered$month), '%b-%Y')}-{platform}-OVERALL-VIEWS.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$overall),
    overwrite = TRUE
  )

  googledrive::drive_upload(
    glue::glue(here::here(
      "figures",
      "overall",
      "{format(last(data_filtered$month), '%b-%Y')}-{platform}-OVERALL-ER.png"
    )),
    path = googledrive::as_id(get_config()$drive_folders$overall),
    overwrite = TRUE
  )
}
