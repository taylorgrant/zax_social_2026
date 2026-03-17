# Pull in monthly followers from data
monthly_followers <- function(sheet_data, month) {
  platforms <- c("fb", "ig", "tiktok", "x")

  current_month <- as.Date(paste0(month, "-01"), format = "%b%y-%d")

  src_fb <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[1]}_followers_{month}.csv")
  )
  src_ig <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[2]}_followers_{month}.csv")
  )
  src_tt <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[3]}_followers_{month}.csv")
  )
  src_x <- here::here(
    "data",
    "monthly_new",
    glue::glue("{platforms[4]}_followers_{month}.csv")
  )

  fb <- sum(readr::read_csv(src_fb, skip = 1, show_col_types = FALSE)$Primary) +
    dplyr::last(sheet_data$Followers$facebook)

  ig <- sum(readr::read_csv(src_ig, skip = 1, show_col_types = FALSE)$Primary) +
    dplyr::last(sheet_data$Followers$instagram)

  tt <- dplyr::last(readr::read_csv(src_tt, show_col_types = FALSE)$Followers)

  x <- dplyr::last(readr::read_csv(src_x, show_col_types = FALSE)$Followers)

  # move files after processing
  fs::file_move(
    src_fb,
    here::here("data", "monthly_processed", fs::path_file(src_fb))
  )
  fs::file_move(
    src_ig,
    here::here("data", "monthly_processed", fs::path_file(src_ig))
  )
  fs::file_move(
    src_tt,
    here::here("data", "monthly_processed", fs::path_file(src_tt))
  )
  fs::file_move(
    src_x,
    here::here("data", "monthly_processed", fs::path_file(src_x))
  )

  tibble::tibble(
    month = current_month,
    facebook = fb,
    instagram = ig,
    tiktok = tt,
    x = x
  )
}
