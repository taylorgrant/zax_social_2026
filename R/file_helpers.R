get_project_structure <- function() {
  list(
    data = list(
      monthly_new = NULL,
      monthly_processed = NULL
    ),
    figures = list(
      boosted = NULL,
      organic = NULL,
      overall = NULL,
      followers = NULL,
      buckets = NULL
    )
  )
}

create_dirs_recursive <- function(base_path, structure) {
  purrr::walk(names(structure), function(name) {
    path <- fs::path(base_path, name)

    fs::dir_create(path)

    if (is.list(structure[[name]])) {
      create_dirs_recursive(path, structure[[name]])
    }
  })
}

setup_project_dirs <- function(root = here::here()) {
  structure <- get_project_structure()
  create_dirs_recursive(root, structure)

  invisible(TRUE)
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
