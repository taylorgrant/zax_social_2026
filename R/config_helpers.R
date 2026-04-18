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
