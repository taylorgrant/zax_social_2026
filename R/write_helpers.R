write_monthly_outputs <- function(ga_id, followers, overall, performance) {
  googlesheets4::sheet_write(overall, ss = ga_id, sheet = "Overall")
  googlesheets4::sheet_write(followers$data, ss = ga_id, sheet = "Followers")

  googlesheets4::sheet_append(
    performance$perf_fb,
    ss = ga_id,
    sheet = "Facebook"
  )

  googlesheets4::sheet_append(
    dplyr::bind_rows(
      performance$perf_igp,
      performance$perf_igp_organic,
      performance$perf_igp_boosted,
      performance$perf_igp_copost
    ),
    ss = ga_id,
    sheet = "IG Post"
  )

  googlesheets4::sheet_append(
    performance$perf_igs,
    ss = ga_id,
    sheet = "IG Stories"
  )

  googlesheets4::sheet_append(
    dplyr::bind_rows(
      performance$perf_tt,
      performance$perf_tt_organic,
      performance$perf_tt_boosted,
      performance$perf_tt_copost
    ),
    ss = ga_id,
    sheet = "TikTok"
  )

  googlesheets4::sheet_append(performance$perf_x, ss = ga_id, sheet = "X")
}
