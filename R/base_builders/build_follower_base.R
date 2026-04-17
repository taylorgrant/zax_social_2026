# Build follower base
fb_base <- tibble(month = as.Date("2025-06-01"), followers = 1275683)
fb_follow <- readr::read_csv(
  "~/Desktop/zaxbys/FB - Zax Data/fb_followers_jan25_dec25.csv",
  skip = 1
) |>
  clean_names() |>
  mutate(date = as.Date(date), month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(followers = sum(primary)) |>
  filter(month > "2025-06-01", month < "2025-12-01") |>
  bind_rows(fb_base) |>
  arrange(month) |>
  mutate(cumfollow = cumsum(followers)) |>
  select(month, facebook = cumfollow)

ig_base <- tibble(month = as.Date("2025-06-01"), followers = 170777)
ig_follow <- readr::read_csv(
  "~/Desktop/zaxbys/IG Posts - Zax Data/ig_followers_mar25_dec25.csv",
  skip = 1
) |>
  clean_names() |>
  mutate(date = as.Date(date), month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(followers = sum(primary)) |>
  filter(month > "2025-06-01", month < "2025-12-01") |>
  bind_rows(ig_base) |>
  arrange(month) |>
  mutate(cumfollow = cumsum(followers)) |>
  select(month, instagram = cumfollow)


tt_follow <- readr::read_csv(
  "~/Desktop/zaxbys/TikTok - Zax Data/tiktok_followers_jun_nov25.csv"
) |>
  clean_names() |>
  mutate(
    date = as.Date(paste0(date, "-2025"), format = "%d-%b-%Y"),
    month = floor_date(date, "month")
  ) |>
  group_by(month) |>
  slice_max(order_by = date, n = 1) |>
  select(month, tiktok = followers)

x_base <- tibble(month = as.Date("2025-12-01"), followers = 95658)
x_follow <- readr::read_csv(
  "~/Desktop/zaxbys/X - Zax Data/x_followers_jan25_dec25.csv"
) |>
  clean_names() |>
  mutate(date = as.Date(paste0(month, "-01"), format = "%b-%y-%d")) |>
  filter(date >= "2025-06-01", date < "2025-12-01") |>
  select(month = date, x = followers)

followers <- fb_follow |>
  left_join(ig_follow) |>
  left_join(tt_follow) |>
  left_join(x_follow) |>
  mutate(
    total = facebook + instagram + tiktok + x,
    diff_total = total - dplyr::lag(total),
    mom_total = (total - dplyr::lag(total)) / dplyr::lag(total)
  )

library(googledrive)
options(gargle_oauth_email = "gspanalytics21@gmail.com")
ga_id <- "1dFJWftQk2-4iPeK-Oo6qoglJryKGJxIGL_q-c4qQ5ic"
googlesheets4::sheet_write(followers, ss = ga_id, sheet = "Followers")
