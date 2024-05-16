pbp_eagles <- load_pbp(seasons = 2016:2020) |>
  filter(posteam == 'PHI')
pbp_jags <- load_pbp(seasons = 2022:2023) |>
  filter(posteam == 'JAX')
all_pbp <- rbind(pbp_eagles, pbp_jags)

pbp_formatted <- all_pbp |>
  filter(play_type == 'pass' | play_type == 'run') |>
  mutate(clutch = ifelse(abs(score_differential) <= 8 & game_seconds_remaining < 300, 1, 0))
pbp_formatted |> group_by(clutch) |> summarise(num = n())

doug_non_clutch <- pbp_formatted |>
  filter(clutch == 0) |>
  group_by(season) |>
  summarise(nc_epa_per_play = sum(epa)/n())
doug_clutch <- pbp_formatted |>
  filter(clutch == 1) |>
  group_by(season) |>
  summarise(c_epa_per_play = sum(epa)/n())

doug_clutch_tbl <- doug_non_clutch |>
  left_join(doug_clutch, by = 'season') |>
  ungroup() |> gt() |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  fmt_number(columns = c(nc_epa_per_play, c_epa_per_play), decimals = 2) |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(nc_epa_per_play = "Non-Clutch EPA/Play", c_epa_per_play = "Clutch EPA/Play"
  ) |> opt_row_striping() |>
  tab_header(title = "Doug Pederson Offenses in Clutch Situations",
             subtitle = "Clutch = 1 score game with less than 5 mins in game") |>
  gt_theme_538()
doug_clutch_tbl
gtsave(doug_clutch_tbl, "Offense/DougClutch.png")
