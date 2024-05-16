# Kickoff spot: 35 yardline, safety kickoffs: 20 yardline, kickoff players: ret 40, rec players: 7+ on 35, 2+ floaters between 35 and 30,
#    max 2 returners in landing zone
# Landing Zone: 20 to goal-line
# Land short of landing zone: 40 yard line
# Land in Landing Zone: Must be returned (even if bounces into end zone)
# Lands in end zone, stays in bound (returned or downed), then returned or touchback to 30
# Back of endzone (in air or bounces), touchback to 30
# No fair catches
# Landing Zone and alignments (besides kicker) don't change if penalty

### Data Notes:
# Kickoff_in_endzone = in end zone & returned, not a touchback
# Kickoff_inside_twenty = in 20 or end zone & a return
# If lands on 0, means a it is a legal kick (I think), but can I differentiate b/w 0 and touchback?
# Touchback = out of end zone or end zone but not touched?
# Don't think I can get difference between kick out of end zone and a touchback in end zone
# defteam is kick team

teams <- load_teams(current = TRUE) |> select(team_abbr) |> rename(team = team_abbr)
team_info <- load_teams(current = TRUE) |> select(team_abbr, team_logo_espn) |>
  rbind(list('NFL Average', 'https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nfl.png&w=64&h=64&scale=crop&cquality=40&location=origin'))

rank_palette <- colorRampPalette(c("green", "yellow", "red"))(32)
rank_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 32) color_index <- 32
  rank_palette[color_index]}

kickoffs_2023 <- load_pbp(2023) |>
  filter(kickoff_attempt == 1 & play_type == "kickoff")

kickoffs_2023_w_stats <- kickoffs_2023 |> filter(grepl('onside', desc) == FALSE) |>
  mutate(land_yardline = 100 - (yardline_100 + kick_distance),
         landing_zone = ifelse(land_yardline > 0 & land_yardline <= 20, 1, 0),
         outside_20 = ifelse(land_yardline > 20 & kickoff_out_of_bounds == 0, 1, 0),
         illegal_kick = ifelse(outside_20 == 1 | kickoff_out_of_bounds == 1, 1, 0),
         return = ifelse(!is.na(kickoff_returner_player_id), 1, 0),
         endzone_land = ifelse(land_yardline < 0, 1, 0),
         drive_start_100 = ifelse(touchback == 1, 30,
                                  ifelse(illegal_kick == 1, 40,
                                         ifelse(return == 1, land_yardline + return_yards, 0))),
         drive_is_td = ifelse(fixed_drive_result == 'Touchdown', 1, 0))

kickoffs_2023_standard <- kickoffs_2023_w_stats |>
  filter(half_seconds_remaining > 120 & yardline_100 == 35 & grepl('onside', desc) == FALSE)

kickoffs_2023_crunchtime <- kickoffs_2023_w_stats |>
  filter(half_seconds_remaining < 120 & yardline_100 == 35 & grepl('onside', desc) == FALSE)

### Kickoff Kicking Stats
kick_lz <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(defteam, landing_zone) |> summarise(num = n()) |> mutate(lz_rate = round(num/sum(num),3)) |>
  filter(landing_zone == 1) |> ungroup()
kick_lz <- teams |> left_join(kick_lz, by = c('team' = 'defteam')) |>
  mutate(lz_rate = ifelse(is.na(lz_rate), 0, lz_rate), lz_rate_rank = rank(-lz_rate, ties.method = "min")) |>
  select(team, lz_rate, lz_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Landing Zone') |>
  rename(stat = team, number = lz_rate, rank = lz_rate_rank)
kick_o20 <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(defteam, outside_20) |> summarise(num = n()) |> mutate(o20_rate = round(num/sum(num),3)) |>
  filter(outside_20 == 1) |> ungroup()
kick_o20 <- teams |> left_join(kick_o20, by = c('team' = 'defteam')) |>
  mutate(o20_rate = ifelse(is.na(o20_rate), 0, o20_rate), o20_rate_rank = rank(o20_rate, ties.method = "min")) |>
  select(team, o20_rate, o20_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Outside 20') |>
  rename(stat = team, number = o20_rate, rank = o20_rate_rank)
kick_ill <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(defteam, illegal_kick) |> summarise(num = n()) |> mutate(ill_rate = round(num/sum(num),3)) |>
  filter(illegal_kick == 1) |> ungroup()
kick_ill <- teams |> left_join(kick_ill, by = c('team' = 'defteam')) |>
  mutate(ill_rate = ifelse(is.na(ill_rate), 0, ill_rate), ill_rate_rank = rank(ill_rate, ties.method = "min")) |>
  select(team, ill_rate, ill_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Illegal Kick') |>
  rename(stat = team, number = ill_rate, rank = ill_rate_rank)
kick_tcbk <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(defteam, touchback) |> summarise(num = n()) |> mutate(tcbk_rate = round(num/sum(num),3)) |>
  filter(touchback == 1) |> ungroup()
kick_tcbk <- teams |> left_join(kick_tcbk, by = c('team' = 'defteam')) |>
  mutate(tcbk_rate = ifelse(is.na(tcbk_rate), 0, tcbk_rate), tcbk_rate_rank = rank(-tcbk_rate, ties.method = "min")) |>
  select(team, tcbk_rate, tcbk_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Touchback') |>
  rename(stat = team, number = tcbk_rate, rank = tcbk_rate_rank)
kick_catch_ydline <- kickoffs_2023_standard |> filter(kickoff_returner_player_id != 'NA' & fumble_lost == 0) |>
  group_by(defteam) |> summarise(num = n(), avg_catch_ydline = mean(land_yardline))
kick_catch_ydline <- teams |> left_join(kick_catch_ydline, by = c('team' = 'defteam')) |>
  mutate(avg_catch_ydline_rank = rank(avg_catch_ydline, ties.method = "min")) |>
  select(team, avg_catch_ydline, avg_catch_ydline_rank) |> filter(team == 'JAX') |> mutate(team = ' Avg Catch Yardline') |>
  rename(stat = team, number = avg_catch_ydline, rank = avg_catch_ydline_rank)
kick_return_rate <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(defteam, return) |> summarise(num = n()) |> mutate(return_rate = round(num/sum(num),3)) |>
  filter(return == 1) |> ungroup()
kick_return_rate <- teams |> left_join(kick_return_rate, by = c('team' = 'defteam')) |>
  mutate(return_rate = ifelse(is.na(return_rate), 0, return_rate), return_rate_rank = rank(return_rate, ties.method = "min")) |>
  select(team, return_rate, return_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Return Rate') |>
  rename(stat = team, number = return_rate, rank = return_rate_rank)
kick_return_yards <- kickoffs_2023_standard |> filter(return == 1 & fumble_lost == 0) |>
  group_by(defteam) |> summarise(num = n(), avg_return_yards = mean(return_yards))
kick_return_yards <- teams |> left_join(kick_return_yards, by = c('team' = 'defteam')) |>
  mutate(avg_return_yards = ifelse(is.na(avg_return_yards), 0, avg_return_yards),
         avg_return_yards_rank = rank(avg_return_yards, ties.method = "min")) |>
  select(team, avg_return_yards, avg_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg Return Yards') |>
  rename(stat = team, number = avg_return_yards, rank = avg_return_yards_rank)
kick_lz_return_yards <- kickoffs_2023_standard |> filter(return == 1 & landing_zone == 1 & fumble_lost == 0) |>
  group_by(defteam) |> summarise(num = n(), avg_lz_return_yards = mean(return_yards))
kick_lz_return_yards <- teams |> left_join(kick_lz_return_yards, by = c('team' = 'defteam')) |>
  mutate(avg_lz_return_yards = ifelse(is.na(avg_lz_return_yards), 0, avg_lz_return_yards),
         avg_lz_return_yards_rank = rank(avg_lz_return_yards, ties.method = "min")) |>
  select(team, avg_lz_return_yards, avg_lz_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg LZ Return Yards') |>
  rename(stat = team, number = avg_lz_return_yards, rank = avg_lz_return_yards_rank)
kick_ez_return_yards <- kickoffs_2023_standard |> filter(return == 1 & land_yardline < 0 & fumble_lost == 0) |>
  group_by(defteam) |> summarise(num = n(), avg_ez_return_yards = mean(return_yards))
kick_ez_return_yards <- teams |> left_join(kick_ez_return_yards, by = c('team' = 'defteam')) |>
  mutate(avg_ez_return_yards = ifelse(is.na(avg_ez_return_yards), 0, avg_ez_return_yards),
         avg_ez_return_yards_rank = rank(avg_ez_return_yards, ties.method = "min")) |>
  select(team, avg_ez_return_yards, avg_ez_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg EZ Return Yards') |>
  rename(stat = team, number = avg_ez_return_yards, rank = avg_ez_return_yards_rank)
kick_drive_start <- kickoffs_2023_standard |>  filter(fumble_lost == 0) |>
  group_by(defteam) |> summarise(num = n(), avg_start_yardline = mean(drive_start_100))
kick_drive_start <- teams |> left_join(kick_drive_start, by = c('team' = 'defteam')) |>
  mutate(avg_start_yardline = ifelse(is.na(avg_start_yardline), 0, avg_start_yardline),
         avg_start_yardline_rank = rank(avg_start_yardline, ties.method = "min")) |>
  select(team, avg_start_yardline, avg_start_yardline_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg Drive Start') |>
  rename(stat = team, number = avg_start_yardline, rank = avg_start_yardline_rank)
### Jags Kickoff Kicking Table
jags_kick_stats <- bind_rows(list(kick_lz, kick_tcbk, kick_o20, kick_ill, kick_return_rate, kick_catch_ydline, kick_return_yards,
                                kick_lz_return_yards, kick_ez_return_yards, kick_drive_start))
rank_numbers <- jags_kick_stats$rank
rank_colors <- sapply(as.numeric(rank_numbers), rank_map_to_color)
jags_kick_stats <- cbind(jags_kick_stats, rank_colors)
jags_kick_tbl <- jags_kick_stats |>
  ungroup() |> gt() |>
  fmt_percent(columns = number, rows = 1:5, decimals = 1) |>
  fmt_number(columns = number, rows = 6:10, decimals = 1) |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  gt_theme_538() |>
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Kickoff Kicking Stats",
             subtitle = "Excludes Onside Kicks, final 2 mins of half, and non-35 yardline kickoffs") |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(stat = "Stat", number = "Number/Rate", rank = "Rank") |>
  tab_style(style = cell_fill(color = from_column(column = "rank_colors")), 
            locations = cells_body(columns = 'rank')) |>
  cols_hide(rank_colors)
jags_kick_tbl
# gtsave(jags_kick_tbl, "Kickoffs/Jags 2024 Kickoff Kicking Tbl.png")
rm(jags_kick_tbl, jags_kick_stats, kick_catch_ydline, kick_drive_start, kick_ez_return_yards, kick_ill, kick_lz, kick_lz_return_yards,
   kick_o20, kick_return_rate, kick_return_yards, kick_tcbk)

### Kickoff Return Stats
ret_lz <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(posteam, landing_zone) |> summarise(num = n()) |> mutate(lz_rate = round(num/sum(num),3)) |>
  filter(landing_zone == 1) |> ungroup()
ret_lz <- teams |> left_join(ret_lz, by = c('team' = 'posteam')) |>
  mutate(lz_rate = ifelse(is.na(lz_rate), 0, lz_rate), lz_rate_rank = rank(lz_rate, ties.method = "min")) |>
  select(team, lz_rate, lz_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Landing Zone') |>
  rename(stat = team, number = lz_rate, rank = lz_rate_rank)
ret_o20 <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(posteam, outside_20) |> summarise(num = n()) |> mutate(o20_rate = round(num/sum(num),3)) |>
  filter(outside_20 == 1) |> ungroup()
ret_o20 <- teams |> left_join(ret_o20, by = c('team' = 'posteam')) |>
  mutate(o20_rate = ifelse(is.na(o20_rate), 0, o20_rate), o20_rate_rank = rank(-o20_rate, ties.method = "min")) |>
  select(team, o20_rate, o20_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Outside 20') |>
  rename(stat = team, number = o20_rate, rank = o20_rate_rank)
ret_ill <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(posteam, illegal_kick) |> summarise(num = n()) |> mutate(ill_rate = round(num/sum(num),3)) |>
  filter(illegal_kick == 1) |> ungroup()
ret_ill <- teams |> left_join(ret_ill, by = c('team' = 'posteam')) |>
  mutate(ill_rate = ifelse(is.na(ill_rate), 0, ill_rate), ill_rate_rank = rank(-ill_rate, ties.method = "min")) |>
  select(team, ill_rate, ill_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Illegal Kick') |>
  rename(stat = team, number = ill_rate, rank = ill_rate_rank)
ret_tcbk <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(posteam, touchback) |> summarise(num = n()) |> mutate(tcbk_rate = round(num/sum(num),3)) |>
  filter(touchback == 1) |> ungroup()
ret_tcbk <- teams |> left_join(ret_tcbk, by = c('team' = 'posteam')) |>
  mutate(tcbk_rate = ifelse(is.na(tcbk_rate), 0, tcbk_rate), tcbk_rate_rank = rank(tcbk_rate, ties.method = "min")) |>
  select(team, tcbk_rate, tcbk_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Touchback') |>
  rename(stat = team, number = tcbk_rate, rank = tcbk_rate_rank)
ret_catch_ydline <- kickoffs_2023_standard |> filter(kickoff_returner_player_id != 'NA' & fumble_lost == 0) |>
  group_by(posteam) |> summarise(num = n(), avg_catch_ydline = mean(land_yardline))
ret_catch_ydline <- teams |> left_join(ret_catch_ydline, by = c('team' = 'posteam')) |>
  mutate(avg_catch_ydline_rank = rank(-avg_catch_ydline, ties.method = "min")) |>
  select(team, avg_catch_ydline, avg_catch_ydline_rank) |> filter(team == 'JAX') |> mutate(team = ' Avg Catch Yardline') |>
  rename(stat = team, number = avg_catch_ydline, rank = avg_catch_ydline_rank)
ret_return_rate <- kickoffs_2023_standard |> filter(fumble_lost == 0) |>
  group_by(posteam, return) |> summarise(num = n()) |> mutate(return_rate = round(num/sum(num),3)) |>
  filter(return == 1) |> ungroup()
ret_return_rate <- teams |> left_join(ret_return_rate, by = c('team' = 'posteam')) |>
  mutate(return_rate = ifelse(is.na(return_rate), 0, return_rate), return_rate_rank = rank(return_rate, ties.method = "min")) |>
  select(team, return_rate, return_rate_rank) |> filter(team == 'JAX') |> mutate(team = 'Return Rate') |>
  rename(stat = team, number = return_rate, rank = return_rate_rank)
ret_return_yards <- kickoffs_2023_standard |> filter(return == 1 & fumble_lost == 0) |>
  group_by(posteam) |> summarise(num = n(), avg_return_yards = mean(return_yards))
ret_return_yards <- teams |> left_join(ret_return_yards, by = c('team' = 'posteam')) |>
  mutate(avg_return_yards = ifelse(is.na(avg_return_yards), 0, avg_return_yards),
         avg_return_yards_rank = rank(-avg_return_yards, ties.method = "min")) |>
  select(team, avg_return_yards, avg_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg Return Yards') |>
  rename(stat = team, number = avg_return_yards, rank = avg_return_yards_rank)
ret_lz_return_yards <- kickoffs_2023_standard |> filter(return == 1 & landing_zone == 1 & fumble_lost == 0) |>
  group_by(posteam) |> summarise(num = n(), avg_lz_return_yards = mean(return_yards))
ret_lz_return_yards <- teams |> left_join(ret_lz_return_yards, by = c('team' = 'posteam')) |>
  mutate(avg_lz_return_yards = ifelse(is.na(avg_lz_return_yards), 0, avg_lz_return_yards),
         avg_lz_return_yards_rank = rank(-avg_lz_return_yards, ties.method = "min")) |>
  select(team, avg_lz_return_yards, avg_lz_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg LZ Return Yards') |>
  rename(stat = team, number = avg_lz_return_yards, rank = avg_lz_return_yards_rank)
ret_ez_return_yards <- kickoffs_2023_standard |> filter(return == 1 & land_yardline < 0 & fumble_lost == 0) |>
  group_by(posteam) |> summarise(num = n(), avg_ez_return_yards = mean(return_yards))
ret_ez_return_yards <- teams |> left_join(ret_ez_return_yards, by = c('team' = 'posteam')) |>
  mutate(avg_ez_return_yards = ifelse(is.na(avg_ez_return_yards), 0, avg_ez_return_yards),
         avg_ez_return_yards_rank = rank(-avg_ez_return_yards, ties.method = "min")) |>
  select(team, avg_ez_return_yards, avg_ez_return_yards_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg EZ Return Yards') |>
  rename(stat = team, number = avg_ez_return_yards, rank = avg_ez_return_yards_rank)
ret_drive_start <- kickoffs_2023_standard |>  filter(fumble_lost == 0) |>
  group_by(posteam) |> summarise(num = n(), avg_start_yardline = mean(drive_start_100))
ret_drive_start <- teams |> left_join(ret_drive_start, by = c('team' = 'posteam')) |>
  mutate(avg_start_yardline = ifelse(is.na(avg_start_yardline), 0, avg_start_yardline),
         avg_start_yardline_rank = rank(-avg_start_yardline, ties.method = "min")) |>
  select(team, avg_start_yardline, avg_start_yardline_rank) |> filter(team == 'JAX') |> mutate(team = 'Avg Drive Start') |>
  rename(stat = team, number = avg_start_yardline, rank = avg_start_yardline_rank)
### Jags Kickoff Return Table
jags_ret_stats <- bind_rows(list(ret_lz, ret_tcbk, ret_o20, ret_ill, ret_return_rate, ret_catch_ydline, ret_return_yards,
                               ret_lz_return_yards, ret_ez_return_yards, ret_drive_start))
rank_numbers <- jags_ret_stats$rank
rank_colors <- sapply(as.numeric(rank_numbers), rank_map_to_color)
jags_ret_stats <- cbind(jags_ret_stats, rank_colors)
jags_ret_tbl <- jags_ret_stats |>
  ungroup() |> gt() |>
  fmt_percent(columns = number, rows = 1:5, decimals = 1) |>
  fmt_number(columns = number, rows = 6:10, decimals = 1) |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  gt_theme_538() |>
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Kickoff Return Stats",
             subtitle = "Excludes Onside Kicks, final 2 mins of half, and non-35 yardline kickoffs") |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(stat = "Stat", number = "Number/Rate", rank = "Rank") |>
    tab_style(style = cell_fill(color = from_column(column = "rank_colors")), 
              locations = cells_body(columns = 'rank')) |>
    cols_hide(rank_colors)
jags_ret_tbl
gtsave(jags_ret_tbl, "Kickoffs/Jags 2024 Kickoff Return Tbl.png")
rm(jags_ret_tbl, jags_ret_stats, ret_lz, ret_o20, ret_ill, ret_tcbk, ret_catch_ydline, ret_return_rate, ret_return_yards,
   ret_lz_return_yards, ret_ez_return_yards, ret_drive_start)

### Drive Success by criteria
lz_rates <- kickoffs_2023_standard |> group_by(posteam, landing_zone) |> summarise(num = n()) |>
  mutate(lz_rate = num/sum(num)) |> filter(landing_zone == 1)
lz_success_avg <- kickoffs_2023_standard |> group_by(landing_zone) |>
  summarise(num = n(), avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  mutate(lz_rate = num/sum(num)) |> filter(landing_zone == 1) |> select(-num) |>
  rename(posteam = landing_zone) |> mutate(posteam = 'NFL Average')
lz_success <- kickoffs_2023_standard |> filter(landing_zone == 1) |> group_by(posteam) |>
  summarise(avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  left_join(select(lz_rates, posteam, lz_rate), by = 'posteam') |>
  rbind(lz_success_avg) |> left_join(team_info, by = c('posteam' = 'team_abbr')) |>
  relocate(posteam, team_logo_espn, lz_rate)
tcbk_rates <- kickoffs_2023_standard |> group_by(posteam, touchback) |> summarise(num = n()) |>
  mutate(tcbk_rate = num/sum(num)) |> filter(touchback == 1)
tcbk_success_avg <- kickoffs_2023_standard |> group_by(touchback) |>
  summarise(num = n(), avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  mutate(tcbk_rate = num/sum(num)) |> filter(touchback == 1) |> select(-num) |>
  rename(posteam = touchback) |> mutate(posteam = 'NFL Average')
tcbk_success <- kickoffs_2023_standard |> filter(touchback == 1) |> group_by(posteam) |>
  summarise(avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  left_join(select(tcbk_rates, posteam, tcbk_rate), by = 'posteam') |>
  rbind(tcbk_success_avg) |> left_join(team_info, by = c('posteam' = 'team_abbr')) |> 
  relocate(posteam, team_logo_espn, tcbk_rate)
ill_rates <- kickoffs_2023_standard |> group_by(posteam, illegal_kick) |> summarise(num = n()) |>
  mutate(ill_rate = num/sum(num)) |> filter(illegal_kick == 1)
ill_success_avg <- kickoffs_2023_standard |> group_by(illegal_kick) |>
  summarise(num = n(), avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  mutate(ill_rate = num/sum(num)) |> filter(illegal_kick == 1) |> select(-num) |>
  rename(posteam = illegal_kick) |> mutate(posteam = 'NFL Average')
ill_success <- kickoffs_2023_standard |> filter(illegal_kick == 1) |> group_by(posteam) |>
  summarise(avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  left_join(select(ill_rates, posteam, ill_rate), by = 'posteam') |>
  rbind(ill_success_avg) |> left_join(team_info, by = c('posteam' = 'team_abbr')) |> 
  relocate(posteam, team_logo_espn, ill_rate)
ret_rates <- kickoffs_2023_standard |> group_by(posteam, return) |> summarise(num = n()) |>
  mutate(ret_rate = num/sum(num)) |> filter(return == 1)
ret_success_avg <- kickoffs_2023_standard |> group_by(return) |>
  summarise(num = n(), avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  mutate(ret_rate = num/sum(num)) |> filter(return == 1) |> select(-num) |>
  rename(posteam = return) |> mutate(posteam = 'NFL Average')
ret_success <- kickoffs_2023_standard |> filter(return == 1) |> group_by(posteam) |>
  summarise(avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  left_join(select(ret_rates, posteam, ret_rate), by = 'posteam') |>
  rbind(ret_success_avg) |> left_join(team_info, by = c('posteam' = 'team_abbr')) |>
  relocate(posteam, team_logo_espn, ret_rate)
ezret_rates <- kickoffs_2023_standard |> group_by(posteam, endzone_land, return) |> summarise(num = n()) |> group_by(posteam) |>
  mutate(ezret_rate = num/sum(num)) |> filter(endzone_land == 1 & return == 1)
ezret_success_avg <- kickoffs_2023_standard |> group_by(endzone_land, return) |>
  summarise(num = n(), avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |> ungroup() |>
  mutate(ezret_rate = num/sum(num)) |> filter(return == 1 & endzone_land == 1) |> select(-return, -num) |>
  rename(posteam = endzone_land) |> mutate(posteam = 'NFL Average')
ezret_success <- kickoffs_2023_standard |> filter(return == 1 & endzone_land == 1) |> group_by(posteam) |>
  summarise(avg_plays = mean(drive_play_count), avg_first_downs = mean(drive_first_downs), avg_redzone = mean(drive_inside20),
            avg_scores = mean(drive_ended_with_score), avg_td = mean(drive_is_td)) |>
  left_join(select(ezret_rates, posteam, ezret_rate), by = 'posteam') |>
  rbind(ezret_success_avg) |> left_join(team_info, by = c('posteam' = 'team_abbr')) |> 
  relocate(posteam, team_logo_espn, ezret_rate)
rm(lz_rates, lz_success_avg, tcbk_rates, tcbk_success_avg, ill_rates, ill_success_avg, ret_rates, ret_success_avg,
   ezret_rates, ezret_success_avg)
write_csv(lz_success, "Kickoffs/Kickoff_app/lzSuccess.csv")
write_csv(tcbk_success, "Kickoffs/Kickoff_app/tcbkSuccess.csv")
write_csv(ill_success, "Kickoffs/Kickoff_app/illSuccess.csv")
write_csv(ret_success, "Kickoffs/Kickoff_app/retSuccess.csv")
write_csv(ezret_success, "Kickoffs/Kickoff_app/ezretSuccess.csv")

### OLD
# old_kickoffs <- load_pbp(2016:2023) |>
#   filter(kickoff_attempt == 1 & play_type == "kickoff" & own_kickoff_recovery == 0 & fumble_lost == 0)
# old_kickoffs$yardline_100[old_kickoffs$yardline_100 == 65] <- 35
# 
# kickoffs_2023 <- old_kickoffs |>
#   filter(season == 2023 & yardline_100 <= 50) |>
#   mutate(kick_team = ifelse(season == 2022, posteam, defteam),
#          catch_yardline = 100 - yardline_100 - kick_distance)
# old_kickoff_stats <- old_kickoffs |>
#   filter(yardline_100 <= 50) |>
#   mutate(is_return = ifelse(kickoff_out_of_bounds == 0 & touchback == 0 & kickoff_downed == 0 & kickoff_fair_catch == 0, 1, 0),
#          kick_team = ifelse(season == 2022, posteam, defteam),
#          catch_yardline = 100 - yardline_100 - kick_distance,
#          in_landing_zone = ifelse(between(catch_yardline, 1, 20), 1, 0))

### Kicking Stats
# jags_kick_touchback <- old_kickoff_stats |>
#   group_by(kick_team, touchback) |> summarise(num = n()) |> mutate(touchback_rate = round(num/sum(num),3)) |>
#   filter(touchback == 1 & kick_team == "JAX") |> select(-touchback, -num)
# nfl_avg_kick_touchback <- old_kickoff_stats |>
#   group_by(touchback) |> summarise(num = n()) |> mutate(touchback_rate = round(num/sum(num),3), kick_team = "NFL Average") |>
#   filter(touchback == 1) |> relocate(kick_team) |> select(-touchback, -num)
# jags_kick_inside_20 <- old_kickoff_stats |>
#   group_by(kick_team, kickoff_inside_twenty) |> summarise(num = n()) |> mutate(inside_20_rate = round(num/sum(num),3)) |>
#   filter(kickoff_inside_twenty == 1 & kick_team == "JAX") |> select(-kickoff_inside_twenty, -num)
# nfl_avg_kick_inside_20 <- old_kickoff_stats |>
#   group_by(kickoff_inside_twenty) |> summarise(num = n()) |> mutate(inside_20_rate = round(num/sum(num),3), kick_team = "NFL Average") |>
#   filter(kickoff_inside_twenty == 1) |> relocate(kick_team) |> select(-kickoff_inside_twenty, -num)
# jags_kick_returns <- old_kickoff_stats |>
#   filter(!is.na(return_yards) & kick_team == "JAX") |> group_by(kick_team) |>
#   summarise(num = n(), avg_return_yards = mean(return_yards)) |> select(-num)
# nfl_avg_kick_returns <- old_kickoff_stats |>
#   filter(!is.na(return_yards)) |> group_by(play_type) |> summarise(num = n(), avg_return_yards = mean(return_yards)) |>
#   mutate(kick_team = "NFL Average") |> select(-num, -play_type) |> relocate(kick_team)
# ### Combine Kicking stats 
# jags_kick_combined <- jags_kick_touchback |>
#   left_join(jags_kick_inside_20, by = 'kick_team') |> left_join(jags_kick_returns, by = 'kick_team')
# nfl_avg_kick_combined <- nfl_avg_kick_touchback |>
#   left_join(nfl_avg_kick_inside_20, by = 'kick_team') |> left_join(nfl_avg_kick_returns, by = 'kick_team')
# rm(jags_kick_touchback, jags_kick_inside_20, jags_kick_returns, nfl_avg_kick_touchback, nfl_avg_kick_inside_20, nfl_avg_kick_returns)
# kick_combined_tbl <- jags_kick_combined |>
#   rbind(nfl_avg_kick_combined) |>
#   # round(avg_return_yards, 2) |>
#   ungroup() |> gt() |>
#   opt_align_table_header("center") |> cols_align("center") |>
#   fmt_percent(columns = c(touchback_rate, inside_20_rate), decimals = 1) |>
#   tab_header(title = "NFL Kickoff Stats",
#              subtitle = "2016-2023") |>
#   cols_label(kick_team = "Kickoff Team", touchback_rate = "Touchback Rate",
#              inside_20_rate = "Inside 20 Yardline Rate", avg_return_yards = "Average Return Yards") |>
#   tab_source_note(andrew_caption) |>
#   gt_theme_538()
# kick_combined_tbl
# gtsave(kick_combined_tbl, "Kickoffs/Kickoff Stats.png")
# 
# ### Return Stats
# jags_return_touchback <- old_kickoff_stats |>
#   group_by(return_team, touchback) |> summarise(num = n()) |> mutate(touchback_rate = round(num/sum(num),3)) |>
#   filter(touchback == 1 & return_team == "JAX") |> select(-touchback, -num)
# nfl_avg_return_touchback <- old_kickoff_stats |>
#   group_by(touchback) |> summarise(num = n()) |> mutate(touchback_rate = round(num/sum(num),3), return_team = "NFL Average") |>
#   filter(touchback == 1) |> relocate(return_team) |> select(-touchback, -num)
# jags_return_return_rate <- old_kickoff_stats |>
#   group_by(return_team, is_return) |> summarise(num = n()) |> mutate(return_rate = round(num/sum(num),3)) |>
#   filter(is_return == 1 & return_team == "JAX") |> select(-is_return, -num)
# nfl_avg_return_return_rate <- old_kickoff_stats |>
#   group_by(is_return) |> summarise(num = n()) |> mutate(return_rate = round(num/sum(num),3), return_team = "NFL Average") |>
#   filter(is_return == 1) |> relocate(return_team) |> select(-is_return, -num)
# jags_return_returns <- old_kickoff_stats |>
#   filter(!is.na(return_yards) & return_team == "JAX") |> group_by(return_team) |>
#   summarise(num = n(), avg_return_yards = mean(return_yards)) |> select(-num)
# nfl_avg_return_returns <- old_kickoff_stats |>
#   filter(!is.na(return_yards)) |> group_by(play_type) |> summarise(num = n(), avg_return_yards = mean(return_yards)) |>
#   mutate(return_team = "NFL Average") |> select(-num, -play_type) |> relocate(return_team)
# ### Combine Return stats 
# jags_return_combined <- jags_return_touchback |>
#   left_join(jags_return_return_rate, by = 'return_team') |> left_join(jags_return_returns, by = 'return_team')
# nfl_avg_return_combined <- nfl_avg_return_touchback |>
#   left_join(nfl_avg_return_return_rate, by = 'return_team') |> left_join(nfl_avg_return_returns, by = 'return_team')
# rm(jags_return_touchback, jags_return_return_rate, jags_return_returns,
#    nfl_avg_return_touchback, nfl_avg_return_return_rate, nfl_avg_return_returns)
# return_combined_tbl <- jags_return_combined |>
#   rbind(nfl_avg_return_combined) |>
#   # round(avg_return_yards, 2) |>
#   ungroup() |> gt() |>
#   opt_align_table_header("center") |> cols_align("center") |>
#   fmt_percent(columns = c(touchback_rate, return_rate), decimals = 1) |>
#   tab_header(title = "NFL Kickoff Return Stats",
#              subtitle = "2016-2023") |>
#   cols_label(return_team = "Return Team", touchback_rate = "Touchback Rate",
#              return_rate = "Kickoff Return Rate", avg_return_yards = "Average Return Yards") |>
#   tab_source_note(andrew_caption) |>
#   gt_theme_538()
# return_combined_tbl
# gtsave(return_combined_tbl, "Kickoffs/Return Stats.png")
# 
# ### 2023 Fair Catch Stats
# jags_kick_fair_catch <- kickoffs_2023 |>
#   group_by(kick_team, kickoff_fair_catch) |> summarise(num = n(), avg_kick_fair_catch_yardline = mean(catch_yardline)) |>
#   mutate(kickoff_fair_catch_rate = round(num/sum(num),3)) |>
#   # round(avg_kick_fair_catch_yardline, 2) |>
#   filter(kickoff_fair_catch == 1 & kick_team == "JAX") |> select(-kickoff_fair_catch, -num)
# jags_return_fair_catch <- kickoffs_2023 |>
#   group_by(return_team) |> summarise(num = n()) |> filter(return_team == "JAX") |>
#   mutate(return_fair_catch_rate = 0) |> select(-num)
# nfl_avg_fair_catch <- kickoffs_2023 |>
#   group_by(kickoff_fair_catch) |> summarise(num = n(), avg_kick_fair_catch_yardline = mean(catch_yardline)) |>
#   mutate(kickoff_fair_catch_rate = round(num/sum(num),3)) |> filter(kickoff_fair_catch == 1) |>
#   mutate(kick_team = "NFL Average") |> select(-kickoff_fair_catch, -num) |> relocate(kick_team)
# ### Combine Fair Catch Stats
# jags_fair_catch_combined <- jags_kick_fair_catch |>
#   left_join(jags_return_fair_catch, by = c('kick_team' = 'return_team'))
# rm(jags_kick_fair_catch, jags_return_fair_catch)
# fair_catch_combined_tbl <- jags_fair_catch_combined |>
#   rbind(nfl_avg_fair_catch) |>
#   # round(avg_return_yards, 2) |>
#   ungroup() |> gt() |>
#   opt_align_table_header("center") |> cols_align("center") |>
#   fmt_percent(columns = c(kickoff_fair_catch_rate, return_fair_catch_rate), decimals = 1) |>
#   tab_header(title = "NFL Kickoff Fair Catch Stats",
#              subtitle = "2023") |>
#   cols_label(kick_team = "Team", avg_kick_fair_catch_yardline = "Average Kickoff Fair Catch Yardline",
#              kickoff_fair_catch_rate = "Kickoff Fair Catch Rate", return_fair_catch_rate = "Return Fair Catch Rate") |>
#   tab_source_note(andrew_caption) |>
#   gt_theme_538()
# fair_catch_combined_tbl
# gtsave(fair_catch_combined_tbl, "Kickoffs/Fair Catch Stats.png")