pass_pbp <- load_pbp(2000:2023) |>
  select(season, passer_player_id, passer_player_name, receiver_player_id, receiver_player_name, pass) |>
  filter(pass == 1 & !is.na(receiver_player_id))
# all_draft <- load_draft_picks(2000:2023) |>
#   rename(draft_season = season)
player_info_skill <- load_players() |>
  filter(position %in% c('RB', 'WR', 'TE', 'QB'))

pass_pbp_info <- pass_pbp |>
  left_join(player_info_skill |> select(gsis_id, rookie_year, headshot), by = c('passer_player_id' = 'gsis_id')) |>
  left_join(player_info_skill |> select(gsis_id, position), by = c('receiver_player_id' = 'gsis_id'))

pass_pbp_info <- pass_pbp_info |>
  filter(!is.na(rookie_year), position %in% c('RB', 'WR', 'TE'), season >= 2004) |>
  mutate(is_rookie = ifelse(rookie_year == season, "Rookie", "Non-Rookie"))

### Splits Pos Target Share by Rook/Non-Rook
binary_pos_split <- pass_pbp_info |>
  group_by(is_rookie, position) |>
  summarise(targets = n()) |>
  mutate(target_share = targets/sum(targets)) |>
  arrange(position)
binary_pos_split_tbl <- binary_pos_split |>
  ungroup() |> gt() |>
  fmt_percent(columns = target_share, decimals = 1) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note(andrew_caption) |>
  cols_label(is_rookie = "QB Rookie Status", target_share = "Target Share") |> opt_row_striping() |>
  tab_header(title = "Positional Target Share by Rookie Status",
             subtitle = "2004-2023") |>
  gt_theme_538()
binary_pos_split_tbl
gtsave(binary_pos_split_tbl, "Fantasy/BinaryPosSplit.png")

### Pos Target Share over time
pos_split_over_time <- pass_pbp_info |>
  group_by(season, is_rookie, position) |>
  summarise(targets = n()) |>
  mutate(target_share = 100*(targets/sum(targets))) |>
  filter(position == "TE")
ggplot(data=pos_split_over_time, aes(x=season, y=target_share, group = is_rookie, color = is_rookie)) +
  geom_line() +
  scale_color_manual(name = "series",values = c('gray45', 'blue')) +
  theme_bw()









