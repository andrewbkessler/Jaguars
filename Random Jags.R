player_info <- load_players()
jags_rushes <- load_pbp(1999:2023) |>
  filter(posteam == "JAX" & (play_type == "run" | (play_type == "pass" & complete_pass == 1)) & !is.na(epa))
jags_dropbacks <- load_pbp(1999:2023) |>
  filter(posteam == "JAX" & qb_dropback == 1 & !is.na(epa))

jags_rbs <- jags_rushes |>
  mutate(rb_id = ifelse(play_type == "run", rusher_player_id, receiver_player_id),
         modified_play_id = sprintf("%004d", play_id),
         modified_week = sprintf("%002d", week),
         rank_play_id = paste(season, modified_week, modified_play_id, sep = "")) |>
  left_join(select(player_info, gsis_id, display_name, headshot, position), by = c('rb_id' = 'gsis_id')) |>
  filter(rb_id %in% c('00-0016098', '00-0024275', '00-0033856', '00-0032209', '00-0036973', '00-0035831'))
jags_rbs$rank_play_id <- as.numeric(jags_rbs$rank_play_id)
jags_rbs <- jags_rbs |>
  select(rb_id, display_name, game_id, season, week, modified_week, play_id, modified_play_id, epa, rank_play_id, headshot) |>
  group_by(rb_id) |>
  mutate(touch_num = rank(as.numeric(rank_play_id), ties.method = "first"),
         cum_epa = cumsum(epa),
         player_color = case_when(rb_id == "00-0036973" ~ teal, TRUE ~ "gray"))
jags_rbs_last_pt <- jags_rbs |> 
  group_by(display_name) |>
  top_n(1, touch_num) 
Jags_rb_epa <- ggplot(jags_rbs, aes(x = touch_num, y = cum_epa, group = display_name, color = player_color)) +
  geom_line() +
  theme_bw() +
  scale_color_identity(labels = c(blue = "00-0036973", gray = "Other")) +
  geom_text_repel(aes(label = display_name), data = jags_rbs_last_pt, size = 3, colour = "black") +
  labs(x = "Touch Number (Rushes & Receptions)",
       y = "Cumulative EPA",
       title = "Historical Jaguars RB's Cumulative EPA by Touch Number",
       subtitle = "Jaguars seasons only (excludes Fred Taylor's 1998 season)",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
Jags_rb_epa
ggsave('Offense/Jags RB Cum EPA.png', width = 14, height = 10, dpi = "retina")

jags_qbs <- jags_dropbacks |>
  mutate(modified_play_id = sprintf("%004d", play_id),
         modified_week = sprintf("%002d", week),
         rank_play_id = paste(season, modified_week, modified_play_id, sep = "")) |>
  left_join(select(player_info, gsis_id, display_name, position), by = c('passer_id' = 'gsis_id')) |>
  filter(passer_id %in% c('00-0031407', '00-0021231', '00-0036971', '00-0022177', '00-0035289', '00-0027948'))
jags_qbs$rank_play_id <- as.numeric(jags_qbs$rank_play_id)
jags_qbs <- jags_qbs |>
  select(passer_id, display_name, game_id, season, week, modified_week, play_id, modified_play_id, epa, rank_play_id) |>
  group_by(passer_id) |>
  mutate(play_num = rank(as.numeric(rank_play_id), ties.method = "first"),
         cum_epa = cumsum(epa),
         player_color = case_when(passer_id == "00-0036971" ~ teal, TRUE ~ "gray"))
jags_qbs_last_pt <- jags_qbs |> 
  group_by(display_name) |>
  top_n(1, play_num) 
trevor_seasons <- jags_qbs |>
  filter(passer_id == "00-0036971") |>
  group_by(season) |>
  summarise(num = n(), last_id = max(play_num))
Jags_qb_epa <- ggplot(jags_qbs, aes(x = play_num, y = cum_epa, group = display_name, color = player_color)) +
  geom_line() +
  theme_bw() +
  scale_color_identity(labels = c(blue = "00-0036971", gray = "Other")) +
  geom_text_repel(aes(label = display_name), data = jags_qbs_last_pt, size = 3, colour = "black") +
  geom_vline(xintercept = min(trevor_seasons$last_id), linetype = "dashed") +
  annotate("text", x= min(trevor_seasons$last_id) - 20, y=-180, label="Trevor's 2021 Season", angle=90) +
  geom_vline(xintercept = median(trevor_seasons$last_id), linetype = "dashed") +
  annotate("text", x= median(trevor_seasons$last_id) - 20, y=-180, label="Trevor's 2022 Season", angle=90) +
  labs(x = "Play Number (Dropbacks Only)",
       y = "Cumulative EPA",
       title = "Historical Jaguars QB's Cumulative EPA on Dropbacks",
       subtitle = "Jaguars seasons only",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
Jags_qb_epa
ggsave('Offense/Jags QB Cum EPA.png', width = 14, height = 10, dpi = "retina")

jags_draft_2024 <- read.csv("Draft/2024 Draftees.csv")
all_drafts <- load_draft_picks(seasons = TRUE) |> filter(season != 2024)
# jags_draft_all <- all_drafts |> filter(team == 'JAX')

team_info <- load_teams(current = FALSE)
all_first_3 <- all_drafts |>
  group_by(season, team) |>
  dplyr::slice(1:3) |>
  group_by(season, team) |>
  summarise(pos_comb = paste0(category, collapse = ", "),
            has_wr = sum(str_detect(pos_comb, 'WR')) > 0,
            has_dl = sum(str_detect(pos_comb, 'DL')) > 0,
            has_db = sum(str_detect(pos_comb, 'DB')) > 0)
perf_combo <- all_first_3 |>
  mutate(team = ifelse(team == 'KAN', 'KC', ifelse(team == 'NWE', 'NE', ifelse(team == 'NOR', 'NO', ifelse(team == 'GNB', 'GB', team))))) |>
  filter(has_wr == 1, has_dl == 1, has_db == 1) |>
  arrange(-season) |> select(-has_wr, -has_dl, -has_db) |> left_join(select(team_info, team_abbr, team_wordmark), by = c('team' = 'team_abbr')) |>
  head(19)
perf_combo_tbl <- perf_combo |>
  relocate(season, team_wordmark) |>
  select(-team) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", season = "Season", pos_comb = "Pos Combo") |> 
  opt_row_striping() |>
  tab_header(title = "WR, DL, DB Draft Combination - First 3 Picks",
             subtitle = "Since 2010") |>
  opt_align_table_header("center") |> cols_align("center") |>
  gt_theme_538()
perf_combo_tbl
gtsave(perf_combo_tbl, "Draft/First 3 Combo.png")

jags_first_3 <- all_drafts |>
  filter(team == 'JAX' & (season == 2020 | season == 2018)) |>
  group_by(season) |>
  dplyr::slice(1:3)
jags_first_3_tbl <- jags_first_3 |>
  select(season, round, pick, pfr_player_name, category, college) |>
  ungroup() |> gt() |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(season = "Season", round = "Round", pick = "Pick", pfr_player_name = "Player", category = "Position", college = "School") |> 
  opt_row_striping() |>
  tab_header(title = "Jags WR, DL, DB Draft Combinations") |>
  opt_align_table_header("center") |> cols_align("center") |>
  gt_theme_538()
jags_first_3_tbl
gtsave(jags_first_3_tbl, "Draft/Jags First 3 Combo.png")
rm(all_drafts, all_first_3, jags_draft_2024, jags_first_3, jags_first_3_tbl, perf_combo, perf_combo_tbl)


### Zay Jones: 00-0033891
pbp <- load_pbp(seasons = 2022:2023)
eligible_recs <- pbp |> filter(play_type == 'pass' & !is.na(receiver_player_id)  & season_type == 'REG') |>
  group_by(receiver_player_id) |> summarise(tgts = n()) |> filter(tgts >= 80) |> mutate(eligible = 1)
rank_palette <- colorRampPalette(c("green", "yellow", "red"))(165)
rank_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 165) color_index <- 165
  rank_palette[color_index]}
### do min 80 targets
targ_2223 <- pbp |> filter(play_type == 'pass' & !is.na(receiver_player_id)) |>
  group_by(receiver_player_id) |> summarise(targets = n()) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(tgt_rank = rank(-targets, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = targets, rank = tgt_rank) |> mutate(stat = 'Targets')
rec_2223 <- pbp |> filter(complete_pass == 1 & !is.na(receiver_player_id)) |>
  group_by(receiver_player_id) |> summarise(receptions = n()) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(rec_rank = rank(-receptions, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = receptions, rank = rec_rank) |> mutate(stat = 'Receptions')
yards_2223 <- pbp |> filter(complete_pass == 1 & !is.na(receiver_player_id)) |>
  group_by(receiver_player_id) |> summarise(yards = sum(yards_gained)) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(yds_rank = rank(-yards, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = yards, rank = yds_rank) |> mutate(stat = 'Yards')
tds_2223 <- pbp |> filter(complete_pass == 1 & !is.na(receiver_player_id)) |>
  group_by(receiver_player_id) |> summarise(tds = sum(touchdown)) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(tds_rank = rank(-tds, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = tds, rank = tds_rank) |> mutate(stat = 'TDs')
ypt_2223 <- pbp |> filter(play_type == 'pass' & !is.na(receiver_player_id)) |> 
  group_by(receiver_player_id) |> summarise(targets = n(), yards = sum(yards_gained), ypt = yards/targets) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(ypt_rank = rank(-ypt, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = ypt, rank = ypt_rank) |> mutate(stat = 'Yards per Target') |> select(-yards, -targets)
ypr_2223 <- pbp |> filter(complete_pass == 1 & !is.na(receiver_player_id)) |> 
  group_by(receiver_player_id) |> summarise(receptions = n(), yards = sum(yards_gained), ypr = yards/receptions) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(ypr_rank = rank(-ypr, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = ypr, rank = ypr_rank) |> mutate(stat = 'Yards per Reception') |> select(-yards, -receptions)
tpt_2223 <- pbp |> filter(play_type == 'pass' & !is.na(receiver_player_id)) |> 
  group_by(receiver_player_id) |> summarise(targets = n(), touchdown = sum(touchdown), tpt = touchdown/targets) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(tpt_rank = rank(-tpt, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = tpt, rank = tpt_rank) |> mutate(stat = 'TDs per Target') |> select(-touchdown, -targets)
tpr_2223 <- pbp |> filter(complete_pass == 1 & !is.na(receiver_player_id)) |> 
  group_by(receiver_player_id) |> summarise(receptions = n(), touchdown = sum(touchdown), tpr = touchdown/receptions) |>
  left_join(select(eligible_recs, receiver_player_id, eligible), by = 'receiver_player_id') |> filter(eligible == 1) |>
  mutate(tpr_rank = rank(-tpr, ties.method = "min")) |> filter(receiver_player_id == '00-0033891') |>
  rename(stat = receiver_player_id, number = tpr, rank = tpr_rank) |> mutate(stat = 'TDs per Reception') |> select(-touchdown, -receptions)
zay_info <- load_rosters(seasons = 2023) |> filter(gsis_id == '00-0033891')
zay_2223 <- rbind(targ_2223, rec_2223, yards_2223, tds_2223, ypt_2223, ypr_2223, tpt_2223, tpr_2223) |> select(-eligible) 
rank_numbers <- zay_2223$rank
rank_colors <- sapply(as.numeric(rank_numbers), rank_map_to_color)
zay_2223_tbl <- cbind(zay_2223, rank_colors)|>
  ungroup() |> gt() |>
  fmt_number(columns = number, rows = 1:4, decimals = 0) |>
  fmt_number(columns = number, rows = 5:6, decimals = 1) |>
  fmt_number(columns = number, rows = 7:8, decimals = 3) |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  gt_theme_538() |>
  opt_row_striping() |>
  tab_header(title = html(web_image(url = zay_info$headshot_url, height = px(50)), 'Zay Jones Stats'),
             subtitle = "2022-2023 Reg Seasons, min. 80 targets") |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(stat = "Stat", number = "Number", rank = "Rank") |>
  tab_style(style = cell_fill(color = from_column(column = "rank_colors")), 
            locations = cells_body(columns = 'rank')) |>
  cols_hide(rank_colors)
zay_2223_tbl
gtsave(zay_2223_tbl, "Offense/Zay Jones Stats.png")
rm(eligible_recs, rec_2223, targ_2223, tds_2223, tpr_2223, tpt_2223, yards_2223, ypr_2223, ypt_2223, zay_2223, zay_2223_tbl,
   rank_colors, rank_map_to_color, rank_numbers, rank_palette)

jags_passes <- pbp |> filter(posteam == 'JAX', play_type == 'pass', !is.na(receiver_player_name))
target_share <- jags_passes |> group_by(receiver_player_name) |> summarise(tgts = n()) |>
  mutate(tgt_share = paste0(100*round(tgts/sum(tgts),3),'%'))
third_down <- jags_passes |> filter(down == 3) |>
  group_by(receiver_player_name) |>
  summarise(tgts = n(), recs = sum(complete_pass), first_down_rate = 100*mean(first_down), success_rate = 100*mean(success)) |>
  mutate(tgt_share = paste0(100*round(tgts/sum(tgts),3),'%'),
         first_down_rate = paste0(round(first_down_rate, digits = 1), '%'),
         success_rate = paste0(round(success_rate, digits = 1), '%')) |> arrange(-tgts) |>
  select(receiver_player_name, tgts, recs, tgt_share, first_down_rate, success_rate)
fourth_down <- jags_passes |> filter(down == 4) |>
  group_by(receiver_player_name) |>
  summarise(tgts = n(), recs = sum(complete_pass), first_down_rate = 100*mean(first_down), success_rate = 100*mean(success)) |>
  mutate(tgt_share = paste0(100*round(tgts/sum(tgts),3),'%'),
         first_down_rate = paste0(round(first_down_rate, digits = 1), '%'),
         success_rate = paste0(round(success_rate, digits = 1), '%')) |> arrange(-tgts) |>
  select(receiver_player_name, tgts, recs, tgt_share, first_down_rate, success_rate)
red_zone <- jags_passes |> filter(yardline_100 <= 20) |>
  mutate(only_first_down = ifelse(first_down == 1 & touchdown == 0, 1, 0)) |>
  group_by(receiver_player_name) |> summarise(tgts = n(), recs = sum(complete_pass), first_downs_only = sum(only_first_down), tds = sum(touchdown),
                                              success_rate = 100*mean(success)) |>
  mutate(tgt_share = paste0(100*round(tgts/sum(tgts),3),'%'), success_rate = paste0(round(success_rate, digits = 1), '%')) |>
  arrange(-tgts) |> rename(first_downs = first_downs_only) |>
  select(receiver_player_name, tgts, recs, tgt_share,first_downs, tds, success_rate)
deep_passes <- jags_passes |> filter(!is.na(air_yards)) |>
  group_by(receiver_player_name) |> summarise(aDOT = mean(air_yards)) |> arrange(-aDOT)
gabe_davis_aDOT <- pbp |> filter(receiver_player_id == '00-0036196' & !is.na(air_yards)) |>
  group_by(receiver_player_id) |> summarise(aDOT = mean(air_yards))
zay_injuries <- load_injuries(seasons = 2017:2023) |> filter(gsis_id == '00-0033891')




