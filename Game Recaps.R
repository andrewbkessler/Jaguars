### Current Week
current_week <- 1
# current_week <- get_current_week(use_date = TRUE)
### Load PBP
full_pbp <- load_pbp(seasons = most_recent_season()) |> filter(!is.na(play_type))
jags_pbp <- full_pbp |>
  filter(home_team == 'JAX' | away_team == 'JAX')
week_pbp <- jags_pbp |> filter(week == current_week) |>
  mutate(play_index = row_number(),
         jags_wpa = ifelse(posteam == 'JAX', wpa, -1*wpa),
         jags_wp = 100*ifelse(home_team == 'JAX', home_wp, away_wp),
         jags_pts = ifelse(home_team == 'JAX', total_home_score, total_away_score),
         opp_pts = ifelse(home_team == 'JAX', total_away_score, total_home_score),
         field_goal_made = ifelse(field_goal_result == 'made', 1, 0),
         long_field_goal = ifelse(field_goal_result == 'made', kick_distance, ''),
         xp_made = ifelse(extra_point_result == 'good', 1, 0),
         turnover = ifelse(field_goal_attempt == 1 & (field_goal_result == 'missed' | field_goal_result == 'blocked'), 1,
                           ifelse(play_type != 'punt' & (interception == 1 | fumble_lost == 1 | fourth_down_failed == 1), 1, 0)),
         drive_top = ifelse(play_id == drive_play_id_ended, period_to_seconds(ms(drive_time_of_possession)), ""),
         wp_desc = ifelse(safety == 1, paste0(safety_player_name, " Safety"),
                    ifelse(touchdown == 1 & play_type == 'run' & fumble_lost == 0, paste0(rusher_player_name, " ", rushing_yards," Yd Rushing TD"),
                    ifelse(touchdown == 1 & play_type == 'pass' & interception == 0 & fumble_lost == 0, paste0(receiver_player_name, " ", receiving_yards," Yd Receiving TD"),
                    ifelse(interception == 1 & touchdown == 0, paste0(interception_player_name, " Interception"),
                    ifelse(interception == 1 & touchdown == 1, paste0(interception_player_name, " Pick 6"),
                    ifelse(fumble_lost == 1 & touchdown == 0 & sack == 1, paste0(forced_fumble_player_1_player_name, " Strip Sack"),
                    ifelse(fumble_lost == 1 & touchdown == 0 & sack == 0 & kickoff_attempt == 0 & punt_attempt == 0, paste0(fumbled_1_player_name, " Fumble"),
                    ifelse(fumble_lost == 1 & touchdown == 1, paste0(td_player_name, " Fumble TD"),
                    ifelse(sack == 1 & fumble_lost == 0 & safety == 0 & !is.na(sack_player_name), paste0(sack_player_name," Sack"),
                    ifelse(sack == 1 & fumble_lost == 0 & safety == 0 & is.na(sack_player_name), paste0(half_sack_1_player_name, " & ", half_sack_2_player_name, " Sack"),
                    ifelse(fourth_down_converted == 1, "Successful 4th Down Conversion",
                    ifelse(fourth_down_failed == 1, "Failed 4th Down Conversion",
                    ifelse(complete_pass == 1 & touchdown == 0 & fumble_lost == 0, paste0(receiver_player_name, " ", yards_gained, " Yard Reception"),
                    ifelse(penalty == 1 & play_type == 'no_play', paste0(penalty_player_name, " ", penalty_type),
                    ifelse(incomplete_pass == 1 & penalty == 0, paste0(air_yards, " Yard Incompletion"),
                    ifelse(rush_attempt == 1 & fumble_lost == 0 & touchdown == 0 & penalty == 0, paste0(rusher_player_name, " ", yards_gained, " Yard Rush"),
                    ifelse(kickoff_attempt == 1 & touchdown == 1, paste0(kickoff_returner_player_name, " ", return_yards, " Yd Kickoff Return TD"),
                    ifelse(two_point_attempt == 1 & two_point_conv_result == 'success', "Successful 2-Pt Conversion",
                    ifelse(two_point_attempt == 1 & two_point_conv_result == 'failure', "2-Pt Conversion Failed",
                    ifelse(field_goal_attempt == 1 & field_goal_result == 'made', paste0(kicker_player_name, " ", kick_distance, " Yd Field Goal"),
                    ifelse(field_goal_attempt == 1 & field_goal_result == 'missed', paste0(kicker_player_name, " Missed ", kick_distance, " Yd Field Goal"),
                    ifelse(field_goal_attempt == 1 & field_goal_result == 'blocked', paste0(blocked_player_name, " Field Goal Block"),
                    ifelse(punt_attempt == 1 & touchdown == 0 & fumble_lost == 0 & punt_blocked == 0 & punt_fair_catch == 0, paste0(punt_returner_player_name, " ", return_yards, " Yd Punt Return"),
                    ifelse(punt_attempt == 1 & touchdown == 1 & fumble_lost == 0 & punt_blocked == 0, paste0(punt_returner_player_name, " ", return_yards, " Yd Punt Return TD"),
                    ifelse(penalty == 1 & penalty_type == 'Intentional Grounding', paste0(penalty_player_name, " Intentional Grounding"),
                    ifelse(punt_attempt == 1 & punt_fair_catch == 1, paste0(punter_player_name, " ", kick_distance, " Yd Punt - Fair Catch"),
                    ifelse(rush_attempt == 1 & penalty == 1, paste0(rusher_player_name, " ", rushing_yards, " Yd Rush, ", penalty_type, " on ", penalty_player_name),
                    ifelse(pass_attempt == 1 & penalty == 1, paste0(receiver_player_name, " ", receiving_yards, " Yd Rec, ", penalty_type, " on ", penalty_player_name),
                    ifelse(kickoff_attempt == 1 & touchdown == 0 & fumble_lost == 0 & kickoff_out_of_bounds == 0, paste0(kickoff_returner_player_name, " ", return_yards, " Yd Kickoff Return"),
                    ifelse(punt_attempt == 1 & punt_blocked == 1 & touchdown == 1, paste0("Punt Block by ", blocked_player_name, ", returned for TD by ", td_player_name),
                    ifelse(punt_attempt == 1 & punt_blocked == 1 & touchdown == 0, paste0("Punt Block by ", blocked_player_name),
                    ifelse(punt_attempt == 1 & punt_blocked == 0 & touchdown == 0 & fumble_lost == 1, paste0("Punt Fumbled by ", fumbled_1_player_name),
                    ifelse(kickoff_attempt == 1 & touchdown == 0 & fumble_lost == 1, paste0("Kickoff Fumbled by ", fumbled_1_player_name),
                    "N/A"))))))))))))))))))))))))))))))))))
### Need to figure out way to get fumble on punt as turnover

jags_logo_df <- teams_colors_logos |> filter(team_abbr == 'JAX') |> select(team_logo_espn)
jags_logo <- jags_logo_df$team_logo_espn[[1]]
jags_opp_df <- week_pbp |> mutate(opp = ifelse(home_team == 'JAX', away_team, home_team))
jags_opp <- jags_opp_df$opp[[1]]
opp_color_df <- teams_colors_logos |> filter(team_abbr == jags_opp_df$opp[[1]]) |> select(team_color)
opp_color <- opp_color_df$team_color[[1]]
opp_logo_df <- teams_colors_logos |> filter(team_abbr == jags_opp_df$opp[[1]]) |> select(team_logo_espn)
opp_logo <- opp_logo_df$team_logo_espn[[1]]
rm(jags_logo_df, jags_opp_df, opp_color_df, opp_logo_df)

# wpa_amts <- full_pbp |> group_by(round(wpa,2)) |> summarise(num = n())
# na_wpa <- full_pbp |> filter(is.na(wpa)) |> group_by(play_type) |> summarise(num = n())

### Top 5 WPA Plays
top_5_wpa <- week_pbp |>
  mutate(abs_wpa = abs(wpa)) |>
  left_join(select(teams_colors_logos, team_abbr, team_logo_espn), by = c('posteam' = 'team_abbr')) |>
  arrange(-abs_wpa) |>
  group_by(week) |>
  slice(1:5) |>
  select(week, team_logo_espn, qtr, time, jags_pts, opp_pts, down, ydstogo, yrdln, wp_desc, jags_wpa)
top_5_wpa_tbl <- top_5_wpa |>
  ungroup() |> gt() |>
  cols_hide(week) |>
  gt_theme_espn() |>
  fmt_percent(columns = jags_wpa, decimals = 1) |>
  data_color(columns = jags_wpa, method = "bin", palette = c("red", "green"), bins = c(-101, 0, 101)) |>
  gt_img_rows(team_logo_espn) |>
  # tab_source_note(andrew_caption) |>
  tab_header(title = "Top 5 Key Plays",
             subtitle = "Based on Winning Percentage Change (+ change is better for Jaguars)") |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  tab_style(style = list(cell_fill(color = teal), cell_text(color = "white")), locations = cells_body(columns = jags_pts)) |>
  tab_style(style = list(cell_fill(color = opp_color), cell_text(color = "white")), locations = cells_body(columns = opp_pts)) |>
  cols_label(team_logo_espn = "Offensive Team", qtr = "Quarter", time = "Time", jags_pts = "JAX", opp_pts = jags_opp, down = "Down",
             ydstogo = "Yards to Go", yrdln = "Yardline", wp_desc = "Play Description", jags_wpa = "Win % Change")
# top_5_wpa_tbl
gtsave(top_5_wpa_tbl, paste('Game Recaps/Week ', current_week, '/', 'Top 5 Plays.png', sep = ''))

### Week Summary Stats
week_stats <- week_pbp |>
  filter(play_type == 'pass' | play_type == 'run' | play_type == 'qb_kneel' | play_type == 'qb_spike') |>
  group_by(posteam) |>
  summarise(num_plays = n(), epa_per_play = mean(epa), success_rate = mean(success), first_downs = sum(first_down), score = max(posteam_score_post),
            turnovers = sum(turnover), yards = sum(yards_gained))
run_stats <- week_pbp |> filter(play_type == 'run') |> group_by(posteam) |>
  summarise(num_runs = n(), epa_per_run = mean(epa), rush_success_rate = mean(success))
pass_stats <- week_pbp |> filter(play_type == 'pass') |> group_by(posteam) |>
  summarise(num_passes = n(), epa_per_pass = mean(epa), pass_success_rate = mean(success))
week_sum_stats <- week_stats |>
  left_join(run_stats, by = 'posteam') |> left_join(pass_stats, by = 'posteam') |>
  mutate(posteam = ifelse(posteam == 'JAX', 'JAX', 'opp')) |>
  relocate(posteam, score, turnovers, num_plays, yards, first_downs, epa_per_play, success_rate, num_passes, epa_per_pass, pass_success_rate, num_runs, epa_per_run, rush_success_rate) |>
  rename(Score = score, 'Offensive Plays' = num_plays, 'First Downs' = first_downs, 'EPA per Play' = epa_per_play, 'Success Rate' = success_rate,
         'Pass Attempts' = num_passes, 'EPA per Pass' = epa_per_pass, 'Pass Success Rate' = pass_success_rate, Rushes = num_runs,
         'EPA per Rush' = epa_per_run, 'Rush Success Rate' = rush_success_rate, 'Turnovers' = turnovers, 'Yards' = yards) |>
  transpose(keep.names = 'Metric')
week_sum_stats <- week_sum_stats |>
  setnames(c('V1', 'V2'), c(week_sum_stats[1,2], week_sum_stats[1,3]), skip_absent = TRUE) |>
  slice(-1)
week_sum_stats$opp <- as.numeric(week_sum_stats$opp)
week_sum_stats$JAX <- as.numeric(week_sum_stats$JAX)
rm(week_stats, run_stats, pass_stats)
week_sum_stats_tbl <- week_sum_stats |>
  ungroup() |> gt() |>
  gt_theme_538() |>
  fmt_percent(columns = c(opp,JAX), rows = c(7, 10, 13), decimals = 1) |>
  fmt_number(columns = c(opp,JAX), rows = c(1, 2, 3, 4, 5, 8, 11), decimals = 0) |>
  fmt_number(columns = c(opp,JAX), rows = c(6, 9, 12), decimals = 3) |>
  data_color(columns = c(opp, JAX), rows =  c(1, 3:7, 9:10, 12:13), direction = "row", palette = c("red", "green")) |>
  data_color(columns = c(opp, JAX), rows =  c(2), direction = "row", palette = c("green", "red")) |>
  cols_label(JAX = img_header("JAX", jags_logo, height = 60, font_size = 14),
             opp = img_header(jags_opp, opp_logo, height = 60, font_size = 14),
             Metric = '') |>
  # tab_source_note(andrew_caption) |>
  # tab_header(title = paste0("Week ", current_week, " Summary")) |>
  opt_align_table_header(align = "center") |> cols_align("center")
# week_sum_stats_tbl
gtsave(week_sum_stats_tbl, paste('Game Recaps/Week ', current_week, '/', 'Summary Stats.png', sep = ''))

### Win Pct
jags_wp <- week_pbp |>
  mutate(wp_col = ifelse(jags_wp > 50, gold, ifelse(jags_wp < 50, opp_color, "black")))
wp_plot <- jags_wp |>
  ggplot(aes(x=play_index, y=jags_wp, color=wp_col, group = 1)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(opp_color, gold)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  scale_x_continuous(1, max(jags_wp$play_index), breaks = NULL) +
  geom_hline(yintercept=50, linetype="dashed") +
  theme_bw() +
  labs(y = "Jaguars Win Probability (%)",
       title = "Jaguars Win Probability",
       subtitle = paste0("Week ", week_pbp$week[[1]], ' vs. ', jags_opp)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, size = 48), plot.subtitle = element_text(hjust=0.5, size = 24),
        legend.position = "none")
# wp_plot
ggsave(paste('Game Recaps/Week ', current_week, '/', 'Win Prob.png', sep = ''),
       width = 14, height = 10, dpi = "retina")

time_of_poss <- week_pbp |> filter(drive_top > 0) |>
  group_by(posteam) |> summarise(top_seconds = sum(as.numeric(drive_top))) |>
  mutate(top_perc = top_seconds/sum(top_seconds),
         top_format = str_sub(hms::as_hms(top_seconds), 4, 8),
         ymax = cumsum(top_perc),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2) |>
  left_join(select(teams_colors_logos, team_abbr, team_color), by = c('posteam' = 'team_abbr'))
top_chart <- time_of_poss |>
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=posteam)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=top_format), size=10, color = "white") +
  scale_fill_manual(values = time_of_poss$team_color) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(title = "Time of Possession") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=48))
# top_chart
ggsave(paste('Game Recaps/Week ', current_week, '/', 'Time of Poss.png', sep = ''),
       width = 14, height = 10, dpi = "retina")

### Penalties
penalties <- week_pbp |>
  filter(!is.na(penalty_team)) |>
  group_by(penalty_team) |>
  summarise(num_pens = sum(penalty), pen_yards = sum(penalty_yards)) |>
  mutate(pen_perc = pen_yards/sum(pen_yards),
         ymax = cumsum(pen_perc),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2) |>
  left_join(select(teams_colors_logos, team_abbr, team_color), by = c('penalty_team' = 'team_abbr'))
penalties_graph<- penalties |>
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=penalty_team)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition,label=paste0("Pens: ", num_pens, '\n', "Yds: ", pen_yards)),
             size=10, color = "white") +
  scale_fill_manual(values = penalties$team_color) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(title = "Penalties") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=48))
# penalties_graph
ggsave(paste('Game Recaps/Week ', current_week, '/', 'Penalties.png', sep = ''), width = 14, height = 10, dpi = "retina")

### Starting Field Position
starting_field_position <- week_pbp |>
  filter(!is.na(drive_start_yard_line) & play_id == drive_play_id_started) |>
  mutate(drive_start_ydline_extract = sub("^\\S+\\s+", '', drive_start_yard_line),
         drive_start_team_extract = sub("\\s+\\S+$", "", drive_start_yard_line),
         drive_start_ydline_100 = ifelse(posteam == drive_start_team_extract, as.numeric(drive_start_ydline_extract), 100-as.numeric(drive_start_ydline_extract))) |>
  group_by(posteam) |>
  summarise(avg_starting_field_pos = mean(drive_start_ydline_100)) |>
  left_join(select(teams_colors_logos, team_abbr, team_color), by = c('posteam' = 'team_abbr'))
start_field_pos_graph <- starting_field_position |>
  ggplot(aes(x=posteam, y=avg_starting_field_pos, fill=posteam)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = starting_field_position$team_color)+
  geom_text(aes(label = round(avg_starting_field_pos,1)), vjust = 1.5, size = 24, color = "white") +
  labs(x = "",
       y="",
       title = "Average Starting Field Position") +
  theme(plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_field_pos_graph
ggsave(paste('Game Recaps/Week ', current_week, '/', 'Starting Field Position.png', sep = ''),
       width = 14, height = 10, dpi = "retina")

### Field Goals
field_goals <- week_pbp |>
  filter(field_goal_attempt == 1 | extra_point_attempt == 1) |>
  group_by(kicker_player_name) |>
  summarise(fgs_made = sum(field_goal_made, na.rm = TRUE),
            fg_attempts = sum(field_goal_attempt),
            long = ifelse(fgs_made>0,max(long_field_goal, na.rm = TRUE),99),
            xp_made = sum(xp_made, na.rm = TRUE),
            xp_attempt = sum(extra_point_attempt)) |>
  mutate(fg_line = paste0(fgs_made,"/",fg_attempts),
         fg_pct = ifelse(fg_attempts > 0, paste0(100*fgs_made/fg_attempts, '%'), 'N/A'),
         xp_line = paste0(xp_made,"/",xp_attempt),
         long = ifelse(long == 99, 'N/A', long)) |>
  select(kicker_player_name, fg_line, fg_pct, long, xp_line)
field_goals_tbl <- field_goals |>
  ungroup() |> gt() |>
  gt_theme_538() |>
  # fmt_percent(columns = fg_pct, decimals = 0) |>
  cols_label(kicker_player_name = 'Kicker', fg_line = 'FG', fg_pct = 'PCT', xp_line = 'XP') |>
  tab_header(title = "Kicking Stats") |>
  opt_align_table_header(align = "center") |> cols_align("center")
field_goals_tbl
gtsave(field_goals_tbl, paste('Game Recaps/Week ', current_week, '/', 'Field Goals.png', sep = ''))



