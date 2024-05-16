sched_2024 <- load_schedules(2024) |>
  filter(week <= 18)
team_info <- load_teams(current = TRUE)
win_totals <- read.csv("schedule/Season Win Totals.csv", header = TRUE) |>
  left_join(select(team_info, team_abbr, team_name), by = c('Team' = 'team_abbr'))
opps_2024 <- read.csv("schedule/2024 Opponents.csv", header = TRUE)
bet_lines <- read.csv("schedule/Odds API - Sheet1.csv", header = TRUE)

Jags_sched <- sched_2024 |>
  mutate(
    isJags = ifelse(away_team == "JAX" | home_team == "JAX", 1, 0),
    JagsHomeAway = ifelse(isJags == 1,
                          ifelse(away_team == "JAX", "Away",
                                 ifelse(home_team == "JAX", "Home", "")),""),
    JagsOpponent = ifelse(isJags == 1,
                          ifelse(JagsHomeAway == "Away", home_team,
                                 ifelse(JagsHomeAway == "Home", away_team,"")),""),
    JagsRest = ifelse(isJags == 1,
                      ifelse(JagsHomeAway == "Away", away_rest,
                             ifelse(JagsHomeAway == "Home", home_rest,"")),""),
    OpponentRest = ifelse(isJags == 1,
                          ifelse(JagsHomeAway == "Away", home_rest,
                                 ifelse(JagsHomeAway == "Home", away_rest,"")),""),
    RestDifferential = as.numeric(JagsRest) - as.numeric(OpponentRest)) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('JagsOpponent' = 'team_abbr')) |>
  left_join(select(win_totals, Team, Season.Win.Total), by = c('JagsOpponent' = 'Team')) |>
  filter(isJags == 1)
JagsSchedTbl <- Jags_sched |>
  select(week, team_wordmark, JagsHomeAway, RestDifferential, Season.Win.Total) |>
  ungroup() |> arrange(week) |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Opponent", JagsHomeAway = "Home/Away", RestDifferential = "Rest Disparity",
             Season.Win.Total = "Season Win Total") |>
  opt_row_striping() |>
  tab_header(title = "Jaguars Regular Season Schedule, 2024",
             subtitle = "Based on DraftKings season win totals: 5/15/24") |>
  gt_theme_538() |>
  tab_style(style = cell_fill(color = "red"), locations = cells_body(columns = RestDifferential, rows = RestDifferential < 0)) |>
  tab_style(style = cell_fill(color = "green"), locations = cells_body(columns = RestDifferential, rows = RestDifferential > 0)) |>
  data_color(columns = Season.Win.Total, palette = c("green", "red"))
# JagsSchedTbl
gtsave(JagsSchedTbl, "schedule/JagsSched2024.png")

 
away_rest <- sched_2024 |>
  mutate(team = away_team,
         RestDifferential = as.numeric(away_rest) - as.numeric(home_rest)) |>
  rename(team_rest = away_rest, opponent_rest = home_rest)
home_rest <- sched_2024 |>
  mutate(team = home_team,
         RestDifferential = as.numeric(home_rest) - as.numeric(away_rest)) |>
  rename(team_rest = home_rest, opponent_rest = away_rest)
nfl_rest <- rbind(away_rest, home_rest) |>
  mutate(pos_neg = ifelse(RestDifferential > 0, 1, ifelse(RestDifferential < 0, 0, ""))) |>
  arrange(abs(RestDifferential))
rm(away_rest, home_rest)
ggplot() +
  geom_col(data=nfl_rest, mapping=aes(x = reorder(team, RestDifferential), y = RestDifferential, group = pos_neg, fill = RestDifferential),
           colour="black", show.legend = FALSE) +
  scale_fill_gradient(low = "red", high = "green") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Team",
       y = "Rest Disparity",
       title = "NFL Rest Disparity, 2024",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('schedule/Rest Disparity Chart 2024.png',  width = 14, height = 10, dpi = "retina")

nfl_rest_agg <- nfl_rest |>
  mutate(neg_rest = ifelse(RestDifferential < 0, 1, 0),
         pos_rest = ifelse(RestDifferential > 0, 1, 0)) |>
  group_by(team) |>
  summarise(RestDisparity = sum(RestDifferential),
            neg_games = sum(neg_rest),
            pos_games = sum(pos_rest)) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr')) |>
  mutate(rank = rank(-RestDisparity, ties.method = "min"))

nfl_rest_tbl <- nfl_rest_agg |>
  relocate(team_wordmark, rank) |>
  subset(select = -c(team)) |>
  arrange(-RestDisparity) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(
    team_wordmark = "Team", RestDisparity = "Rest Disparity",
    neg_games = "Negative Rest Disparity Games", pos_games = "Positive Rest Disparity Games",
  ) |> opt_row_striping() |>
  tab_header(title = "NFL Regular Season Rest Disparity, 2024") |>
  gt_theme_538() |>
  data_color(columns = RestDisparity, palette = c("red", "green")) |>
  data_color(columns = neg_games, palette = c("green", "red")) |>
  data_color(columns = pos_games, palette = c("red", "green"))
# nfl_rest_tbl
gtsave(nfl_rest_tbl, "Schedule/NFLRestDisparity2024.png")

opps_2024 <- opps_2024 |>
  left_join(win_totals, by = c('Opponent' = 'Team'))
opps_2024 <- opps_2024 |>
  rename("opp_win_total" = 'Season.Win.Total')

win_total_sched <- win_totals |>
  left_join(opps_2024, by = 'Team') |>
  rename('win_total' = 'Season.Win.Total') |>
  group_by(Team) |>
  summarise(team_win_total = mean(win_total),
            avg_opp_win_total = round(mean(opp_win_total),2)) |>
  arrange(-avg_opp_win_total) |>
  mutate(rank = rank(-avg_opp_win_total, ties.method = "min")) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('Team' = 'team_abbr'))
win_total_tbl <- win_total_sched |>
  relocate(team_wordmark, rank) |>
  subset(select = -c(Team)) |>
  arrange(-avg_opp_win_total) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", team_win_total = "Team Win Total", avg_opp_win_total = "Avg Opponent Win Total"
  ) |> opt_row_striping() |>
  tab_header(title = "Projected Strength of Schedule, 2024",
             subtitle = "Based on DraftKings season win totals: 5/15/24") |>
  gt_theme_538() |>
  data_color(columns = avg_opp_win_total, palette = c("green", "red"))
# win_total_tbl
gtsave(win_total_tbl, "schedule/NFLSoS2024.png")

away_bet_odds <- bet_lines |>
  filter(bookmaker == 'DraftKings') |>
  mutate(team = away_team,
         opponent = home_team,
         team_spread = point_2,
         team_favored = ifelse(team_spread < 0, 1, 0))
home_bet_odds <- bet_lines |>
  filter(bookmaker == 'DraftKings') |>
  mutate(team = home_team,
         opponent = away_team,
         team_spread = point_1,
         team_favored = ifelse(team_spread < 0, 1, 0))
sched_bet_lines <- rbind(away_bet_odds, home_bet_odds) |>
  select(team, opponent, team_spread, team_favored) |>
  left_join(win_totals, by = c('team' = 'team_name')) |>
  left_join(teams_colors_logos |> select(team_abbr, team_logo_espn), by = c('Team' = 'team_abbr'))
win_tot_v_spread <- sched_bet_lines |>
  group_by(Team, team_logo_espn) |>
  summarise(favored = sum(team_favored), win_total = mean(Season.Win.Total))
ggplot(data=win_tot_v_spread, mapping = aes(x=favored, y=win_total)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_smooth(method = lm, se=FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
  xlim(0, 17) + ylim(0,17) +
  theme_bw() +
  labs(x = "Games favored in",
       y = "Season Win Total",
       title = "NFL Season Win Totals v Games Favored In",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('Schedule/Win Total v Favored 2024.png', width = 14, height = 10, dpi = "retina")
rm(away_bet_odds, home_bet_odds)

rm(Jags_sched, JagsSchedTbl, nfl_rest, nfl_rest_agg, nfl_rest_tbl, opps_2024, win_totals, bet_lines)


all_schedules <- load_schedules(2002:2023)
team_info <- load_teams(current = FALSE)
### Jags Schedule
all_jags_sched <- all_schedules |>
    mutate(isJags = ifelse(away_team == "JAX" | home_team == "JAX", 1, 0),
      JagsHomeAway = ifelse(isJags == 1, ifelse(away_team == "JAX", "Away", ifelse(home_team == "JAX", "Home", "")),""),
      JagsOpponent = ifelse(isJags == 1, ifelse(JagsHomeAway == "Away", home_team, ifelse(JagsHomeAway == "Home", away_team,"")),""),
      reg_finale = ifelse(game_type != 'REG', 0, ifelse(season < 2021 & week != 17, 0, ifelse(season >= 2021 & week != 18, 0, 1)))) |>
  filter(isJags == 1) |>
  left_join(select(team_info, team_abbr, team_name, team_conf, team_division, team_logo_espn, team_wordmark), by = c('JagsOpponent' = 'team_abbr'))

### Season Opener
jags_opener <- all_jags_sched |> filter(week == 1)
jags_opener_opp <- jags_opener |> group_by(JagsOpponent) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_opener_conf <- jags_opener |> group_by(team_conf) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_opener_div <- jags_opener |> group_by(team_division) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_opener_home_away <- jags_opener |> group_by(JagsHomeAway) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))

### Season Finale
jags_finale <- all_jags_sched |> filter(reg_finale == 1)
jags_finale_opp <- jags_finale |> group_by(JagsOpponent) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_finale_conf <- jags_finale |> group_by(team_conf) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_finale_div <- jags_finale |> group_by(team_division) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))
jags_finale_home_away <- jags_finale |> group_by(JagsHomeAway) |> summarise(num = n(), most_rec = max(season)) |> mutate(rate = round(num/sum(num),3))

### Bye Weeks
bye_weeks <- all_jags_sched |> group_by(season) |> mutate(jump = week - lag(week, default = first(week))) |>
  filter(jump == 2) |> select(season, week) |> mutate(bye_week = week - 1)
byes_by_week <- bye_weeks |> group_by(bye_week) |> summarise(num = n(), most_rec = max(season))

# ### 3 Home/Away
# bye_repeated_values <- all_jags_sched$JagsHomeAway == lag(all_jags_sched$JagsHomeAway) &
#   all_jags_sched$JagsHomeAway == lag(all_jags_sched$JagsHomeAway, 2)
# bye_repeats <- all_jags_sched[bye_repeated_values, ] |>
#   filter(week >= 3 & game_type == 'REG')
# bye_repeats$has_bye <- c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
# bye_repeats <- bye_repeats |> left_join(select(bye_weeks, season, bye_week), by = 'season')
# bye_repeats <- bye_repeats |>
#   mutate(weeks = ifelse(has_bye == 0, paste0(week - 2, ", ", week - 1, ", ", week),
#                         ifelse(week-1 == bye_week, paste0(week - 2, ", ", week - 1, ", ", "Bye", ", ", week),
#                                ifelse(week-2 == bye_week, paste0(week - 2, ", ", "Bye", ", ", week - 1, ", ", week), "Error"))))
# home_repeats <- bye_repeats |> filter(JagsHomeAway == 'Home') |> select(season, has_bye, weeks)
# away_repeats <- bye_repeats |> filter(JagsHomeAway == 'Away') |> select(season, has_bye, weeks)

### Primetime
primetime <- all_jags_sched |> filter(game_type == 'REG' & as.numeric(substr(gametime, 1, 2)) > 17 & location == 'Home')
tnf <- primetime |> filter(weekday == 'Thursday')
snf <- primetime |> filter(weekday == 'Sunday')
mnf <- primetime |> filter(weekday == 'Monday')



### OLD, uses load_schedule for win totals
# away_win_total <- sched_2023 |>
#   mutate(team = away_team,
#          opponent = home_team) |>
#   left_join(win_totals, by = c('team' = 'Team')) |>
#   rename(team_win_total = 'Season.Win.Total') |>
#   left_join(win_totals, by = c('opponent' = 'Team')) |>
#   rename(opp_win_total = 'Season.Win.Total')
# 
# home_win_total <- sched_2023 |>
#   mutate(team = home_team,
#          opponent = away_team) |>
#   left_join(win_totals, by = c('team' = 'Team')) |>
#   rename(team_win_total = 'Season.Win.Total') |>
#   left_join(win_totals, by = c('opponent' = 'Team')) |>
#   rename(opp_win_total = 'Season.Win.Total')

# win_total_sched <- rbind(away_win_total, home_win_total) |>
#   select(team, week, opponent, team_win_total, opp_win_total) |>
#   group_by(team) |>
#   summarise(team_win_total = mean(team_win_total),
#             avg_opp_win_total = round(mean(opp_win_total),2)) |>
#   arrange(-avg_opp_win_total) |>
#   mutate(rank = rank(-avg_opp_win_total, ties.method = "min")) |>
#   left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
# win_total_tbl <- win_total_sched |>
#   relocate(team_wordmark, rank) |>
#   subset(select = -c(team)) |>
#   arrange(-avg_opp_win_total) |>
#   ungroup() |> gt() |>
#   gt_img_rows(team_wordmark) |>
#   opt_align_table_header("center") |>
#   cols_align("center") |>
#   tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
#   cols_label(team_wordmark = "Team", team_win_total = "Team Win Total", avg_opp_win_total = "Avg Opponent Win Total"
#   ) |> opt_row_striping() |>
#   tab_header(title = "Projected Strength of Schedule, 2023",
#              subtitle = "Based on sportsbook season win totals") |>
#   gt_theme_538() |>
#   data_color(columns = avg_opp_win_total, palette = c("green", "red"))
# gtsave(win_total_tbl, "schedule/NFLSoS2023.png")
# win_total_tbl
# rm(away_win_total, home_win_total)

