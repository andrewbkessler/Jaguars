sched_2023 <- load_schedules(2023) |>
  filter(week <= 18)
sched_2024 <- load_schedules(2024) |>
  filter(week <= 18)
stadiums <- read.csv("schedule/stadiums.csv", header = TRUE) |>
  filter(League == "NFL")
win_totals <- read.csv("schedule/Season Win Totals.csv", header = TRUE)
# bet_lines <- read.csv("schedule/Bet Lines.csv", header = TRUE)

Jags_sched <- sched_2023 |>
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
  filter(isJags == 1)
JagsSchedTbl <- Jags_sched |>
  select(week, team_wordmark, JagsHomeAway, RestDifferential) |>
  ungroup() |> arrange(week) |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Opponent", JagsHomeAway = "Home/Away", RestDifferential = "Rest Disparity") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars Regular Season Schedule, 2023") |>
  gt_theme_538() |>
  tab_style(style = cell_fill(color = "red"), locations = cells_body(columns = RestDifferential, rows = RestDifferential < 0)) |>
  tab_style(style = cell_fill(color = "green"), locations = cells_body(columns = RestDifferential, rows = RestDifferential > 0))
# JagsSchedTbl
gtsave(JagsSchedTbl, "schedule/JagsSched2023.png")

away_rest <- sched_2023 |>
  mutate(team = away_team,
         RestDifferential = as.numeric(away_rest) - as.numeric(home_rest)) |>
  rename(
    team_rest = away_rest,
    opponent_rest = home_rest)

home_rest <- sched_2023 |>
  mutate(team = home_team,
         RestDifferential = as.numeric(home_rest) - as.numeric(away_rest)) |>
  rename(
    team_rest = home_rest,
    opponent_rest = away_rest)

nfl_rest <- rbind(away_rest, home_rest) |>
  mutate(pos_neg = ifelse(RestDifferential > 0, 1, ifelse(RestDifferential < 0, 0, ""))) |>
  arrange(abs(RestDifferential))
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
ggsave('schedule/Rest Disparity Chart.png',  width = 14, height = 10, dpi = "retina")

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
  tab_header(title = "NFL Regular Season Rest Disparity, 2023") |>
  gt_theme_538() |>
  data_color(columns = RestDisparity, palette = c("red", "green")) |>
  data_color(columns = neg_games, palette = c("green", "red")) |>
  data_color(columns = pos_games, palette = c("red", "green"))
nfl_rest_tbl
gtsave(nfl_rest_tbl, "schedule/NFLRestDisparity2023.png")
rm(away_rest, home_rest)

away_win_total <- sched_2023 |>
  mutate(team = away_team,
         opponent = home_team) |>
  left_join(win_totals, by = c('team' = 'Team')) |>
  rename(team_win_total = 'Season.Win.Total') |>
  left_join(win_totals, by = c('opponent' = 'Team')) |>
  rename(opp_win_total = 'Season.Win.Total')

home_win_total <- sched_2023 |>
  mutate(team = home_team,
         opponent = away_team) |>
  left_join(win_totals, by = c('team' = 'Team')) |>
  rename(team_win_total = 'Season.Win.Total') |>
  left_join(win_totals, by = c('opponent' = 'Team')) |>
  rename(opp_win_total = 'Season.Win.Total')

win_total_sched <- rbind(away_win_total, home_win_total) |>
  select(team, week, opponent, team_win_total, opp_win_total) |>
  group_by(team) |>
  summarise(team_win_total = mean(team_win_total),
            avg_opp_win_total = round(mean(opp_win_total),2)) |>
  arrange(-avg_opp_win_total) |>
  mutate(rank = rank(-avg_opp_win_total, ties.method = "min")) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
win_total_tbl <- win_total_sched |>
  relocate(team_wordmark, rank) |>
  subset(select = -c(team)) |>
  arrange(-avg_opp_win_total) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", team_win_total = "Team Win Total", avg_opp_win_total = "Avg Opponent Win Total"
  ) |> opt_row_striping() |>
  tab_header(title = "Projected Strength of Schedule, 2023",
             subtitle = "Based on sportsbook season win totals") |>
  gt_theme_538() |>
  data_color(columns = avg_opp_win_total, palette = c("green", "red"))
gtsave(win_total_tbl, "schedule/NFLSoS2023.png")
win_total_tbl
rm(away_win_total, home_win_total)

away_bet_odds <- sched_2023 |>
  mutate(team = away_team, opponent = home_team, team_spread = spread_line, team_favored = ifelse(team_spread < 0, 1, 0))

home_bet_odds <- sched_2023 |>
  mutate(team = home_team, opponent = away_team, team_spread = -1*spread_line, team_favored = ifelse(team_spread < 0, 1, 0))

sched_bet_lines <- rbind(away_bet_odds, home_bet_odds) |>
  select(team, week, opponent, team_spread, team_favored) |>
  left_join(win_totals, by = c('team' = 'Team')) |>
  left_join(teams_colors_logos |> select(team_abbr, team_logo_espn), by = c('team' = 'team_abbr'))

win_tot_v_spread <- sched_bet_lines |>
  group_by(team, team_logo_espn) |>
  summarise(favored = sum(team_favored), win_total = mean(Season.Win.Total))
ggplot(data=win_tot_v_spread, mapping = aes(x=favored, y=win_total)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_abline(slope=1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = lm, se=FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
  xlim(0, 17) + ylim(0,17) +
  theme_bw() +
  labs(x = "Games favored in",
       y = "Season Win Total",
       title = "NFL Season Win Totals v Games Favored In",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('Schedule/Win Total v Favored.png', width = 14, height = 10, dpi = "retina")
rm(away_bet_odds, home_bet_odds)







