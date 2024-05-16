schedules <- load_schedules(2000:2023) |>
  filter(game_type == "REG", !is.na(home_rest), !is.na(away_rest), !is.na(result), season != "2020")

away_schedule <- schedules |>
  mutate(team = away_team,
         rest_disparity = as.numeric(away_rest) - as.numeric(home_rest),
         win = ifelse(result < 0, 1, ifelse(result > 0, 0, 0.5)),
         loss = ifelse(result > 0, 1, ifelse(result < 0, 0, 0.5))
         ) |>
  rename(
    team_rest = away_rest,
    opponent_rest = home_rest)

home_schedule <- schedules |>
  mutate(team = home_team,
         rest_disparity = as.numeric(home_rest) - as.numeric(away_rest),
         win = ifelse(result > 0, 1, ifelse(result < 0, 0, 0.5)),
         loss = ifelse(result < 0, 1, ifelse(result > 0, 0, 0.5))
         ) |>
  rename(
    team_rest = home_rest,
    opponent_rest = away_rest)

results <- rbind(away_schedule, home_schedule)

result_by_rest <- results |>
  group_by(rest_disparity) |>
  summarise(num_games = n(),
            win_pct = 100*mean(win)) |>
  filter(num_games >= 25)
ggplot(data=result_by_rest, aes(x=rest_disparity, y=win_pct)) +
  geom_line() +
  geom_hline(yintercept = 50, linetype = "dashed") +
  theme_bw() +
  labs(x = "Game Rest Disparity",
       y = "Winning Percentage (%)",
       title = "Game Win Percentage by Rest Disparity",
       subtitle = "Min. 25 games, Seasons: 2000-2023 (excl. 2020)") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Schedule/Game Win Pct by Rest.png', width = 14, height = 10, dpi = "retina")

record_by_rest <- results |>
  group_by(season, team) |>
  summarise(rest_disparity = sum(rest_disparity),
            wins = sum(win),
            losses = sum(loss),
            num_games = n()) |>
  group_by(rest_disparity) |>
  summarise(wins = sum(wins),
            losses = sum(losses),
            num_seasons = n(),
            win_pct = 100*(wins/(wins+losses))) |>
  filter(num_seasons >= 10)
ggplot(data=record_by_rest, aes(x=rest_disparity, y=win_pct)) +
  geom_point() +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_smooth(method='lm', se = FALSE, color = "black", linewidth = 0.5) +
  theme_bw() +
  labs(x = "Season-long Rest Disparity",
       y = "Average Winning Percentage (%)",
       title = "Avg Win Pct by Season-long Rest Disparity",
       subtitle = "Min. 10 seasons, Seasons: 2000-2023 (excl. 2020)") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Schedule/Season Win Pct by Rest.png', width = 14, height = 10, dpi = "retina")

# rm(away_schedule)
# rm(home_schedule)







