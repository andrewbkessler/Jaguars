pbp_corr <- load_pbp(2020:2023)

pass_pbp <- pbp_corr |>
  filter(play_type == "pass", sack == 0, week !=0) |>
  mutate(
    home_win = if_else(condition = result > 0, true = "1", false = "0"),
    away_win = if_else(condition = result < 0, true = "1", false = "0"),
    posteam_win = if_else(condition = posteam_type == "home", true = home_win, false = away_win))

aDOT_WP <- pass_pbp |>
  filter(!is.na(air_yards)) |>
  group_by(posteam_win) |>
  summarize(aDOT = mean(air_yards)) |>
  rename("Win_Loss" = "posteam_win")
ggplot(data=aDOT_WP, aes(x=Win_Loss, y=aDOT, fill=Win_Loss)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values=c("red","green"))+
  geom_text(aes(label = round(aDOT,2)), vjust = 1.5, color = "black") +
  labs(x = "Win/Loss",
      y="Average Depth of Target (yards)",
      title = "aDOT by Game Result") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('aDOT by Game Result.png', width = 14, height = 10, dpi = "retina")

aDOT_wp_2 <- pass_pbp |>
  filter(!is.na(air_yards)) |>
  group_by(game_id, posteam) |>
  summarize(num_att = n(), aDOT = round(mean(air_yards),0), win_loss = mean(as.integer(posteam_win))) |>
  group_by(aDOT) |>
  summarize(num_games = n(), avg_wp = 100*mean(win_loss)) |>
  filter(num_games >=50)
ggplot(data=aDOT_wp_2, aes(aDOT)) +
  geom_line(aes(y=avg_wp)) +
  scale_x_continuous(breaks = seq(1,11,1)) +
  theme_bw() +
  labs(x = "Average Depth of Target (yards)",
       y = "Average Winning Percentage (%)",
       title = "Game Win Percentage by aDOT",
       subtitle = "Min. 50 Games at that aDOT") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Game Win Percentage by aDOT.png', width = 14, height = 10, dpi = "retina")

aDOT_wp_3 <- pass_pbp |>
  filter(!is.na(air_yards), !is.na(wpa)) |>
  group_by(air_yards) |>
  summarize(num_att = n(), wp_gained = 100*mean(wpa)) |>
  filter(num_att >=75)
ggplot(data=aDOT_wp_3, aes(air_yards)) +
  geom_line(aes(y=wp_gained)) +
  scale_x_continuous(breaks = seq(-10,65,5)) +
  theme_bw() +
  labs(x = "Depth of Target (yards)",
       y = "Winning Percentage Gained (%)",
       title = "Winning Percentage Gained by DOT",
       subtitle = "Min. 200 Attempts at that aDOT") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Winning Percentage Gained by DOT.png', width = 14, height = 10, dpi = "retina")

temp_points <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, !is.na(temp))
lmTemp = lm(total ~ temp, data = temp_points)
summary(lmTemp)

wind_points <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, !is.na(wind))
lmWind = lm(total ~ wind, data = wind_points)
summary(lmWind)

surface_points <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, !is.na(surface)) |>
  group_by(surface) |>
  summarize(num_games = n(), avg_points = mean(total))
ggplot(data=surface_points, aes(x=reorder(surface, -avg_points), y=avg_points)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_points,1)), vjust = 1.5, color = "white") +
  labs(x = "Surface Type",
       y="Average Total Points",
       title = "Average Total Points by Surface") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('Avg Pts by Surface.png', width = 14, height = 10, dpi = "retina")

roof_points <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, !is.na(roof)) |>
  group_by(roof) |>
  summarize(num_games = n(), avg_points = mean(total))
ggplot(data=roof_points, aes(x=reorder(roof, -avg_points), y=avg_points)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_points,1)), vjust = 1.5, color = "white") +
  labs(x = "Roof Type",
       y="Average Total Points",
       title = "Average Total Points by Roof") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('Avg Pts by Roof', width = 14, height = 10, dpi = "retina")

div_points <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, !is.na(div_game)) |>
  group_by(div_game) |>
  summarize(num_games = n(), avg_points = mean(total))
ggplot(data=div_points, aes(x=reorder(div_game, -avg_points), y=avg_points)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_points,1)), vjust = 1.5, color = "white") +
  labs(x = "Division Game",
       y="Average Total Points",
       title = "Average Total Points for Div/Non-Div Games") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ggsave('Avg Pts by Div or non-Div.png', width = 14, height = 10, dpi = "retina")

div_points_2 <- pbp_corr |>
  filter(play_type_nfl == "END_GAME", week != 0, div_game == 1, season_type == "REG") |>
  mutate(
    home_win = if_else(condition = result > 0, true = "1", false = "0"),
    away_win = if_else(condition = result < 0, true = "1", false = "0")) |>
  left_join(team_info, by=c("home_team" = "team_abbr")) |>
  group_by(team_division) |>
  summarize(num_games = n(), avg_points = mean(total), home_wp = mean(as.integer(home_win)))
ggplot(data=div_points_2, aes(x=reorder(team_division, -avg_points), y=avg_points)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_points,1)), vjust = 1.5, color = "white") +
  theme(axis.text.x = element_text(angle=45)) +
  labs(x = "Division",
       y="Average Total Points",
       title = "Average Total Points by Divsion Matchup") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ggsave('Avg Pts by Div Matchup.png', width = 14, height = 10, dpi = "retina")
ggplot(data=div_points_2, aes(x=reorder(team_division, -home_wp), y=home_wp)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(home_wp,0)), vjust = 1.5, color = "white") +
  theme(axis.text.x = element_text(angle=45)) +
  labs(x = "Division",
       y="Home Team Winning Pct",
       title = "Homefield Advantage by Divsion Matchup") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ggsave('Home WP by Div Matchup.png', width = 14, height = 10, dpi = "retina")

st_pbp <- pbp_corr |>
  filter(play_type == "punt" | play_type == "field_goal" | play_type == "extra_point" | play_type == "kickoff", week !=0) |>
  mutate(
    home_win = if_else(condition = result > 0, true = "1", false = "0"),
    away_win = if_else(condition = result < 0, true = "1", false = "0"),
    posteam_win = if_else(condition = posteam_type == "home", true = home_win, false = away_win),
    fg_made = if_else(condition = field_goal_result == "made", true = "1", false = "0"),
    fg_missed = if_else(condition = field_goal_result == "missed" | field_goal_result == "blocked", true = "1", false = "0"))

missed_fg <- st_pbp |>
  filter(play_type == "field_goal", season_type == "REG") |>
  group_by(game_id, posteam) |>
  summarize(fg_pct = 100*mean(as.integer(fg_made)), win = mean(as.integer(posteam_win))) |>
  group_by(fg_pct) |>
  summarize(num_games = n(), win_pct = 100*mean(as.integer(win))) |>
  filter(num_games >=20)
ggplot(data=missed_fg, aes(x=fg_pct, y=win_pct)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45)) +
  labs(x = "Field Goal Percentage",
       y="Winning Percentage",
       title = "Winning Pct by Field Goal Pct") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ggsave('Winning Pct by Field Goal Pct.png', width = 14, height = 10, dpi = "retina")

missed_fg_2023 <- st_pbp |>
  filter(season == "2023", season_type == "REG", play_type == "field_goal") |>
  group_by(game_id, posteam) |>
  summarize(fg_pct = 100*mean(as.integer(fg_made)), win = mean(as.integer(posteam_win))) |>
  mutate(did_miss = if_else(condition = fg_pct <100, true = "1", false = "0")) |>
  group_by(posteam, did_miss) |>
  summarize(num_games = n(),win_pct = 100*mean(as.integer(win)))

punt_dist <- st_pbp |>
  filter(season_type == "REG", play_type == "punt", !is.na(kick_distance), kick_distance>0) |>
  group_by(kick_distance) |>
  summarize(num_plays = n(), avg_wpa = 100*mean(wpa)) |>
  filter(num_plays >= 20)
ggplot(data=punt_dist, aes(kick_distance)) +
  geom_line(aes(y=avg_wpa)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(25,67,5)) +
  theme_bw() +
  labs(x = "Punt Distance (yards)",
       y = "Average Winning Percentage Added (%)",
       title = "Win Pct by Punt Yards",
       subtitle = "Min. 20 punts at that distance") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Win Pct by Punt Yards.png', width = 14, height = 10, dpi = "retina")

punt_inside20 <- st_pbp |>
  filter(season_type == "REG", play_type == "punt", !is.na(punt_inside_twenty)) |>
  group_by(as.character(punt_inside_twenty)) |>
  summarize(num_plays = n(), avg_wpa = 100*mean(wpa)) |>
  rename("punt_inside_twenty" = "as.character(punt_inside_twenty)")
ggplot(data=punt_inside20, aes(x=punt_inside_twenty, y=avg_wpa, fill=punt_inside_twenty)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values=c("red","green"))+
  geom_text(aes(label = round(avg_wpa,2)), vjust = 1.5, color = "black") +
  labs(x = "Punt inside 20",
       y="Win Pct Added (%)",
       title = "Win Pct added by Punt inside 20") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ggsave('Win Pct added by Punt inside 20.png', width = 14, height = 10, dpi = "retina")

home_pt_margin <- schedules |>
  filter(game_type == "REG") |>
  mutate(home_win = if_else(condition = result > 0, true ="1", false ="0"),
         home_loss = if_else(condition = result < 0, true = "1", false = "0")) |>
  group_by(home_team) |>
  summarize(home_points_scored = sum(home_score), home_points_conceded = sum(away_score),
            home_wins = sum(as.integer(home_win)), home_losses = sum(as.integer(home_loss))) |>
  rename("team" = "home_team")
away_pt_margin <- schedules |>
  filter(game_type == "REG") |>
  mutate(away_win = if_else(condition = result < 0, true ="1", false ="0"),
         away_loss = if_else(condition = result > 0, true = "1", false = "0")) |>
  group_by(away_team) |>
  summarize(away_points_scored = sum(away_score), away_points_conceded = sum(home_score),
            away_wins = sum(as.integer(away_win)), away_losses = sum(as.integer(away_loss))) |>
  rename("team" = "away_team")
pt_margin <- home_pt_margin |>
  left_join(away_pt_margin, by="team") |>
  mutate(points_scored = home_points_scored + away_points_scored,
         points_conceded = home_points_conceded + away_points_conceded,
         wins = home_wins + away_wins,
         losses = home_losses + away_losses,
         point_margin = points_scored - points_conceded,
         win_pct = 100*(wins/17)) |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))
pt_margin |> 
  ggplot(aes(x = point_margin, y = win_pct)) +
  geom_smooth(method=lm, se=FALSE, color="black") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "Point Margin",
       y = "Win Percentage",
       title = "Win Pct v Point Margin",
       subtitle = "Regular season, 2023") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle
ggsave('Win Pct v Point Margin.png', width = 14, height = 10, dpi = "retina")








