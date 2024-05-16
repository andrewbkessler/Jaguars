# Let's just get runs and passes
pbp_rp <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  # and get plays that have epa
  filter(!is.na(epa))

# Who was the Jaguars best rusher last season?
jags_rush_epa <- pbp_rp |> 
  filter(posteam == "JAX", rush == 1, !is.na(rusher_player_name)) |> 
  group_by(rusher_player_name) |> 
  summarize(rushes = n(),
            epa_rush = mean(epa)) |> 
  filter(rushes >= 10) |> 
  arrange(-epa_rush) |>
  rename("rusher" = "rusher_player_name")
cat(format(as_tibble(jags_rush_epa))[-3L], sep = "\n")

ngs_pass_avg <- ngs_pass |>
  filter(week != 0, season_type == "REG") |>
  mutate(total_TtT = avg_time_to_throw*attempts,
         total_DOT = avg_intended_air_yards*attempts,
         total_CPOE = completion_percentage_above_expectation*attempts) |>
  group_by(season) |>
  summarize(total_attempts = sum(attempts),
            total_TtT = sum(total_TtT),
            total_DOT = sum(total_DOT),
            total_CPOE = sum(total_CPOE),
            avg_TtT = total_TtT/total_attempts,
            avg_DOT = total_DOT/total_attempts,
            avg_CPOE = 100*(total_CPOE/total_attempts))

ngs_Trev <- ngs_pass |>
  filter(player_display_name == "Trevor Lawrence", week != 0) |>
  left_join(ngs_pass_avg, by = "season")

Trev_ATtT_v_aDOT <- ggplot(ngs_Trev, aes(week)) +
  geom_line(aes(y = avg_time_to_throw), color = gold) +
  geom_line(aes(y = avg_intended_air_yards/4), color = teal) +
  geom_line(aes(y = avg_TtT), color = gold, linetype = "dashed") +
  geom_line(aes(y = avg_DOT/4), color = teal, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1,18,1)) + 
  scale_y_continuous(sec.axis = sec_axis(~.*4, name="Average Depth of Target (yards)")) +
  theme_bw() +
  labs(x = "Week",
       y = "Average Time to Throw (s)",
       title = "Trevor Lawrence ATtT v aDOT, 2023",
       subtitle = "Dashed lines = NFL Average (Regular Season only)",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
Trev_ATtT_v_aDOT
ggsave('Offense/Trevor Lawrence ATtT v aDOT.png', width = 14, height = 10, dpi = "retina")

rushers <- ftn |>
  left_join(select(pbp,play_id,play_type,game_id,posteam,defteam,sack,down,qtr),
            by=c('nflverse_game_id' = 'game_id', 'nflverse_play_id' = 'play_id'))

off_rushers <- rushers |>
  filter(play_type == "pass", week > 0, week <= 18, !is.na(n_pass_rushers)) |>
  group_by(posteam) |>
  summarise(num_pass = n(),
            avg_rush = mean(n_pass_rushers),
            num_sacks = sum(sack)) |>
  mutate(sack_rate = 100*(num_sacks/num_pass)) |>
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
off_rushers |> 
  ggplot(aes(x = avg_rush, y = sack_rate)) +
  geom_hline(yintercept = mean(off_rushers$sack_rate), linetype = "dashed") +
  geom_vline(xintercept = mean(off_rushers$avg_rush), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "Avg Num Rushers Faced",
       y = "Sack Rate (%)",
       title = "Off Sack Rate by Num Rushers in 2023",
       subtitle = "Regular season only",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle
ggsave('Offense/off sacks by num rushers.png',  width = 14, height = 10, dpi = "retina")

### NFL Average Off Sack Rate
nfl_pbp <- load_pbp(2022:2023) |>
  filter(week > 0)
nfl_ftn <- load_ftn_charting(2022:2023) |>
  filter(week > 0)
nfl_off_avg <- nfl_ftn |>
  left_join(select(nfl_pbp,play_id,play_type,game_id,posteam,defteam,sack,down,qtr),
            by=c('nflverse_game_id' = 'game_id', 'nflverse_play_id' = 'play_id')) |>
  filter(play_type == "pass", !is.na(n_pass_rushers)) |>
  group_by(play_type) |>
  summarise(num_pass = n(),
            avg_rush = mean(n_pass_rushers),
            num_sacks = sum(sack)) |>
  mutate(sack_rate = 100*(num_sacks/num_pass))
rm(nfl_pbp)
rm(nfl_ftn)

### Jaguars Off Sack Rate by Rushers per Year
jags_off_pbp <- load_pbp(2022:2023) |>
  filter(week > 0, posteam == "JAX")
jags_off_ftn <- load_ftn_charting(2022:2023) |>
  filter(week > 0)
jags_off_rusher <- jags_off_ftn |>
  left_join(select(jags_off_pbp,play_id,play_type,game_id,posteam,defteam,sack,down,qtr),
            by=c('nflverse_game_id' = 'game_id', 'nflverse_play_id' = 'play_id')) |>
  filter(posteam == "JAX", play_type == "pass", !is.na(n_pass_rushers)) |>
  group_by(season, posteam) |>
  summarise(num_pass = n(),
            avg_rush = mean(n_pass_rushers),
            num_sacks = sum(sack)) |>
  mutate(sack_rate = 100*(num_sacks/num_pass)) |>
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
rm(jags_off_pbp)
rm(jags_off_ftn)
jags_off_rusher |> 
  ggplot(aes(x = avg_rush, y = sack_rate)) +
  geom_hline(yintercept = mean(nfl_off_avg$sack_rate), linetype = "dashed") +
  geom_vline(xintercept = mean(nfl_off_avg$avg_rush), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_text(label = jags_off_rusher$season, nudge_x = .015) +
  theme_bw() +
  labs(x = "Avg Num Rushers Faced",
       y = "Sack Rate (%)",
       title = "Jags Off Sack Rate by Num Rushers",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) #put the title in the middle
ggsave('Offense/Jags Y-Y off sacks by num rushers.png',  width = 14, height = 10, dpi = "retina")

### Jags TE tendencies
pbp_Jags_TE <- load_participation(2023, include_pbp = TRUE) |>
  filter(posteam == 'JAX', !is.na(offense_formation))







