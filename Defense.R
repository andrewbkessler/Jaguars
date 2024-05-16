rushers <- ftn |>
  left_join(select(pbp,play_id,play_type,game_id,posteam,defteam,sack,down,qtr),
            by=c('nflverse_game_id' = 'game_id', 'nflverse_play_id' = 'play_id'))

def_rushers <- rushers |>
  filter(play_type == "pass", week > 0, week <= 18, !is.na(n_pass_rushers)) |>
  group_by(defteam) |>
  summarise(num_pass = n(),
            avg_rush = mean(n_pass_rushers),
            num_sacks = sum(sack)) |>
  mutate(sack_rate = 100*(num_sacks/num_pass)) |>
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))
def_rushers |> 
  ggplot(aes(x = avg_rush, y = sack_rate)) +
  geom_hline(yintercept = mean(def_rushers$sack_rate), linetype = "dashed") +
  geom_vline(xintercept = mean(def_rushers$avg_rush), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "Avg Num Rushers",
       y = "Sack Rate (%)",
       title = "Def Sack Rate by Num Rushers in 2023",
       subtitle = "Regular season only",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle
ggsave('Defense/def sacks by num rushers.png',  width = 14, height = 10, dpi = "retina")

### NFL Average Def Sack Rate
nfl_pbp <- load_pbp(2022:2023) |>
  filter(week > 0)
nfl_ftn <- load_ftn_charting(2022:2023) |>
  filter(week > 0)
nfl_def_avg <- nfl_ftn |>
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

### Jaguars Def Sack Rate by Rushers per Year
jags_def_pbp <- load_pbp(2022:2023) |>
  filter(week > 0, defteam == "JAX")
jags_def_ftn <- load_ftn_charting(2022:2023) |>
  filter(week > 0)
jags_def_rusher <- jags_def_ftn |>
  left_join(select(jags_def_pbp,play_id,play_type,game_id,posteam,defteam,sack,down,qtr),
            by=c('nflverse_game_id' = 'game_id', 'nflverse_play_id' = 'play_id')) |>
  filter(defteam == "JAX", play_type == "pass", !is.na(n_pass_rushers)) |>
  group_by(season, defteam) |>
  summarise(num_pass = n(),
            avg_rush = mean(n_pass_rushers),
            num_sacks = sum(sack)) |>
  mutate(sack_rate = 100*(num_sacks/num_pass)) |>
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))
rm(jags_def_pbp)
rm(jags_def_ftn)
jags_def_rusher |> 
  ggplot(aes(x = avg_rush, y = sack_rate)) +
  geom_hline(yintercept = mean(nfl_def_avg$sack_rate), linetype = "dashed") +
  geom_vline(xintercept = mean(nfl_def_avg$avg_rush), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_text(label = jags_def_rusher$season, nudge_x = .025) +
  theme_bw() +
  labs(x = "Avg Num Rushers",
       y = "Sack Rate (%)",
       title = "Jags Def Sack Rate by Num Rushers",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) #put the title in the middle
ggsave('Defense/Jags Y-Y def sacks by num rushers.png',  width = 14, height = 10, dpi = "retina")





# write.csv(rushers, "Book1.csv", col.names = TRUE, row.names = FALSE)