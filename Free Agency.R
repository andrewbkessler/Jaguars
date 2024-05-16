contracts_2024 <- load_contracts() |>
  filter(year_signed == "2024" & is_active == "TRUE")

pending_fa <- read.csv("Free Agency/Pending FA.csv")

active_contracts <- load_contracts() |>
  filter(is_active == "TRUE") |>
  left_join(pending_fa |> select(Player, Type), by = c('player' = 'Player'))

Jags_FA_tbl <- contracts_2024 |>
  filter(team == "Jaguars") |>
  group_by(player, position) |>
  summarise(years, value, apy, guaranteed, apy_cap_pct) |>
  arrange(-apy) |> 
  # left_join(headshots, by = c("player" = "full_name")) |>
  ungroup() |> gt() |>
  fmt_percent(columns = apy_cap_pct, decimals = 1) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note(andrew_caption) |>
  cols_label(apy = "Average per Year", apy_cap_pct = "APY Cap Perc") |> opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Offseason Signings",
             subtitle = "Per OTC") |>
  gt_theme_538()
Jags_FA_tbl
gtsave(Jags_FA_tbl, "Free Agency/Jags Signings.png")

nfl_off_weapon <- active_contracts |>
  filter(position %in% c('RB', 'WR', 'TE'), is.na(Type))
nfl_off_weapon$team <- str_replace_all(nfl_off_weapon$team, c('CAR/CHI' = 'CHI', 'CLE/DAL' = 'CLE', 'DAL/CLE' = 'CLE', 'CAR/PIT' = 'CAR', 'NYG/LV' = 'NYG', 'SF/CAR' = 'SF',
                                                              'CIN/HOU' = 'HOU', 'HOU/CIN' = 'HOU','CLE/DEN'= 'CLE', 'DEN/CLE' = 'CLE', 'NYG/KC' = 'KC', 'SEA/DEN' = 'SEA',
                                                              'ARI/ATL' = 'ATL','ATL/JAX' = 'JAX', 'NYJ/CLE' = 'CLE', 'CLE/NYJ' = 'CLE','LAR/MIA' = 'LAR', 'LAC/CHI' = 'CHI'))
nfl_off_weapon <- nfl_off_weapon |>
  mutate(team_abbr = team)
nfl_off_weapon$team_abbr <- str_replace_all(nfl_off_weapon$team_abbr, c('49ers' = 'SF', 'Bears' = 'CHI', 'Bengals' = 'CIN', 'Bills' = 'BUF', 'Broncos' = 'DEN',
                                                    'Browns' = 'CLE', 'Buccaneers' = 'TB', 'SDG' = 'LAC', 'Cardinals' = 'ARI','Chargers' = 'LAC', 'Chiefs' = 'KC',
                                                    'Colts' = 'IND', 'Commanders' = 'WAS', 'Cowboys' = 'DAL', 'Dolphins' = 'MIA', 'Eagles' = 'PHI',
                                                    'Falcons' = 'ATL', 'Giants' = 'NYG', 'Jaguars' = 'JAX', 'Jets' = 'NYJ','Lions' = 'DET', 'Packers' = 'GB',
                                                    'Panthers' = 'CAR', 'Patriots' = 'NE', 'Raiders' = 'LV', 'Rams' = 'LAR', 'Ravens' = 'BAL',
                                                    'Steelers' = 'PIT', 'Saints' = 'NO', 'Seahawks' = 'SEA', 'Texans' = 'HOU','Titans' = 'TEN', 'Vikings' = 'MIN'))
nfl_off_weapon <- nfl_off_weapon |>
  arrange(-apy) |>
    mutate(team_abbr = ifelse(grepl("/", team_abbr), sub("\\/.*", "", team_abbr), team_abbr)) |>
  group_by(team_abbr) |>
  mutate(rank = rank(-apy, ties.method = "min")) |>
  slice(1:5)
ggplot() +
  geom_col(data=nfl_off_weapon, mapping=aes(x = reorder(team_abbr, apy), y = apy, group = position, fill = position), colour="black") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Team",
       y = "Average per Year (Millions)",
       title = "Offensive Weapon Spend, 2024",
       subtitle = "Top 5 APY contributors per team",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave('Free Agency/Offensive Weapon Spend.png',  width = 14, height = 10, dpi = "retina")

nfl_off_wpn_disc <- nfl_off_weapon |>
  group_by(team_abbr) |>
  summarise(pos_comb = paste0(position, collapse = ", "),
            total_apy = round(sum(apy),1),
            total_cap_pct = sum(apy_cap_pct),
            max_apy = round(max(apy),1),
            apy_var = round(var(rep(apy,5)),1)) |>
  arrange(-total_apy) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by='team_abbr')
nfl_off_wpn_tbl <- nfl_off_wpn_disc |>
  relocate(team_wordmark) |>
  subset(select = -c(team_abbr)) |>
  ungroup() |> gt() |>
  fmt_percent(columns = total_cap_pct, decimals = 1) |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", pos_comb = "Position Combination", total_apy = "Total APY ($M)", total_cap_pct = "Total Cap %",
             max_apy = "Max player APY ($M)", apy_var = "APY Variance") |> 
  opt_row_striping() |>
  tab_header(title = "Offensive Weapons Spend Discrepancies",
             subtitle = "Top 5 by APY") |>
  gt_theme_538() |>
  data_color(columns = max_apy, palette = c("green", "red")) |>
  data_color(columns = apy_var, palette = c("green", "red")) #|>
nfl_off_wpn_tbl
gtsave(nfl_off_wpn_tbl, "Free Agency/Offense Weapon Discrepancy.png")









