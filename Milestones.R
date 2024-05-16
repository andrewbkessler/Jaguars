offense_all <- load_player_stats(seasons = 1999:2023, stat_type = "offense") 
def_2023 <- load_player_stats(season = 2023, stat_type = "defense") |> filter(team == 'JAX')

### Trevor Lawrence
trev_data <- offense_all |> filter(player_id == '00-0036971') |>
  mutate(comp_pct = completions/attempts, yards_per_att = passing_yards/attempts, yards_per_comp = passing_yards/completions,
         total_tds = passing_tds + rushing_tds, fumbles = sack_fumbles_lost + rushing_fumbles_lost,
         turnovers = interceptions + sack_fumbles_lost + rushing_fumbles_lost)
trev_full_agg <- trev_data |> group_by(player_display_name) |>
  summarise(num_games = n(), comps_max = max(completions), comps_min = min(completions), atts_max = max(attempts), atts_min = min(attempts),
            comp_pct_max = max(comp_pct), comp_pct_min = min(comp_pct), yards_max = max(passing_yards), yards_min = min(passing_yards),
            ypa_max = max(yards_per_att), ypa_min = min(yards_per_att), ypc_max = max(yards_per_comp), ypc_min = min(yards_per_comp),
            pass_tds_max = max(passing_tds), pass_tds_min = min(passing_tds), tot_tds_max = max(total_tds), tot_tds_min = min(total_tds),
            int_max = max(interceptions), int_min = min(interceptions), sacks_max = max(sacks), sacks_min = min(sacks),
            fumb_max = max(fumbles), fumb_min = min(fumbles), turn_max = max(turnovers), turn_min = min(turnovers)) |>
  transpose(keep.names = "Metrics")



