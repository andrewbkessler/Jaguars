combine_2023 <- load_combine(2023)
draft_2023 <- load_draft_picks(2023)
draft_2023$college <- str_replace_all(draft_2023$college, c('St[.]' = 'State', 'Miami [(]FL[)]' = 'Miami', 'North Carolina State' = 'NC State',
                                                          '(?<!Southern )Mississippi(?! St.)' = 'Ole Miss', 'Col[.]' = 'College',
                                                          'Central Florida' = 'UCF', 'Southern Miss(?!i)' = 'Southern Mississippi', 'Hawaii' = 'Hawai\'i'))
trades <- load_trades() |>
  mutate(isJags = ifelse(gave == "JAX" | received == "JAX", 1, 0))
cfb_teams <- load_cfb_teams()
team_info <- load_teams(current = TRUE)
draft_value <- read_csv("Draft/Draft Chart.csv")
draft_2024 <- read_csv("Draft/2024 Draft Picks.csv") |>
  left_join(select(team_info, team_abbr, team_name), by = c('Full team' = 'team_name')) |>
  left_join(draft_value, by = 'Pick')

jags_trade <- trades |>
  filter(isJags == 1)

# Jags_2023 <- draft_2023 |>
#   filter(team == "JAX") |>
#   left_join(combine_2023 |> select(cfb_id, ht, wt, forty, bench, vertical, broad_jump, cone, shuttle),
#             by = c('cfb_player_id' = 'cfb_id')) |>
#   left_join(cfb_teams |> select(school, mascot, logo, logo_2), by = c('college' = 'school'))

jags_2023_trades <- jags_trade |>
  filter(season == 2023) |>
  subset(!is.na(pick_number)) |>
  rbind(list(1765,2023,as.Date('2023-04-29'),'NO','JAX',2024,4,117,0,'','',1)) |>
  mutate(jax_pos = ifelse(gave == "JAX", "gave", "received"),
         jax = "JAX",
         opp_team = ifelse(jax_pos == "gave", received, gave)) |>
  left_join(draft_value, by = c('pick_number' = 'Pick')) |>
  mutate(jax_value = ifelse(received == "JAX", Value, 0),
         opp_value = ifelse(received != "JAX", Value, 0),
         net_value = jax_value - opp_value) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('opp_team' = 'team_abbr')) |>
  group_by(trade_id, opp_team, team_wordmark) |>
  summarise(jax_value = sum(jax_value),
            opp_value = sum(opp_value),
            net_value = sum(net_value))
jags_2023_trades_tbl <- jags_2023_trades |>
  relocate(team_wordmark) |>
  subset(select = -c(trade_id, opp_team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  grand_summary_rows(columns = c(jax_value, opp_value, net_value), fns = list(Total ~ sum(.))) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Trade Partner", jax_value = "Jags Value Gained", opp_value = "Jags Value Lost", net_value = "Next Value Gained") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars 2023 Draft-day Trades",
             subtitle = "Value from fitzgerald-spielberger draft chart") |>
  gt_theme_538()
jags_2023_trades_tbl
gtsave(jags_2023_trades_tbl, "Draft/Jags2023Trades.png")

jags_2024_trans <- draft_2024 |>
  filter((team_abbr == "JAX" & (Compensatory == 1 | !is.na(Notes))) | Notes == "via JAX") |>
  mutate(new_note = ifelse(!is.na(Notes), Notes, ifelse(Compensatory == 1, "Comp Pick", "")),
         jax_value = ifelse(team_abbr == "JAX", Value, 0),
         opp_value = ifelse(team_abbr != "JAX", Value, 0),
         net_value = jax_value - opp_value) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = 'team_abbr') |>
  group_by(Pick, team_wordmark, new_note) |>
  summarise(jax_value = sum(jax_value), opp_value = sum(opp_value), net_value = sum(net_value))
jags_2024_trans_tbl <- jags_2024_trans |>
  relocate(team_wordmark) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  grand_summary_rows(columns = c(jax_value, opp_value, net_value), fns = list(Total ~ sum(.))) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", new_note = "Pick Via",
             jax_value = "Value Gained", opp_value = "Value Lost", net_value = "Net Value") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Draft Pick Adds/Losses",
             subtitle = "Value from fitzgerald-spielberger draft chart") |>
  gt_theme_538()
jags_2024_trans_tbl
gtsave(jags_2024_trans_tbl, "Draft/Jags2024Transactions.png")




