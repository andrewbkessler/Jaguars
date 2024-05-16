### Base info
team_info <- load_teams(current = TRUE)
draft_value <- read_csv("Draft/Draft Chart.csv")
### Load Draft Picks
draft_2024 <- load_draft_picks(seasons = TRUE) |>
  filter(season == 2024)
### Filter on Jags picks
jags_draft_2024 <- draft_2024 |>
  filter(team == "JAX")
### Load Trades
trade_sheet <- read.csv("Draft/Draft Trades.csv")
trade_sheet$trade_date <- as.Date(trade_sheet$trade_date, format = "%Y-%m-%d")
trades <- load_trades() |>
  rbind(trade_sheet) |>
  mutate(isJags = ifelse(gave == "JAX" | received == "JAX", 1, 0))
jags_trade <- trades |>
  filter(isJags == 1)
Sys.setenv(CFBD_API_KEY = "ZO32LHHTA1q3Vk/a8djVsstJzkGb6QxOnf28Nl3Ad88rb0PEdq3ic2UBgMQYM6Zt")
roster_data <- cfbd_team_roster(year = 2023, team = NULL) |>
  mutate(name = paste0(first_name, ' ', last_name),
         hometown = paste0(home_city, ', ', home_state))

### All 2024 trades
jags_2024_trades <- jags_trade |>
  filter(season == 2024 | pick_season == 2024) |>
  subset(trade_id != '1702') |>
  mutate(pick_round = ifelse(trade_id == '1716', 3, pick_round),
         pick_number = ifelse(trade_id == '1716', 79, ifelse(trade_id == '1765', 116, pick_round))) |>
  mutate(jax_pos = ifelse(gave == "JAX", "gave", "received"),
         jax = "JAX",
         opp_team = ifelse(jax_pos == "gave", received, gave)) |>
  left_join(draft_value, by = c('pick_number' = 'Pick')) |>
  mutate(jax_value = ifelse(received == "JAX", Value, 0),
         opp_value = ifelse(received != "JAX", Value, 0),
         net_value = jax_value - opp_value) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('opp_team' = 'team_abbr')) |>
  group_by(trade_id, opp_team, team_wordmark, trade_date) |>
  summarise(jax_value = sum(jax_value),
            opp_value = sum(opp_value),
            net_value = sum(net_value))
jags_2024_trades_tbl <- jags_2024_trades |>
  relocate(team_wordmark) |>
  subset(select = -c(trade_id, opp_team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  grand_summary_rows(columns = c(jax_value, opp_value, net_value), fns = list(Total ~ sum(.))) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Trade Partner", jax_value = "Jags Value Gained", opp_value = "Jags Value Lost",
             net_value = "Net Value Gained", trade_date = "Trade Date") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars All 2024 Trades",
             subtitle = "Value from Fitzgerald-Spielberger draft chart") |>
  gt_theme_538()
jags_2024_trades_tbl
gtsave(jags_2024_trades_tbl, "Draft/Jags2024Trades.png")


### 2024 Draft Day Trades
jags_draftday_trades_2024 <- jags_trade |>
  filter(between(trade_date, as.Date('2024-04-12'), as.Date('2024-04-27'))) |>
  mutate(pick_number = ifelse(is.na(pick_number), (32*pick_round)-16, pick_number),
         jax_pos = ifelse(gave == "JAX", "gave", "received"),
         jax = "JAX",
         opp_team = ifelse(jax_pos == "gave", received, gave)) |>
  left_join(draft_value, by = c('pick_number' = 'Pick')) |>
  mutate(jax_value = ifelse(received == "JAX", Value, 0),
         opp_value = ifelse(received != "JAX", Value, 0),
         net_value = jax_value - opp_value) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('opp_team' = 'team_abbr')) |>
  group_by(trade_id, opp_team, team_wordmark, trade_date) |>
  summarise(jax_value = sum(jax_value),
            opp_value = sum(opp_value),
            net_value = sum(net_value))
net_2024_draftday_value <- sum(jags_draftday_trades_2024$net_value)
trade_value_pick <- which.min(abs(draft_value$Value - net_2024_draftday_value))
jags_2024_draftday_trades_tbl <- jags_draftday_trades_2024 |>
  relocate(team_wordmark) |>
  subset(select = -c(trade_id, opp_team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  grand_summary_rows(columns = c(jax_value, opp_value, net_value), fns = list(Total ~ sum(.))) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Trade Partner", jax_value = "Jags Value Gained", opp_value = "Jags Value Lost",
             net_value = "Net Value Gained", trade_date = "Trade Date") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Draft-day Trades",
             subtitle = paste0("Value from Fitzgerald-Spielberger draft chart | Equivalent Value: Pick # ", trade_value_pick)) |>
  gt_theme_538()
jags_2024_draftday_trades_tbl
gtsave(jags_2024_draftday_trades_tbl, "Draft/Jags2024DraftDayTrades.png")

### Individual Trade Value
jags_draftday_trade_1 <- jags_trade |>
  filter(trade_id == 1) |>
  mutate(pick_number = ifelse(is.na(pick_number), (32*pick_round)-16, pick_number),
         jax_pos = ifelse(gave == "JAX", "gave", "received"),
         jax = "JAX",
         opp_team = ifelse(jax_pos == "gave", received, gave)) |>
  left_join(draft_value, by = c('pick_number' = 'Pick')) |>
  mutate(Value = ifelse(received == 'JAX', Value, -1*Value)) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('received' = 'team_abbr')) |>
  select(team_wordmark, pick_season, pick_round, pick_number, Value)
net_2024_draftday1_value <- sum(jags_draftday_trade_1$Value)
trade1_value_pick <- which.min(abs(draft_value$Value - net_2024_draftday1_value))
jags_draftday_trade_1_tbl <- jags_draftday_trade_1 |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  grand_summary_rows(columns = c(Value), fns = list(Total ~ sum(.))) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", pick_season = "Pick Season", pick_round = "Pick Round", pick_number = "Pick Number",
             Value = "Value Gained/Lost") |> 
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Trade",
             subtitle = paste0("Value from Fitzgerald-Spielberger draft chart | Equivalent Value: Pick # ", trade1_value_pick)) |>
  gt_theme_538()
jags_draftday_trade_1_tbl
gtsave(jags_draftday_trade_1_tbl, "Draft/Jags2024DraftDayTrade1.png")

# Jags Draft Picks
cfb_logos <- cfbd_team_info()
jags_draftees <- jags_draft_2024 |>
  mutate(pfr_player_name = ifelse(pfr_player_name == 'Brian Thomas', 'Brian Thomas Jr.', pfr_player_name)) |>
  left_join(roster_data, by = c('pfr_player_name' = 'name')) |>
  subset(athlete_id != 4698425) |>
  left_join(select(cfb_logos, school, logo), by = c('team.y' = 'school'))
jags_draftee_tbl <- jags_draftees |>
  select(headshot_url, round, pick, pfr_player_name, logo, hometown, position.x, age) |>
  ungroup() |> gt() |>
  gt_img_rows(headshot_url) |>
  gt_img_rows(logo) |>
  tab_source_note("Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  gt_theme_538() |>
  opt_row_striping() |>
  tab_header(title = "Jaguars 2024 Draft Picks") |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(headshot_url = "Player", pfr_player_name = "Name", logo = "School", position.x = "Position")
jags_draftee_tbl
gtsave(jags_draftee_tbl, "Draft/Jags2024Draftees.png")
  




