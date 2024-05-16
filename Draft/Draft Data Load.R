### Break out stats by position group. For each group, starts a dataframe and, like MLB, loop through
### list of players by pos in roster data and fill in stats that matter
### Export to csv for easy read in?

# qb_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "QB") |>
#   select(-position)
# qb_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(), passing_completions = numeric(),
#                             passing_completion_pct = numeric(), passing_interceptions = numeric(), passing_interception_pct = numeric(),
#                             passing_net_passing_yards_per_game = numeric(), passing_passing_attempts = numeric(), passing_passing_touchdowns = numeric(),
#                             passing_passing_touchdown_pct = numeric(), passing_passing_yards = numeric(), passing_sacks = numeric(),
#                             passing_yards_per_pass_attempt = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(qb_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = qb_player_ids[i,1], year = 2023)$passing_passing_attempts > 1,
#          output <- espn_cfb_player_stats(athlete_id = qb_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, passing_completions,
#                   passing_completion_pct, passing_interceptions, passing_interception_pct, passing_net_passing_yards_per_game,
#                   passing_passing_attempts, passing_passing_touchdowns, passing_passing_touchdown_pct, passing_passing_yards, passing_sacks,
#                   passing_yards_per_pass_attempt), next)
#   qb_stats_2023 <- rbind(qb_stats_2023, output)}
# qb_stats_2023 <- qb_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(qb_stats_2023, "Draft/2023 QB Stats w Dupes.csv")
# qb_stats_2023 <- qb_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(qb_stats_2023, "Draft/2023 Pos Stats/QB Stats.csv")


# wr_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "WR") |>
#   select(-position)
# wr_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                        display_name = character(), weight = numeric(), height = numeric(),
#                        position_abbreviation = character(), experience_years = integer(), receiving_long_reception = numeric(),
#                        receiving_receiving_touchdowns = numeric(), receiving_receiving_yards = numeric(),
#                        receiving_receptions = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(wr_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = wr_player_ids[i,1], year = 2023)$receiving_receptions > 1,
#   output <- espn_cfb_player_stats(athlete_id = wr_player_ids[i,1], year = 2023) |>
#     select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, experience_years,
#            receiving_long_reception, receiving_receiving_touchdowns, receiving_receiving_yards, receiving_receptions), next)
#   wr_stats_2023 <- rbind(wr_stats_2023, output)}
# wr_stats_2023 <- wr_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(wr_stats_2023, "Draft/2023 WR Stats.csv")


# rb_player_ids <- roster_data |>
#   filter(position == "RB") |>
#   select(athlete_id)
# rb_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                             experience_years = integer(), rushing_long_rushing = numeric(), rushing_rushing_attempts = numeric(),
#                             rushing_rushing_touchdowns = numeric(), rushing_rushing_yards = numeric(),
#                             receiving_receiving_touchdowns = numeric(), receiving_receiving_yards = numeric(), receiving_receptions = numeric(),
#                             stringsAsFactors = FALSE)
# for(i in 1:nrow(rb_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = rb_player_ids[i,1], year = 2023)$rushing_rushing_attempts > 1,
#          output <- espn_cfb_player_stats(athlete_id = rb_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, experience_years,
#                   rushing_long_rushing, rushing_rushing_attempts, rushing_rushing_touchdowns, rushing_rushing_yards,
#                   receiving_receiving_touchdowns, receiving_receiving_yards, receiving_receptions), next)
#   rb_stats_2023 <- rbind(rb_stats_2023, output)}
# rm(output)
# write.csv(rb_stats_2023, "Draft/RB Stats w Dupes.csv")
# test_rb <- espn_cfb_player_stats(athlete_id = player_id, year = 2023) |>
#   select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, experience_years,
#          rushing_long_rushing, rushing_rushing_attempts, rushing_rushing_touchdowns, rushing_rushing_yards,
#          receiving_receiving_touchdowns, receiving_receiving_yards, receiving_receptions)


# te_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "TE") |>
#   select(-position)
# te_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(),
#                             position_abbreviation = character(), experience_years = integer(), receiving_long_reception = numeric(),
#                             receiving_receiving_touchdowns = numeric(), receiving_receiving_yards = numeric(),
#                             receiving_receptions = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(te_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = te_player_ids[i,1], year = 2023)$receiving_receptions > 1,
#          output <- espn_cfb_player_stats(athlete_id = te_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, experience_years,
#                   receiving_long_reception, receiving_receiving_touchdowns, receiving_receiving_yards, receiving_receptions), next)
#   te_stats_2023 <- rbind(te_stats_2023, output)}
# write_csv(te_stats_2023, "Draft/Dupes/2023 TE Stats w dupes.csv")
# te_stats_2023 <- te_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(te_stats_2023, "Draft/2023 Pos Stats/TE Stats.csv")




# db_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "DB") |>
#   select(-position)
# db_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                             defensive_passes_defended = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                             defensive_total_tackles = numeric(), defensive_interceptions_interceptions = numeric(), stringsAsFactors = FALSE)
# output <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                      display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                      defensive_passes_defended = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                      defensive_total_tackles = numeric(), defensive_interceptions_interceptions = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(db_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = db_player_ids[i,1], year = 2023)$defensive_total_tackles > 1 | 
#            espn_cfb_player_stats(athlete_id = db_player_ids[i,1], year = 2023)$defensive_passes_defended > 1,
#          output <- espn_cfb_player_stats(athlete_id = db_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, defensive_passes_defended,
#                   defensive_sacks, defensive_solo_tackles, defensive_total_tackles, defensive_interceptions_interceptions), next)
#   db_stats_2023 <- rbind(db_stats_2023, output)}
# write_csv(db_stats_2023, "Draft/Dupes/2023 DB Stats w dupes.csv")
# db_stats_2023 <- db_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(db_stats_2023, "Draft/2023 Pos Stats/DB Stats.csv")



# lb_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "LB") |>
#   select(-position)
# lb_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                             general_fumbles_forced = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                             defensive_total_tackles = numeric(), defensive_passes_defended = numeric(), stringsAsFactors = FALSE)
# output <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                      display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                      general_fumbles_forced = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                      defensive_total_tackles = numeric(), defensive_passes_defended = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(lb_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = lb_player_ids[i,1], year = 2023)$defensive_total_tackles > 1,
#          output <- espn_cfb_player_stats(athlete_id = lb_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, general_fumbles_forced,
#                   defensive_sacks, defensive_solo_tackles, defensive_total_tackles, defensive_passes_defended), next)
#   lb_stats_2023 <- rbind(lb_stats_2023, output)}
# write_csv(lb_stats_2023, "Draft/Dupes/2023 LB Stats w dupes.csv")
# lb_stats_2023 <- lb_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(lb_stats_2023, "Draft/2023 Pos Stats/LB Stats.csv")




# dl_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "DL") |>
#   select(-position)
# dl_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                             general_fumbles_forced = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                             defensive_total_tackles = numeric(), defensive_sack_yards = numeric(), stringsAsFactors = FALSE)
# output <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                      display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                      general_fumbles_forced = numeric(),defensive_sacks = numeric(), defensive_solo_tackles = numeric(),
#                      defensive_total_tackles = numeric(), defensive_sack_yards = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(dl_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = dl_player_ids[i,1], year = 2023)$defensive_total_tackles > 1,
#          output <- espn_cfb_player_stats(athlete_id = dl_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, general_fumbles_forced,
#                   defensive_sacks, defensive_solo_tackles, defensive_total_tackles, defensive_sack_yards), next)
#   dl_stats_2023 <- rbind(dl_stats_2023, output)}
# write_csv(dl_stats_2023, "Draft/Dupes/2023 DL Stats w dupes.csv")
# dl_stats_2023 <- dl_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(dl_stats_2023, "Draft/2023 Pos Stats/DL Stats.csv")



# pk_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "K") |>
#   select(-position)
# pk_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                             display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                             kicking_extra_point_attempts = numeric(), kicking_extra_points_made = numeric(), kicking_field_goal_attempts = numeric(),
#                             kicking_field_goals_made = numeric(), kicking_long_field_goal_made = numeric(),  kicking_field_goal_attempts40_49 = numeric(),
#                             kicking_field_goal_attempts50_59 = numeric(), kicking_field_goal_attempts60_99 = numeric(), kicking_field_goals_made40_49 = numeric(),
#                             kicking_field_goals_made50_59 = numeric(), kicking_field_goals_made60_99 = numeric(), stringsAsFactors = FALSE)
# output <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                      display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                      kicking_extra_point_attempts = numeric(), kicking_extra_points_made = numeric(), kicking_field_goal_attempts = numeric(),
#                      kicking_field_goals_made = numeric(), kicking_long_field_goal_made = numeric(),  kicking_field_goal_attempts40_49 = numeric(),
#                      kicking_field_goal_attempts50_59 = numeric(), kicking_field_goal_attempts60_99 = numeric(), kicking_field_goals_made40_49 = numeric(),
#                      kicking_field_goals_made50_59 = numeric(), kicking_field_goals_made60_99 = numeric(), stringsAsFactors = FALSE)
# for(i in 1:nrow(pk_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = pk_player_ids[i,1], year = 2023)$kicking_field_goal_attempts > 1,
#          output <- espn_cfb_player_stats(athlete_id = pk_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, kicking_extra_point_attempts,
#                   kicking_extra_points_made, kicking_field_goal_attempts, kicking_field_goals_made, kicking_long_field_goal_made,
#                   kicking_field_goal_attempts40_49, kicking_field_goal_attempts50_59, kicking_field_goal_attempts60_99,
#                   kicking_field_goals_made40_49, kicking_field_goals_made50_59, kicking_field_goals_made60_99), next)
#   pk_stats_2023 <- rbind(pk_stats_2023, output)}
# write_csv(pk_stats_2023, "Draft/Dupes/2023 PK Stats w dupes.csv")
# pk_stats_2023 <- pk_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(pk_stats_2023, "Draft/2023 Pos Stats/PK Stats.csv")



# p_player_ids <- select(roster_data, athlete_id, position) |>
#   filter(position == "P") |>
#   select(-position)
# p_stats_2023 <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                            display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                            punting_gross_avg_punt_yards = numeric(), punting_long_punt = numeric(), punting_net_avg_punt_yards = numeric(),
#                            punting_punts = numeric(), punting_punts_inside20 = numeric(),  punting_punts_inside20pct = numeric(),
#                            punting_punt_yards = numeric(), punting_touchbacks = numeric(), punting_touchback_pct = numeric(),
#                            stringsAsFactors = FALSE)
# output <- data.frame(athlete_id = character(), first_name = character(), last_name = character(), full_name = character(),
#                      display_name = character(), weight = numeric(), height = numeric(), position_abbreviation = character(),
#                      punting_gross_avg_punt_yards = numeric(), punting_long_punt = numeric(), punting_net_avg_punt_yards = numeric(),
#                      punting_punts = numeric(), punting_punts_inside20 = numeric(),  punting_punts_inside20pct = numeric(),
#                      punting_punt_yards = numeric(), punting_touchbacks = numeric(), punting_touchback_pct = numeric(),
#                      stringsAsFactors = FALSE)
# for(i in 1:nrow(p_player_ids)) {
#   ifelse(espn_cfb_player_stats(athlete_id = p_player_ids[i,1], year = 2023)$punting_punts > 0,
#          output <- espn_cfb_player_stats(athlete_id = p_player_ids[i,1], year = 2023) |>
#            select(athlete_id, first_name, last_name, full_name, display_name, weight, height, position_abbreviation, punting_gross_avg_punt_yards,
#                   punting_long_punt, punting_net_avg_punt_yards, punting_punts, punting_punts_inside20,  punting_punts_inside20pct, punting_punt_yards,
#                   punting_touchbacks, punting_touchback_pct), next)
#   p_stats_2023 <- rbind(p_stats_2023, output)}
# write_csv(p_stats_2023, "Draft/Dupes/2023 P Stats w dupes.csv")
# p_stats_2023 <- p_stats_2023 |>
#   distinct(athlete_id, .keep_all = TRUE)
# write_csv(p_stats_2023, "Draft/2023 Pos Stats/P Stats.csv")