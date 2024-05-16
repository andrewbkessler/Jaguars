### Live Tests
draft_picks <- load_draft_picks(seasons = 2024)
day_1_trades <- load_trades() |>
  filter(trade_date == "2024-04-25")
rm(draft_picks, day_1_trades)

Sys.setenv(CFBD_API_KEY = "ZO32LHHTA1q3Vk/a8djVsstJzkGb6QxOnf28Nl3Ad88rb0PEdq3ic2UBgMQYM6Zt")

### Player Lookup
player_info <- cfbd_player_info(search_term = "Myles Cole", team = "Texas Tech", year = 2023) |>
  mutate(year = 2024)
player_info$athlete_id <- as.integer(player_info$athlete_id)
player_gp <- data.frame(year = 2018:2023,
                      gp = c(0, 0, 6, 6, 1, 12))
### Load 2023 CFB roster data
roster_data <- cfbd_team_roster(year = 2023, team = NULL) |>
  mutate(position = case_when(
    position %in% c('DB', 'CB', 'S') ~ 'DB',
    position %in% c('C', 'OT', 'G', 'OL') ~ 'OL',
    position %in% c('DT', 'DE', 'NT', 'DL') ~ 'DL',
    position %in% c('FB', 'RB') ~ 'RB',
    position %in% 'PK' ~ 'K',
    TRUE ~ position)) |>
  filter(position != "?" & position != "ATH" & position != "PR" & !is.na(position))
roster_data$athlete_id <- as.integer(roster_data$athlete_id)

### Get Pictures
cfb_logos <- cfbd_team_info()
player_pictures <- select(player_info, athlete_id, name, team) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id') |>
  left_join(select(cfb_logos, school, logo), by = c('team' = 'school'))
for (i in 1:1) {
  headshot_url <- paste(player_pictures[i,4], sep = "")
  headshot_z <- tempfile()
  download.file(headshot_url,headshot_z,mode="wb")
  headshot_pic <- readPNG(headshot_z, native = FALSE, info = FALSE)
  writePNG(headshot_pic, target = paste("Draft/Player Pictures/Headshot-", player_pictures[i,2], ".jpg", sep = ""), dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
  file.remove(headshot_z)
  school_url <- paste(player_pictures[i,5], sep = "")
  school_z <- tempfile()
  download.file(school_url,school_z,mode="wb")
  school_pic <- readPNG(school_z, native = FALSE, info = FALSE)
  writePNG(school_pic, target = paste("Draft/School Logos/Team-", player_pictures[i,2], ".jpg", sep = ""), dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
  file.remove(school_z)}
rm(player_pictures)

### Load Combine Data
combine_data <- load_combine(seasons = 2010:2024)  |>
  mutate(position = case_when(
      pos %in% c('DB', 'CB', 'S', 'SAF') ~ 'DB',
      pos %in% c('C', 'OT', 'G', 'OG', 'OL') ~ 'OL',
      pos %in% c('DT', 'DE', 'NT', 'DL', 'EDGE') ~ 'DL',
      pos %in% c('FB', 'RB') ~ 'RB',
      pos %in% c('LB', 'ILB', 'OLB') ~ 'LB',
      TRUE ~ pos))



### QB Data Loading/Formatting
qb_stats_2023 <- read.csv("Draft/2023 Pos Stats/QB Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2),
         spa = passing_sacks/passing_passing_attempts)
qb_combine <- combine_data |>
  filter(position == "QB") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'QB' & !is.na(combine_data$shuttle))+1,0)))
qb_ranks <- qb_stats_2023 |>
  filter(passing_passing_attempts >= 100) |>
  mutate(pas_cpc_rank = rank(-passing_completion_pct, ties.method = "min"),
         pas_ipc_rank = rank(passing_interception_pct, ties.method = "min"),
         pas_ypg_rank = rank(-passing_net_passing_yards_per_game, ties.method = "min"),
         pas_tds_rank = rank(-passing_passing_touchdowns, ties.method = "min"),
         pas_tpc_rank = rank(-passing_passing_touchdown_pct, ties.method = "min"),
         pas_yds_rank = rank(-passing_passing_yards, ties.method = "min"),
         pas_sac_rank = rank(passing_sacks, ties.method = "min"),
         pas_ypa_rank = rank(-passing_yards_per_pass_attempt, ties.method = "min"),
         pas_spa_rank = rank(spa, ties.method = "min"))
qb_stats_2023$passing_completion_pct <- percent(qb_stats_2023$passing_completion_pct/100, accuracy = .1)
qb_stats_2023$passing_yards_per_pass_attempt <- round(qb_stats_2023$passing_yards_per_pass_attempt, 2)
qb_stats_2023$passing_passing_touchdown_pct <- percent(qb_stats_2023$passing_passing_touchdown_pct/100, accuracy = .1)
qb_stats_2023$passing_interception_pct <- percent(qb_stats_2023$passing_interception_pct/100, accuracy = .1)
qb_stats_2023$spa <- percent(qb_stats_2023$spa, accuracy = .1)
qb_stats_expanded <- qb_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(qb_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(qb_ranks, athlete_id, pas_cpc_rank, pas_ipc_rank, pas_ypg_rank, pas_tds_rank, pas_tpc_rank, pas_yds_rank, pas_sac_rank,
                   pas_ypa_rank, pas_spa_rank), by = 'athlete_id')
### QB 1 Table
qb_1_ranks <- qb_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'pas_cpc_rank', 'pas_ipc_rank',
         'pas_ypg_rank', 'pas_tds_rank', 'pas_tpc_rank', 'pas_yds_rank', 'pas_sac_rank', 'pas_ypa_rank', 'pas_spa_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "passing_completion_pct" = "pas_cpc_rank",
         "passing_interception_pct" = "pas_ipc_rank", "passing_net_passing_yards_per_game" = "pas_ypg_rank",
         "passing_passing_touchdowns" = "pas_tds_rank", "passing_passing_touchdown_pct" = "pas_tpc_rank", "passing_passing_yards" = "pas_yds_rank",
         "passing_sacks"  = "pas_sac_rank", "passing_yards_per_pass_attempt" = "pas_ypa_rank", "spa" = "pas_spa_rank")
qb_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
qb_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(qb_stats_2023, athlete_id, passing_completion_pct, passing_passing_yards, passing_net_passing_yards_per_game,
                   passing_yards_per_pass_attempt, passing_passing_touchdowns, passing_passing_touchdown_pct, passing_interception_pct,
                   passing_sacks, spa), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(qb_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Pass Comp %" = "passing_completion_pct", "Interception Rate" = "passing_interception_pct",
         "Net Pass Yards per Game" = "passing_net_passing_yards_per_game", "Pass TDs" = "passing_passing_touchdowns",
         "TD Rate" = "passing_passing_touchdown_pct", "Pass Yards" = "passing_passing_yards",
         "Sacks" = "passing_sacks", "Pass Yards per Att" = "passing_yards_per_pass_attempt", "Sack Rate" = "spa") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 100 Atts" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- qb_1_stats$`Min. 100 Atts`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(qb_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(qb_ranks)) color_index <- nrow(qb_ranks)
  stat_palette[color_index]}
stat_numbers <- qb_1_stats$`Min. 100 Atts`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
qb_1_stats <- cbind(qb_1_stats, perc_colors, stat_colors)
qb_1_tbl <- qb_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = qb_1_info$headshot_url, height = px(50)), paste(qb_1_info$name, ', ',qb_1_info$position, ' - ', qb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', qb_1_info$home_town, ' | Ht (in.): ', qb_1_info$height, ' | Wt (lbs.): ', qb_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 100 Atts', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 100 Atts', rows = 7:15)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
qb_1_tbl
gtsave(qb_1_tbl, paste("Draft/Player Tables/", qb_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate QB 1 stats by year
qb_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$passing_passing_attempts),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  qb_1_career_stats <- rbind(qb_1_career_stats, output, fill = TRUE)}
qb_1_career_stats[is.na(qb_1_career_stats)] <- 0
qb_1_career_stats$passing_net_passing_yards_per_game[qb_1_career_stats$passing_net_passing_yards_per_game == 0] <- NA
rm(output)
### QB 1 Career Graphs
qb_1_scale <- max(qb_1_career_stats$passing_completion_pct, na.rm = TRUE)/max(qb_1_career_stats$passing_yards_per_pass_attempt, na.rm = TRUE)
qb_1_graph <- ggplot(qb_1_career_stats, aes(year)) +
  geom_line(aes(y = passing_yards_per_pass_attempt), color = gold) +
  geom_line(aes(y = passing_completion_pct/qb_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(qb_1_career_stats$year), max(qb_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*qb_1_scale, name="Completion Perc (%)")) +
  theme_bw() +
  labs(y = "Yards per Attempt",
       title = paste(qb_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
qb_1_graph
ggsave(paste('Draft/Player Charts/', qb_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### QB 1 Career Stats
qb_1_career_tbl <- qb_1_career_stats |>
  mutate(spa = passing_sacks/passing_passing_attempts,
         passing_yards_per_pass_attempt = round(passing_yards_per_pass_attempt, 2),
         passing_completion_pct = passing_completion_pct/100,
         passing_passing_touchdown_pct = passing_passing_touchdown_pct/100,
         passing_interception_pct = passing_interception_pct/100) |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, passing_completion_pct, passing_passing_yards, passing_net_passing_yards_per_game, passing_yards_per_pass_attempt,
         passing_passing_touchdowns, passing_passing_touchdown_pct, passing_interception_pct, passing_sacks, spa) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  fmt_percent(columns = c(passing_completion_pct, passing_passing_touchdown_pct, passing_interception_pct, spa), decimals = 1) |>
  fmt_number(columns = passing_net_passing_yards_per_game, decimals = 1) |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = qb_1_info$headshot_url, height = px(50)), paste(qb_1_info$name, ', ',qb_1_info$position, ' - ', qb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', qb_1_info$home_town, ' | Ht (in.): ', qb_1_info$height, ' | Wt (lbs.): ', qb_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", passing_completion_pct = "Comp Pct", passing_passing_yards = "Pass Yards",
             passing_net_passing_yards_per_game = "Yards per Game", passing_yards_per_pass_attempt = "Yards per Attempt",
             passing_passing_touchdowns = "Pass TDs", passing_passing_touchdown_pct = "TD Rate", passing_interception_pct = "Int Rate",
             passing_sacks = "Sacks", spa = "Sack Rate")
qb_1_career_tbl
gtsave(qb_1_career_tbl, paste("Draft/Player Tables/", qb_1_info$name, ' Career Stats.png', sep = ''))
rm(qb_1_info, qb_1_ranks, qb_1_stats, qb_combine, qb_ranks, qb_stats_expanded, qb_1_tbl, qb_stats_2023, qb_1_career_stats, qb_1_career_tbl, qb_1_graph)





### WR Data Loading/Formatting
wr_stats_2023 <- read.csv("Draft/2023 Pos Stats/WR Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2),
         rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1),
         rec_tpc = round(receiving_receiving_touchdowns/receiving_receptions, 2))
wr_combine <- combine_data |>
  filter(position == "WR") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'WR' & !is.na(combine_data$shuttle))+1,0)))
wr_ranks <- wr_stats_2023 |>
  filter(receiving_receptions >= 50) |>
  mutate(rec_rec_rank = rank(-receiving_receptions, ties.method = "min"),
         rec_yds_rank = rank(-receiving_receiving_yards, ties.method = "min"),
         rec_tds_rank = rank(-receiving_receiving_touchdowns, ties.method = "min"),
         rec_long_rank = rank(-receiving_long_reception, ties.method = "min"),
         rec_ypc_rank = rank(-rec_ypc, ties.method = "min"),
         rec_tpc_rank = rank(-rec_tpc, ties.method = "min")) 
wr_stats_expanded <- wr_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(wr_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(wr_ranks, athlete_id, rec_rec_rank, rec_yds_rank, rec_tds_rank, rec_long_rank, rec_ypc_rank, rec_tpc_rank), by = 'athlete_id')
### WR 1 Table
wr_1_ranks <- wr_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'rec_rec_rank', 'rec_yds_rank',
         'rec_tds_rank', 'rec_long_rank', 'rec_ypc_rank', 'rec_tpc_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "receiving_receptions" = "rec_rec_rank",
         "receiving_receiving_yards" = "rec_yds_rank", "receiving_receiving_touchdowns" = "rec_tds_rank",
         "receiving_long_reception" = "rec_long_rank", "rec_ypc" = "rec_ypc_rank", "rec_tpc" = "rec_tpc_rank")
wr_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
wr_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(wr_stats_2023, athlete_id, receiving_receptions, receiving_receiving_yards, receiving_receiving_touchdowns,
                   receiving_long_reception, rec_ypc, rec_tpc), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(wr_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Receptions" = "receiving_receptions", "Rec Yards" = "receiving_receiving_yards",
         "Rec Touchdowns" = "receiving_receiving_touchdowns", "Long Reception" = "receiving_long_reception",
         "Yards per Catch" = "rec_ypc", "TDs per Catch" = "rec_tpc") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 50 rec" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- wr_1_stats$`Min. 50 rec`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(wr_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(wr_ranks)) color_index <- nrow(wr_ranks)
  stat_palette[color_index]}
stat_numbers <- wr_1_stats$`Min. 50 rec`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
wr_1_stats <- cbind(wr_1_stats, perc_colors, stat_colors)
wr_1_tbl <- wr_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = wr_1_info$headshot_url, height = px(50)), paste(wr_1_info$name, ', ',wr_1_info$position, ' - ', wr_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', wr_1_info$home_town, ' | Ht (in.): ', wr_1_info$height, ' | Wt (lbs.): ', wr_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 50 rec', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 50 rec', rows = 7:12)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
wr_1_tbl
gtsave(wr_1_tbl, paste("Draft/Player Tables/", wr_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate WR 1 stats by year
wr_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$receiving_receptions),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  wr_1_career_stats <- rbind(wr_1_career_stats, output, fill = TRUE)}
wr_1_career_stats[is.na(wr_1_career_stats)] <- 0
rm(output)
### WR 1 Career Graphs
wr_1_scale <- max(wr_1_career_stats$receiving_receiving_yards, na.rm = TRUE)/max(wr_1_career_stats$receiving_receptions, na.rm = TRUE)
wr_1_graph <- ggplot(wr_1_career_stats, aes(year)) +
  geom_line(aes(y = receiving_receptions), color = gold) +
  geom_line(aes(y = receiving_receiving_yards/wr_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(wr_1_career_stats$year), max(wr_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*wr_1_scale, name="Receiving Yards")) +
  theme_bw() +
  labs(y = "Receptions",
       title = paste(wr_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
wr_1_graph
ggsave(paste('Draft/Player Charts/', wr_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### WR 1 Career Stats
wr_1_career_tbl <- wr_1_career_stats |>
  mutate(rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1),
         rec_tpc = round(receiving_receiving_touchdowns/receiving_receptions, 2)) |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, receiving_receptions, receiving_receiving_yards, receiving_receiving_touchdowns,
         receiving_long_reception, rec_ypc, rec_tpc) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = wr_1_info$headshot_url, height = px(50)), paste(wr_1_info$name, ', ',wr_1_info$position, ' - ', wr_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', wr_1_info$home_town, ' | Ht (in.): ', wr_1_info$height, ' | Wt (lbs.): ', wr_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", receiving_receptions = "Receptions", receiving_receiving_yards = "Yards",
             receiving_receiving_touchdowns = "TDs", receiving_long_reception = "Long", rec_ypc = "Yards per Catch", rec_tpc = "TDs per Catch")
wr_1_career_tbl
gtsave(wr_1_career_tbl, paste("Draft/Player Tables/", wr_1_info$name, ' Career Stats.png', sep = ''))
rm(wr_1_info, wr_1_ranks, wr_1_stats, wr_1_gp, wr_combine, wr_ranks, wr_stats_expanded, wr_1_tbl, wr_stats_2023, wr_1_career_tbl,
   wr_1_career_stats, wr_1_graph)




### RB Data Loading/Formatting
rb_stats_2023 <- read.csv("Draft/2023 Pos Stats/RB Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2),
         rus_ypc = round(rushing_rushing_yards/rushing_rushing_attempts, 1),
         rus_tpc = round(rushing_rushing_touchdowns/rushing_rushing_attempts, 2),
         rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1))
rb_combine <- combine_data |>
  filter(position == "RB") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'RB' & !is.na(combine_data$shuttle))+1,0)))
rb_ranks <- rb_stats_2023 |>
  filter(rushing_rushing_attempts >= 100) |>
  mutate(rus_att_rank = rank(-rushing_rushing_attempts, ties.method = "min"),
         rus_yds_rank = rank(-rushing_rushing_yards, ties.method = "min"),
         rus_tds_rank = rank(-rushing_rushing_touchdowns, ties.method = "min"),
         rus_long_rank = rank(-rushing_long_rushing, ties.method = "min"),
         rus_ypc_rank = rank(-rus_ypc, ties.method = "min"),
         rus_tpc_rank = rank(-rus_tpc, ties.method = "min"),
         rec_rec_rank = rank(-receiving_receptions, ties.method = "min"),
         rec_yds_rank = rank(-receiving_receiving_yards, ties.method = "min"),
         rec_ypc_rank = rank(-rec_ypc, ties.method = "min"))
rb_stats_expanded <- rb_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(rb_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(rb_ranks, athlete_id, rus_att_rank, rus_yds_rank, rus_tds_rank, rus_long_rank, rus_ypc_rank, rus_tpc_rank,
                   rec_rec_rank, rec_yds_rank, rec_ypc_rank), by = 'athlete_id')
### RB 1 Table
rb_1_ranks <- rb_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'rus_att_rank', 'rus_yds_rank', 'rus_tds_rank',
         'rus_long_rank', 'rus_ypc_rank', 'rus_tpc_rank', 'rec_rec_rank', 'rec_yds_rank', 'rec_ypc_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "rushing_rushing_attempts" = "rus_att_rank", "rushing_rushing_yards" = "rus_yds_rank",
         "rushing_rushing_touchdowns" = "rus_tds_rank", "rushing_long_rushing" = "rus_long_rank", "rus_ypc" = "rus_ypc_rank", "rus_tpc" = "rus_tpc_rank",
         "receiving_receptions" = "rec_rec_rank", "receiving_receiving_yards" = "rec_yds_rank", "rec_ypc" = "rec_ypc_rank")
rb_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
rb_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(rb_stats_2023, athlete_id, rushing_rushing_attempts, rushing_rushing_yards, rushing_rushing_touchdowns, rushing_long_rushing,
                   rus_ypc, rus_tpc, receiving_receptions, receiving_receiving_yards, rec_ypc), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(rb_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Rush Attempts" = "rushing_rushing_attempts", "Rush Yards" = "rushing_rushing_yards",
         "Rush Touchdowns" = "rushing_rushing_touchdowns", "Long Rush" = "rushing_long_rushing", "Yards per Carry" = "rus_ypc",
         "TDs per Carry" = "rus_tpc", "Receptions" = "receiving_receptions", "Rec Yards" = "receiving_receiving_yards", "Yards per Catch" = "rec_ypc") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 100 carries" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- rb_1_stats$`Min. 100 carries`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(rb_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(rb_ranks)) color_index <- nrow(rb_ranks)
  stat_palette[color_index]}
stat_numbers <- rb_1_stats$`Min. 100 carries`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
rb_1_stats <- cbind(rb_1_stats, perc_colors, stat_colors)
rb_1_tbl <- rb_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = rb_1_info$headshot_url, height = px(50)), paste(rb_1_info$name, ', ',rb_1_info$position, ' - ', rb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', rb_1_info$home_town, ' | Ht (in.): ', rb_1_info$height, ' | Wt (lbs.): ', rb_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 100 carries', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 100 carries', rows = 7:15)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
rb_1_tbl
gtsave(rb_1_tbl, paste("Draft/Player Tables/", rb_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate RB 1 stats by year
rb_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$rushing_rushing_attempts),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  rb_1_career_stats <- rbind(rb_1_career_stats, output, fill = TRUE)}
rb_1_career_stats[is.na(rb_1_career_stats)] <- 0
rm(output)
### RB 1 Career Graphs
rb_1_scale <- max(rb_1_career_stats$rushing_rushing_yards, na.rm = TRUE)/max(rb_1_career_stats$rushing_rushing_attempts, na.rm = TRUE)
rb_1_graph <- ggplot(rb_1_career_stats, aes(year)) +
  geom_line(aes(y = rushing_rushing_attempts), color = gold) +
  geom_line(aes(y = rushing_rushing_yards/rb_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(rb_1_career_stats$year), max(rb_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*rb_1_scale, name="Rushing Yards")) +
  theme_bw() +
  labs(y = "Rushing Attempts",
       title = paste(rb_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
rb_1_graph
ggsave(paste('Draft/Player Charts/', rb_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### RB 1 Career Stats
rb_1_career_tbl <- rb_1_career_stats |>
  mutate(rus_ypc = round(rushing_rushing_yards/rushing_rushing_attempts, 1),
         rus_tpc = round(rushing_rushing_touchdowns/rushing_rushing_attempts, 1),
         rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1)) |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, rushing_rushing_attempts, rushing_rushing_yards, rushing_rushing_touchdowns, rushing_long_rushing,
         rus_ypc, rus_tpc, receiving_receptions, receiving_receiving_yards, rec_ypc) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = rb_1_info$headshot_url, height = px(50)), paste(rb_1_info$name, ', ',rb_1_info$position, ' - ', rb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', rb_1_info$home_town, ' | Ht (in.): ', rb_1_info$height, ' | Wt (lbs.): ', rb_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started",  rushing_rushing_attempts = "Rush Attempts", rushing_rushing_yards = "Rush Yards",
             rushing_rushing_touchdowns = "Rush TDs", rushing_long_rushing = "Rush Long", rus_ypc = "Yards per Carry", rus_tpc = "TDs per Carry",
             receiving_receptions = "Receptions", receiving_receiving_yards = "Yards", rec_ypc = "Yards per Catch")
rb_1_career_tbl
gtsave(rb_1_career_tbl, paste("Draft/Player Tables/", rb_1_info$name, ' Career Stats.png', sep = ''))
rm(rb_1_info, rb_1_ranks, rb_1_stats, rb_combine, rb_ranks, rb_stats_expanded, rb_1_tbl, rb_stats_2023, rb_1_career_stats,
   rb_1_career_tbl, rb_1_graph)




### TE Data Loading/Formatting
te_stats_2023 <- read.csv("Draft/2023 Pos Stats/TE Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2),
         rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1),
         rec_tpc = round(receiving_receiving_touchdowns/receiving_receptions, 2))
te_combine <- combine_data |>
  filter(position == "TE") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'TE' & !is.na(combine_data$shuttle))+1,0)))
te_ranks <- te_stats_2023 |>
  filter(receiving_receptions >= 25) |>
  mutate(rec_rec_rank = rank(-receiving_receptions, ties.method = "min"),
         rec_yds_rank = rank(-receiving_receiving_yards, ties.method = "min"),
         rec_tds_rank = rank(-receiving_receiving_touchdowns, ties.method = "min"),
         rec_long_rank = rank(-receiving_long_reception, ties.method = "min"),
         rec_ypc_rank = rank(-rec_ypc, ties.method = "min"),
         rec_tpc_rank = rank(-rec_tpc, ties.method = "min"))
te_stats_expanded <- te_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(te_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(te_ranks, athlete_id, rec_rec_rank, rec_yds_rank, rec_tds_rank, rec_long_rank, rec_ypc_rank, rec_tpc_rank), by = 'athlete_id')
### TE 1 Table
te_1_ranks <- te_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'rec_rec_rank', 'rec_yds_rank',
         'rec_tds_rank', 'rec_long_rank', 'rec_ypc_rank', 'rec_tpc_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "receiving_receptions" = "rec_rec_rank",
         "receiving_receiving_yards" = "rec_yds_rank", "receiving_receiving_touchdowns" = "rec_tds_rank",
         "receiving_long_reception" = "rec_long_rank", "rec_ypc" = "rec_ypc_rank", "rec_tpc" = "rec_tpc_rank")
te_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
te_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(te_stats_2023, athlete_id, receiving_receptions, receiving_receiving_yards, receiving_receiving_touchdowns,
                   receiving_long_reception, rec_ypc, rec_tpc), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(te_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Receptions" = "receiving_receptions", "Rec Yards" = "receiving_receiving_yards",
         "Rec Touchdowns" = "receiving_receiving_touchdowns", "Long Reception" = "receiving_long_reception",
         "Yards per Catch" = "rec_ypc", "TDs per Catch" = "rec_tpc") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 25 rec" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- te_1_stats$`Min. 25 rec`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(te_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(te_ranks)) color_index <- nrow(te_ranks)
  stat_palette[color_index]}
stat_numbers <- te_1_stats$`Min. 25 rec`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
te_1_stats <- cbind(te_1_stats, perc_colors, stat_colors)
te_1_tbl <- te_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = te_1_info$headshot_url, height = px(50)), paste(te_1_info$name, ', ',te_1_info$position, ' - ', te_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', te_1_info$home_town, ' | Ht (in.): ', te_1_info$height, ' | Wt (lbs.): ', te_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 25 rec', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 25 rec', rows = 7:12)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
te_1_tbl
gtsave(te_1_tbl, paste("Draft/Player Tables/", te_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate TE 1 stats by year
te_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$receiving_receptions),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  te_1_career_stats <- rbind(te_1_career_stats, output, fill = TRUE)}
te_1_career_stats[is.na(te_1_career_stats)] <- 0
rm(output)
### TE 1 Career Graphs
te_1_scale <- max(te_1_career_stats$receiving_receiving_yards, na.rm = TRUE)/max(te_1_career_stats$receiving_receptions, na.rm = TRUE)
te_1_graph <- ggplot(te_1_career_stats, aes(year)) +
  geom_line(aes(y = receiving_receptions), color = gold) +
  geom_line(aes(y = receiving_receiving_yards/te_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(te_1_career_stats$year), max(te_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*te_1_scale, name="Receiving Yards")) +
  theme_bw() +
  labs(y = "Receptions",
       title = paste(te_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
te_1_graph
ggsave(paste('Draft/Player Charts/', te_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### TE 1 Career Stats
te_1_career_tbl <- te_1_career_stats |>
  mutate(rec_ypc = round(receiving_receiving_yards/receiving_receptions, 1),
         rec_tpc = round(receiving_receiving_touchdowns/receiving_receptions, 2)) |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, receiving_receptions, receiving_receiving_yards, receiving_receiving_touchdowns,
         receiving_long_reception, rec_ypc, rec_tpc) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = te_1_info$headshot_url, height = px(50)), paste(te_1_info$name, ', ',te_1_info$position, ' - ', te_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', te_1_info$home_town, ' | Ht (in.): ', te_1_info$height, ' | Wt (lbs.): ', te_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", receiving_receptions = "Receptions", receiving_receiving_yards = "Yards",
             receiving_receiving_touchdowns = "TDs", receiving_long_reception = "Long", rec_ypc = "Yards per Catch", rec_tpc = "TDs per Catch")
te_1_career_tbl
gtsave(te_1_career_tbl, paste("Draft/Player Tables/", te_1_info$name, ' Career Stats.png', sep = ''))
rm(te_1_info, te_1_ranks, te_1_stats, te_combine, te_ranks, te_stats_expanded, te_1_tbl, te_stats_2023)





### OL Data Loading/Formatting
ol_stats_2023 <- roster_data |>
  filter(position == "OL") |>
  mutate(name = paste0(first_name, ' ', last_name),
         hometown = paste0(home_city, ', ', home_state),
         year = 2024)
ol_combine_meas <- read.csv("Draft/2023 Pos Stats/2024 OL Combine.csv")
ol_combine <- combine_data |>
  filter(position == 'OL') |>
  select(-bench)
ol_combine <- ol_combine |>
  mutate(player_name = ifelse(player_name == 'Kaitori Leveston', 'KT Leveston', player_name),
         player_name = ifelse(player_name == 'Sedrick Van Pran-Granger', 'Sedrick Van Pran', player_name)) |>
  left_join(select(ol_combine_meas, player, height, bench, hand, arm), by = c('player_name' = 'player')) |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(99*rank(bench, ties.method = 'max')/29,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$shuttle))+1,0)),
         height_perc = ifelse(is.na(height),NA,round(98*rank(height, ties.method = 'max')/70,0)),
         weight_perc = ifelse(is.na(wt),NA,round(98*rank(wt, ties.method = 'max')/sum(combine_data$position == 'OL' & !is.na(combine_data$wt))+1,0)),
         hand_perc = ifelse(is.na(hand),NA,round(98*rank(hand, ties.method = 'max')/70,0)),
         arm_perc = ifelse(is.na(arm),NA,round(98*rank(arm, ties.method = 'max')/70,0)))
ol_stats_expanded <- ol_stats_2023 |>
  # mutate(combine_name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(ol_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc, height_perc, weight_perc,
                   hand_perc, arm_perc),
            by = c('year' = 'season', 'name' = 'player_name'))
### OL 1 Table
ol_1_ranks <- ol_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'height_perc', 'weight_perc',
         'hand_perc', 'arm_perc') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc", "cone" = "cone_perc",
         "shuttle" = "shuttle_perc", "ht" = "height_perc", "wt" = "weight_perc", "hand" = "hand_perc", "arm" = "arm_perc")
ol_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
ol_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(ol_combine, season, player_name, ht, wt, hand, arm, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)')) |>
  rbind(ol_1_ranks) |>
  rename("Height" = "ht", "Weight" = "wt", "Hand" = "hand", "Arm" = "arm", "40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical",
         "Broad Jump" = "broad_jump", "Cone Drill" = "cone", "Shuttle Dash" = "shuttle") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Height, Hand, Arm, & Bench: 2024 only" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- ol_1_stats$`Height, Hand, Arm, & Bench: 2024 only`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
ol_1_stats <- cbind(ol_1_stats, perc_colors)
ol_1_tbl <- ol_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = ol_1_info$headshot_url, height = px(50)), paste(ol_1_info$name, ', ',ol_1_info$position, ' - ', ol_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', ol_1_info$home_town, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Height, Hand, Arm, & Bench: 2024 only', rows = 1:10)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors))
ol_1_tbl
gtsave(ol_1_tbl, paste("Draft/Player Tables/", ol_1_info$name, ' 2023 Stats.png', sep = ''))
rm(ol_1_info, ol_1_ranks, ol_1_stats, ol_1_tbl, ol_combine, ol_combine_meas, ol_stats_2023, ol_stats_expanded)








### DB Data Loading/Formatting
db_stats_2023 <- read.csv("Draft/2023 Pos Stats/DB Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2))
db_combine <- combine_data |>
  filter(position == "DB") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'DB' & !is.na(combine_data$shuttle))+1,0)))
db_ranks <- db_stats_2023 |>
  filter(defensive_passes_defended >= 5 | defensive_total_tackles >= 25) |>
  mutate(def_pd_rank = rank(-defensive_passes_defended, ties.method = "min"),
         def_sks_rank = rank(-defensive_sacks, ties.method = "min"),
         def_stk_rank = rank(-defensive_solo_tackles, ties.method = "min"),
         def_ttk_rank = rank(-defensive_total_tackles, ties.method = "min"),
         def_int_rank = rank(-defensive_interceptions_interceptions, ties.method = "min"))
db_stats_expanded <- db_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(db_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(db_ranks, athlete_id, def_pd_rank, def_int_rank, def_stk_rank, def_ttk_rank, def_sks_rank), by = 'athlete_id')
### DB 1 Table
db_1_ranks <- db_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'def_pd_rank', 'def_int_rank', 'def_stk_rank', 
         'def_ttk_rank', 'def_sks_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "defensive_passes_defended" = "def_pd_rank",
         "defensive_interceptions_interceptions" = "def_int_rank", "defensive_solo_tackles" = "def_stk_rank",
         "defensive_total_tackles" = "def_ttk_rank", "defensive_sacks" = "def_sks_rank")
db_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
db_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(db_stats_2023, athlete_id, defensive_passes_defended, defensive_interceptions_interceptions, defensive_solo_tackles,
                   defensive_total_tackles, defensive_sacks), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(db_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Passes Defended" = "defensive_passes_defended",
         "INTs" = "defensive_interceptions_interceptions", "Solo Tackles" = "defensive_solo_tackles", "Total Tackles" = "defensive_total_tackles",
         "Sacks" = "defensive_sacks") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 10 PDs or 50 TKLs" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- db_1_stats$`Min. 10 PDs or 50 TKLs`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(db_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(db_ranks)) color_index <- nrow(db_ranks)
  stat_palette[color_index]}
stat_numbers <- db_1_stats$`Min. 10 PDs or 50 TKLs`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
db_1_stats <- cbind(db_1_stats, perc_colors, stat_colors)
db_1_tbl <- db_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = db_1_info$headshot_url, height = px(50)), paste(db_1_info$name, ', ',db_1_info$position, ' - ', db_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', db_1_info$home_town, ' | Ht (in.): ', db_1_info$height, ' | Wt (lbs.): ', db_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 10 PDs or 50 TKLs', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 10 PDs or 50 TKLs', rows = 7:11)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
db_1_tbl
gtsave(db_1_tbl, paste("Draft/Player Tables/", db_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate DB 1 stats by year
db_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$defensive_passes_defended),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  db_1_career_stats <- rbind(db_1_career_stats, output, fill = TRUE)}
db_1_career_stats[is.na(db_1_career_stats)] <- 0
rm(output)
### DB 1 Career Graphs
db_1_scale <- max(db_1_career_stats$defensive_total_tackles, na.rm = TRUE)/max(db_1_career_stats$defensive_passes_defended, na.rm = TRUE)
db_1_graph <- ggplot(db_1_career_stats, aes(year)) +
  geom_line(aes(y = defensive_passes_defended), color = gold) +
  geom_line(aes(y = defensive_total_tackles/db_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(db_1_career_stats$year), max(db_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*db_1_scale, name="Total Tackles")) +
  theme_bw() +
  labs(y = "Passes Defended",
       title = paste(db_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
db_1_graph
ggsave(paste('Draft/Player Charts/', db_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### DB 1 Career Stats
db_1_career_tbl <- db_1_career_stats |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, defensive_passes_defended, defensive_interceptions_interceptions, defensive_solo_tackles,
         defensive_total_tackles, defensive_sacks) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = db_1_info$headshot_url, height = px(50)), paste(db_1_info$name, ', ',db_1_info$position, ' - ', db_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', db_1_info$home_town, ' | Ht (in.): ', db_1_info$height, ' | Wt (lbs.): ', db_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", defensive_passes_defended = "Passes Defended", defensive_interceptions_interceptions = "INTs",
             defensive_solo_tackles = "Solo Tackles", defensive_total_tackles = "Total Tackles", defensive_sacks = "Sacks")
db_1_career_tbl
gtsave(db_1_career_tbl, paste("Draft/Player Tables/", db_1_info$name, ' Career Stats.png', sep = ''))
rm(db_1_info, db_1_ranks, db_1_stats, db_combine, db_ranks, db_stats_expanded, db_1_tbl, db_stats_2023, db_1_career_stats,
   db_1_career_tbl, db_1_graph)







### LB Data Loading/Formatting
lb_stats_2023 <- read.csv("Draft/2023 Pos Stats/LB Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2))
lb_combine <- combine_data |>
  filter(position == "LB") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'LB' & !is.na(combine_data$shuttle))+1,0)))
lb_ranks <- lb_stats_2023 |>
  filter(defensive_total_tackles >= 60) |>
  mutate(def_pd_rank = rank(-defensive_passes_defended, ties.method = "min"),
         def_sks_rank = rank(-defensive_sacks, ties.method = "min"),
         def_stk_rank = rank(-defensive_solo_tackles, ties.method = "min"),
         def_ttk_rank = rank(-defensive_total_tackles, ties.method = "min"),
         def_ff_rank = rank(-general_fumbles_forced, ties.method = "min"))
lb_stats_expanded <- lb_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(lb_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(lb_ranks, athlete_id, def_pd_rank, def_ff_rank, def_stk_rank, def_ttk_rank, def_sks_rank), by = 'athlete_id')
### LB 1 Table
lb_1_ranks <- lb_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'def_pd_rank', 'def_ff_rank', 'def_stk_rank', 
         'def_ttk_rank', 'def_sks_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "defensive_passes_defended" = "def_pd_rank",
         "general_fumbles_forced" = "def_ff_rank", "defensive_solo_tackles" = "def_stk_rank",
         "defensive_total_tackles" = "def_ttk_rank", "defensive_sacks" = "def_sks_rank")
lb_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
lb_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(lb_stats_2023, athlete_id, defensive_passes_defended, general_fumbles_forced, defensive_solo_tackles,
                   defensive_total_tackles, defensive_sacks), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(lb_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Passes Defended" = "defensive_passes_defended",
         "Fumbles Forced" = "general_fumbles_forced", "Solo Tackles" = "defensive_solo_tackles", "Total Tackles" = "defensive_total_tackles",
         "Sacks" = "defensive_sacks") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 60 TKLs" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- lb_1_stats$`Min. 60 TKLs`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(lb_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(lb_ranks)) color_index <- nrow(lb_ranks)
  stat_palette[color_index]}
stat_numbers <- lb_1_stats$`Min. 60 TKLs`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
lb_1_stats <- cbind(lb_1_stats, perc_colors, stat_colors)
lb_1_tbl <- lb_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = lb_1_info$headshot_url, height = px(50)), paste(lb_1_info$name, ', ',lb_1_info$position, ' - ', lb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', lb_1_info$home_town, ' | Ht (in.): ', lb_1_info$height, ' | Wt (lbs.): ', lb_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 60 TKLs', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 60 TKLs', rows = 7:11)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
lb_1_tbl
gtsave(lb_1_tbl, paste("Draft/Player Tables/", lb_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate LB 1 stats by year
lb_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$defensive_total_tackles),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  lb_1_career_stats <- rbind(lb_1_career_stats, output, fill = TRUE)}
lb_1_career_stats[is.na(lb_1_career_stats)] <- 0
rm(output)
### LB 1 Career Graphs
lb_1_scale <- max(lb_1_career_stats$defensive_total_tackles, na.rm = TRUE)/max(lb_1_career_stats$defensive_passes_defended, na.rm = TRUE)
lb_1_graph <- ggplot(lb_1_career_stats, aes(year)) +
  geom_line(aes(y = defensive_passes_defended), color = gold) +
  geom_line(aes(y = defensive_total_tackles/lb_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(lb_1_career_stats$year), max(lb_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*lb_1_scale, name="Total Tackles")) +
  theme_bw() +
  labs(y = "Passes Defended",
       title = paste(lb_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
lb_1_graph
ggsave(paste('Draft/Player Charts/', lb_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### LB 1 Career Stats
lb_1_career_tbl <- lb_1_career_stats |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, defensive_total_tackles, defensive_solo_tackles, general_fumbles_forced, defensive_sacks, defensive_passes_defended) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = lb_1_info$headshot_url, height = px(50)), paste(lb_1_info$name, ', ',lb_1_info$position, ' - ', lb_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', lb_1_info$home_town, ' | Ht (in.): ', lb_1_info$height, ' | Wt (lbs.): ', lb_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", defensive_passes_defended = "Passes Defended", general_fumbles_forced = "Forced Fumbles",
             defensive_solo_tackles = "Solo Tackles", defensive_total_tackles = "Total Tackles", defensive_sacks = "Sacks")
lb_1_career_tbl
gtsave(lb_1_career_tbl, paste("Draft/Player Tables/", lb_1_info$name, ' Career Stats.png', sep = ''))
rm(lb_1_info, lb_1_ranks, lb_1_stats, lb_combine, lb_ranks, lb_stats_expanded, lb_1_tbl, lb_stats_2023)




### DL Data Loading/Formatting
dl_stats_2023 <- read.csv("Draft/2023 Pos Stats/DL Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         combine_name = word(full_name, 1, 2),
         def_yps = round(defensive_sack_yards/defensive_sacks,1))
dl_combine <- combine_data |>
  filter(position == "DL") |>
  mutate(forty_perc = ifelse(is.na(forty),NA,round(98*rank(-forty, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$forty))+1,0)),
         bench_perc = ifelse(is.na(bench),NA,round(98*rank(bench, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$bench))+1,0)),
         vertical_perc = ifelse(is.na(vertical),NA,round(98*rank(vertical, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$vertical))+1,0)),
         broad_perc = ifelse(is.na(broad_jump),NA,round(98*rank(broad_jump, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$broad_jump))+1,0)),
         cone_perc = ifelse(is.na(cone),NA,round(98*rank(-cone, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$cone))+1,0)),
         shuttle_perc = ifelse(is.na(shuttle),NA,round(98*rank(-shuttle, ties.method = 'max')/sum(combine_data$position == 'DL' & !is.na(combine_data$shuttle))+1,0)))
dl_ranks <- dl_stats_2023 |>
  filter(defensive_total_tackles >= 30 | defensive_sacks >= 5) |>
  mutate(def_syd_rank = rank(-defensive_sack_yards, ties.method = "min"),
         def_sks_rank = rank(-defensive_sacks, ties.method = "min"),
         def_stk_rank = rank(-defensive_solo_tackles, ties.method = "min"),
         def_ttk_rank = rank(-defensive_total_tackles, ties.method = "min"),
         def_ff_rank = rank(-general_fumbles_forced, ties.method = "min"),
         def_yps_rank = rank(-def_yps, ties.method = "min"))
dl_stats_expanded <- dl_stats_2023 |>
  mutate(combine_name = ifelse(combine_name == 'Theo Johnson', 'Theodore Johnson', combine_name)) |>
  left_join(select(dl_combine, player_name, season, forty_perc, bench_perc, vertical_perc, broad_perc, cone_perc, shuttle_perc),
            by = c('year' = 'season', 'combine_name' = 'player_name')) |>
  left_join(select(dl_ranks, athlete_id, def_syd_rank, def_ff_rank, def_stk_rank, def_ttk_rank, def_sks_rank, def_yps_rank), by = 'athlete_id')
### DL 1 Table
dl_1_ranks <- dl_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select('forty_perc', 'bench_perc', 'vertical_perc', 'broad_perc', 'cone_perc', 'shuttle_perc', 'def_ttk_rank', 'def_stk_rank', 
         'def_sks_rank', 'def_syd_rank', 'def_yps_rank', 'def_ff_rank') |>
  rename("forty" = "forty_perc", "bench" = "bench_perc", "vertical" = "vertical_perc", "broad_jump" = "broad_perc",
         "cone" = "cone_perc", "shuttle" = "shuttle_perc", "defensive_sack_yards" = "def_syd_rank",
         "general_fumbles_forced" = "def_ff_rank", "defensive_solo_tackles" = "def_stk_rank",
         "defensive_total_tackles" = "def_ttk_rank", "defensive_sacks" = "def_sks_rank", "def_yps" = "def_yps_rank")
dl_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
dl_1_stats <- select(player_info, year, athlete_id, name) |>
  mutate(name = ifelse(name == 'Theo Johnson', 'Theodore Johnson', name)) |>
  left_join(select(combine_data, season, player_name, forty, bench, vertical, broad_jump, cone, shuttle),
            by = c('year' = 'season', 'name' = 'player_name')) |>
  left_join(select(dl_stats_2023, athlete_id, defensive_total_tackles, defensive_solo_tackles, defensive_sacks, defensive_sack_yards,
                   def_yps, general_fumbles_forced), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)', 'Measurables (w/ perc)',
             'Measurables (w/ perc)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)',
             '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(dl_1_ranks) |>
  rename("40-Yd Dash" = "forty", "Bench Press" = "bench", "Vertical Jump" = "vertical", "Broad Jump" = "broad_jump",
         "Cone Drill" = "cone", "Shuttle Dash" = "shuttle", "Sack Yards" = "defensive_sack_yards",
         "Fumbles Forced" = "general_fumbles_forced", "Solo Tackles" = "defensive_solo_tackles", "Total Tackles" = "defensive_total_tackles",
         "Sacks" = "defensive_sacks", "Yards per Sack" = "def_yps") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 30 TKLs or 5 Sacks" = "V3")
perc_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
perc_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > 100) color_index <- 100
  perc_palette[color_index]}
perc_numbers <- dl_1_stats$`Min. 30 TKLs or 5 Sacks`
perc_colors <- sapply(as.numeric(perc_numbers), perc_map_to_color)
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(dl_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(dl_ranks)) color_index <- nrow(dl_ranks)
  stat_palette[color_index]}
stat_numbers <- dl_1_stats$`Min. 30 TKLs or 5 Sacks`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
dl_1_stats <- cbind(dl_1_stats, perc_colors, stat_colors)
dl_1_tbl <- dl_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = dl_1_info$headshot_url, height = px(50)), paste(dl_1_info$name, ', ',dl_1_info$position, ' - ', dl_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', dl_1_info$home_town, ' | Ht (in.): ', dl_1_info$height, ' | Wt (lbs.): ', dl_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "perc_colors")), 
            locations = cells_body(columns = 'Min. 30 TKLs or 5 Sacks', rows = 1:6)) |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 30 TKLs or 5 Sacks', rows = 7:12)) |>
  cols_label(number = "") |> cols_hide(c(perc_colors, stat_colors))
dl_1_tbl
gtsave(dl_1_tbl, paste("Draft/Player Tables/", dl_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate DL 1 stats by year
dl_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$defensive_total_tackles),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  dl_1_career_stats <- rbind(dl_1_career_stats, output, fill = TRUE)}
dl_1_career_stats[is.na(dl_1_career_stats)] <- 0
rm(output)
### DL 1 Career Graphs
dl_1_scale <- max(dl_1_career_stats$defensive_total_tackles, na.rm = TRUE)/max(dl_1_career_stats$defensive_sacks, na.rm = TRUE)
dl_1_graph <- ggplot(dl_1_career_stats, aes(year)) +
  geom_line(aes(y = defensive_sacks), color = gold) +
  geom_line(aes(y = defensive_total_tackles/dl_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(dl_1_career_stats$year), max(dl_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*dl_1_scale, name="Total Tackles")) +
  theme_bw() +
  labs(y = "Total Sacks",
       title = paste(dl_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
dl_1_graph
ggsave(paste('Draft/Player Charts/', dl_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### DL 1 Career Stats
dl_1_career_tbl <- dl_1_career_stats |>
  mutate(def_yps = round(defensive_sack_yards/defensive_sacks,2)) |>
  left_join(player_gp, by = 'year') |>
  select(year, logo_href, gp, defensive_total_tackles, defensive_solo_tackles, defensive_sacks, defensive_sack_yards, def_yps, general_fumbles_forced) |>
  ungroup() |> gt()  |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = dl_1_info$headshot_url, height = px(50)), paste(dl_1_info$name, ', ',dl_1_info$position, ' - ', dl_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', dl_1_info$home_town, ' | Ht (in.): ', dl_1_info$height, ' | Wt (lbs.): ', dl_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", gp = "Games Started", defensive_sack_yards = "Sack Yards", general_fumbles_forced = "Forced Fumbles",
             defensive_solo_tackles = "Solo Tackles", defensive_total_tackles = "Total Tackles", defensive_sacks = "Sacks", def_yps = "Yards per Sack")
dl_1_career_tbl
gtsave(dl_1_career_tbl, paste("Draft/Player Tables/", dl_1_info$name, ' Career Stats.png', sep = ''))
rm(dl_1_info, dl_1_ranks, dl_1_stats, dl_combine, dl_ranks, dl_stats_expanded, dl_1_tbl, dl_stats_2023, dl_1_career_stats, dl_1_career_tbl, dl_1_graph)







### PK Data Loading/Formatting
pk_stats_2023 <- read.csv("Draft/2023 Pos Stats/PK Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         xp_pct = kicking_extra_points_made/kicking_extra_point_attempts,
         fg_pct = kicking_field_goals_made/kicking_field_goal_attempts,
         fg40_pct = (kicking_field_goals_made40_49 + kicking_field_goals_made50_59)/(kicking_field_goal_attempts40_49 + kicking_field_goal_attempts50_59))
pk_ranks <- pk_stats_2023 |>
  filter(kicking_field_goal_attempts >= 15 & kicking_extra_point_attempts >= 15) |>
  mutate(fg_pct_rank = rank(-fg_pct, ties.method = "min"),
         fg_made_rank = rank(-kicking_field_goals_made, ties.method = "min"),
         fg_long_rank = rank(-kicking_long_field_goal_made, ties.method = "min"),
         fg_40_rank = rank(-fg40_pct, ties.method = "min"),
         xp_pct_rank = rank(-xp_pct, ties.method = "min"))
pk_stats_2023$xp_pct <- percent(pk_stats_2023$xp_pct, accuracy = .1)
pk_stats_2023$fg_pct <- percent(pk_stats_2023$fg_pct, accuracy = .1)
pk_stats_2023$fg40_pct <- percent(pk_stats_2023$fg40_pct, accuracy = .1)
pk_stats_expanded <- pk_stats_2023 |>
  left_join(select(pk_ranks, athlete_id, fg_pct_rank, fg_made_rank, fg_long_rank, fg_40_rank, xp_pct_rank), by = 'athlete_id')
### PK 1 Table
pk_1_ranks <- pk_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select(fg_pct_rank, fg_made_rank, fg_long_rank, fg_40_rank, xp_pct_rank) |>
  rename("fg_pct" = "fg_pct_rank", "kicking_field_goals_made" = "fg_made_rank", "kicking_long_field_goal_made" = "fg_long_rank",
         "fg40_pct" = "fg_40_rank", "xp_pct" = "xp_pct_rank")
pk_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
pk_1_stats <- select(player_info, year, athlete_id, name) |>
  left_join(select(pk_stats_2023, athlete_id, fg_pct, kicking_field_goals_made, kicking_long_field_goal_made,
                   fg40_pct, xp_pct), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(pk_1_ranks) |>
  rename("FG Pct" = "fg_pct", "FGs Made" = "kicking_field_goals_made", "Long FG" = "kicking_long_field_goal_made",
         "40+ Pct" = "fg40_pct", "XP Pct" = "xp_pct") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 15 FG & XP Atts" = "V3")
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(pk_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(pk_ranks)) color_index <- nrow(pk_ranks)
  stat_palette[color_index]}
stat_numbers <- pk_1_stats$`Min. 15 FG & XP Atts`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
pk_1_stats <- cbind(pk_1_stats, stat_colors)
pk_1_tbl <- pk_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = pk_1_info$headshot_url, height = px(50)), paste(pk_1_info$name, ', ',pk_1_info$position, ' - ', pk_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', pk_1_info$home_town, ' | Ht (in.): ', pk_1_info$height, ' | Wt (lbs.): ', pk_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 15 FG & XP Atts', rows = 1:5)) |>
  cols_label(number = "") |> cols_hide(c(stat_colors))
pk_1_tbl
gtsave(pk_1_tbl, paste("Draft/Player Tables/", pk_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate PK 1 stats by year
pk_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$kicking_field_goal_attempts),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  pk_1_career_stats <- rbind(pk_1_career_stats, output, fill = TRUE)}
pk_1_career_stats <- pk_1_career_stats |>
  mutate(xp_pct = 100*kicking_extra_points_made/kicking_extra_point_attempts,
         fg_pct = 100*kicking_field_goals_made/kicking_field_goal_attempts,
         fg40_pct = (kicking_field_goals_made40_49 + kicking_field_goals_made50_59)/(kicking_field_goal_attempts40_49 + kicking_field_goal_attempts50_59))
pk_1_career_stats[is.na(pk_1_career_stats)] <- 0
rm(output)
### PK 1 Career Graphs
pk_1_scale <- max(pk_1_career_stats$xp_pct, na.rm = TRUE)/max(pk_1_career_stats$fg_pct, na.rm = TRUE)
pk_1_graph <- ggplot(pk_1_career_stats, aes(year)) +
  geom_line(aes(y = fg_pct), color = gold) +
  geom_line(aes(y = xp_pct/pk_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(pk_1_career_stats$year), max(pk_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*pk_1_scale, name="Extra Pt Pct")) +
  theme_bw() +
  labs(y = "Field Goal Pct",
       title = paste(pk_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
pk_1_graph
ggsave(paste('Draft/Player Charts/', pk_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### PK 1 Career Stats
pk_1_career_tbl <- pk_1_career_stats |>
  select(year, logo_href, fg_pct, kicking_field_goals_made, kicking_long_field_goal_made, fg40_pct, xp_pct) |>
  mutate(fg_pct = fg_pct/100, xp_pct = xp_pct/100) |>
  ungroup() |> gt()  |>
  fmt_percent(columns = c(fg_pct, xp_pct, fg40_pct), decimals = 1) |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = pk_1_info$headshot_url, height = px(50)), paste(pk_1_info$name, ', ',pk_1_info$position, ' - ', pk_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', pk_1_info$home_town, ' | Ht (in.): ', pk_1_info$height, ' | Wt (lbs.): ', pk_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", fg_pct = "Field Goal Pct", kicking_field_goals_made = "Field Goals Made",
             kicking_long_field_goal_made = "Long Field Goal", fg40_pct = "40+ Field Goal Pct", xp_pct = "Extra Point Pct")
pk_1_career_tbl
gtsave(pk_1_career_tbl, paste("Draft/Player Tables/", pk_1_info$name, ' Career Stats.png', sep = ''))
rm(pk_1_info, pk_1_ranks, pk_1_stats, pk_ranks, pk_stats_expanded, pk_1_tbl, pk_stats_2023)





### P Data Loading/Formatting
p_stats_2023 <- read.csv("Draft/2023 Pos Stats/P Stats.csv", header = TRUE) |>
  mutate(year = 2024,
         punting_punts_inside20pct = punting_punts_inside20pct/100,
         punting_touchback_pct = punting_touchback_pct/100)
p_ranks <- p_stats_2023 |>
  filter(punting_punts >= 30) |>
  mutate(p_gross_rank = rank(-punting_gross_avg_punt_yards, ties.method = "min"),
         p_long_rank = rank(-punting_long_punt, ties.method = "min"),
         p_net_rank = rank(-punting_net_avg_punt_yards, ties.method = "min"),
         p_in20_rank = rank(-punting_punts_inside20pct, ties.method = "min"),
         p_tbk_rank = rank(punting_touchback_pct, ties.method = "min"))
p_stats_2023$punting_gross_avg_punt_yards <- round(p_stats_2023$punting_gross_avg_punt_yards, 1)
p_stats_2023$punting_net_avg_punt_yards <- round(as.numeric(p_stats_2023$punting_net_avg_punt_yards), 1)
p_stats_2023$punting_punts_inside20pct <- percent(p_stats_2023$punting_punts_inside20pct, accuracy = .1)
p_stats_2023$punting_touchback_pct <- percent(p_stats_2023$punting_touchback_pct, accuracy = .1)
p_stats_expanded <- p_stats_2023 |>
  left_join(select(p_ranks, athlete_id, p_gross_rank, p_long_rank, p_net_rank, p_in20_rank, p_tbk_rank), by = 'athlete_id')
### P 1 Table
p_1_ranks <- p_stats_expanded |>
  filter(athlete_id == player_info$athlete_id) |>
  select(p_gross_rank, p_net_rank, p_in20_rank, p_long_rank, p_tbk_rank) |>
  rename("punting_gross_avg_punt_yards" = "p_gross_rank", "punting_long_punt" = "p_long_rank", "punting_net_avg_punt_yards" = "p_net_rank",
         "punting_punts_inside20pct" = "p_in20_rank", "punting_touchback_pct" = "p_tbk_rank")
p_1_info <- select(player_info, year, athlete_id, name, team, position, home_town, height, weight) |>
  left_join(select(roster_data, athlete_id, headshot_url), by = 'athlete_id')
p_1_stats <- select(player_info, year, athlete_id, name) |>
  left_join(select(p_stats_2023, athlete_id, punting_gross_avg_punt_yards, punting_net_avg_punt_yards, punting_punts_inside20pct,
                   punting_long_punt, punting_touchback_pct), by = 'athlete_id') |>
  select(-athlete_id, -name, -year) |>
  rbind(list('2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)', '2023 Production (w/ rank)')) |>
  rbind(p_1_ranks) |>
  rename("Avg Gross Yards" = "punting_gross_avg_punt_yards", "Long Punt" = "punting_long_punt", "Avg Net Yards" = "punting_net_avg_punt_yards",
         "Inside 20 Pct" = "punting_punts_inside20pct", "Touchback Pct" = "punting_touchback_pct") |>
  transpose(keep.names = "metric") |>
  rename("number" = "V1", "group" = "V2", "Min. 30 Punts" = "V3")
stat_palette <- colorRampPalette(c("green", "yellow", "red"))(nrow(p_ranks))
stat_map_to_color <- function(value) {
  if (is.na(value)) {return("grey")}
  color_index <- round(value)
  if (color_index < 1) color_index <- 1
  if (color_index > nrow(p_ranks)) color_index <- nrow(p_ranks)
  stat_palette[color_index]}
stat_numbers <- p_1_stats$`Min. 30 Punts`
stat_colors <- sapply(as.numeric(stat_numbers), stat_map_to_color)
p_1_stats <- cbind(p_1_stats, stat_colors)
p_1_tbl <- p_1_stats |>
  ungroup() |> gt(rowname_col = "metric", groupname_col = "group") |>
  gt_theme_538() |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = p_1_info$headshot_url, height = px(50)), paste(p_1_info$name, ', ',p_1_info$position, ' - ', p_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', p_1_info$home_town, ' | Ht (in.): ', p_1_info$height, ' | Wt (lbs.): ', p_1_info$weight, sep = '')) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_style(style = cell_fill(color = from_column(column = "stat_colors")), 
            locations = cells_body(columns = 'Min. 30 Punts', rows = 1:5)) |>
  cols_label(number = "") |> cols_hide(c(stat_colors))
p_1_tbl
gtsave(p_1_tbl, paste("Draft/Player Tables/", p_1_info$name, ' 2023 Stats.png', sep = ''))
### Populate P 1 stats by year
p_1_career_stats <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = 2023, season_type = "regular") |>
  mutate(year = 2023)
for(i in 2022:2018) {
  ifelse(!is.null(espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i)$punting_punts),
         output <- espn_cfb_player_stats(athlete_id = player_info$athlete_id, year = i), next)
  output <- output |> mutate(year = i)
  p_1_career_stats <- rbind(p_1_career_stats, output, fill = TRUE)}
p_1_career_stats[is.na(p_1_career_stats)] <- 0
rm(output)
### P 1 Career Graphs
p_1_scale <- max(p_1_career_stats$punting_net_avg_punt_yards, na.rm = TRUE)/max(p_1_career_stats$punting_punts_inside20pct, na.rm = TRUE)
p_1_graph <- ggplot(p_1_career_stats, aes(year)) +
  geom_line(aes(y = punting_net_avg_punt_yards), color = gold) +
  geom_line(aes(y = punting_punts_inside20pct/p_1_scale), color = teal) +
  scale_x_continuous(breaks = seq(min(p_1_career_stats$year), max(p_1_career_stats$year), by = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*p_1_scale, name="Inside 20 Perc")) +
  theme_bw() +
  labs(y = "Avg Net Yards",
       title = paste(p_1_info$name, ' Career Stats',sep = ''),
       caption = andrew_caption) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y.left = element_text(color = gold),
        axis.title.y.right = element_text(color = teal))
p_1_graph
gggsave(paste('Draft/Player Charts/', p_1_info$name, ' Career Stats.png', sep = ''), width = 14, height = 10, dpi = "retina")
### P 1 Career Stats
p_1_career_tbl <- p_1_career_stats |>
  select(year, logo_href, punting_gross_avg_punt_yards, punting_net_avg_punt_yards, punting_punts_inside20pct, punting_long_punt, punting_touchback_pct) |>
  mutate(punting_punts_inside20pct = punting_punts_inside20pct/100, punting_touchback_pct = punting_touchback_pct/100) |>
  ungroup() |> gt()  |>
  fmt_percent(columns = c(punting_punts_inside20pct, punting_touchback_pct), decimals = 1) |>
  gt_theme_538() |>
  gt_img_rows(logo_href) |>
  tab_source_note("Table: Andrew Kessler | data: cfbfastR | @DuvalAndrew904") |>
  tab_header(title = html(web_image(url = p_1_info$headshot_url, height = px(50)), paste(p_1_info$name, ', ',p_1_info$position, ' - ', p_1_info$team,sep = '')),
             subtitle = paste('Hometown: ', p_1_info$home_town, ' | Ht (in.): ', p_1_info$height, ' | Wt (lbs.): ', p_1_info$weight, sep = '')) |>
  opt_align_table_header(align = "center") |> cols_align("center") |>
  cols_label(logo_href = "Team", punting_gross_avg_punt_yards = "Avg Gross Yards", punting_net_avg_punt_yards = "Avg Net Yards",
             punting_punts_inside20pct = "Inside 20 Pct", punting_long_punt = "Long Punt", punting_touchback_pct = "Touchback Pct")
p_1_career_tbl
gtsave(p_1_career_tbl, paste("Draft/Player Tables/", p_1_info$name, ' Career Stats.png', sep = ''))
rm(p_1_info, p_1_ranks, p_1_stats, p_ranks, p_stats_expanded, p_1_tbl, p_stats_2023, p_1_career_tbl, p_1_career_stats, p_1_graph)

