all_combine <- load_combine(2000:2023)
all_draft <- load_draft_picks(2000:2023)
all_draft$team <- str_replace_all(all_draft$team, c('OAK' = 'LV', 'GNB' = 'GB', 'KAN' = 'KC', 'LAR' = 'LA', 'LVR' = 'LV',
                                                    'NOR' = 'NO', 'NWE' = 'NE', 'SDG' = 'LAC', 'SFO' = 'SF','STL' = 'LA', 'TAM' = 'TB'))
all_draft$college <- str_replace_all(all_draft$college, c('St[.]' = 'State', 'Miami [(]FL[)]' = 'Miami',
                                                          'North Carolina State' = 'NC State',
                                                          '(?<!Southern )Mississippi(?! St.)' = 'Ole Miss',
                                                          'Col[.]' = 'College','Central Florida' = 'UCF',
                                                          'Southern Miss(?!i)' = 'Southern Mississippi', 'Hawaii' = 'Hawai\'i'))
all_draft$side <- str_replace_all(all_draft$side, c('O(?!f)' = 'Offense', 'D(?!e)' = 'Defense', '(?<!n)S(?!p)' = 'Special Teams'))
cfb_teams <- load_cfb_teams()
player_info <- load_players()

all_draft_data <- all_draft |>
  left_join(cfb_teams |> select(school, mascot, conference, classification, logo, state), by = c('college' = 'school')) |>
  filter(!is.na(mascot) & !is.na(conference) & !is.na(logo))

player_draft_data <- select(all_draft, c(season, round, pick, team, pfr_player_id, pfr_player_name,
                                         position, category, side, college, age)) |>
  left_join(all_combine |> select(pfr_id, ht, wt), by = c('pfr_player_id' = 'pfr_id')) |>
  filter(!is.na(pfr_player_id) & !is.na(ht) & !is.na(wt)) |>
  mutate(height = as.numeric(substr(ht, 1, 1))*12 + as.numeric(sub('.*-','',ht)),
         height_m = height*.0254,
         weight_kg = wt*.453592,
         bmi = weight_kg/(height_m^2))

most_pop_mascot <- all_draft_data |>
  group_by(team, mascot) |>
  summarise(num = n()) |>
  arrange(-num) |>
  group_by(team) |>
  mutate(rank = rank(-num, ties.method = "min")) |>
  filter(rank == 1) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
most_pop_mascot_tbl <- most_pop_mascot |>
  relocate(team_wordmark) |>
  subset(select = -c(team, rank)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |> 
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", num = "Number of Picks") |> opt_row_striping() |>
  tab_header(title = "NFL teams by most popular college mascots drafted",
             subtitle = "2000-2023") |>
  gt_theme_538()
most_pop_mascot_tbl
gtsave(most_pop_mascot_tbl, "Draft/MostPopMascot.png")

most_pop_conference <- all_draft_data |>
  group_by(team, conference) |>
  summarise(num = n()) |>
  arrange(-num) |>
  group_by(team) |>
  mutate(rank = rank(-num, ties.method = "min")) |>
  filter(rank == 1) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
most_pop_conference_tbl <- most_pop_conference |>
  relocate(team_wordmark) |>
  subset(select = -c(team, rank)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |> 
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", num = "Number of Picks") |> opt_row_striping() |>
  tab_header(title = "NFL teams by most popular college conference drafted",
             subtitle = "2000-2023") |>
  gt_theme_538()
most_pop_conference_tbl
gtsave(most_pop_conference_tbl, "Draft/MostPopConference.png")

most_pop_school <- all_draft_data |>
  group_by(team, college, logo) |>
  summarise(num = n()) |>
  arrange(-num) |>
  group_by(team) |>
  mutate(rank = rank(-num, ties.method = "min")) |>
  filter(rank == 1) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
most_pop_school_tbl <- most_pop_school |>
  relocate(team_wordmark, logo) |>
  subset(select = -c(team, rank, college)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", num = "Number of Picks", logo = "College") |> opt_row_striping() |>
  tab_header(title = "NFL teams by most popular college drafted",
             subtitle = "2000-2023") |>
  gt_theme_538()
most_pop_school_tbl
gtsave(most_pop_school_tbl, "Draft/MostPopSchool.png")

### Avg NFL Draft Height
height_by_team <- player_draft_data |>
  group_by(team) |>
  summarise(avg_height = mean(height)) |>
  arrange(-avg_height) |>
  mutate(rank = rank(-avg_height, ties.method = "min")) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
height_by_team_tbl <- height_by_team |>
  relocate(team_wordmark) |>
  subset(select = -c(team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", avg_height = "Average Height (in.)") |> opt_row_striping() |>
  tab_header(title = "NFL teams by average draftee height",
             subtitle = "2000-2023, combine measurements") |>
  gt_theme_538()
height_by_team_tbl
gtsave(height_by_team_tbl, "Draft/HeightbyTeam.png")

### Avg NFL Draft Height
weight_by_team <- player_draft_data |>
  group_by(team) |>
  summarise(avg_weight = mean(wt)) |>
  arrange(-avg_weight) |>
  mutate(rank = rank(-avg_weight, ties.method = "min")) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
weight_by_team_tbl <- weight_by_team |>
  relocate(team_wordmark) |>
  subset(select = -c(team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", avg_weight = "Average Weight (lb.)") |> opt_row_striping() |>
  tab_header(title = "NFL teams by average draftee weight",
             subtitle = "2000-2023, combine measurements") |>
  gt_theme_538()
weight_by_team_tbl
gtsave(weight_by_team_tbl, "Draft/WeightbyTeam.png")

### Avg NFL Draft BMI
bmi_by_team <- player_draft_data |>
  group_by(team) |>
  summarise(avg_bmi = mean(bmi)) |>
  arrange(-avg_bmi) |>
  mutate(rank = rank(-avg_bmi, ties.method = "min")) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
bmi_by_team_tbl <- bmi_by_team |>
  relocate(team_wordmark) |>
  subset(select = -c(team)) |>
  ungroup() |> gt() |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", avg_bmi = "Average BMI") |> opt_row_striping() |>
  tab_header(title = "NFL teams by average draftee BMI",
             subtitle = "2000-2023, combine measurements") |>
  gt_theme_538()
bmi_by_team_tbl
gtsave(bmi_by_team_tbl, "Draft/BMIbyTeam.png")

f_rd_off_def <- all_draft |>
  select(season, round, pick, team, pfr_player_id, pfr_player_name, position, category, side, college, age) |>
  filter(round == 1) |>
  group_by(team, side) |>
  summarise(num = n()) |>
  mutate(perc = num/sum(num)) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('team' = 'team_abbr'))
f_rd_off_def_tbl <- f_rd_off_def |>
  relocate(team_wordmark) |>
  subset(select = -c(team)) |>
  ungroup() |> gt() |>
  fmt_percent(columns = perc, decimals = 1) |>
  gt_img_rows(team_wordmark) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  cols_label(team_wordmark = "Team", side = "Offense/Defense", num = "Number of Picks", perc = "Percent") |> 
  opt_row_striping() |>
  tab_header(title = "NFL teams by 1st Round Selections",
             subtitle = "2000-2023") |>
  gt_theme_538()
f_rd_off_def_tbl
gtsave(f_rd_off_def_tbl, "Draft/fRdOffDef.png")

### OL v DL height
OL_v_DL_Ht <- player_draft_data |>
  group_by(team, category) |>
  summarise(avg_ht = mean(height)) |>
  filter(category == "OL" | category == "DL") |>
  mutate(OL_height = ifelse(category == "OL", avg_ht, ""),
         DL_height = ifelse(category == "DL", avg_ht, "")) |>
  group_by(team) |>
  dplyr::summarize(OL_height = paste(unique(OL_height), collapse = '#'),
                   DL_height = paste(unique(DL_height), collapse = '#')) |>
  mutate_at(c("OL_height", "DL_height"), str_replace, "#", "") |>
  transform(OL_height = as.numeric(OL_height),
            DL_height = as.numeric(DL_height)) |>
  left_join(teams_colors_logos |> select(team_abbr, team_logo_espn), by = c('team' = 'team_abbr'))
ggplot(OL_v_DL_Ht, aes(x = OL_height, y = DL_height)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  geom_hline(yintercept = mean(OL_v_DL_Ht$DL_height), linetype = "dashed") +
  geom_vline(xintercept = mean(OL_v_DL_Ht$OL_height), linetype = "dashed") +
  labs(x = "Avg OL Height (in.)",
       y = "Avg DL Height (in.)",
       title = "Average height for OL/DL draft picks",
       subtitle = "2000-2023",
       caption = andrew_caption) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle
ggsave('Draft/Avg OL v DL Ht.png',  width = 14, height = 10, dpi = "retina")






