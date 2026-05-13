install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
1
1
library(StatsBombR)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(gt)
library(shiny)
library(shinydashboard)
library(shinylive)
library(bslib)
library(DT)
install.packages("tinytex")
library(tinytex)
N
install.packages("shinylive")
ninstall.packages("shinydashboard")
install.packages("gt")
install.packages("bslib")
install.packages("DT")
instal

competitions <- FreeCompetitions()

view(competitions)


competitions |>
  select(competition_id, competition_name, season_name) |>
  distinct() |>
  head(20)
)

#AFCON 2023 ----

afcon <- competitions %>% 
  filter(competition_id == 1267)

afcon_matches <- FreeMatches(Competitions = afcon)

view(afcon_matches)



teams <- afcon_matches %>%
  select(home_team.home_team_name, away_team.away_team_name) %>%
  pivot_longer(cols = everything(), values_to = "team") %>%
  select(team) %>%
  distinct()

view(teams)

# Total Tournament Goals 

total_afcon_goals <- afcon_matches %>%
  select(home_team.home_team_name, away_team.away_team_name, home_score, away_score) %>%
  mutate(total_goals = home_score + away_score) %>%
  summarise(total_goals = sum(total_goals))

# Total Team Goals per round ----

# Get all shots/goals from all matches
all_events <- bind_rows(
  lapply(unique(afcon_matches$match_id), function(id) {
    afcon_matches %>%
      filter(match_id == id) %>%
      get.matchFree()
  })
)

# Goals per team per game
goals_table <- all_events %>%
  filter(type.name == "Shot", shot.outcome.name == "Goal") %>%
  filter(period != 5) %>% 
  group_by(team.name, match_id) %>%
  summarise(goals = n(), .groups = "drop") %>%
  left_join(afcon_matches %>% select(match_id, competition_stage.name), by = "match_id")

# Total goals per team with breakdown
goalsperteam_table <- goals_table %>%
  group_by(team.name) %>%
  summarise(
    group_stage_goals = sum(goals[competition_stage.name == "Group Stage"], na.rm = TRUE),
    ro16_goals = sum(goals[competition_stage.name == "Round of 16"], na.rm = TRUE),
    quarterfinal_goals = sum(goals[competition_stage.name == "Quarter-finals"], na.rm = TRUE),
    semifinal_goals = sum(goals[competition_stage.name == "Semi-finals"], na.rm = TRUE),
    final_goals = sum(goals[competition_stage.name == "Final"], na.rm = TRUE),
    total_goals = sum(goals, na.rm = TRUE)
  ) %>%
  arrange(desc(total_goals))

view(goalsperteam_table)

goals_cumulative <- goals_table %>%
  mutate(competition_stage.name = factor(competition_stage.name, levels = c(
    "Group Stage", "Round of 16", "Quarter-finals", "Semi-finals", "Final"
  ))) %>%
  group_by(team.name, competition_stage.name) %>%
  summarise(stage_goals = sum(goals), .groups = "drop") %>%
  group_by(team.name) %>%
  arrange(competition_stage.name, .by_group = TRUE) %>%
  mutate(cumulative_goals = cumsum(stage_goals)) %>%
  ungroup()

cumgoals <- ggplot(goals_cumulative, aes(x = competition_stage.name, y = cumulative_goals,
                                         colour = team.name, group = team.name,
                                         text = paste("Team:", team.name,
                                                      "<br>Round:", competition_stage.name,
                                                      "<br>Cumulative Goals:", cumulative_goals))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(aes(label = cumulative_goals), nudge_y = 0.3, size = 3, show.legend = FALSE) +
  labs(
    title = "Cumulative Goals Throughout AFCON 2023",
    x = "Round",
    y = "Cumulative Goals",
    colour = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggplotly(cumgoals, tooltip = "text")

# Where each team finished -----

team_rounds <- afcon_matches %>%
  pivot_longer(cols = c(home_team.home_team_name, away_team.away_team_name), 
               values_to = "team") %>%
  select(team, competition_stage.name) %>%
  distinct() %>%
  group_by(team) %>%
  slice_max(order_by = factor(competition_stage.name, levels = c(
    "Group Stage",
    "Round of 16",
    "Quarter-finals",
    "Semi-finals",
    "3rd Place Final",
    "Final"
  )), n = 1) %>%
  ungroup() %>%
  mutate(competition_stage.name = ifelse(competition_stage.name == "3rd Place Final", "Semi-final → 3rd Place Final", competition_stage.name)) %>%
  mutate(competition_stage.name = factor(competition_stage.name, levels = c(
    "Final",
    "Semi-finals",
    "Semi-final → 3rd Place Final",
    "Quarter-finals",
    "Round of 16",
    "Group Stage"
  ))) %>%
  arrange(competition_stage.name)

view(team_rounds)





#NIGERIA SECTION ----

nigeria_matches <- afcon_matches %>%
  filter(home_team.home_team_name == "Nigeria" | away_team.away_team_name == "Nigeria")

view(nigeria_matches)

# Nigeria game 1 
nigeria_groupstage_game1 <- nigeria_matches %>% 
  filter(match_id == 3920398)
nigeria_groupstage_game1_events <- nigeria_groupstage_game1 %>% 
  get.matchFree()
nigeria_groupstage_game1_xg <- nigeria_groupstage_game1_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_groupstage_game1_totalteamxg <- nigeria_groupstage_game1_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_groupstage_game1_totalteamxg

#Nigeria Game 2
nigeria_groupstage_game2 <- nigeria_matches %>% 
  filter(match_id == 3920409)
nigeria_groupstage_game2_events <- nigeria_groupstage_game2 %>% 
  get.matchFree()
nigeria_groupstage_game2_xg <- nigeria_groupstage_game2_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
view(nigeria_groupstage_game2_xg)
nigeria_groupstage_game2_totalteamxg <- nigeria_groupstage_game2_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_groupstage_game2_totalteamxg

#Nigeria Group Stage Game 3 
nigeria_groupstage_game3 <- nigeria_matches %>% 
  filter(match_id == 3920386)
nigeria_groupstage_game3_events <- nigeria_groupstage_game3 %>% 
  get.matchFree()
nigeria_groupstage_game3_xg <- nigeria_groupstage_game3_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
view(nigeria_groupstage_game3_xg)
nigeria_groupstage_game3_totalteamxg <- nigeria_groupstage_game3_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_groupstage_game3_totalteamxg

#Nigeria Round Of 16
nigeria_RO16 <- nigeria_matches %>% 
  filter(match_id == 3922238)
nigeria_RO16_events <- nigeria_RO16 %>% 
  get.matchFree()
nigeria_RO16_xg <- nigeria_RO16_events  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_RO16_totalteamxg <- nigeria_RO16_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_RO16_totalteamxg

#Nigeria Quarter Final
nigeria_quarterfinal <- nigeria_matches %>% 
  filter(match_id == 3922321)
nigeria_quarterfinal_event <- nigeria_quarterfinal %>% 
  get.matchFree()
nigeria_quarterfinal_xg <- nigeria_quarterfinal_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_quarterfinal_totalteamxg <- nigeria_quarterfinal_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_quarterfinal_totalteamxg

#Nigeria Semi Final
nigeria_semifinal <- nigeria_matches %>% 
  filter(match_id == 3922837)
nigeria_semifinal_event <- nigeria_semifinal %>% 
  get.matchFree()
nigeria_semifinal_xg <- nigeria_semifinal_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_semifinal_totalteamxg <- nigeria_semifinal_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

nigeria_semifinal_totalteamxg

# Nigeria Final 
nigeria_final <- nigeria_matches %>% 
  filter(match_id == 3923881)
nigeria_final_event <- nigeria_final %>% 
  get.matchFree()
nigeria_final_xg <- nigeria_final_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_final_totalteamxg <- nigeria_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
nigeria_final_totalteamxg

nigeria_totalcompetiton_xg <- nigeria_groupstage_game1_totalteamxg$total_xg + nigeria_groupstage_game2_totalteamxg$total_xg + nigeria_groupstage_game3_totalteamxg$total_xg + nigeria_RO16_totalteamxg$total_xg + nigeria_quarterfinal_totalteamxg$total_xg + nigeria_semifinal_totalteamxg$total_xg + nigeria_final_totalteamxg$total_xg
#9.754533



combinedxg_IvoryCoast_Nigeria <- data.frame(
  Team = c("Ivory Coast", "Nigeria"),
  Total_xG = c(ivorycoast_totalcompetiton_xg, nigeria_totalcompetiton_xg)
)

nigeria_xg_table <- data.frame(
  team = "Nigeria",
  round = c("Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3", 
            "Round of 16", "Quarter Final", "Semi Final", "Final"),
  xg = c(
    round(nigeria_groupstage_game1_totalteamxg$total_xg, 2),
    round(nigeria_groupstage_game2_totalteamxg$total_xg, 2),
    round(nigeria_groupstage_game3_totalteamxg$total_xg, 2),
    round(nigeria_RO16_totalteamxg$total_xg, 2),
    round(nigeria_quarterfinal_totalteamxg$total_xg, 2),
    round(nigeria_semifinal_totalteamxg$total_xg, 2),
    round(nigeria_final_totalteamxg$total_xg, 2)
  )
)

ivorycoast_xg_table <- data.frame(
  team = "Côte d'Ivoire",
  round = c("Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3", 
            "Round of 16", "Quarter Final", "Semi Final", "Final"),
  xg = c(
    round(ivorycoast_groupstage_game1_totalteamxg$total_xg, 2),
    round(ivorycoast_groupstage_game2_totalteamxg$total_xg, 2),
    round(ivorycoast_groupstage_game3_totalteamxg$total_xg, 2),
    round(ivorycoast_ro16_totalteamxg$total_xg, 2),
    round(ivorycoast_quarterfinal_totalteamxg$total_xg, 2),
    round(ivorycoast_semifinal_totalteamxg$total_xg, 2),
    round(ivorycoast_final_totalteamxg$total_xg, 2)
  )
)

# Add totals row to each
nigeria_xg_table <- bind_rows(
  nigeria_xg_table,
  data.frame(team = "Nigeria", round = "TOTAL", xg = round(nigeria_totalcompetiton_xg, 2))
)

ivorycoast_xg_table <- bind_rows(
  ivorycoast_xg_table,
  data.frame(team = "Côte d'Ivoire", round = "TOTAL", xg = round(ivorycoast_totalcompetiton_xg, 2))
)

# View them
nigeria_xg_table
ivorycoast_xg_table

# Or combined into one table
combined_xg_table <- bind_rows(nigeria_xg_table, ivorycoast_xg_table)
combined_xg_table



# XG per game comparison per round for Ivory Coast and Nigeria 
plot_data <- combined_xg_table %>%
  filter(round != "TOTAL") %>%
  mutate(round = factor(round, levels = c(
    "Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3",
    "Round of 16", "Quarter Final", "Semi Final", "Final"
  )))

ggplot(plot_data, aes(x = round, y = xg, colour = team, group = team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(aes(label = xg), nudge_y = 0.1, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Nigeria" = "#008000",
    "Côte d'Ivoire" = "#FF6600"
  )) +
  labs(
    title = "xG Throughout the AFCON Tournament",
    subtitle = "Nigeria vs Côte d'Ivoire",
    x = "Round",
    y = "xG",
    colour = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )



#Accumulative xg graph for two finalists throughout the tournament 

plot_data_cumulative <- combined_xg_table %>%
  filter(round != "TOTAL") %>%
  mutate(round = factor(round, levels = c(
    "Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3",
    "Round of 16", "Quarter Final", "Semi Final", "Final"
  ))) %>%
  group_by(team) %>%
  mutate(cumulative_xg = cumsum(xg)) %>%
  ungroup()

ggplot(plot_data_cumulative, aes(x = round, y = cumulative_xg, colour = team, group = team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(aes(label = round(cumulative_xg, 2)), nudge_y = 0.3, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Nigeria" = "#008000",
    "Côte d'Ivoire" = "#FF6600"
  )) +
  labs(
    title = "Cumulative xG Throughout the AFCON Tournament",
    subtitle = "Nigeria vs Côte d'Ivoire",
    x = "Round",
    y = "Cumulative xG",
    colour = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )










# AFCON Final

final_afcon <- afcon_matches %>% 
  filter(match_id == 3923881)

view(final_afcon)
final_events <- final_afcon %>% 
  get.matchFree()

view(final_events)
glimpse(final_events)

xg_final <- final_events %>% 
  filter(type.name == "Shot") %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg)
view(xg_final)


xg_plot <- xg_final %>%
  mutate(time = minute + second / 60) %>%   # create continuous match time
  arrange(possession_team.name, time) %>%
  group_by(possession_team.name) %>%
  mutate(cum_xg = cumsum(shot.statsbomb_xg)) %>%
  ungroup()


ggplot(xg_plot, aes(x = time, y = cum_xg, colour = possession_team.name)) +
  geom_step(size = 1.2) +
  labs(
    title = "Cumulative Expected Goals (xG)",
    x = "Match Time (minutes)",
    y = "Cumulative xG",
    colour = "Team"
  ) +
  theme_minimal()





#Crafting a Table ----

# Who had more possession ----

possession_nigeria_final <- final_events %>%
  summarise(
    nigeria_poss = round(mean(possession_team.name == "Nigeria") * 100, 1)
  )
possession_ivorycoast_final <- final_events %>% 
  summarise(
    ivc_poss = round(mean(possession_team.name == "Côte d'Ivoire") * 100, 1)
  )

possession_final <- final_events %>% 
  summarise(
    nigeria_poss = round(mean(possession_team.name == "Nigeria") * 100, 1),
    ivc_poss = round(mean(possession_team.name == "Côte d'Ivoire") * 100, 1)
  )

#xG per team in the final ----

# Nigeria

nigeria_final <- nigeria_matches %>% 
  filter(match_id == 3923881)
nigeria_final_event <- nigeria_final %>% 
  get.matchFree()
nigeria_final_xg <- nigeria_final_event  %>% 
  filter(type.name == "Shot") %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_final_totalteamxg <- nigeria_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
nigeria_final_totalteamxg
nigeria_final_totalteamxg$total_xg

# Ivory Coast

ivorycoast_final <- ivorycoast_matches %>% 
  filter(match_id == 3923881)
ivorycoast_final_event <- ivorycoast_final %>% 
  get.matchFree()
ivorycoast_final_xg <- ivorycoast_final_event  %>% 
  filter(type.name == "Shot") %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_final_totalteamxg <- ivorycoast_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
ivorycoast_final_totalteamxg

# Big Chances ----

bc_nigeria_final <- final_events %>%
  filter(type.name == "Shot", possession_team.name == "Nigeria",
         shot.statsbomb_xg > 0.3) %>%
  nrow()

bc_ivorycoast_final <- final_events %>% 
  filter(type.name == "Shot", possession_team.name == "Côte d'Ivoire",
         shot.statsbomb_xg > 0.3) %>% 
  nrow()

#Total Shots per team ---- 

nigeria_shots_final <- final_events %>% 
  filter(type.name == "Shot", possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_shots_final <- final_events %>% 
  filter(type.name == "Shot", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

#Shots on targets per team ----
nigeria_sot_final <- final_events %>% 
  filter(type.name == "Shot", shot.outcome.name %in% c("Saved", "Goal"), possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_sot_final <- final_events %>% 
  filter(type.name == "Shot", shot.outcome.name %in% c("Saved", "Goal", possession_team.name == "Côte d'Ivoire")) %>% 
  nrow()

#Corners ----

nigeria_corners_final <- final_events %>% 
  filter(pass.type.name == "Corner", possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_corner_final <- final_events %>% 
  filter(pass.type.name == "Corner", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

#Fouls

nigeria_fouls_final <- final_events %>% 
  filter(type.name == "Foul Committed", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

ivorycoast_fouls_final <- final_events %>% 
  filter(type.name == "Foul Committed", possession_team.name == "Nigeria") %>% 
  nrow()

#Total passes in the final

nigeria_passes_final <- final_events %>% 
  filter(type.name == "Pass", possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_passes_final <- final_events %>% 
  filter(type.name == "Pass", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

#Tackles

nigeria_tackles_final <- final_events %>% 
  filter(duel.type.name == "Tackle", possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_tackles_final <- final_events %>% 
  filter(duel.type.name == "Tackle", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

#Free Kicks 

nigeria_freekicks_final <- final_events %>% 
  filter(pass.type.name == "Free Kick", possession_team.name == "Nigeria") %>% 
  nrow()

ivorycoast_freekicks_final <- final_events %>% 
  filter(pass.type.name == "Free Kick", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

#Yellow Cards 
nigeria_yellowcards_final <- final_events %>% 
  filter(bad_behaviour.card.name == "Yellow Card", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

ivorycoast_yellowcards_final <- final_events %>% 
  filter(bad_behaviour.card.name == "Yellow Card", possession_team.name == "Nigeria") %>% 
  nrow()

# Red Cards

nigeria_redcard_final <- final_events %>% 
  filter(bad_behaviour.card.name == "Red Card", possession_team.name == "Côte d'Ivoire") %>% 
  nrow()

ivorycoast_redcard_final <- final_events %>% 
  filter(bad_behaviour.card.name == "Red Card", possession_team.name == "Nigeria") %>% 
  nrow()

final_stats <- data_frame(
  Nigeria = c(
    possession_nigeria_final,
    round(nigeria_final_totalteamxg$total_xg, 2),
    bc_nigeria_final,
    nigeria_shots_final,
    nigeria_sot_final,
    nigeria_corners_final,
    nigeria_fouls_final,
    nigeria_passess_final, 
    nigeria_tackles_final, 
    nigeria_freekicks_final,
    nigeria_yellowcards_final, 
    nigeria_redcard_final
  ),
  Stat = c("Possession (%)", "xG", "Big Chance", "Total Shots", "Shots on Target", "Corners", "Fouls", "Passes", "Tackles", "Free Kicks", "Yellow Cards", "Red Cards"),
  `Côte d'Ivoire` = c(
    possession_ivorycoast_final,
    round(ivorycoast_final_totalteamxg$total_xg, 2),
    bc_ivorycoast_final,
    ivorycoast_shots_final,
    ivorycoast_sot_final,
    ivorycoast_corner_final,
    ivorycoast_fouls_final,
    ivorycoast_passes_final,
    ivorycoast_tackles_final,
    ivorycoast_freekicks_final,
    ivorycoast_yellowcards_final,
    ivorycoast_redcard_final
  ),
  check.names = FALSE
)

table <- match_stats %>%
  gt() %>%
  tab_header(
    title    = md("**AFCON 2023 Final**"),
    subtitle = md("**Nigeria vs Côte d'Ivoire**")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_label(
    Nigeria = md("🇳🇬 **Nigeria**"),
    Stat = "",
    `Côte d'Ivoire` = md("🇨🇮 **Côte d'Ivoire**")
  ) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 1,   decimals = 1) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 2,   decimals = 2) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 3:12, decimals = 0) %>%
  tab_style(
    style = list(cell_fill(color = "#1e1e2e"),
                     cell_text(color = "white", weight = "bold", size = px(16))),
    locations = cells_title()
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#2a2a3e"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#12421a"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = "Nigeria")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#5a2000"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = "Côte d'Ivoire")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#1e1e2e"),
                     cell_text(color = "#cccccc", style = "italic")),
    locations = cells_body(columns = "Stat")
  ) %>%
  tab_style(
    style  = cell_fill(color = "#161626"),
    locations = cells_body(rows = seq(1, 12, 2))
  ) %>%
  tab_style(
    style = cell_fill(color = "#12421a"),
    locations = cells_body(columns = "Nigeria", rows = seq(1, 12, 2))
  ) %>%
  tab_style(
    style     = cell_fill(color = "#5a2000"),
    locations = cells_body(columns = "Côte d'Ivoire", rows = seq(1, 12, 2))
  ) %>%
  tab_options(
    table.background.color = "#1e1e2e",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.color = "#444466",
    data_row.padding = px(10),
    table.font.size = px(14)
  ) %>%
  tab_source_note(source_note = md("*Data: StatsBomb*"))



###################################
#Tournament overview ivory Coast 


ivory_coast_timeline <- ivorycoast_matches %>%
  mutate(
    opponent = if_else(home_team.home_team_name == "Côte d'Ivoire", away_team.away_team_name, home_team.home_team_name),
    ivc_score = if_else(home_team.home_team_name == "Côte d'Ivoire", home_score, away_score),
    opp_score = if_else(home_team.home_team_name == "Côte d'Ivoire",away_score, home_score),
    result = case_when(
      ivc_score > opp_score ~ "Win",
      ivc_score < opp_score ~ "Loss",
      TRUE ~ "Draw"
    )
  ) %>%
  select(competition_stage.name, opponent, ivc_score, opp_score, result)

ivory_coast_timeline

saveRDS(ivory_coast_timeline, "ivory_coast_timeline.rds")

#xG per game 


ivorycoast_matches <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | away_team.away_team_name == "Côte d'Ivoire")
view(ivorycoast_matches)

#Ivory coast groupstage game 1 
ivorycoast_groupstage_game1 <- ivorycoast_matches %>% 
  filter(match_id == 3920384)
ivorycoast_groupstage_game1_events <- ivorycoast_groupstage_game1 %>% 
  get.matchFree()
ivorycoast_groupstage_game1_xg <- ivorycoast_groupstage_game1_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_groupstage_game1_totalteamxg <- ivorycoast_groupstage_game1_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_groupstage_game1_totalteamxg

#Ivory Coast Group Stage game 2 

ivorycoast_groupstage_game2 <- ivorycoast_matches %>% 
  filter(match_id == 3920398)
ivorycoast_groupstage_game2_events <- ivorycoast_groupstage_game2 %>% 
  get.matchFree()
ivorycoast_groupstage_game2_xg <- ivorycoast_groupstage_game2_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_groupstage_game2_totalteamxg <- ivorycoast_groupstage_game2_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_groupstage_game2_totalteamxg

#Ivory Coast Group Stage game 3 

ivorycoast_groupstage_game3 <- ivorycoast_matches %>% 
  filter(match_id == 3920408)
ivorycoast_groupstage_game3_events <- ivorycoast_groupstage_game3 %>% 
  get.matchFree()
ivorycoast_groupstage_game3_xg <- ivorycoast_groupstage_game3_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_groupstage_game3_totalteamxg <- ivorycoast_groupstage_game3_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_groupstage_game3_totalteamxg

#Ivory Coast Round of 16 

ivorycoast_ro16 <- ivorycoast_matches %>% 
  filter(match_id == 3922659)
ivorycoast_ro16_events <- ivorycoast_ro16 %>% 
  get.matchFree()
ivorycoast_ro16_xg <- ivorycoast_ro16_events %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_ro16_totalteamxg <- ivorycoast_ro16_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_ro16_totalteamxg

#Ivory Coast Quarter Final

ivorycoast_quarterfinal <- ivorycoast_matches %>% 
  filter(match_id == 3922242)
ivorycoast_quarterfinal_event <- ivorycoast_quarterfinal %>% 
  get.matchFree()
ivorycoast_quarterfinal_xg <- ivorycoast_quarterfinal_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_quarterfinal_totalteamxg <- ivorycoast_quarterfinal_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_quarterfinal_totalteamxg

#Ivory Coast Semi Final

ivorycoast_semifinal <- ivorycoast_matches %>% 
  filter(match_id == 3922838)
ivorycoast_semifinal_event <- ivorycoast_semifinal %>% 
  get.matchFree()
ivorycoast_semifinal_xg <- ivorycoast_semifinal_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_semifinal_totalteamxg <- ivorycoast_semifinal_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

ivorycoast_semifinal_totalteamxg


#Ivory Coast Final

ivorycoast_final <- ivorycoast_matches %>% 
  filter(match_id == 3923881)
ivorycoast_final_event <- ivorycoast_final %>% 
  get.matchFree()
ivorycoast_final_xg <- ivorycoast_final_event  %>% 
  filter(type.name == "Shot", period != 5) %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_final_totalteamxg <- ivorycoast_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
ivorycoast_final_totalteamxg

#Ivory Coast total team xg

ivorycoast_totalcompetiton_xg <- ivorycoast_groupstage_game1_totalteamxg$total_xg + ivorycoast_groupstage_game2_totalteamxg$total_xg + ivorycoast_groupstage_game3_totalteamxg$total_xg + ivorycoast_ro16_totalteamxg$total_xg + ivorycoast_quarterfinal_totalteamxg$total_xg + ivorycoast_semifinal_totalteamxg$total_xg + ivorycoast_final_totalteamxg$total_xg
#7.605106


ivorycoast_xg_table <- data.frame(
  team = "Côte d'Ivoire",
  round = c("Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3", 
            "Round of 16", "Quarter Final", "Semi Final", "Final"),
  xg = c(
    round(ivorycoast_groupstage_game1_totalteamxg$total_xg, 2),
    round(ivorycoast_groupstage_game2_totalteamxg$total_xg, 2),
    round(ivorycoast_groupstage_game3_totalteamxg$total_xg, 2),
    round(ivorycoast_ro16_totalteamxg$total_xg, 2),
    round(ivorycoast_quarterfinal_totalteamxg$total_xg, 2),
    round(ivorycoast_semifinal_totalteamxg$total_xg, 2),
    round(ivorycoast_final_totalteamxg$total_xg, 2)
  )
)

plot_data_cumulative_ivory <- ivorycoast_xg_table %>%
  filter(round != "TOTAL") %>%
  mutate(round = factor(round, levels = c(
    "Group Stage Game 1", "Group Stage Game 2", "Group Stage Game 3",
    "Round of 16", "Quarter Final", "Semi Final", "Final"
  ))) %>%
  group_by(team) %>%
  mutate(cumulative_xg = cumsum(xg)) %>%
  ungroup()


cuumulative_ivorycoast_xgplot <- ggplot(plot_data_cumulative_ivory, aes(x = round, y = cumulative_xg, colour = team, group = team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(aes(label = round(cumulative_xg, 2)), nudge_y = 0.3, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Côte d'Ivoire" = "#FF6600"
  )) +
  labs(
    title = "Cumulative xG Throughout the AFCON Tournament",
    subtitle = "Côte d'Ivoire",
    x = "Round",
    y = "Cumulative xG",
    colour = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

saveRDS(cuumulative_ivorycoast_xgplot, "cuumulative_ivorycoast_xgplot.rds")


draw_pitch <- function(pitch_fill = "#3a6b35") {
  list(
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
             fill = pitch_fill, color = "white", linewidth = 0.8),
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80,
             color = "white", linewidth = 0.5, alpha = 0.6),
    annotate("path",
             x = 60 + 10 * cos(seq(0, 2*pi, length.out = 150)),
             y = 40 + 10 * sin(seq(0, 2*pi, length.out = 150)),
             color = "white", linewidth = 0.5, alpha = 0.6),
    annotate("point", x = 60, y = 40, color = "white", size = 1.2),
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
             fill = NA, color = "white", linewidth = 0.5, alpha = 0.7),
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
             fill = NA, color = "white", linewidth = 0.6),
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
             fill = NA, color = "white", linewidth = 0.5, alpha = 0.7),
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
             fill = NA, color = "white", linewidth = 0.6),
    annotate("rect", xmin = 120, xmax = 122, ymin = 36, ymax = 44,
             fill = NA, color = "white", linewidth = 0.8),
    annotate("rect", xmin = -2, xmax = 0, ymin = 36, ymax = 44,
             fill = NA, color = "white", linewidth = 0.5, alpha = 0.7),
    annotate("point", x = 108, y = 40, color = "white", size = 1.2),
    annotate("path",
             x = 108 + 10 * cos(seq(pi - 0.84, pi + 0.84, length.out = 60)),
             y = 40  + 10 * sin(seq(pi - 0.84, pi + 0.84, length.out = 60)),
             color = "white", linewidth = 0.5)
  )
}

#Overall stats 

ivory_coast_summary <- data.frame(
  Stat = c(
    "Matches Played",
    "Goals Scored",
    "Total xG For",
    "Total xG Against",
    "Total xG Difference",
    "Win % — Gasset (Group Stage)",
    "Win % — Faé (Knockout)"
  ),
  Value = c(
    ivorycoast_matchesplayed,
    ivorycoast_totalgoals,
    round(sum(ivorycoastxg_for_and_against$xg_for),    2),
    round(sum(ivorycoastxg_for_and_against$xg_against), 2),
    ivorycoast_total_xg_diff$total_xg_diff,
    paste0(ivorycoast_manager_comparison %>% filter(stage == "Group Stage") %>% pull(win_pct), "%"),
    paste0(ivorycoast_manager_comparison %>% filter(stage == "Knockout")    %>% pull(win_pct), "%")
  )
)

ivory_coast_summary


#############################################################################################################################

#KPIs - Matches Played, Goals Scored, xG Difference, Win % After Managerial Change ----

# Total matches played

ivorycoast_matchesplayed <- nrow(ivorycoast_matches)

# Total goals 

ivorycoast_totalgoals <- all_events %>% 
  filter(type.name == "Shot", shot.outcome.name == "Goal", possession_team.name == "Côte d'Ivoire") %>% 
  filter(period != 5) %>% 
  nrow

#xG difference 

ivorycoastxg_for_and_against <- all_events %>% 
  filter(type.name == "Shot") %>%
  filter(period != 5) %>% 
  group_by(match_id) %>% 
  summarise(
    xg_for = round(sum(shot.statsbomb_xg[possession_team.name == "Côte d'Ivoire"], na.rm = TRUE), 2),
    xg_against = round(sum(shot.statsbomb_xg[possession_team.name != "Côte d'Ivoire"], na.rm = TRUE), 2)
  ) %>% 
  filter(match_id %in% ivorycoast_matches$match_id) %>% 
  mutate(xg_diff = round(xg_for - xg_against, 2)) %>%
  left_join(afcon_matches %>% select(match_id, competition_stage.name), by = "match_id")

ivorycoast_total_xg_diff <- ivorycoastxg_for_and_against %>%
  summarise(total_xg_diff = round(sum(xg_diff), 2))

# Managerial win percentage before and after sacking 

ivorycoast_manager_comparison <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | 
           away_team.away_team_name == "Côte d'Ivoire") %>%
  mutate(
    won = case_when(
      home_team.home_team_name == "Côte d'Ivoire" & home_score > away_score ~ TRUE,
      away_team.away_team_name == "Côte d'Ivoire" & away_score > home_score ~ TRUE,
      TRUE ~ FALSE
    ),
    stage = if_else(competition_stage.name == "Group Stage", "Group Stage", "Knockout")
  ) %>%
  group_by(stage) %>%
  summarise(
    played = n(),
    wins = sum(won),
    win_pct = round(wins / played * 100, 1),
    .groups = "drop"
  )

ivorycoast_manager_comparison <- ivorycoast_manager_comparison %>% 
  mutate(manager = case_when(
    stage == "Group Stage" ~ "Jean-Louis Gasset",
    stage == "Knockout" ~ "Emerse Faé"
  ))

#############################################################################################################################

#Tactical Changes ----
##Passing Structures
passing_data <- all_events %>%
  filter(team.name == "Côte d'Ivoire", type.name == "Pass") %>%
  filter(match_id %in% ivorycoast_matches$match_id) %>%
  left_join(afcon_matches %>% dplyr::select(match_id, competition_stage.name), by = "match_id") %>%
  mutate(
    x       = sapply(location, `[[`, 1),
    y       = sapply(location, `[[`, 2),
    manager = if_else(competition_stage.name == "Group Stage",
                      "Jean-Louis Gasset", "Emerse Faé")
  )

passing_heatmap <- ggplot(passing_data, aes(x = x, y = y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha       = 0.75,
    contour_var = "ndensity",
    bins        = 12
  ) +
  scale_fill_viridis_d(option = "inferno", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  facet_wrap(~manager) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 12))

ggsave("app/passing_heatmap.png", passing_heatmap, width = 10, height = 6)


saveRDS(passing_heatmap, "passing_heatmap.rds")

# Comparison of shot areas between the two managers 

ivorycoast_shots_manager <- all_events %>%
  filter(type.name == "Shot",
         team.name == "Côte d'Ivoire",
         period != 5,
         match_id %in% ivorycoast_matches$match_id) %>%
  left_join(
    afcon_matches %>% select(match_id, competition_stage.name),
    by = "match_id"
  ) %>%
  mutate(
    x = sapply(location,          `[[`, 1),
    y = sapply(location,          `[[`, 2),
    end_x = sapply(shot.end_location, `[[`, 1),
    end_y = sapply(shot.end_location, `[[`, 2),
    manager = if_else(competition_stage.name == "Group Stage",
                        "Jean-Louis Gasset", "Emerse Faé"),
    is_goal = shot.outcome.name == "Goal",
    xg_label = round(shot.statsbomb_xg, 3),
    shot_type = case_when(
      shot.outcome.name == "Goal"          ~ "Goal",
      shot.outcome.name == "Saved"         ~ "Saved",
      shot.outcome.name == "Saved to Post" ~ "Saved",
      shot.outcome.name == "Blocked"       ~ "Blocked",
      TRUE                                 ~ "Off Target"
    )
  )

outcome_colours <- c("Goal" = "#ffffff", "Saved" = "#6b8fc4",
                     "Blocked" = "#e07b39", "Off Target" = "#8aab96")

manager_comparison_shotmap <- ggplot(ivorycoast_shots_manager) +
  draw_pitch() +
  geom_segment(
    data  = filter(ivorycoast_shots_manager, is_goal),
    aes(x = x, y = y, xend = end_x, yend = end_y),
    color = "white",
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.9,
    arrow = arrow(length = unit(0.18, "cm"), type = "open")
  ) +
  geom_point(
    aes(x = x, y = y, color = shot_type,
        text = paste("Player:", player.name,
                     "<br>Outcome:", shot.outcome.name,
                     "<br>xG:", xg_label,
                     "<br>Minute:", minute)),
    fill   = NA,
    size   = 3,
    shape  = 21,
    stroke = 1.2,
    alpha  = 0.85
  ) +
  scale_color_manual(values = outcome_colours, guide = "none") +
  coord_flip(xlim = c(58, 123), ylim = c(0, 80), expand = FALSE) +
  scale_x_continuous() +
  scale_y_reverse() +
  facet_wrap(~manager) +
  labs(
    title = "Côte d'Ivoire — Shot Map by Manager",
    caption = "Data: StatsBomb  |  Excludes penalties"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#1e1e2e", color = NA),
    panel.background = element_rect(fill = "#1e1e2e", color = NA),
    plot.title = element_text(color = "#f0f0f0", face = "bold",
                                    hjust = 0.5, size = 16,
                                    margin = margin(t = 14, b = 4)),
    plot.caption = element_text(color = "#666666", hjust = 0.98,
                                    size = 9, margin = margin(t = 6, b = 10)),
    strip.text = element_text(color = "white", face = "bold",
                                    size = 12, margin = margin(b = 6)),
    plot.margin = margin(10, 20, 10, 20)
  )


saveRDS(manager_comparison_shotmap, "manager_comparison_shotmap.rds")

ggplotly(manager_comparison_shotmap, tooltip = "text")
#Compore the shot conceded and xg performance between the two managers http://127.0.0.1:9383/graphics/plot_zoom_png?width=2115&height=1103

ivorycoast_managers_shotsconceded_xgagainst <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | 
           away_team.away_team_name == "Côte d'Ivoire") %>%
  select(match_id, competition_stage.name) %>%
  mutate(stage = if_else(competition_stage.name == "Group Stage", "Group Stage", "Knockout")) %>%
  left_join(
    all_events %>%
      filter(type.name == "Shot", team.name != "Côte d'Ivoire", period != 5) %>%
      group_by(match_id) %>%
      summarise(
        shots_conceded = n(),
        xg_conceded = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2)
      ),
    by = "match_id"
  ) %>%
  group_by(stage) %>%
  summarise(
    matches = n(),
    shots_conceded = sum(shots_conceded, na.rm = TRUE),
    xg_conceded = round(sum(xg_conceded, na.rm = TRUE), 2),
    shots_conceded_per_game = round(shots_conceded / matches, 1),
    xg_conceded_per_game    = round(xg_conceded / matches, 2)
  ) %>% 
  mutate(manager = case_when(
    stage == "Group Stage" ~ "Jean-Louis Gasset",
    stage == "Knockout" ~ "Emerse Faé"
  )) %>%
  select(manager, matches, shots_conceded, xg_conceded,
         shots_conceded_per_game, xg_conceded_per_game) %>%
  pivot_longer(cols = -manager, names_to = "Stat", values_to = "value") %>%
  mutate(value = as.numeric(value) %>% format(drop0trailing = TRUE) %>% trimws()) %>%
  pivot_wider(names_from = manager, values_from = value) %>%
  select(Stat, "Jean-Louis Gasset", "Emerse Faé")

ivorycoast_managers_shotsconceded_xgagainst
saveRDS(ivorycoast_managers_shotsconceded_xgagainst, "ivorycoast_managers_shotsconceded_xgagainst.rds")

#Dribble heat map comparison 

dribble_manager <-  all_events %>%
  dplyr::filter(team.name == "Côte d'Ivoire",
                type.name == "Dribble",
                dribble.outcome.name == "Complete",
                match_id %in% ivorycoast_matches$match_id) %>%
  left_join(afcon_matches %>% dplyr::select(match_id, competition_stage.name), by = "match_id") %>%
  dplyr::mutate(
    x       = sapply(location, `[[`, 1),
    y       = sapply(location, `[[`, 2),
    manager = if_else(competition_stage.name == "Group Stage",
                      "Jean-Louis Gasset", "Emerse Faé")
  )


dribble_manager_comparison_plot <- ggplot(dribble_manager, aes(x = x, y = y)) +
  draw_pitch() +
  stat_density_2d_filled(aes(fill = after_stat(level)),
                         alpha = 0.75, contour_var = "ndensity", bins = 12) +
  scale_fill_viridis_d(option = "inferno", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  facet_wrap(~manager) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 12))
saveRDS(dribble_manager_comparison_plot, "dribble_manager_comparison_plot.rds")

# Defensice actions per manager comparison 

defensive_actions_manager_comparison <- all_events %>%
  filter(team.name == "Côte d'Ivoire",
         match_id %in% ivorycoast_matches$match_id) %>%
  left_join(afcon_matches %>% select(match_id, competition_stage.name), by = "match_id") %>%
  mutate(manager = if_else(competition_stage.name == "Group Stage",
                           "Jean-Louis Gasset", "Emerse Faé")) %>%
  group_by(manager) %>%
  summarise(
    matches = n_distinct(match_id),
    tackles = sum(type.name == "Duel" & duel.type.name == "Tackle"),
    pressures = sum(type.name == "Pressure"),
    duels = sum(type.name == "Duel"),
    interceptions = sum(type.name == "Interception"),
    clearances = sum(type.name == "Clearance")
  ) %>%
  mutate(
    tackles_pg = round(tackles/matches, 1),
    pressures_pg = round(pressures/matches, 1),
    duels_pg = round(duels/matches, 1),
    interceptions_pg = round(interceptions/matches, 1),
    clearances_pg = round(clearances/matches, 1)
  ) %>%
  select(manager, matches, tackles_pg, pressures_pg,
         duels_pg, interceptions_pg, clearances_pg) %>%
  pivot_longer(cols = -manager, names_to = "Stat", values_to = "value") %>%
  mutate(value = format(value, drop0trailing = TRUE) %>% trimws()) %>%
  pivot_wider(names_from = manager, values_from = value) %>%
  select(Stat, "Jean-Louis Gasset", "Emerse Faé")
defensive_actions_manager_comparison


saveRDS(defensive_actions_manager_comparison, "defensive_actions_manager_comparison.rds")


# Shot quality vs quantity 

shotquality_vs_quantity <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | 
           away_team.away_team_name == "Côte d'Ivoire") %>%
  select(match_id, competition_stage.name) %>%
  mutate(manager = if_else(competition_stage.name == "Group Stage",
                           "Jean-Louis Gasset", "Emerse Faé")) %>%
  left_join(
    all_events %>%
      filter(type.name == "Shot", 
             team.name == "Côte d'Ivoire",
             period != 5) %>%
      group_by(match_id) %>%
      summarise(
        total_shots = n(),
        shots_on_target = sum(shot.outcome.name %in% c("Saved", "Goal"), na.rm = TRUE),
        total_xg = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2),
        avg_xg_per_shot = round(mean(shot.statsbomb_xg, na.rm = TRUE), 3),
        goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)
      ),
    by = "match_id"
  ) %>%
  group_by(manager) %>%
  summarise(
    matches         = n(),
    total_shots     = sum(total_shots, na.rm = TRUE),
    shots_on_target = sum(shots_on_target, na.rm = TRUE),
    total_xg        = round(sum(total_xg, na.rm = TRUE), 2),
    goals           = sum(goals, na.rm = TRUE),
    shots_per_game  = round(total_shots/matches, 1),
    avg_xg_per_shot = round(total_xg/total_shots, 3),
    shot_accuracy   = round(shots_on_target/total_shots * 100, 1)
  ) %>%
  arrange(desc(avg_xg_per_shot)) %>%
  mutate(
    matches         = as.character(matches),
    total_shots     = as.character(total_shots),
    shots_on_target = as.character(shots_on_target),
    total_xg        = as.character(total_xg),
    goals           = as.character(goals),
    shots_per_game  = as.character(shots_per_game),
    avg_xg_per_shot = as.character(avg_xg_per_shot),
    shot_accuracy   = as.character(shot_accuracy)
  ) %>%
  pivot_longer(-manager, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = manager, values_from = "Value") %>%
  mutate(Metric = recode(Metric,
                         "matches"          = "Matches",
                         "total_shots"      = "Total Shots",
                         "shots_on_target"  = "Shots on Target",
                         "total_xg"         = "Total xG",
                         "goals"            = "Goals",
                         "shots_per_game"   = "Shots per Game",
                         "avg_xg_per_shot"  = "Avg xG per Shot",
                         "shot_accuracy"    = "Shot Accuracy (%)"
  ))

saveRDS(shotquality_vs_quantity, "shotquality_vs_quantity.rds")



# Set piece efficiency -> goals, shots, shots on targets, big chances 

ivorycoast_setpiece_efficiency <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | 
           away_team.away_team_name == "Côte d'Ivoire") %>%
  select(match_id, competition_stage.name) %>%
  mutate(manager = if_else(competition_stage.name == "Group Stage",
                           "Jean-Louis Gasset", "Emerse Faé")) %>%
  left_join(
    all_events %>%
      filter(type.name == "Shot",
             team.name == "Côte d'Ivoire",
             period != 5,
             shot.type.name %in% c("Free Kick", "Corner", "Penalty")) %>%
      group_by(match_id) %>%
      summarise(
        setpiece_shots = n(),
        setpiece_sot = sum(shot.outcome.name %in% c("Saved", "Goal"), na.rm = TRUE),
        setpiece_goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
        setpiece_bigchance = sum(shot.statsbomb_xg > 0.3, na.rm = TRUE),
        setpiece_xg = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2)
      ),
    by = "match_id"
  ) %>%
  group_by(manager) %>%
  summarise(
    matches = n(),
    setpiece_shots = sum(setpiece_shots, na.rm = TRUE),
    setpiece_sot = sum(setpiece_sot, na.rm = TRUE),
    setpiece_goals = sum(setpiece_goals, na.rm = TRUE),
    setpiece_bigchance = sum(setpiece_bigchance, na.rm = TRUE),
    setpiece_xg = round(sum(setpiece_xg, na.rm = TRUE), 2),
    shots_per_game = round(setpiece_shots/matches, 1),
    xg_per_game = round(setpiece_xg/matches, 2),
    conversion_rate = round(setpiece_goals/ifelse(setpiece_shots == 0, 1, setpiece_shots) * 100, 1),
    sot_pct = round(setpiece_sot/ifelse(setpiece_shots == 0, 1, setpiece_shots) * 100, 1)
  ) %>%
  arrange(desc(setpiece_goals))


#############################################################################################################################

# Key Players ----

# Shot map of best performing attackers 

# Get top 5 Ivory Coast shooters across all their matches
ivorycoast_top5_shooters <- all_events %>%
  filter(type.name == "Shot", team.name == "Côte d'Ivoire", period != 5) %>%
  count(player.name, sort = TRUE) %>%
  slice_head(n = 5)

ivorycoast_top5_shooters <- all_events %>%
  filter(type.name == "Shot", team.name == "Côte d'Ivoire", period != 5) %>%
  mutate(
    x = sapply(location, `[[`, 1),
    y = sapply(location, `[[`, 2),
    end_x = sapply(shot.end_location, `[[`, 1),
    end_y = sapply(shot.end_location, `[[`, 2)
  ) %>%
  mutate(
    shot_type = case_when(
      shot.outcome.name == "Goal" ~ "Goal",
      shot.outcome.name == "Saved" ~ "Saved",
      shot.outcome.name == "Saved to Post" ~ "Saved",
      shot.outcome.name == "Blocked" ~ "Blocked",
      TRUE ~ "Off Target"
    ),
    is_goal = shot.outcome.name == "Goal"
  ) %>%
  group_by(player.name) %>%
  mutate(total_shots = n()) %>%
  ungroup() %>%
  filter(player.name %in% (count(., player.name, sort = TRUE) %>% 
                             slice_head(n = 5) %>% 
                             pull(player.name)))

view(ivorycoast_top5_shooters)

outcome_colours <- c(
  "Goal"       = "#ffffff",
  "Saved"      = "#6b8fc4",
  "Blocked"    = "#e07b39",
  "Off Target" = "#8aab96"
)

final_shotmap <- ggplot(ivorycoast_top5_shooters) +
  draw_pitch() +
  geom_segment(
    data      = filter(ivorycoast_top5_shooters, is_goal),
    aes(x = x, y = y, xend = end_x, yend = end_y),
    color     = "white",
    linewidth = 0.7,
    linetype  = "dashed",
    alpha     = 0.9,
    arrow     = arrow(length = unit(0.18, "cm"), type = "open")
  ) +
  geom_point(
    aes(x = x, y = y, color = shot_type, size = shot.statsbomb_xg),
    fill   = NA,
    shape  = 21,
    stroke = 1.2,
    alpha  = 0.85
  ) +
  scale_color_manual(
    values = outcome_colours,
    name   = "Outcome"
  ) +
  scale_size_continuous(
    name   = "xG",
    range  = c(2, 8),
    breaks = c(0.05, 0.15, 0.3, 0.5),
    labels = c("0.05", "0.15", "0.30", "0.50")
  ) +
  coord_flip(xlim = c(58, 123), ylim = c(0, 80), expand = FALSE) +
  scale_x_continuous() +
  scale_y_reverse() +
  facet_wrap(~player.name, ncol = 3) +
  labs(
    title   = "Côte d'Ivoire — Top 5 Shooters Shot Map",
    caption = "Data: StatsBomb  |  Excludes penalties  |  Size = xG"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#1e1e2e", color = NA),
    panel.background = element_rect(fill = "#1e1e2e", color = NA),
    plot.title       = element_text(color = "#f0f0f0", face = "bold",
                                    hjust = 0.5, size = 16,
                                    margin = margin(t = 14, b = 4)),
    plot.caption     = element_text(color = "#666666", hjust = 0.98,
                                    size = 9, margin = margin(t = 6, b = 10)),
    strip.text       = element_text(color = "white", face = "bold",
                                    size = 11, margin = margin(b = 6)),
    plot.margin      = margin(10, 20, 10, 20),
    legend.position  = "bottom",
    legend.text      = element_text(color = "white", size = 9),
    legend.title     = element_text(color = "white", size = 10)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4), nrow = 1),
    size  = guide_legend(nrow = 1)
  )



saveRDS(final_shotmap, "final_shotmap.rds")

# Table of goals and assists for ivory coast 
goals_graph <- all_events %>%
  filter(type.name == "Shot", shot.outcome.name == "Goal", team.name == "Côte d'Ivoire", period != 5) %>%
  count(player.name, sort = TRUE)
assists_graph <- all_events %>% 
  filter(type.name == "Pass", pass.goal_assist == TRUE, team.name == "Côte d'Ivoire", period != 5) %>%
  count(player.name, sort = TRUE)
  
combined_goals_and_assists <- full_join(goals_graph, assists_graph, by = "player.name") %>% 
  replace_na(list(Goals = 0, Assists = 0))
  
combined_goals_and_assists <- combined_goals_and_assists %>%
  rename(Goals = n.x, Assists = n.y)

combined_goals_and_assists <- combined_goals_and_assists %>% 
  mutate(GA = Goals + Assists)

combined_goals_and_assists <- combined_goals_and_assists %>%
  replace_na(list(Goals = 0, Assists = 0))
saveRDS(combined_goals_and_assists, "combined_goals_and_assists.rds")

# Dribbling stats 

dribbling_stats <- all_events %>%
  filter(team.name == "Côte d'Ivoire", type.name == "Dribble") %>%
  group_by(player.name) %>%
  summarise(
    attempted  = n(),
    completed  = sum(dribble.outcome.name == "Complete", na.rm = TRUE),
    failed = sum(dribble.outcome.name == "Incomplete", na.rm = TRUE),
    success_pct = round(completed / attempted * 100, 1)
  ) %>%
  arrange(desc(completed))

dribbling_stats


dribble_locations <- all_events %>%
  filter(team.name == "Côte d'Ivoire",
         type.name == "Dribble",
         dribble.outcome.name == "Complete") %>%
  mutate(
    x = sapply(location, `[[`, 1),
    y = sapply(location, `[[`, 2)
  )

dribble_heatmap <- ggplot(dribble_locations, aes(x = x, y = y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = stat(level)),
    alpha = 0.75,
    contour_var = "ndensity",
    bins = 12
  ) +
  scale_fill_viridis_d(option = "inferno", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  labs(
    title = "Côte d'Ivoire — Successful Dribble Heatmap",
    subtitle = "Location of completed dribbles across the tournament",
    caption  = "Data: StatsBomb"
  ) +
  pitch_theme

dribbling_stats_top5 <- all_events %>%
  filter(team.name == "Côte d'Ivoire", type.name == "Dribble") %>%
  group_by(player.name) %>%
  summarise(
    attempted = n(),
    completed = sum(dribble.outcome.name == "Complete", na.rm = TRUE),
    failed = sum(dribble.outcome.name == "Incomplete", na.rm = TRUE),
    success_pct = round(completed / attempted * 100, 1)
  ) %>%
  arrange(desc(completed)) %>% 
  slice_head(n = 5)

dribbling_stats_top5 <- all_events %>%
  filter(team.name == "Côte d'Ivoire", type.name == "Dribble") %>%
  group_by(player.name) %>%
  summarise(
    attempted = n(),
    completed = sum(dribble.outcome.name == "Complete",   na.rm = TRUE),
    failed = sum(dribble.outcome.name == "Incomplete", na.rm = TRUE),
    success_pct = round(completed / attempted * 100, 1)
  ) %>%
  arrange(desc(completed)) %>%
  slice_head(n = 5)

top5_dribblers <- dribbling_stats_top5 %>% pull(player.name)

dribble_locations <- all_events %>%
  filter(team.name == "Côte d'Ivoire",
         type.name == "Dribble",
         dribble.outcome.name == "Complete",
         player.name %in% top5_dribblers) %>%
  mutate(
    x = sapply(location, `[[`, 1),
    y = sapply(location, `[[`, 2)
  )

top5_dribble_heatmap <- ggplot(dribble_locations, aes(x = x, y = y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = stat(level)),
    alpha = 0.75,
    contour_var = "ndensity",
    bins = 12
  ) +
  scale_fill_viridis_d(option = "inferno", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  facet_wrap(~player.name, ncol = 3) +
  labs(
    title    = "Côte d'Ivoire — Top 5 Dribblers Heatmap",
    subtitle = "Location of completed dribbles across the tournament",
    caption  = "Data: StatsBomb"
  ) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 11))

saveRDS(top5_dribble_heatmap,"top5_dribble_heatmap.rds")


# Defensive actions -> tackles, blocks etc 

tackle_stats <- all_events %>%
  filter(team.name == "Côte d'Ivoire", 
         type.name == "Duel",
         duel.type.name == "Tackle") %>%
  group_by(player.name) %>%
  summarise(
    attempted = n(),
    won_ball = sum(duel.outcome.name == "Won", na.rm = TRUE),
    lost = sum(duel.outcome.name %in% c("Lost In Play", "Lost Out"), na.rm = TRUE),
    success = sum(duel.outcome.name %in% c("Success Out", "Success In Play"), na.rm = TRUE),
    success_pct = round((won_ball + success) / attempted * 100, 1)
  ) %>%
  arrange(desc(attempted))

tackle_stats

defensive_stats <- all_events %>%
  filter(team.name == "Côte d'Ivoire") %>%
  group_by(player.name) %>%
  summarise(
    tackles = sum(type.name == "Duel" & duel.type.name == "Tackle"),
    tackles_won = sum(type.name == "Duel" & duel.type.name == "Tackle" & duel.outcome.name %in% c("Won", "Success In Play", "Success Out")),
    tackle_pct = round(tackles_won / ifelse(tackles == 0, 1, tackles) * 100, 1),
    pressures = sum(type.name == "Pressure"),
    interceptions = sum(type.name == "Interception"),
    inter_success = sum(type.name == "Interception" &
                            interception.outcome.name %in% c("Won", "Success In Play", "Success Out")),
    inter_pct = round(inter_success / ifelse(interceptions == 0, 1, interceptions) * 100, 1),
    duels = sum(type.name == "Duel"),
    duels_won = sum(type.name == "Duel" &
                            duel.outcome.name %in% c("Won", "Success In Play", "Success Out")),
    duel_pct = round(duels_won / ifelse(duels == 0, 1, duels) * 100, 1),
    clearances = sum(type.name == "Clearance")
  ) %>%
  filter(tackles + pressures + interceptions + duels + clearances > 0) %>%
  arrange(desc(tackles))
saveRDS(defensive_stats, "defensive_stats.rds")

view(defensive_stats)

# Top 5 Passers 

top5_passers <- all_events %>%
  dplyr::filter(team.name == "Côte d'Ivoire", type.name == "Pass") %>%
  dplyr::count(player.name, sort = TRUE) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::pull(player.name)

pass_locations <- all_events %>%
  dplyr::filter(team.name == "Côte d'Ivoire", type.name == "Pass",
                player.name %in% top5_passers) %>%
  dplyr::mutate(
    x = sapply(location, `[[`, 1),
    y = sapply(location, `[[`, 2)
  )

top5_passers_heatmap <- ggplot(pass_locations, aes(x = x, y = y)) +
  draw_pitch() +
  stat_density_2d_filled(aes(fill = after_stat(level)),
                         alpha = 0.75, contour_var = "ndensity", bins = 12) +
  scale_fill_viridis_d(option = "inferno", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  facet_wrap(~player.name, ncol = 3) +
  labs(
    title = "Côte d'Ivoire — Top 5 Passers Heatmap",
    subtitle = "Pass origin locations across the tournament",
    caption  = "Data: StatsBomb"
  ) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 11))


saveRDS(top5_passers_heatmap, "top5_passers_heatmap.rds")


#Attacking metrics table

attacking_metrics <- all_events %>%
  filter(team.name == "Côte d'Ivoire") %>%
  mutate(
    loc_x = map_dbl(location, ~ if (length(.x) == 0) NA_real_ else .x[[1]]),
    carry_end_x = map_dbl(carry.end_location, ~ if (length(.x) == 0) NA_real_ else .x[[1]]),
    carry_end_y = map_dbl(carry.end_location, ~ if (length(.x) == 0) NA_real_ else .x[[2]]),
    pass_end_x  = map_dbl(pass.end_location,  ~ if (length(.x) == 0) NA_real_ else .x[[1]])
  ) %>%
  group_by(player.name) %>%
  summarise(
    carries_final3rd = sum(type.name == "Carry" & loc_x < 80 & carry_end_x >= 80, na.rm = TRUE),
    box_entries = sum(type.name == "Carry" & loc_x < 102 & carry_end_x >= 102 & carry_end_y >= 18 & carry_end_y <= 62, na.rm = TRUE),
    dribbles = sum(type.name == "Dribble" & dribble.outcome.name == "Complete", na.rm = TRUE),
    touches_final3rd = sum(type.name %in% c("Pass", "Carry", "Shot", "Dribble") & loc_x >= 80, na.rm = TRUE),
    progressive_passes = sum(type.name == "Pass" & !is.na(pass_end_x) & pass_end_x - loc_x >= 10, na.rm = TRUE),
    shot_assists = sum(type.name == "Pass" & pass.shot_assist == TRUE, na.rm = TRUE)
  ) %>%
  filter(carries_final3rd + dribbles + touches_final3rd + progressive_passes + shot_assists > 0) %>%
  arrange(desc(touches_final3rd))

view(attacking_metrics)

saveRDS(attacking_metrics, "attacking_metrics.rds")




#############################################################################################################################

# Was it deserved ----


# Finishing efficiency


# Compare stats with opponents 

# Show analysis of the final

# Compare the stats and performance between Nigeria and Ivory Coast from the match in the group stage and the one in the final


#Total team goals

goalsperteam_table <- goals_table %>%
  group_by(team.name) %>%
  summarise(
    group_stage_goals = sum(goals[competition_stage.name == "Group Stage"], na.rm = TRUE),
    ro16_goals = sum(goals[competition_stage.name == "Round of 16"], na.rm = TRUE),
    quarterfinal_goals = sum(goals[competition_stage.name == "Quarter-finals"], na.rm = TRUE),
    semifinal_goals = sum(goals[competition_stage.name == "Semi-finals"], na.rm = TRUE),
    final_goals = sum(goals[competition_stage.name == "Final"], na.rm = TRUE),
    total_goals = sum(goals, na.rm = TRUE)
  ) %>%
  arrange(desc(total_goals))

# cumulative xg graph compared to Nigeria 

#Metric to see who deserved to win 

deserved_winner_tournament <- all_events %>%
  filter(period != 5) %>%
  mutate(
    loc_x = map_dbl(location, ~ if (length(.x) == 0) NA_real_ else .x[[1]]),
    carry_end_x = map_dbl(carry.end_location, ~ if (length(.x) == 0) NA_real_ else .x[[1]]),
    carry_end_y = map_dbl(carry.end_location, ~ if (length(.x) == 0) NA_real_ else .x[[2]]),
    pass_end_x = map_dbl(pass.end_location,  ~ if (length(.x) == 0) NA_real_ else .x[[1]])
  ) %>%
  group_by(team.name) %>%
  summarise(
    matches = n_distinct(match_id),
    xg = round(sum(shot.statsbomb_xg,    na.rm = TRUE), 2),
    shots = sum(type.name == "Shot"),
    shots_on_target = sum(type.name == "Shot" & shot.outcome.name %in% c("Saved", "Goal")),
    goals = sum(type.name == "Shot" & shot.outcome.name == "Goal"),
    shot_assists = sum(type.name == "Pass" & pass.shot_assist == TRUE, na.rm = TRUE),
    passes = sum(type.name == "Pass"),
    progressive_passes = sum(type.name == "Pass" & !is.na(pass_end_x) & pass_end_x - loc_x >= 10, na.rm = TRUE),
    dribbles = sum(type.name == "Dribble" & dribble.outcome.name == "Complete"),
    carries_final3rd = sum(type.name == "Carry" & !is.na(carry_end_x) & loc_x < 80 & carry_end_x >= 80, na.rm = TRUE),
    box_entries = sum(type.name == "Carry" & !is.na(carry_end_x) & loc_x < 102 & carry_end_x >= 102 & carry_end_y >= 18 & carry_end_y <= 62, na.rm = TRUE),
    touches_final3rd = sum(type.name %in% c("Pass", "Carry", "Shot", "Dribble") & loc_x >= 80, na.rm = TRUE),
    tackles_won = sum(type.name == "Duel" & duel.type.name == "Tackle" & duel.outcome.name %in% c("Won", "Success In Play", "Success Out")),
    interceptions = sum(type.name == "Interception"),
    clearances = sum(type.name == "Clearance"),
    pressures = sum(type.name == "Pressure"),
    fouls = sum(type.name == "Foul Committed")
  ) %>%
  left_join(
    team_rounds %>%
      mutate(stage_weight = case_when(
        competition_stage.name == "Final" ~ 1.00,
        competition_stage.name == "Semi-finals" ~ 0.95,
        competition_stage.name == "Semi-final → 3rd Place Final" ~ 0.90,
        competition_stage.name == "Quarter-finals" ~ 0.85,
        competition_stage.name == "Round of 16" ~ 0.80,
        TRUE ~ 0.75
      )) %>%
      select(team, competition_stage.name, stage_weight),
    by = c("team.name" = "team")
  ) %>%
  mutate(
    xg_pg = round(xg/matches, 2),
    shots_on_target_pg = round(shots_on_target/matches, 1),
    goals_pg = round(goals/matches, 2),
    shot_assists_pg = round(shot_assists/matches, 1),
    passes_pg = round(passes/matches, 1),
    progressive_passes_pg = round(progressive_passes/matches, 1),
    dribbles_pg = round(dribble/matches, 1),
    carries_final3rd_pg = round(carries_final3rd/matches, 1),
    box_entries_pg = round(box_entries/matches, 1),
    touches_final3rd_pg = round(touches_final3rd/matches, 1),
    tackles_won_pg = round(tackles_won/matches, 1),
    interceptions_pg = round(interceptions/matches, 1),
    clearances_pg = round(clearances/matches, 1),
    pressures_pg = round(pressures/matches, 1)
  ) %>%
  mutate(
    attack_score = round((xg_pg / max(xg_pg) +
                                 shots_on_target_pg / max(shots_on_target_pg) +
                                 goals_pg / max(goals_pg) +
                                 shot_assists_pg / max(shot_assists_pg)) / 4 * 10 * stage_weight, 1),
    progressive_score = round((passes_pg / max(passes_pg) +
                                 progressive_passes_pg / max(progressive_passes_pg) +
                                 dribbles_pg / max(dribbles_pg) +
                                 carries_final3rd_pg / max(carries_final3rd_pg) +
                                 box_entries_pg / max(box_entries_pg) +
                                 touches_final3rd_pg / max(touches_final3rd_pg)) / 6 * 10 * stage_weight, 1),
    defensive_score   = round((tackles_won_pg / max(tackles_won_pg) +
                                 interceptions_pg / max(interceptions_pg) +
                                 clearances_pg / max(clearances_pg) +
                                 pressures_pg / max(pressures_pg)) / 4 * 10 * stage_weight, 1),
    overall_score     = round((attack_score + progressive_score + defensive_score) / 3, 1)
  ) %>%
  select(team.name, competition_stage.name, stage_weight, matches, goals,
         xg_pg, shots_on_target_pg, shot_assists_pg,
         progressive_passes_pg, dribbles_pg, carries_final3rd_pg,
         box_entries_pg, touches_final3rd_pg,
         tackles_won_pg, interceptions_pg, clearances_pg, pressures_pg,
         attack_score, progressive_score, defensive_score, overall_score) %>%
  arrange(desc(overall_score))
  
deserved_winner_tournament

saveRDS(deserved_winner_tournament, "deserved_winner_tournament.rds")

# SHINY LIVE DASHBOARD ----

library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  style = "background:#12131a;",
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setPage', function(p) {
      Shiny.setInputValue('page', p, {priority: 'event'});
    });
  ")),
  
  # ── Home ──────────────────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'home' || input.page == undefined",
    h1("AFCON 2023 — How Ivory Coast Won AFCON despite managerial change, and was it really deserved?",
       style = "color:white; text-align:center; padding:30px;"),
    fluidRow(
      column(3, actionButton("btn_journey",  "🏆 Tournament Journey",        width = "100%",
                             style = "background:#12421a; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_managers", "👤 Before & After Manager Sacking", width = "100%",
                             style = "background:#5a2000; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_players",  "⭐ Key Players",               width = "100%",
                             style = "background:#1a1a4e; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_deserved", "📊 Was It Deserved?",          width = "100%",
                             style = "background:#4a1a00; color:white; height:150px; font-size:16px; border:none; border-radius:10px;"))
    )
  ),
  
  # ── Tournament Journey ────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'journey'",
    actionButton("back1", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Tournament Journey", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Overall Stats",
                div(style = "background:white; padding:10px; border-radius:8px; margin:10px;",
                    tableOutput("ivory_coast_summary"))),
      nav_panel("cuumulative_ivorycoast_xgplot",
                plotOutput("cuumulative_ivorycoast_xgplot")),
      nav_panel("Match Results",
                div(style = "background:white; padding:10px; border-radius:8px; margin:10px;",
                    tableOutput("ivory_coast_timeline")))
    )
  ),
  
  # ── Before & After Manager ────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'managers'",
    actionButton("back2", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Before & After Manager Sacking", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Shot Map",
                plotlyOutput("manager_shotmap")),
      nav_panel("Shots Conceded & xG Against",
                DTOutput("shots_conceded_table")),
      nav_panel("Shot Quality vs Quantity",
                DTOutput("shot_quality_table")),
      nav_panel("Passing Heatmap",
                plotOutput("manager_pass_heatmap")),
      nav_panel("Defensive Stats",
                DTOutput("manager_defensive_table")),
      nav_panel("Dribble Areas",
                plotOutput("manager_dribble_heatmap"))
    )
  ),
  
  # ── Key Players ───────────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'players'",
    actionButton("back3", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Key Players", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Shot Map",
                plotlyOutput("top5_shotmap", height = "600px")),
      nav_panel("Goals & Assists",
                DTOutput("goals_assists_table")),
      nav_panel("Top 5 Dribblers",
                plotOutput("top5_dribble_heatmap")),
      nav_panel("Defensive Stats",
                DTOutput("player_defensive_table")),
      nav_panel("Top 5 Passers",
                plotOutput("top5_pass_heatmap")),
      nav_panel("Attacking Stats",
                DTOutput("attacking_table"))
    )
  ),
  
  # ── Was It Deserved ───────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'deserved'",
    actionButton("back4", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Was It Deserved?", style = "color:white; text-align:center;"),
    DTOutput("deserved_table")
  )
)

server <- function(input, output, session) {
  
  # ── Navigation ─────────────────────────────────────────────────────────────
  page <- reactiveVal("home")
  
  observeEvent(input$btn_journey, page("journey"))
  observeEvent(input$btn_managers, page("managers"))
  observeEvent(input$btn_players, page("players"))
  observeEvent(input$btn_deserved, page("deserved"))
  observeEvent(input$back1, page("home"))
  observeEvent(input$back2, page("home"))
  observeEvent(input$back3, page("home"))
  observeEvent(input$back4, page("home"))
  
  observe({
    session$sendCustomMessage("setPage", page())
  })
  
  # ── Tournament Journey ─────────────────────────────────────────────────────
  output$ivory_coast_summary <- renderTable({ ivory_coast_summary })
  output$cuumulative_ivorycoast_xgplot  <- renderPlot({ cuumulative_ivorycoast_xgplot })
  output$ivory_coast_timeline <- renderTable({ ivory_coast_timeline })
  
  # ── Manager Comparison ─────────────────────────────────────────────────────
  output$manager_shotmap <- renderPlotly({ ggplotly(manager_comparison_shotmap, tooltip = "text") })
  output$shots_conceded_table <- renderDT({
    ivorycoast_managers_shotsconceded_xgagainst
  })
  output$shot_quality_table <- renderDT({ shotquality_vs_quantity },
                                             options = list(pageLength = 10,
                                                            order = list(list(1, "desc"))))
  output$manager_pass_heatmap <- renderPlot({ passing_heatmap })
  output$manager_defensive_table <- renderDT({ defensive_actions_manager_comparison },
                                             options = list(pageLength = 10,
                                                            order = list(list(1, "desc"))))
  output$manager_dribble_heatmap <- renderPlot({ dribble_manager_comparison_plot })
  
  # ── Key Players ────────────────────────────────────────────────────────────
  output$top5_shotmap <- renderPlotly({ ggplotly(final_shotmap, tooltip = "text") })
  output$goals_assists_table <- renderDT({ combined_goals_and_assists },
                                            options = list(pageLength = 10,
                                                           order = list(list(1, "desc"))))
  output$top5_dribble_heatmap   <- renderPlot({ top5_dribble_heatmap })
  output$player_defensive_table <- renderDT({ defensive_stats },
                                            options = list(pageLength = 10,
                                                           order = list(list(1, "desc"))))
  output$top5_pass_heatmap <- renderPlot({ top5_passers_heatmap })
  output$attacking_table <- renderDT({ attacking_metrics },
                                            options = list(pageLength = 10,
                                                           order = list(list(1, "desc"))))
  
  # ── Was It Deserved ────────────────────────────────────────────────────────
  output$deserved_table <- renderDT({ deserved_winner_tournament },
                                    options = list(pageLength = 25,
                                                   order = list(list(
                                                     ncol(deserved_winner_tournament) - 1,
                                                     "desc"))))
}

shinyApp(ui, server)

library(shin)

# Save each heatmap as a PNG instead
detach("package:plotly", unload = TRUE)

ggsave("app/top5_passers_heatmap.png", top5_passers_heatmap, width= 10 , height = 6)
ggsave("app/top5_dribble_heatmap.png",       top5_dribble_heatmap,            width = 10, height = 6)
ggsave("app/dribble_manager_comparison.png", dribble_manager_comparison_plot, width = 10, height = 6)
ggsave("app/passing_heatmap.png",            passing_heatmap,                 width = 10, height = 6)

library(plotly)
