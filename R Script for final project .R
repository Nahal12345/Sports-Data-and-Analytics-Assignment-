install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
1
library(StatsBombR)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(gt)

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



egyptdrcongo <- afcon_matches %>% 
  filter(match_id == 3922240)
egyptdrcongo_events <- egyptdrcongo %>% 
  get.matchFree()
view(egyptdrcongo_events)


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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Nigeria")
nigeria_final_totalteamxg <- nigeria_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
nigeria_final_totalteamxg

nigeria_totalcompetiton_xg <- nigeria_groupstage_game1_totalteamxg$total_xg + nigeria_groupstage_game2_totalteamxg$total_xg + nigeria_groupstage_game3_totalteamxg$total_xg + nigeria_RO16_totalteamxg$total_xg + nigeria_quarterfinal_totalteamxg$total_xg + nigeria_semifinal_totalteamxg$total_xg + nigeria_final_totalteamxg$total_xg
#13.67203


#IVORY COAST SECTION ----

ivorycoast_matches <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | away_team.away_team_name == "Côte d'Ivoire")
view(ivorycoast_matches)

#Ivory coast groupstage game 1 
ivorycoast_groupstage_game1 <- ivorycoast_matches %>% 
  filter(match_id == 3920384)
ivorycoast_groupstage_game1_events <- ivorycoast_groupstage_game1 %>% 
  get.matchFree()
ivorycoast_groupstage_game1_xg <- ivorycoast_groupstage_game1_events %>% 
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
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
  filter(type.name == "Shot") %>% 
  select(id, minute, second, possession_team.name, player.name, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
  filter(team.name == "Côte d'Ivoire")
ivorycoast_final_totalteamxg <- ivorycoast_final_xg %>% 
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE))
ivorycoast_final_totalteamxg

#Ivory Coast total team xg

ivorycoast_totalcompetiton_xg <- ivorycoast_groupstage_game1_totalteamxg$total_xg + ivorycoast_groupstage_game2_totalteamxg$total_xg + ivorycoast_groupstage_game3_totalteamxg$total_xg + ivorycoast_ro16_totalteamxg$total_xg + ivorycoast_quarterfinal_totalteamxg$total_xg + ivorycoast_semifinal_totalteamxg$total_xg + ivorycoast_final_totalteamxg$total_xg
#11.52261

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


# Top scorers in the tournament with xg ----


top_scorers <- all_events %>%
  filter(type.name == "Shot", shot.outcome.name == "Goal") %>%
  filter(period != 5) %>%
  group_by(player.name, team.name) %>%
  summarise(goals = n(), .groups = "drop") %>%
  arrange(desc(goals)) %>%
  slice_head(n = 10)

top_scorers_xg <- all_events %>%
  filter(type.name == "Shot") %>%
  filter(period != 5) %>%
  group_by(player.name) %>%
  summarise(xg = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2), .groups = "drop")

top_scorers <- top_scorers %>%
  left_join(top_scorers_xg, by = "player.name") %>%
  pivot_longer(cols = c(goals, xg), names_to = "metric", values_to = "value")

ggplot(top_scorers, aes(x = reorder(paste(player.name, paste0("(", team.name, ")")), value), 
                        y = value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("goals" = "red", "xg" = "lightblue"),
                    labels = c("Goals", "xG")) +
  labs(
    title = "Top Goalscorers vs xG at AFCON 2023",
    subtitle = "Green = Goals Scored, Orange = Expected Goals (xG)",
    x = "Player",
    y = "Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


#Creating a passing heatmap for Nigeria ----

view(nigeria_final_event)

view(finalbreakdown_nigeria)

# Step 1 — fix the base dataframe first
nigeria_final_event <- nigeria_final_event %>%
  mutate(
    location_x = sapply(location, function(loc) as.numeric(loc[[1]][1])),
    location_y = sapply(location, function(loc) as.numeric(loc[[2]][1]))
  )

finalbreakdown_nigeria <- nigeria_final_event %>%
  filter(team.name == "Nigeria",
         type.name == "Pass") %>% 
  select(id, minute, second, type.name, possession_team.name, player.name, 
         pass.outcome.name, pass.recipient.name, pass.type.name, 
         location, location_x, location_y)

class(finalbreakdown_nigeria$location_x)  # must say "numeric"
head(finalbreakdown_nigeria$location_x) 

finalbreakdown_nigeria <- finalbreakdown_nigeria %>%
  mutate(
    location_x = unlist(location_x),
    location_y = unlist(location_y)
  )


pitch_theme <- theme_void() +
  theme(
    plot.background  = element_rect(fill = "#1a472a", colour = NA),
    panel.background = element_rect(fill = "#1a472a", colour = NA),
    plot.title       = element_text(colour = "white", face = "bold",
                                    size = 16, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle    = element_text(colour = "#b0c4b1", size = 11,
                                    hjust = 0.5, margin = margin(b = 12)),
    legend.position  = "bottom",
    legend.title     = element_text(colour = "white", size = 10),
    legend.text      = element_text(colour = "white", size = 9)
  )

draw_pitch <- function() {
  list(
    # Pitch outline
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
             fill = NA, colour = "white", linewidth = 0.8),
    # Centre line
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80,
             colour = "white", linewidth = 0.6),
    # Centre circle
    annotate("path",
             x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 100)),
             y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 100)),
             colour = "white", linewidth = 0.6),
    # Centre spot
    annotate("point", x = 60, y = 40, colour = "white", size = 1.5),
    # Left penalty area
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
             fill = NA, colour = "white", linewidth = 0.6),
    # Left 6-yard box
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
             fill = NA, colour = "white", linewidth = 0.6),
    # Left penalty spot
    annotate("point", x = 12, y = 40, colour = "white", size = 1.5),
    # Right penalty area
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right 6-yard box
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right penalty spot
    annotate("point", x = 108, y = 40, colour = "white", size = 1.5),
    # Left goal
    annotate("rect", xmin = -2, xmax = 0, ymin = 36, ymax = 44,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right goal
    annotate("rect", xmin = 120, xmax = 122, ymin = 36, ymax = 44,
             fill = NA, colour = "white", linewidth = 0.6)
  )
}

# ── 2. All passes heat map ─────────────────────────────────────────────────────
ggplot(finalbreakdown_nigeria, aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha     = 0.75,
    contour_var = "ndensity",
    bins      = 12
  ) +
  scale_fill_viridis_d(option = "inferno", name = "Pass density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  pitch_theme +
  labs(
    title    = "AFCON 2023 - Nigeria — Pass Heat Map",
    subtitle = "Density of pass origin locations"
  )

# ── 3. Heat map split by pass outcome ─────────────────────────────────────────
# (Complete vs Incomplete; drops rows where outcome is NA = successful pass in StatsBomb)
finalbreakdown_nigeria %>%
  mutate(outcome = if_else(is.na(pass.outcome.name), "Complete", pass.outcome.name)) %>%
  filter(outcome %in% c("Complete", "Incomplete")) %>%
  ggplot(aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha       = 0.75,
    contour_var = "ndensity",
    bins        = 12
  ) +
  scale_fill_viridis_d(option = "plasma", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  facet_wrap(~outcome) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 12)) +
  labs(
    title    = "AFCON FINAL 2023 - Nigeria — Pass Locations by Outcome",
    subtitle = "Complete vs Incomplete passes"
  )

# ── 4. Heat map split by player (top N passers) ───────────────────────────────
top_n_players <- 6   # change as needed


top_passers <- finalbreakdown_nigeria %>%
  count(player.name, sort = TRUE) %>%
  slice_head(n = top_n_players) %>%
  pull(player.name)

finalbreakdown_nigeria %>%
  filter(player.name %in% top_passers) %>%
  ggplot(aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha       = 0.75,
    contour_var = "ndensity",
    bins        = 10
  ) +
  scale_fill_viridis_d(option = "mako", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  facet_wrap(~player.name, ncol = 3) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 9)) +
  labs(
    title    = "Nigeria — Individual Pass Heat Maps",
    subtitle = paste("Top", top_n_players, "passers by volume")
  )




# Check Nigeria
class(finalbreakdown_nigeria$location_x)
class(finalbreakdown_nigeria$location_y)
head(finalbreakdown_nigeria$location_x)

# Check Ivory Coast
class(finalbreakdown_ivorycoast$location_x)
class(finalbreakdown_ivorycoast$location_y)
head(finalbreakdown_ivorycoast$location_x)

# Ivory Coast final pass heatmap ---- 


finalbreakdown_ivorycoast <- ivorycoast_final_event %>%
  filter(team.name == "Côte d'Ivoire",
         type.name == "Pass") %>% 
  select(id, minute, second, type.name, possession_team.name, player.name, pass.outcome.name, pass.recipient.name, pass.type.name, location, location_x, location_y)

view(ivorycoast_final_event)
view(finalbreakdown_nigeria)


ivorycoast_final_event <- ivorycoast_final_event %>%
  mutate(
    location_x = sapply(location, `[[`, 1),
    location_y = sapply(location, `[[`, 2)
  )

finalbreakdown_ivorycoast <- finalbreakdown_ivorycoast %>%
  mutate(
    location_x = unlist(location_x),
    location_y = unlist(location_y)
  )

write.csv(finalbreakdown_nigeria, "nigeria_passes.csv", row.names = FALSE)


pitch_theme <- theme_void() +
  theme(
    plot.background  = element_rect(fill = "#1a472a", colour = NA),
    panel.background = element_rect(fill = "#1a472a", colour = NA),
    plot.title       = element_text(colour = "white", face = "bold",
                                    size = 16, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle    = element_text(colour = "#b0c4b1", size = 11,
                                    hjust = 0.5, margin = margin(b = 12)),
    legend.position  = "bottom",
    legend.title     = element_text(colour = "white", size = 10),
    legend.text      = element_text(colour = "white", size = 9)
  )

draw_pitch <- function() {
  list(
    # Pitch outline
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
             fill = NA, colour = "white", linewidth = 0.8),
    # Centre line
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80,
             colour = "white", linewidth = 0.6),
    # Centre circle
    annotate("path",
             x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 100)),
             y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 100)),
             colour = "white", linewidth = 0.6),
    # Centre spot
    annotate("point", x = 60, y = 40, colour = "white", size = 1.5),
    # Left penalty area
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
             fill = NA, colour = "white", linewidth = 0.6),
    # Left 6-yard box
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
             fill = NA, colour = "white", linewidth = 0.6),
    # Left penalty spot
    annotate("point", x = 12, y = 40, colour = "white", size = 1.5),
    # Right penalty area
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right 6-yard box
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right penalty spot
    annotate("point", x = 108, y = 40, colour = "white", size = 1.5),
    # Left goal
    annotate("rect", xmin = -2, xmax = 0, ymin = 36, ymax = 44,
             fill = NA, colour = "white", linewidth = 0.6),
    # Right goal
    annotate("rect", xmin = 120, xmax = 122, ymin = 36, ymax = 44,
             fill = NA, colour = "white", linewidth = 0.6)
  )
}

# ── 2. All passes heat map ─────────────────────────────────────────────────────
ggplot(finalbreakdown_ivorycoast, aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha     = 0.75,
    contour_var = "ndensity",
    bins      = 12
  ) +
  scale_fill_viridis_d(option = "inferno", name = "Pass density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  pitch_theme +
  labs(
    title    = "AFCON FINAL 2023 - Ivory Coast — Pass Heat Map",
    subtitle = "Density of pass origin locations"
  )

# ── 3. Heat map split by pass outcome ─────────────────────────────────────────

finalbreakdown_ivorycoast %>%
  mutate(outcome = if_else(is.na(pass.outcome.name), "Complete", pass.outcome.name)) %>%
  filter(outcome %in% c("Complete", "Incomplete")) %>%
  ggplot(aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha       = 0.75,
    contour_var = "ndensity",
    bins        = 12
  ) +
  scale_fill_viridis_d(option = "plasma", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  facet_wrap(~outcome) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 12)) +
  labs(
    title    = "AFCON FINAL 2023 - Ivory Coast — Pass Locations by Outcome",
    subtitle = "Complete vs Incomplete passes"
  )

# 4. Heat map split by player (top N passers) 
top_n_players <- 6   # change as needed

top_passers <- finalbreakdown_ivorycoast %>%
  count(player.name, sort = TRUE) %>%
  slice_head(n = top_n_players) %>%
  pull(player.name)

finalbreakdown_ivorycoast %>%
  filter(player.name %in% top_passers) %>%
  ggplot(aes(x = location_x, y = location_y)) +
  draw_pitch() +
  stat_density_2d_filled(
    aes(fill = after_stat(level)),
    alpha       = 0.75,
    contour_var = "ndensity",
    bins        = 10
  ) +
  scale_fill_viridis_d(option = "mako", name = "Density") +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  facet_wrap(~player.name, ncol = 3) +
  pitch_theme +
  theme(strip.text = element_text(colour = "white", face = "bold", size = 9)) +
  labs(
    title    = "Ivory Coast — Individual Pass Heat Maps",
    subtitle = paste("Top", top_n_players, "passers by volume")
  )

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
    Nigeria         = md("🇳🇬 **Nigeria**"),
    Stat            = "",
    `Côte d'Ivoire` = md("🇨🇮 **Côte d'Ivoire**")
  ) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 1,   decimals = 1) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 2,   decimals = 2) %>%
  fmt_number(columns = c("Nigeria", "Côte d'Ivoire"), rows = 3:12, decimals = 0) %>%
  tab_style(
    style     = list(cell_fill(color = "#1e1e2e"),
                     cell_text(color = "white", weight = "bold", size = px(16))),
    locations = cells_title()
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#2a2a3e"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#12421a"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = "Nigeria")
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#5a2000"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = "Côte d'Ivoire")
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#1e1e2e"),
                     cell_text(color = "#cccccc", style = "italic")),
    locations = cells_body(columns = "Stat")
  ) %>%
  tab_style(
    style     = cell_fill(color = "#161626"),
    locations = cells_body(rows = seq(1, 12, 2))
  ) %>%
  tab_style(
    style     = cell_fill(color = "#12421a"),
    locations = cells_body(columns = "Nigeria", rows = seq(1, 12, 2))
  ) %>%
  tab_style(
    style     = cell_fill(color = "#5a2000"),
    locations = cells_body(columns = "Côte d'Ivoire", rows = seq(1, 12, 2))
  ) %>%
  tab_options(
    table.background.color            = "#1e1e2e",
    table.border.top.color            = "transparent",
    table.border.bottom.color         = "transparent",
    column_labels.border.top.color    = "transparent",
    column_labels.border.bottom.color = "#444466",
    data_row.padding                  = px(10),
    table.font.size                   = px(14)
  ) %>%
  tab_source_note(source_note = md("*Data: StatsBomb*"))

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
    played  = n(),
    wins    = sum(won),
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

# Passing Structures


# Defensive structures 


#Compore the shot conceded and xg performance between the two managers 

ivorycoast_managers_shotscondeded_xgagainst <- afcon_matches %>%
  filter(home_team.home_team_name == "Côte d'Ivoire" | 
           away_team.away_team_name == "Côte d'Ivoire") %>%
  select(match_id, competition_stage.name) %>%
  mutate(stage = if_else(competition_stage.name == "Group Stage", 
                         "Group Stage", "Knockout")) %>%
  left_join(
    all_events %>%
      filter(type.name == "Shot", team.name != "Côte d'Ivoire", period != 5) %>%
      group_by(match_id) %>%
      summarise(
        shots_conceded = n(),
        xg_conceded    = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2)
      ),
    by = "match_id"
  ) %>%
  group_by(stage) %>%
  summarise(
    matches        = n(),
    shots_conceded = sum(shots_conceded, na.rm = TRUE),
    xg_conceded    = round(sum(xg_conceded, na.rm = TRUE), 2),
    shots_conceded_per_game = round(shots_conceded / matches, 1),
    xg_conceded_per_game    = round(xg_conceded / matches, 2)
  ) %>% 
  mutate(manager = case_when(
    stage == "Group Stage" ~ "Jean-Louis Gasset",
    stage == "Knockout" ~ "Emerse Faé"
  ))




# Attacking areas focus 





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
        total_shots     = n(),
        shots_on_target = sum(shot.outcome.name %in% c("Saved", "Goal"), na.rm = TRUE),
        total_xg        = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2),
        avg_xg_per_shot = round(mean(shot.statsbomb_xg, na.rm = TRUE), 3),
        goals           = sum(shot.outcome.name == "Goal", na.rm = TRUE)
      ),
    by = "match_id"
  ) %>%
  group_by(manager) %>%
  summarise(
    matches         = n(),
    total_shots     = sum(total_shots,     na.rm = TRUE),
    shots_on_target = sum(shots_on_target, na.rm = TRUE),
    total_xg        = round(sum(total_xg,  na.rm = TRUE), 2),
    goals           = sum(goals,           na.rm = TRUE),
    shots_per_game  = round(total_shots     / matches, 1),
    avg_xg_per_shot = round(total_xg        / total_shots, 3),
    shot_accuracy   = round(shots_on_target / total_shots * 100, 1)
  ) %>%
  arrange(desc(avg_xg_per_shot))

ivorycoast_shot_quality

# Physical intensity -> pressures, sprints, duels 



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
        setpiece_shots     = n(),
        setpiece_sot       = sum(shot.outcome.name %in% c("Saved", "Goal"), na.rm = TRUE),
        setpiece_goals     = sum(shot.outcome.name == "Goal",               na.rm = TRUE),
        setpiece_bigchance = sum(shot.statsbomb_xg > 0.3,                   na.rm = TRUE),
        setpiece_xg        = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2)
      ),
    by = "match_id"
  ) %>%
  group_by(manager) %>%
  summarise(
    matches            = n(),
    setpiece_shots     = sum(setpiece_shots,     na.rm = TRUE),
    setpiece_sot       = sum(setpiece_sot,       na.rm = TRUE),
    setpiece_goals     = sum(setpiece_goals,      na.rm = TRUE),
    setpiece_bigchance = sum(setpiece_bigchance,  na.rm = TRUE),
    setpiece_xg        = round(sum(setpiece_xg,  na.rm = TRUE), 2),
    shots_per_game     = round(setpiece_shots / matches, 1),
    xg_per_game        = round(setpiece_xg    / matches, 2),
    conversion_rate    = round(setpiece_goals  / ifelse(setpiece_shots == 0, 1, setpiece_shots) * 100, 1),
    sot_pct            = round(setpiece_sot    / ifelse(setpiece_shots == 0, 1, setpiece_shots) * 100, 1)
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
    x     = sapply(location,          `[[`, 1),
    y     = sapply(location,          `[[`, 2),
    end_x = sapply(shot.end_location, `[[`, 1),
    end_y = sapply(shot.end_location, `[[`, 2)
  ) %>%
  mutate(
    shot_type = case_when(
      shot.outcome.name == "Goal"          ~ "Goal",
      shot.outcome.name == "Saved"         ~ "Saved",
      shot.outcome.name == "Saved to Post" ~ "Saved",
      shot.outcome.name == "Blocked"       ~ "Blocked",
      TRUE                                 ~ "Off Target"
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
    data   = filter(ivorycoast_top5_shooters, !is_goal),
    aes(x = x, y = y, color = shot_type),
    fill   = NA,
    size   = 3,
    shape  = 21,
    stroke = 1.2,
    alpha  = 0.85
  ) +
  geom_point(
    data   = filter(ivorycoast_top5_shooters, is_goal),
    aes(x = x, y = y),
    color  = "white",
    fill   = "white",
    size   = 4.5,
    shape  = 21,
    stroke = 1.5
  ) +
  scale_color_manual(values = outcome_colours, guide = "none") +
  coord_flip(xlim = c(58, 123), ylim = c(0, 80), expand = FALSE) +
  scale_x_continuous() +
  scale_y_reverse() +
  facet_wrap(~player.name, ncol = 3) +
  labs(
    title    = "Côte d'Ivoire — Top 5 Shooters Shot Map",
    subtitle = "○ Off Target / Blocked  ·  ● Saved  ·  ● Goal",
    caption  = "Data: StatsBomb  |  Excludes penalties"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#1e1e2e", color = NA),
    panel.background = element_rect(fill = "#1e1e2e", color = NA),
    plot.title       = element_text(color = "#f0f0f0", face = "bold",
                                    hjust = 0.5, size = 16,
                                    margin = margin(t = 14, b = 4)),
    plot.subtitle    = element_text(color = "#aaaaaa", hjust = 0.5,
                                    size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(color = "#666666", hjust = 0.98,
                                    size = 9, margin = margin(t = 6, b = 10)),
    strip.text       = element_text(color = "white", face = "bold",
                                    size = 11, margin = margin(b = 6)),
    plot.margin      = margin(10, 20, 10, 20)
  )


# Expected goals graph, highlight ivory coast pkayers compared to the rest of the players in the tourney 





# Table of goals and assists for ivoru coast 


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

# Shot creating actions


# Defensive actions -> tackles, blocks etc 

tackle_stats <- all_events %>%
  filter(team.name == "Côte d'Ivoire", 
         type.name == "Duel",
         duel.type.name == "Tackle") %>%
  group_by(player.name) %>%
  summarise(
    attempted       = n(),
    won             = sum(duel.outcome.name == "Won",             na.rm = TRUE),
    lost_in_play    = sum(duel.outcome.name == "Lost In Play",    na.rm = TRUE),
    lost_out        = sum(duel.outcome.name == "Lost Out",        na.rm = TRUE),
    success_out     = sum(duel.outcome.name == "Success Out",     na.rm = TRUE),
    success_in_play = sum(duel.outcome.name == "Success In Play", na.rm = TRUE),
    success_pct     = round((won + success_out + success_in_play) / attempted * 100, 1)
  ) %>%
  arrange(desc(attempted))


# Chnace creation zones per player

# Touches in the oppposition box 



#############################################################################################################################

# Was it deserved ----


# Finishing efficiency


# Compare stats with opponents 

# Show analysis of the final

# Compare the stats and performance between Nigeria and Ivory Coast from the match in the group stage and the one in the final







