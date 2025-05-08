#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
library(elo)
library(data.table)
library(purrr)
#####################################################
results <- fetch_results_afltables(1897:2025)
colnames(results)
restructure_afl_data <- function(afl_data) {
  afl_home <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Round = afl_data$Round.Number,
    Game_Id = paste0(afl_data$Date, "_", afl_data$Home.Team, "_vs_", afl_data$Away.Team),
    Venue = afl_data$Venue,
    Team = afl_data$Home.Team,
    Opponent = afl_data$Away.Team,
    Result = ifelse(afl_data$Home.Points > afl_data$Away.Points, "W", "L"),
    Points_For = afl_data$Home.Points,
    Points_Against = afl_data$Away.Points,
    Spread = afl_data$Home.Points - afl_data$Away.Points,
    Played = TRUE,
    Home = TRUE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA,
    Home.team = afl_data$Home.Team,
    Away.team = afl_data$Away.Team
  )
  afl_away <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Round = afl_data$Round.Number,
    Game_Id = paste0(afl_data$Date, "_", afl_data$Home.Team, "_vs_", afl_data$Away.Team),
    Venue = afl_data$Venue,
    Team = afl_data$Away.Team,
    Opponent = afl_data$Home.Team,
    Result = ifelse(afl_data$Away.Points > afl_data$Home.Points, "W", "L"),
    Points_For = afl_data$Away.Points,
    Points_Against = afl_data$Home.Points,
    Spread = afl_data$Away.Points - afl_data$Home.Points,
    Played = TRUE,
    Home = FALSE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA,
    Home.team = afl_data$Home.Team,
    Away.team = afl_data$Away.Team
  )
  return(bind_rows(afl_home, afl_away))
}
results <- restructure_afl_data(results)

results <- results %>%
  filter(!(Team %in% c("Fitzroy", "University") | Opponent %in% c("Fitzroy", "University"))) # Don't exist anymore

colnames(results)
colSums(is.na(results)) # ELO will be NA
#####################################################
results <- results %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)
#####################################################
results <- results %>%
  mutate(Result_Binary = ifelse(Result == 1, 1, 0))

results <- results %>% arrange(Date, Season, Round, Game_Id)

teams_elo <- unique(c(results$Team))

teams_elo <- data.frame(
  Team = teams_elo,
  ELO = 1500,
  stringsAsFactors = FALSE
)

teams_elo <- teams_elo[order(teams_elo$Team), ]

for(i in 1:nrow(results)){
  if(i %% 2 != 0){ 
    # i = 1
    print(i)
    
    Team_A <- results$Team[i]
    Team_B <- results$Team[i+1]
    
    Result_A <- results$Result[i]
    Result_B <- results$Result[i+1]
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    results$ELO[i] <- ELO_A
    results$Opp_ELO[i] <- ELO_B
    
    results$ELO[i+1] <- ELO_B
    results$Opp_ELO[i+1] <- ELO_A
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 20 * (Result_A - E_A) 
    Elo_Updated_B <- ELO_B + 20 * (Result_B - E_B)
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

results <- results %>%
  mutate(Elo_Difference = ELO - Opp_ELO)

colnames(results)

results <- results %>% select(
  Date, Season, Round, Game_Id, Game_ID, Venue, Team, Opponent, Points_For, Points_Against, Result, Spread, Home, ELO, Opp_ELO,
  Elo_Difference, Home.team, Away.team
)
#####################################################
# H2H stats
h2h_sum <- results %>%
  group_by(Team, Opponent) %>%
  summarise(
    Games = n(),
    Wins = sum(Result == 1),
    Losses = sum(Result == 0),
    Draws = sum(Result == 0.5),
    WinRate = mean(Result),
    AvgMargin = mean(Spread),
    CloseGames = sum(abs(Spread) <= 6),
    .groups = 'drop'
  ) %>%
  arrange(WinRate, AvgMargin)

boogey_teams <- h2h_sum %>%
  filter(Games >= 10, WinRate <= 0.35, AvgMargin < 0)
boogey_carlton <- boogey_teams %>% filter(Team == "Carlton")

# Each teams worst matchup
boogey_worst <- h2h_sum %>%
  group_by(Team) %>%
  slice_min(order_by = WinRate, n = 1, with_ties = FALSE) %>%
  arrange(WinRate)

# Pick the worst matchup (lowest winrate) per team
worst_matchups <- h2h_sum %>%
  group_by(Team) %>%
  filter(Games >= 10) %>%
  slice_min(order_by = WinRate, n = 1, with_ties = FALSE)

ggplot(worst_matchups, aes(x = reorder(Team, WinRate), y = WinRate, fill = Opponent)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Boogey Teams: Worst Win Rate Matchups",
    x = "Team",
    y = "Win Rate",
    fill = "Opponent"
  ) +
  theme_minimal()

ggplot(worst_matchups, aes(x = reorder(Team, AvgMargin), y = AvgMargin, fill = Opponent)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Boogey Teams: Average Margin in Worst Matchup",
    x = "Team",
    y = "Average Margin (Negative = Losing)",
    fill = "Opponent"
  ) +
  theme_minimal()
#####################################################
# Hawks havent defeated Melbourne since 2018 (written in 2025 Round 9)
melb_hawks <- results %>%
  filter((Team == "Hawthorn" & Opponent == "Melbourne") | 
           (Team == "Melbourne" & Opponent == "Hawthorn"))

hawks_v_melb <- melb_hawks %>%
  filter(Team == "Hawthorn") %>%
  summarise(
    Games = n(),
    Wins = sum(Result == 1),
    Losses = sum(Result == 0),
    Draws = sum(Result == 0.5),
    WinRate = mean(Result),
    AvgMargin = mean(Spread),
    CloseGames = sum(abs(Spread) <= 12)
  )

print(hawks_v_melb)

ggplot(melb_hawks %>% filter(Team == "Hawthorn"), aes(x = Date, y = Spread)) +
  geom_line(color = "red") +
  geom_point(aes(color = Result), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Hawthorn's Margins vs Melbourne Over Time",
    y = "Margin (Positive = Win)",
    x = "Year"
  ) +
  theme_minimal()
#####################################################
# Special games tend to always be close?
rivalries <- list(
  c("Collingwood", "Melbourne"),
  c("Carlton", "Richmond"),
  c("Collingwood", "Essendon"),
  c("Port Adelaide", "Adelaide"),
  c("West Coast", "Fremantle")
)

rivalry_games <- purrr::map_dfr(rivalries, function(teams) {
  results %>%
    filter((Team == teams[1] & Opponent == teams[2]) | (Team == teams[2] & Opponent == teams[1])) %>%
    mutate(Rivalry = paste(teams[1], "vs", teams[2]))
})

rivalry_summary <- rivalry_games %>%
  group_by(Rivalry) %>%
  summarise(
    Total_Games = n(),
    Close_Games = sum(abs(Spread) <= 12),
    Close_Game_Pct = round(mean(abs(Spread) <= 12) * 100, 1),
    Avg_Margin = round(mean(abs(Spread)), 1),
    .groups = "drop"
  )

print(rivalry_summary)

ggplot(rivalry_summary, aes(x = reorder(Rivalry, Close_Game_Pct), y = Close_Game_Pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Close Games (≤12 pts) in AFL Rivalries",
    x = "Rivalry",
    y = "Percentage of Close Games"
  ) +
  theme_minimal()
#####################################################
# Player stats
stats <- fetch_player_stats_afltables(1990:2025)
colnames(stats)
colSums(is.na(stats))

stats <- stats %>%
  mutate(
    Brownlow.Votes = replace_na(Brownlow.Votes, 0),
    Bounces = replace_na(Bounces, 0),
    Time.on.Ground = replace_na(Time.on.Ground, 0)
  )

stats <- stats %>%
  select(
    Date, Season, Round, 
    Home.team, Away.team, 
    Player, ID, Team, 
    Kicks, Marks, Handballs, Disposals, Goals, Behinds,
    Tackles, Rebounds, Inside.50s, Clearances, Clangers, Brownlow.Votes,
    Contested.Possessions, Uncontested.Possessions, Contested.Marks,
    Marks.Inside.50, One.Percenters, Goal.Assists, Time.on.Ground,
    Age, Career.Games, Coach
  ) %>%
  arrange(Date, Season, Round, Team) %>%
  mutate(
    Game_Id = paste0(Date, "_", Home.team, "_vs_", Away.team)
  )

colSums(is.na(stats))

stats <- na.omit(stats)

career_avg <- stats %>%
  group_by(Player) %>%
  summarise(
    Career_Disposals = mean(Disposals, na.rm = TRUE),
    Career_Goals = mean(Goals, na.rm = TRUE),
    Career_Tackles = mean(Tackles, na.rm = TRUE),
    Career_Votes = mean(Brownlow.Votes, na.rm = TRUE),
    .groups = "drop"
  )

player_opponent_perf <- stats %>%
  group_by(Player, Team, Opponent = ifelse(Team == Home.team, Away.team, Home.team)) %>%
  summarise(
    Games = n(),
    Avg_Disposals = mean(Disposals, na.rm = TRUE),
    Avg_Goals = mean(Goals, na.rm = TRUE),
    Avg_Tackles = mean(Tackles, na.rm = TRUE),
    Avg_Votes = mean(Brownlow.Votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Games >= 5) %>%
  left_join(career_avg, by = "Player") %>%
  mutate(
    Disp_Diff = Avg_Disposals - Career_Disposals,
    Goal_Diff = Avg_Goals - Career_Goals,
    Tackle_Diff = Avg_Tackles - Career_Tackles,
    Votes_Diff = Avg_Votes - Career_Votes
  )

plot_data <- player_opponent_perf %>%
  select(Player, Opponent, Disp_Diff, Goal_Diff, Tackle_Diff, Votes_Diff) %>%
  pivot_longer(
    cols = ends_with("_Diff"),
    names_to = "Stat",
    values_to = "Difference"
  ) %>%
  mutate(
    Stat = case_when(
      Stat == "Disp_Diff" ~ "Disposals",
      Stat == "Goal_Diff" ~ "Goals",
      Stat == "Tackle_Diff" ~ "Tackles",
      Stat == "Votes_Diff" ~ "Brownlow Votes"
    )
  )

player_name <- "Patrick Dangerfield"

ggplot(plot_data %>% filter(Player == player_name), 
       aes(x = reorder(Opponent, Difference), y = Difference, fill = Stat)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Stat, scales = "free_y") +
  coord_flip() +
  labs(
    title = paste("Performance vs Opponents Compared to Career Avg:", player_name),
    x = "Opponent",
    y = "Difference from Career Average"
  ) +
  theme_minimal()

# Highlighting certain players
highlight_players <- player_opponent_perf %>%
  filter(
    Disp_Diff >= 5 | Disp_Diff <= -5 |
      Goal_Diff >= 1 | Goal_Diff <= -1 |
      Tackle_Diff >= 2 | Tackle_Diff <= -2 |
      Votes_Diff >= 1 | Votes_Diff <= -1
  ) %>%
  arrange(desc(Disp_Diff), desc(Goal_Diff), desc(Votes_Diff))

# Top 10 overperformers
highlight_players %>% 
  arrange(desc(Disp_Diff)) %>% 
  select(Player, Opponent, Games, Disp_Diff, Goal_Diff, Tackle_Diff, Votes_Diff) %>% 
  head(10)

# Top 10 underperformers
highlight_players %>% 
  arrange(Disp_Diff) %>% 
  select(Player, Opponent, Games, Disp_Diff, Goal_Diff, Tackle_Diff, Votes_Diff) %>% 
  head(10)

# Get top and bottom 10 based on disposal difference
top10 <- highlight_players %>%
  arrange(desc(Disp_Diff)) %>%
  slice_head(n = 10)

bottom10 <- highlight_players %>%
  arrange(Disp_Diff) %>%
  slice_head(n = 10)

# Combine and plot
top_bottom <- bind_rows(top10, bottom10)

ggplot(top_bottom, 
       aes(x = reorder(paste(Player, Opponent, sep = " vs "), Disp_Diff), y = Disp_Diff)) +
  geom_col(aes(fill = Disp_Diff > 0)) +
  scale_fill_manual(values = c("firebrick", "forestgreen"), labels = c("Struggles", "Dominates")) +
  coord_flip() +
  labs(
    title = "Top 10 Overperformers and Underperformers by Disposals vs Opponents",
    x = "Player vs Opponent",
    y = "Disposals Difference from Career Average",
    fill = "Performance"
  ) +
  theme_minimal()
#####################################################
# Do players play better against worse teams and worse agaisnt better teams?
# Merge player stats with opponent ELO
player_stats_with_elo <- stats %>%
  left_join(results %>% select(Game_Id, Team, Opp_ELO), by = c("Game_Id", "Team")) %>%
  filter(!is.na(Opp_ELO)) %>%
  mutate(Opp_Strength = -Opp_ELO)  # Higher = weaker opponent

# League-wide correlation (baseline)
correlation_results <- player_stats_with_elo %>%
  summarise(
    Disposals_ELO_Cor = cor(Disposals, Opp_ELO),
    Goals_ELO_Cor = cor(Goals, Opp_ELO),
    Votes_ELO_Cor = cor(Brownlow.Votes, Opp_ELO)
  )

correlation_against_weak <- player_stats_with_elo %>%
  summarise(
    Disposals_vs_Weak = cor(Disposals, Opp_Strength),
    Goals_vs_Weak = cor(Goals, Opp_Strength),
    Votes_vs_Weak = cor(Brownlow.Votes, Opp_Strength)
  )

print(correlation_results)
print(correlation_against_weak)

# Per-player correlation with opponent strength/weakness
player_strength_correlation <- player_stats_with_elo %>%
  group_by(Player) %>%
  filter(n() >= 20) %>%
  summarise(
    Games = n(),
    Disposals_vs_Weak = cor(Disposals, Opp_Strength),
    Goals_vs_Weak = cor(Goals, Opp_Strength),
    Votes_vs_Weak = cor(Brownlow.Votes, Opp_Strength),
    .groups = "drop"
  )

# Visualise distribution of correlation
ggplot(player_strength_correlation, aes(x = Disposals_vs_Weak)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  labs(
    title = "Disposals vs Opponent Weakness (ELO) Correlation",
    x = "Correlation (Higher = Better vs Weaker Teams)",
    y = "Number of Players"
  ) +
  theme_minimal()

# Top 10 flat-track bullies
top_flat_track <- player_strength_correlation %>%
  arrange(desc(Disposals_vs_Weak)) %>%
  slice_head(n = 10)

# Top 10 big-game players
top_big_game <- player_strength_correlation %>%
  arrange(Disposals_vs_Weak) %>%
  slice_head(n = 10)

# Players who perform worse vs weak teams (unexpected)
players_worse_vs_weak <- player_strength_correlation %>%
  filter(Disposals_vs_Weak < -0.2 | Goals_vs_Weak < -0.2 | Votes_vs_Weak < -0.2) %>%
  arrange(Disposals_vs_Weak)

# Print tables
print(top_flat_track)
print(top_big_game)
print(players_worse_vs_weak %>% slice_head(n = 10))

player_name <- "Shaun Higgins"

player_example <- player_stats_with_elo %>%
  filter(Player == player_name)

ggplot(player_example, aes(x = Opp_Strength, y = Disposals)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = paste("Disposals vs Opponent Weakness for", player_name),
    x = "Opponent Weakness (Higher = Weaker Team)",
    y = "Disposals"
  ) +
  theme_minimal()
