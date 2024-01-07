
# Import Libraries --------------------------------------------------------

pacman::p_load(nflverse, tidyverse, xts, espnscrapeR, ggtext, vroom, scales, 
               nflreadr, janitor, ggrepel, gganimate, gt, gtExtras, extrafont)

font_import()
loadfonts()

# Import Data -------------------------------------------------------------

games <- read_csv("Data/games.csv")
players <- read_csv("Data/players.csv")
plays <- read_csv("Data/plays.csv")
tackles <- read_csv("Data/tackles.csv")

#Read in weekly information
#Week_1 <- read_csv("Data/tracking_week_1.csv")
#Week_2 <- read_csv("Data/tracking_week_2.csv")
#Week_3 <- read_csv("Data/tracking_week_3.csv")
weeks <- 1:9

week_data <- list()

for (i in weeks) {
  file_path <- sprintf("Data/tracking_week_%d.csv", i)
  week_data[[i]] <- read_csv(file_path)
}

All_Weeks <- do.call(rbind, week_data)

# Import Additional Data --------------------------------------------------

Test <- load_participation(2022, include_pbp = TRUE)

Player_Stats <- load_player_stats(2022, stat_type = "defense") |> 
  select(player_display_name, team, headshot_url)

Test_filtered <- Test |> 
  select(old_game_id, play_id, posteam, defteam, qtr, down, ydstogo, play_type)

# Clean Data --------------------------------------------------------------

#Set nflId column to character values for matching purposes
players <- players |> 
  mutate(nflId = as.character(nflId))

plays <- plays |> 
  mutate(gameId = as.character(gameId),
         playId = as.character(playId))

tackles <- tackles |> 
  mutate(gameId = as.character(gameId),
         playId = as.character(playId),
         combined_id = paste(gameId, playId, nflId, sep = "_"))

#Merge with player values to bring in position data
Primary <- All_Weeks |> 
  mutate(Tackle = if_else(event == "tackle", "1", "0"),
         Tackle = ifelse(is.na(Tackle), "0", Tackle),
         Tackle = as.numeric(Tackle),
         nflId = ifelse(is.na(nflId), "Ball", nflId)) |> 
  left_join(players, by = "nflId") |> 
  mutate(gameId = as.character(gameId),
         playId = as.character(playId)) |> 
  left_join(plays, by = c("playId", "gameId"))

#Create separate data table for ball and others
Ball <- Primary |> 
  filter(nflId == "Ball")

defensive_positions <- c("DE", "NT", "SS", "FS", "OLB", "DT", "CB", "ILB", "MLB", "DB")

positions <- players |> 
  select(position, displayName)

defensive_line <- c("DE", "NT", "DT")
linebackers <- c("OLB", "ILB", "MLB")
secondary <- c("SS", "FS", "CB", "DB")

# Calculate Metrics for Analysis ------------------------------------------

#Proximity to ball
Primary_Step1 <- Primary |> 
  group_by(gameId, frameId, playId) |> 
  mutate(
    Ball_x = x[nflId == 'Ball'],
    Ball_y = y[nflId == 'Ball']) |> 
  rowwise() |> 
  mutate('Proximity_To_Ball' = ifelse(nflId != 'Ball', sqrt((x - Ball_x)^2 + (y - Ball_y)^2), NA)) |> 
  ungroup()

#Angle of player to ball

#Convert from angles to radians
Primary_Step1$o_radian <- Primary$o * (pi / 180)
Primary_Step1$dir_radian <- Primary$dir * (pi / 180)

Primary_Step2 <- Primary_Step1 |> 
  group_by(gameId, frameId, playId) |>
  mutate(Angle_To_Ball = ifelse(nflId != 'Ball', atan2(Ball_y - y, Ball_x - x), NA),
         Relative_Angle_To_Ball = ifelse(nflId != 'Ball', abs(o_radian - Angle_To_Ball), NA),
         Movement_Angle_To_Ball = ifelse(nflId != 'Ball', abs(dir_radian - Angle_To_Ball), NA)) |> 
  ungroup()

#Number of players within 3 yards
Primary_Step3 <- Primary_Step2 |> 
  group_by(gameId, frameId, playId) |>
  mutate(Players_Near_Ball = sum(Proximity_To_Ball <= 3, na.rm = TRUE)) 

#Only focus on defensive players in the dataset and remove unnecessary columns
Primary_Step4 <- Primary_Step3 |> 
  filter(position %in% defensive_positions) |> 
  select(gameId, playId, nflId, displayName.x, frameId, s, a, dis, o, dir, Tackle, position,
         Ball_x, Ball_y, Proximity_To_Ball, o_radian, dir_radian, Angle_To_Ball, Relative_Angle_To_Ball, 
         Movement_Angle_To_Ball, Players_Near_Ball) |> 
  mutate(combined_id = paste(gameId, playId, nflId, sep = "_"))

#Incorporate final player who made tackle to dataframe
Primary_Step5 <- Primary_Step4 |> 
  mutate(Player_Made_Tackle = as.numeric(combined_id %in% tackles$combined_id))

# Run Regression ----------------------------------------------------------

# Run regression to calculate tackle probability given inputs of proximity
# to ball, Angle to ball carrier, closing speed to ball, and number of players
# within 2 yards. Test on week 1 and see how predictive it is in future.
Model <- glm(Player_Made_Tackle ~ Proximity_To_Ball + 
               Relative_Angle_To_Ball + s + a + Players_Near_Ball, 
             data = Primary_Step5, family = binomial())

#Assess multicollinearity
Reg_Cor <- cor(Primary_Step5[, c("Proximity_To_Ball", "Relative_Angle_To_Ball", "s", "a", "Players_Near_Ball")])

summary(Model)

tackle_probabilities <- predict(Model, newdata = Primary_Step5, type = "response")
tackle_percentages <- tackle_probabilities * 100

Primary_Step5$Tackle_Probability <- tackle_percentages

# Show Example Play -------------------------------------------------------

# Run calculations using example play (from Bears game) during the season
problematic_group <- Primary_Step5 |> 
  filter(gameId == "2022091102", playId == "1107")

#Set color palette
cbp2 <- c("#AA0000", "#241773", "#0B162A", "#E64100", "#B0B7BC", "#002244",
                   "#203731", "#FFB612", "#006778", "#4B92DB", "#008E97")
                   
p <- ggplot(problematic_group, aes(x = frameId, y = Tackle_Probability, color = `displayName.x`)) +
  geom_line(linewidth = 0.75, linejoin = "round") +
  theme(
    text = element_text(family = "Georgia",
                        color = "black",
                        size = 12),
    plot.title = element_markdown(size = 13,
                                  vjust = .02,
                                  hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 8),
    legend.title.align = 0.5,
    legend.background = element_rect(color = "#fbfbfb", fill = "#fbfbfb"),
    panel.background = element_rect(fill = "#fbfbfb"),
    plot.background = element_rect(fill = "#fbfbfb"),
    legend.key = element_rect(color = "#fbfbfb"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_color_manual(values = cbp2) +
  xlab("Frame of Play") +
  ylab("Tackle Probability") +
  labs(
    title = "**Tackle Likelihood Throughout Duration of The Play**",
    subtitle = "Week 1 2022 | CHI vs. SF | Q2 13:43",
    legend = "Player") +
  guides(color = guide_legend(title = "Players"))
p

p + transition_reveal(frameId)

anim_save("player_trends.gif")

# Show Tackle Probabilities -----------------------------------------------

#Calculate Average Tackle Probability per Play
average_tackle_prob <- Primary_Step5 |> 
  group_by(combined_id) |> 
  mutate(count = n()) |> 
  ungroup() |> 
  filter(count >= 50) |> 
  group_by(displayName.x) |> 
  summarize(Average_Probability = mean(Tackle_Probability, na.rm = TRUE))

number_of_snaps <- Primary_Step5 |> 
  group_by(gameId, nflId) |> 
  mutate(number_of_plays = n_distinct(playId)) |> 
  ungroup() |> 
  group_by(displayName.x) |> 
  mutate(Average_Probability = mean(Tackle_Probability, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Expected_Tackles = Average_Probability/100 * number_of_plays)

Snaps_Variable <- number_of_snaps |> 
  select(nflId, displayName.x)

Tackle_Calcs <- number_of_snaps |> 
  filter(Player_Made_Tackle == 1) |> 
  group_by(gameId, nflId) |> 
  mutate(tackle_count = n_distinct(playId)) |> 
  ungroup() |> 
  filter(number_of_plays >= 25) |> 
  group_by(displayName.x) |> 
  summarize(Tackles_Over_Expected = round(tackle_count/Expected_Tackles, digits = 2)) |> 
  left_join(Player_Stats, by = c("displayName.x" = "player_display_name")) |> 
  distinct() |> 
  left_join(positions, by = c("displayName.x" = "displayName"))


# Create Tables -----------------------------------------------------------
Player_Table <- as.tibble(Tackle_Calcs) |> 
  arrange(desc(Tackles_Over_Expected)) |> 
  select(displayName.x, headshot_url, team, position, Tackles_Over_Expected) |>
  slice(1:20) |> 
  gt() |> 
  tab_header(
    title = md("**Average Tackles Over Expected**"),
    subtitle = md("*2022 Season | Over 50 Snaps Played")) |> 
  tab_stubhead(label = "Name") |> 
  gt_img_rows(headshot_url, height = 25) |> 
  gt_color_rows(Tackles_Over_Expected, palette = "ggsci::blue_material") |> 
  cols_label(
    displayName.x = "Name",
    team = "Team",
    position = "Position",
    Tackles_Over_Expected = "Tackles Over Expected",
    headshot_url = "") |> 
  cols_align(align = "center")
Player_Table

#Player Table for Defensive Line Only
DLine_Player_Table <- as.tibble(Tackle_Calcs) |> 
  arrange(desc(Tackles_Over_Expected)) |> 
  select(displayName.x, headshot_url, team, position, Tackles_Over_Expected) |>
  filter(position %in% defensive_line) |> 
  slice(1:10) |> 
  gt() |> 
  tab_header(
    title = md("**Average Tackles Over Expected**"),
    subtitle = md("*2022 Season | Over 50 Snaps Played")) |> 
  tab_stubhead(label = "Name") |> 
  gt_img_rows(headshot_url, height = 25) |> 
  gt_color_rows(Tackles_Over_Expected, palette = "ggsci::blue_material") |> 
  cols_label(
    displayName.x = "Name",
    team = "Team",
    position = "Position",
    Tackles_Over_Expected = "Tackles Over Expected",
    headshot_url = "") |> 
  cols_align(align = "center")
DLine_Player_Table

#Player Table for players in secondary only
Secondary_Player_Table <- as.tibble(Tackle_Calcs) |> 
  arrange(desc(Tackles_Over_Expected)) |> 
  select(displayName.x, headshot_url, team, position, Tackles_Over_Expected) |>
  filter(position %in% secondary) |> 
  slice(1:10) |> 
  gt() |> 
  tab_header(
    title = md("**Average Tackles Over Expected**"),
    subtitle = md("*2022 Season | Over 50 Snaps Played")) |> 
  tab_stubhead(label = "Name") |> 
  gt_img_rows(headshot_url, height = 25) |> 
  gt_color_rows(Tackles_Over_Expected, palette = "ggsci::blue_material") |> 
  cols_label(
    displayName.x = "Name",
    team = "Team",
    position = "Position",
    Tackles_Over_Expected = "Tackles Over Expected",
    headshot_url = "") |> 
  cols_align(align = "center")
Secondary_Player_Table

#Player table for linebackers only
Linebacker_Player_Table <- as.tibble(Tackle_Calcs) |> 
  arrange(desc(Tackles_Over_Expected)) |> 
  select(displayName.x, headshot_url, team, position, Tackles_Over_Expected) |>
  distinct(displayName.x, .keep_all = TRUE) |> 
  filter(position %in% linebackers) |> 
  slice(1:10) |> 
  gt() |> 
  tab_header(
    title = md("**Average Tackles Over Expected**"),
    subtitle = md("*2022 Season | Over 50 Snaps Played")) |> 
  tab_stubhead(label = "Name") |> 
  gt_img_rows(headshot_url, height = 25) |> 
  gt_color_rows(Tackles_Over_Expected, palette = "ggsci::blue_material") |> 
  cols_label(
    displayName.x = "Name",
    team = "Team",
    position = "Position",
    Tackles_Over_Expected = "Tackles Over Expected",
    headshot_url = "") |> 
  cols_align(align = "center")
Linebacker_Player_Table


# Save --------------------------------------------------------------------

#Save RData file for easy load later
save.image(file = "Big_Data_Bowl.RData")

# Play around with stuff ---------------------------------------------

Test <- Primary_Step3 |> 
  filter(gameId == "2022091102", playId == "1107")


LB_Test <- Tackle_Calcs |> 
  filter(position %in% linebackers) |> 
  distinct()


Test_Colors <- load_teams()
