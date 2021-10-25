#' Create a xG Race Chart using data from Understat through a plotting function.
#' 
#' @param data is for the dataset used.
#' @param team_home is for the home team according to data.
#' @param team_away is for the away team according to data.
#' @param home_color is for the colour of the line for the home team.
#' @param away_color is for the colour of the line for the away team.
#' @param theme to select the colours. Choose from 4 themes -> dark, almond, rose, white.
#' 
#' @example_plot <- plot_xgrace(data = data, team_home = "Manchester United", team_away = "Liverpool",
#'                              home_color = "#e31a1c", away_color = "#980043", theme = "dark")

library(worldfootballR)
library(dplyr)
library(ggtext)
library(berryFunctions)
library(ggplot2)
library(ggrepel)

data <- understat_league_season_shots(league = "EPL", season_start_year = 2021)

plot_xgrace <- function(data, team_home, team_away, home_color, away_color, theme = "") {

fill_b = ""
colour_b = ""
colorLine = ""
colorText = ""
gridc = ""
  
if(theme == "dark" || theme == ""){
    fill_b = "#0d1117"
    colour_b = "white"
    
    
    colorLine = "white"
    gridc = "#525252"
    colorText = "white"
  }
else if(theme == "white"){
    fill_b = "#F5F5F5"
    colour_b = "black"
    
    colorLine = "black"
    gridc = "grey"
    colorText = "black"
  }
else if(theme == "rose"){
    fill_b = "#FFE4E1"
    colour_b = "#696969"
    
    colorLine = "#322E2E"
    gridc = "grey"
    colorText = "#322E2E"
  }
else if(theme == "almond"){
    fill_b = "#FFEBCD"
    colour_b = "#696969"
    
    colorLine = "#322E2E"
    gridc = "grey"
    colorText = "#322E2E"
}

data$minute <- as.numeric(data$minute)

if ("home_away" %in% colnames(data)) {
} else {
  data <- data %>%
    mutate(home_away = h_a)
}
  
data <- data %>%
 filter(home_team == team_home,
         away_team == team_away)
data1 <- data %>%
  filter(home_away == "h") %>%
  mutate(xGsum = cumsum(xG))
data2 <- data %>%
  filter(home_away == "a") %>%
  mutate(xGsum = cumsum(xG))

data1 <- insertRows(data1, 1, new = 0)
data2 <- insertRows(data2, 1, new = 0)

dat1 <- data1 %>%
  filter(result == "Goal")
d1 <- data1 %>%
  filter(result == "OwnGoal")
dat1 <- rbind(dat1, d1)
dat2 <- data2 %>%
  filter(result == "Goal")
d2 <- data2 %>%
  filter(result == "OwnGoal")
dat2 <- rbind(dat2, d2)

team1 <- data$home_team
team2 <- data$away_team

xG1 <- sum(data1$xG)
xG2 <- sum(data2$xG)

g1 <- data$home_goals
g2 <- data$away_goals

team1 <- paste(team1, "=", round(xG1, digits = 2), "xG", ",", "Goals = ", g1)
team2 <- paste(team2, "=", round(xG2, digits = 2), "xG", ",", "Goals = ", g2)

min1 <- dat1$minute
min2 <- dat2$minute
p1 <- dat1$player
p2 <- dat2$player

player_lab1 <- paste(p1, min1)
player_lab2 <- paste(p2, min2)

ggplot() +
  geom_step(data = data1, aes(x = minute, y = xGsum), colour = home_color, size = 3) +
  geom_step(data = data2, aes(x = minute, y = xGsum), colour = away_color, size = 3) +
  geom_point(data = dat1, aes(x = minute, y = xGsum), colour = home_color, size = 6) +
  geom_point(data = dat2, aes(x = minute, y = xGsum), colour = away_color, size = 6) +
  geom_label_repel(data = dat1, aes(x = minute, y = xGsum, label = player_lab1),
                   box.padding   = 0.35, 
                   point.padding = 1.5,
                   segment.color = colorLine, 
                   alpha = 0.8) +
  geom_label_repel(data = dat2, aes(x = minute, y = xGsum, label = player_lab2),
                   box.padding   = 1.5, 
                   point.padding = 1.5,
                   segment.color = colorLine, 
                   alpha = 0.8) +
  theme_minimal() +
  labs(title= team1, 
       subtitle = team2,
       caption = "Data from Understat
       Created by Harsh Krishna") + 
  theme(plot.title = element_markdown(lineheight = 1.1, colour = home_color, size = 30, face = "bold"),
        plot.subtitle = element_markdown(lineheight = 1.1, colour = away_color, size = 20, face = "bold"),
        plot.caption = element_text(colour = colorText, size = 12)) +
  theme(plot.background = element_rect(fill = fill_b, colour = colour_b)) +
  theme(panel.background = element_rect(fill = fill_b, colour = colour_b)) +
  labs(x = "Minute", y = "Cumulative xG") +
  theme(axis.title.x = element_text(colour = colorLine, size = 15, face = "bold")) +
  theme(axis.title.y = element_text(colour = colorLine, size = 15, face = "bold")) +
  theme(axis.text.x = element_text(colour = colorLine, size = 12),
        axis.text.y = element_text(colour = colorLine, size = 12)) +
  theme(panel.grid.major.y = element_line(colour = gridc, size = 0.5, linetype = "dashed"),
        panel.grid.major = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.line = element_line(size = 0.8, colour = colorLine)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
  geom_vline(xintercept = 45, linetype = "dashed", colour = colorLine, size = 1)
}
