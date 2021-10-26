#' Create a line chart depicting finishing variance of a player over a period of time using data from Understat.
#' 
#' @param data is for the dataset used.
#' @param player_id is for the data extraction from Understat using the player's id.
#' @param roll_avg is for setting the rolling average according to the data.
#' @param theme to select the colours. Choose from 2 themes -> dark and white.
#' 
#' @example_plot <- plot_varline(player_id = 5555, roll_avg = 50, theme = "dark")

library(dplyr)
library(understatr)
library(RcppRoll)
library(ggplot2)
library(ggtext)

plot_varline <- function(player_id, roll_avg, theme = "") {
  
  fill_b = ""
  colour_b = ""
  colorLine = ""
  colorText = ""
  gridc = ""
  
  if(theme == "dark" || theme == ""){
    fill_b = "black"
    colour_b = "black"
    
    
    colorLine = "white"
    gridc = "grey"
    colorText = "white"
  }
  else if(theme == "white"){
    fill_b = "#F5F5F5"
    colour_b = "black"
    
    colorLine = "black"
    gridc = "grey"
    colorText = "black"
  }
  

  
data <- get_player_shots(player_id)
data <- data %>%
  filter(!situation == "Penalty")

data$result[data$result == "Goal"] <- 1
data$result[data$result == "MissedShots"] <- 0
data$result[data$result == "SavedShot"] <- 0
data$result[data$result == "BlockedShot"] <- 0
data$result[data$result == "ShotOnPost"] <- 0
data$result[data$result == "OwnGoal"] <- 0

data <- data %>%
  mutate(goals = as.numeric(result),
         GxG = goals - xG,
         GxGSM = TTR::SMA(GxG, n = roll_avg),
         no = 1:nrow(data))
  
player_name <- data$player
player_name <- paste(player_name, "Finishing Variance")
subtitle <- paste(roll_avg, "Shot Rolling Average (Non-Penalty Shots only)")

ggplot(data, aes(x = no, y = GxGSM, colour = GxGSM)) +
  geom_line(size = 3) + geom_point(size = 4) + 
  scale_colour_gradient2(low = "red", mid = "yellow" , high = "seagreen") +
  theme(legend.position="none") +
  labs(title=player_name,
       subtitle=subtitle,
       caption = "Data from Understat
       Inspired by @Jair1970. Created by Harsh Krishna") +
  theme(plot.title = element_text(size = 40, colour = colorText, face = "bold"),
        plot.subtitle = element_text(size = 30, colour = colorText),
        plot.caption = element_text(size = 18, colour = colorText)) +
  theme(plot.background = element_rect(fill = fill_b, colour = colour_b)) +
  theme(panel.background = element_rect(fill = fill_b, colour = colour_b)) +
  labs(x = "Shots Taken", y = "NPGoals - NPxG") +
  theme(axis.title.x = element_text(colour = colorText, size = 18, face = "bold")) +
  theme(axis.title.y = element_text(colour = colorText, size = 18, face = "bold")) +
  theme(axis.text.x = element_text(colour = colorText, size = 15),
        axis.text.y = element_text(colour = colorText, size = 15)) +
  theme(panel.grid.major = element_line(size = 0.8, colour = gridc, linetype = "dashed"),
        panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) + 
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.line = element_line(size = 0.8, colour = colorLine)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 2, colour = colorLine) +
  xlim(roll_avg, nrow(data))
}
