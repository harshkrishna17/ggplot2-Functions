library(worldfootballR)  
library(tidyverse)       
library(forcats)         
library(ggtext)
library(stringr)

data <- fb_player_scouting_report("https://fbref.com/en/players/2146785a/Davide-Calabria", pos_versus = "primary")

plot_pizza(data = data, template = "full back", 
           colour_poss = "#41ab5d", colour_att = "#2171b5", colour_def = "#fec44f", 
           theme = "dark")
ggsave("calab.png", bg = "#0d1117", height = 2800, width = 2500, units = "px")

plot_pizza <- function(data, template = "", colour_poss, colour_att, colour_def, theme = "") {
  
  colorText = ""
  gridline = ""
  fill_b = ""
  colour_b = ""
  
  if(theme == "dark" || theme == "") {
    fill_b = "#0d1117"
    colour_b = "#0d1117"
    
    colorText = "white"
    gridline = "565656"
  }
  
  else if(theme == "black") {
    fill_b = "black"
    colour_b = "black"
    
    colorText = "white"
    gridline = "565656"
  }
  
  else if(theme == "white") {
    fill_b = "white"
    colour_b = "white"
    
    colorText = "black"
    gridline = "565656"
  }
   
  if(template == "forward" || template == "") {
    
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,8,13,24,42,128,45,115,133,107,101,102,26,149),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                              Statistic == "xG"|
                              Statistic == "Shots Total"|
                              Statistic == "Non-Penalty Goals - npxG"|
                              Statistic == "npxG/Shot" ~ "Attacking",
                            Statistic == "xA"|
                              Statistic == "Miscontrols"|
                              Statistic == "Passes into Penalty Area"|
                              Statistic == "Touches (Att Pen)"|
                              Statistic == "Progressive Passes Rec" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[4] <- "Attacking"
  }

  else if(template == "midfielder") {
    
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,9,10,13,53,44,47,116,125,133,146,149,107,96),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "npxG + xA"|
                              Statistic == "Average Shot Distance"|
                              Statistic == "Shots Total"|
                              Statistic == "Non-Penalty Goals" ~ "Attacking",
                            Statistic == "Progressive Passes"|
                              Statistic == "Passes Under Pressure"|
                              Statistic == "Passes into Final Third"|
                              Statistic == "Touches (Live-Ball)"|
                              Statistic == "Progressive Carries"|
                              Statistic == "Progressive Passes Rec" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[2] <- "Attacking"
    data_selected$stat[3] <- "Attacking"
  }
  
  else if(template == "defender") {
    
    if(nrow(data) > 148) {
    
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,11,13,44,47,129,125,110,88,96,102,106,149,108),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                              Statistic == "npxG + xA"|
                              Statistic == "Shots Total" ~ "Attacking",
                            Statistic == "Passes into Final Third"|
                              Statistic == "Progressive Passes"|
                              Statistic == "Progressive Carries"|
                              Statistic == "Touches"|
                              Statistic == "Dispossessed" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[2] <- "Attacking"
  } else {
   
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,11,13,43,46,128,124,109,87,95,101,105,148,107),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                              Statistic == "npxG + xA"|
                              Statistic == "Shots Total" ~ "Attacking",
                            Statistic == "Passes into Final Third"|
                              Statistic == "Progressive Passes"|
                              Statistic == "Progressive Carries"|
                              Statistic == "Touches"|
                              Statistic == "Dispossessed" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[2] <- "Attacking"
  }
 }
    
  else if(template == "full back") {
    
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,9,10,13,114,46,47,125,43,44,147,96,107,102),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                              Statistic == "npxG"|
                              Statistic == "xA"|
                              Statistic == "Shots Total" ~ "Attacking",
                            Statistic == "Passes into Final Third"|
                              Statistic == "Progressive Passes"|
                              Statistic == "Progressive Carries"|
                              Statistic == "Touches (Att 3rd)"|
                              Statistic == "Crosses into Penalty Area"|
                              Statistic == "Key Passes" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[3] <- "Attacking"
  }

  else if(template == "winger") {
    
    data$no <- 1:nrow(data)
    data_selected <- data[c(3,22,24,42,143,45,119,47,124,133,107,146,101,102),]
    data_selected <- data_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                              Statistic == "xG"|
                              Statistic == "xA"|
                              Statistic == "Penalty Kicks Won"|
                              Statistic == "npxG/Shot" ~ "Attacking",
                            Statistic == "Progressive Carrying Distance"|
                              Statistic == "Successful Dribble %"|
                              Statistic == "Progressive Passes"|
                              Statistic == "Passes into Penalty Area"|
                              Statistic == "Progressive Passes Rec" ~ "Possession",
                            TRUE ~ "Defending"))
    
    data_selected$stat[3] <- "Attacking"
  }

player_name <- data$Player  
title <- paste(player_name, "Percentile Chart")
sub <- data$Versus
subtitle <- paste("Compared to", sub, "|", "Last 365 days")

temp <- (360/(length(data_selected$Player))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = length(data_selected$Player))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)

x <- c(data_selected$Statistic, data_selected$stat)

ggplot(data_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       
  geom_bar(aes(y=100),fill= fill_b,stat="identity",width=1,colour= gridline,                 
           alpha=0.5,show.legend = FALSE) +      
  geom_bar(stat="identity",width=1,aes(fill=stat),colour= fill_b,alpha=1) +                     
  coord_polar(clip = "off") +                                                                      
  geom_hline(yintercept=25, colour=gridline,linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=50, colour=gridline,linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=75, colour=gridline,linetype="longdash",alpha=0.5)+ 
  scale_fill_manual(values=c("Possession" = colour_poss,                                   
                             "Attacking" = colour_att,
                             "Defending" = colour_def)) +                                                        
  geom_label(aes(label=Per90,fill=stat),size=3,color=fill_b,show.legend = FALSE)+ 
  scale_y_continuous(limits = c(-20,100))+                                              
  labs(fill="",   
       caption = "Data from FBref via StatsBomb
       Plot code by @RobinWilhelmus, Inspired by @Worville
       Created by Harsh Krishna",
       title=title,
       subtitle = subtitle)+
  theme_minimal() +
  theme(plot.background = element_rect(fill = fill_b,color = colour_b),
        panel.background = element_rect(fill = fill_b,color = colour_b),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12,colour = colorText, angle = ang),
        text = element_text(family="Spartan-Light",colour= colorText, size = 20),                                   
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium", size = 26, colour = colorText, face = "bold"),
        plot.subtitle = element_text(hjust=0.5,size=20, colour = colorText),
        plot.caption = element_text(hjust=1,size=14, colour = colorText),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
}

  
