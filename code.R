getwd()
setwd("C:/Users/lsyku/OneDrive/Documents/Player Analysis/Spencer Swellenbach Analysis")

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)

#### Load Data

swell <- read.csv("Schwellenbach.csv")

## Stikezone all pitches

swell_pitches <- swell



swell_pitches$pitch_name[swell_pitches$pitch_name == "4-Seam Fastball"] <- "Fastball"
swell_pitches$pitch_name[swell_pitches$pitch_name == "Sinker"] <- "Fastball"
swell_pitches$pitch_name[swell_pitches$pitch_name == "Cutter"] <- "Fastball"

swell_pitches$pitch_name[swell_pitches$pitch_name == "Curveball"] <- "Breakingball"
swell_pitches$pitch_name[swell_pitches$pitch_name == "Slider"] <- "Breakingball"

swell_pitches$pitch_name[swell_pitches$pitch_name == "Split-Finger"] <- "Offspeed"

swell_pitches$pitch_name <- factor(swell_pitches$pitch_name, levels = c("Fastball", "Offspeed", "Breakingball"))


# Add in Parameters for Strike Zone / Plate
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3

TPL <- ggplot(swell_pitches, mapping = aes(x= plate_x, y= plate_z)) +
  geom_point(aes(color = pitch_name),size = 1.5) +
  

  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("Spencer Schwellenbach Pitches")) +
  labs(color = "Pitch Type") +
  theme(
    legend.position = "right",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA))
  



### Release Position

TPR <- ggplot(swell_pitches, mapping = aes(x = release_pos_x, y = release_pos_z)) +
  geom_point(aes(color = pitch_name), size = 1) +
  xlim(-3.5, 3.5) + ylim(-0.5, 6) + ggtitle(paste("Release Point")) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(color = "Pitch Type") +
  theme(
    legend.position = "right",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA))
  

combined_plot <- ggarrange(TPL,TPR)

annotate_figure(combined_plot,
                bottom = text_grob("**Fastball: 4-Seam Fastball, Cutter, Sinker. Offspeed: Split-Finger. Breakingball: Curveball, Slider**",
                                   face = "italic", size = 10, hjust = 0.5, color = "gray40"))


### Heatmaps

  ###color use
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(10)


  ### Righthand batter data
swell_rb <- swell_pitches %>%
  filter(swell_pitches$stand == "R")

freqr <- swell_rb%>%group_by(pitch_name)%>%tally()
freqr_total <- as.numeric(sum(freqr$n))
freqr$'Usage %' <- round((freqr$n/freqr_total)*100, digits = 0)
freqr <- subset(freqr, select = -c(n))

swell_rb <- merge(swell_rb, freqr)

  ### Lefthand batter data

swell_lb <- swell_pitches %>%
  filter(swell_pitches$stand == "L")


freql <- swell_lb%>%group_by(pitch_name)%>%tally()
freql_total <- as.numeric(sum(freql$n))
freql$'Usage %' <- round((freql$n/freql_total)*100, digits = 0)
freql <- subset(freql, select = -c(n))



### RH batter viz

BTL <- ggplot(swell_rb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH Batter and Usage")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)) +
  facet_wrap(~pitch_name, labeller = labeller(pitch_name =
    c("Fastball" = "Fastball 57%",
      "Breakingball" = "Breakingball 40%",
      "Offspeed" = "Offspeed 2%")))



 ###LH Batter Viz

BTR <- ggplot(swell_lb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH Batter and Usage")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)) +
  facet_wrap(~pitch_name, labeller = labeller(pitch_name =
                                                c("Fastball" = "Fastball 48%",
                                                  "Breakingball" = "Breakingball 23%",
                                                  "Offspeed" = "Offspeed 29%")))

all <- ggarrange(TPL,TPR, BTR, BTL)

annotate_figure(all,
                bottom = text_grob("**Fastball: 4-Seam Fastball, Cutter, Sinker. Offspeed: Split-Finger. Breakingball: Curveball, Slider**",
                                   face = "italic", size = 10, hjust = 0.5, color = "gray40"))


cutoff_date <- as.Date("2024-06-25")

ggplot(swell_pitches, mapping = aes(x = release_pos_x, y = release_pos_z)) +
  geom_point(aes(color = pitch_name), size = 1) +
  xlim(-3.5, 3.5) + ylim(-0.5, 6) + ggtitle(paste("Release Point")) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA)) +
  labs(color = "Pitch Type")

cutoff_date <- as.Date("2024-06-25")

ggplot(swell_pitches, mapping = aes(x = release_pos_x, y = release_pos_z)) +
  geom_point(aes(color = factor(ifelse(game_date < cutoff_date, "Before", "After"),
                          levels = c("Before", "After"))), size = 1) + 
  xlim(-3.5, 3.5) + ylim(-0.5, 6) + 
  ggtitle("Release Point") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  labs(color = "Game Date",
       caption = "**Data includes release points Before and After June 25, 2024**") +  
  theme(
    legend.position = "right",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    plot.caption = element_text(face = "italic", size = 10, hjust = 0.5, color = "gray40")
  ) +
  scale_color_manual(values = c("Before" = "blue", "After" = "red")) # Custom colors




  