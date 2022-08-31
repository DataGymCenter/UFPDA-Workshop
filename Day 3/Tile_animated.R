##############################
library(readtext)
library(LexisNexisTools)
library(tidyverse)
library(tools)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(darklyplot)
library(ggfx)
library(gganimate)
library(gifski)
library(directlabels)
library(vroom)
###################################
## VROOMING it
olympics <- vroom("C:/RTuts/UFPDA_WORKSHOP_2022/UFPDA_R_Workshop/Day 3/olympics.csv")

#####
olympics_mod <- olympics %>%
  filter(!Year < 1960 & !Year > 2016 & !Medal == "NA" & !Year %in% c(1993,1994,1997,1998,2001,2002,2005,2006,2009,2010,2013,2014)) %>%
  mutate(Period = case_when(Year %in% c(1960:1969) ~ '1960-1969',
                            Year %in% c(1970:1979) ~ '1970-1979',
                            Year %in% c(1980:1989) ~ '1980-1989',
                            Year %in% c(1990:1999) ~ '1990-1999',
                            Year %in% c(2000:2009) ~ '2000-2009',
                            Year %in% c(2010:2016) ~ '2010-2016')) %>%
  dplyr::select(Year, Sex, NOC, Sport, Medal, Period) %>%
  group_by(Year, Sex, NOC, Sport, Medal, Period) %>%
  count() %>%
  summarise(Count = sum(n)) %>%
  ungroup()

### Create custom color palette
library(RColorBrewer)
nb.cols <- 48 #62
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

###### Plot the TILE and ANIMATE

olym_sport <- olympics_mod %>% select(Year, Sport) %>% filter(!is.na(Year))

library(ggthemes)
gg1 <- ggplot(olym_sport) +
  aes(x = factor(Year), y = Sport, fill=factor(Sport)) +
  geom_tile(size = 1.0, na.rm = FALSE) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values=mycolors) + 
  theme_base() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=18, angle = 90, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size=16, face = "italic", vjust = 0.38),
    axis.title.y = element_blank(),
    panel.spacing = unit(2, "lines"),
    panel.grid.major = element_line(colour = "black", size=0.05),
    panel.background = element_rect(fill = "#232732"),
    legend.position = "none",
    axis.line.x = element_line(colour = "darkorange", size=1.5, lineend = "butt"),
    axis.line.y = element_line(colour = "darkorange", size=1.5, lineend = "butt") 
  ); gg1 

###
anims_tile <- gg1 + transition_reveal(Year) + ease_aes('linear'); anims_tile

#####
animate(anims_tile, duration = 15, end_pause = 30, fps = 40, width = 2000, height = 1200, renderer = gifski_renderer())

#### SAVE GIF - Reversed
anim_save(file.path("C:/RTuts/UFPDA_WORKSHOP_2022/UFPDA_R_Workshop/Day 3/", "Opympics_sports.gif"))


