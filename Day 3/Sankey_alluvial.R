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
  #dplyr::select(Year, Sex, NOC, Medal, Period) %>%
  group_by(Medal, Sex, City, Period) %>%
  count() %>%
  summarise(Count = sum(n)) %>%
  ungroup()

########## Convert to factors
olympics_mod$City <- factor(olympics_mod$City)
olympics_mod$Sex <- factor(olympics_mod$Sex)
#olympics_mod$NOC <- factor(olympics_mod$NOC)
olympics_mod$Medal <- factor(olympics_mod$Medal)
olympics_mod$Period <- factor(olympics_mod$Period)  

#################################
#######        1        #########
library(flipPlots)
#
SankeyDiagram(olympics_mod[, -5],
              link.color = "Target", # Source Target First variable, First variable
              weights = olympics_mod$Count, 
              font.size = 20, font.unit = "pt",
              # variables.share.values = F, 
              label.show.counts = F,
              label.show.percentages = F, 
              #hovertext.show.percentages = F, 
              max.categories = 66, 
              label.show.varname = F, node.padding = 10) 

################
olympics_mod <- olympics %>%
  filter(!Year < 1960 & !Year > 2016 & !Medal == "NA" & !Year %in% c(1993,1994,1997,1998,2001,2002,2005,2006,2009,2010,2013,2014)) %>%
  mutate(Period = case_when(Year %in% c(1960:1969) ~ '1960-1969',
                            Year %in% c(1970:1979) ~ '1970-1979',
                            Year %in% c(1980:1989) ~ '1980-1989',
                            Year %in% c(1990:1999) ~ '1990-1999',
                            Year %in% c(2000:2009) ~ '2000-2009',
                            Year %in% c(2010:2016) ~ '2010-2016')) %>%
  #dplyr::select(Year, Sex, NOC, Medal, Period) %>%
  group_by(Sex, City, Medal, Period) %>%
  count() %>%
  summarise(Count = sum(n)) #%>%

#################################
#######        2        #########
library(sfo)

sankey_ly(x = olympics_mod, 
          cat_cols = c("Sex", "City", "Medal", "Period"), 
          num_col = "Count", 
          title = " ")

#################################
#######        3        #########
library(ggforce)  ### Transforms the data for use by geom_parallel_sets
##
alluv <- gather_set_data(olympics_mod, 1:4); alluv

### Create custom color palette
mycolors <- c("brown", "gold", "darkgrey")

#### Graphing 
alluv_chart <- ggplot(alluv, aes(fct_inorder(x), id = id, split = y, value=Count)) +
  geom_parallel_sets(aes(fill = Medal), alpha = 4, axis.width = 0.3, n=50, strength = 0.8) +# 'n' similar to pixel
  geom_parallel_sets_axes(axis.width = 0.3, fill="grey41") +
  geom_parallel_sets_labels(colour = "white", angle = 360, size = 4.6) +
  scale_fill_manual(values = mycolors) +
  #theme_economist_white() +
  theme_base() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#232732"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20, face = "bold", vjust = 0.5, hjust = 0.5),
    axis.title.x  = element_blank(),
    plot.title = element_text(size = 28, vjust = -25, hjust = 0.6, color = "brown",face = "bold"),
    plot.subtitle =  element_text(size = 21, color = "navyblue", vjust =-30, hjust = 0.6,face = "bold", lineheight = 0.9),
    plot.caption =  element_text(size = 14, vjust = 0.9, hjust = 0.84, face = "bold.italic", color = "brown",lineheight =0.5),
    plot.margin = margin(-0.6, # Top
                         -6.2, # Right
                         0.4, # Bottom
                         -6.2, # Left
                         "cm")
  ); alluv_chart
