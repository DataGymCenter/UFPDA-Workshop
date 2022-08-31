######
library(readxl) 
library(rio) ## install_formats()
library(dplyr)

#####
setwd("C:/RTuts/UFPDA_WORKSHOP_2022/UFPDA_R_Workshop/Data")

### IMPORT MULTIPLE EXCEL SHEETS
all_oly <- import_list("olympics_multiple_sheets.xlsx", which = c(1:5), rbind = TRUE) # Numbers refer to sheet position ### , which = c(1,2,3,4,5)

dim(all_oly)
#View(all_oly)

### Sheet name reference
all_oly2 <- import_list("olympics_multiple_sheets.xlsx", which = c("Sheet1","Sheet2","Sheet3","Sheet4","Sheet5"), rbind = TRUE)

### IMPORT MULTIPLE EXCEL FILES (from readxl package)
paths = "C:/RTuts/UFPDA_WORKSHOP_2022/UFPDA_R_Workshop/Data"

##
files_XL <- list.files(path = paths, pattern = "*.xlsx", full.names = T); files_XL

(ALL_EXCEL_FILES <- sapply(files_XL, read_excel, simplify=FALSE) %>% bind_rows())


###
library(ggplot2)
ggplot(ALL_EXCEL_FILES) +
  aes(x = Medal, weight = Height) +
  geom_bar(fill = "#440154") +
  coord_flip() +
  ggthemes::theme_economist()
### IMPORT MULTIPLE CSV FILES (from readr package)
library(readr)
files_CSV <- list.files(path = paths, pattern = "*.csv", full.names = T)

(ALL_CSV_FILES <- sapply(files_CSV, read.csv, simplify=FALSE) %>% bind_rows())

#### VROOM # install.packages("vroom")
library(vroom)
files <- fs::dir_ls(paths, glob = "*csv"); files

## VROOMING it
ALL_vroomed <- vroom(files)

##
ALL_vroomed <- vroom(files, col_select = c(Sex, Age, Team, Medal, Year, Season, City,  Sport)) #%>%
#select(Sex, Age)

#### Try it 10GB data - Full directory
## Rows: 5111688 Columns: 27
system.time(Ten_GB_Twits <- vroom("C:/RTuts/UFPDA_WORKSHOP_2022/UFPDA_R_Workshop/Data/Others/all_covid_tweets_cleaned.csv"))

#### Try it yourself

