# nraaHonourBoard Global
library(shiny)
library(DT)
library(dplyr)
library(readr)
library(plotly)

# Import csv files
df.hist <- read_csv("data/nraaHistorical.csv")
df.comm <- read_csv("data/commGames.csv")
df.nonQ <- read_csv("data/nonQueens.csv")
df.wlrc <- read_csv("data/wlrc.csv")

# Sort historical data for ui select input
df.hist %>%
  select(everything()) %>% 
  filter(Winner != 'NA') %>%
  group_by(Winner) %>%
  summarise(`Kings/Queens Prize Count` = n()) %>%
  arrange(desc(`Kings/Queens Prize Count`)) ->
df.sort





  
  


  
