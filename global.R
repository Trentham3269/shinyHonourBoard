library(dplyr)        # data wrangling
library(DT)           # datatables
library(plotly)       # js visualisation
library(readr)        # fast I/O
library(shiny)        # web framework
library(shinythemes)  # free bootstrap themes from bootswatch.com

# Import csv file
df <- read_csv("data/df.csv")

# Subset full data frame by honour board type
df %>% filter(Honour_Board == "Kings/Queens")                   -> df_kings_queens
df %>% filter(Honour_Board == "Commonwealth Games")             -> df_comm_games
df %>% filter(Honour_Board == "Overseas Championships")         -> df_overseas_champs
df %>% filter(Honour_Board == "World Long Range Championships") -> df_world_champs