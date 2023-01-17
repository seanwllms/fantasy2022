library(shiny)
library(tidyverse)
library(ggrepel)

#load league stats
load("league_stats.rda")

ALL_POSITIONS <- c("Catcher", "1B", "2B", "SS", "3B", "CI", "MI", "OF", "DH")

#list of teams
TEAMS <- c("marmaduke",
           "pkdodgers",
           "ottawa",
           "isotopes",
           "perpetual",
           "stircrazy",
           "dumpsterfire",
           "deano",
           "hebrewhammer",
           "bellevegas",
           "bookhouse",
           "balco",
           "sturgeon",
           "evilempire",
           "pasadena",
           "deener",
           "shohei",
           "bears")

todays_auction <- c("Rafael Ortega",
                    "Trevor Bauer",
                    "Devin Williams",
                    "James Kaprielian",
                    "Adbert Alzolay", 
                    "Jeff McNeil", 
                    "Lucas Sims",
                    "Paul Sewald"
                    
)


#read in full player pool
player_pool <- bind_rows(
  #hitter projections
  read_csv("hitter_projections.csv"),
  read_csv("pitcher_projections.csv")
)

nfbc_simplified <- player_pool %>% 
  select(playerid, NFBC_ADP) %>% 
  mutate(NFBC= as.character(round(NFBC_ADP, 1))) %>% 
  filter(!is.na(NFBC)) %>% 
  select(-NFBC_ADP) %>% 
  distinct()

player_pool <- player_pool %>% 
  arrange(-dollar_value) %>% 
  select(-NFBC_ADP) %>% 
  mutate(across(PA:SB, ~as.character(round(.x, 0))),
         across(c(AVG, ERA, WHIP), ~as.character(round(.x, 3))),
         across(c(IP,SV:K), ~as.character(round(.x, 0))),
         across(marginal_total_points:auction_value, ~as.character(round(.x, 1))),
         status = ifelse(is.na(status), "", as.character(salary))) %>% 
  rename(Points = marginal_total_points, Dollars = dollar_value, Auction = auction_value) %>% 
  left_join(nfbc_simplified) %>% 
  distinct()
  

#read in standings
standings <- read_csv("standings.csv") %>% 
  select(-1) %>% 
  mutate(across(spent:picks_left, ~as.character(round(.x,0))),
         across(total_points:W_points, ~as.character(round(.x,1)))) %>% 
  rename_with(~str_remove(.x,"_points"), contains("_points")) %>% 
  rename(Points = total)


#read in rosters
rosters <- read_csv("rosters.csv") %>% 
  mutate(across(AB:SB, ~as.character(round(.x, 0))),
         across(c(AVG, ERA, WHIP), ~as.character(round(.x, 3))),
         across(c(salary,IP,SV:K), ~as.character(round(.x, 0))),
         across(marginal_total_points:dollar_value, ~as.character(round(.x, 1))),
         across(AB:dollar_value, ~ifelse(is.na(.x), "", .x))) 

#read in target list
targets <- read_csv("sean_targets.csv") %>% 
  filter(model_adp > 0) %>% 
  mutate(across(dollar_value:model_adp, ~round(.x, 1)))

#standigns for plot
plotstandings <- standings %>% 
  mutate(Points = as.numeric(Points),
         spent = as.numeric(spent),
         left = as.numeric(left),
         picks_left = as.numeric(picks_left)) %>% 
  select(team, spent, left, picks_left, Points)

#graph theme f
fantasy_theme <- theme_void() +
  theme(axis.title.y = element_text(size = rel(1), angle = 90, margin=margin(0,0,100,0)),
        axis.title.x = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)))
  

#function to filter data frame by position.
filter_position <- function(df, position) {
  
  position_lower <- tolower(position)
  
  if (position == "Pitchers") {
    filter(df, position == "pitcher")
  } else if (position %in% c("DH", "Hitters")) {
    filter(df, position != "pitcher")
  } else if (position == "MI") {
    filter(df, str_detect(position, "2b") | str_detect(position, "ss"))
  } else if (position == "CI") {
    filter(df, str_detect(position, "1b") | str_detect(position, "3b"))
  } else {
    filter(df, str_detect(position, position_lower))
  }
  
}