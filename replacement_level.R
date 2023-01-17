# #vector of hitter replacement values
# replacement_hitter <- read_xlsx("./replacement/replacement_hitters.xlsx", sheet = "positionless_replacement") %>% 
#   filter(Player == "Adjusted Average") %>% 
#   select(R, HR, RBI, SB, AVG) 
# 
# replacement_hitter$AB <- c(400)
# 
# #create replacement pitcher values
# #these are the mean projections for the 170th through 190th best players
# replacement_pitcher <- readxl::read_xlsx("./replacement/replacement_pitchers.xlsx") %>% 
#   filter(Name == "Adjusted Average") %>% 
#   select(W:WHIP) 


replacement_pitcher <- tibble(
  "ERA" = 4.75,
  "WHIP" = 1.368,
  "SV" = .42,
  "W" = 4.8,
  "K" = 94.7
)

replacement_hitter <- tibble(
  "R" = 51,
  "HR" = 13.25,
  "RBI" = 51,
  "AVG" = .2365,
  "SB" = 3.3
)

