

#################################
### Roster Parameters Go Here ###
#################################

NUMBER_OF_PITCHERS <- 10
NUMBER_OF_HITTERS <- 15

NUMBER_OF_TEAMS <-  18

START_OF_AUCTION <- lubridate::mdy("1/1/2020")


HITTER_SPOTS <- c("C","1B","2B","SS","3B","CI","MI","OF","DH")

POSITION_ELIGIBILITY <- readxl::read_xlsx("position_eligibility.xlsx")


CURRENT_YEAR <- 2022

PROJECTIONS <- c("steamer", "depthcharts", "fans", "zips", "atc", "thebat", "thebatx")


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

POSITIONS <- c("C1","C2","1B","2B","SS","3B","CI","MI","OF1","OF2","OF3","OF4","OF5","OF6","DH",
               "P1","P2","P3","P4","P5","P6","P7","P8","P9","P10",
               "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10")

BENCH_SPOTS <- c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10")

#DUPLICATE_PLAYERS_TO_EXCLUDE <- c("sa917669")
