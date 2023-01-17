
library(tidyverse)

###Load the coefficients data frame
load("coefs.rda")

PITCHER_AUCTION_PERCENT = .35
HITTER_AUCTION_PERCENT = .65

###############################################################
################HITTER STUFF LIVES HERE#########################
################################################################

#make lists of file names
getfiles <- function(proj) {

  #build list of paths to files
  folder <- paste0("./", proj, "/") 
  
  
  filelist <- list.files(folder) %>% 
    discard(~(.x == "pitchers.csv" | .x == "pitcher.csv"))

  if (length(filelist) > 0) {
    filelist <- map_chr(filelist, function(x) paste0(folder, x))
  } 
  
  #for each path, read csv file and clean it up
  dfs <- map(filelist, function(x) {
    
    pos_name <- str_remove(x, folder) %>% str_remove(".csv")
    
    read_csv(x) %>%
      mutate(proj=proj) %>% 
      select(Name, Team, AB, PA, R,HR, RBI, SB, AVG, OBP, proj, playerid) %>% 
      mutate( 
        HR_pa = HR/PA,
        R_pa = R/PA,
        RBI_pa = RBI/PA,
        SB_pa = SB/PA,
        playerid = as.character(playerid),
        position = pos_name)
  })
  
  keep(dfs, ~!is.null(.x))
}

#build nested list with all of the data frames.
hitter_data_frames <- map(PROJECTIONS, getfiles) %>% 
  keep(~length(.x) > 1) 


#build list of all projection systems successfully read
proj_systems <- map_chr(hitter_data_frames, function(x){
                  pluck(x, 1) %>% 
                    pull(proj) %>% 
                    unique() 
                }) 
                
#assign names in df list
names(hitter_data_frames) <- proj_systems

####################################
############   PECOTA   ############
####################################
#read in PECOTA data and rname variables to line up
library(readxl)
library(readxl)
if (file.exists("./pecota/pecota2022_hitting_feb15.xlsx")) {
  pecotahit<- read_xlsx("./pecota/pecota2022_hitting_feb15.xlsx", sheet = "50") %>% 
    select(name, bpid, team, pa, ab, dc_fl, r, hr, rbi, sb, avg, obp)
  
  #crosswalk PECOTA to BP
  if (file.exists("./pecota/crosswalk.rda")) {
    load("./pecota/crosswalk.rda")
  } else {
    crosswalk <- read_csv(url("https://www.smartfantasybaseball.com/PLAYERIDMAPCSV")) %>%
      rename(Name = FANGRAPHSNAME, playerid=IDFANGRAPHS, bpid = BPID) %>% 
      select(playerid, bpid) %>% 
      mutate(bpid = as.numeric(bpid))
    save(crosswalk, file="./pecota/crosswalk.rda")
  }
  
  pecotahit <- left_join(pecotahit, crosswalk) %>% 
    select(-bpid) %>% 
    rename(AB=ab,PA=pa, R=r, HR=hr, RBI=rbi, SB=sb, AVG = avg, OBP = obp) %>% 
    mutate(OBP = as.numeric(OBP),
           AVG = as.numeric(AVG),
           HR_pa = HR/PA,
           R_pa = R/PA,
           RBI_pa = RBI/PA,
           SB_pa = SB/PA,
           playerid = as.character(playerid),
           proj="pecota") %>% 
    filter(!is.na(playerid))
}

####################################
##### AGGREGAGE PROJECTIONS ########
####################################
aggregated <- bind_rows(hitter_data_frames)

#merge all projections into data frame
if (exists("pecotahit")) {
  aggregated <- aggregated %>% 
    mutate(dc_fl = TRUE) %>% 
    bind_rows(pecotahit)
}



#get vector of player names and teams
names <- aggregated %>%
  filter(proj != "pecota") %>% 
  select(playerid, Name) %>% 
  distinct()

#get vector of eligible positions 
all_positions <- pull(aggregated, position) %>% 
  unique() 
 
positions <- select(aggregated, playerid, position) %>% 
  distinct() %>% 
  filter(!position == "NA") %>% 
  pivot_wider(id_cols = playerid, names_from = position, values_from = position) %>% 
  unite("position", one_of(all_positions), na.rm = TRUE, sep = ", ") %>% 
  distinct() 

#grab the plate appearances for the depth charts projections
at_bats <- filter(aggregated, proj=="depthcharts" | proj=="pecota" | proj=="atc") %>%
  #filter(dc_fl == TRUE) %>% 
  group_by(playerid) %>% 
  mutate(depthpa = mean(PA, na.rm=TRUE)) %>%
  select(playerid, depthpa) %>% 
  distinct()


#remove duplicate projections
mean_hitter_proj <- aggregated %>% 
  filter(proj != "depthcharts") %>% 
  select(playerid, proj, AB, PA, AVG, OBP, HR_pa:SB_pa) %>% 
  distinct() 

#average across projection systems
mean_hitter_proj <- mean_hitter_proj %>% 
  group_by(playerid) %>%
  summarise(AB = mean(AB, na.rm = TRUE),
            PA = mean(PA),
            R_ab = mean(R_pa),
            HR_ab = mean(HR_pa),
            RBI_ab = mean(RBI_pa),
            SB_ab = mean(SB_pa),
            AVG = mean(AVG),
            OBP = mean(OBP)) %>% 
  
    #merge in the PA projections
    #left_join(at_bats)  %>% 
    mutate(depthpa=NA) %>% 
    #use depth charts PA if available
    mutate(PA = ifelse(is.na(depthpa), PA, depthpa)) %>%
  
    #multiply rate based projections by PA
    mutate(R = R_ab*PA, 
           HR = HR_ab*PA,
           RBI = RBI_ab*PA,
           SB = SB_ab*PA) %>%
    select(playerid, PA, AB, R, HR, RBI, SB, AVG, OBP) %>% 
   distinct()


#vector of replacement level values
nopos_replacement <- replacement_hitter %>% 
  rename(r_repl = R, hr_repl = HR, rbi_repl = RBI, sb_repl = SB, avg_repl = AVG) 
  

#final vector of hitter projections.
hitter_projections <- names %>% 
  left_join(positions) %>% 
  left_join(mean_hitter_proj) %>% 
  mutate(r_repl  = pull(nopos_replacement, r_repl),
         hr_repl = pull(nopos_replacement, hr_repl),
         rbi_repl = pull(nopos_replacement, rbi_repl),
         sb_repl = pull(nopos_replacement, sb_repl),
         avg_repl = pull(nopos_replacement, avg_repl))


#convert coefficients frame to a normal data frame
coefs_for_calc <- as.numeric(coefs$estimate)
names(coefs_for_calc) <- coefs$Category

#calculate marginal runs and marginal points.
hitter_projections <- hitter_projections %>% 
 mutate(marginal_hr = HR - hr_repl, 
        marginal_runs = R - r_repl,
        marginal_rbi = RBI - rbi_repl,
        marginal_sb = SB - sb_repl,
        marginal_avg = AVG - avg_repl,
        marginal_runs_points = marginal_runs * coefs_for_calc[["r"]],
        marginal_hr_points = marginal_hr * coefs_for_calc[["hr"]],
        marginal_rbi_points = marginal_rbi * coefs_for_calc[["rbi"]],
        marginal_sb_points = marginal_sb * coefs_for_calc[["sb"]],
        marginal_avg_points = marginal_avg * coefs_for_calc[["avg"]]/15,
        marginal_total_points = (marginal_runs_points +
                                   marginal_hr_points +
                                   marginal_rbi_points +
                                   marginal_avg_points +
                                   marginal_sb_points)
        #total of 4680 dollars exist in the league. 1700 marginal points exist. Therefore, marginal
        #point is worth 4680/1700
        #dollar_value = marginal_total_points*(4680/1700)
        
        ) %>% 
  filter(PA > 1) %>% 
  arrange(-marginal_total_points)


#add in team info 
teams <- aggregated %>% 
  filter(!proj=="pecota") %>% 
  select(playerid, Team) %>%
  distinct()

hitter_projections <- hitter_projections %>% 
  left_join(teams)

#calculate dollar values
total_hitter_points <- hitter_projections %>% 
  filter(row_number() <= 270) %>% 
  pull(marginal_total_points) %>% 
  sum()

#estimate cost to buy 1 hitter point
hitter_cost_per_point <- (HITTER_AUCTION_PERCENT*4680)/total_hitter_points
hitter_projections <- hitter_projections %>% 
  mutate(dollar_value = marginal_total_points * hitter_cost_per_point)

#do some pitcher diagnostics on how well calibrated replacement values are 
hitter_diagnostics <- hitter_projections %>% 
  arrange(-dollar_value) %>% 
  filter(row_number() < 271) %>% 
  summarise(marginal_r_points = sum(marginal_runs_points),
            marginal_hr_points = sum(marginal_hr_points),
            marginal_rbi_points = sum(marginal_rbi_points),
            marginal_avg_points = sum(marginal_avg_points),
            marginal_sb_points = sum(marginal_sb_points))

hitter_diagnostics

total_hitter_value <- filter(hitter_projections, row_number() <=270) %>% 
  pull(dollar_value) %>% 
  sum()
total_hitter_value

#########################################################################
################# CALIBRATE PROJECTIONS TO MATCH ROSTERS################# 
#########################################################################

#calculate number of positive players at given_position
positive_hitters <- function(pos=NA) {
  if (is.na(pos)) {
    hitter_projections %>% 
      filter(dollar_value > 1) %>% 
      nrow()
  } else{
    hitter_projections %>% 
      filter(str_detect(position, pos) & dollar_value >= 1) %>% 
      nrow()
  }
}

#first, adjust total number of positive players downward
while(positive_hitters() > 270) {
  hitter_projections <- hitter_projections %>% 
    mutate(dollar_value = dollar_value - .1)
}

#function to adjust up pool by position
adjust_position <- function(adj_position, min) {
  cumulative_adjustment <- 0
  
  while(positive_hitters(adj_position) < min) {

    #adjust up projections
    hitter_projections <<- hitter_projections %>% 
      mutate(dollar_value = ifelse(str_detect(position, adj_position), dollar_value + .2, dollar_value))
    
    cumulative_adjustment <- cumulative_adjustment + .2
  }
  print(paste0(adj_position, " Positional_adjustment: ", cumulative_adjustment))
}

#adjust each position upward as needed to hit minimum number of players at each
adjust_position("catcher", 36)
adjust_position("of", 108)
walk(c("1b","2b","ss", "3b"), adjust_position, min = 27) #31 = 18 at position, 9 at CI/MI


################################################################
################PITCHER STUFF LIVES HERE########################
################################################################

#read in the list of pitcher projections and set list item names
pitcher_proj <- map_chr(PROJECTIONS, function(x) paste("./", x, "/pitchers.csv", sep="")) 
names(pitcher_proj) <- PROJECTIONS


pitcher_proj <- pitcher_proj %>% 
  map(function(x) {
    if (file.exists(x)) read_csv(x) %>% mutate(proj=x)
  }) 

#mutate zips data frame to deal with missing saves
if (!is.null(pitcher_proj[["zips"]])) {
  pitcher_proj[["zips"]] <- mutate(pitcher_proj[["zips"]], SV = NA)
} 


pitcher_proj <- pitcher_proj %>% 
      keep(function(x) !is.null(x)) %>%
      map(select, Name, playerid, Team, IP, ERA, WHIP, SO, SV, W, proj) %>% 
      map(rename, K = SO) %>% 
      map(mutate, proj = str_remove(proj, "./"),
                  proj = str_remove(proj, "/pitchers.csv"),
                  playerid = as.character(playerid)) %>% 
  bind_rows()

  
####################################
############   PECOTA   ############
####################################
if (file.exists("./pecota/pecota2022_pitching_feb15.xlsx")) {
  #read in PECOTA data and rename variables to line up
  pecotapitch <- read_xlsx("./pecota/pecota2022_pitching_feb15.xlsx") %>% 
    rename(Name = name, K = so, IP=ip, ERA = era, WHIP = whip, SV = sv, W = w) %>% 
    select(bpid, IP, ERA, WHIP, K, SV, W, dc_fl) %>% 
    mutate(proj = "pecota") 
  
  pecotapitch <- left_join(pecotapitch, crosswalk) %>% 
    select(-bpid) %>% 
    mutate(playerid = as.character(playerid),
           WHIP = as.numeric(WHIP),
           IP = as.numeric(IP)) %>% 
    filter(!is.na(playerid))  
}


#group pecota with other projections 
if (exists("pecotapitch")) {
  pitcher_proj <- pitcher_proj %>% 
    bind_rows(pecotapitch)  
}

# get team names
pitcher_teams <- pitcher_proj %>% 
  filter(proj != "pecota") %>% 
  select(playerid, Team) %>% 
  distinct()

#get vector of innings pitched
innings <- filter(pitcher_proj, proj=="depthcharts" | proj=="pecota" | proj=="atc") %>%
    #filter(dc_fl == TRUE) %>% 
    group_by(playerid) %>% 
    mutate(depthip = mean(IP, na.rm=TRUE)) %>%
    ungroup() %>% 
    select(playerid, depthip) %>% 
    distinct()

#get vector of saves per IP
saves_per_ip <- pitcher_proj %>% 
  filter(proj == "depthcharts" | proj=="pecota" | proj=="atc") %>% 
  group_by(playerid) %>% 
  mutate(SV_IP = mean(SV/IP, na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(playerid, SV_IP) %>% 
  distinct()

# get vecotr of names
pitcher_names <- pitcher_proj %>% 
  filter(proj!="pecota") %>% 
  select(playerid, Name) %>% 
  distinct()


#spread per ip numbers across depth charts IP
pitcher_proj <- mutate(pitcher_proj,
                       K_IP = K/IP,
                       W_IP = W/IP) %>%
      group_by(playerid) %>%
      summarise(ERA = mean(ERA),
                WHIP = mean(WHIP),
                K_IP = mean(K/IP),
                W_IP = mean(W/IP)) %>%
  left_join(saves_per_ip) %>%
  mutate(SV_IP = ifelse(is.na(SV_IP), 0, SV_IP))

#merge everything together, multiply per inning stats by IP
pitcher_proj <- pitcher_names %>%
  left_join(innings) %>%
  left_join(pitcher_proj) %>%
  left_join(pitcher_teams) %>%
  mutate(
         IP = depthip,
         ERA = round(ERA, 2),
         WHIP = round(WHIP, 2),
         K = round(K_IP*IP, 0),
         SV = round(SV_IP*IP, 0),
         W = round(W_IP*IP, 0),
         position = "pitcher") %>%
  select(Name, Team, IP, W, ERA, SV, K, WHIP, playerid, position) %>% 
  distinct() 
  



#calculate marginal values and points
pitcher_projections <- pitcher_proj %>%
      mutate(era_repl = pull(replacement_pitcher, ERA),
             whip_repl = pull(replacement_pitcher, WHIP),
             k_repl = pull(replacement_pitcher, K),
             win_repl = pull(replacement_pitcher, W),
             sv_repl = pull(replacement_pitcher, SV),
             marginal_ERA = ERA - era_repl,
             marginal_WHIP = WHIP - whip_repl,
             marginal_W = W - win_repl,
             marginal_SV = SV - sv_repl,
             marginal_K = K - k_repl,
             ERA_points = (marginal_ERA *coefs_for_calc[["era"]])*(IP/1217), #average top 270 pitcher in 2020 pitched 45 innings
             WHIP_points = (marginal_WHIP*coefs_for_calc[["whip"]])*(IP/1217), #45 prorates to 121.7 over a 162-game
             W_points = marginal_W*coefs_for_calc[["w"]],
             SV_points = marginal_SV*coefs_for_calc[["sv"]],
             K_points = marginal_K*coefs_for_calc[["k"]],
             marginal_total_points =  ERA_points + WHIP_points + W_points + SV_points + K_points
      ) %>% 
  arrange(-marginal_total_points)



#calculate dollar values
total_pitcher_points <- pitcher_projections %>% 
  filter(row_number() <= 180) %>% 
  pull(marginal_total_points) %>% 
  sum()

#estimate cost to buy 1 pitcher point
pitcher_cost_per_point <- (PITCHER_AUCTION_PERCENT*4680)/total_pitcher_points
pitcher_projections <- pitcher_projections %>% 
  mutate(dollar_value = marginal_total_points * pitcher_cost_per_point)

#do some pitcher diagnostics on how well calibrated replacement values are 
pitcher_diagnostics <- pitcher_projections %>% 
  arrange(-dollar_value) %>% 
  filter(row_number() < 181) %>% 
  summarise(marginal_era_points = sum(ERA_points),
            marginal_whip_points = sum(WHIP_points),
            marginal_k_points = sum(K_points),
            marginal_sv_points = sum(SV_points),
            marginal_w_points = sum(W_points))
  
  
pitcher_diagnostics
  

pitcher_projections <- pitcher_projections %>% 
      #sort by dollar value
      arrange(desc(dollar_value)) %>%
      
      #select relevant columns
      select(Name,Team,position,playerid,IP,ERA,WHIP,SV,W,K,marginal_total_points,dollar_value) %>%
      
      #round points and dollars columns
      mutate(marginal_total_points = round(marginal_total_points, 2), dollar_value = round(dollar_value, 2)) %>%
      
      #select only pithcers with at least 1 IP
      filter(IP > 1)

#########################################################################
############# CALIBRATE PROJECTIONS TO MATCH ROSTER SIZES ############### 
#########################################################################

#calculate number of positive players at given_position
positive_pitchers <- function() {
  pitcher_projections %>% 
  filter(dollar_value >= 1) %>% 
  nrow()
}
positive_pitchers() 

pitcher_adjustment <- 0

while (positive_pitchers() < 180) {
  pitcher_projections <- pitcher_projections %>% 
    mutate(dollar_value = dollar_value + .1)
  
  pitcher_adjustment <- pitcher_adjustment + .1
}

while (positive_pitchers() > 181) {
  pitcher_projections <- pitcher_projections %>% 
    mutate(dollar_value = dollar_value - .1)
  
  pitcher_adjustment <- pitcher_adjustment - .1
}

print(paste0("Pitcher Adjustment: ", pitcher_adjustment))
positive_pitchers() 


#adjust for the will smith problem
hitter_projections <- hitter_projections %>% 
  mutate(Name = ifelse(playerid == 19197, "Will Smith (C)", Name),
         Name = ifelse(playerid == 20391, "Luis Garcia (2B)", Name)) %>% 
  filter(!Name == "Edwin Diaz") %>% 
  unique()

pitcher_projections <- pitcher_projections %>% 
  mutate(Name = ifelse(playerid == 8048, "Will Smith (P)", Name),
         Name = ifelse(playerid == 23735, "Luis Garcia (P)", Name)) %>% 
  
  #drop Luis garcia, padres pitcher
  filter(playerid != 6984)



save(hitter_projections, pitcher_projections, file = "projections.rda")
 

