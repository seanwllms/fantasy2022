#read csv of draft picks
draftpicks <- readxl::read_xlsx("draftpicks.xlsx", 
                                sheet = "draftpicks",
                                col_types = c("text", 
                                              "text",
                                              "numeric",
                                              "text",
                                              "date")) %>% 
  mutate(player = trimws(player))


draft_for_rosters <- draftpicks %>% 
  group_by(position, team) %>% 
  mutate(duplicatenum = row_number()) %>% 
  mutate(position = ifelse(position %in% c("C", "OF", "P", "B"), 
                           paste0(position, duplicatenum),
                           position)) %>% 
  select(team, position, player, salary) %>% 
  #fix dan's salary calculations for bench dudes
  mutate(salary = ifelse(position %in% BENCH_SPOTS, salary-1, salary))


roster_draft <- rosters %>% 
  left_join(draft_for_rosters, by = c("team", "position")) %>% 
  mutate(salary = ifelse(is.na(salary), 0, salary),
         player = ifelse(is.na(player), "", player))


#######################################################
###############Mark Drafted Function###################
#######################################################

hitter_projections <- hitter_projections %>% 
      mutate(status = ifelse(Name %in% draftpicks$player, "drafted", ""))

pitcher_projections <- pitcher_projections %>% 
      mutate(status = ifelse(Name %in% draftpicks$player, "drafted", ""))

drafterrors <- select(draftpicks, player, team) %>%
      mutate(error = ifelse(player %in% hitter_projections$Name | player %in% pitcher_projections$Name,
             "matched", "not matched")
      ) %>%
      filter(error == "not matched")


