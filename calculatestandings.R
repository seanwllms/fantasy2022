
#load coeficients for calculating standings
load("standingscoefs.rda")

#calculate standings for each team
standings <- rosters_merged %>% 
  group_by(team) %>% 
  summarise(R = sum(R, na.rm=TRUE),
            HR = sum(HR, na.rm=TRUE),
            RBI = sum(RBI, na.rm=TRUE),
            SB = sum(SB, na.rm=TRUE),
            AVG = sum(AVG*AB, na.rm=TRUE)/sum(AB, na.rm=TRUE),
            ERA = sum(IP*ERA, na.rm=TRUE)/sum(IP, na.rm=TRUE),
            WHIP = sum(IP*WHIP, na.rm=TRUE)/sum(IP, na.rm=TRUE),
            K = sum(K, na.rm=TRUE),
            SV =sum(SV, na.rm=TRUE),
            W = sum(W, na.rm=TRUE),
            spent = sum(salary, na.rm=TRUE),
            left = 260-sum(salary, na.rm=TRUE), 
            picks_left = sum(salary==0 & player == "" & !(position %in% BENCH_SPOTS))) %>% 
  mutate(across(R:SB, round, 1),
         across(AVG:WHIP, round, 4),
         across(K:W, round, 1))


calc_points <- function(baseball_stat, stat_value) {
  
  regresults <- coefs.standings %>% filter(Category == baseball_stat)
  
  int <- pull(regresults, yint)
  coef <- pull(regresults, coef)
  
  #calculate value
  points <- int + coef * stat_value
  points
}


#limit points to 1 to 18, and round to nearest tenth
rational_points <- function(x) {
  y <- pmax(x,1)
  y <- pmin(y,18)
  y <- round(y, 1)
  y
}

#calculate points for each category
standings <- standings %>% 
  mutate(R_points = calc_points("r", R),
         HR_points = calc_points("hr", HR),
         RBI_points = calc_points("rbi", RBI),
         SB_points = calc_points("sb", SB),
         AVG_points = calc_points("avg", AVG),
         ERA_points = calc_points("era", ERA),
         WHIP_points = calc_points("whip", WHIP),
         K_points = calc_points("k", K),
         SV_points = calc_points("sv", SV),
         W_points = calc_points("w", W)) %>% 
  mutate(across(matches("_points"), rational_points))


  

#calculate and round total points
standings <- mutate(standings, total_points = 
                          R_points +
                          HR_points+
                          RBI_points+
                          SB_points+
                          AVG_points+
                          ERA_points+
                          WHIP_points+
                          K_points+
                          SV_points+
                          W_points) %>%
      mutate(total_points = round(total_points,1)) %>%
      arrange(desc(total_points))


# 
# bench_bucks <- draftpicks %>% 
#   filter(position == "B", salary > 0) %>% 
#   group_by(team) %>% 
#   summarise(bench_dollars = sum(salary)) 
# 
# if (nrow(bench_bucks) > 0) {
#   standings <- standings %>% 
#     left_join(bench_bucks, by=c("team" = "team")) %>% 
#     mutate(
#       bench_dollars = ifelse(is.na(bench_dollars), 0, bench_dollars),
#       spent = spent + bench_dollars,
#       left = left - bench_dollars)
# }