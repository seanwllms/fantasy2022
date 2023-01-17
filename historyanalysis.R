#set up file
library(tidyverse)
library(readxl)

#read in results from pre-2014
results <- read_xlsx("./history/historicalresults.xlsx") %>% 
  mutate(Category = tolower(Category))



#reorder columns to match 2014 and 2015
results <- select(results, Category, Value, Year, Points)

sb <- filter(results, Category == "sb")

######################################
#####read in the 2014 results#########
######################################
standings.2014 <- read_csv("./history/results2014.csv") %>% 
      select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) %>% 
  rename_all(.funs = tolower) %>% 
  gather("Category", "Value") %>%
  mutate(Value = as.numeric(Value)) %>% 
  group_by(Category) %>% 
  arrange(desc(Value)) %>%
  mutate(Points = min_rank(Value)) %>% 
  mutate(Points = ifelse(Category %in% c("era", "whip"), 
                         19-Points, 
                         Points), 
         Year = 2014)

######################################
#####read in the 2015 results#########
######################################
standings.2015 <- read_csv("./history/results2015.csv") %>% 
  select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) %>% 
  rename_all(.funs = tolower) %>% 
  gather("Category", "Value") %>% 
  group_by(Category) %>% 
  arrange(desc(Value)) %>%
  mutate(Points = min_rank(Value)) %>% 
  mutate(Points = ifelse(Category %in% c("era", "whip"), 
                         19-Points, 
                         Points), 
         Year = 2015)

######################################
##### Merge All Results Together######
######################################
all_results <- bind_rows(results, standings.2014, standings.2015) %>%
      #filter out rows from 2013 that seem to be outliers
      filter(!(Year==2013 & Category %in% c("r", "rbi", "hr"))) %>% 
  arrange(Category, Year, -Value) %>% 
  mutate(Value = ifelse(Category %in% counting_cats & Year == 2020, Value*2.7, Value))

save(all_results, file="historicalresults.rda")

######################################
#####Graphs and Analysis go Here######
######################################
library(ggplot2)
library(broom)

counting_cats <- c("r","hr","rbi","sb", "k","sv","w")

catplot <- all_results %>% 
  mutate(LastYear = ifelse(Year == 2021, "2021", "Earlier Years")) %>% 
  ggplot(aes(x=Value, y=Points)) +
  geom_point(aes(color = LastYear)) +
  facet_wrap(~ Category, ncol=2, scales="free_x") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank())

ggsave("catplot.png", catplot, width=6, height = 8)

###Regression Time####

#filter out 1, 2, 17 and 18 point recipients (skew results)
regress <- all_results %>% filter(Points > 2 & Points < 17) %>% 
  filter(CURRENT_YEAR - Year < 4) %>% 
  mutate(weight = case_when(
    Year == CURRENT_YEAR - 1 ~ 3,
    Year == CURRENT_YEAR - 2 ~ 2,
    Year == CURRENT_YEAR - 3 ~ 1
  )) 

regress %>% filter(Category == "r") %>% 
  ggplot(aes(x = Value, y = Points, color=factor(Year))) + 
  geom_point() +
  scale_color_discrete()

#run the regression for each category
regress_results <- regress %>% 
  nest(data = -Category) %>% 
  mutate(fit = map(data, ~lm(Points ~ Value + factor(Year), weights=weight, data = .)),
         tidied = map(fit, tidy)) %>% 
  unnest(tidied)
  

#organize regression results in to tidy df for calculating value 
coefs <- regress_results %>% 
      filter(term == "Value") %>%
      select(Category, estimate)

save(coefs, file="coefs.rda")

#organize regression results in to tidy df for calculating standings
coefs.standings <- regress %>% 
  nest(data = -Category) %>% 
  mutate(fit = map(data, ~lm(Points ~ Value, weights=weight, data = .)),
         tidied = map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  select(Category, term, estimate)  %>% 
  filter(!str_detect(term, "factor")) %>% 
  spread(term, estimate)


names(coefs.standings)[2:3] <- c("yint", "coef")

save(coefs.standings, file="standingscoefs.rda")
