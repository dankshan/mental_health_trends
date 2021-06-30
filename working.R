library(tidyverse)
library(survey)
library(srvyr)
library(jtools)
library(keyring)
library(openssl)

if (!("youth19_secret" %in% key_list()$service)) {
  key_set("youth19_secret")
}

source("functions.R")


svy <- cyphr::decrypt(
  readRDS("../Master data/data/svydesignCombinedCalibrated.rds"),
  cyphr::key_sodium(sha256(key_get_raw("youth19_secret")))
)

#set as srvyr survey for compatibility
svy <- as_survey_design(svy)



# ###verify model works -------
# #we want the to set the intercept as the year in the correct order
# svy2 <- 
#   svy %>%
#   mutate_at(vars(Year), list(~fct_relevel(., c("2001", "2007", "2012", "2019"))))
#             
# output <- 
#   svy2 %>%
#   #how we want to group the models
#   nest_by(sex) %>%
#   #mutate a new variable called model, this is a list with each item of the list being the glm results
#   mutate(model = list(svyglm(depressed28 ~ Year, design = data, family = quasibinomial)))
# 
# 
# #lets look at the output, in this case the first item in the list is "Female"
# summary(output$model[[1]])

#and it matches if we do a svyglm manually
#summary(svyglm(depressed28 ~ Year, subset(svy, sex == "Female"), family = quasibinomial))

svy <-
  svy %>%
  mutate(
    
    Total = "Total",
    
    
    age2 = case_when(AgeUnder16 ~ "Under16",
                     TRUE ~ "Over16"),
    
    dep2 = case_when(Year == "2001" ~ "NoDep1",
                     
                     Year %in% c("2007", "2012") ~ case_when(NZDep2006 %in% c(1,2) ~ "1-2",
                                                             NZDep2006 %in% c(3,4) ~ "2-4",
                                                             NZDep2006 %in% c(5,6) ~ "5-6",
                                                             NZDep2006 %in% c(7,8) ~ "7-8",
                                                             NZDep2006 %in% c(9,10) ~ "9-10"),
                     
                     Year %in% c("2019") ~ case_when(NZDep2018 %in% c(1,2) ~ "1-2",
                                                     NZDep2018 %in% c(3,4) ~ "2-4",
                                                     NZDep2018 %in% c(5,6) ~ "5-6",
                                                     NZDep2018 %in% c(7,8) ~ "7-8",
                                                     NZDep2018 %in% c(9,10) ~ "9-10")),
    
    dep3 = case_when(NZDep_band3 == 1 ~ "dep_1-3",
                     NZDep_band3 == 2 ~ "dep_4-7",
                     NZDep_band3 == 3 ~ "dep_8-10"),
    
    decile_band2 = case_when(Decile %in% c(1,2,3) ~ "low_decile",
                             Decile %in% c(4,5,6,7) ~ "medium_decile",
                             Decile %in% c(8,9,10) ~ "high_decile"),
    
    
    haplife = case_when(Year == "2001" & Emot2 == 4 ~ 1,
                        Year == "2001" & Emot2 %in% c(1,2,3) ~ 0,
                        Year %in% c("2007", "2012", "2019") & Emot2 == 1 ~ 1,
                        Year %in% c("2007", "2012", "2019") & Emot2 %in% c(2,3,4) ~ 0),
    
    satislife = case_when(Year == "2001" & Emot2 %in% c(4,3) ~ 1,
                          Year == "2001" & Emot2 %in% c(1,2) ~ 0,
                          Year %in% c("2007", "2012", "2019") & Emot2 %in% c(1,2) ~ 1,
                          Year %in% c("2007", "2012", "2019") & Emot2 %in% c(3,4) ~ 0),
    
    Emot5_3_rev = case_when(Year == "2019" & Emot5_3 == 1 ~ 2,
                            Year == "2019" & Emot5_3 == 2 ~ 1,
                            Year == "2019" & Emot5_3 == 3 ~ 0),
    
    Emot5_7_rev = case_when(Year %in% c("2007", "2012") & Emot5_7 == 1 ~ 2,
                            Year %in% c("2007", "2012") & Emot5_7 == 2 ~ 1,
                            Year %in% c("2007", "2012") & Emot5_7 == 3 ~ 0),
    
    urban2 = case_when(Year %in% c("2007", "2012") ~ case_when(Urban_Rural_Ind %in% c(1, 2) ~ "urban", #urban
                                                               Urban_Rural_Ind %in% c(3) ~ "town", #town
                                                               Urban_Rural_Ind %in% c(4, 5) ~ "rural"),  #rural
                       
                       Year == "2019" ~ case_when(IUR2019_code %in% c(11, 12, 13) ~ "urban", #urban
                                                  IUR2018_code %in% c(14) ~ "town", #towns
                                                  IUR2019_code %in% c(21, 22) ~ "rural" #rural
                       ))
  ) %>%
  
  mutate_at(vars(selfHarm), list(~case_when(Year == "2019" ~ .,
                                            Year %in% c("2007", "2012") & Emot13 %in% c(2,3,4,5) ~ 1,
                                            Year %in% c("2007", "2012") & Emot13 == 1 ~ 0,
                                            Year == "2001" ~ 0,
                                            TRUE ~ NA_real_))) %>%
  
  mutate_at(vars(dep2wks), list(~case_when(Year == "2019" ~ .,
                                           Year %in% c("2007", "2012") & Emot11 == 1 ~ 1,
                                           Year %in% c("2007", "2012") & Emot11 == 2 ~ 0,
                                           Year == "2001" ~ 0,
                                           TRUE ~ NA_real_))) %>%
  
  mutate_at(vars(thoughtSuicide), list(~case_when(Year == "2019" ~ .,
                                                  Year == "2001" & EmotH58 %in% c(3,4) ~ 1,
                                                  Year == "2001" & EmotH58 %in% c(1,2) ~ 0,
                                                  Year %in% c("2007", "2012") & Emot15_1 %in% c(3,4) ~ 1,
                                                  Year %in% c("2007", "2012") & Emot15_1 %in% c(1,2) ~ 0,
                                                  TRUE ~ NA_real_))) %>%
  
  mutate_at(vars(attemptSuicide), list(~case_when(Year %in% c("2007", "2012", "2019") ~ .,
                                                  Year == "2001" & EmotH61 %in% c(3,4) ~ 1,
                                                  Year == "2001" & EmotH61 %in% c(1,2) ~ 0,
                                                  TRUE ~ NA_real_))) %>%
  
  mutate_at(vars(Emot5_2, Emot5_4, Emot5_5, Emot5_6, Emot5_12, Emot5_18, Emot5_22), list(~ . -1)) %>%
  
  mutate(conductProblems = case_when(Year == "2001" ~ 0,
                                     
                                     Year %in% c("2007", "2012") & (Emot5_5 + Emot5_7_rev + Emot5_12 + Emot5_18 + Emot5_22) >= 5 ~ 1,
                                     Year %in% c("2007", "2012") & (Emot5_5 + Emot5_7_rev + Emot5_12 + Emot5_18 + Emot5_22) < 5 ~ 0,
                                     
                                     Year == "2019" & (Emot5_2 + Emot5_3_rev + Emot5_4 + Emot5_5 + Emot5_6) >= 5 ~ 1,
                                     Year == "2019" & (Emot5_2 + Emot5_3_rev + Emot5_4 + Emot5_5 + Emot5_6) < 5 ~ 0))


#need to generate the prevalence tables

#Table1 ####

#how to group the data
groups <- 
  data.frame(
    grp = c("Total", "sex", "age2", "ethnic_p5", "dep3")
  )

#variables to report
variables <-
  data.frame(
    var = c("wellbeing", "depressed28", "dep2wks", "selfHarm", "thoughtSuicide", "attemptSuicide", "conductProblems")
  )

#produce the prevalence tables
prevs <-
  variables %>%
  pmap_dfr(
    function(var){
      groups %>%
        pmap_df(
          function(grp) {
            prev(variable = var, group = grp)
          }
        )
    }
  )
  

#reshape to match paper tables
prevs <-
  prevs %>%
  pivot_wider(names_from = Year, names_glue = "{Year}_{.value}", values_from = c(n, N, pct, pct_low, pct_upp)) %>%
  select(variable, group_name, starts_with("2001"), starts_with("2007"), starts_with("2012"), starts_with("2019"))



#table 2 ####

#how to group the data - include decile and urban2
groups <- 
  data.frame(
    grp = c("Total", "sex", "age2", "ethnic_p5", "dep3", "decile_band2", "urban2")
  )

#variables to report
variables <-
  data.frame(
    var = c("wellbeing", "depressed28", "phq4anx", "selfHarm", "thoughtSuicide", "attemptSuicide", "conductProblems")
  )

#produce the prevalence tables
prevs2 <-
  variables %>%
  pmap_dfr(
    function(var){
      groups %>%
        pmap_df(
          function(grp) {
            prev2(variable = var, group = grp)
          }
        )
    }
  )

#reshape table to fit paper
prevs2 %>%
  filter(!is.na(group_name)) %>%
  pivot_wider(names_from = variable, names_glue = "{variable}_{.value}", values_from = c(n, N, pct, pct_low, pct_upp)) %>%
  select(group_name, starts_with("wellbeing"), starts_with("depressed"), starts_with("phq4anx"), starts_with("selfHarm"), starts_with("thoughtSuicide"), starts_with("attemptSuicide"), starts_with("conductProblems")) %>%
  write_csv("./outputs/MH_trends_table2.csv")




#odds ratios ####


#how to group the data
groups <- 
  data.frame(
    grp = c("Total", "sex", "age2", "ethnic_p5", "dep3")
  )

#variables to report
variables <-
  data.frame(
    var = c("wellbeing", "depressed28", "dep2wks", "selfHarm", "thoughtSuicide", "attemptSuicide", "conductProblems")
  )


odds_ratio <-
  variables %>%
  pmap_dfr(
    function(var) {
      groups %>%
        pmap_df(
          function(grp) {
            OR(group = grp, variable = var)
          }
        )
    }
  )



#join the prevs and OR
joined <-
  prevs %>%
  left_join(odds_ratio, by = c("variable", "group_name"))


write_csv(joined, "./outputs/MH_trends_prev_OR.csv")




#https://journals.sagepub.com/doi/pdf/10.1177/0004867413514489

#to view the next item in the list (i.e. Male) then change [[1]] to [[2]]




library("wesanderson")

pal <- wes_palette("Zissou1", 100, type = "continuous")

prevs %>%
  filter(Year != 2001, !is.na(group_name)) %>%
  mutate_at(vars(variable), ~fct_relevel(as.factor(variable), levels = c("wellbeing", "dep2wks", "selfHarm", "thoughtSuicide", "depressed28", "attemptSuicide", "conductProblems"))) %>%
  mutate(group1 = paste(group, group_name, sep = "_")) %>%
  mutate_at(vars(pct, pct_low, pct_upp), ~if_else(pct == 0, NA_real_, .)) %>%
  ggplot(aes(x = Year, y = pct, group = variable, colour = variable)) +
  geom_line() + 
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width= 0.1) +
  geom_point() +
  facet_grid(~group1)


# scale_colour_manual(values = wes_palette("Zissou1", 7, type = "continuous")) +
