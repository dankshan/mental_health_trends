#function to generate prevalence data for agroup and variable
prev <- function(group, variable) {
  
  svy %>%
    group_by(Year, group_name = !!as.name(group)) %>%
    summarise(
      n = unweighted(sum(!!as.name(variable) == 1, na.rm = TRUE)),
      N = unweighted(sum(!is.na(!!as.name(variable)), na.rm = TRUE)),
      pct = survey_ratio(!!as.name(variable) == 1, !is.na(!!as.name(variable)), na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(variable = !!as.character(variable),
           group = !!as.character(group)) %>%
    select(variable, everything())
  
}

#For table 2 we just need 2019
prev2 <- function(group, variable) {
  
  svy %>%
    filter(Year == "2019") %>%
    group_by(group_name = !!as.name(group)) %>%
    summarise(
      n = unweighted(sum(!!as.name(variable) == 1, na.rm = TRUE)),
      N = unweighted(sum(!is.na(!!as.name(variable)), na.rm = TRUE)),
      pct = survey_ratio(!!as.name(variable) == 1, !is.na(!!as.name(variable)), na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(variable = !!as.character(variable)) %>%
    select(variable, everything())
  
}



#need to generate the ORs
#function will produce a table of OR, CI, t value, and p value for the group and variable passed to it

OR <- function(group, variable) {
  
  #need to exclude the grouped variable from the glm formula
  #the variable that we're grouping by can't be in the formula so remove it from the formula list
  
  variables <- tibble(vars = c("Year", "age2", "sex", "ethnic_p5", "dep3", "urban2")) %>% filter(vars != group) %>% pull(vars)
  
  formula <- as.formula(paste(variable, paste(variables, collapse = " + "), sep = " ~ "))
  
  ### put it to use ------
  output <- 
    svy %>%
    #only want to know the OR between 2012 and 2019
    filter(Year %in% c("2012", "2019")) %>%
    mutate_at(vars(Year), list(~fct_relevel(., c("2012", "2019")))) %>%
    #this is the variable we want to group by
    nest_by(!!as.name(group)) %>%
    #mutate a new variable called model, this is a list with each item of the list being the glm results
    mutate(model = list(svyglm(formula, design = data, family = quasibinomial)))
  
  
  
  
  datalist <- list()
  
  for(i in 1:length(output[[1]])){
    
    if(is.na(as.character(output[[1]][[i]]))){
      break()
    }
    
    coefs <- as.data.frame(summ(output$model[[i]], exp = TRUE, confint = TRUE, digits = 5)["coeftable"])
    
    
    
    dat <-
      data.frame(
        variable = as.character(variable),
        group_name = as.character(output[[1]][[i]]),
        OR = coefs["Year2019", 1],
        ci_low = coefs["Year2019", 2],
        ci_upp = coefs["Year2019", 3],
        t_val = coefs["Year2019", 4],
        p = coefs["Year2019", 5]
      )
    
    datalist[[i]] <- dat
  }
  
  return(bind_rows(datalist))
  # return(output)
  
}
