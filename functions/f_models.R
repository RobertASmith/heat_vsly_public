# Model functions for the 4 different models

#f.vsl

f.vsly.mb <- function(n_people = 1, # number of people
                      add.mins = 10, # additional minutes
                      country = country,  # country
                      mode = "walking",     # mode of travel
                      select.age.dist = 20:74, # age distribution
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                      merged_lt = merged_lt,
                      merged_data = merged_data
                      ){ 
  
  
  # read in data from merged datatables.
  vsly    = merged_data[ISO_Code == country,vsly_all]
  gen_pop = merged_lt[ISO_Code == country & age %in% select.age.dist, pop]
  dlyr    = merged_lt[ISO_Code == country & age %in% select.age.dist, dlyr]
  mr      = merged_lt[ISO_Code == country & age %in% select.age.dist, mr]
  
  if(mode=="walking"){   
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
  } else if(mode=="cycling"){ 
    
    rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
  
    } else { rr <- "broken"
    }
  
  # calculates percent in each age group
  prop.pop <- gen_pop / sum(gen_pop)  
  
  lys  <- sum(mr  *  (1-rr) *  dlyr *   prop.pop)     # proportion of population in each year
 
  mb   <- lys * vsly * n_people            # calculates monetary benefit per person
  
  return(mb)
  
  }







# ONE GROUP


f.vsl.1g <- function( add.mins = 10,
                      n_people = 1,
                      country = NA,
                      mode = "walking",
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                      merged_data = merged_data){
  
  # input parameters from merged_data
  heat_mort2074 = merged_data[ISO_Code == country,mort20_74]
  heat_mort2064 = merged_data[ISO_Code == country,mort20_64]
  vsl           = merged_data[ISO_Code == country,VSL]
  
  if(mode=="walking"){   
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
    ls <- heat_mort2074 * (1-rr)
  
      } else if(mode=="cycling"){ 
      
      rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
      
      ls <- heat_mort2064 * (1-rr)
      
        } else {print("broken")}
  
  mb <- ls * vsl * n_people   # monetary benefit just lives saved (rate*number) multiplied by vsl.
  
  return(mb)
}



# TWO GROUPS




f.vsl.2g <- function(add.mins = 10,
                     n_people = 1,
                     country = "UKR",
                     mode = "walking",
                     select.age.dist = 20:74, # age distribution
                     rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                     rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                     ref_duration_walk = params$ref_duration_walk,# reference duration walking
                     ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                     max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                     max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                     merged_data = merged_data,
                     merged_lt = merged_lt
                     ){
  
  # input parameters from merged_data
  heat_mort2074 = merged_data[ISO_Code == country,mort20_74]
  heat_mort2064 = merged_data[ISO_Code == country,mort20_64]
  heat_mort2044 = merged_data[ISO_Code == country,mort20_44]
  heat_mort4574 = merged_data[ISO_Code == country,mort45_74]
  heat_mort4564 = merged_data[ISO_Code == country,mort45_64]
  vsl    = merged_data[ISO_Code == country,VSL]
  
  # input parameters from merged_lt
  gen_pop = merged_lt[ISO_Code == country & age %in% select.age.dist, .(pop,age)]
  dlyr    = merged_lt[ISO_Code == country & age %in% select.age.dist, dlyr]
  mr      = merged_lt[ISO_Code == country & age %in% select.age.dist, mr]
  
  
  if(mode=="walking"){   
    
    # calculate the proportion of the population young and old.
    prop.yng = sum(gen_pop[age %in% 20:44,pop]) / sum(gen_pop[age %in% 20:74,pop])
    prop.old = sum(gen_pop[age %in% 45:74,pop]) / sum(gen_pop[age %in% 20:74,pop]) 
    
    # calculate relative risk
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    # estimate lives saved per capita.
    ls <- sum( heat_mort2044 * prop.yng * (1-rr) ) + sum(heat_mort4574 * prop.old * (1-rr) )
    
  } else if(mode=="cycling"){
    
    # calculate the proportion of the population young and old.
    prop.yng = sum(gen_pop[age %in% 20:44,pop]) / sum(gen_pop[age %in% 20:64,pop])
    prop.old = sum(gen_pop[age %in% 45:64,pop]) / sum(gen_pop[age %in% 20:64,pop]) 
    
    # calculate relative risk
    rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
    # estimate lives saved per capita.
    ls <- sum( heat_mort2044 * prop.yng * (1-rr) ) + sum(heat_mort4564 * prop.old * (1-rr) )
    
  } else { print("broken") }
  
  
  mb <- ls *  n_people * vsl    # monetary benefit is lives saved (rate*number) multiplied by vsl
  
  return(mb)
}


f.vsl.55g <- function(add.mins = 10,
                      n_people = 1,
                      country = NA,
                      mode = "walking",
                      select.age.dist = 20:74, # age distribution
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,
                      merged_data = merged_data,
                      merged_lt = merged_lt){
  
  # input parameters from merged_data
  vsl    = merged_data[ISO_Code == country,VSL]
  
  # input parameters from merged_lt
  gen_pop = merged_lt[ISO_Code == country & age %in% select.age.dist, .(pop,age)]
  dlyr    = merged_lt[ISO_Code == country & age %in% select.age.dist, dlyr]
  mr      = merged_lt[ISO_Code == country & age %in% select.age.dist, mr]
  
  if(mode=="walking"){ 
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
  } else if(mode=="cycling"){ 
    
    rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
    
  } else { rr <- "broken"
  }
  
  prop.pop <- gen_pop / sum(gen_pop)  

  ls       <- sum(mr * (1-rr) * prop.pop)            # lives saved
  
  mb       <- ls * vsl * n_people                     # monetary benefit
  
  return(mb)
  
  }


