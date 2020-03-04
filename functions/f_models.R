# Model functions for the 4 different models

#f.vsl

f.vsly.mb <- function(n_people = 1, # number of people
                      add.mins = 10, # additional minutes
                      country = NA,  # country
                      mode = "walking",     # mode of travel
                      select.age.dist = NA, # age distribution
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                      gen_pop = inputs$gen_pop,                     # population each age
                      perc_fmle = inputs$perc_fmle,                 # percentage female each age
                      vsly = inputs$vsly,                           # vsly
                      m_dlyr = inputs$dlyr,                         # discounted life years remaining
                      m_lt = inputs$gbd_mort                       # gbd lifetables
                      ){ 
  
  if(mode=="walking"){   
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
  } else if(mode=="cycling"){ 
    
    rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
  
    } else { rr <- "broken"
  }
  
  # calculates percent in each age group
  prop.pop <- gen_pop[gen_pop$age %in% select.age.dist,"All"] / sum(gen_pop[gen_pop$age %in% select.age.dist,"All"])  
  
  lys  <- sum(m_lt[m_lt$age %in% select.age.dist,c("Female","Male")] *         # mortality rate for each age and sex
              (1-rr) *                                                         # relative risk reduction.
              m_dlyr[m_dlyr$age %in% select.age.dist,c("dlyr_f","dlyr_m")] *   # discounted life years remaining
              prop.pop)                                                        # proportion of population in each year
 
  mb   <- lys * vsly ** n_people            # calculates monetary benefit per person
  
  return(mb)
  
  }

# change this code to reflect new data sources,

f.vsl.1g <- function( add.mins = 10,
                      n_people = 1,
                      country = NA,
                      mode = "walking",
                      vsl = inputs$vsl,
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                      heat_mort2074 = inputs$heat_mort2074,
                      heat_mort2064 = inputs$heat_mort2064){
  
  if(mode=="walking"){   
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
    ls <- heat_mort2074 * (1-rr)
  
      } else if(mode=="cycling"){ 
      
      rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
      
      ls <- heat_mort2064 * (1-rr)
      
        } else {print("broken")}
  
  mb <- ls*vsl*n_people   # monetary benefit just lives saved (rate*number) multiplied by vsl.
  
  return(mb)
}


f.vsl.2g <- function(add.mins = 10,
                     n_people = 1,
                     country = NA,
                     mode = "walking",
                     vsl = inputs$vsl,
                     select.age.dist = NA, # age distribution
                     gen_pop = inputs$gen_pop,                    # population each age
                     rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                     rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                     ref_duration_walk = params$ref_duration_walk,# reference duration walking
                     ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                     max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                     max_rr_cycle = params$max_rr_cycle,           # max risk reduction cycling
                     heat_mort2074 = inputs$heat_mort2074,
                     heat_mort2064 = inputs$heat_mort2064,
                     heat_mort2044 = inputs$heat_mort2044,
                     heat_mort4574 = inputs$heat_mort4574,
                     heat_mort4564 = inputs$heat_mort4564 
                     ){
  
  population <- matrix(data = 0,nrow = 100,ncol = 1,dimnames = list(1:100,"pop"))
  population[select.age.dist,"pop"] <- gen_pop[gen_pop$age %in% select.age.dist,"All"]
 # population = gen_pop[gen_pop$age %in% select.age.dist,"All"]   # vector of total population 
  
  if(mode=="walking"){   
    
    # calculate the proportion of the population young and old.
    prop.yng = sum(population[20:44,"pop"]) / sum(population[20:74,"pop"])
    prop.old = sum(population[45:74,"pop"]) / sum(population[20:74,"pop"]) 
    
    # calculate relative risk
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    # estimate lives saved per capita.
    ls <- sum( heat_mort2044 * prop.yng * (1-rr) ) + sum(heat_mort4574 * prop.old * (1-rr) )
    
  } else if(mode=="cycling"){
    
    # calculate the proportion of the population young and old.
    prop.yng = sum(population[20:44,"pop"]) / sum(population[20:74,"pop"])
    prop.old = sum(population[45:64,"pop"]) / sum(population[20:64,"pop"]) 
    
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
                      vsl = inputs$vsl,
                      select.age.dist = NA, # age distribution
                      gen_pop = inputs$gen_pop,                    # population each age
                      rr_walk_lit = params$rr_walk_lit,            # relative risk walking
                      rr_cycle_lit = params$rr_cycle_lit,          # relative risk cycling
                      ref_duration_walk = params$ref_duration_walk,# reference duration walking
                      ref_duration_cycle = params$ref_duration_cycle, # reference duration cycling
                      max_rr_walk = params$max_rr_walk,             # max risk reduction walking
                      max_rr_cycle = params$max_rr_cycle,
                      m_lt = inputs$gbd_mort                       # gbd lifetables
                      ){
  
  if(mode=="walking"){ 
    
    rr <- max(1 - (1-rr_walk_lit) * (add.mins/ ref_duration_walk), max_rr_walk)
    
  } else if(mode=="cycling"){ 
    
    rr <- max(1 - (1-rr_cycle_lit) * (add.mins/ ref_duration_cycle), max_rr_cycle)
    
  } else { rr <- "broken"
  }
  
  prop.pop <- gen_pop[gen_pop$age %in% select.age.dist,"All"] / sum(gen_pop[gen_pop$age %in% select.age.dist,"All"])  
  #prop.pop <- (gen_pop[country,5:15]*select.age.dist) / sum(gen_pop[country,5:15]*select.age.dist)  # calculates percent in each age group
  
  ls  <- sum(m_lt[m_lt$age %in% select.age.dist,c("Female","Male")] *            # mortality rate for each age and sex
                (1-rr) *                                                         # relative risk reduction.
                prop.pop)                                                        # proportion of population in each year
  
 
  mb <- ls * vsl * n_people
  
  return(mb)
  
  }


