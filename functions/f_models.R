# Model functions for the 4 different models

#f.vsl

f.vsly.mb <- function(n_people, add.mins, country, mode, select.age.dist){
  
  if(mode=="walking"){   rr <- max(1 - (1-inputs$rr_walk_lit) * (add.mins/ inputs$ref_duration_walk), inputs$max_rr_walk) 
  } else if(mode=="cycling"){ rr <- max(1 - (1-inputs$rr_cycle_lit) * (add.mins/ inputs$ref_duration_cycle), inputs$max_rr_cycle)
  } else { rr <- "broken"
  }
  
  prop.pop <- (gen_pop[country,5:15]*select.age.dist) / sum(gen_pop[country,5:15]*select.age.dist)  # calculates percent in each age group
  
  lys  <- sum(who_mort[country,5:15] * (1-rr) * dlyr[country,5:15] * prop.pop)                    # calculates life years saved by change
 
  mbpc   <- lys * heat_data[country,"VSLY"]                                     # calculates monetary benefit per person
  
  return(mbpc * n_people)
  
  }



f.vsl.1g <- function(n_people, add.mins, country, mode, select.age.dist){
  
  if(mode=="walking"){   rr <- max(1 - (1-inputs$rr_walk_lit) * (add.mins/ inputs$ref_duration_walk), inputs$max_rr_walk) 
  } else if(mode=="cycling"){ rr <- max(1 - (1-inputs$rr_cycle_lit) * (add.mins/ inputs$ref_duration_cycle), inputs$max_rr_cycle)
  } else { rr <- "broken"
  }
  
  if(mode=="walking"){
    ls <- inputs$heat_mort2074 * (1-rr)
  } else if(mode=="cycling"){
    ls <- inputs$heat_mort2064 * (1-rr)
  } else { ls <- "broken"
  }
  
  mbpc <- ls*heat_data[country,"VSL"]
  
  return(mbpc * n_people)
}


f.vsl.2g <- function(n_people, add.mins, country, mode, select.age.dist){
  
  if(mode=="walking"){   rr <- max(1 - (1-inputs$rr_walk_lit) * (add.mins/ inputs$ref_duration_walk), inputs$max_rr_walk) 
  } else if(mode=="cycling"){ rr <- max(1 - (1-inputs$rr_cycle_lit) * (add.mins/ inputs$ref_duration_cycle), inputs$max_rr_cycle)
  } else { rr <- "broken"
  }
  
  prop.pop <- (gen_pop[country,5:15]*select.age.dist) / sum(gen_pop[country,5:15]*select.age.dist)  # calculates percent in each age group
  
  if(mode=="walking"){
  ls <- sum( inputs$heat_mort2044 * prop.pop[1:5] * (1-rr) ) + sum(inputs$heat_mort4574 * prop.pop[6:11] * (1-rr) )
  } else if(mode=="cycling"){
  ls <- sum(inputs$heat_mort2044 * prop.pop[1:5] * (1-rr) )  + sum(inputs$heat_mort4564 * prop.pop[6:11] * (1-rr) )
  } else { ls <- "broken"
  }
  
  mbpc <- ls*heat_data[country,"VSL"]
  
  return(mbpc * n_people)
}


f.vsl.11g <- function(n_people, add.mins, country, mode, select.age.dist){
  
  if(mode=="walking"){   rr <- max(1 - (1-inputs$rr_walk_lit) * (add.mins/ inputs$ref_duration_walk), inputs$max_rr_walk) 
  } else if(mode=="cycling"){ rr <- max(1 - (1-inputs$rr_cycle_lit) * (add.mins/ inputs$ref_duration_cycle), inputs$max_rr_cycle)
  } else { rr <- "broken"
  }
  
  prop.pop <- (gen_pop[country,5:15]*select.age.dist) / sum(gen_pop[country,5:15]*select.age.dist)  # calculates percent in each age group
  
  ls  <- sum(who_mort[country,5:15] * (1-rr) * prop.pop)                    # calculates life years saved by change
  
  mbpc <- ls*heat_data[country,"VSL"]
  
  return(mbpc * n_people)
  }


