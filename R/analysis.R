# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script undertakes the core analysis. It is stand-alone.  
# ====== #
rm(list =ls())

# source all functions needed.
source("functions/f_install_n_load.R")
source("functions/f_models.R")
#source("functions/f_dlyr.R")


# run clean and load files.

#source(file = "R/clean_load.R") # cleans all raw data and loads necessary data.

heat_data   <- read.csv("cleandata/heat_data.csv",stringsAsFactors = F,row.names = 2) # heat data download from cleaned data file
gen_pop     <- read.csv("cleandata/gen_pop.csv",stringsAsFactors = F,row.names = 1)
dlyr        <- read.csv("cleandata/dlyr.csv",stringsAsFactors = F,row.names = 1)
vsly        <- read.csv("cleandata/vsly.csv",stringsAsFactors = F,row.names = 1)
gbd_lt      <- read.csv("cleandata/gbd17_mortrates.csv",stringsAsFactors = F,row.names = 1)
# who_mort  <- read.csv("cleandata/who_mort.csv",stringsAsFactors = F,row.names = 1)

v_countries = rownames(heat_data)

#source(file = "R/calc_dependencies.R")


# CREATE RESULTS TABLE
v_resultnames <- c( "vsly_w2074","heat1_w2074","heat2_w2074","vsl11_w2074",
                    "vsly_w2044","heat1_w2044","heat2_w2044","vsl11_w2044",
                    "vsly_w4574","heat1_w4574","heat2_w4574","vsl11_w4574",
                    "vsly_c2064","heat1_c2064","heat2_c2064","vsl11_c2064",
                    "vsly_c2044","heat1_c2044","heat2_c2044","vsl11_c2044",
                    "vsly_c4564","heat1_c4564","heat2_c4564","vsl11_c4564")

results <- matrix(data = NA,
                  nrow = length(v_countries),
                  ncol = length(v_resultnames),
                  dimnames = list(v_countries,v_resultnames)) # empty results table

# CREATE AGE DISTRIBUTION OPTIONS
l_age_dist <- list(age20to44 = c(1,1,1,1,1,0,0,0,0,0,0),
                   age20to74 = c(1,1,1,1,1,1,1,1,1,1,1),
                   age20to64 = c(1,1,1,1,1,1,1,1,1,1,0),
                   age45to64 = c(0,0,0,0,0,1,1,1,1,1,0),
                   age45to74 = c(0,0,0,0,0,1,1,1,1,1,1))


####     CREATE TABLE OF VARIABLES   ######
variables <-      c("age_min",               # minimum age in model
                    "age_max",               # maximum age in model
                    "extra",                 # additional mins activity
                    "prop_exp",              # proportion exposed
                    "heat_mort2074",         # mortality rate for heat 20-74 model
                    "heat_mort2044",         # mortality rate for heat_2 20-44 year olds
                    "heat_mort4574",         # mortality rate for heat_2 45-74 year olds
                    "heat_mort4564",         # mortality rate for heat_2 45-64 year olds
                    "heat_mort2064",         # mortality rate for heat 20-64 model. 
                    "rr_walk_lit",           # relative risk in the literature
                    "rr_cycle_lit",          # relative risk cycling literature
                    "ref_duration_walk",     # reference duration for walking.
                    "ref_duration_cycle",    # reference duration for cycling.
                    "vsly",                  # country specific VSLY
                    "vsl",                   # country specific VSL
                    "max_rr_walk",           # most possible benefit, lowest rr
                    "max_rr_cycle")          # most possible benefit, lowest rr         

inputs <- list(age_min = 20,     # Minimum age assumed to be 20
               age_max = 74,     # Maximum age assumed to be 74
               extra   = 10,     # Case study 90mins walking
               prop_exp=0.001,             # proportion exposed fixed
               rr_walk_lit =  0.886,             # risk ratio as fixed in HEAT
               rr_cycle_lit = 0.9,
               ref_duration_walk = 168,    # ref duration walking as fixed in HEAT
               ref_duration_cycle = 100,
               max_rr_cycle = 0.55,
               max_rr_walk = 0.7,
               vsl = NULL,
               vsly = NULL,
               heat_mort2074=NULL,
               heat_mort2044=NULL,
               heat_mort4574=NULL,
               heat_mort2064=NULL,
               heat_mort4564=NULL)


#inputs <- matrix(NA,
#                 nrow = length(variables), 
#                 ncol = 1,
#                 dimnames = list(variables,"value"))                 # input matrix creation.
#
#### INDEPENDENT ###   therefore outside of loop
#inputs["age_min" ,  "value"]          <-   20                # Minimum age assumed to be 20
#inputs["age_max" ,  "value"]          <-   74                # Maximum age assumed to be 74
#inputs["extra"  ,  "value"]           <-   10                # Case study 90mins walking
#inputs["prop_exp"   ,  "value"]       <-   0.001             # proportion exposed fixed
#inputs["rr_walk_lit",  "value"]       <-   0.886             # risk ratio as fixed in HEAT
#inputs["rr_cycle_lit", "value"]       <-   0.9
#inputs["ref_duration_walk", "value"]  <-   168               # ref duration walking as fixed in HEAT
#inputs["ref_duration_cycle", "value"] <-   100
#inputs["max_rr_cycle","value"]        <-   0.55
#inputs["max_rr_walk","value"]         <-   0.7

##### RUN MODEL FOR ALL COUNTRIES  #####
for(c in 1:length(v_countries)){
  country <- v_countries[c]
  
  # COUNTRY SPECIFIC
  inputs$vsl             <-   heat_data[country,"VSL"]                   # VSL taken from HEAT Gitbhub dataset.
  inputs$vsly            <-   heat_data[country,"VSLY"]                  # VSLY estimated in previous analysis.   
  inputs$heat_mort2074   <-   heat_data[country,"age2074"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort2044   <-   heat_data[country,"age2044"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort4574   <-   heat_data[country,"age4574"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort2064   <-   heat_data[country,"age2064"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort4564   <-   heat_data[country,"age4564"] /100000       # baseline risk from HEAT Github
  
  # Use predefined functions to estimate monetary benefit given population characteristics.
  # walking 20-74
  results[country,"vsly_w2074"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age20to74)
  
  results[country,"heat1_w2074"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking",
                                             select.age.dist = l_age_dist$age20to74)
  
  results[country,"heat2_w2074"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age20to74)
  
  results[country,"vsl11_w2074"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = l_age_dist$age20to74)
  
  # walking 20-44
  results[country,"vsly_w2044"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age20to44)
  
  results[country,"heat1_w2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking",
                                             select.age.dist = l_age_dist$age20to44)
  
  results[country,"heat2_w2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age20to44)
  
  results[country,"vsl11_w2044"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = l_age_dist$age20to44)
  
  # walking 45-74
  results[country,"vsly_w4574"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age45to74)
  
  results[country,"heat1_w4574"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking",
                                             select.age.dist = l_age_dist$age45to74)
  
  results[country,"heat2_w4574"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = l_age_dist$age45to74)
  
  results[country,"vsl11_w4574"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = l_age_dist$age45to74)
  
  # cycling 20-64
  results[country,"vsly_c2064"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age20to64)
  
  results[country,"heat1_c2064"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling",
                                             select.age.dist = l_age_dist$age20to64)
  
  results[country,"heat2_c2064"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age20to64)
  
  results[country,"vsl11_c2064"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = l_age_dist$age20to64)
  
  # walking 20-44
  results[country,"vsly_c2044"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age20to44)
  
  results[country,"heat1_c2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling",
                                             select.age.dist = l_age_dist$age20to44)
  
  results[country,"heat2_c2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age20to44)
  
  results[country,"vsl11_c2044"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = l_age_dist$age20to44)
  
  # walking 45-74
  results[country,"vsly_c4564"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age45to64)
  
  results[country,"heat1_c4564"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling",
                                             select.age.dist = l_age_dist$age45to64)
  
  results[country,"heat2_c4564"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = l_age_dist$age45to64)
  
  results[country,"vsl11_c4564"]  <- f.vsl.11g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = l_age_dist$age45to64)
  
}
  
  write.csv(results, file="outputs/Results.csv") 
  
#------------------------#
#  ADDITIONAL ANALYSIS   #
#------------------------#

# simple example graph for latvia
  
  # set country to latvia
  temp.country <- "LVA"   
  
  # set individual ages from 20 to 74
  ages <- c(rep(NA,19),
            rep(c("20-24","25-29","30-34","35-39","40-44","45-49",
                  "50-54","55-59","60-64","65-69","70-74"),each=5),
            rep(NA,25))
  
  # age distribution, 1s and 0s indicating whethether in specific age group or not.
  input.agedist    <-   matrix(0,nrow = 1,ncol = 11) 
  colnames(input.agedist)<-c("20-24","25-29","30-34","35-39",
                             "40-44","45-49","50-54","55-59",
                             "60-64","65-69","70-74")
  
  # Create results table:
  age.results <- matrix(data = NA,nrow = 100,ncol = 4)
  colnames(age.results) <- c("vsly","heat1", "heat2","vsl11")
  age.results.c <- age.results # age results c to be used for cycling
  
  for(a in 20:74){
    input.agedist[,] <- 0         # initialise age distributions to zeros
    input.agedist[,ages[a]] <- 1  # assign age to appropriate band.
    
    age.results[a,]  <- c(f.vsly.mb(n_people = 1,
                                    add.mins = 10,
                                    country = temp.country,
                                    mode = "walking",
                                    select.age.dist = input.agedist),
                          f.vsl.1g( n_people = 1,
                                    add.mins = 10,
                                    country = temp.country,
                                    mode = "walking",
                                    select.age.dist = input.agedist),
                          f.vsl.2g( n_people = 1,
                                    add.mins = 10,
                                    country = temp.country,
                                    mode = "walking",
                                    select.age.dist = input.agedist),
                          f.vsl.11g(n_people = 1,
                                    add.mins = 10,
                                    country = temp.country,
                                    mode = "walking",
                                    select.age.dist = input.agedist)
    )
    
    # ignore cycling for now.
    #  age.results.c[a,] <- c(f.vsly.mb(n_people = 1,
    #                                  add.mins = 10,
    #                                  country = temp.country,
    #                                  mode = "cycling",
    #                                  select.age.dist = input.agedist),
    #                        f.vsl.1g( n_people = 1,
    #                                  add.mins = 10,
    #                                  country = temp.country,
    #                                  mode = "cycling",
    #                                  select.age.dist = input.agedist),
    #                        f.vsl.2g( n_people = 1,
    #                                  add.mins = 10,
    #                                  country = temp.country,
    #                                  mode = "cycling",
    #                                  select.age.dist = input.agedist),
    #                        f.vsl.11g(n_people = 1,
    #                                  add.mins = 10,
    #                                  country = temp.country,
    #                                  mode = "cycling",
    #                                  select.age.dist = input.agedist)
    #  )
    
  } # end age loop

write.csv(x = age.results,file = "outputs/age_specific_results.csv" ) 
