# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script undertakes the core analysis. It is stand-alone.  
# ====== #
rm(list =ls())

#-----------------------#
# INSTALL PACKAGES (Function courtesty of Paul Schneider)
#-----------------------#

f_install_n_load <- function(packages){    # install & load package.
  
  for(package in packages){ # for each package
    
    if(package %in% rownames(installed.packages()) == FALSE) {  # if the package is not already installed.
      
      print(paste("installing",package))  # print the necessary package.
      
      install.packages(package)           # install necessary package
    }
    eval(parse(text=paste('require(',package,')'))) # if it is already installed then load it.
  }
}

required_packages = c('knitr','tidyverse','rworldmap',
                      'reshape2','ggplot2','dplyr',
                      'tidyr','mc2d','ggrepel',
                      'gridExtra','kableExtra','rgeos',
                      'flextable','viridis','readxl','magrittr', 'data.table')

f_install_n_load(required_packages)

#-----------------------#
#     SOURCE FUNCTIONS

source("functions/f_models.R")
#source("functions/f_dlyr.R")

#f_get_ages <- function(start,end){
#  template = rep(0,55)
#  template[start:end] <- 1
#  return(template)
#}

#-------------------------# 
#         LOAD DATA      
#-------------------------#

# heat_data & gen_pop cleaned in 'clean_load.R'.
# dlyr cleaned in 'calculate_dlyr.R'
# vsly cleaned in 'calculate_vsly.R'
# gbd_lt cleaned in 'calculate_dlyr.R'

heat_data   <- read.csv("cleandata/heat_data.csv",stringsAsFactors = F,row.names = 2) # heat data download from cleaned data file
#gen_pop     <- read.csv("cleandata/gen_pop.csv",stringsAsFactors = F,row.names = 1)
dlyr        <- read.csv("cleandata/dlyr.csv",stringsAsFactors = F,row.names = 1)
vsly        <- read.csv("cleandata/vsly.csv",stringsAsFactors = F,row.names = 5)
gbd_lt      <- read.csv("cleandata/gbd17_mortrates.csv",stringsAsFactors = F,row.names = 1)
gbd_pop     <- read.csv("cleandata/gbd_pop.csv",stringsAsFactors = F,row.names = 1)
#perc_fmle   <- read.csv("cleandata/perc_fmle.csv",stringsAsFactors = F,row.names = 1)
# who_mort  <- read.csv("cleandata/who_mort.csv",stringsAsFactors = F,row.names = 1)
gbd_heatmort <- read.csv("cleandata/gbd17_heatmorts.csv",stringsAsFactors = F,row.names = 2)

#-------------------------# 
#        INITIALISE       #
#-------------------------#

# empty list
params <- list()

params$heat_or_gbd = "gbd"

# vector of countries
params$v_countries = rownames(heat_data)[rownames(heat_data) %in% rownames(vsly)]

# number of countries
params$n_countries = length(params$v_countries)

# add in heat age distributions - start at age 20 finish at 74.
params$age_dist <- list()
params$age_dist$age20to44 = 20:44
params$age_dist$age20to74 = 20:74
params$age_dist$age20to64 = 20:64
params$age_dist$age45to64 = 45:64
params$age_dist$age45to74 = 45:74

# results names for columns
params$v_resultnames <- c( "vsly_w2074","heat1_w2074","heat2_w2074","vsl55_w2074",
                           "vsly_w2044","heat1_w2044","heat2_w2044","vsl55_w2044",
                           "vsly_w4574","heat1_w4574","heat2_w4574","vsl55_w4574",
                           "vsly_c2064","heat1_c2064","heat2_c2064","vsl55_c2064",
                           "vsly_c2044","heat1_c2044","heat2_c2044","vsl55_c2044",
                           "vsly_c4564","heat1_c4564","heat2_c4564","vsl55_c4564")

# create results table
results <- matrix(data = NA,
                  nrow = length(params$v_countries),
                  ncol = length(params$v_resultnames),
                  dimnames = list(params$v_countries,params$v_resultnames)) # empty results table

# set model assumption parameters
params$age_min = 20               # Minimum age assumed to be 20
params$age_max = 74               # Maximum age assumed to be 74
params$extra   = 10               # Case study 90mins walking
params$prop_exp=0.001             # proportion exposed fixed
params$rr_walk_lit =  0.886       # risk ratio as fixed in HEAT
params$rr_cycle_lit = 0.9         # relative risk cycling in the literature
params$ref_duration_walk = 168    # ref duration walking as fixed in HEAT
params$ref_duration_cycle = 100   # reference duration of cycling in the literature
params$max_rr_cycle = 0.55        # min relative risk from cycling (max effect)
params$max_rr_walk = 0.7          # min relative risk from walking (max effect)

# create a new list of COUNTRY SPECIFIC model inputs, empty to begin with
inputs <- list()
inputs$vsl           = NULL         # country specific VSL
inputs$vsly          = NULL         # country specific VSLY
inputs$heat_mort2074 = NULL         # mortality rate for heat 20-74 model
inputs$heat_mort2044 = NULL         # mortality rate for heat_2 20-44 year olds
inputs$heat_mort4574 = NULL         # mortality rate for heat_2 45-74 year olds
inputs$heat_mort2064 = NULL         # mortality rate for heat 20-64 model.
inputs$heat_mort4564 = NULL         # mortality rate 45-64.
inputs$gen_pop       = NULL
inputs$perc_fmle     = NULL
inputs$gbd_mort      = NULL

#-------------------------# 
#        RUN MODEL        #
#-------------------------#

for(c in 1:params$n_countries){
  
  country <- params$v_countries[c] # set country
  
  # COUNTRY SPECIFIC MODEL PARAMETERS
  inputs$vsl             <-   heat_data[country,"VSL"]                   # VSL taken from HEAT Gitbhub dataset.
  inputs$vsly            <-   vsly[country,"vsly_all"]                   # VSLY estimated in previous analysis.   
  
  if(params$heat_or_gbd == "heat"){
  
  inputs$heat_mort2074   <-   heat_data[country,"age2074"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort2044   <-   heat_data[country,"age2044"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort4574   <-   heat_data[country,"age4574"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort2064   <-   heat_data[country,"age2064"] /100000       # baseline risk from HEAT Github
  inputs$heat_mort4564   <-   heat_data[country,"age4564"] /100000       # baseline risk from HEAT Github
  
    }else if(params$heat_or_gbd == "gbd"){
    
    inputs$heat_mort2074   <-   gbd_heatmort[country,"mort20_74"]        # baseline risk from GBD & clean_load.R
    inputs$heat_mort2044   <-   gbd_heatmort[country,"mort20_44"]        # baseline risk from GBD & clean_load.R
    inputs$heat_mort4574   <-   gbd_heatmort[country,"mort45_74"]        # baseline risk from GBD & clean_load.R
    inputs$heat_mort2064   <-   gbd_heatmort[country,"mort20_64"]        # baseline risk from GBD & clean_load.R
    inputs$heat_mort4564   <-   gbd_heatmort[country,"mort45_64"]        # baseline risk from GBD & clean_load.R
    
    
  }else{print("fail")} 
  
  
  # population at each age from 1 to 94
  inputs$gen_pop         <-   gbd_pop[gbd_pop$ISO_Code == country,c("age","All")]
  # discounted life years remaining from 1 to 109
  inputs$dlyr            <-   dlyr[dlyr$ISO_Code == country,c("age","dlyr_m","dlyr_f")]
  # percentage female from 1 to 94
  inputs$perc_fmle       <-   gbd_pop[gbd_pop$ISO_Code == country,c("age","perc_fmle")]
  # life table from 1 to 100
  inputs$gbd_mort        <-   gbd_lt[gbd_lt$ISO_Code == country,c("age","Female","Male")]
  
  
  # RUN FUNCTIONS 
  
  # walking 20-74
  results[country,"vsly_w2074"]  <- f.vsly.mb(country = country,
                                              n_people = 1,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age20to74)
  
  results[country,"heat1_w2074"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking")
  
  results[country,"heat2_w2074"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age20to74)
  
  results[country,"vsl55_w2074"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = params$age_dist$age20to74)
  
  # walking 20-44
  results[country,"vsly_w2044"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age20to44)
  
  results[country,"heat1_w2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking")
  
  results[country,"heat2_w2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age20to44)
  
  results[country,"vsl55_w2044"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = params$age_dist$age20to44)
  
  # walking 45-74
  results[country,"vsly_w4574"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age45to74)
  
  results[country,"heat1_w4574"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "walking")
  
  results[country,"heat2_w4574"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              select.age.dist = params$age_dist$age45to74)
  
  results[country,"vsl55_w4574"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               select.age.dist = params$age_dist$age45to74)
  
  # cycling 20-64
  results[country,"vsly_c2064"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age20to64)
  
  results[country,"heat1_c2064"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling")
  
  results[country,"heat2_c2064"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age20to64)
  
  results[country,"vsl55_c2064"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = params$age_dist$age20to64)
  
  # walking 20-44
  results[country,"vsly_c2044"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age20to44)
  
  results[country,"heat1_c2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling")
  
  results[country,"heat2_c2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age20to44)
  
  results[country,"vsl55_c2044"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = params$age_dist$age20to44)
  
  # walking 45-74
  results[country,"vsly_c4564"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age45to64)
  
  results[country,"heat1_c4564"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             mode = "cycling")
  
  results[country,"heat2_c4564"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              select.age.dist = params$age_dist$age45to64)
  
  results[country,"vsl55_c4564"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               select.age.dist = params$age_dist$age45to64)
  
}
  
  write.csv(results, file="outputs/Results.csv") 
  
#------------------------#
#  ADDITIONAL ANALYSIS   #
#------------------------#

  # set country to Latvia
  temp.country <- "LVA"   
  
  # Create empty results table:
  age.results <- matrix(data = NA,nrow = 100,ncol = 4)
  colnames(age.results) <- c("vsly","heat1", "heat2","vsl55")
  age.results.c <- age.results # age results c to be used for cycling
  
  # loop through ages
  for(age in 20:74){
    
    age.results[age,]  <- c(f.vsly.mb(country = temp.country,
                                      n_people = 1,
                                      mode = "walking",
                                      select.age.dist = age),
                            f.vsl.1g(n_people = 1,
                                     add.mins = 10,
                                     country = temp.country,
                                     mode = "walking"),
                            f.vsl.2g(n_people = 1,
                                     add.mins = 10,
                                     country = temp.country,
                                     mode = "walking",
                                     select.age.dist = age),
                            f.vsl.55g(n_people = 1,
                                      add.mins = 10,
                                      country = temp.country,
                                      mode = "walking",
                                      select.age.dist = age)
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
    #                        f.vsl.55g(n_people = 1,
    #                                  add.mins = 10,
    #                                  country = temp.country,
    #                                  mode = "cycling",
    #                                  select.age.dist = input.agedist)
    #  )
    
  } # end age loop
write.csv(x = age.results,file = "outputs/age_specific_results.csv" ) 
