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

f_install_n_load(c('knitr','tidyverse','rworldmap','dplyr',
                   'reshape2','ggplot2','dplyr',
                   'tidyr','mc2d','ggrepel',
                   'gridExtra','kableExtra','rgeos',
                   'flextable','viridis','readxl','magrittr', 
                   'data.table'))

# Source Functions ----

source("functions/f_models.R")
#source("functions/f_dlyr.R")

# Load Data ----      

# heat_data & gen_pop cleaned in 'clean_load.R'.
# dlyr cleaned in 'calculate_dlyr.R'
# vsly cleaned in 'calculate_vsly.R'
# gbd_lt cleaned in 'calculate_dlyr.R'

# to be merged
heat_data   <- read.csv("cleandata/heat_data.csv",stringsAsFactors = F) # heat data download from cleaned data file
gbd_heatmort <- read.csv("cleandata/gbd17_heatmorts.csv",stringsAsFactors = F)
vsly        <- read.csv("cleandata/vsly.csv",stringsAsFactors = F)

# to be merged
dlyr        <- read.csv("cleandata/dlyr.csv",stringsAsFactors = F,row.names = 1)
gbd_lt      <- read.csv("cleandata/gbd17_mortrates.csv",stringsAsFactors = F,row.names = 1)
gbd_pop     <- read.csv("cleandata/gbd_pop.csv",stringsAsFactors = F,row.names = 1)

# deprecated datasets
#perc_fmle   <- read.csv("cleandata/perc_fmle.csv",stringsAsFactors = F,row.names = 1)
# who_mort  <- read.csv("cleandata/who_mort.csv",stringsAsFactors = F,row.names = 1)
#gen_pop     <- read.csv("cleandata/gen_pop.csv",stringsAsFactors = F,row.names = 1)


# merging group mortrates, vsl and vsly datasets.
merged_data <- merge(x = gbd_heatmort[,c("ISO_Code","mort20_74","mort20_64","mort20_74","mort20_44","mort45_74","mort45_64")],
                y = vsly[,c("ISO_Code","vsly_m","vsly_f","vsly_all")], by = "ISO_Code") %>% 
               merge(heat_data[,c("ISO_Code","VSL")]) %>% as.data.table()

merged_lt <-  merge(x = dlyr[,c("ISO_Code","age","dlyr_m","dlyr_f")],
                    y = gbd_lt[,c("ISO_Code","age","Female","Male")], 
                    by = c("ISO_Code","age")) %>% 
              merge(gbd_pop[,c("ISO_Code","age","Female","Male")], by = c("ISO_Code","age")) %>%
              filter(age>19 & age <75) %>% 
              arrange(ISO_Code,age) %>%
              as.data.table()

colnames(merged_lt) = c("ISO_Code","age", "dlyr_m","dlyr_f","mr_f","mr_m","pop_f","pop_m")

merged_lt <- merged_lt[ ,.(mr   = (mr_f * pop_f + mr_m * pop_m)/ sum(pop_f + pop_m),
                           dlyr = (dlyr_f * pop_f + dlyr_m * pop_m)/sum(pop_f + pop_m),
                           pop  =  pop_f + pop_m), 
                        by = .(ISO_Code,age)]

# remove all previous (now) unnecessary datasets
rm(heat_data,gbd_heatmort,vsly,dlyr,gbd_lt,gbd_pop)




# Initialise Model ----

# empty list
params <- list()

# vector of 51 countries
params$v_countries = merged_data$ISO_Code %>% unique

# number of countries
params$n_countries = length(params$v_countries)

# add in heat age distributions - start at age 20 finish at 74.

params$age20to44 = 20:44
params$age20to74 = 20:74
params$age20to64 = 20:64
params$age45to64 = 45:64
params$age45to74 = 45:74

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
params$extra   = 10               # Case study extra 10mins duration of daily activity (walking or cycling)
params$rr_walk_lit =  0.886       # risk ratio as fixed in HEAT
params$rr_cycle_lit = 0.9         # relative risk cycling in the literature
params$ref_duration_walk = 168    # ref duration walking as fixed in HEAT
params$ref_duration_cycle = 100   # reference duration of cycling in the literature
params$max_rr_cycle = 0.55        # min relative risk from cycling (max effect)
params$max_rr_walk = 0.7          # min relative risk from walking (max effect)


#-------------------------# 
#        RUN MODEL        #
#-------------------------#

for(c in 1:params$n_countries){
  
  country <- params$v_countries[c] # set country
  
  # COUNTRY SPECIFIC MODEL PARAMETERS
  inputs$vsl             <-   merged_data[ISO_Code == country,VSL]              # VSL taken from HEAT Gitbhub dataset.
  inputs$vsly            <-   merged_data[ISO_Code == country,vsly_all]         # VSLY estimated in previous analysis.   
  inputs$heat_mort2074   <-   merged_data[ISO_Code == country,mort20_74]        # baseline risk from GBD & clean_load.R
  inputs$heat_mort2044   <-   merged_data[ISO_Code == country,mort20_44]        # baseline risk from GBD & clean_load.R
  inputs$heat_mort4574   <-   merged_data[ISO_Code == country,mort45_74]        # baseline risk from GBD & clean_load.R
  inputs$heat_mort2064   <-   merged_data[ISO_Code == country,mort20_64]        # baseline risk from GBD & clean_load.R
  inputs$heat_mort4564   <-   merged_data[ISO_Code == country,mort45_64]        # baseline risk from GBD & clean_load.R

    
  # RUN FUNCTIONS 
  
  # walking 20-74
  results[country,"vsly_w2074"]  <- f.vsly.mb(country = country,
                                              n_people = 1,
                                              mode = "walking",
                                              select.age.dist = 20:74,
                                              add.mins = 10,
                                              merged_lt = merged_lt,
                                              merged_data = merged_data)
  
  results[country,"heat1_w2074"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             merged_data = merged_data,
                                             country = country,
                                             mode = "walking")
  
  results[country,"heat2_w2074"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:74)
  
  results[country,"vsl55_w2074"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               merged_data = merged_data,
                                               merged_lt = merged_lt,
                                               select.age.dist = 20:74)
  
  
  
  
  # walking 20-44
  results[country,"vsly_w2044"]  <- f.vsly.mb(country = country,
                                              n_people = 1,
                                              mode = "walking",
                                              select.age.dist = 20:44,
                                              add.mins = 10,
                                              merged_lt = merged_lt,
                                              merged_data = merged_data)
  
  results[country,"heat1_w2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             merged_data = merged_data,
                                             mode = "walking")
  
  results[country,"heat2_w2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:44)
  
  results[country,"vsl55_w2044"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               merged_data = merged_data,
                                               merged_lt = merged_lt,
                                               select.age.dist = 20:44)
  
  
  # walking 45-74
  results[country,"vsly_w4574"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 45:74)
  
  results[country,"heat1_w4574"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             merged_data = merged_data,
                                             mode = "walking")
  
  results[country,"heat2_w4574"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "walking",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 45:74)
  
  results[country,"vsl55_w4574"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "walking",
                                               merged_data = merged_data,
                                               merged_lt = merged_lt,
                                               select.age.dist = 45:74)
  
  
  # cycling 20-64
  results[country,"vsly_c2064"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:64)
  
  results[country,"heat1_c2064"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             merged_data = merged_data,
                                             mode = "cycling")
  
  results[country,"heat2_c2064"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:64)
  
  results[country,"vsl55_c2064"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               merged_data = merged_data,
                                               merged_lt = merged_lt,
                                               select.age.dist = 20:64)
  
  # walking 20-44
  results[country,"vsly_c2044"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:44)
  
  results[country,"heat1_c2044"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             merged_data = merged_data,
                                             mode = "cycling")
  
  results[country,"heat2_c2044"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 20:44)
  
  results[country,"vsl55_c2044"]  <- f.vsl.55g(n_people = 1,
                                               add.mins = 10,
                                               country = country,
                                               mode = "cycling",
                                               merged_data = merged_data,
                                               merged_lt = merged_lt,
                                               select.age.dist = 20:44)
  
  # walking 45-74
  results[country,"vsly_c4564"]  <- f.vsly.mb(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 45:64)
  
  results[country,"heat1_c4564"] <- f.vsl.1g(n_people = 1,
                                             add.mins = 10,
                                             country = country,
                                             merged_data = merged_data,
                                             mode = "cycling")
  
  results[country,"heat2_c4564"]  <- f.vsl.2g(n_people = 1,
                                              add.mins = 10,
                                              country = country,
                                              mode = "cycling",
                                              merged_data = merged_data,
                                              merged_lt = merged_lt,
                                              select.age.dist = 45:64)
  
  results[country,"vsl55_c4564"]  <-  f.vsl.55g(n_people = 1,
                                                add.mins = 10,
                                                country = country,
                                                mode = "cycling",
                                                merged_data = merged_data,
                                                merged_lt = merged_lt,
                                                select.age.dist = 45:64)
  
}

# save to file named depending on heat or gbd
  write.csv(x = results,file = "outputs/gbd_results.csv" )

  
  
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
  
  if(params$heat_or_gbd == "heat"){
    write.csv(x = age.results,file = "outputs/heat_age_results.csv" ) 
    }else if(params$heat_or_gbd == "gbd"){
      write.csv(x = age.results,file = "outputs/gbd_age_results.csv" )
        }else{print("fail")}