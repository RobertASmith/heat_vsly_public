# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  Calculating discounted life expectancies at each age.  
# ====== #

# set-up
rm(list=ls())
#install.packages("data.table")   # install datatable if you don't already have it.
library(data.table)

#------------------#
#   LOAD DATA      #
#------------------#
# load necessary data (heat data for countries and iso codes, lifetables for life expectancy/mortality rates)

# heat_data source: HEAT team
heat_data <- read.csv("cleandata/heat_data.csv",stringsAsFactors = F)
# gbd lifetables source: global burden of disease here: http://www.healthdata.org/research-article/global-regional-and-national-age-sex-specific-mortality-and-life-expectancy-1950
dt        <- rbind(fread("rawdata/gbd_lifetables_2017_fmle.csv"),fread("rawdata/gbd_lifetables_2017_male.csv"))
# remove georgia USA from dataset so doesn't get mixed up with European country
dt        <- dt[location_id != 533]
# identify which countries are missing from data
#heat_data$DisplayString[is.na(match(heat_data$DisplayString,unique(dt[,location_name])))] # monoco and san marino don't match

#------------------#
# SETUP DTA/PARAMS #
#------------------#

params <- list() # initialise list
params$ages <- 1:109 # ages from 1 to 109
params$countries <- heat_data$DisplayString[heat_data$DisplayString %in% unique(dt[,location_name])] # only countries in dataset.
params$ISOCode   <- heat_data$ISO_Code[heat_data$DisplayString %in% unique(dt[,location_name])] # same for ISO codes.
params$measures  <- c("Probability of death","Life expectancy")
params$d_r       <- 0

# filter data.table to include only mortality rates for ages 1-109.
dt <- dt[measure_name == "Probability of death" & age_group_name %in% params$ages & location_name %in% params$countries,
         .(age_group_name,val), 
         by= .(location_name,sex_name,measure_name)]

# save this file for use in analysis 
write.csv(x = dt, file = "cleandata/gbd17_mortrates.csv")

#------------------#
#   DLYR FUNCTION  #
#------------------#

f_calc_dlyr <- function(country = "Georgia", 
                        sex = "Male", 
                        dt_lifetables = dt, 
                        d_r = params$d_r){

  # create a vector of survival probabilities for each age for country.
v_ps <- (1-dt_lifetables[location_name == country & sex_name == sex, val])
  
  # create empty square matrices for discount rates and life years remaining
m_dr <- m_lyr <- matrix(0,nrow = length(v_ps),ncol = length(v_ps))

  # fill matrix by column then by row, years below age are zero, expected life above age >1.
for (a in 1:length(v_ps)){
  
  for(x in 1:length(v_ps)){
    if(x>=a){
      m_lyr[x,a] <- prod(v_ps[a:x])
    }else{
      m_lyr[x,a] <- 0
    }
  }
}

# fill discounted matrix based upon age too (along columns)
for(a in 1:ncol(m_lyr)){
  m_dr[a:nrow(m_lyr),a] <- 1/(1+d_r)^(0:(nrow(m_lyr)-a))
}

# multiply life years remainig by discount rate
m_dlyr <- m_lyr * m_dr

# use column sums to calculate discounted expected life years remaining for each age.
return(colSums(m_dlyr))

}

#------------------#
#   CALC DLYR M/F  #
#------------------#

# loop through each country storing the results in a list.
results <- list()
results$male <- matrix(data = NA, nrow= 109, ncol = length(params$countries),dimnames = list(1:109,params$countries))
for(c in params$countries){
  results$male[,c] <- f_calc_dlyr(country = c,sex = "Male")
}

# loop through each country storing the results in a list.
results$fmle <- matrix(data = NA, nrow= 109, ncol = length(params$countries),dimnames = list(1:109,params$countries))
for(c in params$countries){
  results$fmle[,c] <- f_calc_dlyr(country = c,sex = "Female")
}

# storing data as one long dataset
df_dlyr <- rbind(melt(data = results$male,value.name = "dlyr"),
              melt(data = results$fmle,value.name = "dlyr"))
df_dlyr$sex = rep(c("male","fmle"), each= nrow(df_dlyr)/2)
dimnames(df_dlyr) <- list(1:nrow(df_dlyr),c("age","DisplayString","dlyr","sex"))
df_dlyr$ISO_Code  <- heat_data$ISO_Code[match(df_dlyr$DisplayString,heat_data$DisplayString)]

#------------------#
# SAVE RESULTS CSV #
#------------------#

# write to csv
write.csv(x = df_dlyr,file = "cleandata/dlyr.csv")








#====== archive 
#
#v_ps <- (1-dt_lifetables[location_name == country & sex_name == sex, val])
#
#
## calculate vector of lyr
#temp2 <- c() # ages
#
#
#for(a in 1:109){
#  
#  temp <- c()
#  temp[1] <- v_ps[a]
#  for(x in (a+1):109){ # internal
#    temp[x-a] = prod(v_ps[a:x])
#  }
#  
#  # external
#  temp2[a] <- sum(temp * 1/(1+d_r)^(1:(109-a)))
#}
#
#plot(temp2)
#
#v_ps[108]
#d_r <- 0
#
#dlyr <- list()
#
#for(x in 1:109){
#  
#  # inner loop to calculate all the loops
#  temp <- c()
#  temp[1] <- v_ps[x]
#  for(a in (x+1):109){
#    temp[a] <- prod(v_ps[1:a])
#  }
#  
#  dlyr[x] <- sum(temp * 1/(1+d_r)^(1:(109-x+1)))
#}
#
#
#v_pa * v_ps
#
#
## Create function which takes inputs: country & sex and returns VSLY.
#f_calc_dlyr <- function(country = "United Kingdom", 
#                        sex = "Male", 
#                        dt_lifetables = dt, 
#                        d_r = params$d_r){
#  
#  # create vector of survival probabilities from lifetables from 1-109
#  v_ps <- (1-dt_lifetables[location_name == country & sex_name == sex, val])
#  
#  # work out the proportion of the population alive at each age.
#  v_pa <- c()
#  for(a in 1:109){
#    v_pa[a] = prod(v_ps[1:a])  
#  }
#  
#  # work out discounted expected life years from 1:109
#  # expected life years at age 50
#  v_pa
#  
#  v_delyr <- c()
#  for(a in 1:109){ # loop through values
#    v_delyr[a] = sum(v_pa[a:109] * 1/(1+d_r)^(1:(109-a+1))) # sum of the discounted life years remaining at age x.
#    
#  }
#  
#  
#  dt[measure_name == "Life expectancy" & age_group_name %in% params$ages & location_name %in% params$countries,
#     .(age_group_name,val), 
#     by= .(location_name,sex_name,measure_name)]
#  
#  
#  
#  
#  
#  
#  
#  plot(v_pa)
#  temp <- list()
#  for(a in 1:100){
#    temp[a] <- v_ps[a] * sum(v_ps[1:a])
#  }
#  
#  # calculate a vector of expected life years from 1 to x.
#  v_ely <- c()
#  v_ely[1] <- v_ps[1]
#  
#  for(x in 2:109){
#    v_ely[x] = v_ely[x-1] * v_ps[x] # product of the survival probabilities.
#  }
#  
#  # calculate discounted expected life years from 1:109
#  v_delyr <- c()
#  for(x in 1:109){ # loop through values
#    v_delyr[x] = sum(v_ely[x:109] * 1/(1+d_r)^(1:(109-x+1))) # sum of the discounted life years remaining at age x.
#    
#  }
#  
#  return(v_delyr)                # return this vector
#  
#}
#
#