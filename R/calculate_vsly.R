#===============#
# Calculating the Value of a Statistical Life Year
# Robert Smith (https://github.com/RobertASmith)
# ScHARR, University of Sheffield
# rasmith3@sheffield.ac.uk
#===============#

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
# united kingdom lifetables from ONS here: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/bulletins/nationallifetablesunitedkingdom/latest
#uk_valid  <- read.csv("rawdata/united_kingdom_lifetables.csv")

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

# filter data.table to include only mortality rates for ages 1-109, and life expectancy for 1-109.
dt <- dt[measure_name %in% params$measures & age_group_name %in% params$ages & location_name %in% params$countries,
            .(age_group_name,val), 
            by= .(location_name,sex_name,measure_name)]

#------------------#
#   VSLY FUNCTION  #
#------------------#

# Create function which takes inputs: country & sex and returns VSLY.
f_calc_vsly <- function(country = NA, 
                        sex = NA, 
                        dt_lifetables = dt, 
                        d_r = params$d_r,
                        df_vsl = heat_data){

# create vector of survival probabilities from UK lifetable for males from 1-109
v.ps <- (1-dt_lifetables[measure_name == "Probability of death" & location_name == country & sex_name == sex, val])

# calculate a vector of expected life years from 50 to 109
v.ely <- c()
for(x in 50:109){
  v.ely[x] = prod(v.ps[1:x])
}

# apply half cycle correction, assume additional half year 
v.ely[49] <- 0.5 

  # calculate vector of discounted expected life years from 50 to 109
v.dely <- v.ely[49:109] * 1/(1+d_r)^(0:(109-49))         # apply discount rate, in this case zero but adaptable.

# use equation: VSYL = VSL(50)/sum(dely(50)) where dely is the discounted expected life years.
VSLY = df_vsl$VSL[df_vsl$DisplayString ==country] /sum(v.dely)

return(VSLY)

}

#------------------#
#   CALC VSLY M/F  #
#------------------#

# loop through each country storing the results in a list.
male_results <- list()
for(c in params$countries){
male_results[c] <- f_calc_vsly(country = c,sex = "Male")
}

# loop through each country storing the results in a list.
fmle_results <- list()
for(c in params$countries){
  fmle_results[c] <- f_calc_vsly(country = c,sex = "Female")
}

#------------------#
# SAVE RESULTS CSV #
#------------------#

# store results in cleaned
# <- unlist(male_results,use.names = T)  # unlist the results
df_vsly <- data.frame(vsly_m = unlist(male_results,use.names = T),
                      vsly_f = unlist(fmle_results,use.names = T))     # store in data-frame
df_vsly$vsly_all = (df_vsly$vsly_m + df_vsly$vsly_f)/2                 # take mean of male and female VSLY - since heat doesn't differentiate.
df_vsly$ISO_Code = heat_data$ISO_Code[match(rownames(df_vsly),heat_data$DisplayString)] # add iso-code
df_vsly$DisplayString = heat_data$DisplayString[match(rownames(df_vsly),heat_data$DisplayString)] # and displaystring

# write to csv
write.csv(x = df_vsly,file = "cleandata/vsly.csv" )
  



#### ARCHIVED ###
#
#sum(temp[50:109]) + dt[ measure_name == "Life expectancy" & 
#                          location_name == "United Kingdom" & 
#                          sex_name == "Male" &
#                          age_group_name %in% 109, val] * temp[109]
#
#
#### UNITED KINGDOM ###
#length(dt[ measure_name == "Probability of death" & 
#             location_name == "United Kingdom" & 
#             sex_name == "Male" &
#             age_group_name %in% 1:109, val])
#
## validation against UK lifetable, close enough.
#plot(uk_valid$qx[2:101],
#     type="l",
#     col = "red",
#     xlab = "Age",
#     ylab = "Mortality rate between age and age+1",
#     main = "Validating GBD dataset")
#lines(dt[ measure_name == "Probability of death" & 
#            location_name == "United Kingdom" & 
#            sex_name == "Male" &
#            age_group_name %in% 1:100, val],
#      col="blue")
#legend(x = 1,y=0.3,legend = c("GBD", "ONS"),col = c("red","blue"),lty = 1,cex=0.5,bty = "n")
#
#
#dt[ measure_name == "Life expectancy" & 
#      location_name == "United Kingdom" & 
#      sex_name == "Male" &
#      age_group_name %in% 50, val]
#
#
## unique countries in mortality dataset
#params$countries[params$countries %in% unique(dt[,location_name])]
##params$countries[match(params$countries,unique(dt[,location_name]))] # monoco and san marino don't match
#sort(unique(dt[,location_name]),decreasing = T)
## and life expectancies for the same ages
#dt.LE <- dt[measure_name == "Life expectancy" & age_group_name %in% ages,.(age_group_name,val), by= .(location_name,sex_name)]
