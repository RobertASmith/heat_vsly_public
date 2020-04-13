# FUNCTION TO CALCULATE DISCOUNTED LIFE YEARS REMAINING AT AGE
rm(list=ls())

#install.packages("data.table")   # install datatable if you don't already have it.
library(data.table)
library(countrycode)
library(dplyr)
library(heemod)

#------------------#
#   LOAD DATA      #
#------------------#
# load necessary data (heat data for countries and iso codes, lifetables for life expectancy/mortality rates)

# heat_data source: HEAT team
heat_data <- read.csv("cleandata/heat_data.csv",stringsAsFactors = F)

# load lifetables
dt_lifetables <- fread("rawdata/WPP2019_Life_Table_OtherVariants.csv")
# how many countries match? 48
countries <- dt_lifetables$Location[dt_lifetables$Location %in% heat_data$DisplayString] %>% unique

# identify countries missing & give short strings to search for.
missing = heat_data$DisplayString[heat_data$DisplayString %in% countries == F]
find = c("Andorra","Czech","Monaco","Moldova","Macedonia","Marino")

# recode countries that are missing, still missing a few, but have a total of 50 countries:
#dt_lifetables$Location[grep(pattern = find[1],x = dt_lifetables$Location,ignore.case = TRUE)] # no Andorra
dt_lifetables$Location[grep(pattern = find[2], x = dt_lifetables$Location,ignore.case = TRUE)] <- missing[2]
#dt_lifetables$Location[grep(pattern = find[3],x = dt_lifetables$Location,ignore.case = TRUE)]  # No Monaco 
dt_lifetables$Location[grep(pattern = find[4], x = dt_lifetables$Location,ignore.case = TRUE)] <- missing[4]
dt_lifetables$Location[grep(pattern = find[5], x = dt_lifetables$Location,ignore.case = TRUE)] <- missing[5]
# dt_lifetables$Location[grep(pattern = find[6], x = dt_lifetables$Location,ignore.case = TRUE)] # no San Marino

# create a vector of new countries which are included in the dataset.
new.countries <- dt_lifetables$Location[dt_lifetables$Location %in% heat_data$DisplayString] %>% unique

# filter dataset by country, sex and time and attribute ISO code: 
dt_lifetables <- dt_lifetables[Location %in% new.countries & Time == "2020-2025" & Sex == "Total" & Variant == "Median PI" & AgeGrpStart<=99,
                               .(ISO3 = countrycode(sourcevar = Location, destination ="iso3c", origin = "country.name"),
                                 Location, Sex, AgeGrp, AgeGrpStart,AgeGrpSpan, Variant, Time,
                                  mx, qx, px, lx, dx, Lx, Sx, Tx, ex, ax)]

# replicate each age-group by span 
dt_lifetables     <- dt_lifetables[rep(1:.N,AgeGrpSpan)][,Age:=1:.N,by=Location] 

# calc probability of survival to end of year.
dt_lifetables$ps  <- 1-dt_lifetables$mx   

# results table
m_alive <- matrix(data = NA,
                  nrow = 100,
                  ncol = dt_lifetables$ISO3 %>% unique() %>% length(),
                  dimnames = list(paste("age",1:100),
                                  dt_lifetables$ISO3 %>% unique()))

# fill the proportions alive at each time period.
for(c in (dt_lifetables$ISO3 %>% unique)){
  
  for(a in 1:100){
    m_alive[a,c] = prod(dt_lifetables[ISO3 == c,ps][1:a])
    } # close age bracket

  } # close country bracket


# calculate discounted life expectancy - create empty matrix
m_lyr <- m_alive*NA ; m_dlyr <- m_lyr
d_r <- 0.0
for(c in (dt_lifetables$ISO3 %>% unique)){
  
  for(a in 1:100){  
  
  # vector of discount rates from 1 to 101 - x (so first period has length 100, 2nd has 99 etc.) 
  v_dr <- (1-d_r)^(1:(101-a)) 
  
  # non-discounted life expectancy
  m_lyr[a,c]   <- sum(m_alive[a:100,c]/m_alive[a,c])
  
  # discounted life-expectancy
  m_dlyr[a,c] <- sum(m_alive[a:100,c]/m_alive[a,c] * v_dr) #
  
} # close age bracket
} # close country bracket


# make data.frame
dt_dlyr <- as.data.table(x = m_dlyr,row.names = NULL)
dt_lyr <- as.data.table(x = m_lyr,row.names = NULL)

# add in age 1:100
dt_dlyr$Age = 1:100
dt_lyr$Age = 1:100

# melt the data.frame
dt_dlyr <- melt(data = dt_dlyr, value.name = "dlyr", variable.name = "ISO3", measure.vars = dt_lifetables$ISO3 %>% unique )
dt_lyr <- melt(data = dt_lyr, value.name = "lyr", variable.name = "ISO3", measure.vars = dt_lifetables$ISO3 %>% unique )


dt_lifetables <- left_join(x = dt_lifetables,y = dt_dlyr, by = c("ISO3","Age")) %>% as.data.table()
dt_lifetables <- left_join(x = dt_lifetables,y = dt_lyr, by = c("ISO3","Age"))  %>% as.data.table()

saveRDS(object = dt_lifetables,file = "cleandata/dlyr_alternative.rda")

data <- read.csv(file = "cleandata/dlyr.csv") %>% as.data.table


# plot(dt_lifetables[ISO3 == "ALB",dlyr])
# lines(data[ISO_Code == "ALB" & age %in% 1:100,dlyr_m])
# lines(data[ISO_Code == "ALB" & age %in% 1:100,dlyr_f])
