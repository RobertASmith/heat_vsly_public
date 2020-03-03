# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script creates discounted life expectancies at each age and calculates the VSLY
#               for each country based upon previous formulas.  
# ====== #
library(trqwe)
#remotes::install_github("traversc/trqwe")

##==== 
## Calculating Discounted Life Years Remaining for each year of age.
##====

# limit countries to those with GBD data
heat_data <- heat_data[heat_data$ISO_Code %in% rownames(who_mort),]
# set discount rate for time.
d.r <- 0.00

#===
# 1. get life years remaining at 85 for each country:
#===

temp <- read.csv("rawdata/who_complete.csv",header = TRUE)

# create dataframe with life years remaining in two columns (male and female).
lyr85 <- data.frame(male = t(temp[133,seq(4,length(temp[1,]),2)]),
                    female = t(temp[133,seq(5,length(temp[1,]),2)])) %>% 
                        set_colnames(c("male","female"))
lyr85[lyr85>100] <- NA  # RWA has weird values.

# weight lyr by sex using percentage in age group female.
v.female <- perc_fmle["80-84",rownames(who_mort)] %>% t()
  
# vector of life years remaining at 85 * vector percent female age 80-84.
lyr85 <- lyr85[rownames(who_mort),"male"]*(1-v.female) + lyr85[rownames(who_mort),"female"] * v.female


#===
# 2. Use Mortality rates at each age to estimate expected life years remaining at each age 20-85.
#===

f.dlyr <- function(country,d.r){
  
  # Get survival probabilities for each year from 1 to 84
  v.Ps <- t(1- who_mort[country,]) %>% 
              rep(each=5) %>%  tail(., -1)

  # Create matrices for discount rates and life years remaining.
  m.dr <- m.lyr <- matrix(data = NA,
                          nrow = 100,
                          ncol = length(v.Ps),
                          dimnames = list(1:100))
  
  # Create expected life years per year from 1-85
  for (a in 1:length(v.Ps)){
    
    for(x in 1:length(v.Ps)){
      if(x>=a){
        m.lyr[x,a] <- prod(v.Ps[a:x])
      }else{
        m.lyr[x,a] <- 0
      }
    }
  }
  
  # create expected life years from 86-100 within the same matrix
  s.clyr85 <- floor(lyr85[country,])  # number of complete years remaining.
  
  m.lyr[(85:(85+s.clyr85-1)),]  <-  matrix(m.lyr[84,],ncol = 84 ,nrow = s.clyr85, byrow="T") # add the expected life years remaining above 85 at each age. 
  m.lyr[(85+s.clyr85),]         <-  m.lyr[84,] * (lyr85[country,] - s.clyr85) 
  m.lyr[(85+s.clyr85+1):100,]   <-  0                                           
  
  #create a matrix of discount rates using a similar method.
  for(a in 1:ncol(m.lyr)){
    m.dr[a:nrow(m.lyr),a] <- 1/(1+d.r)^(0:(nrow(m.lyr)-a))
  }
  
  m.dr[is.na(m.dr)] <- 0 # to set NAs to zero.
  
  # multiply the matrix of discount rates by the matrix of expected life by the matrix of discount rates
  d.lyr <- m.lyr * m.dr
  
  return(colSums(d.lyr))
}

#=====
# 3. Store these discounted life years remaining for each age for each country
#=====
m.C_dlyr <- matrix(NA,
                   ncol = 84,
                   nrow = 49)
rownames(m.C_dlyr)  <-  rownames(heat_data)

for(x in 1:nrow(heat_data)){
  m.C_dlyr[x,] <- f.dlyr(paste(heat_data$ISO_Code[x]),d.r=d.r)    # function using countries and discount rate of 0.015
}

write.csv(x = m.C_dlyr,file = "cleandata/country_dlyr.csv")


# And use these to calculate discounted life years remaining for each 5 year age band.
dlyr <- matrix(data = NA,
               nrow = nrow(m.C_dlyr),
               ncol = 17)
rownames(dlyr) <- heat_data$ISO_Code
colnames(dlyr) <- colnames(who_mort)

# fill in the first column age 1-4
for(x in 1:nrow(m.C_dlyr)){
  
  dlyr[x,1] <- mean(m.C_dlyr[x,1:4])
# fill in all other columns to 80-84
  for (a in 2:17) {
    dlyr[x,a] <- mean(m.C_dlyr[x,((a-1)*5):(a*5-1)])
  }
}


#====
# Calculate VSLY for each country using this equation.
#    VSLY = VSL / dLYR(@50)

heat_data$VSLY <- heat_data$VSL / m.C_dlyr[,50]      # simply the VSL divided by the discounted life years remaining at 50 

#===

# save both sets of data
write.csv(x = dlyr, file = "cleandata/d_lyr.csv") # discounted life years remaining for each 5 year age group. 
write.csv(x = heat_data, file = "cleandata/heat_data.csv") # this overwrites the old heat_data which didn't have VSLY in it.


