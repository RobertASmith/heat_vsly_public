# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script cleans data from original files and saves to cleaned folder.  
# ====== #


rm(list=ls())

# INSTALL PACKAGES (Function courtesty of Paul Schneider)
# install_n_load <- function(packages){
#   for(package in packages){
#     if(package %in% rownames(installed.packages()) == FALSE) {
#       print(paste("installing",package))
#       install.packages(package)
#     }
#     eval(parse(text=paste('require(',package,')')))
#   }
# }

required_packages = c('knitr','tidyverse','rworldmap',
                      'reshape2','ggplot2','dplyr',
                      'tidyr','mc2d','ggrepel',
                      'gridExtra','kableExtra','rgeos',
                      'flextable','viridis','readxl','magrittr')

f_install_n_load(required_packages)

# Load and clean data:

# 1 Heat mortality rates

# mortality rates

heat_mort <- read.csv("rawdata/mortality_rates.txt",header = TRUE)[,c(2,12,13)] %>% 
              spread(key = age_group, value = value_heatdata) %>% 
              set_rownames(.$iso3) %>% 
              set_colnames(c("ISO_Code","age2044","age2064","age2074","age4564","age4574"))

# 2. Country names: merge with heat mortality above.
countries <- as.data.frame(read_excel("rawdata/country_names_iso.xlsx")[,c("DisplayString","ISO")])%>% 
                set_rownames(.$ISO) %>% 
                set_colnames(c("DisplayString","ISO_Code")) %>%
                filter(ISO_Code %in% rownames(heat_mort)) %>% 
                merge(x = .,y = heat_mort) %>% 
                          set_rownames(.$ISO_Code)

# 3. VSL data from HEAT
heat_data <- read.csv("rawdata/vsl_heat.csv")[,c(1,3)]%>% 
                set_colnames(c("ISO_Code","VSL")) %>%
                merge(x = countries,y = .) %>% 
                set_rownames(.$ISO_Code) %>%
                mutate(DisplayString = recode(DisplayString,  
                                'United Kingdom of Great Britain and Northern Ireland' = 'United Kingdom',
                                "Czechia" = "Czech Republic",
                                "Republic of Moldova" = "Moldova",
                                "The former Yugoslav republic of Macedonia" = "Macedonia"))

# set the list of ages.
ages <- c("1-4","5-9","10-14","15-19","20-24",
          "25-29","30-34","35-39","40-44",
          "45-49","50-54","55-59","60-64","65-69",
          "70-74","75-79","80-84")

# 4. GBD data clean so has only population at each age, and percentage female at each age.

# create a cleaned dataset of GBD data with country, age, and populations
GBD_data <- read.csv("rawdata/GBD AgeDists.csv") %>%  # read in age distributions from GBD data
                  filter(age_group_id %in% c(5:20,30) & year_id == 2017 & location_id != 533)%>%  # 533 is the state of georgia not the country.
                  select(location_name, sex_name, age_group_name, age_group_id, val) %>%
                  filter(location_name %in% heat_data$DisplayString) %>%
                  spread(key = sex_name,value = val) %>%
                  mutate(perc_fmle = Female / Both) %>%
                  arrange(location_name,age_group_id)

# manipulate to get percent female at each age group only.
perc_fmle <- GBD_data %>% select(-c(Both,Female,Male)) %>% 
                          spread(value = perc_fmle,key = location_name) %>%  
                          arrange(age_group_id) %>% 
                          select(-c(age_group_name, age_group_id)) %>%
                          set_rownames(ages) %>%
                          set_colnames(heat_data$ISO_Code[match(colnames(.), heat_data$DisplayString)])

# get the general population age each age group only.
gen_pop   <- GBD_data %>% select(-c(perc_fmle,Female,Male)) %>% 
                          spread(value = Both,key = location_name) %>%  
                          arrange(age_group_id) %>%
                          select(-c(age_group_id,age_group_name)) %>% 
                          t() %>% 
                          set_colnames(ages) %>% 
                          set_rownames(heat_data$ISO_Code[match(rownames(.) ,heat_data$DisplayString)])

# get the percentage female age 80+ only - this needs adapting so have data for every year.
#pf_age80 <- read.csv("rawdata/GBD AgeDists.csv") %>%  # read in age distributions from GBD data
#                          filter(age_group_id == 24 & year_id == 2017 & location_id != 533)%>%  # 533 is the state of georgia not the country.
#                          select(location_name, sex_name, age_group_name, age_group_id, val) %>%
#                          filter(location_name %in% heat_data$DisplayString) %>%
#                          spread(key = sex_name,value = val) %>%
#                          mutate(perc_fmle = Female / Both) %>%
#                          arrange(location_name,age_group_id) %>%
#                          select(location_name,perc_fmle) %>% 
#                          set_rownames(heat_data$ISO_Code[match(.$location_name ,heat_data$DisplayString)])

#  5.  Use WHO Country Specific Lifetables to estimate General Mortality rates by 5 year age bands.
m_lifetable <- read.csv("rawdata/m_lifetable.csv",header = TRUE,row.names = 1) %>% 
                  t() %>% as.data.frame() %>%  
                  rownames_to_column(.data = .,'rn') %>%  
                  filter(rn %in% heat_data$ISO_Code) %>%
                  set_rownames(.$rn) %>% select(-c("rn","0-1","85+"))
    
f_lifetable <- read.csv("rawdata/f_lifetable.csv",header = TRUE,row.names = 1) %>% 
                  t() %>% as.data.frame() %>%  
                  rownames_to_column(.data = .,'rn') %>%  
                  filter(rn %in% heat_data$ISO_Code) %>% 
                  set_rownames(.$rn) %>% select(-c("rn","0-1","85+"))

who_mort <- m_lifetable*(1-t(perc_fmle[,rownames(m_lifetable)])) + f_lifetable*t(perc_fmle[,rownames(m_lifetable)])

#-----------------------------#
# LIFE EXPECTANCY AT EACH AGE #
#-----------------------------#

# limit countries to those with heat data and mortality data.
countries <- heat_data$ISO_Code[heat_data$ISO_Code %in% rownames(who_mort)]

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



# save all cleaned files to csv format.                          
write.csv(x = heat_data, file = "cleandata/heat_data.csv") 
write.csv(x = perc_fmle, file = "cleandata/perc_fmle.csv") 
write.csv(x = gen_pop,   file = "cleandata/gen_pop.csv") 
write.csv(x = who_mort,  file = "cleandata/who_mort.csv") 


# remove unnecessary data

rm(countries,heat_mort,GBD_data,install_n_load,required_packages,ages,m_lifetable,f_lifetable)
