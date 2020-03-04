# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script cleans data from original files and saves to cleaned folder.  
# ====== #

rm(list=ls())

# INSTALL PACKAGES (Function courtesty of Paul Schneider)
f_install_n_load <- function(packages){
   for(package in packages){
     if(package %in% rownames(installed.packages()) == FALSE) {
       print(paste("installing",package))
       install.packages(package)
     }
     eval(parse(text=paste('require(',package,')')))
   }
  }

required_packages = c('knitr','tidyverse','rworldmap',
                      'reshape2','ggplot2','dplyr',
                      'tidyr','mc2d','ggrepel',
                      'gridExtra','kableExtra','rgeos',
                      'flextable','viridis','readxl','magrittr','data.table')

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
#GBD_data <- fread("rawdata/GBD AgeDists.csv") %>%  # read in age distributions from GBD data
#                  filter(age_group_id %in% c(5:20,30) & year_id == 2017 & location_id != 533)%>%  # 533 is the state of georgia not the country.
#                  select(location_name, sex_name, age_group_name, age_group_id, val) %>%
#                  filter(location_name %in% heat_data$DisplayString) %>%
#                  spread(key = sex_name,value = val) %>%
#                  mutate(perc_fmle = Female / Both) %>%
#                  arrange(location_name,age_group_id)

# load data from GBD study, excluding the US state of georgia
dt_gbdpop       <- fread("rawdata/GBD AgeDists.csv")[age_group_name %in% 1:94 & 
                               year_id == 2017 & 
                               location_id != 533 & 
                               location_name %in% heat_data$DisplayString,
                              .(location_name, sex_name, age_group_name, val)] 

dt_gbdpop       <- dcast(data = dt_gbdpop,formula = ... ~ sex_name,value.var = "val") # make wide

dt_gbdpop$perc_fmle = dt_gbdpop$Female / dt_gbdpop$Both  # create new column percent female.

dt_gbdpop$ISO_Code = heat_data$ISO_Code[match(dt_gbdpop$location_name,heat_data$DisplayString)]

colnames(dt_gbdpop) <- c("DisplayString","age","All","Female","Male","perc_fmle","ISO_Code")

class(dt_gbdpop$age) = "numeric"   # change age-group name to be numeric

dt_gbdpop <- dt_gbdpop[order(ISO_Code,age)]   # order by ISO code then age.

#=====================================#
#             Mort rates              #
#=====================================#

# load data.
dt_mort     <- rbind(fread("rawdata/gbd_lifetables_2017_fmle.csv"),fread("rawdata/gbd_lifetables_2017_male.csv"))

# filter data -remove georgia USA from dataset so doesn't get mixed up with European country
dt_mort <- dt_mort[measure_name == "Probability of death" & age_group_name %in% 1:100 & 
                     location_name %in% heat_data$DisplayString & location_id != 533,
         .(age_group_name,val), 
         by= .(location_name,sex_name)]

# Reshape to wide
dt_mort       <- dcast(data = dt_mort,formula = ... ~ sex_name,value.var = "val") # make wide

# assign ISO Codes
dt_mort$ISO_Code = heat_data$ISO_Code[match(dt_mort$location_name,heat_data$DisplayString)]

colnames(dt_mort) <- c("DisplayString","age","Female","Male","ISO_Code")

class(dt_mort$age) = "numeric"   # change age-group name to be numeric

dt_mort <- dt_mort[order(ISO_Code,age)]   # order by ISO code then age.


# save all cleaned files to csv format.  
write.csv(x = dt_mort, file = "cleandata/gbd17_mortrates.csv")
write.csv(x = heat_data, file = "cleandata/heat_data.csv") 
write.csv(x = dt_gbdpop, file = "cleandata/gbd_pop.csv")

#write.csv(x = perc_fmle, file = "cleandata/perc_fmle.csv") 
#write.csv(x = gen_pop,   file = "cleandata/gen_pop.csv") 
#write.csv(x = who_mort,  file = "cleandata/who_mort.csv") 


# remove unnecessary data
rm(countries,heat_mort,GBD_data,required_packages,ages, f_install_n_load)
