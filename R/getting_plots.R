# ====== #
# Author:       Robert Smith
# Contact:      rasmith3@sheffield.ac.uk
# Project:      HEAT VSLY scripts
# Description:  This script creates the plots from the results file, it is stand-alone.  
# ====== #

library(kableExtra)
library(DT)
library(reshape2)
library(ggplot2)
library(ggrepel) 
library(gridExtra)
library(dplyr)


rm(list=ls())
#--- PLOTTING COLOURS  ---# 
# fixed to ensure consistency
col_list = list(col_vsly = "blue",
                col_heat1 = "red",
                col_vsl55 = "green")

params <- list()
params$heat_or_gbd = "gbd"   # select either heat or gbd.

#--- LOAD RESULTS ---#

# either heat or gbd
if(params$heat_or_gbd == "gbd"){
results <- read.csv("./outputs/heat_results.csv",stringsAsFactors = F,row.names = 1) 
}else if(params$heat_or_gbd == "heat"){
  results <- read.csv("./outputs/gbd_results.csv",stringsAsFactors = F,row.names = 1)
}

#---- Table 1 ----

# Latex
kable(x = results[c("FRA", "DEU","LUX","ROU","LVA", "POL"),1:4],
      align = 'c',
      format = "latex",
      digits = 2,
      col.names = c("VSLY","Heat1","Heat2","VSL55"),
      caption = "Walking age 20-74")

# datatable
table1 <- datatable(round(results[c("FRA", "DEU","LUX","ROU","LVA", "POL"),1:4],2),
          colnames = c("VSLY","Heat1","Heat2","VSL55"),
          options = list(dom = 't'))

# SAVE DATATABLE either heat or gbd # not working.
if(params$heat_or_gbd == "gbd"){
  ggsave(filename = "./outputs/gbd_table1.png",plot = table1)
}else if(params$heat_or_gbd == "heat"){
  ggsave(filename = "./outputs/heat_table1.png",plot = table1)
}

# full table of results for all countries
datatable(results[,1:4],
          colnames = c("VSLY","Heat1","Heat2","VSL55"))

#---- Figure 1 ----
# Comparing estimated monetary benefit per capita (2017 Euro) using four approaches.

# construct data-frame.
df <- data.frame(vsly  = results[,"vsly_w2074"],
                 heat1 = results[,"heat1_w2074"],
                 heat2 = results[,"heat2_w2074"],
                 vsl55 = results[,"vsl55_w2074"],
                 country = rownames(results))
#class(df$country) = "character"
# change to long format.
long <- melt(data=df,
             measure.vars =c("vsly", "heat1","vsl55"),
             variable.name = "method",
             value.name = "estimate")

# create plot
fig1 <- ggplot(data = long,
         aes(x=heat2,y=estimate))+
    theme_classic()+
    geom_point(aes(colour = method))+
    geom_abline(slope = 1)+
    #annotate(geom="text", x=130, y=150, 
    #         label="VSLY = HEAT 2Grp", color="black")+
    labs(title = "Estimated Annual Monetary Benefit per capita (in 2017 Euro) in Scenario 1",
         subtitle = "Comparing alternative methods to HEAT-2Grp",
         caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
         x = "HEAT 2 Group Method (Euro)", 
         y = "Other Methods (Euro)",
         col = "Model")+
    xlim(0, 150) + ylim(0,150)+
    geom_label_repel(data = long[long$method=="vsly",], 
                     label = long$country[long$method=="vsly"], 
                     size = 2,nudge_x = 2, nudge_y = -5,direction = "y",
                     segment.color = "blue",colour = "blue")+
    theme(legend.position = c(0.9, 0.2))+
    scale_colour_manual(values = c(vsly = col_list$col_vsly,
                                   heat1 = col_list$col_heat1,
                                   vsl55 = col_list$col_vsl55))

# SAVE FIG1 either heat or gbd
if(params$heat_or_gbd == "gbd"){
  ggsave(filename = "./outputs/gbd_figure1.png",plot = fig1, width = 8, height = 8)
}else if(params$heat_or_gbd == "heat"){
  ggsave(filename = "./outputs/heat_figure1.png",plot = fig1, width = 8, height = 8)
}

#---- Figure 2 ----
# Population age 20-44

long_yng <- data.frame(vsly  = results[,"vsly_w2044"],
                 heat1 = results[,"heat1_w2044"],
                 heat2 = results[,"heat2_w2044"],
                 vsl55 = results[,"vsl55_w2044"],
                 country = rownames(results)) %>% 
                      melt(.,
                           measure.vars =c("vsly", "heat1","vsl55"),
                           variable.name = "method",
                           value.name = "estimate")
class(long_yng$country) = "character"

long_old <- data.frame(vsly  = results[,"vsly_w4574"],
                     heat1 = results[,"heat1_w4574"],
                     heat2 = results[,"heat2_w4574"],
                     vsl55 = results[,"vsl55_w4574"],
                     country = rownames(results)) %>% 
                          melt(data=.,
                               measure.vars =c("vsly", "heat1","vsl55"),
                               variable.name = "method",
                               value.name = "estimate")
class(long_old$country) = "character"


# create young plot
plot_yng <- ggplot(data = long_yng,
               aes(x=heat2,y=estimate, col = method))+
        theme_classic()+
        geom_point()+
        geom_abline(slope = 1)+
        labs(title = "Age 20-44",
             #subtitle = "Comparing alternative methods to HEAT-2Grp",
             caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
             x = "HEAT 2 Group Method (Euro)", 
             y = "Other Methods (Euro)",
             col = "Model")+
        xlim(0, 150) + ylim(0,150)+
        theme(legend.position = c(0.9, 0.2),plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(values = c(vsly=col_list$col_vsly,
                                       heat1 = col_list$col_heat1,
                                       vsl55 = col_list$col_vsl55))

# create old plot
plot_old <- ggplot(data = long_old,
                   aes(x=heat2, y=estimate,col=method))+
  theme_classic()+
  geom_point()+
  geom_abline(slope = 1)+
  labs(title = "Age 45-74",
       caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
       x = "HEAT 2 Group Method (Euro)", 
       y = "Other Methods (Euro)",
       col = "Model")+
  xlim(0, 150) + ylim(0,150)+
  theme(legend.position = c(0.9, 0.2),plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c(vsly  = col_list$col_vsly,
                                 heat1 = col_list$col_heat1,
                                 vsl55 = col_list$col_vsl55))


fig2 <- grid.arrange(plot_yng, plot_old, ncol=2) # create gridplot

# SAVE FIG2 either heat or gbd
if(params$heat_or_gbd == "gbd"){
  ggsave(filename = "./outputs/gbd_figure2.png",plot = fig2, width = 8, height = 8)
}else if(params$heat_or_gbd == "heat"){
  ggsave(filename = "./outputs/heat_figure2.png",plot = fig2, width = 8, height = 8)
}


#-------------#
#   Figure 3  #
#-------------#

d.f <- read.csv("outputs/gbd_age_results.csv",row.names = 1,col.names = c("","vsly","heat1","heat2","vsl55"))
d.f$age <- 1:100
# melt dataframe to use ggplot.
d.f <- melt(d.f,measure.vars = c("vsly","heat1","heat2","vsl55")); colnames(d.f)[colnames(d.f)=="variable"] <- "Model" 


# ggplot
fig3 <- (ggplot(d.f, aes(x = age, y= value, col = Model))+
  
  theme_classic()+
  
  geom_step()+
  
  ylim(c(0,150))+
  
  labs(caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL",  # note need main title in paper.
       x = "Age", 
       y = "Monetary Benefit (Euro)")+
  
  scale_x_continuous(limits = c(20,74),
                     breaks = c(20,30,40,50,60,70))+
  #xlim(20,74)+
    ylim(c(0,60))+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  
  theme(legend.position = c(0.1, 0.8)) + 
  
  scale_colour_manual(values = c(vsly  = col_list$col_vsly,
                                 heat1 = col_list$col_heat1,
                                 vsl55 = col_list$col_vsl55,
                                 heat2 = "black"))
)

# SAVE FIG1 either heat or gbd
if(params$heat_or_gbd == "gbd"){
  ggsave(filename = "./outputs/gbd_figure3.png",plot = fig3, width = 8, height = 8)
}else if(params$heat_or_gbd == "heat"){
  ggsave(filename = "./outputs/heat_figure3.png",plot = fig3, width = 8, height = 8)
}

