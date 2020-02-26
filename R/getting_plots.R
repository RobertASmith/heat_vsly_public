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
                col_vsl11 = "green")

#--- LOAD RESULTS ---#

results <- read.csv("./outputs/Results.csv",stringsAsFactors = F,row.names = 1)

kable(x = results[c("FRA", "DEU","LUX","ROU","LVA", "POL"),1:4],
      align = 'c',
      format = "latex",
      digits = 2,
      col.names = c("VSLY","Heat1","Heat2","VSL11"),
      caption = "Walking age 20-74")

#---- Table 1 ----
datatable(results[c("FRA", "DEU","LUX","ROU","LVA", "POL"),1:4],
          colnames = c("VSLY","Heat1","Heat2","VSL11"),
          options = list(dom = 't'))

# table of results for all countries
datatable(results[,1:4],colnames = c("VSLY","Heat1","Heat2","VSL11"))

#---- Figure 1 ----
# Comparing estimated monetary benefit per capita (2017 Euro) using four approaches.

# construct data-frame.
df <- data.frame(vsly  = results[,"vsly_w2074"],
                 heat1 = results[,"heat1_w2074"],
                 heat2 = results[,"heat2_w2074"],
                 vsl11 = results[,"vsl11_w2074"],
                 country = rownames(results))

# change to long format.
long <- melt(data=df,
             measure.vars =c("vsly", "heat1","vsl11"),
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
    scale_colour_manual(values = c(vsly=col_list$col_vsly,
                                   heat1 = col_list$col_heat1,
                                   vsl11 = col_list$col_vsl11))
  
ggsave(plot = fig1,filename = "./outputs/figure1.png")

#---- Figure 2 ----
# Population age 20-44

long_yng <- data.frame(vsly  = results[,"vsly_w2044"],
                 heat1 = results[,"heat1_w2044"],
                 heat2 = results[,"heat2_w2044"],
                 vsl11 = results[,"vsl11_w2044"],
                 country = rownames(results)) %>% 
                      melt(.,
                           measure.vars =c("vsly", "heat1","vsl11"),
                           variable.name = "method",
                           value.name = "estimate")

long_old <- data.frame(vsly  = results[,"vsly_w4574"],
                     heat1 = results[,"heat1_w4574"],
                     heat2 = results[,"heat2_w4574"],
                     vsl11 = results[,"vsl11_w4574"],
                     country = rownames(results)) %>% 
                          melt(data=.,
                               measure.vars =c("vsly", "heat1","vsl11"),
                               variable.name = "method",
                               value.name = "estimate")

# create young plot
plot_yng <- ggplot(data = long_yng,
               aes(x=heat2,y=estimate))+
        theme_classic()+
        geom_point(aes(colour = method))+
        geom_abline(slope = 1)+
        #annotate(geom="text", x=130, y=150, 
        #         label="VSLY = HEAT 2Grp", color="black")+
        labs(title = "Age 20-44",
             #subtitle = "Comparing alternative methods to HEAT-2Grp",
             caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
             x = "HEAT 2 Group Method (Euro)", 
             y = "Other Methods (Euro)",
             col = "Model")+
        xlim(0, 150) + ylim(0,150)+
        #geom_label_repel(data = long_yng[long_yng$method=="vsly",], 
        #                 label = long_yng$country[long_yng$method=="vsly"], 
        #                 size = 2,segment.color = "blue",colour = "blue")+
        theme(legend.position = c(0.9, 0.2),plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(values = c(vsly=col_list$col_vsly,
                                       heat1 = col_list$col_heat1,
                                       vsl11 = col_list$col_vsl11))

# create old plot
plot_old <- ggplot(data = long_old,
                   aes(x=heat2, y=estimate))+
  theme_classic()+
  geom_point(aes(colour = method))+
  geom_abline(slope = 1)+
  #annotate(geom="text", x=130, y=150, 
  #         label="VSLY = HEAT 2Grp", color="black")+
  labs(title = "Age 45-74",
       caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
       x = "HEAT 2 Group Method (Euro)", 
       y = "Other Methods (Euro)",
       col = "Model")+
  xlim(0, 150) + ylim(0,150)+
  #geom_label_repel(data = long_old[long_old$method=="vsly",], 
  #                 label = long_old$country[long_old$method=="vsly"], 
  #                 size = 2,nudge_x = 2, nudge_y = -5,direction = "y",
  #                 segment.color = "blue",colour = "blue")+
  theme(legend.position = c(0.9, 0.2),plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c(vsly=col_list$col_vsly,
                                 heat1 = col_list$col_heat1,
                                 vsl11 = col_list$col_vsl11))


fig2 <- grid.arrange(plot_yng, plot_old, ncol=2) # create gridplot
ggsave(plot = fig2,filename = "./outputs/figure2.png")

#  plot1 <- (ggplot(data = d.f,aes(x = HEAT2,y = value))+
#              theme_classic()+
#              geom_point(aes(colour=Method))+
#              geom_abline(slope = 1)+
#              annotate(geom="text", x=130, y=150, 
#                       label="VSLY = HEAT 2Grp", color="black",size=3)+
#              labs(title = "Age 20-44", 
#                   x = "HEAT 2 Group Method (Euro)", 
#                   y = "Other Methods (Euro)",
#                   caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL")+
#              xlim(0, 150) + ylim(0,150)+
#              theme(legend.position = c(0.9, 0.2),
#                    plot.title = element_text(hjust = 0.5))
            
  )
  
  # older
  VSLY <- results[,"vsly_w4574"]
  d.f <- as.data.frame(VSLY)
  d.f$HEAT2 <- HEAT2 <- results[,"heat2_w4574"]
  d.f$VSL11 <- VSL11 <- results[,"vsl11_w4574"]
  d.f$HEAT1 <- HEAT1 <- results[,"heat1_w4574"]
  d.f$country <- rownames(d.f)
  
  # change to long format
  d.f <- gather(data=d.f,key=Method,value=value,c(VSLY,VSL11,HEAT1))
  
  plot2 <- (ggplot(data = d.f,aes(x = HEAT2,y = value))+
              theme_classic()+
              geom_point(aes(colour=Method))+
              geom_abline(slope = 1)+
              annotate(geom="text", x=130, y=150, 
                       label="VSLY = HEAT 2Grp", color="black",size=3)+
              labs(title = "Age 45-74",
                   caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
                   x = "HEAT 2 Group Method (Euro)", 
                   y = "Other Methods (Euro)")+
              xlim(0, 150) + ylim(0,150)+
              theme(legend.position = c(0.9, 0.2),plot.title = element_text(hjust = 0.5))
            #geom_label_repel(data= d.f[d.f$Method=="VSLY",],
            #                 label = d.f[d.f$Method=="VSLY",]$country, 
            #                 size = 2, nudge_x = 2, nudge_y = -5,direction = "y",
            #                 segment.color = "blue",colour="blue")
  )
  
  
  return(
    grid.arrange(plot1, plot2, ncol=2)
    
