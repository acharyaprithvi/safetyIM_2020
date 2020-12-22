###############################################################################
#                 04. Regression Analyses
# Having identified the key variables and developed the panels, we know conduct
# FE and 2SLS regressions. 
#
# P.S. Acharya, 2020
###############################################################################

library(tidyverse)
library(knitr)
library(plm)
library(stargazer)
library(sjPlot)
library(broom)
library(sandwich)
library(magrittr)

panel <- read_csv('Results/passenger_instate_panel_tidy.csv')
panel_wider <- read_csv('Results/passenger_instate_panel_tidy_wide.csv')

pan <- pdata.frame(
  panel,
  index = c('State Code', 'Year'),
  #(group, time)
  drop.index = FALSE,
  row.names = TRUE
)

pan_wider <- pdata.frame(
  panel_wider,
  index = c('State Code', 'Year'),
  #(group, time)
  drop.index = FALSE,
  row.names = TRUE
)



####### FE Model I
fe1<- plm(asinh(Fatals_Per_100k_Vehicles) ~
            Program +
            Program_Repeal +
            Program_Ever + 
            asinh(Population) +
            #asinh(Prop_Rural_VMT) +
            asinh(Driver_Age) +
            asinh(Median_Income) +
            asinh(Vehicle_Age) +
            asinh(Vehicles_Per_Accident) +
            asinh(Lanes) +
            asinh(Speed_Limit) +
            asinh(Prop_DUI) +
            asinh(Prop_Speeding) +
            asinh(Prop_VehCF) +
            asinh(Prop_Weather) +
            asinh(Prop_Surface) +
            #asinh(VMT_Per_Vehicle) +
            asinh(Veh_Per_Capita), 
          data = pan,
          model = 'within',
          effect = 'twoways'
)

stargazer::stargazer(fe1, ci = FALSE, type = 'text', style = 'all')

######## FE Model II
fe2<- plm(asinh(Fatals_Per_100k_Vehicles) ~
            Program + 
            Program_Repeal + 
            Program_Ever + 
            # asinh(Population) + 
            asinh(Prop_Rural_VMT) + 
            asinh(Driver_Age) + 
            asinh(Median_Income) + 
            asinh(Vehicle_Age) + 
            asinh(Vehicles_Per_Accident) + 
            asinh(Lanes) + 
            asinh(Speed_Limit) + 
            asinh(Prop_DUI) + 
            asinh(Prop_Speeding) + 
            asinh(Prop_VehCF) +  
            asinh(Prop_Weather) +
            asinh(Prop_Surface) + 
            # asinh(VMT_Per_Vehicle) +
            asinh(Veh_Per_Capita) + 
            # asinh(Area) + 
            # asinh(Road.length) + 
            # asinh(Road.density) + 
            asinh(GDP) + 
            # asinh(Disposable.income) + 
            asinh(Highway_expend_perCap) + 
            asinh(Precipitation) + 
            Dem,
          data = pan_wider,
          model = 'within',
          effect = 'twoways'
)

stargazer::stargazer(fe2, ci = FALSE, type = 'text', style = 'all')

######### 2SLS
twosls1 <- AER::ivreg(
  asinh(Fatals_Per_100k_Vehicles) ~
    Program +
    Program_Repeal +
    #Program_Ever +
    asinh(Population) +
    #asinh(Prop_Rural_VMT) +
    asinh(Driver_Age) +
    asinh(Median_Income) +
    asinh(Vehicle_Age) +
    asinh(Vehicles_Per_Accident) +
    asinh(Lanes) +
    asinh(Speed_Limit) +
    # asinh(Prop_DUI) +
    # asinh(Prop_Speeding) +
    asinh(Prop_VehCF) +
    asinh(Prop_Weather) +
    asinh(Prop_Surface) +
    #asinh(VMT_Per_Vehicle) +
    asinh(Veh_Per_Capita) +
    factor(Abbreviation) + 
    factor(Year) | .-Program + asinh(plm::lag(Prop_DUI,3)) + asinh(plm::lag(Prop_Speeding,3)),
  data = pan)


summary(twosls1, vcov = sandwich, df = Inf, diagnostics = TRUE) 


#########################



