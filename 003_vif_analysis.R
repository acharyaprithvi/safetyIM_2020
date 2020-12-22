##################################################################
#                 03. VIF Analyses
# We now conduct VIF analyses based on OLS models for each of the 
# two panels, to identify correlated variables and exclude them from
# our analyses
#
# P.S. Acharya, 2020
##################################################################
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


ols.check1 <- lm(asinh(Fatals_Per_100k_Vehicles) ~
                  Program + 
                  Program_Repeal + 
                  asinh(Population) + 
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
                  asinh(VMT_Per_Vehicle) +
                  asinh(Veh_Per_Capita), 
                data = panel)

car::vif(ols.check2) %>%
  data.frame() %>%
  rownames_to_column('Variable') %>%
  rename('VIF' = 2)%>%
  mutate (R_squared = 1-(1/VIF))%>% 
  arrange(R_squared) %>%
  rename('VIF' = 2) %>%
  print()

# We then excluded every variable with VIF > 5.

# Now for the second panel:

ols.check2.1 <- lm(asinh(Fatals_Per_100k_Vehicles) ~
                  Program + 
                  Program_Repeal + 
                  Program_Ever + 
                  asinh(Population) + 
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
                  asinh(VMT_Per_Vehicle) +
                  asinh(Veh_Per_Capita) + 
                  asinh(Area) + 
                  asinh(Road.length) + 
                  asinh(Road.density) + 
                  asinh(GDP) + 
                  asinh(Disposable.income) + 
                  asinh(Highway_expend_perCap) + 
                  asinh(Precipitation) + 
                  Dem,
                data = panel_wider)

car::vif(ols.check2.1) %>%
  data.frame() %>%
  rownames_to_column('Variable') %>%
  rename('VIF' = 2)%>%
  mutate (R_squared = 1-(1/VIF))%>% 
  arrange(R_squared) %>%
  rename('VIF' = 2) %>%
  mutate('VIF < 5.0?' = VIF <= 5.0) %>%
  mutate('VIF < 10.0?' = VIF <= 10.0) %>%
  print()

# This shows a lot of aliased variables. Given the correlations between several 
# of these regressors, we every variable from above where the
# VIF was found to be > 10.0, and conduct a new analysis.

ols.check2.2 <- lm(asinh(Fatals_Per_100k_Vehicles) ~
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
                   asinh(VMT_Per_Vehicle) +
                   asinh(Veh_Per_Capita) + 
                   # asinh(Area) + 
                   # asinh(Road.length) + 
                   # asinh(Road.density) + 
                   asinh(GDP) + 
                   # asinh(Disposable.income) + 
                   asinh(Highway_expend_perCap) + 
                   asinh(Precipitation) + 
                   Dem,
                 data = panel_wider)

car::vif(ols.check2.2) %>%
  data.frame() %>%
  rownames_to_column('Variable') %>%
  rename('VIF' = 2)%>%
  mutate (R_squared = 1-(1/VIF))%>% 
  arrange(R_squared) %>%
  rename('VIF' = 2) %>%
  mutate('VIF < 5.0?' = VIF <= 5.0) %>%
  print()
# We then excluded every variable with VIF > 5.