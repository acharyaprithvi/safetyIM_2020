##################################################################
#                 02. Developing Regressor Panel
# Having assessed that a sizeable number of fatal accidents in
# the dataset involve vehicles either not registered in-state, or
# of a type (trucks, buses, etc.,) that is not covered by the
# I/M programs in question, we decided to develop the longitudinal
# data panel based SOLELY ON fatal accidents where, every single
# vehicle involved was :
#     (i) registered in the state where the accident occurred, and
#     (ii) a passenger vehicle.
#
# P.S. Acharya, 2020
##################################################################
library(readr)
library(tidyverse)
source('000_customFunctions.R')
options(readr.num_columns = 0)

dates <- read_csv('Misc/program_start_end_dates.csv')
dates$ABOLISHED[is.na(dates$ABOLISHED) == TRUE] <- 9999
#based on raw data from GAO (2010)

states <- read_csv('Misc/GLC_Codes.csv')
#FIPS, GLC Code and USPS abbreviation for each state

income <- read.csv('Misc/incomes.csv')
#from US Census Bureau
#median income as a prop. of
#nationwide median income
colnames(income) <- c('State', c(2018:1975))
income <- pivot_longer(income, -State,
                       names_to = 'Year',
                       values_to = 'Median Income')

income$Year <- as.numeric(as.character(income$Year))

##################################################################

if (exists('panel')) {
  rm(panel)
}

panel <- c(1975:2018) %>% 
  expand.grid(states$`State Code`) %>%
  rename('Year' = 1) %>%
  rename('State Code' = 2) %>%
  left_join(states, by = 'State Code') %>%
  select(1, 4, 2) %>%
  arrange(Year)

thisyear <- 0
start.time <- Sys.time()


for(i in 1:nrow(panel)){
  cat('\014') 
  print(paste0('Processing data for ', panel$Abbreviation[i],
              ' in year ', panel$Year[i], '.'))
  t <- round(as.numeric(difftime(Sys.time(),
                                 start.time,
                                 units = 'mins')),
             digits = 0)
  print(paste('Time since start:', t, ' minutes.'))
  
  
  if(panel$Year[i] != thisyear) { #loading each set of files only once
    rm(acc, veh, per, fac)
    thisyear = panel$Year[i]
    load(paste0('Renamed/files_',thisyear,'.RData'))
    if (thisyear < 1982) {
      pass1 <- veh$BODY_TYP %in%
        c(1:9, 43, 50:52)
      pass2 <- veh$BODY_TYP %in%
        c(60) & veh$TOW_VEH %in% c(0)
      PASSENGER <- pass1 + pass2 > 0
    } else if (thisyear < 1991) {
      pass1 <-
        veh$BODY_TYP %in%
        c(1:12, 40, 41, 48:51, 53:56, 58, 59, 67:69)
      pass2 <- veh$BODY_TYP %in%
        c(79) & veh$TOW_VEH %in% c(0, 9)
      PASSENGER <- pass1 + pass2 > 0
    } else {
      pass1 <-
        veh$BODY_TYP %in%
        c(1:11, 14:22, 24, 25, 28:41, 45:49)
      pass2 <- veh$BODY_TYP %in%
        c(79) & veh$TOW_VEH %in% c(0, 9, 17)
      PASSENGER <- pass1 + pass2 > 0
    }
    
    veh <- veh %>% 
      filter(PASSENGER == TRUE) %>%
      filter(STATE == REG_STAT)
    
    acc <- acc %>% filter(ST_CASE %in% veh$ST_CASE == TRUE)
  }
  
  acc_state <- acc %>% 
    dplyr::filter(STATE == panel$`State Code`[i])
  veh_state <- veh %>% 
    dplyr::filter(STATE == panel$`State Code`[i])
  
  #### Did state j have an I/M program in year i?
  panel$Program[i] <- dates %>% filter(ESTABLISHED < thisyear & 
                       ABOLISHED > thisyear) %>%
    mutate(this = STATE == panel$Abbreviation[i]) %>%
    pull(this) %>%
    sum()
  
  #### was state j's program repealed in year i?
  panel$Program_Repeal[i] <- dates %>%
    mutate(theyear = (ABOLISHED == thisyear)) %>%
    mutate(thestate = (STATE == panel$Abbreviation[i])) %>%
    mutate(this = (thestate + theyear)) %>%
    mutate(this = (this == 2)) %>%
    pull(this) %>%
    sum()
  
  #### did state ever have a program?
  panel$Program_Ever[i] <- dates %>%
    mutate(this = STATE == panel$Abbreviation[i]) %>%
    pull(this) %>%
    sum()
  
  
  #### Years active
  panel$Years_Active[i] <- dates %>%
    filter(ABOLISHED > thisyear) %>%
    mutate(this = (thisyear-ESTABLISHED) * 
             (STATE == panel$Abbreviation[i])) %>%
    pull(this) %>%
    sum()
  
  
  #### Years since repeal
  panel$Years_Since_Repeal[i] <- dates %>%
    filter(ABOLISHED <= thisyear) %>%
    mutate(this = (thisyear-ABOLISHED) *
             (STATE == panel$Abbreviation[i])) %>%
    pull(this) %>%
    sum()
  
  #### Population
  panel$Population[i] <- getpop(state = panel$Abbreviation[i], 
                                year = thisyear)
  
  #### Statewide mileage (Total, Prop_Rural)
  panel$VMT[i] <- getvmt(panel$Abbreviation[i], thisyear)[3]
  panel$Prop_Rural_VMT[i] <- getvmt(panel$Abbreviation[i], thisyear)[2]/
    getvmt(panel$Abbreviation[i], thisyear)[3]
  
  #### Number of registered passenger
  panel$Registered_Vehicles[i] <- regvehs(state = panel$Abbreviation[i],
                        year = thisyear)
  
  #### Number of accidents
  panel$Fatal_Accidents[i] <- acc_state %>%
    nrow()
  
  
  #### Mean driver age
  panel$Driver_Age[i] <-  per %>%
    filter(STATE == panel$`State Code`[i]) %>%
    filter(PER_TYP == 1) %>% 
    filter(AGE <= 100) %>%
    right_join(veh, by = c('ST_CASE', 'VEH_NO')) %>%
    pull(AGE) %>%
    mean(na.rm = TRUE)
  
  #### Statewide median income as a fraction of
  # Nationwide median income (for that year).
  panel$Median_Income[i] <- income %>%
    filter(State == panel$Abbreviation[i]) %>%
    filter(Year == thisyear) %>%
    select(3) %>%
    as.numeric()
  
  #### Fatalities
  panel$Fatalities[i] <- acc_state %>% 
    pull(FATALS) %>% sum()
  
  
  #### mean age of vehicles involved
  panel$Vehicle_Age[i] <- veh_state %>%
    mutate(VehAge = mapply(vehage,
                        modyr = MOD_YEAR,
                        yr = thisyear)) %>%
    group_by(ST_CASE) %>%
    summarize(VehAge = mean(VehAge, na.rm = TRUE)) %>% 
    pull(VehAge) %>% 
    mean(na.rm = TRUE)
  
  #### mean number of vehicles per accident
  panel$Vehicles_Per_Accident[i] <- veh_state %>% nrow()/ 
    acc_state %>% nrow()
  
  #### No. of lanes of traffic at accident site
  if (thisyear <= 2009) {
      panel$Lanes[i] <- acc_state %>%
      pull(NO_LANES) %>% 
      na_if(9) %>%
      mean(na.rm = TRUE)
  } else {
    panel$Lanes[i] <- acc_state %>% 
      left_join(veh_state,
                by = 'ST_CASE') %>%
      filter(!VNUM_LAN == 9) %>%
      group_by(ST_CASE) %>%
      summarize(VNUM_LAN = mean(VNUM_LAN)) %>% 
      pull(VNUM_LAN) %>% 
      mean(na.rm = TRUE)
  }
  
  
  ##### Mean speed-limit at accident site (where available)
  if(thisyear <= 2009) {
    panel$Speed_Limit[i] <- acc_state %>%
      filter(SP_LIMIT %in% c(1:94)) %>%
      pull(SP_LIMIT) %>%
      mean(na.rm = TRUE)
  } else {
    panel$Speed_Limit[i] <- veh_state %>%
      select(ST_CASE, VSPD_LIM) %>% 
      mutate(VSPD_LIM = replace(VSPD_LIM, VSPD_LIM %in% c(1:94) == FALSE, NA)) %>%
      group_by(ST_CASE) %>%
      summarize(SP_MEAN = mean(VSPD_LIM, na.rm = TRUE)) %>%
      pull(SP_MEAN) %>%
      mean(na.rm = TRUE)
  }
  
  #### fraction of accidents involving DUI
  panel$Prop_DUI[i] <- acc_state %>%
    mutate(DRUNK_DR = DRUNK_DR>0) %>%
    pull(DRUNK_DR) %>%
    mean(na.rm = TRUE)
  
  
  #### fraction of accidents involving a speeding vehicle
  if(thisyear <= 1996) {
    panel$Prop_Speeding[i] <- veh_state %>%
      mutate(Speed = DR_CF1 == 44 |
               DR_CF2 == 44 |
               DR_CF3 == 44) %>%
      filter(Speed == TRUE) %>%
      select(ST_CASE, Speed) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      pull(Speed) %>%
      replace_na(0) %>%
      mean()
  } else if (thisyear == 1997) {
    panel$Prop_Speeding[i] <- veh_state %>%
      mutate(Speed = DR_CF1 == 44 | 
               DR_CF2 == 44 | 
               DR_CF3 == 44 |
               DR_CF4 == 44) %>%
      filter(Speed == TRUE) %>%
      select(ST_CASE, Speed) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      pull(Speed) %>%
      replace_na(0) %>%
      mean()
  } else if (thisyear <= 2007) {
    panel$Prop_Speeding[i] <- veh_state %>%
      mutate(Speed = DR_CF1 %in% c(44, 46) | 
               DR_CF2 %in% c(44, 46) | 
               DR_CF3 %in% c(44, 46) |
               DR_CF4 %in% c(44, 46)) %>%
      filter(Speed == TRUE) %>%
      select(ST_CASE, Speed) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      pull(Speed) %>%
      replace_na(0) %>%
      mean()
  } else if (thisyear == 2008) {
    panel$Prop_Speeding[i] <- veh_state %>%
      mutate(Speed = DR_CF1 %in% c(43, 44, 46) | 
               DR_CF2 %in% c(43, 44, 46) | 
               DR_CF3 %in% c(43, 44, 46) |
               DR_CF4 %in% c(43, 44, 46)) %>%
      filter(Speed == TRUE) %>%
      select(ST_CASE, Speed) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      pull(Speed) %>%
      replace_na(0) %>%
      mean()
  } else {
    panel$Prop_Speeding[i] <- veh_state %>%
      mutate(Speed = SPEEDREL!=0) %>%
      filter(Speed == TRUE) %>%
      select(ST_CASE, Speed) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      pull(Speed) %>%
      replace_na(0) %>%
      mean()
  }
      #### fraction of accidents involving at least one vehicle w/
    # a 'contributing factor'.
    #For years <=2009: 
    #   VEH_CF1 or VEH_CF2 (vehicles file) take values 1-19.
    #For years 2010+: 
    #   MFACTOR (Factor file) take values 1-17.
  
  if(thisyear <= 2009){
    panel$Prop_VehCF[i] <- veh_state %>%
      mutate(ContFac = VEH_CF1 %in% c(1:19) |
               VEH_CF2 %in% c(1:19)) %>% 
      filter(ContFac == TRUE) %>%
      select(ST_CASE, ContFac) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      mutate(ContFac = as.numeric(ContFac)) %>%
      pull(ContFac) %>%
      replace_na(0) %>%
      mean()
  } else {
    panel$Prop_VehCF[i] <- fac %>%
      filter(STATE == panel$Abbreviation[i]) %>%
      mutate(ContFac = MFACTOR %in% c(1:17)) %>%
      filter(ContFac == TRUE) %>%
      select(ST_CASE, ContFac) %>%
      unique() %>%
      right_join(acc_state, by = 'ST_CASE') %>%
      mutate(ContFac = as.numeric(ContFac)) %>%
      pull(ContFac) %>%
      replace_na(0) %>%
      mean()
  }
  
  
  #### Proportion of accidents in inclement weather
  if(thisyear <= 2006) {
    panel$Prop_Weather[i] <- acc_state %>%
      mutate(Inclement = !WEATHER %in% c(0, 1, 9, 98, 99)) %>%
      pull(Inclement) %>%
      mean(na.rm = TRUE)
  } else {
    panel$Prop_Weather[i] <- acc_state %>%
      mutate(Inclement = !WEATHER %in% c(0, 1, 9, 98, 99) |
               !WEATHER1 %in% c(0, 1, 9, 98, 99) |
               !WEATHER2 %in% c(0, 1, 9, 98, 99))%>%
      pull(Inclement) %>%
      mean(na.rm = TRUE)
  }
  
  
  #### Fraction of accidents with inclement road conditions
  if(thisyear <= 2009) {
    panel$Prop_Surface[i] <- acc_state %>% 
      mutate(Surface = SUR_COND %in% c(2:8)) %>%
      pull(Surface) %>%
      mean(na.rm = TRUE)
  } else {
    panel$Prop_Surface[i] <- veh_state %>% 
      mutate(Surface = VSURCOND %in% c(2:11)) %>%
      filter(Surface == TRUE) %>%
      select(c('ST_CASE', 'Surface')) %>%
      unique() %>%
      right_join(acc_state, 
                 by = c('ST_CASE')) %>%
      pull(Surface) %>%
      replace_na(FALSE) %>%
      mean(na.rm = TRUE)
  }
}
# Adding some calculated regressors
panel$VMT_Per_Vehicle <- panel$VMT/panel$Registered_Vehicles
panel$Veh_Per_Capita <- panel$Registered_Vehicles/panel$Population

# Adding some calculated Y-variables
panel$Fatals_Per_100k_Pop <-
  panel$Fatalities / (panel$Population/100000)
panel$Fatals_Per_Billion_VMT <- 
  panel$Fatalities / (panel$VMT/1000000000)
panel$Fatals_Per_100k_Vehicles <- 
  panel$Fatalities / (panel$Registered_Vehicles/100000)

# Binned factor variables for duration
bins <- c(0:18)*5 + 1
Binned <- cut(panel$Years_Active, breaks = bins)
level5 <- (c(0, levels(Binned)))
Binned <-  factor(Binned, levels = level5) 
Binned[is.na(Binned)] <- '0'
panel$Years_Active_Bins <- Binned

bins <- c(0:9)*5 + 1
Binned <- cut(panel$Years_Since_Repeal, breaks = bins)
level5 <- (c(0, levels(Binned)))
Binned <-  factor(Binned, levels = level5) 
Binned[is.na(Binned)] <- '0'
panel$Years_Since_Repeal_Bins <- Binned

write_csv(panel, 'Results/passenger_instate_panel_tidy.csv')
#### this is panel 1.

#### now adding the additional variables for panel 2.
new <- readxl::read_excel('Misc/Final data set_1980-2017.xlsx') %>%
  select(3:5,7,8,15:19,22:24) %>% #selecting only non-dupe columns
  rename(Abbreviation = State.code) %>%
  rename(GDP = `GDP per capita`) %>%
  rename(Disposable.income = `Disposable income.per capita`)

panel_wider <- panel %>%
  dplyr::filter(as.numeric(Year) %in% new$Year == TRUE) %>% 
  left_join(new, by = c("Year", "Abbreviation"))

write_csv(panel_wider, 'Results/passenger_instate_panel_tidy_wide.csv')
