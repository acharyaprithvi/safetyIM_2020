################################################################################
#                  Custome functions
# Here we design custom functions to calculate vehicle age and to 
# retrieve VMT, population, and the number of registered passenger vehicles.
#
# P.S. Acharya, 2020
################################################################################


##################################################################
## vehicle age from model year + year of accident
vehage <- function(modyr, yr) {
  age <- NA
  if (modyr %in% c(9998, 9999, 99999)) {
    age <- NA
  } else if (modyr > 1900) {
    age <- yr - modyr + 1
  } else if (modyr %in% c(0:99)) {
    age <- yr - (modyr + 1900) + 1
  } else {
    age <- NA
  }
  return(age)
}

##################################################################
#population for a given state in a given year
getpop <- function(state = 'PA', year = 2004) {
  if (exists('censusdata') == FALSE) {
    censusdata <- read_csv('Misc/censusdata.csv')
  }
  
  if (year > 2018 || year < 1970) {
    print('Data for this year not available.')
    
  } else{
    row <- match(state, censusdata$State)
    column <- year - 1970 + 2
    return(as.integer(censusdata[row, column]))
  }
}

##################################################################
#vmt from NHTSA HM-1 by state and year
getvmt <- function(state = 'PA', year = 2004) {
  if (exists('nhtsa.vmt') == FALSE) {
    nhtsa.vmt <- read_csv('Misc/vmt.csv')
    #based on FWHA Highway Stats VM-2
  }
  if (year > 2018 || year < 1975) {
    print('Data for this year not available.')
    urban <- 0
    rural <- 0
    total <- 0
  } else{
    vmt2 <- filter(nhtsa.vmt, 
                   nhtsa.vmt$Year == year)
    if (state == 'US') {
      urban <- sum(vmt2$Urban_Miles)
      rural <- sum(vmt2$Rural_Miles)
      total <- urban + rural
    } else {
      row <- match(state, vmt2$State)
      urban <- vmt2$Urban_Miles[row]
      rural <- vmt2$Rural_Miles[row]
      total <- urban + rural
    }
  }
  return(c(urban, rural, total))
}


##################################################################
# Number of Registered Vehicles

regvehs <- function(state = 'PA', year = 2018){
    regs <- read_csv('Misc/veh_regs.csv')
    regs <- regs[,c(2:ncol(regs))]
    regs <- pivot_longer(regs, -ABBR,
                         names_to = 'Year',
                         values_to = 'REG_PASS_VEH')
    regs$Year <- as.numeric(regs$Year)
    r2 <- dplyr::filter(regs, regs$Year == year)
    r2 <- dplyr::filter (r2, r2$ABBR %in% state == TRUE)
    return(as.numeric(r2[1,3]))
}
