# Placeholder code - To clean up soon.


# ACS GEOGRAPHY PARAMETER VALUES - "county", "mssa", "tract"

# HPI GEOGRAPHY PARAMETER VALUES - "counties", "mssa", "tracts"

# Global variables --------------------------------------------------------------------------------------------------------

ACS_recent2 <- 2024
ACS_recent <- 2019
ACS_old <- 2014
ACS_YEARS <- c(ACS_old, ACS_recent, ACS_recent2)

yearG5_recent2 <- "2020-2024"
yearG5_recent <- paste(ACS_recent - 4, ACS_recent, sep = "-")
yearG5_old <- paste(ACS_old - 4, ACS_old, sep = "-")

# Load libraries ----------------------------------------------------------------------------------------------------------

library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)

# Load standards ---------------------------------------------------------------------------------------------------------

server <- TRUE
if (server) source("Standards.R")

# Assign paths -----------------------------------------------------------------------------------------------------------

sdohPlace <- paste0(fusionPlace, 'SDOH/')

 # Load functions  --------------------------------------------------------------------------------------

source(paste0(sdohPlace, "ACS/Tidycensus Variables/compare_acs_tables.R"))
source(paste0(sdohPlace, "Generate SDOH data/pullACS_function.R"))
source(paste0(sdohPlace, 'Generate SDOH data/pullHPI_function.R'))


# Read in info files --------------------------------------------------------------------------------------------------

sdohLink <- readxl::read_xlsx(paste0(standardsPlace, "sdohLink.xlsx")) %>%
  filter(if_any(starts_with("from"), ~ . == "x"))%>% 
  filter(!grepl("CALCULATED", sdohPullName))

tractLink <- read_csv(paste0(standardsPlace, "Tract to Community Linkage.csv"))


# Load ACS Variable --------------------------
if (F) {
  
  load_variables_24 <- tidycensus::load_variables(2024, "acs5")
  load_variables_23 <- tidycensus::load_variables(2023, "acs5")
  load_variables_22 <- tidycensus::load_variables(2022, "acs5")
  load_variables_19 <- tidycensus::load_variables(2019, "acs5")
  
  load_variables_24 <- tidycensus::load_variables(2024, "acs5/profile")
  load_variables_23 <- tidycensus::load_variables(2023, "acs5/profile")
  load_variables_22 <- tidycensus::load_variables(2022, "acs5/profile")
  load_variables_19 <- tidycensus::load_variables(2019, "acs5/profile")
  
  load_variables_24 <- tidycensus::load_variables(2024, "acs5/subject")
  load_variables_23 <- tidycensus::load_variables(2023, "acs5/subject")
  load_variables_22 <- tidycensus::load_variables(2022, "acs5/subject")
  load_variables_19 <- tidycensus::load_variables(2019, "acs5/subject")
}


# SDOH variables from each data source -------------------------------------------------------------------------------

# Any tables that differ?
acsVars2024 <- sdohLink %>% 
  filter(fromACS == "x") %>% 
  filter(if_any(c("inSCODA", "mainSCODA"), ~.x == "x"))

acsVars2019 <- sdohLink %>% 
  filter(fromACS == "x") %>% 
  filter(if_any(c("inSCODA", "mainSCODA"), ~.x == "x"))

acsVars2014 <- acsVars2019 %>% 
  mutate(sdohPullName = ifelse(sdohPullName == "automobile", "automobile2014", sdohPullName), # automobile 2014 and 2019 tables differ
         sdohPullName = ifelse(sdohPullName == "poverty200", "poverty200_2014", sdohPullName)) # poverty200 2014 and 2019 tables differ


ejiVars <- sdohLink %>% 
  filter(fromEJI == "x" & is.na(fromACS)) %>% 
  filter(if_any(c("inSCODA", "mainSCODA"), ~.x == "x"))

enviroVars <- sdohLink %>% 
  filter(fromCalenviro == "x" & is.na(fromACS)) %>% 
  filter(if_any(c("inSCODA", "mainSCODA"), ~.x == "x"))


# Voting & Tree Cover data from HPI
# Pulled these from original sources, so no longer using HPI

hpiVars <- sdohLink %>% 
  filter(fromHPI == "x" & is.na(fromACS)) %>% 
  filter(sdohPullName != "redlined") %>% 
  filter(if_any(c("inSCODA", "mainSCODA"), ~.x == "x"))

hpiLink <- readxl::read_xlsx(paste0(sdohPlace, "Generate SDOH data/linkageHPI.xlsx")) %>% 
  filter(indicator_value %in% hpiVars$sdohPullName)


# Compare ACS Tables ----------------------------------------------------------------------------------------------------

sheetNames <- c("Demographics", "Language", "Internet",
                "Education", "Industry", "Housing", "Economic", "groupQuarters", "Healthcare Access", "Transportation")

# Read in SCODA ACS variables from linkageACS spreadsheet
acsVarsInfo <- lapply(sheetNames, function(x) {
  
  readxl::read_xlsx(paste0(fusionPlace, "SDOH/Generate SDOH data/linkageACS.xlsx"), sheet = x, col_types = "text")
  
}) %>% 
  bind_rows() %>%
  filter(measureLabel %in% acsVars2019$sdohPullName) %>%
  select(measure, measureLabel, measureGroup, table, numerator, denominator) %>%
  replace_na(list(table = "")) %>% 
  mutate(isSummary = case_when(substr(table, 1, 1) == "B" ~ F, 
                               substr(table, 1, 1) == "S" ~ T, 
                               TRUE ~ NA)) %>% 
  filter(!is.na(isSummary))


for (i in 1:nrow(acsVarsInfo)) {
  
  if (acsVarsInfo$table[i] != "") {
    print(paste0("Checking.... ", acsVarsInfo$measureLabel[i]))
    compare_acs_tables(ACS_YEARS, acsVarsInfo$table[i], subject = acsVarsInfo$isSummary[i])
    print("-------------------------------------------------------------------------------------------------------------------------------------------")
  }
  
}

# Compare 2019 and 2024 ACS5
# Total (B01001) - Identical
# Black (B03002) - Identical
# LEP (B16005) - Identical
# hsGradAndBelow (B15003) - Identical
# poverty150 (B06012) - Identical
# nointernetaccess (B28002) - Identical
# overcrowding (B25014) - Identical
# noExtraIncome (B19054) - Identical
# unemployed (B23025) - Identical
# automobile (DP04) - Identical
# notinsured (S2701) - Identical
# above50_rent_ow_own:
# Numerator (B25070_010, B25091_011, B25091_022) - Identical
# Denominator (DP04_0002) - Identical


# Compare 2014 and 2019
# Not identical: 
# "noInternetAccess" - became available in 2017; Use 2019 estimates for 2014
# "noInternetSub" - became available in 2017; Use 2019 estimates for 2014
# "enrolled_3_4" - Have to pull enrolled_3_4 separately for 2014 (only has %, not estimate); For now use 2019 estimates for 2014
# "poverty200" - differs in 2014 and 2019; Created poverty200_2014 in ACS database
# "not_insured_adults" - 18-64 in 2014, 19-64 in 2019. For now use 2019 estimates for 2014
# "noCommute2" - Same
# caraccess - differs in 2014 and 2019; Created automobile14 in ACS database


# acsVars2019 <- acsVars2019 %>% 
#   filter(sdohPullName != "internetSub")
# 
# acsVars2014 <- acsVars2014 %>% 
#   filter(sdohPullName != "internetSub")

acsVarsDiff2014 <- c("noInternetAccess", "noInternetSub", "not_insured_adults", "enrolled_3_4")


# Pull Data -----------------------------------------------------------------------------------------------------------------

## ACS Data -------------------------------------------------------------

### Recent
acsTract_recent2 <- pullACS(Geography = "tract", Year = ACS_recent2, sdohVars = acsVars2024$sdohPullName) %>% mutate(yearG5 = yearG5_recent2)
acsTract_recent <- pullACS(Geography = "tract", Year = ACS_recent, sdohVars = acsVars2019$sdohPullName) %>% mutate(yearG5 = yearG5_recent)

### Old 
acsTract_old <- pullACS(Geography = "tract", Year = ACS_old, sdohVars = filter(acsVars2014, !sdohPullName %in% acsVarsDiff2014)$sdohPullName) %>% 
  mutate(yearG5 = yearG5_old, 
         measureLabel = ifelse(measureLabel == "automobile2014", "automobile", measureLabel), 
         measureLabel = ifelse(measureLabel == "poverty200_2014", "poverty200", measureLabel))

### ACS Variables with different survey tables - use 2019 estimates for 2014
diffTract <- acsTract_recent %>% 
  filter(measureLabel %in% acsVarsDiff2014) %>% 
  mutate(yearG5 = yearG5_old)

# Final ACS tables 
# acsTract_final <- bind_rows(acsTract_recent, mutate(acsTract_recent, yearG5 = "2020-2022")) %>%
#   bind_rows(acsTract_old) %>%
#   bind_rows(diffTract) %>%
#   left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = c("measureLabel" = "sdohPullName")) %>%
#   mutate(estimate = ifelse(direction == "pos", 1 - estimate, estimate),
#          moe = ifelse(direction == "pos", NA, moe)) %>%
#   select(yearG5, GEOID, sdohCode, estimate, moe) %>%
#   ungroup()


# Final ACS Tables - Adding 2020-2024
acsTract_final <- bind_rows(acsTract_recent2, acsTract_recent) %>%
  bind_rows(acsTract_old) %>%
  bind_rows(diffTract) %>%
  left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = c("measureLabel" = "sdohPullName")) %>%
  mutate(estimate = ifelse(direction == "pos", 1 - estimate, estimate),
         moe = ifelse(direction == "pos", NA, moe)) %>%
  select(yearG5, GEOID, sdohCode, estimate, moe) %>%
  ungroup()


## HPI Data -----------------------------------------------------------------

hpiTract <- pullHPI(geography = "tracts", recentYear = T, indicators = hpiVars$sdohPullName) 

hpiTract_2019 <- hpiTract %>% mutate(yearG5 = "2015-2019")
hpiTract_2014 <- hpiTract_2019 %>% mutate(yearG5 = "2010-2014")
# hpiTract_2020 <- hpiTract_2019 %>% mutate(yearG5 = "2020-2022")

hpiTract_t <- hpiTract_2019 %>%
  bind_rows(hpiTract_2014)
#   bind_rows(hpiTract_2020)

allTracts <- acsTract_final %>%
  filter(yearG5 == "2015-2019") %>% 
  distinct(GEOID)

hpiTract_final <- hpiTract_t %>%
  left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = c("indicator_short" = "sdohPullName"))%>%
  full_join(allTracts, by = c("geoid" = "GEOID")) %>%
  mutate(value = ifelse(direction == "pos" & indicator_short != "hpi2score", 1 - value, value)) %>% 
  select(yearG5, GEOID = geoid, sdohCode, estimate = value) %>%
  complete(., yearG5, GEOID, sdohCode) %>%
  filter(!is.na(sdohCode), !is.na(yearG5))

colSums(is.na(hpiTract_final))
table(hpiTract_final$yearG5, hpiTract_final$sdohCode, useNA = "ifany")
junk <- hpiTract_final %>% 
  filter(is.na(estimate)) 
table(junk$yearG5, junk$sdohCode, useNA = "ifany")
## EJI Data ----------------------------------------------------------------------------

if (nrow(ejiVars) != 0) {
  
  ejiData_raw <- read_csv(paste0(sdohPlace, "Environmental Justice Index Indicators/eji_data.csv")) %>% 
    select(GEOID = geoid, estimate = E_WLKIND)
  
  length(unique(ejiData_raw$GEOID)) # There are duplicate GEOIDs
  
  dupTracts <- ejiData_raw %>% 
    count(GEOID) %>% 
    filter(n > 1) %>% 
    pull(GEOID)
  
  checkDupTracts <- ejiData_raw %>% # Duplicate tracts have identical estimates, so we can remove the duplicates
    filter(GEOID %in% dupTracts)
  
  ejiData <- ejiData_raw %>% 
    distinct(GEOID, .keep_all = TRUE) %>% 
    full_join(allTracts, by = "GEOID") %>%
    mutate(sdohPullName = "E_WLKIND") %>% 
    left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = "sdohPullName") %>%
    mutate(estimate = ifelse(direction == "pos", abs(20 - estimate), estimate)) %>% 
    select(GEOID, sdohCode, estimate)
  
  ejiData_final <- mutate(ejiData, yearG5 = "2015-2019") %>% 
    bind_rows(mutate(ejiData, yearG5 = "2010-2014")) %>% 
    bind_rows(mutate(ejiData, yearG5 = "2020-2022"))
  
}


  

## Calenviro Data ----------------------------------------------------------------------

### 4.0 ----------------------------------------------------

enviroData_raw <- read_excel(paste0(sdohPlace, "CalEnviroscreen/4.0/calenviroscreen40resultsdatadictionary_F_2021.xlsx")) %>% 
  select(GEOID = `Census Tract`, estimate = `Pollution Burden Score`) %>% 
  mutate(GEOID = paste0("0", GEOID)) 

length(unique(enviroData_raw$GEOID)) # No duplicates

enviroData <- enviroData_raw %>% 
  full_join(allTracts, by = "GEOID") %>%
  mutate(sdohPullName = "Pollution Burden Score") %>% 
  left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = "sdohPullName") %>%
  select(GEOID, sdohCode, estimate) # Yields 8,058 Tracts

setdiff(enviroData$GEOID, allTracts$GEOID) # Enviro data has Tract 06037930401, which was recoded in ~2013
setdiff(allTracts$GEOID, enviroData$GEOID)

enviroData <- enviroData %>% 
  filter(GEOID != "06037137000") %>% 
  mutate(GEOID = ifelse(GEOID == "06037930401", "06037137000", GEOID))

enviroData_final <- mutate(enviroData, yearG5 = "2015-2019") %>% 
  bind_rows(mutate(enviroData, yearG5 = "2010-2014")) 

### 5.0 ----------------------------------------------------

enviroData_raw <- read_excel(paste0(sdohPlace, "CalEnviroscreen/5.0/calenviroscreen50results_d_12226.xlsx")) %>% 
  select(GEOID = `Census Tract`, estimate = `Pollution Burden Score`) %>% 
  mutate(GEOID = paste0("0", GEOID)) 

length(unique(enviroData_raw$GEOID)) # No duplicates

allTracts20 <- acsTract_final %>%
  filter(yearG5 == "2020-2024") %>% 
  distinct(GEOID)

enviroData <- enviroData_raw %>% 
  full_join(allTracts20, by = "GEOID") %>%
  mutate(sdohPullName = "Pollution Burden Score") %>% 
  left_join(select(sdohLink, sdohPullName, direction, sdohCode), by = "sdohPullName") %>%
  select(GEOID, sdohCode, estimate) # Yields 8,058 Tracts

setdiff(enviroData$GEOID, allTracts20$GEOID) # Enviro data has Tract 06037930401, which was recoded in ~2013
setdiff(allTracts20$GEOID, enviroData$GEOID)

enviroData_final <- enviroData_final %>% 
  bind_rows(mutate(enviroData, yearG5 = "2020-2024")) 

## Compute ICE values -------------------------------------------------------------------


if (F) {
  computeICE <- function(myGeoLevel = "tract", 
                         myYear = 2019, 
                         myRefRace = "white",
                         myCompRace = "black",
                         myServer = TRUE) {
    
    myYearG <- paste0(myYear - 4, "-", myYear)
    myGeoName <- ifelse(myGeoLevel == "tract", "GEOID", "comID")
    mySdohCode = ifelse(myCompRace == "black", "iceBlack", "iceLatino")
    
    pullACS(Geography = myGeoLevel, Year = myYear, server = myServer,
            sdohVars = c("total", myRefRace, myCompRace)) %>% 
      select({{ myGeoName }}, measureLabel, numerator) %>% 
      pivot_wider(names_from = measureLabel, values_from = numerator) %>% 
      mutate(estimate = (!!as.symbol(myCompRace) - !!as.symbol(myRefRace)) / total, 
             moe = NA, 
             sdohCode = mySdohCode, 
             yearG5 = myYearG) %>% 
      select(yearG5, {{ myGeoName }}, sdohCode, estimate, moe)
    
  }
  
  ### ICE - Tract Level
  iceTract <- bind_rows(
    computeICE(myGeoLevel = "tract", myYear = 2019, myCompRace = "black", myServer = TRUE),
    computeICE(myGeoLevel = "tract", myYear = 2019, myCompRace = "latino", myServer = TRUE),
    computeICE(myGeoLevel = "tract", myYear = 2014, myCompRace = "black", myServer = TRUE),
    computeICE(myGeoLevel = "tract", myYear = 2014, myCompRace = "latino", myServer = TRUE)
  )
  
  iceTract2020 <- iceTract %>% 
    filter(yearG5 == "2015-2019") %>% 
    mutate(yearG5 = "2020-2022")
  
  iceTract_final <- bind_rows(iceTract2020, iceTract)
}



# Bind final data frames ---------------------------------------------------------------------------------------

sdohTract_final <- bind_rows(acsTract_final, hpiTract_final) %>% 
  # bind_rows(ejiData_final) %>% 
  bind_rows(enviroData_final) 
  # bind_rows(iceTract_final)


# Series of Checks -------------------------------------------------------------------------------------------

table(sdohTract_final$yearG5, sdohTract_final$sdohCode, useNA = "ifany") 

numberOfMissing <- sdohTract_final %>% 
  filter(is.na(estimate) | is.nan(estimate)) %>% 
  count(yearG5, sdohCode) %>% 
  pivot_wider(names_from = yearG5, values_from = n) %>% 
  replace(is.na(.), 0)

sdoh1 <- sdohLink$sdohCode[!sdohLink$sdohCode %in% c("Pollution_Burden", "No_Area_Walkability", "PM25", "HPI_Score")]
sdoh2 <- "Pollution_Burden"
sdoh3 <- "No_Area_Walkability"
sdoh4 <- "PM25"
sdoh5 <- "HPI_Score"

checkHist <- function(myYear, mySDOH, myBins = 100) {
  
  tDat <- sdohTract_final %>% 
    filter(yearG5 == myYear, 
           sdohCode %in% mySDOH)
  
  tPlot <- ggplot(tDat, aes(x = estimate)) +
    geom_histogram(bins = myBins)
  
  if (length(mySDOH) > 1) tPlot <- tPlot + facet_wrap(facets = vars(sdohCode)) + labs(title = myYear)
  if (length(mySDOH) == 1) tPlot <- tPlot + labs(title = paste0(mySDOH, ", ", myYear))
  
  return(tPlot)
  
}

checkHist(myYear = "2015-2019", mySDOH = sdoh1)
checkHist(myYear = "2010-2014", mySDOH = sdoh1)

# Compare to previous dataset
if (F) {
  
  
  previousSDOH <- readRDS(paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract23.RDS")) %>% 
    filter(yearG5 == "2015-2019")
  
  # %>% 
  #   mutate(sdohNameShort = ifelse(sdohNameShort == "Education", "Education3", sdohNameShort), 
  #          sdohNameShort = ifelse(sdohNameShort == "Not_In_School", "Not_In_Pre_School", sdohNameShort))
  
  
  previousSDOHs <- unique(previousSDOH$sdohCode)
  currentSDOHs <- unique(sdohTract_final$sdohCode)
  sdohComp <- currentSDOHs[currentSDOHs %in% previousSDOHs]
  
  
  for (sdoh in sdohComp) {
    
    tPrevious <- previousSDOH %>% 
      ungroup() %>% 
      as.data.frame() %>% 
      filter(sdohCode == sdoh) %>% 
      arrange(yearG5, GEOID)
    
    tCurrent <- sdohTract_final %>% 
      filter(yearG5 == "2015-2019") %>% 
      ungroup() %>% 
      as.data.frame() %>% 
      filter(sdohCode == sdoh) %>% 
      arrange(yearG5, GEOID)
    
    checkIdentical <- identical(tPrevious, tCurrent)
    
    print(paste0(sdoh, ": ", checkIdentical))
    
  } 
  
}


# Save File --------------------------------------------------------------------------------------------------------

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))


# Add ICE RE-Income to final dataset -----------------------------------------------------------------------------------------------------

sdohTract_final <- readRDS(paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))

acsVarsLoad24 <- tidycensus::load_variables(year = 2024, dataset = "acs5")
# acsVarsLoad23 <- tidycensus::load_variables(year = 2023, dataset = "acs5")
acsVarsLoad19 <- tidycensus::load_variables(year = 2019, dataset = "acs5") 
acsVarsLoad14 <- tidycensus::load_variables(year = 2014, dataset = "acs5")

## Cutoff points for 20th and 80th income percentiles ------------------------------------------------------
# B19001 - Household income in the past 12 months (in inflation-adjusted dollars)
income2024_link <- acsVarsLoad24 %>% filter(name %in% sprintf("B19001_%03d", 2:17)) %>% select(variable = name, label)
# income2023_link <- acsVarsLoad23 %>% filter(name %in% sprintf("B19001_%03d", 2:17)) %>% select(variable = name, label)
income2014_link <- acsVarsLoad14 %>% filter(name %in% sprintf("B19001_%03d", 2:17)) %>% select(variable = name, label)
income2019_link <- acsVarsLoad19 %>% filter(name %in% sprintf("B19001_%03d", 2:17)) %>% select(variable = name, label)

# 2019: 25-30k (006); >150k
# 2024: 35-40k (008); >200k

getCutOffs <- function(myLink, myYear) {
  
  if (F) {
    myLink <- income2024_link
    myYear <- 2024
  }
  
  income <- get_acs("state", variables = myLink$variable, year = myYear, state = 06) %>% 
    left_join(myLink) %>% 
    mutate(cumulative_freq = cumsum(estimate), 
           total_households = sum(estimate), 
           cumulative_percent = cumulative_freq / total_households, 
           label = sub("Estimate!!Total!!", "", label))
  
  P20 <- income %>% 
    filter(cumulative_percent >= 0.20) %>% 
    slice(1) %>% 
    select(variable, label, cumulative_percent)
  
  P80 <- income %>% 
    filter(cumulative_percent >= 0.80) %>% 
    slice(1) %>% 
    select(variable, label, cumulative_percent)
  
  P20_last2 <- substr(P20$variable, nchar(P20$variable)-1, nchar(P20$variable))
  P80_last2 <- substr(P80$variable, nchar(P80$variable)-1, nchar(P80$variable))
  
  return(list(
    p20_id = P20_last2,
    p20_label = P20$label,
    p80_id = P80_last2,
    p80_label = P80$label
  ))
  
}

income2014_p <- getCutOffs(income2014_link, 2014)
income2019_p <- getCutOffs(income2019_link, 2019)
# income2023_p <- getCutOffs(income2023_link, 2023)
income2024_p <- getCutOffs(income2024_link, 2024)

## Compute ICE RE-Poverty (poorBlack, richWhite) -------------------------------------------------------------------

# B19101 - Family Income in the Past 12 Months (in Inflation-Adjusted Dollars) - Alternate table? Not used

# Tables below are used:
# B19001 - Household income in the past 12 months (in inflation-adjusted dollars)
# B19001B - Household Income in the Past 12 Months (in Inflation-Adjusted Dollars) (Black or African American Alone Householder)
# B19001H - Household Income in the Past 12 Months (in Inflation-Adjusted Dollars) (White Alone, Not Hispanic or Latino Householder)

# Black householders - households income < 20th percentile
# White, NH householders - households income > 80th percentile



totalIncomeVars <- "B19001_001"
blackLowIncomeVars_2014 <- sprintf("B19001B_%03d", 2:as.numeric(income2014_p$p20_id))
blackLowIncomeVars_2019 <- sprintf("B19001B_%03d", 2:as.numeric(income2019_p$p20_id))
# blackLowIncomeVars_2023 <- sprintf("B19001B_%03d", 2:as.numeric(income2023_p$p20_id))
blackLowIncomeVars_2024 <- sprintf("B19001B_%03d", 2:as.numeric(income2024_p$p20_id))


whiteHighIncomeVars_2014 <- sprintf("B19001H_%03d", as.numeric(income2014_p$p80_id):17)
whiteHighIncomeVars_2019 <- sprintf("B19001H_%03d", as.numeric(income2019_p$p80_id):17)
# whiteHighIncomeVars_2023 <- sprintf("B19001H_%03d", as.numeric(income2023_p$p80_id):17)
whiteHighIncomeVars_2024 <- sprintf("B19001H_%03d", as.numeric(income2024_p$p80_id):17)


computeICE_raceIncome <- function(myYear) {
  
  if (myYear == 2014) {
    blackLowIncomeVars <- blackLowIncomeVars_2014
    whiteHighIncomeVars <- whiteHighIncomeVars_2014
  } else if (myYear == 2019) {
    blackLowIncomeVars <- blackLowIncomeVars_2019
    whiteHighIncomeVars <- whiteHighIncomeVars_2019
  } else if (myYear == 2023) {
    blackLowIncomeVars <- blackLowIncomeVars_2023
    whiteHighIncomeVars <- whiteHighIncomeVars_2023
  } else if (myYear == 2024) {
    blackLowIncomeVars <- blackLowIncomeVars_2024
    whiteHighIncomeVars <- whiteHighIncomeVars_2024
  }
  
  myYearG <- paste0(myYear - 4, "-", myYear)
  
  blackLowIncome <- get_acs("tract", variables = blackLowIncomeVars, year = myYear, state = 06) %>% 
    group_by(GEOID) %>% 
    summarise(nBlackLowIncome = sum(estimate)) %>% 
    ungroup()
  
  whiteHighIncome <- get_acs("tract", variables = whiteHighIncomeVars, year = myYear, state = 06) %>% 
    group_by(GEOID) %>% 
    summarise(nWhiteHighIncome = sum(estimate)) %>% 
    ungroup()
  
  totalIncome <- get_acs("tract", variables = totalIncomeVars, year = myYear, state = 06) %>% 
    select(GEOID, nTotal = estimate)
  
  iceRaceIncome <- full_join(blackLowIncome, whiteHighIncome) %>% 
    full_join(totalIncome) %>% 
    mutate(iceRaceIncome = (nBlackLowIncome - nWhiteHighIncome) / nTotal, 
           yearG5 = myYearG)
  
}

iceRaceIncome2014 <- computeICE_raceIncome(2014)
iceRaceIncome2019 <- computeICE_raceIncome(2019)
# iceRaceIncome2020 <- iceRaceIncome2019 %>% mutate(yearG5 = "2020-2022")
iceRaceIncome2024 <- computeICE_raceIncome(2024) %>% mutate(yearG5 = "2020-2024")


iceRaceIncome <- bind_rows(iceRaceIncome2014, iceRaceIncome2019, iceRaceIncome2024) %>% 
  mutate(sdohCode = "iceRaceIncome") %>% 
  select(yearG5, GEOID, sdohCode, estimate = iceRaceIncome)

sdohTract_final <- sdohTract_final %>% 
  filter(sdohCode != "iceRaceIncome") %>% 
  bind_rows(iceRaceIncome)

table(sdohTract_final$yearG5, sdohTract_final$sdohCode, useNA = "ifany")

if (F) {
  saveRDS(sdohTract_final, paste0(sdohPlace, "Generate SDOH data/iceRaceIncome/sdoh_tract_newIceRaceIncome.RDS"))
}

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))

# Add crime data to final dataset -----------------------------------------------------------------------------------------------------

# sdohNameShort: crimeTotal, crimePersonal, crimeProperty

sdohTract_final <- readRDS(paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))

# library(foreign)


# 2021 Crime data with 2010 Census Tracts
crimeData <- read_csv(paste0(sdohPlace, "Crime Data/ESRI_2021B_CRIMEINDEX_202406.csv"))

crimeData19 <- crimeData %>% 
  select(GEOID = Census.Tract, crimeTotal = X2021.Total.Crime.Index, 
         crimePersonal = X2021.Personal.Crime.Index, crimeProperty = X2021.Property.Crime.Index) %>% 
  mutate(GEOID = sub("[.]", "", GEOID)
         ) %>% 
  mutate(GEOID = ifelse(GEOID == "06037930401", "06037137000", GEOID))

crimeData19 <- allTracts %>% 
  left_join(crimeData19) %>% 
  mutate(yearG5 = yearG5_recent)

crimeData14 <- crimeData19 %>% 
  mutate(yearG5 = "2010-2014")


# 2023 Crime data with 2020 Census Tracts
crimeData2 <- read_csv(paste0(sdohPlace, "Crime Data/2023CrimeIndexData.csv"))

crimeData23 <- crimeData2 %>% 
  select(GEOID = `Census Tract`, crimeTotal = `2023 Total Crime Index`, 
         crimePersonal = `2023 Personal Crime Index`, crimeProperty = `2023 Property Crime Index`) %>% 
  mutate(GEOID = sub("[.]", "", GEOID))

crimeData23 <- allTracts20 %>% 
  left_join(crimeData23) %>% 
  mutate(yearG5 = yearG5_recent2)

## Investigate ---------------------------------
cor(crimeData14_19$crimePersonal, crimeData14_19$crimeProperty, use = "complete.obs")
cor(crimeData23$crimePersonal, crimeData23$crimeProperty, use = "complete.obs")


pairs(crimeData14_19[,2:4], pch = 19, lower.panel = NULL)
pairs(crimeData23[,2:4], pch = 19, lower.panel = NULL)

## Final crime data ------------------------------
crimeData_final <- crimeData23 %>% 
  bind_rows(crimeData19) %>% 
  bind_rows(crimeData14) %>% 
  select(yearG5, GEOID, crimeTotal) %>% 
  pivot_longer(-c("GEOID", "yearG5"), names_to = "sdohCode", values_to = "estimate")


sdohTract_final <- sdohTract_final %>% 
  filter(sdohCode != "crimeTotal") %>% 
  bind_rows(crimeData_final)

table(sdohTract_final$sdohCode, useNA = "ifany")
table(sdohTract_final$yearG5, sdohTract_final$sdohCode, useNA = "ifany")

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))



## Voting Data ---------------------------------------

sdohTract_final <- readRDS(paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))

voting <- readRDS(paste0(sdohPlace, "Voting Data/voting_data.RDS")) %>% 
  filter(year_election %in% c(2018, 2022)) %>% 
  mutate(sdohCode = "voted") %>% 
  select(yearG5, GEOID, sdohCode, estimate = pVote) %>% 
  mutate(estimate = 1 - estimate) 

sdohTract_final <- sdohTract_final %>% 
  filter(sdohCode != "voted") %>% 
  bind_rows(voting)

table(sdohTract_final$sdohCode, useNA = "ifany")
table(sdohTract_final$yearG5, sdohTract_final$sdohCode, useNA = "ifany")
colSums(is.na(sdohTract_final))
sdohTract_final %>% 
  filter(is.na(estimate)) %>% 
  count(yearG5, sdohCode) %>% 
  pivot_wider(names_from = yearG5, values_from = n) %>% 
  View()

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))


# Add Tree Coverage Data -------------------------------------------------

sdohTract_final <- readRDS(paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))

# Read in pop-weighted tree coverage data (2017 and 2021)

tree_cover_2022 <- read_csv(paste0(sdohPlace, "Tree Cover Data/data/tree_cover_data_2022.csv")) %>% 
  mutate(yearG5 = yearG5_recent2, 
         sdohCode = "treecover", 
         estimate = 1 - pTree_popWt) %>% 
  dplyr::select(yearG5, GEOID, sdohCode, estimate)

tree_cover_2017 <- read_csv(paste0(sdohPlace, "Tree Cover Data/data/tree_cover_data_2017.csv")) %>% 
  mutate(yearG5 = yearG5_recent, 
         sdohCode = "treecover", 
         estimate = 1 - pTree) %>% 
  dplyr::select(yearG5, GEOID, sdohCode, estimate)


sdohTract_final <- sdohTract_final %>% 
  filter(sdohCode != "treecover") %>% 
  bind_rows(tree_cover_2017) %>% 
  bind_rows(tree_cover_2022)

table(sdohTract_final$sdohCode, useNA = "ifany")
table(sdohTract_final$yearG5, sdohTract_final$sdohCode, useNA = "ifany")
colSums(is.na(sdohTract_final))
sdohTract_final %>% 
  filter(is.na(estimate)) %>% 
  count(yearG5, sdohCode) %>% 
  pivot_wider(names_from = yearG5, values_from = n) %>% 
  View()

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract24.RDS"))


# Change LA Tract -------------------------------------------------
length(unique(filter(sdohTract_final, yearG5 == "2015-2019")$GEOID))

# cbdDat0$GEOID[cbdDat0$GEOID=="06037930401"] <- "06037137000"
# 
# sdohTract_final %>% filter(GEOID %in% c("06037137000", "06037930401")) %>% count(yearG5, GEOID, sdohCode) %>% View()
# 
# sdohTract_final <- sdohTract_final %>% 
#   mutate(GEOID = ifelse(GEOID == "06037930401", "06037137000", GEOID))
# 
length(unique(filter(sdohTract_final, yearG5 == "2015-2019")$GEOID))

saveRDS(sdohTract_final, paste0(sdohPlace, "SCODA/dataRAW/sdoh_tract23.RDS"))
