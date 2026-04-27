# Placeholder code - To clean up soon.


require(janitor)
require(dplyr)
require(tidyr)
require(default)
require(tidycensus)
require(readxl)
# require(parallel)
require(readr)


# Read in your census key
census_api_key() # Census Key
# readRenviron("~/.Renviron") # To use API key

if (1==2) {
  State = 06
  Geography = "state"
  Survey = "acs5"
  Year = 2019
  moeLevel = 90
  asList = F
  server = T
  sdohVars <- c("insured", "insured_adults", "langSpanish", "above50_rent_or_own")
}

if (FALSE) {
  
  fusionPlace <- ""
  
  sheetNames <- c("Age10", "Age5", "Sex by Age5", "Sex by Age10", 
                  "Sex by Age10 - Latino", "Sex by Age5 - Latino", 
                  "Sex by Age10 - White", "Sex by Age5 - White", 
                  "Sex by Race", "Internet", "Demographics", "Language", 
                  "Education", "Industry", "Housing", "Economic", "groupQuarters", 
                  "Healthcare Access", "Transportation")
  
  sdohVarsInfo <- lapply(sheetNames, function(x) {
    
    readxl::read_xlsx(paste0(fusionPlace, "SDOH/linkageACS.xlsx"), sheet = x, col_types = "text")
    
  }) %>% 
    bind_rows()
  
  sdohVarsInfo %>% count(measureLabel) %>% View()
  saveRDS(sdohVarsInfo, paste0(fusionPlace, "SDOH/linkageACS.RDS"))
  
}


# Standard ACS function to pull ACS variables simultaneously

pullACS <- function(State=06, # 06 = California
                    Geography="tract", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5",
                    Year=2019,
                    moeLevel=90,
                    asList = F, # Want as a list, or as one data frame?
                    server = T, 
                    sdohVars # Vector of sdoh vars, using `Our Variable Name` column
) {
  
  fusionPlace <- ifelse(server, "", "")
  myGeo <- Geography
  
  if (Geography == "mssa") {
    commLinkage <- read_csv(paste0(fusionPlace, "Tract to Community Linkage 2020.csv")) %>% select(-year) %>% filter(!is.na(comID)) 
    myGeo <- "tract"
  }
  
  # Set the default arguments for get_acs function
  default(get_acs) <- list(state=State, geography=myGeo, survey=Survey, year=Year, moe_level=moeLevel)
  
  # Scott Fujimoto's and Ben Hick's Function: Calls ACS API to pull variables and structures data
  ACSEst <- function(numeratorID, denominatorID, DESCRIPTIVELABEL, SHORTLABEL, GROUPLABEL){    ##change case etc last three for consistency/standarization?
    
    get_acs(variables = numeratorID, summary_var = denominatorID) %>% 
      group_by(GEOID, NAME) %>%                                                   ## what is NAME? consider here... rename(geoName = NAME)
      summarise(numerator      = sum(estimate),          # Summing up numerators
                numeratorMOE   = moe_sum(moe, estimate), # Numerator MOE sum
                denominator    = mean(summary_est),      # Denominator   ## (note: summary_est is same for all values, so using mean to pull one (or min or max)) ## any "better" way?            
                denominatorMOE = mean(summary_moe)) %>%  # Denominator MOE
      mutate(measure  = DESCRIPTIVELABEL,       # make new column of descriptive labels (passed as argument)   ## exclude this?
             measureLabel        = SHORTLABEL,             # make new column of short labels (passed as argument)
             measureGroup        = GROUPLABEL) %>%         # make new column of group labels (passed as argument)
      mutate(estimate = numerator/denominator,           # Calculation proportion estimate  ## minor "risk" here using name that already exits; but fine...
             moe = moe_ratio(numerator, denominator, numeratorMOE, denominatorMOE) ) %>% # Calculate MOE
      select(GEOID, geoName = NAME, measure, measureLabel, measureGroup, 
             numerator, numeratorMOE, denominator, denominatorMOE, estimate, moe)
    
  }
  

  # ----Load full list of SDOH vars, filter on user selection, and prepare to pass it through ACSest function above ----------------
  

  #sdohVarsInfo <- readRDS(paste0(fusionPlace, "SDOH/linkageACS.RDS")) %>%  
  sdohVarsInfo <- readRDS(paste0(fusionPlace, "SDOH/Generate SDOH data/linkageACS.RDS")) %>%    
    filter(measureLabel %in% sdohVars) %>%
    select(measure, measureLabel, measureGroup, table, numerator, denominator) %>%
    replace_na(list(table = "")) 
  
  # --------------------------------------------------------------------------------------------------------------
  
  # Pull ACS data into a list - parallel processing
  sdohData <- lapply(1:nrow(sdohVarsInfo), function(i) {
    
    numID <- strsplit(sdohVarsInfo$numerator[i], split = ", ", fixed = TRUE)[[1]]
    denomID <- sdohVarsInfo$denominator[i]
    if (sdohVarsInfo$table[i] != "") { numID <- paste(sdohVarsInfo$table[i], numID, sep = "_"); denomID <- paste(sdohVarsInfo$table[i], denomID, sep = "_") }

    # Call API
    tDat <- ACSEst(numeratorID = numID,
                   denominatorID = denomID,
                   SHORTLABEL = sdohVarsInfo$measureLabel[i],
                   DESCRIPTIVELABEL = sdohVarsInfo$measure[i], 
                   GROUPLABEL = sdohVarsInfo$measureGroup[i])
    
    # If MSSA level selected, aggregate to MSSA
    if (Geography == "mssa") {
      
      tDat <- commLinkage %>%
        left_join(tDat, by = "GEOID") %>%
        group_by(comID, comName, county, measure, measureLabel, measureGroup) %>%
        summarise(numerator = sum(numerator, na.rm = T), 
                  numeratorMOE = moe_sum(numeratorMOE, numerator, na.rm = T),
                  denominator = sum(denominator, na.rm = T), 
                  denominatorMOE = moe_sum(denominatorMOE, denominator, na.rm = T)) %>%
        mutate(estimate = numerator/denominator, 
               moe=moe_ratio(numerator,denominator,numeratorMOE,denominatorMOE)) %>%
        ungroup()
    }
    
    return(tDat)
    
  })
  
  
  # Return as list or data frame
  if (asList) names(sdohData) <- sdohVarsInfo$measureLabel else sdohData <- bind_rows(sdohData) 
  
  
  return(sdohData)
  
}








