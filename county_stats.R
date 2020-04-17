# COVID-19 stats tracker--county version, county stats compilation
# Mikaela Springsteen, contactmspringsteen@gmail.com

# poverty, education, unemployment, codes (from 'population' data)
# https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
# codebooks for codes data
# https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
# https://www.ers.usda.gov/data-products/urban-influence-codes.aspx
# https://www.ers.usda.gov/data-products/county-typology-codes/descriptions-and-maps.aspx

# population by characteristics
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html

# employment, household size, housing, insurance
# https://data.census.gov/

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load data
poverty <-  read.csv("data/PovertyEstimates.csv", stringsAsFactors = FALSE)
education <- read.csv("data/Education.csv", stringsAsFactors = FALSE)
unemployment <- read.csv("data/Unemployment.csv", stringsAsFactors = FALSE)
population <- read.csv("data/Population.csv", stringsAsFactors = FALSE)
employment <- read.csv("data/Employment.csv", stringsAsFactors = FALSE)
householdsize <- read.csv("data/HouseholdSize.csv", stringsAsFactors = FALSE)
housing <- read.csv("data/Housing.csv", stringsAsFactors = FALSE)
insurance <- read.csv("data/Insurance.csv", stringsAsFactors = FALSE)
codes <- read.csv("data/Codes.csv", stringsAsFactors = FALSE)

# select population variables
population <- select(population, STATE, COUNTY, YEAR, AGEGRP, TOT_POP, TOT_MALE, TOT_FEMALE, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, AA_MALE, AA_FEMALE, H_MALE, H_FEMALE, TOM_MALE, TOM_FEMALE)

# format population FIPS
population$COUNTY <- formatC(population$COUNTY, width = 3, format = "d", flag = "0")
population$FIPS <- paste0(population$STATE, population$COUNTY)

# restrict to most recent obs, age == all or 65+
population <- filter(population, YEAR == "11")
population <- filter(population, AGEGRP %in% c("0", "14", "15", "16", "17", "18"))

# get population totals
populationtot <- filter(population, AGEGRP == "0")

# get populations 65+
population65 <- filter(population, AGEGRP != "0")
population65 <- population65 %>% 
  group_by(FIPS) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

# select, rename population variables
populationtot <- select(populationtot, -STATE, -COUNTY, -YEAR, -AGEGRP)
population65 <- select(population65, -STATE, -YEAR, -AGEGRP)
names(population65) <- c("FIPS", "oldTOT_POP", "oldTOT_MALE", "oldTOT_FEMALE", "oldWA_MALE", "oldWA_FEMALE", "oldBA_MALE", "oldBA_FEMALE", "oldAA_MALE", "oldAA_FEMALE", "oldH_MALE", "oldH_FEMALE", "oldTOM_MALE", "oldTOM_FEMALE")
population65 <- select(population65, "FIPS", "oldTOT_POP")

# merge populationtot and population65, recalc variables
population <- merge(populationtot, population65, all = TRUE)
population$Male <- (population$TOT_MALE/population$TOT_POP)*100
population$Female <- (population$TOT_FEMALE/population$TOT_POP)*100
population$WhiteAlone <- ((population$WA_MALE+population$WA_FEMALE)/population$TOT_POP)*100
population$WhiteAlone_F <- (population$WA_FEMALE/population$TOT_POP)*100
population$WhiteAlone_M <- (population$WA_MALE/population$TOT_POP)*100
population$BlackAlone <- ((population$BA_MALE+population$BA_FEMALE)/population$TOT_POP)*100
population$BlackAlone_F <- (population$BA_FEMALE/population$TOT_POP)*100
population$BlackAlone_M <- (population$BA_MALE/population$TOT_POP)*100
population$AsianAlone <- ((population$AA_MALE+population$AA_FEMALE)/population$TOT_POP)*100
population$AsianAlone_F <- (population$AA_FEMALE/population$TOT_POP)*100
population$AsianAlone_M <- (population$AA_MALE/population$TOT_POP)*100
population$Hispanic <- ((population$H_MALE+population$H_FEMALE)/population$TOT_POP)*100
population$Hispanic_F <- (population$H_FEMALE/population$TOT_POP)*100
population$Hispanic_M <- (population$H_MALE/population$TOT_POP)*100
population$Multiple <- ((population$TOM_MALE+population$TOM_FEMALE)/population$TOT_POP)*100
population$Multiple_F <- (population$TOM_FEMALE/population$TOT_POP)*100
population$Multiple_M <- (population$TOM_MALE/population$TOT_POP)*100
population <- select(population, FIPS, 16:32)

# clean employment data
employment$FIPS <- gsub("0500000US", "", employment$GEO_ID)
employment$FIPS <- as.numeric(employment$FIPS)
employment$employed_pop <- as.numeric(employment$employed_pop)
employment$employed_pop_retail <- as.numeric(employment$employed_pop_retail)
employment$employed_pop_eduhealthsoc <- as.numeric(employment$employed_pop_eduhealthsoc)
employment$Employed_Retail <- (employment$employed_pop_retail/employment$employed_pop)*100
employment$Employed_EduHealthSoc <- (employment$employed_pop_eduhealthsoc/employment$employed_pop)*100

# clean householdsize data
householdsize$FIPS <- gsub("0500000US", "", householdsize$GEO_ID)
householdsize$FIPS <- as.numeric(householdsize$FIPS)
names(householdsize)[3] <- "HouseholdSize"

# clean housing data
housing$FIPS <- gsub("0500000US", "", housing$GEO_ID)
housing$FIPS <- as.numeric(housing$FIPS)
housing$Owner_Occupied <- (housing$owner_occupied_housing_units/housing$occupied_housing_units)*100

# clean insurance data
insurance$FIPS <- gsub("0500000US", "", insurance$GEO_ID)
insurance$FIPS <- as.numeric(insurance$FIPS)
insurance$civ_noninst_pop <- as.numeric(insurance$civ_noninst_pop)
insurance$civ_noninst_uninsured_pop <- as.numeric(insurance$civ_noninst_uninsured_pop)
insurance$Uninsured <- (insurance$civ_noninst_uninsured_pop/insurance$civ_noninst_pop)*100

# clean unemployment data
names(unemployment) <- c("FIPS", "State", "Area_Name", "MetroArea", "LabourForce","Unemployment", "HHIncome_med", "HHInc_PercStateTotal")
unemployment$MetroArea[unemployment$MetroArea == "1"] <- "Yes"
unemployment$MetroArea[unemployment$MetroArea == "0"] <- "No"
unemployment$HHIncome_med <- gsub("\\$|,", "", unemployment$HHIncome_med)

# clean codes
names(codes) <- c("FIPS", "State", "Area_Name", "Rural_Urban", "UrbanInfCode", "EconType", "Population")
codes$Rural_Urban[codes$Rural_Urban == "1"] <- "1 - Metro (1 million+ urban pop.)"
codes$Rural_Urban[codes$Rural_Urban == "2"] <- "2 - Metro (250,000 to 1 million urban pop.)"
codes$Rural_Urban[codes$Rural_Urban == "3"] <- "3 - Metro (< 250,000 urban pop.)"
codes$Rural_Urban[codes$Rural_Urban == "4"] <- "4 - Nonmetro (20,000+ urban pop., adjacent to metro)"
codes$Rural_Urban[codes$Rural_Urban == "5"] <- "5 - Nonmetro (20,000+ urban pop., not adjacent to metro)"
codes$Rural_Urban[codes$Rural_Urban == "6"] <- "6 - Nonmetro (2,500 to 19,999 urban pop., adjacent to metro)"
codes$Rural_Urban[codes$Rural_Urban == "7"] <- "7 - Nonmetro (2,500 to 19,999 urban pop., not adjacent to metro)"
codes$Rural_Urban[codes$Rural_Urban == "8"] <- "8 - Nonmetro (rural or < 2,500 urban pop., adjacent to metro)"
codes$Rural_Urban[codes$Rural_Urban == "9"] <- "9 - Nonmetro (rural or < 2,500 urban pop., not adjacent to metro)"
codes$UrbanInfCode[codes$UrbanInfCode == "1"] <- "1 - Large (metro, 1 million+ residents)"
codes$UrbanInfCode[codes$UrbanInfCode == "2"] <- "2 - Small (metro, <1 million residents)"
codes$UrbanInfCode[codes$UrbanInfCode == "3"] <- "3 - Micropolitan (adjacent to large metro area)"
codes$UrbanInfCode[codes$UrbanInfCode == "4"] <- "4 - Noncore (adjacent to large metro area)"
codes$UrbanInfCode[codes$UrbanInfCode == "5"] <- "5 - Micropolitan (adjacent to small metro area)"
codes$UrbanInfCode[codes$UrbanInfCode == "6"] <- "6 - Noncore (adjacent to small metro, with a town of ≥ 2,500)"
codes$UrbanInfCode[codes$UrbanInfCode == "7"] <- "7 - Noncore (adjacent to small metro, with a town of ≤ 2,500)"
codes$UrbanInfCode[codes$UrbanInfCode == "8"] <- "8 - Micropolitan (not adjacent to metro area)"
codes$UrbanInfCode[codes$UrbanInfCode == "9"] <- "9 - Noncore (adjacent to micro area, with a town of 2,500 to 19,999)"
codes$UrbanInfCode[codes$UrbanInfCode == "10"] <- "10 - Noncore (adjacent to micro area, with no town of ≥ 2,500)"
codes$UrbanInfCode[codes$UrbanInfCode == "11"] <- "11 - Noncore (not adjacent to micro area, with a town of ≥ 2,500)"
codes$EconType[codes$EconType == "0"] <- "Nonspecialized"
codes$EconType[codes$EconType == "1"] <- "Farming"
codes$EconType[codes$EconType == "2"] <- "Mining"
codes$EconType[codes$EconType == "3"] <- "Manufacturing"
codes$EconType[codes$EconType == "4"] <- "Government"
codes$EconType[codes$EconType == "5"] <- "Recreation"
codes$Population <- gsub(",", "", codes$Population)

# select variables
employment <- select(employment, FIPS, Employed_Retail, Employed_EduHealthSoc)
householdsize <- select(householdsize, FIPS, HouseholdSize)
housing <- select(housing, FIPS, Owner_Occupied)
insurance <- select(insurance, FIPS, Uninsured)
poverty <- select(poverty, FIPS, Poverty)
unemployment <- select(unemployment, FIPS, MetroArea, Unemployment, HHIncome_med, HHInc_PercStateTotal)
codes <- select(codes, FIPS, Rural_Urban, UrbanInfCode, EconType, Population)

# merge data
countystats <- Reduce(function(x, y) merge(x, y, by = c("FIPS"), all = TRUE), list(education, employment, householdsize, housing, insurance, poverty, unemployment, codes, population))

# select for app
countystats <- select(countystats, FIPS, Population, HS, BAorHigher, Owner_Occupied, Uninsured, Poverty, MetroArea, Unemployment, HHIncome_med, HHInc_PercStateTotal, Rural_Urban, UrbanInfCode, EconType, Male, WhiteAlone, BlackAlone, AsianAlone, Hispanic, Multiple)

# write csv
write.csv(countystats, "/Users/mikaelaspringsteen/Desktop/counting-covid19-TEST/countystats.csv", row.names = FALSE)
rm(list=ls())
