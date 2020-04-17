# COVID-19 stats tracker--county version, data cleaning script
# Mikaela Springsteen, contactmspringsteen@gmail.com

# COVID-19 data from Johns Hopkins University:
# https://github.com/CSSEGISandData/COVID-19

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load data
# JHU
total <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
# county stats
countystats <-  read.csv("countystats.csv", stringsAsFactors = FALSE)

# JHU data

# remove Population column from deaths
deaths <- select(deaths, -Population)

# 0 observations to NA
covid_cases_usa <- list(total, deaths)
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df[ , c(12:ncol(df))][df[ , c(12:ncol(df))] == 0] <- NA
  df
})

# reshape
covid_cases_usa[[1]] <- gather(covid_cases_usa[[1]], Date, Total, -c(1:11))
covid_cases_usa[[2]] <- gather(covid_cases_usa[[2]], Date, Deaths, -c(1:11))

# select, rename variables
names(covid_cases_usa[[1]])[7] <- "State"
names(covid_cases_usa[[2]])[7] <- "State"
names(covid_cases_usa[[1]])[6] <- "County"
names(covid_cases_usa[[2]])[6] <- "County"
covid_cases_usa[[1]] <- select(covid_cases_usa[[1]], -UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_)
covid_cases_usa[[2]] <- select(covid_cases_usa[[2]], -UID, -iso2, -iso3, -code3, -Country_Region, -Lat, -Long_)

# format variables
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df$Date <- as.Date(df$Date, "%m/%d/%y")
  df
})

# keep complete cases
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df <- df[complete.cases(df[6]), ]
  df
})

# merge covid_cases_usa
covid_cases_usa <- covid_cases_usa %>% reduce(left_join, by = c("FIPS", "County", "State", "Combined_Key", "Date"))

# combine covid_cases_usa with countystats
covid_cases_usa <- merge(covid_cases_usa, countystats, by = c("FIPS"), all.x = TRUE)

# restrict to NYC area cases: state == NJ, NY, CT
#test <- filter(covid_cases_usa, State == "New York" | State == "New Jersey" | State == "Connecticut")

# sort by date
covid_cases_usa <- arrange(covid_cases_usa, Date)

# add DayCount variable
covid_cases_usa <- covid_cases_usa %>% group_by(Combined_Key) %>% mutate(DayCount = row_number())

# add Day variable (day 1 = the first day a county has at least 10 cases)
Day_dat <- covid_cases_usa %>% group_by(Combined_Key) %>% filter(Total >= 10) %>% mutate(Day = row_number())
covid_cases_usa <- merge(covid_cases_usa, Day_dat, all = TRUE)

# add NewCases variable
covid_cases_usa <- covid_cases_usa %>% group_by(Combined_Key) %>% mutate(NewCases = Total - lag(Total, default = first(Total)))

# add NewDeaths variable
covid_cases_usa <- covid_cases_usa %>% group_by(Combined_Key) %>% mutate(NewDeaths = Deaths - lag(Deaths, default = first(Deaths)))

# add Totalper100_000 variable
covid_cases_usa$Totalper100_000 <- (covid_cases_usa$Total/covid_cases_usa$Population)*100000

# add TotalRate variable
covid_cases_usa$TotalRate <- covid_cases_usa$Total/covid_cases_usa$Population

# add DeathRate variable
covid_cases_usa$DeathRate <- covid_cases_usa$Deaths/covid_cases_usa$Total

# add Deathsper100_000 variable
covid_cases_usa$Deathsper100_000 <- (covid_cases_usa$Deaths/covid_cases_usa$Population)*100000

# add Population_hundthou variable
covid_cases_usa$Population_hundthou <- (covid_cases_usa$Population)/100000

# remove cruise ships, unnasigned, outside cases
covid_cases_usa <- filter(covid_cases_usa, State != "Diamond Princess")
covid_cases_usa <- filter(covid_cases_usa, State != "Grand Princess")
covid_cases_usa <- filter(covid_cases_usa, County != "Unassigned")
covid_cases_usa <- filter(covid_cases_usa, County != "Out of GA")
covid_cases_usa <- filter(covid_cases_usa, County != "Out of MI")
covid_cases_usa <- filter(covid_cases_usa, County != "Out of TN")
covid_cases_usa <- filter(covid_cases_usa, County != "Out of UT")

# restructuring for app
covid_cases_usa <- covid_cases_usa %>% drop_na(Day)
covid_cases_usa <- select(covid_cases_usa, -FIPS, -DayCount, -NewCases, -NewDeaths)

# write csv
write.csv(covid_cases_usa, "covid_cases_usa.csv", row.names = FALSE)
rm(list=ls())
