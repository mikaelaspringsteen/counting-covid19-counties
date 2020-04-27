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
# coronanet
coronanet <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv"))
# county stats
countystats <-  read.csv("countystats.csv", stringsAsFactors = FALSE)

# coronanet
# select, rename coronanet variables
coronanet <- select(coronanet, -date_announced, -date_end, -type_text, -source_file_Type, -target_direction, -travel_mechanism, -compliance, -enforcer, -ISO_A3, -ISO_A2, -link_type, -target_country, -target_geog_level, -target_region, -target_province, -target_city, -target_other, -target_who_what, -index_high_est, -index_med_est, -index_low_est, -index_country_rank, -domestic_policy, -type_num_vaccines)
coronanet$type[coronanet$type == "Restriction of Non-Essential Businesses"] <- "Restrict businesses"
coronanet$type[coronanet$type == "Restriction of Non-Essential Government Services"] <- "Restrict gov. services"
coronanet$type[coronanet$type == "Closure of Schools"] <- "School closure"
coronanet$type[coronanet$type == "External Border Restrictions"] <- "Restrict external borders"
coronanet$type[coronanet$type == "Internal Border Restrictions"] <- "Restrict internal borders"
coronanet$type[coronanet$type == "Restrictions of Mass Gatherings"] <- "Restrict mass gatherings"
coronanet$type[coronanet$type == "Social Distancing"] <- "Social distancing"
coronanet$type[coronanet$type == "Quarantine/Lockdown"] <- "Quarantine"
coronanet$type[coronanet$type == "Health Monitoring"] <- "Health monitoring"
coronanet$type[coronanet$type == "Health Resources"] <- "Health resources"
coronanet$type[coronanet$type == "Health Testing"] <- "Testing"
coronanet$type[coronanet$type == "New Task Force or Bureau"] <- "New task force"
coronanet$type[coronanet$type == "Public Awareness Campaigns"] <- "Public awareness campaigns"
coronanet$type[coronanet$type == "Declaration of Emergency"] <- "Declaration of emergency"
coronanet$type[coronanet$type == "Other Policy Not Listed Above"] <- "Other"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Doctors"] <- " (doctors)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Government Quarantine (i.e. quarantine at a government hotel or facility)"] <- " (by gov.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hand Sanitizer"] <- " (hand sanitizer)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Certificates"] <- " (health certificates)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Research Facilities"] <- " (research facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Screenings (e.g. temperature checks)"] <- " (screenings)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Volunteers"] <- " (health volunteers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Higher education (i.e. degree granting institutions)"] <- " (higher ed.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hospitals"] <- " (hospitals)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Masks"] <- " (masks)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Non-Essential Commercial Businesses"] <- " (commercial businessess)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Nurses"] <- " (nurses)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other"] <- " (other)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Health Infrastructure"] <- " (other infrastructure)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Health Materials"] <- " (other materials)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Heath Staff"] <- " (other staff)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Personal Grooming (e.g. hair salons)"] <- " (personal grooming)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Preschool or childcare facilities (generally for children ages 5 and below)"] <- " (preschool/childcare)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Primary Schools (generally for children ages 10 and below)"] <- " (primary)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Public Testing Facilities (e.g. drive-in testing for COVID-19)"] <- " (testing facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Quarantine only applies to people of certain ages. Please note the age restrictions in the text box."] <- " (certain ages only)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Quarantine outside the home or government facility (i.e. quarantine in a hotel)"] <- " (outside home or gov.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Restaurants/Bars"] <- " (restaurants/bars)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Retail Businesses"] <- " (retail)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Secondary Schools (generally for children ages 10 to 18)"] <- " (secondary ed.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Self-Quarantine (i.e. quarantine at home)"] <- " (self-quarantine)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Shopping Centers"] <- " (shopping centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Temporary Medical Centers"] <- " (temp. medical centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Temporary Quarantine Centers"] <- " (temp. quarantine centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Test Kits"] <- " (test kits)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Travel History Form (e.g. documents where traveler has recently been)"] <- " (travel history form)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Infrastructure"] <- " (unspecified infrastructure)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Materials"] <- " (unspecified materials)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Staff"] <- " (unspecified staff)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Ventilators"] <- " (ventilators)"
coronanet$type_sub_cat[is.na(coronanet$type_sub_cat)] <- ""
coronanet$Measure <- paste0(coronanet$type, coronanet$type_sub_cat)
coronanet <- select(coronanet, date_start, country, province, init_country_level, link, Measure)
coronanet <- filter(coronanet, country == "United States of America")
coronanet <- filter(coronanet, init_country_level == "Municipal" | init_country_level == "Yes, it is at the province/state level")
coronanet <- filter(coronanet, !is.na(province))
coronanet <- select(coronanet, -country, -init_country_level, -link)
names(coronanet) <- c("Date", "State", "Measure")
coronanet <- unique(coronanet[ , 1:3])
coronanet <- coronanet %>%
  group_by(State, Date) %>%
  mutate(Measure = paste0(Measure, collapse = " <br> "))
coronanet <- unique(coronanet[ , 1:3])

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

# merge covid_cases and coronanet on State and Date
covid_cases_usa <- merge(covid_cases_usa, coronanet, by = c("State", "Date"), all = TRUE)

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

# limit to obs where Day exists
covid_cases_usa <- covid_cases_usa %>% drop_na(Day)

# restructuring for app
covid_cases_usa <- select(covid_cases_usa, -FIPS, -DayCount, -NewCases, -NewDeaths)

# write csv
write.csv(covid_cases_usa, "covid_cases_usa.csv", row.names = FALSE)
rm(list=ls())
