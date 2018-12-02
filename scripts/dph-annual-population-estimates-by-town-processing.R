library(dplyr)
library(readxl)
library(data.table)
library(stringr)
source("./scripts/utils.R")

##################################################################
#
# Processing Script for DPH Annual Population Estimates by Town
# Created by Jenna Daly
# On 09/06/2017
# Updated: Dec 2, 2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xls") 

# retrieve town and state popoulation data
town_pop <- retrieveTownPopulation(path_to_raw, raw_data)
state_pop <- retrieveStatePopulation(path_to_raw, raw_data)

# sanitise data
town_pop$Value <- as.numeric(gsub(",", "", town_pop$Value))
town_pop <- town_pop[!is.na(town_pop$Town),]
town_pop$Town[town_pop$Town == "Ea stfo rd"] <- "Eastford"
state_pop$Value <- as.numeric(gsub(",", "", state_pop$Value))

#Combine county and state values
total_pop <- rbind(town_pop, state_pop)

# derive percent of state total
percents <- merge(total_pop, state_pop, by = "Year")

percents <- percents %>% 
  mutate(Value = round((as.numeric(Value.x) / Value.y)*100, 2)) %>% 
  select(Year, Town.x, Value) %>% 
  rename(Town = Town.x)

percents$`Measure Type` <- "Percent"
total_pop$`Measure Type` <- "Number"

# bind all the data
total_pop <- rbind(total_pop, percents)

total_pop_fips <- merge(total_pop, get.ct.fips(), all=T)
total_pop_fips$Variable <- "Estimated Population"
total_pop_fips <- total_pop_fips %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`)

# Write collated data to file.
WriteDFToTable(total_pop_fips, "dph-population-by-town_2017.csv")
