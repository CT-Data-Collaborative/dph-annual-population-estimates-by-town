library(dplyr)
library(datapkg)
library(readxl)
library(data.table)
library(stringr)

##################################################################
#
# Processing Script for DPH Annual Population Estimates by Town
# Created by Jenna Daly
# On 09/06/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xls") 

# iterate through files, getting town data
town_pop <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read_excel(paste0(path_to_raw, "/", raw_data[i]), sheet=1, range = cell_rows(10:54))
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  town_cols <- grep("Town", names(current_file), value=T)
  names(current_file)[names(current_file) %in% town_cols] <- "Town"
  pop_cols <- grep("Pop", names(current_file), value=T) 
  names(current_file)[names(current_file) %in% pop_cols] <- "Value"
  current_file1 <- current_file[,1:2]
  current_file2 <- current_file[,3:4]
  current_file3 <- current_file[,5:6]
  current_file4 <- current_file[,7:8]
  current_file <- rbind(current_file1, current_file2, current_file3, current_file4)
  # Add year as column in dataset
  get_year <- unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")
  current_file$Year <- get_year
  # bind this year's data to main container.
  town_pop <- rbind(current_file, town_pop)
}

#Remove any commas from value column
town_pop$Value <- as.numeric(gsub(",", "", town_pop$Value))

#Remove blank rows (leftover from raw file)
town_pop <- town_pop[!is.na(town_pop$Town),]

town_pop$Town[town_pop$Town == "Ea stfo rd"] <- "Eastford"

# iterate through files, getting state data
state_pop <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read_excel(paste0(path_to_raw, "/", raw_data[i]), sheet=1, range = cell_rows(2:3))
  current_file <- current_file[,1]
  current_file$Town <- "Connecticut"
  names(current_file) <- c("Value", "Town")
  #Extract population value 
  current_file$Value <- as.numeric(gsub("[^\\d]+", "", current_file$Value, perl=TRUE))
  # Add year as column in dataset
  get_year <- unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")
  current_file$Year <- get_year
  # bind this year's data to main container.
  state_pop <- rbind(current_file, state_pop)
}

#Remove any commas from value column
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

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

total_pop_fips <- merge(total_pop, fips, all=T)

total_pop_fips$Variable <- "Estimated Population"

total_pop_fips <- total_pop_fips %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`)

# Write collated data to file.
write.table(
  total_pop_fips,
  file.path(getwd(), "data", "dph-population-by-town_2019.csv"),
  sep = ",",
  row.names = F
)
