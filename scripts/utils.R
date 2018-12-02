# Write <df> as a csv file to data/<filename>
WriteDFToTable <- function(df, filename){
  write.table(
    df,
    file = file.path(getwd(), "data", filename),
    sep = ",",
    row.names = FALSE
  )
}

get.ct.fips <- function(){
  #Manually inject datapkg dependency since the repo is deprecated
  source("./scripts/datapkg_read.R")
  town_fips_dp <- datapkg_read(path = 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json')
  # fips data
  return(town_fips_dp$data[[1]])
}

retrieveTownPopulation <- function(path_to_raw, files) {
  town_pop <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(files)) {
    current_file <- read_excel(paste0(path_to_raw, "/", files[i]), sheet=1, range = cell_rows(10:54))
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
    current_file$Year <- unlist(gsub("[^0-9]", "", unlist(files[i])), "")
    # <- get_year
    # bind this year's data to main container.
    town_pop <- rbind(current_file, town_pop)
  }
  return (town_pop)
}

retrieveStatePopulation <- function(path_to_raw, files){
  state_pop <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(files)) {
    current_file <- read_excel(paste0(path_to_raw, "/", files[i]), sheet=1, range = cell_rows(2:3))
    current_file <- current_file[,1]
    current_file$Town <- "Connecticut"
    names(current_file) <- c("Value", "Town")
    #Extract population value 
    current_file$Value <- as.numeric(gsub("[^\\d]+", "", current_file$Value, perl=TRUE))
    # Add year as column in dataset
    get_year <- unlist(gsub("[^0-9]", "", unlist(files[i])), "")
    current_file$Year <- get_year
    # bind this year's data to main container.
    state_pop <- rbind(current_file, state_pop)
  }
  return(state_pop)
}

sanitise <- function(df){
}