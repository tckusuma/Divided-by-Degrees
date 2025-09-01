# Header ####
# Primary Author: Timothy Kusuma
# Date Started: 3/12/2025
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 3/12/2025
# Project: IPUMS Data Analysis
# Title: Data Ask Upload

# The Purpose of this file is to pull in the specific Data Requests From a specially constructed excel file
# The output for this file: **all_tables_list** it is a list of all ask values listed according to tab and list number

# libraries ####################################################################
library(here)
library(ipumsr)
library(tidyverse)
library(haven)
library(labelled)
library(readxl)
library(readr)
library(purrr)
library(rlang)
library(circhelp)
library(Hmisc)

# pull in IPUMS Functions ######################################################
source(here("Scripts", "IPUMS Functions.R"))

# data upload ##################################################################
# data location
ask_location <- here("Data Analysis Ask.xlsx")

# name of sheets in ask file
sheet_name <- excel_sheets(here("Data Analysis Ask.xlsx"))

# pull data: not including the last sheet of ask file
ask_file_list <- list()
for (i in 1:(length(sheet_name)-1)) {
  ask_file_list[[sheet_name[i]]] <- read_excel(here("Data Analysis Ask.xlsx"), sheet = sheet_name[i])
}

# Separate out information tables ##############################################
# pattern "setup page"
pattern_findme <- "Find\\s*Me\\s*"
end_location <- c("))))!", "))))@", "))))#", "))))$", "))))%")

#Function to individually clean up table
Data_Ask_Table_Build <- function(tab_name, table_number, pattern_findme, end_location){
  #"Find Me" new name
  findme_location = paste0(pattern_findme, table_number)
  
  #Initial Box
  Initial_table = find_column_name(ask_file_list[[tab_name]], findme_location, remove_NA = F)
  
  #File Index
  matching = apply(Initial_table, 2, function(column) grepl(end_location, column))
  loc_index = which(matching, arr.ind = TRUE)
  
  #Cleaned Table
  cleaned_table = Initial_table[1:(loc_index[1,"row"]-1),loc_index[1,"col"]:loc_index[nrow(loc_index),"col"]]
  
  #Output
  return(cleaned_table)
  
}

#Applying above function across all ask data sheets

all_tables_list <- list()

for (i in 1:length(ask_file_list)){
  #Count the number of tables
  all_data = apply(ask_file_list[[i]], 1, paste, collapse = " ")
  all_data_combined = paste(all_data, collapse = " ")
  total_count = str_count(all_data_combined, pattern_findme)
  
  #Build List
  data_list <- vector("list", total_count)
  
  for (j in 1:total_count) {
    #Build Table
    data_table = Data_Ask_Table_Build(names(ask_file_list)[i], j, pattern_findme = pattern_findme, end_location = end_location[j])
    
    #Assign data_table to list
    data_list[[j]] = data_table
  }
  
  #Tab name
  tab_named = names(ask_file_list)[i]
  
  #List of Lists
  all_tables_list[[tab_named]] = data_list
}

#remove data sets not important
rm(ask_file_list, data_list, data_table)

#Separate out desired data frames
`File Setup` <- all_tables_list[["Setup"]][[1]]
`Crosswalk/Label Match` <- all_tables_list[["Setup"]][[2]]
`IPUMS Setup` <- all_tables_list[["Setup"]][[3]]
`Interested Data Breakups` <- all_tables_list[["Analysis Request"]][[1]]
`New Data (concentrate)` <- all_tables_list[["Variable Adjustments"]][[1]]
`New Data (combine)` <- all_tables_list[["Variable Adjustments"]][[2]]
`Information Bucket` <- all_tables_list[["Additional Information"]][[1]]
`Crosswalk Formats` <- all_tables_list[["Additional Information"]][[2]]
`Missing Crosswalk` <- all_tables_list[["Additional Information"]][[3]]
`Backend Overwrite` <- all_tables_list[["Backend Overwrites"]][[1]]
