# Header ####
# Primary Author: Timothy Kusuma
# Date Started: 4/8/2025
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 4/8/2025
# Project: IPUMS Data Analysis
# Title: Run Functions

# The purpose of this script is to collect "Data Analysis Ask" and "IPUMS Functions" and create the desired outputs
# Outputs:
# Summary Tables of Interest
# Cleaned IPUMS DF
# Graphs?

# Clean environment #####
rm(list = ls())
gc()

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

# pull in Data Ask Upload ######################################################
source(here("Scripts", "Data Ask Upload Code.R"))

# data locations ###############################################################

#Data File Locations
Data_folder <- `File Setup`$`Data Location`[`File Setup`$`Desired Data` == "Data Folder:"]

#Survey Data Location
Survey_Data_folder <- `File Setup`$`Data Location`[`File Setup`$`Desired Data` == "Holding IPUMS Data:"]
Survey_Data_location <- here(Data_folder, Survey_Data_folder)

#Crosswalk and Label Locations
crosswalk_location_list = list()

for (i in 1:(length(`File Setup`$`Data Location`)-2)){
  #Name of Values of Interest
  Crosswalks = `File Setup`$`Desired Data`[i+2]
  
  #Name of location
  location_name = paste0(`Crosswalk/Label Match`$`IPUMS Variable`[`Crosswalk/Label Match`$`Desired Data` == Crosswalks],"_", "Crosswalk_location")
  
  #crosswalk location
  crosswalk_location_list[[location_name]] = here(Data_folder, `File Setup`$`Data Location`[`File Setup`$`Desired Data` == Crosswalks])
}

# Data #########################################################################
#Survey Data Pull
Survey_Data_master_list <- upload_data_by_folder(Survey_Data_location)

#Crosswalk and Labels Master List
Labels_Data_master_list <- list()
Crosswalk_Data_master_list <- list()

for (i in 1:(length(crosswalk_location_list))){
  #Name of Values of Interest
  labels_value = paste0(sub("_.*", "", names(crosswalk_location_list)[[i]]), "_", "labs")
  crosswalk_value = paste0(sub("_.*", "", names(crosswalk_location_list)[[i]]), "_", "crosswalk")
  
  #Data Pull
  Labels_Data_master_list[[labels_value]] = upload_data_by_folder(crosswalk_location_list[[i]], csv_or_xls_name_convention = labels_value, xls_pull_type = `Backend Overwrite`$`Current Preset`[`Backend Overwrite`$`Name of Variable` == "Data Pull Type (Labels)"])
  Crosswalk_Data_master_list[[crosswalk_value]] = upload_data_by_folder(crosswalk_location_list[[i]], csv_or_xls_name_convention = crosswalk_value, xls_pull_type = `Backend Overwrite`$`Current Preset`[`Backend Overwrite`$`Name of Variable` == "Data Pull Type (Crosswalk)"])
}

Labels_Data_master_list %>% View()

# IPUMS Data ###################################################################
# We need to create a code that splits the list and determines if the data are year or month values and then combine into one df

# Default bucket of merging columns ********************************************
merging_bucket_for_use <- c("PERNUM", "CPSID", "YEAR", "SERIAL", "MONTH", "CPSIDP", "CPSIDV")

# IPUMS Function 1.) This function takes data frames of month IPUMS and year IPUMS types and merges them into one data frame based on a bucket of merge able columns
# the two different data frame types should be built into a list type variable
merging_IPUMS_ym <-  function(list_data_m, list_data_y, merging_bucket = merging_bucket_for_use) {
  #Merge the lists of data into one df
  data_y = list_data_y %>% reduce(full_join, by = merging_bucket)
  data_m = list_data_m %>% reduce(full_join, by = merging_bucket)
  
  #Now let us merge the df
  merged_df = left_join(data_y, data_m, by = merging_bucket)
  
  #output
  return(merged_df)
}

# IPUMS Function 2.) pulling apart the ipums data list and flagging those that are for months and those that are for monthsw
IPUMS_List_Condensing <- function(ipums_data_list, flag = "ASECFLAG", merging_bucket = merging_bucket_for_use) {
  #setting in the internal lists for separation
  monthly_ipums <- list()
  yearly_ipums <- list()
  
  #This for loop separates out the ipums list
  for (i in 1:length(ipums_data_list)) {
    #New df name
    ipums_df_march_name = names(ipums_data_list)[i]
    
    #Pulling the specific df
    ipums_df = ipums_data_list[[i]]
    
    #Removing values not in March
    ipums_df_march = subset(ipums_df, !is.na(ipums_df[[flag]]))
    
    #sum of flagged column
    sum_flag = sum(ipums_df_march[[flag]])
    
    #assign the data frame to year or month lists
    if (sum_flag == nrow(ipums_df)) {
      yearly_ipums[[ipums_df_march_name]] = ipums_df_march
    } else {
      monthly_ipums[[ipums_df_march_name]] = ipums_df_march
    }
  }
  
  #Create a merged df using the split values (use IPUMS function 1)
  merged_df = merging_IPUMS_ym(monthly_ipums, yearly_ipums, merging_bucket = merging_bucket)
  
  #output
  return(merged_df)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
Mod_IPUMs_df <- IPUMS_List_Condensing(IPUMS_master_data_list)

# Crosswalks & Labels ############################################################
# Label/Crosswalk function 1.)
# iterates through columns of a df and puts either the first row element as the column name or the column number.
# Function to update column names based on the first row
first_row_column_name <- function(data_df) {
  # Iterate through each column
  for (i in 1:ncol(data_df)) {
    # Check if the first row value is NA or empty
    if (is.na(data_df[1, i]) || data_df[1, i] == "") {
      # Set the column name to the column number
      colnames(data_df)[i] = paste0("Column", i)
    } else {
      # Set the column name to the value in the first row
      colnames(data_df)[i] = data_df[1, i]
    }
  }
  # Remove the first row as it is now used as column names
  data_df = data_df[-1, ]
  
  #output
  return(data_df)
}

# Label/Crosswalk function 2.)
#This function will find the column names row placing that as the column name and erasing everything above.
find_column_name <- function(data_df, column_code_contains, year_label) {
  #adjust the last column word for use
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")
  
  #The name of the column of interest
  column_name = paste0("(?i).*\\b.*\\b(", year_label, ")\\b.*\\b(", mod_column_code_contains, ")\\b.*\\bcode$") #the name of the code column
  
  #find the column name in the data frame
  matching = apply(data_df, 2, function(column) grepl(column_name, column))
  
  #check the number of code column matches
  if (sum(matching) != 1) {
    #stop statement
    stop_statement_column_code = paste0("Incorrect number of cells ending in CODE. Suggestion: add a period to everything except the code column title.")
    stop(stop_statement_column_code)
  }
  
  #index the matching point
  row_index = which(matching, arr.ind = TRUE)[,"row"]
  column_index = which(matching, arr.ind = TRUE)[,"col"]
  
  #modified df
  data_df_column_corrected = data_df[row_index:nrow(data_df), ]
  
  #Collapse the name
  data_df_name_corrected = first_row_column_name(data_df_column_corrected)
  
  #Drop NA values
  data_df_NA_Corrected = data_df_name_corrected[!is.na(data_df_name_corrected[, column_index]), ]
  
  #output
  return(data_df_NA_Corrected)
}

# Label function 1.)
# take a master list of data and adjust the dfs within correcting the column names and data start.
# defaults for label function 1.
column_code_contains_default = c("census", "industry")
column_description_last_word_default = c("title","Description") #we can probably eliminate this idea and combine it with finding the below info

#function
Collapse_df_in_list <- function(data_list, 
                                column_description_last_word = column_description_last_word_default, 
                                column_code_contains = column_code_contains_default, 
                                label_name_overwrite = NULL, code_name_overwrite = NULL){
  #adjust the last column word for use
  mod_column_description_last_word = paste(tolower(column_description_last_word), collapse = "|")
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")
  
  #set up modifying list
  mod_data_list = list()
  
  #Loops over to adjust to the data of interest
  for (i in 1:length(data_list)) {
    #Pull the df name
    original_df_name = names(data_list)[i]
    
    #Pull the year of the data_frame
    year_labeling_adj = str_extract(original_df_name, "\\d+")
    
    #The df of interest
    census_label_df = data_list[[i]]
    
    #Adjust the column name by finding the variable of interest
    mod_census_df = find_column_name(census_label_df, column_code_contains, year_labeling_adj)
    
    #The name of the column of interest
    column_name = paste0("(?i).*\\b.*\\b", year_labeling_adj, "\\b.*(", mod_column_description_last_word, ").*") #the description name
    column_name2 = paste0("(?i).*\\b.*\\b(", year_labeling_adj, ")\\b.*\\b(", mod_column_code_contains, ")\\b.*\\bcode$") #the name of the code column
    
    #Find Column Index
    matching1 = apply(census_label_df, 2, function(column) grepl(column_name, column))
    matching2 = apply(census_label_df, 2, function(column) grepl(column_name2, column))
    column_index1 = which(matching1, arr.ind = TRUE)[,"col"]
    column_index2 = which(matching2, arr.ind = TRUE)[,"col"]
    
    #Drop all family titles
    column_names = names(mod_census_df)[c(column_index1, column_index2)] # Convert column indices to names
    
    mod2_census_df = mod_census_df %>% 
      select(where(~ any(!is.na(.)))) %>%
      bind_rows(setNames(data.frame(t(c("NIU", "0000"))), column_names))
    
    if (length(label_name_overwrite) > 0) {
      mod2_census_df = mod2_census_df %>% rename_with(~ rep(label_name_overwrite, length(.)), contains(mod_column_description_last_word))
    } else {
      mod2_census_df = mod2_census_df %>% rename_with(~ rep("label", length(.)), contains(mod_column_description_last_word))
    }
    if (length(code_name_overwrite) > 0) {
      mod2_census_df = mod2_census_df %>% rename_with(~ rep(code_name_overwrite, length(.)), contains(mod_column_code_contains))
    } else {
      mod2_census_df = mod2_census_df %>% rename_with(~ rep("code", length(.)), contains(mod_column_code_contains))
    }
    
    #Force into list
    mod_data_list[[original_df_name]] = mod2_census_df
  }
  
  #output
  return(mod_data_list)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
mod_Industry_labels_master_list <- Collapse_df_in_list(Industry_labels_master_list)
mod_Occupation_labels_master_list <- Collapse_df_in_list(Occupation_labels_master_list)

# Label function 2.)
# Separate out the labels from a label list.
separate_labels_census <- function(data_list, column_code_contains = column_code_contains_default) {
  #make column code usable
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")
  
  #new_data list
  new_data_list = list()
  
  #Loop over data list 
  for (i in 1:length(data_list)) {
    #Pull the df name
    original_df_name = names(data_list)[i]
    
    #Pull the df
    df = data_list[[i]]
    
    #finding the column of interest
    year_labeling_adj = str_extract(original_df_name, "\\d+")
    column_name = paste0("(?i).*\\b.*\\b", year_labeling_adj, "\\b.*(", mod_column_code_contains, ")\\b.*\\bcode$") #the description name
    column_index = grep(column_name, colnames(df))
    
    #Date Frame filtering out values that 
    df = df %>% 
      filter(!grepl("\\d+-\\d+", df[[column_index]]) & !duplicated(df[[column_index]], fromLast = TRUE)) %>%
      select(where(~ any(!is.na(.))))
    
    #force into list
    new_data_list[[original_df_name]] = df
  }
  
  #output
  return(new_data_list)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
Industry_labels_only_list <- separate_labels_census(mod_Industry_labels_master_list)
Occupation_labels_only_list <- separate_labels_census(mod_Occupation_labels_master_list)

# Label function 3.)
# Separate out the family names from a label list.
separate_family_census <- function(data_list, column_code_contains = column_code_contains_default) {
  #make column code usable
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")
  
  #new_data list
  new_data_list = list()
  
  #Loop over data list 
  for (i in 1:length(data_list)) {
    #Pull the df name
    original_df_name = names(data_list)[i]
    
    #Pull the df
    df = data_list[[i]]
    
    #finding the column of interest
    year_labeling_adj = str_extract(original_df_name, "\\d+")
    column_name = paste0("(?i).*\\b.*\\b", year_labeling_adj, "\\b.*(", mod_column_code_contains, ")\\b.*\\bcode$") #the description name
    column_index = grep(column_name, colnames(df))
    
    #Date Frame filtering out values that 
    df = df %>% 
      filter(grepl("\\d+-\\d+", df[[column_index]]) & !duplicated(df[[column_index]])) %>%
      select(where(~ any(!is.na(.))))
    
    #force into list
    new_data_list[[original_df_name]] = df
  }
  
  #output
  return(new_data_list)
}

Industry_family_only_list <- separate_family_census(mod_Industry_labels_master_list)
Occupation_family_only_list <- separate_family_census(mod_Occupation_labels_master_list)

# Label function 4.)
# Pull the most recent label list to set up as a labelling vector
ready_labels <- function(data_list,
                         column_description_last_word = column_description_last_word_default, 
                         column_code_contains = column_code_contains_default){
  #make column values usable
  mod_column_description_last_word = paste(tolower(column_description_last_word), collapse = "|")
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")
  
  #Ordering the List by name
  ordered_list = data_list[order(names(data_list))]
  
  #pulling the needed df
  foruse_df = ordered_list[[length(data_list)]]
  
  #Pull the year of the data_frame
  year_labeling_adj = str_extract(names(ordered_list)[[length(ordered_list)]], "\\d+")
  
  #The name of the column of interest
  column_name = paste0("(?i).*\\b.*\\b", year_labeling_adj, "\\b.*(", mod_column_description_last_word, ").*") #the description name
  column_name2 = paste0("(?i).*\\b.*\\b(", year_labeling_adj, ")\\b.*\\b(", mod_column_code_contains, ")\\b.*\\bcode$") #the name of the code column
  
  #Find Column Index
  column_index1 = grep(column_name, colnames(foruse_df))
  column_index2 = grep(column_name2, colnames(foruse_df))
  
  # The labeling vector - EDIT
  Labeling_vector = setNames(as.numeric(foruse_df[[column_index2]]), foruse_df[[column_index1]])
  
  # Final Output
  return(Labeling_vector)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
Ind_readied_labels <- ready_labels(Industry_labels_only_list)
Occ_readied_labels <- ready_labels(Occupation_labels_only_list)

# GN Function 1.) to remove spaces and convert to lowercase
normalize <- function(x) {
  tolower(gsub("\\s+", "", x))
}

# GN Function 1.) to sort columns in a df alphabetically
sort_columns_alphabetically <- function(df) {
  df %>% select(order(names(df)))
}

# Crosswalk Function 1.) 
# This function edits crosswalk files to data of interest
#crosswalk_format: "last list": the most recent df contains all crosswalks, "split list": the crosswalk is split over multiple df (assuming the crosswalk is previous values to current)
#NEEDED EDIT (the df sometimes have values that are not codes we would be interested in see if there is an ability to cut check OCC crosswalk to 2018)
census_crosswalk_to_data_interest <- function(data_list, column_code_contains = column_code_contains_default, crosswalk_format) {
  #set up modifying list
  mod_data_list = list()
  
  #Loops over to adjust to the data of interest
  for (i in 1:length(data_list)) {
    #Pull the df name
    original_df_name = names(data_list)[i]
    
    #Pull the year of the data_frame
    year_labeling_adj = str_extract(original_df_name, "\\d+")
    
    #The df of interest
    census_crosswalk_df = data_list[[i]]
    
    #Use Find Column Name to Collapse the DF
    mod_census_crosswalk_df = find_column_name(census_crosswalk_df, column_code_contains, year_labeling_adj)
    
    #Force into list
    mod_data_list[[original_df_name]] = mod_census_crosswalk_df
  }
  
  if (crosswalk_format == "last list") {
    #Ordering the List by name
    ordered_list = mod_data_list[order(names(mod_data_list))]
    
    #pulling the needed df
    foruse_df = ordered_list[[length(mod_data_list)]]
    
    #drop if column does not contain a year
    foruse_df = foruse_df %>% select(matches("\\d{4,}"))
    
    # Iterate over the columns of the data frame
    foruse_df = foruse_df %>% select(matches("\\d{4,}"))
    names_foruse_df = names(foruse_df)
    years_foruse_df = names_foruse_df %>% str_extract("\\d+") %>% unique() %>% sort()
    
    #create list for df
    split_list <- list()
    
    #for loop splits foruse_df
    for (i in 1:ncol(foruse_df)) {
      #Name of column
      column_name = normalize(names(foruse_df)[i])
      current_index = which(normalize(names_foruse_df) == column_name)
      string_no_digits = gsub("\\d", "", column_name)
      
      #specifying years of interest
      current_year = str_extract(column_name, "\\d{4}")
      next_year = years_foruse_df[which(years_foruse_df == current_year) + 1]
      
      #next year names
      next_year_names = paste0(next_year, string_no_digits)
      next_year_index = which(normalize(names_foruse_df) == next_year_names)
      
      if (!is.na(next_year)) {
        #new name
        new_name = paste0(current_year, "-", next_year)
        
        #If the new_name is not already a key in the list, add it
        if (!new_name %in% names(split_list)) {
          split_list[[new_name]] = data.frame(matrix(ncol = 0, nrow = nrow(foruse_df)))
        }
        
        # Add the column to the corresponding year's data frame
        split_list[[new_name]][[column_name]] = as.character(foruse_df[[current_index]])
        split_list[[new_name]][[next_year_names]] = as.character(foruse_df[[next_year_index]])
      }
    }
    
    #organize
    foroutput_list = lapply(split_list, sort_columns_alphabetically)
    
  } else if (crosswalk_format == "split list") {
    #Ordering the List by name
    foruse_list = mod_data_list[order(names(mod_data_list))]
    
    #name
    names_foruse_df = names(foruse_list)
    
    #set up output list
    foroutput_list = list()
    
    for (i in 1:length(foruse_list)) {
      #drop if column does not contain a year
      foruse_df = foruse_list[[i]] %>% select(matches("\\d{4,}"))
      
      #normalize names
      names(foruse_df) = normalize(names(foruse_df))
      
      #new df name
      years_in_col = str_extract(names(foruse_df), "\\d{4,}") %>% unique() %>% sort()
      new_name = paste0(years_in_col[1],"-",years_in_col[length(years_in_col)])
      
      #recombine into list
      foroutput_list[[new_name]] = foruse_df %>% mutate_all(as.character)
    }
    
    #organize
    foroutput_list = lapply(foroutput_list, sort_columns_alphabetically)
    
  } else {
    stop("Improper crosswalk_format input please use either 'last list' or 'split list'.")
  }
  
  #output
  return(foroutput_list)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
mod_Industry_Crosswalk_list <- census_crosswalk_to_data_interest(Industry_Crosswalk_master_list, crosswalk_format = "last list")
mod_Occupation_Crosswalk_master_list <- census_crosswalk_to_data_interest(Occupation_Crosswalk_master_list, crosswalk_format = "split list")

# Crosswalk Function 2.) This function takes the crosswalk list and adds NIU value - EDIT (need to add a check if zero exists in the code to ensure no duplication)
add_zero <- function(data_list, specifier = "code") {
  for (i in 1:length(data_list)) {
    #data_name
    data_list_names = names(data_list[[i]])
    
    #values that include the specifier
    col_index = which(str_extract(data_list_names, specifier) %in% specifier)
    
    #which values
    data_list[[i]][nrow(data_list[[i]]) + 1, col_index] = "0"
  }
  
  #output
  return(data_list)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
mod_with_zero_Industry_Crosswalk_list <- add_zero(mod_Industry_Crosswalk_list)
mod_with_zero_Occupation_Crosswalk_master_list <- add_zero(mod_Occupation_Crosswalk_master_list)

# Internal Function to check coverage - EDIT (see if we can make inputs the same type)
check_coverage <- function(years_IPUMS, year_crosswalks) {
  coverage = rep(FALSE, nrow(years_IPUMS))
  for (i in seq_along(year_crosswalks)) {
    start_year = year_crosswalks[i]
    if (i < length(year_crosswalks)) {
      end_year = year_crosswalks[i + 1] - 1
    } else {
      end_year = max(years_IPUMS)
    }
    coverage[years_IPUMS >= start_year & years_IPUMS <= end_year] <- TRUE
  }
  return(coverage)
}

# Generic Function 1.): creating a new variable name adding a numeric counter to the number of variables with that name.
adjust_name_by_count <- function(column_name, data) {
  #Current Column Names
  existing_columns = names(data)
  
  #initial value
  counter = 2
  
  #building an initial new name
  new_name = paste0(column_name, ".", counter)
  
  #looping through e
  while (new_name %in% existing_columns) {
    counter = counter + 1
    new_name = paste0(column_name, ".", counter)
  }
  
  #output
  return(new_name)
}

# integrated notes on crosswalk issues: this is a column
overwrite_crosswalk_note <- data.frame(years_shift = c("2002 - 2007", "2002 - 2007"),
                                       `2002_code` = c(6675, 6692),
                                       `2007_code`  = c(7772, 6680),
                                       old_label = c("Internet publishing and broadcasting", "Internet service providers"),
                                       new_label = c("Internet publishing and broadcasting and web search portals", "Wired telecommunications carriers"))

# Crosswalk Function 3.) This function takes the crosswalk files and IPUMS files and adjusts values In IPUMS accordingly
# NEEDED EDIT (this function needs to fix some elements like making it easier to pick out the early state and the coverage of the crosswalk)
modify_by_census_crosswalk <- function(IPUMS_df, crosswalk_list, year_variable = "YEAR", variable_for_modification, 
                                       missing_crosswalk, missing_crosswalk_early_year_col, missing_crosswalk_late_year_col) {
  #create a vector of years covered by IPUMS Data.
  Years_ordered = IPUMS_df %>% 
    select(year_variable) %>% 
    distinct() %>% 
    arrange(year_variable)
  
  #create vector of the years covered in the crosswalk data 
  crosswalk_years_foruse = as.numeric(str_extract(names(crosswalk_list), "\\d+"))
  
  #end dates for crosswalk
  crosswalk_end_year = vector(mode = "numeric", length = length(crosswalk_years_foruse))
  for (i in 1:length(crosswalk_years_foruse)) {
    if (i < length(crosswalk_years_foruse)) {
      crosswalk_end_year[i] = crosswalk_years_foruse[i + 1] - 1
    } else {
      crosswalk_end_year[i] = max(Years_ordered)
    }
  }
  
  #check coverage
  covered_values = check_coverage(Years_ordered, crosswalk_years_foruse)
  
  #separate values if missing earlier date
  if (sum(covered_values) == nrow(Years_ordered)) {
    #set initial step
    modified_ipums = IPUMS_df
    
    #looping over the list of crosswalks
    for (i in 1:length(crosswalk_list)) {
      #crosswalk years
      col_names = names(crosswalk_list[[i]])
      col_years = str_extract(col_names, "\\d{4,}") %>% unique() %>% sort()
      col_names_ofinterest = paste0("(", paste(col_years, collapse = "|"), ").*census.*code.*") #HERE
      column_indices = grep(col_names_ofinterest, tolower(col_names)) %>% sort()
      
      #crosswalk df
      crosswalk_df = crosswalk_list[[i]]
      
      #clean crosswalk
      clean_crosswalk_df = crosswalk_df %>% 
        group_by_at(column_indices[1]) %>%
        filter(any(!duplicated(crosswalk_df[column_indices[1]])) | any(!is.na(crosswalk_df[column_indices[length(column_indices)]]))) %>%
        ungroup()
      
      #matches values
      match_early = match(modified_ipums[[variable_for_modification]], as.numeric(clean_crosswalk_df[[col_names[column_indices[1]]]]))
      
      #attach the value assigned in the crosswalk list according to their 
      modified_ipums = modified_ipums %>%
        mutate(!!variable_for_modification := case_when(
          crosswalk_end_year[i] > .data[[year_variable]] ~ as.numeric(clean_crosswalk_df[[column_indices[length(column_indices)]]][match_early]),
          TRUE ~ modified_ipums[[variable_for_modification]]
        ))
    }
  } else {
    #matching up 
    match_early = match(IPUMS_df[[variable_for_modification]], as.numeric(missing_crosswalk[[missing_crosswalk_early_year_col]]))
    
    #pull in values that are not 
    modified_ipums = IPUMS_df %>%
      mutate(!!variable_for_modification := case_when(
        IPUMS_df[[year_variable]] < crosswalk_years_foruse[1] & 
          IPUMS_df[[variable_for_modification]] %in% as.numeric(missing_crosswalk[[missing_crosswalk_early_year_col]]) ~ 
          as.numeric(missing_crosswalk[[missing_crosswalk_late_year_col]][match_early]),
        TRUE ~ IPUMS_df[[variable_for_modification]]
      ))
    
    #looping over the list of crosswalks
    for (i in 1:length(crosswalk_list)) {
      #crosswalk years
      col_names = names(crosswalk_list[[i]])
      col_years = str_extract(col_names, "\\d{4,}") %>% unique() %>% sort()
      col_names_ofinterest = paste0("(", paste(col_years, collapse = "|"), ").*census.*code.*") #HERE
      column_indices = grep(col_names_ofinterest, tolower(col_names)) %>% sort()
      
      #crosswalk df
      crosswalk_df = crosswalk_list[[i]]
      
      #clean crosswalk
      clean_crosswalk_df = crosswalk_df %>% 
        group_by_at(column_indices[1]) %>%
        filter(any(!duplicated(crosswalk_df[column_indices[1]])) | any(!is.na(crosswalk_df[column_indices[length(column_indices)]]))) %>%
        ungroup()
      
      #matches values
      match_early = match(modified_ipums[[variable_for_modification]], as.numeric(clean_crosswalk_df[[col_names[column_indices[1]]]]))
      
      #attach the value assigned in the crosswalk list according to their 
      modified_ipums = modified_ipums %>%
        mutate(!!variable_for_modification := case_when(
          crosswalk_end_year[i] > .data[[year_variable]] ~ as.numeric(clean_crosswalk_df[[column_indices[length(column_indices)]]][match_early]),
          TRUE ~ modified_ipums[[variable_for_modification]]
        ))
    }
  }
  
  #output
  return(modified_ipums)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
IPUMS_EDIT_1 <- modify_by_census_crosswalk(Mod_IPUMs_df, mod_with_zero_Industry_Crosswalk_list, 
                                           variable_for_modification = "IND", 
                                           missing_crosswalk = overwrite_crosswalk_note,
                                           missing_crosswalk_early_year_col = "X2002_code",
                                           missing_crosswalk_late_year_col = "X2007_code")

IPUMS_EDIT_2 <- modify_by_census_crosswalk(IPUMS_EDIT_1, mod_with_zero_Occupation_Crosswalk_master_list, 
                                           variable_for_modification = "OCC")

#label OCC and IND
IPUMS_EDIT_2$IND <- labelled(IPUMS_EDIT_2$IND, Ind_readied_labels)
IPUMS_EDIT_2$OCC <- labelled(IPUMS_EDIT_2$OCC, Occ_readied_labels)

# Creating Usable Data #########################################################
# Replace NIU with NA
NIU_Bucket <- c("N.I.U.", "NIU", "Not in universe", "Blank", "Do not know", "Don't Know", 
                "Refused", "Missing", "missing", "Unknown", "N/A", "No response", "Armed Forces")

replace_niu <- function(df, NA_bucket = NIU_Bucket) { #Function that replaces an IPUMS column of NIU or unknowns to an NA value
  NA_Combined = str_c(NA_bucket, collapse = "|") #Collapses the NIU bucket to a grouping of or statements
  col_labels_cps = var_label(df)
  if (!is.null(col_labels_cps) && !str_detect(col_labels_cps, "Data quality flag")) {
    labels_df = val_labels(df) # Get the value labels
    if (!is.null(labels_df)) {
      # Replace values labeled as NIU with NA
      df[as_factor(df) %in% names(labels_df)[str_detect(names(labels_df), NA_Combined)]] = NA
    }
  }
  return(df)
}

replace_niu_dataset <- function(data){ #This then runs the IPUMS column function above on a whole data set
  mod_data = data %>%
    mutate(across(everything(), ~replace_niu(.)))
  return(mod_data)
}

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
useable_IPUMS <- replace_niu_dataset(IPUMS_EDIT_2)

# Define a function to create the new variable with dynamic breaks and labels
concentrating_variable_values <- function(data, columns_info){
  for (column in names(columns_info)) {
    column_name = columns_info[[column]]$column_name
    breakpoints = columns_info[[column]]$breakpoints
    labels = columns_info[[column]]$labels
    new_column = columns_info[[column]]$column_name_overwrite
    
    # Ensure the breakpoints and labels are of the same length
    if (length(breakpoints) != length(labels)) {
      stop("The number of labels must be equal to the number of breakpoints.")
    }
    
    # Adjust the Breakpoints to include the necessary end state
    breakpoints_adj = c(breakpoints, Inf)
    
    if (length(new_column) < 1){
      # New Column Name
      new_column = adjust_name_by_count(column_name = column_name, data = data) 
    }
    
    # Use the cut function to create the new variable
    data[[new_column]] = cut(data[[column_name]], breaks = breakpoints_adj, labels = labels, right = FALSE)
    
  }
  return(data)
}

# Create a variable
useable_IPUMS_modification_info <- list(
  EDUC2 = list( #Splitting Education into 4 groups
    column_name = "EDUC",
    breakpoints = c(0, 70, 80, 110),
    labels = c("Less than high school", "High school", "Some college", "College or more")
  ),
  EDUC3 = list( #Splitting Education into 2 groups
    column_name = "EDUC",
    breakpoints = c(0, 110),
    labels = c("Non-college", "College")
  ),
  AGE2 = list(#spitting age into intervals of 10
    column_name = "AGE",
    breakpoints = c(0, 18, 25, 35, 45, 55, 65),
    labels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  ),
  HISPAN2 = list(
    column_name = "HISPAN",
    breakpoints = c(0, 100),
    labels = c("Not hispanic", "Hispanic")
  ),
  RACE2 = list(
    column_name = "RACE",
    breakpoints = c(100, 200, 300, 650, 700),
    labels = c("White", "Black", "Other", "Asian", "Other")
  ),
  CARE_WRK = list(
    column_name = "OCC",
    column_name_overwrite = "CARE_WRK",
    breakpoints = c(0, 2000, 2040, 3600, 3700, 4330, 4340, 4600, 4621, 4655, 4700),
    labels = c("Non-Care Profession", "Care Profession", "Non-Care Profession", "Care Profession", 
               "Non-Care Profession", "Care Profession", "Non-Care Profession", "Care Profession",
               "Non-Care Profession", "Care Profession", "Non-Care Profession")
  ),
  EMPSTAT2 = list(
    column_name = "EMPSTAT",
    breakpoints = c(10, 20, 30),
    labels = c("Employed", "Unemployed", "Not in Labor Force")
  ),
  Work_Time_Status = list(
    column_name = "WKSTAT",
    breakpoints = c(10, 20, 40, 50, 60),
    labels = c("Full-time schedules", "Part-time for economic reasons", "Part-time for non-economic reasons, usually part-time",
               "Unemployed, seeking full-time work", "Unemployed, seeking part-time work")
  )
)

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
useable_IPUMS2 <- concentrating_variable_values(useable_IPUMS, useable_IPUMS_modification_info)

#special data modifications - THIS IS OBVIOUSLY A BAD FUNCTION
# Making a new race variable to simplify and include latinos
new_variable_list <- list(
  RACE_HISP = list(#combining hispanic and race variable
    column_names = c("RACE", "HISP"),
    column_name_overwrite = "CARE_WRK",
    breakpoints = c(0, 70, 80, 110),
    labels = c("Less than high school", "High school", "Some college", "College or more")
  ))




adjust_name_by_concatenate <- function(data, ...) {
  # Capture the column names passed as arguments
  column_names = list(...)
  
  #initial concatenation
  new_name = paste(column_names, collapse = "_")
  
  #Current Column Names
  existing_columns = names(data)
  
  if(new_name %in% existing_columns){
    #adjust the new name by the counting new variable types
    new_name = adjust_name_by_count(new_name, data = data)
  }
  
  #output
  return(new_name)
}

combining_multiple_variables <- function(data, mapping_list) {
  if (length(new_column) < 1){
    # New Column Name
    new_column = adjust_name_by_concatenate(column_name = column_name, data = data) 
  }
  
  
  
  mod_data = data %>%
    rowwise() %>%
    mutate(!!new_column = {})
}





# Define the function
create_race_variable <- function(data, mapping_list) {
  data = data %>%
    rowwise() %>%
    mutate(new_race = {
      race <- NA_character_
      for (category in names(mapping_list)) {
        if (all(mapping_list[[category]])) {
          race <- category
          break
        }
      }
      race
    }) %>%
    ungroup()
  return(data)
}

# Example usage
# Create a sample dataframe
sample_data <- tibble(
  hispanic = c(TRUE, FALSE, TRUE, FALSE),
  black = c(FALSE, TRUE, TRUE, FALSE),
  white = c(FALSE, FALSE, FALSE, TRUE)
)

# Define the mapping list
mapping_list <- list(
  "Hispanic" = sample_data$hispanic,
  "Black" = sample_data$black,
  "White" = sample_data$white
)

# Apply the function
result <- create_race_variable(sample_data, mapping_list)

# Print the result
print(result)



# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
useable_IPUMS3 <- race_hispanic_mod(useable_IPUMS2, hispanic_col = "HISPAN.2", race_col = "RACE.2")


#before the summarization step need code that looks at the variables of interest and terms them into factor (as_factor())

# Find Summarized Results ######################################################

conditions <- exprs(
  PERNUM == 1,
  AGE >= 25,
  AGE < 65
)


weighted_counting_ipums <- function(ipums_df, column_names, count_value, conditions = list()) {
  #code run
  summary_df = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    group_by(across(all_of(column_names))) %>%
    summarise(count = sum({{count_value}}, na.rm = T)) %>% #count the weighted values
    ungroup() %>%
    filter(across(all_of(column_names)), ~ !is.na(.)) %>% #remove na values
    mutate(pct = 100*count/sum(count)) #create a percent of the groups
  
  #output
  return(summary_df)
}

weighted_mean_ipums <- function(ipums_df, weight_value, numeric_value, conditions = list()) {
  #summary run
  summary_mean = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) %>% #cut out NA values
    group_by(across(all_of(column_names))) %>%
    summarise(weighted_mean = weighted.mean(x = {{numeric_value}}, w = {{weighted_value}}),
              weighted_sd = sqrt(wtd.var(x = {{numeric_value}}, weights = {{weight_value}})),
              weighted_se = weighted_sem(x = {{numeric_value}}, w = {{weight_value}}, na.rm = T)
    )
  
  #output
  return(summary_mean)
}

for_use_prob_vector = c(0, .25, .5, .75, 1)

weighted_quantiles_ipums <- function(ipums_df, weight_value, numeric_value, prob_vector = c(0, .25, .5, .75, 1), conditions = list()) {
  #summary run
  summary_quantiles = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) %>% #cut out NA values
    group_by(across(all_of(column_names))) %>%
    summarise(weighted_quantile = list(wtd.quantile({{numeric_value}}, weights = {{weight_value}}, probs = prob_vector))) %>%
    unnest(weighted_quantile) %>%
    mutate(quantile = rep(prob_vector, times = n() / length(prob_vector))) %>%
    pivot_wider(names_from = quantile, values_from = weighted_quantile, names_prefix = "weighted_q")
  
  #output
  return(summary_quantiles)
}

counting_ipums <- function(ipums_df, column_names, conditions = list()) {
  #code run
  summary_df = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    group_by(across(all_of(column_names))) %>%
    summarise(count = n(), .groups = 'drop') %>% #count
    ungroup() %>%
    filter(across(all_of(column_names)), ~ !is.na(.)) %>% #remove na values
    mutate(pct = 100*count/sum(count)) #create a percent of the groups
  
  #output
  return(summary_df)
}

mean_ipums <- function(ipums_df, numeric_value, conditions = list()) {
  #summary run
  summary_mean = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) %>% #cut out NA values
    group_by(across(all_of(column_names))) %>%
    summarise(mean = mean({{numeric_value}}),
              sd = sd({{numeric_value}}),
              se = sd / sqrt(length(data))
    )
  
  #output
  return(summary_mean)
}

for_use_prob_vector = c(0, .25, .5, .75, 1)

quantiles_ipums <- function(ipums_df, weight_value, numeric_value, prob_vector = c(0, .25, .5, .75, 1), conditions = list()) {
  #summary run
  summary_quantiles = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) %>% #cut out NA values
    group_by(across(all_of(column_names))) %>%
    summarise(quantile = list(quantile({{numeric_value}}, probs = prob_vector))) %>%
    unnest(quantile) %>%
    mutate(quantile2 = rep(prob_vector, times = n() / length(prob_vector))) %>%
    pivot_wider(names_from = quantile2, values_from = quantile, names_prefix = "quantile")
  
  #output
  return(summary_quantiles)
}

# Header ####
# Primary Author: Timothy Kusuma
# Pulled Code From: Anthony Colavito & Joshua Kendall (Two Economies Report Statistics)
# Date Started: 2/26/2025
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 2/26/2025
# Project: Women Non-College
# Title: Manual Pull

# Clean environment #####
rm(list = ls())
gc()

#libraries
library(haven)
library(survey)
library(srvyr)
library(ipumsr)
library(jtools)
library(remotes)
library(tidyverse)
library(htmltools)
library(shiny)
library(DT)
library(xlsx)
library(DescTools)
library(sf)
library(maptools)
library(colorspace)
library(matrixStats)
library(spatstat)
library(dineq)
library(data.table)
library(bit64)
library(here)

# Import data #### Ok here I am just taking from my code in the non-college statistical component after removing all NA Values
data <- useable_IPUMS

# Modify Data As Needed ##########################################################################
#Education Variable
data$EDUC2 <- lbl_collapse(data$EDUC, ~.val %/% 10) %>% # Creating a new education variable thats divided into 4 groups
  lbl_relabel(
    lbl(2, "Less than high school") ~.val >= 0 & .val < 7,
    lbl(3, "High school") ~.val == 7,
    lbl(4, "Some college") ~.val > 7 & .val < 11,
    lbl(5, "College or more") ~.val >= 11
  ) 

data$EDUC3 <- lbl_relabel(data$EDUC, #Creating a third education variable thats divided into two groups
                          lbl(2, "Non-college") ~.val < 111, 
                          lbl(3, "College") ~.val >= 111
) %>%
  as_factor()

data$EDUC2 <- data$EDUC2 %>%
  as_factor()

# Making a new race variable to simplify and include latinos
data$HISPAN2 <- lbl_relabel(data$HISPAN,
                            lbl(1, "Not hispanic") ~.val == 0,
                            lbl(2, "Hispanic") ~.val != 0 & .val != 902
)

data$RACE1 <- data$RACE %>%
  lbl_collapse(~.val %/% 10) %>%
  lbl_relabel(
    lbl(1, "White") ~.val == 10,
    lbl(2, "Black") ~.val == 20,
    lbl(3, "Asian") ~.val == 65,
    lbl(4, "Other") ~.val == 30 | .val >= 70 & .val < 99
  ) %>%
  lbl_clean()

data <- data %>%
  mutate(
    RACE2 = case_when(
      HISPAN2 == 2 ~ "Hispanic",
      RACE1 == 1 ~ "White",
      RACE1 == 2 ~ "Black",
      RACE1 == 3 ~ "Asian",
      RACE1 == 4 ~ "Other"
    )
  )

#Modifying occupation and industry categories for simplification
data <- data %>%
  mutate(maj_occ = case_when( #I used the family splits as in the census occupation and industries lists: https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
    OCC >= 10 & OCC <= 3550 ~ "Management, Business, Science, and Arts",
    OCC >= 3601 & OCC <= 4655 ~ "Service",
    OCC >= 4700 & OCC <= 5940 ~ "Sales and Office Occupations",
    OCC >= 6005 & OCC <= 7640 ~ "Natural Resources, Construction, and Maintenance",
    OCC >= 7700 & OCC <= 9760 ~ "Production, Transportation, and Material Moving",
    OCC >= 9800 & OCC <= 9830 ~ "Military Specific",
    OCC > 9830 ~ "Unemployed"
  ),
  min_occ = case_when( 
    OCC >= 10 & OCC <= 960 ~ "Management, business, and financial",
    OCC >= 1005 & OCC <= 1980 ~ "Computer, Engineering, and Science Occupations",
    OCC >= 2001 & OCC <= 2970 ~ "Education, Legal, Community Service, Arts, and Media",
    OCC >= 3000 & OCC <= 3550 ~ "Healthcare Practitioners and Technical",
    OCC >= 3601 & OCC <= 4655 ~ "Service",
    OCC >= 4700 & OCC <= 4965 ~ "Sales and related",
    OCC >= 5000 & OCC <= 5940 ~ "Office and administrative support",
    OCC >= 6005 & OCC <= 6130 ~ "Farming, fishing, and forestry",
    OCC >= 6200 & OCC <= 6950 ~ "Construction and extraction",
    OCC >= 7000 & OCC <= 7640 ~ "Installation, maintenance, and repair",
    OCC >= 7700 & OCC <= 8990 ~ "Production",
    OCC >= 9005 & OCC <= 9760 ~ "Transportation and material moving",
    OCC >= 9800 & OCC <= 9830 ~ "Military Specific",
    OCC > 9830 ~ "Unemployed"
  ),
  sub_min_occ = case_when(
    OCC >= 10 & OCC <= 440 ~ "Management",
    OCC >= 500 & OCC <= 960 ~ "Business and financial operations",
    OCC >= 1005 & OCC <= 1240 ~ "Computer and mathematical science",
    OCC >= 1305 & OCC <= 1560 ~ "Architecture and engineering",
    OCC >= 1600 & OCC <= 1980 ~ "Life, physical, and social science",
    OCC >= 2001 & OCC <= 2060 ~ "Community and social service",
    OCC >= 2100 & OCC <= 2180 ~ "Legal",
    OCC >= 2205 & OCC <= 2555 ~ "Education Instruction, and library",
    OCC >= 2600 & OCC <= 2970 ~ "Arts, design, entertainment, sports, and media",
    OCC >= 3000 & OCC <= 3550 ~ "Healthcare Practicioners and Technical",
    OCC >= 3601 & OCC <= 3655 ~ "Healthcare support",
    OCC >= 3700 & OCC <= 3960 ~ "Protective service",
    OCC >= 4000 & OCC <= 4160 ~ "Food preparation and serving related",
    OCC >= 4200 & OCC <= 4255 ~ "Building and grounds cleaning and maintenance",
    OCC >= 4330 & OCC <= 4655 ~ "Personal care and service",
    OCC >= 4700 & OCC <= 4965 ~ "Sales and related",
    OCC >= 5000 & OCC <= 5940 ~ "Office and administrative support",
    OCC >= 6005 & OCC <= 6130 ~ "Farming, fishing, and forestry",
    OCC >= 6200 & OCC <= 6950 ~ "Construction and extraction",
    OCC >= 7000 & OCC <= 7640 ~ "Installation, maintenance, and repair",
    OCC >= 7700 & OCC <= 8990 ~ "Production",
    OCC >= 9005 & OCC <= 9430 ~ "Transportation",
    OCC >= 9510 & OCC <= 9760 ~ "Production",
    OCC >= 9800 & OCC <= 9830 ~ "Military Specific",
    OCC > 9830 ~ "Unemployed"
  ),
  collar = case_when(
    maj_occ == "Management, Business, Science, and Arts" ~ "White collar",
    maj_occ == "Service" ~ "Blue collar",
    maj_occ == "Sales and Office Occupations" ~ "White collar",
    maj_occ ==  "Natural Resources, Construction, and Maintenance" ~ "Blue collar",
    maj_occ ==  "Production, Transportation, and Material Moving" ~ "Blue collar",
    maj_occ == "Military Specific" ~ NA,
    maj_occ == "Unemployed" ~ NA
  ),
  maj_ind = case_when(
    IND >= 170 & IND <= 490 ~ "Agriculture, forestry, fishing, and hunting, and mining",
    IND == 770 ~ "Construction",
    IND >= 1070 & IND <= 3990 ~ "Manufacturing",
    IND >= 4070 & IND <= 4590 ~ "Wholesale trade",
    IND >= 4670 & IND <= 5791 ~ "Retail trade",
    IND >= 6070 & IND <= 6390 ~ "Transportation and utilities",
    IND >= 570 & IND <= 690 ~ "Transportation and utilities",
    IND >= 6471 & IND <= 6781 ~ "Information",
    IND >= 6871 & IND <= 7190 ~ "Financial and Insurance, and Real estate, and Rental and leasing",
    IND >= 7270 & IND <= 7790 ~ "Professional, Scientific, and Management, and Administreateve, and Waste Managemment",
    IND >= 7860 & IND <= 8470 ~ "Education and health care and social assistance",
    IND >= 8561 & IND <= 8690 ~ "Arts, Entertainement, and Recreation, and Accomodation and Food Services",
    IND >= 8770 & IND <= 9290 ~ "Other Services, Except Public Administration",
    IND >= 8770 & IND <= 9290 ~ "Other services",
    IND >= 9370 & IND <= 9590 ~ "Public administration",
    IND >= 9670 & IND <= 9870 ~ "Military",
    IND > 9870 ~ "Unemployed"
  ),
  min_ind = case_when(
    IND >= 170 & IND <= 290 ~ "Agriculture, forestry, fishing, and hunting",
    IND >= 370 & IND <= 490 ~ "Mining, quarrying, and oil and gas extraction",
    IND == 770 ~ "Construction",
    IND >= 1070 & IND <= 3990 ~ "Manufacturing",
    IND >= 4070 & IND <= 4590 ~ "Wholesale trade",
    IND >= 4670 & IND <= 5791 ~ "Retail trade",
    IND >= 6070 & IND <= 6390 ~ "Transportation and Warehousing",
    IND >= 570 & IND <= 690 ~ "Utilities",
    IND >= 6471 & IND <= 6781 ~ "Information",
    IND >= 6871 & IND <= 6992 ~ "Finance and insurance",
    IND >= 7071 & IND <= 7190 ~ "Rental Estate and Rental and Leasing",
    IND >= 7270 & IND <= 7490 ~ "Professional, Scientific, and Technical",
    IND == 7570 ~ "Management of Companies and Enterprises",
    IND >= 7580 & IND <= 7790 ~ "Administrative and Support and Waste Management",
    IND >= 7860 & IND <= 7890 ~ "Educational Services",
    IND >= 7970 & IND <= 8470 ~ "Health Care and Social Assistance",
    IND >= 8561 & IND <= 8590 ~ "Arts, Entertainement, and Recreation",
    IND >= 8660 & IND <= 8690 ~ "Accomodation and food",
    IND >= 8770 & IND <= 9290 ~ "Other services, Except Public Administration",
    IND >= 9370 & IND <= 9590 ~ "Public administration",
    IND >= 9670 & IND <= 9870 ~ "Military",
    IND > 9870 ~ "Unemployed"
  ))

#See CPS occupational codes, the definition of care work could easily be shifted.
#This definition is quite narrow
data <- data %>%
  mutate(care_wrk = ifelse(
    OCC %in% c(2001:2025,3601:3655,4330,4600,4655),
    "Care profession", "Non-care profession"
  ))

#Modifying age variables
data$AGE2 <- as.numeric(data$AGE)
data <- data %>%
  mutate(age_cat = case_when(
    AGE2 < 18 ~ "Under 18",
    AGE2 >= 18 & AGE2 <= 24 ~ "18-24",
    AGE2 > 24 & AGE2 <= 44 ~ "25-44",
    AGE2 > 44 & AGE2 <= 64 ~ "45-64",
    AGE2 > 65 ~ "65+"
  )) %>%
  mutate(age_cat = factor(age_cat,
                          levels = c("Under 18","18-24","25-44", "45-64","65+")))

#Employment status
data$EMPSTAT2 <- lbl_collapse(data$EMPSTAT, ~.val %/% 10)
data$WKSTAT2 <- lbl_collapse(data$WKSTAT, ~.val %/% 10)

# Household income
hh_heads <- select(filter(data, RELATE == 0101), HHINCOME, ASECWTH)
hh_inc_tiles_all <- as.numeric(Quantile(hh_heads$HHINCOME, weights = hh_heads$ASECWTH, probs = seq(0,1,.01), na.rm = TRUE))

data <- data %>%
  mutate(hh_inc_quantile = cut(HHINCOME, hh_inc_tiles_all, label = FALSE),
         test_quintile = ntile(HHINCOME*ASECWTH, 100))


#Sex
data$SEX2 <- data$SEX %>% as_factor()
data$WHYPTLY2 <- data$WHYPTLY %>% as_factor()
data$EMPSTAT2 <- data$EMPSTAT2 %>% as_factor()
data <- data %>% mutate(Earnyearly = as.numeric(EARNWEEK2)*52)
data <- data %>% mutate(NotWorking = if_else(EMPSTAT2 == "At work", "Working", "Not Working"))

data = data %>% mutate(ASECWT2 = case_when(
  YEAR >= 2019 & YEAR <= 2021 ~ ASECWTCVD,
  YEAR == 2014 & HFLAG == 2 ~ NA_integer_,
  TRUE ~ ASECWT
))

# Check Harmonized Variables ########################################################

data %>%
  mutate(
    WHYPTLWK2 = case_when(
      WHYPTLWK %in% c(121,122) ~ "family/personal obligations",
      is.na(WHYPTLWK) ~ NA_character_,
      WHYPTLWK %in% c(0) ~ NA_character_,
      TRUE ~ "Other"),
    EDUC3 = case_when(
      EDUC == 0 ~ NA_character_,
      EDUC < 111 ~ "Non-College",
      EDUC >= 111 ~ "College"),
    AGE2 = as.numeric(AGE),
    age_cat = case_when(
      AGE2 < 18 ~ "Under 18",
      AGE2 >= 18 & AGE2 <= 24 ~ "18-24",
      AGE2 > 24 & AGE2 <= 44 ~ "25-44",
      AGE2 > 44 & AGE2 <= 64 ~ "45-64",
      AGE2 > 65 ~ "65+"
    ),
    SEX2 = as_factor(SEX)
  ) %>%
  filter(YEAR == 2024 & AGE >= 25 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, WHYPTLWK2) %>%
  summarise(count = n(),
            weight_count = sum(ASECWT),
  ) %>%
  ungroup(WHYPTLWK2) %>%
  filter(!is.na(WHYPTLWK2)) %>%
  mutate(pct = 100*weight_count/sum(weight_count)) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         sd = sqrt(sum(count)*(proportion*(1-proportion))),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         mock_interpretation = se/proportion,
         pct = 100*count/sum(count))

person_cps_upload <- read.csv(here("Data", "2024 Census CPS", "pppub24.csv"))

person_cps_upload %>%
  mutate(
    PRPTREA2 = case_when(
      PRPTREA %in% c(7, 8, 17, 18) ~ "family/personal obligations",
      is.na(PRPTREA) ~ NA_character_,
      PRPTREA %in% c(-1, 0) ~ NA_character_,
      TRUE ~ "Other"),
    EDUC3 = case_when(
      A_HGA == 0 ~ NA_character_,
      A_HGA < 43 ~ "Non-College",
      A_HGA >= 43 ~ "College",
      TRUE ~ NA_character_),
    age_cat = case_when(
      A_AGE < 18 ~ "Under 18",
      A_AGE >= 18 & A_AGE <= 24 ~ "18-24",
      A_AGE > 24 & A_AGE <= 44 ~ "25-44",
      A_AGE > 44 & A_AGE <= 64 ~ "45-64",
      A_AGE > 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    A_SEX2 = case_when(
      A_SEX == 1 ~ "Male",
      A_SEX == 2 ~ "Female"
    )
  ) %>%
  filter(A_AGE >= 25 & A_AGE < 65) %>%
  group_by(EDUC3, A_SEX2, age_cat, PRPTREA2) %>%
  summarise(count = n()
  ) %>%
  ungroup(PRPTREA2) %>%
  filter(!is.na(PRPTREA2)) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         sd = sqrt(sum(count)*(proportion*(1-proportion))),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         mock_interpretation = se/proportion,
         pct = 100*count/sum(count)) %>%
  summarise(count = sum(count))


ddi = read_ipums_ddi(here("Data", "IPUMS Data", "cps_00013.xml"))
IPUMS_data = read_ipums_micro(ddi, data_file = here("Data", "IPUMS Data", "cps_00013.dat"))

IPUMS_data %>%
  mutate(
    UH_PTREAS_A3_2 = case_when(
      WKSTAT %in% c(10,11,13,14,15,50,60,99) ~ NA_character_,
      UH_PTREAS_A3 %in% c(7, 8, 17, 18) ~ "family/personal obligations",
      is.na(UH_PTREAS_A3) ~ NA_character_,
      UH_PTREAS_A3 %in% c(0) ~ NA_character_,
      TRUE ~ "Other"),
    UH_GRDATN_A1_2 = case_when(
      UH_GRDATN_A1  == 0 ~ NA_character_,
      UH_GRDATN_A1 < 43 ~ "Non-College",
      UH_GRDATN_A1 >= 43 ~ "College"),
    age_cat = case_when(
      UH_AGE_A5 < 18 ~ "Under 18",
      UH_AGE_A5 >= 18 & UH_AGE_A5 <= 24 ~ "18-24",
      UH_AGE_A5 > 24 & UH_AGE_A5 <= 44 ~ "25-44",
      UH_AGE_A5 > 44 & UH_AGE_A5 <= 64 ~ "45-64",
      UH_AGE_A5 > 65 ~ "65+"
    ),
    SEX2 = as_factor(SEX)
  ) %>%
  filter(YEAR == 2024 & UH_AGE_A5 >= 25 & UH_AGE_A5 < 65) %>%
  group_by(UH_GRDATN_A1_2, SEX2, age_cat, UH_PTREAS_A3_2) %>%
  summarise(count = n(),
            weight_count = sum(UH_WGTS_A1),
  ) %>%
  ungroup(UH_PTREAS_A3_2) %>%
  filter(!is.na(UH_PTREAS_A3_2)) %>%
  mutate(pct = 100*weight_count/sum(weight_count)) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         sd = sqrt(sum(count)*(proportion*(1-proportion))),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         mock_interpretation = se/proportion,
         pct = 100*count/sum(count))



IPUMS_data %>%
  filter(AGE >= 15) %>%
  mutate(UH_PTREAS_A3_2 = as_factor(UH_PTREAS_A3),
         WHYPTLWK2 = as_factor(WHYPTLWK)) %>%
  mutate(Look1 = paste0(UH_PTREAS_A3_2," + ", WHYPTLWK2)) %>%
  #select(c(Look1, UH_PTREAS_A3_2, WHYPTLWK2)) %>%
  count(Look1, UH_PTREAS_A3_2, WHYPTLWK2, name = "Look1_count") %>%
  distinct(Look1, .keep_all = TRUE) %>%
  add_count(UH_PTREAS_A3_2, name = "UH_PTREAS_A3_2_count") %>%
  arrange(UH_PTREAS_A3_2) %>%
  View()


# Running Cuts ######################################################################

data_listy = list()

for (i in 2004:2024) {
  data_listy[[i]] = data %>% #Question 1: Why Part Time Last Week
    filter(YEAR == i & AGE > 24 & AGE < 65) %>%
    group_by(EDUC3, SEX2, age_cat) %>%
    summarise(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count >= people_in_survey & count >= 30
    )
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)


###########################################################################################################################


data <- data %>% mutate(WHYPTLWK2 = case_when(
  WHYPTLWK %in% c(121,122) ~ "family/personal obligations",
  is.na(WHYPTLWK) ~ NA_character_,
  TRUE ~ "Other"
))

data_listy = list()

precision = 0.025
z_score = 1.96

for (i in 2004:2024) {
  data_listy[[i]] = data %>% #Question 3: Why Part Time Last Week
    filter(YEAR == i & AGE > 24 & AGE < 65) %>%
    group_by(EDUC3, SEX2, age_cat, USFTPTLW, WHYPTLWK2) %>%
    summarise(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYPTLWK2)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count >= people_in_survey & count >= 30
    )
}

bind_rows(data_listy) %>% 
  filter(USFTPTLW == 1) %>%
  select(year, everything()) %>%
  select(-USFTPTLW) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)


###########################################################################################################################

data_listy = data %>% #Pop
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Race
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, RACE2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Collar
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, collar) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(collar)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Labor Force Participation/Employment
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, EMPSTAT2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(EMPSTAT2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Earnings yearly
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat) %>%
  summarise(year = 2024,
            count = n(),
            weight_mean = weighted.mean(EARNWEEK2*52, EARNWT, na.rm = T),
  ) %>%
  ungroup()

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Earnings weekly
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat) %>%
  summarise(year = 2024,
            count = n(),
            weight_mean = weighted.mean(EARNWEEK2, EARNWT, na.rm = T),
  ) %>%
  ungroup()

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data_listy = data %>% #Not Working
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, NotWorking) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(NotWorking)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data$SPMPOV2 <- data$SPMPOV %>% as_factor()
data_listy = data %>% #Poverty
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, SPMPOV2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(SPMPOV2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data$ANYCOVNW2 <- data$ANYCOVNW %>% as_factor()
data_listy = data %>% #Any Health Care
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, ANYCOVNW2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(ANYCOVNW2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data <- data %>% mutate(UNION2 = if_else(UNION == 2, "Union Member", "Not Union Member"))
data_listy = data %>% #Union
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, UNION2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(UNION2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

data$DISABWRK2 <- data$DISABWRK %>% as_factor()
data_listy = data %>% #Disability
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, DISABWRK2) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(DISABWRK2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

################################################################################

data = data %>%
  mutate(WKSTAT2 = as_factor(WKSTAT2),
         WKSTAT3 = case_when(
           EMPSTAT2 == "Not in labor force" ~ "Not in Labor Force",
           WKSTAT2 %in% c("Unemployed, seeking full-time work", "Unemployed, seeking part-time work") ~ "Unemployed",
           is.na(WKSTAT2) ~ NA_character_,
           TRUE ~ as.character(WKSTAT2)
         ))

data = data %>%
  mutate(Child_HH = ifelse(NCHILD > 0, "Children", "No Children"))


data_listy = list()

for (i in 2004:2024) {
  data_listy[[i]] = data %>% #Question 3: Part Time/Full Time
    filter(YEAR == i & AGE >= 25 & AGE < 65) %>%
    group_by(EDUC3, SEX2, age_cat, WKSTAT3) %>%
    summarise(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup(WKSTAT3) %>%
    filter(!is.na(WKSTAT3)) %>%
    ungroup() %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count > people_in_survey & count > 30)
}


bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)



################################################################################


data_listy = data %>% #Disability
  filter(YEAR == 2024 & AGE > 24 & AGE < 65) %>%
  group_by(EDUC3, SEX2, age_cat, care_wrk) %>%
  summarise(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(care_wrk)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = count >= people_in_survey & count >= 30
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)


# create whynotfulltime value #####################################################


data = data %>%
  mutate(WHYABSNT2 = as_factor(WHYABSNT),
         WHYPTLWK2 = as_factor(WHYPTLWK),
         WHYNFT = case_when(
           WHYABSNT %in% c(7,8,9) ~ "Family/personal Obligation",
           WHYPTLWK %in% c(121,122) ~ "Family/personal Obligation",
           is.na(WHYABSNT) & is.na(WHYPTLWK) ~ NA_character_,
           TRUE ~ "Other"
         ))


data_listy = list()

for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter(YEAR == i & AGE >= 25 & AGE < 65) %>%
    group_by(EDUC3, SEX2, WHYNFT) %>%
    summarise(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup(WHYNFT) %>%
    filter(!is.na(WHYNFT)) %>%
    ungroup() %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count > people_in_survey & count > 30)
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)