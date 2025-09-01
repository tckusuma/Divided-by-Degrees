# Header ####
# Primary Author: Timothy Kusuma
# Pulled Code From: Anthony Colavito & Joshua Kendall
# Date Started: 2/4/2024
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 2/4/2024
# Project: IPUMS Data Analysis
# Title: Functions

# The Following are all the IPUMS related functions.

# Libraries ####
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

# Data Frame Sorting Functions #################################################
# Data Frame Sorting Function 1.) first_row_column_name()
# iterates through columns of a df and puts either the first row element as the column name or the column number
# data: should be of df storage type
first_row_column_name <- function(data) {
  # Iterate through each column
  for (i in 1:ncol(data)) {
    # Check if the first row value is NA or empty
    if (is.na(data[1, i]) || data[1, i] == "") {
      # Set the column name to the column number
      colnames(data)[i] = paste0("Column", i)
    } else {
      # Set the column name to the value in the first row
      colnames(data)[i] = data[1, i]
    }
  }
  # Remove the first row as it is now used as column names
  data = data[-1, ]
  
  #output
  return(data)
}

# Data Frame Sorting Function 2.) find_column_name()
# finds the column names row placing that as the column name and erasing everything above.
# data: should be of storage type data frame
# column_name: character variable that is the name of the column to find
find_column_name <- function(data, column_name, remove_NA = T) {
  #find the column name in the data frame
  matching = apply(data, 2, function(column) grepl(column_name, column))
  
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
  data_df_column_corrected = data[row_index:nrow(data), ]
  
  #Collapse the name
  data_df_name_corrected = first_row_column_name(data_df_column_corrected)
  
  if (remove_NA == T) {
    #Drop NA values
    data_df_name_corrected = data_df_name_corrected[!is.na(data_df_name_corrected[, column_index]), ]
  }
  
  #output
  return(data_df_name_corrected)
}


# General Use Functions ########################################################
# GN Function 1.) normalize_character()
# in a string removes spaces and convert to lowercase
normalize_character <- function(x) {
  tolower(gsub("\\s+", "", x))
}

# GN Function 1.) to sort columns in a data frame alphabetically
# data: data of type data frame
sort_columns_alphabetically <- function(data) {
  data %>% select(order(names(data)))
}

# Generic Function 1.) adjust_name_by_count()
# creating a new variable name adding a numeric counter to the number of variables with that name.
# column_name: name of the column name being adjusted
# data: data frame the column exists in
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

# Generic Function 2.) adjust_name_by_concatenate()
# creating a new variable name combining multiple column names
# data: data frame the columns exists in
# ...: column names
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



# Data Uploading Function ######################################################
# Pulls in functions based off of location. Can pick up types (.csx, .xls, .xlsx, .dat, and .xml)
#xls_pull_type: 
## "first": pulls just the first tab, 
## "labels": pulls the first tab that is not an overview, 
## "cross walk": pulls the tab that says walk.
upload_data_by_folder <- function(folder_location, csv_or_xls_name_convention, date_overwrite = NA, xls_pull_type = "first"){
  #List of the folders in the file
  xls_files <- list.files(path = folder_location, pattern = "*.xls", full.names = TRUE)
  csv_files <- list.files(path = folder_location, pattern = "*.csv", full.names = TRUE)
  xml_files <- list.files(path = folder_location, pattern = "*.xml", full.names = TRUE)
  dat_files <- list.files(path = folder_location, pattern = "*.dat", full.names = TRUE)
  
  #Check to ensure that enough information exists to run this code
  if (length(xls_files) == 0 && length(csv_files) == 0 && (length(xml_files) == 0 || length(dat_files) == 0)) {
    stop("Unable to upload. Missing files.")
  }
  
  #Upload Based on File Types
  if (length(xls_files) > 0) {
    #Arbitrary list to pull df
    xls_list = list()
    
    #for loop to pull data based on tab location
    for (i in 1:length(xls_files)) {
      #Create the year values
      if (is.na(date_overwrite)){
        xls_year_name = str_extract(basename(xls_files), "\\d+")[i]
      } else {
        warning("upload_data_by_folder is unable to check file order. Check if date_overwrite is in the desired order corresponding with file order in folder_location")
        xls_year_name = date_overwrite[i]
      }
      
      #Create new df
      df_name = paste0(csv_or_xls_name_convention, "_", xls_year_name)
      
      #Get the Tab Name
      tab_names = excel_sheets(xls_files[i])
      
      #Pull data based on pull type
      if (xls_pull_type == "first") {
        #Choose the first sheet
        xls_data = read_excel(xls_files[i], sheet = tab_names[1])
      } else if (xls_pull_type == "labels") {
        #Choose the first sheet unless "overview" is the first sheet
        if (tolower(tab_names[1]) == "overview") {
          # Read the second sheet
          xls_data = read_excel(xls_files[i], sheet = tab_names[2])
        } else {
          # Read the first sheet
          xls_data = read_excel(xls_files[i], sheet = tab_names[1])
        }
      } else if (xls_pull_type == "cross walk") {
        crosswalk_name_index = grep("(?i)(\\d{4}.*walk|walk.*\\d{4})", tab_names)
        if (length(crosswalk_name_index) > 0) {
          xls_data = read_excel(xls_files[i], sheet = tab_names[crosswalk_name_index])
          attr(xls_data, "year") = xls_year_name
        } else {
          next #skip excel files missing crosswalk
        }
      }
      
      # Store the data frame in the list
      xls_list[[df_name]] = xls_data
    }
  }
  if (length(csv_files) > 0) {
    #Arbitrary list to pull df
    csv_list = list()
    
    #for loop is to combine in the file names
    for (i in 1:length(csv_files)) {
      #Create the year values
      if (is.na(date_overwrite)){
        csv_year_name = str_extract(basename(csv_files), "\\d+")[i]
      } else {
        warning("upload_data_by_folder is unable to check file order. Check if date_overwrite is in the desired order corresponding with file order in folder_location")
        csv_year_name = date_overwrite[i]
      }
      
      #Create new df
      csv_df_name = paste0(csv_or_xls_name_convention, "_", csv_year_name)
      
      #upload csv
      csv_data = read_csv(csv_files[i])
      
      #Store the data frame in the list
      csv_list[[csv_year_name[i]]] = csv_data
    }
  }
  if (length(dat_files) > 0 && length(dat_files) > 0) {
    #Pulling file names
    xml_base_names_no_ext = sub("\\.xml$", "", basename(xml_files))
    dat_base_names_no_ext = sub("\\.dat.gz$", "", basename(dat_files))
    
    #Aligning the IPUMS data frames
    aligned_IPUMS_names <- data.frame(
      xml_files = xml_files[match(dat_base_names_no_ext, xml_base_names_no_ext)],
      dat_files = dat_files
    )
    
    #Arbitrary list to pull df
    ipums_list = list()
    
    #upload IPUMS
    for (i in 1:length(dat_files)) {
      #Create New df Name
      cps_val = str_extract(aligned_IPUMS_names[i,1], "cps_\\d+")
      IPUMS_df_name = paste0("IPUMS_", cps_val)
      
      #IPUMS Extract
      ddi = read_ipums_ddi(aligned_IPUMS_names[i,1])
      IPUMS_data = read_ipums_micro(ddi, data_file = aligned_IPUMS_names[i,2])
      
      #Store the data frame in the list
      ipums_list[[IPUMS_df_name]] = IPUMS_data
    }
  }
  
  # Initialize an empty list to store the lists that exist
  existing_lists = list()
  
  # Check if each list exists and add it to the existing_lists
  if (exists("xls_list")) {
    existing_lists = c(existing_lists, xls_list)
  }
  if (exists("csv_list")) {
    existing_lists = c(existing_lists, csv_list)
  }
  if (exists("ipums_list")) {
    existing_lists = c(existing_lists, ipums_list)
  }
  
  #order list before exporting
  existing_lists = existing_lists[order(names(existing_lists))]
  
  #output
  return(existing_lists)
}

# IPUMS Data Merging ###########################################################

## Find Method of Combining ASEC info into a monthly formatting instead of a yearly formatting
## Check that the adjustment made works may need to figure out how the CPSID value works.
## since we are creating a year or month frequency separation we should probably create a time variable if year frequency looks like 2004 and if monthly looks like mar2004 think about putting in function 2 after function 1 call
## another edit in second function need to find a method that does not cut out all values that are not ASEC flag so that monthly frequency can be preserved if desired.

# IPUMS Function 1.) merging_IPUMS_ym()
# merges data frame to build one of year or month frequency. Uses merging bucket as determinant for merging.
# list_data_m/list_data_y: list of df of either month or year type
# merging_bucket: character vector of column names to merge over
# merge_to_frequency: "year", "month"
merging_IPUMS_ym <-  function(list_data_m, list_data_y, merging_bucket, merge_to_frequency = "year") {
  #Merge the lists of data into one df
  data_y = list_data_y %>% reduce(full_join, by = merging_bucket)
  data_m = list_data_m %>% reduce(full_join, by = merging_bucket)
  
  #Either Merge to Have a Year or a month IPUMS df
  if (merge_to_frequency == "year") {
    #year merge
    merged_df = left_join(data_y, data_m, by = merging_bucket)
  }
  if (merge_to_frequency == "month") {
    #month merge
    merged_df = left_join(data_m, data_y, by = merging_bucket)
  }
  
  #output
  return(merged_df)
}

# IPUMS Function 2.) IPUMS_List_Condensing()
# pulls apart the IPUMS data list and flags month frequency versus year frequency data.
# ipums_data_list: last of IPUMS data sets
# flag: variable name that flags whether something is from the year or month supplement
# merge_to_frequency: "year", "month"
IPUMS_List_Condensing <- function(ipums_data_list, flag = "ASECFLAG", merging_bucket, merge_to_frequency = "year") {
  #setting in the internal lists for separation
  monthly_ipums = list()
  yearly_ipums = list()
  
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

# Crosswalks & Labels Functions ################################################

## Make sure you build out a system to pull out family names and label those names

# Label function 1.) Collapse_df_in_list()
# take a master list of data and adjust the dfs within correcting the column names and data start.
# data_list: list of data frames
# column_description_last_word: character vector with words specific to description column name such that it can be easily found
# column_code_contains: character vector with words specific to code column name such that it can be easily found
# label_name_overwrite: character vector of names if the labeling adjustment does not exist
# code_name_overwrite: numeric vector of columns corresponding to and same size as label_name_overwrite specifically if the labeling adjustment does not exist
Collapse_df_in_list <- function(data_list, column_description_last_word, column_code_contains, 
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
    
    #The name of the column of interest
    column_name = paste0("(?i).*\\b.*\\b(", year_labeling_adj, ")\\b.*\\b(", mod_column_code_contains, ")\\b.*\\bcode$") #the name of the code column
    
    #Adjust the column name by finding the variable of interest
    mod_census_df = find_column_name(census_label_df, column_name = column_name)
    
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

# Label function 2.) separate_labels_census()
# Separate out the labels from a label list.
# data_list: list of data frames
# column_code_contains: character vector with words specific to code column name such that it can be easily found
separate_labels_census <- function(data_list, column_code_contains) {
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

# Label function 3.) separate_family_census()
# Separate out the family names from a label list.
# data_list: list of data frames
# column_code_contains: character vector with words specific to code column name such that it can be easily found
separate_family_census <- function(data_list, column_code_contains) {
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

# Label function 4.) ready_labels()
# Pull the most recent label list to set up as a labeling vector
# column_description_last_word: character vector with words specific to description column name such that it can be easily found
# column_code_contains: character vector with words specific to code column name such that it can be easily found
ready_labels <- function(data_list, column_description_last_word, column_code_contains) {
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

##NEEDED EDIT (the df sometimes have values that are not codes we would be interested in see if there is an ability to cut check OCC crosswalk to 2018)

# Crosswalk Function 1.) census_crosswalk_to_data_interest()
# This function cleans crosswalk files to data that is of specific interest
# data_list: list of crosswalk data frames
# column_code_contains: character vector with words specific to code column name such that it can be easily found
# crosswalk_format: "last list": the most recent df contains all crosswalks, "split list": the crosswalk is split over multiple df (assuming the crosswalk is previous values to current)
census_crosswalk_to_data_interest <- function(data_list, column_code_contains, crosswalk_format) {
  #set up modifying list
  mod_data_list = list()
  
  #adjust column code info as needed
  mod_column_code_contains = paste(tolower(column_code_contains), collapse = "|")

  #Loops over to adjust to the data of interest
  for (i in 1:length(data_list)) {
    #Pull the df name
    original_df_name = names(data_list)[i]
    
    #Pull the year of the data_frame
    year_labeling_adj = str_extract(original_df_name, "\\d+")
    
    #The df of interest
    census_crosswalk_df = data_list[[i]]
    
    #The name of the column of interest
    column_name = paste0("(?i).*\\b.*\\b(", year_labeling_adj, ")\\b.*\\b(", mod_column_code_contains, ")\\b.*\\bcode$") #the name of the code column
    
    #Use Find Column Name to Collapse the DF
    mod_census_crosswalk_df = find_column_name(census_crosswalk_df, column_name = column_name)
    
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
      column_name = normalize_character(names(foruse_df)[i])
      current_index = which(normalize_character(names_foruse_df) == column_name)
      string_no_digits = gsub("\\d", "", column_name)
      
      #specifying years of interest
      current_year = str_extract(column_name, "\\d{4}")
      next_year = years_foruse_df[which(years_foruse_df == current_year) + 1]
      
      #next year names
      next_year_names = paste0(next_year, string_no_digits)
      next_year_index = which(normalize_character(names_foruse_df) == next_year_names)
      
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
      
      #normalize_character names
      names(foruse_df) = normalize_character(names(foruse_df))
      
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

## EDIT (need to add a check if zero exists in the code to ensure no duplication)
## think about finding a way to generalize this function so it can be added when needed

# Crosswalk Function 2.) add_zero()
# This function takes the crosswalk list and adds NIU value
# data_list: list of data frames
# specifier: what is the column value of interest such as to add a zero value
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

## EDIT (see if we can make inputs the same type)

# Internal Function to check coverage
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

## NEEDED EDIT (this function needs to fix some elements like making it easier to pick out the early state and the coverage of the crosswalk)
## see if we can make this more generic (less ipums specific)

# Crosswalk Function 3.) modify_by_census_crosswalk()
# This function takes the crosswalk files and IPUMS files and adjusts values In IPUMS accordingly
# IPUMS_df: data frame with values to be adjusted
# year_variable: name of column of YEAR variable
# variable_for_modification: name of variable being adjusted
# missing_crosswalk: crosswalk data frame of missing crosswalk values
# missing_crosswalk_early_year_col: name of early year in the missing_crosswalk data frame
# missing_crosswalk_late_year_col: name of late year in the missing_crosswalk data frame
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

# Creating Usable Data #########################################################
# Creating Usable Data Function 1.) replace_niu()
# Replace NIU with NA
# data_vec: vector being checked for NIU values
# NA_bucket: character vector of the labels that should be removed from the data
replace_niu <- function(data_vec, NA_bucket) { #Function that replaces an IPUMS column of NIU or unknowns to an NA value
  NA_Combined = str_c(NA_bucket, collapse = "|") #Collapses the NIU bucket to a grouping of or statements
  col_labels_cps = var_label(data_vec)
  if (!is.null(col_labels_cps) && !str_detect(col_labels_cps, "Data quality flag")) {
    labels_df = val_labels(data_vec) # Get the value labels
    if (!is.null(labels_df)) {
      # Replace values labeled as NIU with NA
      data_vec[as_factor(data_vec) %in% names(labels_df)[str_detect(names(labels_df), NA_Combined)]] = NA
    }
  }
  return(data_vec)
}

# Creating Usable Data Function 2.) replace_niu_dataset()
# run  the replace_niu over all columns in a data frame
# data: data frame over which replace_niu is run
replace_niu_dataset <- function(data){ #This then runs the IPUMS column function above on a whole data set
  mod_data = data %>%
    mutate(across(everything(), ~replace_niu(.)))
  return(mod_data)
}

# Creating Usable Data Function 3.) concentrating_variable_values()
# create new variables concentrating categories into a smaller number of categories
# data: data frame that will be adjusted
# columns_info: specially created list of lists containing the column name, breakpoints, new labels, and new column names
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

# holding off on fixing until done writing in the code to use functions
# Creating Usable Data Function 4.) combined_variable_values()
# create new variables pulling in values from multiple variables
combined_variable_values <- function(data, mapping_list) {
  data = data %>%
    rowwise() %>%
    mutate(new_variable = {
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

# Analysis Functions ###########################################################
# Analysis Function 1.) counting_ipums()
# counts all values within a specified category
# ipums_df: IPUMS data frame
# column_names: character vector of variables to group by
# conditions: for when we want to remove variables of a specific type
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

# Analysis Function 2.) mean_ipums()
# provides a mean, sd, and se split across all values within a specified category
# ipums_df: IPUMS data frame
# numeric_value: numeric variable taking mean of
# column_names: character vector of variables to group by
# conditions: for when we want to filter out specific data points
mean_ipums <- function(ipums_df, numeric_value, column_names = NULL, conditions = list()) {
  # Apply conditions
  filtered_df = ipums_df %>%
    filter(!!!conditions) %>% # Filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) # Cut out NA values
  
  # Check if column_names is NULL
  if (is.null(column_names)) {
    summary_mean = filtered_df %>%
      summarise(mean = mean({{numeric_value}}, na.rm = TRUE),
                sd = sd({{numeric_value}}, na.rm = TRUE),
                se = sd / sqrt(n()))
  } else {
    summary_mean = filtered_df %>%
      group_by(across(all_of(column_names))) %>%
      summarise(mean = mean({{numeric_value}}, na.rm = TRUE),
                sd = sd({{numeric_value}}, na.rm = TRUE),
                se = sd / sqrt(n()))
  }
  
  # Output
  return(summary_mean)
}

# Analysis Function 3.) quantiles_ipums()
# provides quantiles across the numeric_value according to split across all values within a specified category
# ipums_df: IPUMS data frame
# numeric_value: numeric variable finding the quantile of
# column_names: character vector of variables to group by
# conditions: for when we want to filter out specific data points
quantiles_ipums <- function(ipums_df, numeric_value, column_names = NULL, prob_vector = c(0, .25, .5, .75, 1), conditions = list()) {
  # Apply conditions
  filtered_df = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) #cut out NA values
  
  # Check if column_names is NULL
  if (is.null(column_names)) {
    summary_quantiles = filtered_df %>%
      summarise(quantile = list(quantile({{numeric_value}}, probs = prob_vector))) %>%
      unnest(quantile) %>%
      mutate(quantile2 = rep(prob_vector, times = n() / length(prob_vector))) %>%
      pivot_wider(names_from = quantile2, values_from = quantile, names_prefix = "quantile")
  } else {
    summary_quantiles = filtered_df %>%
      group_by(across(all_of(column_names))) %>%
      summarise(quantile = list(quantile({{numeric_value}}, probs = prob_vector))) %>%
      unnest(quantile) %>%
      mutate(quantile2 = rep(prob_vector, times = n() / length(prob_vector))) %>%
      pivot_wider(names_from = quantile2, values_from = quantile, names_prefix = "quantile")
  }
  
  #output
  return(summary_quantiles)
}

# Analysis Function 4.) weighted_counting_ipums()
# counts all values within a specified category according to a weight (good for surveys)
# ipums_df: IPUMS data frame
# column_names: character vector of variables to group by
# weight_value: weight variable that adjusts survey data
# conditions: for when we want to remove variables of a specific type
weighted_counting_ipums <- function(ipums_df, column_names, weight_value, conditions = list()) {
  #code run
  summary_df = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    group_by(across(all_of(column_names))) %>%
    summarise(count = sum({{weight_value}}, na.rm = T)) %>% #count the weighted values
    ungroup() %>%
    filter(across(all_of(column_names)), ~ !is.na(.)) %>% #remove na values
    mutate(pct = 100*count/sum(count)) #create a percent of the groups
  
  #output
  return(summary_df)
}

# Analysis Function 5.) weighted_mean_ipums()
# provides a weighted mean,weighted sd, and weighted se split across all values within a specified category
# ipums_df: IPUMS data frame
# weight_value: weight to adjust different representative data points
# numeric_value: numeric variable taking mean of
# column_names: character vector of variables to group by
# conditions: for when we want to filter out specific data points
weighted_mean_ipums <- function(ipums_df, weight_value, numeric_value, column_names = NULL, conditions = list()) {
  #Apply conditions
  summary_mean = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) #cut out NA values
  
  # Check if column_names is NULL
  if (is.null(column_names)) {
    summary_mean = filtered_df %>%
      summarise(weighted_mean = weighted.mean(x = {{numeric_value}}, w = {{weighted_value}}),
                weighted_sd = sqrt(wtd.var(x = {{numeric_value}}, weights = {{weight_value}})),
                weighted_se = weighted_sem(x = {{numeric_value}}, w = {{weight_value}}, na.rm = T)
      )
  } else {
    summary_mean = filtered_df %>%
      group_by(across(all_of(column_names))) %>%
      summarise(weighted_mean = weighted.mean(x = {{numeric_value}}, w = {{weighted_value}}),
                weighted_sd = sqrt(wtd.var(x = {{numeric_value}}, weights = {{weight_value}})),
                weighted_se = weighted_sem(x = {{numeric_value}}, w = {{weight_value}}, na.rm = T)
      )
  }
  
  #output
  return(summary_mean)
}

# Analysis Function 6.) weighted_quantiles_ipums()
# provides weighted quantiles across the numeric_value according to split across all values within a specified category
# ipums_df: IPUMS data frame
# weight_value: weight to adjust different representative data points
# numeric_value: numeric variable finding the quantile of
# column_names: character vector of variables to group by
# conditions: for when we want to filter out specific data points
weighted_quantiles_ipums <- function(ipums_df, weight_value, numeric_value, column_names = NULL, prob_vector = c(0, .25, .5, .75, 1), conditions = list()) {
  #Apply conditions
  summary_quantiles = ipums_df %>%
    filter(!!!conditions) %>% #filter out the conditions
    filter(if_all(all_of(column_names), ~ !is.na(.))) #cut out NA values

  # Check if column_names is NULL
  if (is.null(column_names)) {
    summary_quantiles = filtered_df %>%
      summarise(weighted_quantile = list(wtd.quantile({{numeric_value}}, weights = {{weight_value}}, probs = prob_vector))) %>%
      unnest(weighted_quantile) %>%
      mutate(quantile = rep(prob_vector, times = n() / length(prob_vector))) %>%
      pivot_wider(names_from = quantile, values_from = weighted_quantile, names_prefix = "weighted_q")
  } else {
    summary_quantiles = filtered_df %>%
      group_by(across(all_of(column_names))) %>%
      summarise(weighted_quantile = list(wtd.quantile({{numeric_value}}, weights = {{weight_value}}, probs = prob_vector))) %>%
      unnest(weighted_quantile) %>%
      mutate(quantile = rep(prob_vector, times = n() / length(prob_vector))) %>%
      pivot_wider(names_from = quantile, values_from = weighted_quantile, names_prefix = "weighted_q")
    }
  
  
  #output
  return(summary_quantiles)
}






