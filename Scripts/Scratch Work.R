# Header ####
# Primary Author: Timothy Kusuma
# Pulled Code From: Anthony Colavito & Joshua Kendall
# Date Started: 2/4/2024
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 2/4/2024
# Project: IPUMS Data Analysis
# Title: Scratch Work

# Clean environment #####
rm(list = ls())
gc()

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

# Locations ####################################################################
#Data File Locations
Data_folder <- "Data"

#ASC Code Location
IPUMS_Data_folder <- "IPUMS Data"
IPUMS_Data_location <- here(Data_folder, IPUMS_Data_folder)

#Crosswalk for Industry Labels
Industry_Crosswalk_folder <- "Industry Crosswalk Codes"
Industry_Crosswalk_location <- here(Data_folder, Industry_Crosswalk_folder)

#Crosswalk for Occupational Labels
Occupation_Crosswalk_folder <- "Occupation Crosswalk Codes"
Occupation_Crosswalk_location <- here(Data_folder, Occupation_Crosswalk_folder)

# Data #########################################################################
#I want to build a function that pulls in all files in a folder whether csv, xls, or (dat and xml) files
#xls_pull_type: "first": pulls just the first tab, "labels": pulls the first tab that is not an overview, "cross walk": pulls the tab that says walk.
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

# use code ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~ ~~~~~~~~~
IPUMS_master_data_list <- upload_data_by_folder(IPUMS_Data_location)
Industry_labels_master_list <- upload_data_by_folder(Industry_Crosswalk_location, csv_or_xls_name_convention = "Ind_labs", xls_pull_type = "labels")
Occupation_labels_master_list <- upload_data_by_folder(Occupation_Crosswalk_location, csv_or_xls_name_convention = "Occ_labs", xls_pull_type = "labels")
Industry_Crosswalk_master_list <- upload_data_by_folder(Industry_Crosswalk_location, csv_or_xls_name_convention = "Ind_crosswalk", xls_pull_type = "cross walk")
Occupation_Crosswalk_master_list <- upload_data_by_folder(Occupation_Crosswalk_location, csv_or_xls_name_convention = "Occ_crosswalk", xls_pull_type = "cross walk")

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

