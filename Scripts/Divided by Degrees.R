# Header ####
# Primary Author: Timothy Kusuma
# Pulled Code From: Anthony Colavito & Joshua Kendall (Two Economies Report Statistics)
# Date Started: 2/26/2025
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 7/14/2025
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
#library(maptools)
library(colorspace)
library(matrixStats)
library(spatstat)
library(dineq)
library(data.table)
library(bit64)
library(here)
library(labelled)
library(readxl)
library(readr)
library(purrr)
library(rlang)
library(circhelp)
library(Hmisc)


# Data ####################################################################
#Data Upload
ddi = read_ipums_ddi(here("Data", "IPUMS Data", "cps_00022.xml"))
IPUMS_data_read = read_ipums_micro(ddi, data_file = here("Data", "IPUMS Data", "cps_00022.dat"))

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
data <- replace_niu_dataset(IPUMS_data_read)

# Modify Data As Needed ##########################################################################
#Education Variable ####
#Creating a new education variable divided into 4 groups
data$EDUC2 <- lbl_collapse(data$EDUC, ~.val %/% 10) %>% # Creating a new education variable thats divided into 4 groups
  lbl_relabel(
    lbl(2, "Less than high school") ~(.val >= 0 & .val < 7),
    lbl(3, "High school") ~(.val == 7),
    lbl(4, "Some college") ~(.val > 7 & .val < 11),
    lbl(5, "College or more") ~(.val >= 11)
  ) 

#Creating a new education variable divided into 2 groups
data$EDUC3 <- lbl_relabel(data$EDUC, #Creating a third education variable thats divided into two groups
                          lbl(2, "Non-college") ~(.val < 111), 
                          lbl(3, "College") ~(.val >= 111)) %>%
  as_factor()

#Adjusting education variable to education
data$EDUC2 <- data$EDUC2 %>%
  as_factor()

#Race Variable ####
#Reducing Hispanic variable divided into 2 groups
data$HISPAN2 <- lbl_relabel(data$HISPAN,
                            lbl(1, "Not hispanic") ~(.val == 0),
                            lbl(2, "Hispanic") ~(.val != 0 & .val != 902 & .val != 901)
)

#Reducing Race variable divided into 4 groups
data$RACE1 <- data$RACE %>%
  lbl_collapse(~.val %/% 10) %>%
  lbl_relabel(
    lbl(1, "White") ~.val == 10,
    lbl(2, "Black") ~.val == 20,
    lbl(3, "Asian") ~.val == 65,
    lbl(4, "Other") ~.val == 30 | (.val >= 70 & .val < 99)
  ) %>%
  lbl_clean()

#Combining Race and Hispanic variables
data <- data %>%
  mutate(
    RACE2 = case_when(
      (HISPAN2 == 2) ~ "Hispanic",
      (RACE1 == 1) ~ "White",
      (RACE1 == 2) ~ "Black",
      (RACE1 == 3) ~ "Asian",
      (RACE1 == 4) ~ "Other"
    )
  )

#Occupation and Industry Variables ####
#Modifying occupation and industry categories for simplification
data <- data %>%
  mutate(maj_occ = case_when( #I used the family splits as in the census occupation and industries lists: https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
    (OCC2010 >= 10 & OCC2010 <= 3550) ~ "Management, Business, Science, and Arts",
    (OCC2010 >= 3601 & OCC2010 <= 4655) ~ "Service",
    (OCC2010 >= 4700 & OCC2010 <= 5940) ~ "Sales and Office Occupations",
    (OCC2010 >= 6005 & OCC2010 <= 7640) ~ "Natural Resources, Construction, and Maintenance",
    (OCC2010 >= 7700 & OCC2010 <= 9760) ~ "Production, Transportation, and Material Moving",
    (OCC2010 >= 9800 & OCC2010 <= 9830) ~ "Military Specific",
    (OCC2010 > 9830) ~ "Unemployed"
  ),
  min_occ = case_when( 
    (OCC2010 >= 10 & OCC2010 <= 960) ~ "Management, business, and financial",
    (OCC2010 >= 1005 & OCC2010 <= 1980) ~ "Computer, Engineering, and Science Occupations",
    (OCC2010 >= 2001 & OCC2010 <= 2970) ~ "Education, Legal, Community Service, Arts, and Media",
    (OCC2010 >= 3000 & OCC2010 <= 3550) ~ "Healthcare Practitioners and Technical",
    (OCC2010 >= 3601 & OCC2010 <= 4655) ~ "Service",
    (OCC2010 >= 4700 & OCC2010 <= 4965) ~ "Sales and related",
    (OCC2010 >= 5000 & OCC2010 <= 5940) ~ "Office and administrative support",
    (OCC2010 >= 6005 & OCC2010 <= 6130) ~ "Farming, fishing, and forestry",
    (OCC2010 >= 6200 & OCC2010 <= 6950) ~ "Construction and extraction",
    (OCC2010 >= 7000 & OCC2010 <= 7640) ~ "Installation, maintenance, and repair",
    (OCC2010 >= 7700 & OCC2010 <= 8990) ~ "Production",
    (OCC2010 >= 9005 & OCC2010 <= 9760) ~ "Transportation and material moving",
    (OCC2010 >= 9800 & OCC2010 <= 9830) ~ "Military Specific",
    (OCC2010 > 9830) ~ "Unemployed"
  ),
  sub_min_occ = case_when(
    (OCC2010 >= 10 & OCC2010 <= 440) ~ "Management",
    (OCC2010 >= 500 & OCC2010 <= 960) ~ "Business and financial operations",
    (OCC2010 >= 1005 & OCC2010 <= 1240) ~ "Computer and mathematical science",
    (OCC2010 >= 1305 & OCC2010 <= 1560) ~ "Architecture and engineering",
    (OCC2010 >= 1600 & OCC2010 <= 1980) ~ "Life, physical, and social science",
    (OCC2010 >= 2001 & OCC2010 <= 2060) ~ "Community and social service",
    (OCC2010 >= 2100 & OCC2010 <= 2180) ~ "Legal",
    (OCC2010 >= 2205 & OCC2010 <= 2555) ~ "Education Instruction, and library",
    (OCC2010 >= 2600 & OCC2010 <= 2970) ~ "Arts, design, entertainment, sports, and media",
    (OCC2010 >= 3000 & OCC2010 <= 3550) ~ "Healthcare Practicioners and Technical",
    (OCC2010 >= 3601 & OCC2010 <= 3655) ~ "Healthcare support",
    (OCC2010 >= 3700 & OCC2010 <= 3960) ~ "Protective service",
    (OCC2010 >= 4000 & OCC2010 <= 4160) ~ "Food preparation and serving related",
    (OCC2010 >= 4200 & OCC2010 <= 4255) ~ "Building and grounds cleaning and maintenance",
    (OCC2010 >= 4330 & OCC2010 <= 4655) ~ "Personal care and service",
    (OCC2010 >= 4700 & OCC2010 <= 4965) ~ "Sales and related",
    (OCC2010 >= 5000 & OCC2010 <= 5940) ~ "Office and administrative support",
    (OCC2010 >= 6005 & OCC2010 <= 6130) ~ "Farming, fishing, and forestry",
    (OCC2010 >= 6200 & OCC2010 <= 6950) ~ "Construction and extraction",
    (OCC2010 >= 7000 & OCC2010 <= 7640) ~ "Installation, maintenance, and repair",
    (OCC2010 >= 7700 & OCC2010 <= 8990) ~ "Production",
    (OCC2010 >= 9005 & OCC2010 <= 9430) ~ "Transportation",
    (OCC2010 >= 9510 & OCC2010 <= 9760) ~ "Production",
    (OCC2010 >= 9800 & OCC2010 <= 9830) ~ "Military Specific",
    (OCC2010 > 9830) ~ "Unemployed"
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
    (IND1990 >= 170 & IND1990 <= 490) ~ "Agriculture, forestry, fishing, and hunting, and mining",
    (IND1990 == 770) ~ "Construction",
    (IND1990 >= 1070 & IND1990 <= 3990) ~ "Manufacturing",
    (IND1990 >= 4070 & IND1990 <= 4590) ~ "Wholesale trade",
    (IND1990 >= 4670 & IND1990 <= 5791) ~ "Retail trade",
    (IND1990 >= 6070 & IND1990 <= 6390) ~ "Transportation and utilities",
    (IND1990 >= 570 & IND1990 <= 690) ~ "Transportation and utilities",
    (IND1990 >= 6471 & IND1990 <= 6781) ~ "Information",
    (IND1990 >= 6871 & IND1990 <= 7190) ~ "Financial and Insurance, and Real estate, and Rental and leasing",
    (IND1990 >= 7270 & IND1990 <= 7790) ~ "Professional, Scientific, and Management, and Administreateve, and Waste Managemment",
    (IND1990 >= 7860 & IND1990 <= 8470) ~ "Education and health care and social assistance",
    (IND1990 >= 8561 & IND1990 <= 8690) ~ "Arts, Entertainement, and Recreation, and Accomodation and Food Services",
    (IND1990 >= 8770 & IND1990 <= 9290) ~ "Other Services, Except Public Administration",
    (IND1990 >= 8770 & IND1990 <= 9290) ~ "Other services",
    (IND1990 >= 9370 & IND1990 <= 9590) ~ "Public administration",
    (IND1990 >= 9670 & IND1990 <= 9870) ~ "Military",
    (IND1990 > 9870) ~ "Unemployed"
  ),
  min_ind = case_when(
    (IND1990 >= 170 & IND1990 <= 290) ~ "Agriculture, forestry, fishing, and hunting",
    (IND1990 >= 370 & IND1990 <= 490) ~ "Mining, quarrying, and oil and gas extraction",
    (IND1990 == 770) ~ "Construction",
    (IND1990 >= 1070 & IND1990 <= 3990) ~ "Manufacturing",
    (IND1990 >= 4070 & IND1990 <= 4590) ~ "Wholesale trade",
    (IND1990 >= 4670 & IND1990 <= 5791) ~ "Retail trade",
    (IND1990 >= 6070 & IND1990 <= 6390) ~ "Transportation and Warehousing",
    (IND1990 >= 570 & IND1990 <= 690) ~ "Utilities",
    (IND1990 >= 6471 & IND1990 <= 6781) ~ "Information",
    (IND1990 >= 6871 & IND1990 <= 6992) ~ "Finance and insurance",
    (IND1990 >= 7071 & IND1990 <= 7190) ~ "Rental Estate and Rental and Leasing",
    (IND1990 >= 7270 & IND1990 <= 7490) ~ "Professional, Scientific, and Technical",
    (IND1990 == 7570) ~ "Management of Companies and Enterprises",
    (IND1990 >= 7580 & IND1990 <= 7790) ~ "Administrative and Support and Waste Management",
    (IND1990 >= 7860 & IND1990 <= 7890) ~ "Educational Services",
    (IND1990 >= 7970 & IND1990 <= 8470) ~ "Health Care and Social Assistance",
    (IND1990 >= 8561 & IND1990 <= 8590) ~ "Arts, Entertainement, and Recreation",
    (IND1990 >= 8660 & IND1990 <= 8690) ~ "Accomodation and food",
    (IND1990 >= 8770 & IND1990 <= 9290) ~ "Other services, Except Public Administration",
    (IND1990 >= 9370 & IND1990 <= 9590) ~ "Public administration",
    (IND1990 >= 9670 & IND1990 <= 9870) ~ "Military",
    (IND1990 > 9870) ~ "Unemployed"
  ))

#Care Worker variable####
#Following definition from previous work
#See CPS occupational codes, the definition of care work could easily be shifted.
data <- data %>%
  mutate(care_wrk = ifelse(
    OCC2010 %in% c(2001:2025,3601:3655,4330,4600,4655),
    "Care profession", "Non-care profession"
  ))

#Age variable ####
#Reducing Age variable to 5 groups
data$AGE2 <- as.numeric(data$AGE)
data <- data %>%
  mutate(age_cat = case_when(
    (AGE2 < 18) ~ "Under 18",
    (AGE2 >= 18 & AGE2 <= 24) ~ "18-24",
    (AGE2 > 24 & AGE2 <= 44) ~ "25-44",
    (AGE2 > 44 & AGE2 <= 64) ~ "45-64",
    (AGE2 > 65) ~ "65+"
  )) %>%
  mutate(age_cat = factor(age_cat,
                          levels = c("Under 18","18-24","25-44", "45-64","65+")))

#Employment variables ####
data$EMPSTAT2 <- lbl_collapse(data$EMPSTAT, ~.val %/% 10)
data$EMPSTAT2 <- data$EMPSTAT2 %>% as_factor()
data <- data %>% mutate(NotWorking = if_else(EMPSTAT2 == "At work", "Working", "Not Working"))

#Work Status Variable ####
data$WKSTAT2 <- lbl_collapse(data$WKSTAT, ~.val %/% 10)
data <- data %>% 
  mutate(WKSTAT3 = case_when(
    WKSTAT2 %in% c(5,6) ~ "Unemployed",
    EMPSTAT2 %in% "Not in labor force" ~ "Not in labor force",
    TRUE ~ as_factor(WKSTAT2)
  ))

#Why Part Time variable ####
data$WHYPTLY2 <- data$WHYPTLY %>% as_factor()

#reduce variable to 2 groups
data <- data %>% mutate(WHYPTLWK2 = case_when(
  WHYPTLWK %in% c(121,122) ~ "family/personal obligations",
  is.na(WHYPTLWK) ~ NA_character_,
  TRUE ~ "Other"
))

# Household income
#hh_heads <- select(filter(data, RELATE == 0101), HHINCOME, ASECWTH)
#hh_inc_tiles_all <- as.numeric(Quantile(hh_heads$HHINCOME, weights = hh_heads$ASECWTH, probs = seq(0,1,.01), na.rm = TRUE))
#data <- data %>% mutate(hh_inc_quantile = cut(HHINCOME, hh_inc_tiles_all, label = FALSE), test_quintile = ntile(HHINCOME*ASECWTH, 100))

#Sex variable ####
data$SEX2 <- data$SEX %>% as_factor()

#Children variable ####
data = data %>%
  mutate(Child_HH = ifelse(NCHILD > 0, "Children", "No Children"))

#Earn Yearly variable ####
data <- data %>% mutate(Earnyearly = as.numeric(EARNWEEK2)*52)

#Weight variable ####
#adjust variables as needed, remove HFLAG and use more reliable 
data = data %>% mutate(ASECWT2 = case_when(
    (YEAR >= 2019 & YEAR <= 2021) ~ ASECWTCVD,
    (YEAR == 2014) & (HFLAG == 2) ~ NA_integer_,
    TRUE ~ ASECWT
))

# Check Harmonized Variables ########################################################

#This section is unused

# data %>%
#   mutate(
#     WHYPTLWK2 = case_when(
#       WHYPTLWK %in% c(121,122) ~ "family/personal obligations",
#       is.na(WHYPTLWK) ~ NA_character_,
#       WHYPTLWK %in% c(0) ~ NA_character_,
#       TRUE ~ "Other"),
#     EDUC3 = case_when(
#       EDUC == 0 ~ NA_character_,
#       EDUC < 111 ~ "Non-College",
#       EDUC >= 111 ~ "College"),
#     AGE2 = as.numeric(AGE),
#     age_cat = case_when(
#       AGE2 < 18 ~ "Under 18",
#       AGE2 >= 18 & AGE2 <= 24 ~ "18-24",
#       AGE2 > 24 & AGE2 <= 44 ~ "25-44",
#       AGE2 > 44 & AGE2 <= 64 ~ "45-64",
#       AGE2 > 65 ~ "65+"
#     ),
#     SEX2 = as_factor(SEX)
#   ) %>%
#   filter(YEAR == 2024 & AGE >= 25 & AGE < 65) %>%
#   group_by(EDUC3, SEX2, age_cat, WHYPTLWK2) %>%
#   reframe(count = n(),
#             weight_count = sum(ASECWT),
#   ) %>%
#   ungroup(WHYPTLWK2) %>%
#   filter(!is.na(WHYPTLWK2)) %>%
#   mutate(pct = 100*weight_count/sum(weight_count)) %>%
#   ungroup() %>%
#   mutate(proportion = count / sum(count),
#          sd = sqrt(sum(count)*(proportion*(1-proportion))),
#          se = sqrt(proportion*(1-proportion)/sum(count)),
#          mock_interpretation = se/proportion,
#          pct = 100*count/sum(count))
# 
# person_cps_upload <- read.csv(here("Data", "2024 Census CPS", "pppub24.csv"))
# 
# person_cps_upload %>%
#   mutate(
#     PRPTREA2 = case_when(
#       PRPTREA %in% c(7, 8, 17, 18) ~ "family/personal obligations",
#       is.na(PRPTREA) ~ NA_character_,
#       PRPTREA %in% c(-1, 0) ~ NA_character_,
#       TRUE ~ "Other"),
#     EDUC3 = case_when(
#       A_HGA == 0 ~ NA_character_,
#       A_HGA < 43 ~ "Non-College",
#       A_HGA >= 43 ~ "College",
#       TRUE ~ NA_character_),
#     age_cat = case_when(
#       A_AGE < 18 ~ "Under 18",
#       A_AGE >= 18 & A_AGE <= 24 ~ "18-24",
#       A_AGE > 24 & A_AGE <= 44 ~ "25-44",
#       A_AGE > 44 & A_AGE <= 64 ~ "45-64",
#       A_AGE > 65 ~ "65+",
#       TRUE ~ NA_character_
#     ),
#     A_SEX2 = case_when(
#       A_SEX == 1 ~ "Male",
#       A_SEX == 2 ~ "Female"
#     )
#   ) %>%
#   filter(A_AGE >= 25 & A_AGE < 65) %>%
#   group_by(EDUC3, A_SEX2, age_cat, PRPTREA2) %>%
#   reframe(count = n()
#   ) %>%
#   ungroup(PRPTREA2) %>%
#   filter(!is.na(PRPTREA2)) %>%
#   ungroup() %>%
#   mutate(proportion = count / sum(count),
#          sd = sqrt(sum(count)*(proportion*(1-proportion))),
#          se = sqrt(proportion*(1-proportion)/sum(count)),
#          mock_interpretation = se/proportion,
#          pct = 100*count/sum(count)) %>%
#   reframe(count = sum(count))
# 
# 
# ddi = read_ipums_ddi(here("Data", "IPUMS Data", "cps_00013.xml"))
# IPUMS_data = read_ipums_micro(ddi, data_file = here("Data", "IPUMS Data", "cps_00013.dat"))
# 
# IPUMS_data %>%
#   mutate(
#     UH_PTREAS_A3_2 = case_when(
#       WKSTAT %in% c(10,11,13,14,15,50,60,99) ~ NA_character_,
#       UH_PTREAS_A3 %in% c(7, 8, 17, 18) ~ "family/personal obligations",
#       is.na(UH_PTREAS_A3) ~ NA_character_,
#       UH_PTREAS_A3 %in% c(0) ~ NA_character_,
#       TRUE ~ "Other"),
#     UH_GRDATN_A1_2 = case_when(
#       UH_GRDATN_A1  == 0 ~ NA_character_,
#       UH_GRDATN_A1 < 43 ~ "Non-College",
#       UH_GRDATN_A1 >= 43 ~ "College"),
#     age_cat = case_when(
#       UH_AGE_A5 < 18 ~ "Under 18",
#       UH_AGE_A5 >= 18 & UH_AGE_A5 <= 24 ~ "18-24",
#       UH_AGE_A5 > 24 & UH_AGE_A5 <= 44 ~ "25-44",
#       UH_AGE_A5 > 44 & UH_AGE_A5 <= 64 ~ "45-64",
#       UH_AGE_A5 > 65 ~ "65+"
#     ),
#     SEX2 = as_factor(SEX)
#   ) %>%
#   filter(YEAR == 2024 & UH_AGE_A5 >= 25 & UH_AGE_A5 < 65) %>%
#   group_by(UH_GRDATN_A1_2, SEX2, age_cat, UH_PTREAS_A3_2) %>%
#   reframe(count = n(),
#             weight_count = sum(UH_WGTS_A1),
#   ) %>%
#   ungroup(UH_PTREAS_A3_2) %>%
#   filter(!is.na(UH_PTREAS_A3_2)) %>%
#   mutate(pct = 100*weight_count/sum(weight_count)) %>%
#   ungroup() %>%
#   mutate(proportion = count / sum(count),
#          sd = sqrt(sum(count)*(proportion*(1-proportion))),
#          se = sqrt(proportion*(1-proportion)/sum(count)),
#          mock_interpretation = se/proportion,
#          pct = 100*count/sum(count))
# 
# 
# 
# IPUMS_data %>%
#   filter(AGE >= 15) %>%
#   mutate(UH_PTREAS_A3_2 = as_factor(UH_PTREAS_A3),
#          WHYPTLWK2 = as_factor(WHYPTLWK)) %>%
#   mutate(Look1 = paste0(UH_PTREAS_A3_2," + ", WHYPTLWK2)) %>%
#   #select(c(Look1, UH_PTREAS_A3_2, WHYPTLWK2)) %>%
#   count(Look1, UH_PTREAS_A3_2, WHYPTLWK2, name = "Look1_count") %>%
#   distinct(Look1, .keep_all = TRUE) %>%
#   add_count(UH_PTREAS_A3_2, name = "UH_PTREAS_A3_2_count") %>%
#   arrange(UH_PTREAS_A3_2) %>%
#   View()


# Running Cuts ######################################################################

#unused for result

# data_listy = list()
# 
# for (i in 2004:2024) {
#   data_listy[[i]] = data %>% #Question 1: Why Part Time Last Week
#     filter(YEAR == i & AGE > 24 & AGE < 65) %>%
#     group_by(EDUC3, SEX2, age_cat) %>%
#     reframe(year = i,
#               count = n(),
#               weight_count = sum(ASECWT2, na.rm = T),
#     ) %>%
#     ungroup() %>%
#     mutate(proportion = count / sum(count),
#            se = sqrt(proportion*(1-proportion)/sum(count)),
#            weighted_proportion = weight_count/sum(weight_count),
#            people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
#            count_good = count >= people_in_survey & count >= 30
#     )
# }
# 
# bind_rows(data_listy) %>% 
#   select(year, everything()) %>%
#   write.csv(., here("curran_numbers.csv"), row.names = FALSE)


# Splitting Data ####################################################################################################################

#Set default values ####
#default list to print
data_listy = list()

#standard values for parameter estimation
precision = 0.05
z_score = 1.96

# Build Data Ask Curran ####
# Education, Sex, Age
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, age_cat) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
              ) %>%
    ungroup() %>%
    mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
    )
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Part or Full Time
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, age_cat, WKSTAT3) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WKSTAT3)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count >= people_in_survey & count >= 30)
    )
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Part or Full Time, Children
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, Child_HH, WKSTAT3) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WKSTAT3)) %>%
    group_by(EDUC3, SEX2, Child_HH) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count > people_in_survey) & (count > 30))
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  filter((SEX2 %in% "Female")) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Reason for Part Time, Education, and Sex (Keep Normally Full Time)
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, WHYPTLWK2) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYPTLWK2)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count > people_in_survey & count > 30)
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Reason for Part Time, Education, and Sex (Remove Normally Full Time)
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65) & (USFTPTLW == 1)) %>%
    group_by(EDUC3, SEX2, WHYPTLWK2) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYPTLWK2)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count > people_in_survey) & (count > 30))
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Reason for Part Time, Education, Sex, and age categories (Keep Normally Full Time)
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, age_cat, WHYPTLWK2) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYPTLWK2)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count > people_in_survey) & (count > 30))
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Reason for Part Time, Education, Sex, and age categories (Remove Normally Full Time)
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65) & (USFTPTLW == 1)) %>%
    group_by(EDUC3, SEX2, age_cat, WHYPTLWK2) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYPTLWK2)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count > people_in_survey) & (count > 30))
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# create whynotfulltime value ####
data = data %>%
  mutate(WHYABSNT2 = as_factor(WHYABSNT),
         #WHYPTLWK2 = as_factor(WHYPTLWK),
         WHYNFT = case_when(
           WHYABSNT %in% c(7,8,9) ~ "Family/personal Obligation",
           WHYPTLWK %in% c(121,122) ~ "Family/personal Obligation",
           is.na(WHYABSNT) & is.na(WHYPTLWK) ~ NA_character_,
           TRUE ~ "Other"
         ))

# Reason for not full time, Education, and Sex
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, WHYNFT) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYNFT)) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = count > people_in_survey & count > 30)
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Reason for not full time, Education, Sex, and Age
for (i in 2004:2024) {
  data_listy[[i]] = data %>%
    filter((YEAR == i) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2, age_cat, WHYNFT) %>%
    reframe(year = i,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    filter(!is.na(WHYNFT)) %>%
    group_by(EDUC3, SEX2, age_cat) %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count > people_in_survey) & (count > 30))
}

bind_rows(data_listy) %>% 
  select(year, everything()) %>%
  filter((age_cat %in% "25-44") & (SEX2 %in% "Female")) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Misc ##########################################################################################################################

#Population
data_listy = data %>%
    filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
    group_by(EDUC3, SEX2) %>%
    reframe(year = 2024,
              count = n(),
              weight_count = sum(ASECWT2, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(proportion = count / sum(count),
           se = sqrt(proportion*(1-proportion)/sum(count)),
           weighted_proportion = weight_count/sum(weight_count),
           people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
           count_good = (count >= people_in_survey) & (count >= 30)
    )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Race
data_listy = data %>%
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, RACE2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Collar
data_listy = data %>%
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, collar) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(collar)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Labor Force Participation/Employment (Employment Status)
data_listy = data %>% 
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, EMPSTAT2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(EMPSTAT2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Earnings yearly
data_listy = data %>%
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat) %>%
  reframe(year = 2024,
            count = n(),
            weight_mean = weighted.mean(EARNWEEK2*52, EARNWT, na.rm = T),
  ) %>%
  ungroup()

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Earnings weekly
data_listy = data %>%
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat) %>%
  reframe(year = 2024,
            count = n(),
            weight_mean = weighted.mean(EARNWEEK2, EARNWT, na.rm = T),
  ) %>%
  ungroup()

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Not Working
data_listy = data %>%
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, NotWorking) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(NotWorking)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Poverty
data$SPMPOV2 <- data$SPMPOV %>% as_factor()
data_listy = data %>% #Poverty
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, SPMPOV2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(SPMWT, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(SPMPOV2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Healthcare
data$ANYCOVNW2 <- data$ANYCOVNW %>% as_factor()
data_listy = data %>% #Any Health Care
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, ANYCOVNW2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(ANYCOVNW2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Union
data <- data %>% mutate(UNION2 = if_else(UNION == 2, "Union Member", "Not Union Member"))
data_listy = data %>% #Union
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, UNION2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(UNION2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

#Disability
data$DISABWRK2 <- data$DISABWRK %>% as_factor()
data_listy = data %>% #Disability
  filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
  group_by(EDUC3, SEX2, age_cat, DISABWRK2) %>%
  reframe(year = 2024,
            count = n(),
            weight_count = sum(ASECWT2, na.rm = T),
  ) %>%
  ungroup() %>%
  filter(!is.na(DISABWRK2)) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion*(1-proportion)/sum(count)),
         weighted_proportion = weight_count/sum(weight_count),
         people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
         count_good = (count >= people_in_survey) & (count >= 30)
  )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)

# Care Profession
data_listy = data %>%
      filter((YEAR == 2024) & (AGE > 24 & AGE < 65)) %>%
      group_by(EDUC3, SEX2, age_cat, care_wrk) %>%
      reframe(year = 2024,
                count = n(),
                weight_count = sum(ASECWT2, na.rm = T),
      ) %>%
      ungroup() %>%
      filter(!is.na(care_wrk)) %>%
      mutate(proportion = count / sum(count),
             se = sqrt(proportion*(1-proportion)/sum(count)),
             weighted_proportion = weight_count/sum(weight_count),
             people_in_survey = (z_score^2 * weighted_proportion * (1 - weighted_proportion)) / precision^2,
             count_good = (count >= people_in_survey) & (count >= 30)
      )

data_listy %>% 
  select(year, everything()) %>%
  write.csv(., here("curran_numbers.csv"), row.names = FALSE)
