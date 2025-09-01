# Header ####
# Primary Author: Timothy Kusuma
# Date Started: 2/4/2024
# Last Editor: Timothy Kusuma
# Team: Economic Program
# Last Edited: 2/4/2024
# Project: Women Non-College
# Title: Initial Data Dump

# Clean environment #####
rm(list = ls())
gc()

# Libraries ####
library(here)
library(ipumsr)
library(tidyverse)

# Data ####
ddi <- read_ipums_ddi(here("Data", "cps_00006.xml"))
data <- read_ipums_micro(ddi, data_file = here("Data", "cps_00006.dat")) #CPS Survey (2004-2014)

# Data Clean ####
data_2 <- data %>%
  filter(MONTH == 3)

data_3 <- data_2 %>% 
  filter(ASECFLAG == 1) %>%
  select(-c(PROFCERT, STATECERT, JOBCERT))

data_4 <- data_2 %>%
  select(c(YEAR, SERIAL, MONTH, ASECFLAG, PERNUM, PROFCERT, STATECERT, JOBCERT)) %>%
  filter(ASECFLAG == 2) %>%
  select(-ASECFLAG)

cps_data <- left_join(data_3, data_4, by = c("PERNUM","YEAR", "SERIAL", "MONTH"))

# Saving to CSV ####
write.csv(cps_data, here("Data", "cps_data_noncollege.csv"), row.names = FALSE)
