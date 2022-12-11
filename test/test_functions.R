#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    13-Sep-2021
# Author:  Rob Fletcher
# Purpose: Test functions in `src`
#
#*******************************************************************************

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)


# Load data ---------------------------------------------------------------

stroke <- read_rds(here::here("data", "stroke_data.rds"))


# Source functions --------------------------------------------------------

source(here::here("src", "summarise_continuous.R"))
source(here::here("src", "summarise_categorical.R"))


# Summarise continuous variables ---------------------------------------------

cont <- 
  summarise_continuous(
    .data = stroke,
    .cols = c(age, bmi, avg_glucose_level),
    .group_by = stroke_desc,
    .normally_distributed = TRUE
  )


# Summarise categorical variables -----------------------------------------

cat <- 
  summarise_categorical(
    .data = stroke,
    .cols = c(gender, smoking_status, hypertension, work_type), 
    .group_by = stroke_desc,
    .include_missing = TRUE
  )


# Concatenate to produce baseline table -----------------------------------

tbl <- 
  bind_rows(cont, cat) %>%
  mutate(
    Characteristic = case_when(
      str_detect(Characteristic, "^age") ~ "Age, years",
      str_detect(Characteristic, "^bmi") ~ "Body-mass index, kg/m^2",
      str_detect(Characteristic, "^avg") ~ "Average blood glucose level, mmol/L",
      str_detect(Characteristic, "^gender") ~ "Gender",
      str_detect(Characteristic, "^smoking") ~ "Smoking status",
      str_detect(Characteristic, "^hypertension") ~ "Hypertension",
      str_detect(Characteristic, "^work") ~ "Employment",
      TRUE ~ Characteristic
    )
  )
