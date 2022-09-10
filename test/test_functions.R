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

stroke_df = read_rds(here::here("data", "stroke_data.rds"))


# Source functions --------------------------------------------------------

source(here::here("src", "summarise_continuous.R"))
source(here::here("src", "summarise_categorical.R"))


# Define groups for the table ---------------------------------------------

group_sizes = table(stroke_df$stroke_desc) %>%
  as.list()

tbl_col_names = 
  list(
    cases = paste0("Cases (n=", group_sizes$cases, ")"),
    controls = paste0("Controls (n=", group_sizes$controls, ")"),
    p_value = "P-value"
  )


# Summarise continuous variables ---------------------------------------------

baseline_continuous = stroke_df %>% 
  summarise_continuous(
    c(age, bmi, avg_glucose_level),
    .group_by = stroke_desc,
    normally_distributed = TRUE
  )


# Summarise categorical variables -----------------------------------------

baseline_categorical = stroke_df %>% 
  summarise_categorical(
    c(gender, smoking_status, hypertension, work_type), 
    .group_by = stroke_desc,
    include_missing = TRUE
  )


# Concatenate to produce baseline table -----------------------------------

baseline_tbl =
  bind_rows(
    baseline_continuous, 
    baseline_categorical, 
  ) %>%
  select(
    Characteristic,
    !!tbl_col_names$cases := `1`,
    !!tbl_col_names$controls := `0`,
    `P-value`
  ) %>% 
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
