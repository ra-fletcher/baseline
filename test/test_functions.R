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

# Summarise numeric variables ---------------------------------------------

baseline_continuous = bind_rows(
  summarise_continuous(
    stroke_df, age, stroke, normally_distributed = TRUE,
    set_name = "Age, years"
  ),
  summarise_continuous(
    stroke_df, bmi, stroke, normally_distributed = FALSE,
    set_name = "Body-mass index, kg/m^2"
  ),
  summarise_continuous(
    stroke_df, avg_glucose_level, stroke, normally_distributed = TRUE,
    set_name = "Average blood glucose level, mmol/L"
  )
)

# Summarise categorical variables -----------------------------------------

baseline_categorical = bind_rows(
  summarise_categorical(
    stroke_df, gender, stroke, decimals = 1, include_missing = TRUE,
    set_name = "Sex"
  ),
  summarise_categorical(
    stroke_df, smoking_status, stroke, decimals = 1, include_missing = TRUE,
    set_name = "Smoking status"
  ),
  summarise_categorical(
    stroke_df, hypertension, stroke, decimals = 1, include_missing = TRUE,
    set_name = "Has hypertension"
  ),
  summarise_categorical(
    stroke_df, work_type, stroke, decimals = 1, include_missing = TRUE,
    set_name = "Current employment status"
  ),
  summarise_categorical(
    stroke_df, residence_type, stroke, decimals = 1, include_missing = TRUE,
    set_name = "Region"
  ),
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
  )