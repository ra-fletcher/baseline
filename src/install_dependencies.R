#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    22-Sep-2021
# Authors: Rob Fletcher, Tom Matcham
# Purpose: Install dependences for `summarise_continuous()` and 
#          `summarise_categorical()`
#
#*******************************************************************************

# Package names
packages = c("here", "tidyverse")

# Install
install.packages(setdiff(packages, rownames(installed.packages())))