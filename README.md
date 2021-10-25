# Functions for creating baseline characteristics tables

<!-- badges: start -->
![Languages](https://img.shields.io/badge/Languages-R-6498d3)
![Status](https://img.shields.io/badge/Status-In--Development-orange)
<!-- badges: end -->

## Introduction

This is the repository for a project to generate a set of functions to more easily create tables of baseline characteristics for epidemiological studies.

If you clone this repository, everything to get it working should be available, including the test data, which is a [stroke risk prediction dataset from Kaggle](https://www.kaggle.com/fedesoriano/stroke-prediction-dataset). I've pre-cleaned it so that categorical variables load-in as factors, but any missing data is still in there. This is by-design so that the functions for creating the baseline characteristics table can be tested on data with missing values.

## Dependencies

To get these functions to work, please install all dependencies. To do this, run the following script:

``` r
# Package names
packages = c("here", "tidyverse")

# Install
install.packages(setdiff(packages, rownames(installed.packages())))
```

## Overview

The current version of baseline provides two functions which facilitate the creation of baseline characteristics tables:

  - `summarise_continuous()` provides either mean±SD with the P-value for a T-test between groups for normally-distributed variables, or median (IQR) with the P-value for a Wilcoxon rank-sum test between groups for skewed variables.
  - `summarise_categorical()` provides number and percentage of individuals in each group of a categorical  variable, as well as the P-value for a Chi-squared test between groups.

## Usage

``` r
stroke_df %>%
  summarise_continuous(
    age, 
    .group_by = stroke, 
    normally_distributed = TRUE
  )
#> # A tibble: 1 × 4
#>   Characteristic `0`     `1`       `P-value`
#>   <chr>          <chr>   <chr>     <chr>    
#> 1 Age, years     42±22.3 67.7±12.7 <0.001 

stroke_df %>%
  summarise_continuous(
    c(bmi, avg_glucose_level), 
    .group_by = stroke, 
    normally_distributed = FALSE,
  )
#> # A tibble: 2 × 4
#>   Characteristic    `0`                `1`                 `P-value`
#>   <chr>             <chr>              <chr>               <chr>    
#> 1 bmi               28 (23.4, 33.1)    29.7 (26.4, 33.7)   <0.001   
#> 2 avg_glucose_level 91.5 (77.1, 112.8) 105.2 (79.8, 196.7) <0.001 

stroke_df %>%
  summarise_categorical(
    c(gender, smoking_status), 
    stroke, 
    include_missing = TRUE,
  )
#> # A tibble: 7 × 4
#>   Characteristic     `0`            `1`           `P-value`
#>   <chr>              <chr>          <chr>         <chr>    
#> 1 "gender"           ""             ""            "0.56"   
#> 2 "  Female"         "2853 (58.7%)" "141 (56.6%)" ""       
#> 3 "  Male"           "2007 (41.3%)" "108 (43.4%)" ""       
#> 4 "smoking_status"   ""             ""            "0.003"  
#> 5 "  Never smoker"   "1802 (37.1%)" "90 (36.1%)"  ""       
#> 6 "  Former smoker"  "815 (16.8%)"  "70 (28.1%)"  ""       
#> 7 "  Current smoker" "747 (15.4%)"  "42 (16.9%)"  ""       
```

## Folder structure

Below is an overview of the folders in this repository that are actively synchronised with GitHub. It is structured roughly in-line with what is recommended in this [R best practices blog](https://kdestasio.github.io/post/r_best_practices/).

**baseline**   
     ├── test  
     ├── data  
     └── src  

### test

`test` contains the code used to import the data and test the functions.

### data

`data` contains the data used to test the functions, described in the introduction above.

### src

`src` contains any files that are called via `source()` in the scripts in `code`, i.e. functions.

## Contact

If you encounter a clear bug or have any questions/suggestions, please feel free to contact me via my [email](mailto:rfletcher@georgeinstitute.org.au?subject=Inquiry).

-----

## Credits

**Rob Fletcher** - Associate Biostatistician / Research Associate | The George Institute for Global Health

**Tom Matcham** - Head of Data Science | Intechnica
