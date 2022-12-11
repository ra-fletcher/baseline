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
stroke %>%
  summarise_continuous(
    .cols = age, 
    .group_by = stroke_desc, 
    .normally_distributed = TRUE
  )
#> # A tibble: 1 × 4
#>   Characteristic `Cases (n = 249)` `Controls (n = 4861)` `P-value`
#>   <chr>          <chr>             <chr>                 <chr>    
#> 1 age            67.7±12.7         42.0±22.3             <0.001

stroke %>%
  summarise_continuous(
    .cols = c(bmi, avg_glucose_level),
    .group_by = stroke_desc,
    .normally_distributed = TRUE
  )
#> # A tibble: 2 × 4
#>   Characteristic     `Cases (n = 249)` `Controls (n = 4861)` `P-value`
#>   <chr>              <chr>             <chr>                 <chr>    
#> 1 bmi                30.5±6.3          28.8±7.9              <0.001   
#> 2 avg_glucose_level  132.5±61.9        104.8±43.8            <0.001   

stroke %>%
  summarise_categorical(
    .cols = c(gender, smoking_status), 
    .group_by = stroke_desc, 
    .include_missing = TRUE,
  )
#> # A tibble: 7 × 4
#>   Characteristic     `Cases (n = 249)` `Controls (n = 4861)` `P-value`
#>   <chr>              <chr>             <chr>                 <chr>    
#> 1 "gender"           ""                ""                    "0.560"  
#> 2 "  Female"         "141 (56.6%)"     "2853 (58.7%)"        ""       
#> 3 "  Male"           "108 (43.4%)"     "2007 (41.3%)"        ""       
#> 4 "smoking_status"   ""                ""                    "0.003"  
#> 5 "  Never smoker"   "90 (36.1%)"      "1802 (37.1%)"        ""       
#> 6 "  Former smoker"  "70 (28.1%)"      "815 (16.8%)"         ""       
#> 7 "  Current smoker" "42 (16.9%)"      "747 (15.4%)"         ""         
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

**Patrick Rockenschaub** - Research Fellow | Charité Lab for Artificial Intelligence in Medicine, Berlin

**Tom Matcham** - Head of Data Science | Goss
