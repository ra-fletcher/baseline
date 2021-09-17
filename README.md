# Functions for creating baseline characteristics tables

<!-- badges: start -->
![Languages](https://img.shields.io/badge/Languages-R-6498d3)
![Status](https://img.shields.io/badge/Status-In--Development-orange)
<!-- badges: end -->

## Introduction

This is the repository for a project to generate a set of functions to more easily create tables of baseline characteristics for epidemiological studies. I have written the basis for these functions (one to summarise normally-distributed, continuous variables, and one to summarise categorical variables) and they work well, but I would greatly appreciate collaboration and input from more experienced data scientists.

If you clone this repository, everything to get it working should be available, including the test data, which is a [stroke risk prediction dataset from Kaggle](https://www.kaggle.com/fedesoriano/stroke-prediction-dataset). I've pre-cleaned it so that categorical variables load-in as factors, but any missing data is still in there. This is by-design so that the functions for creating the baseline characteristics table can be tested on data with missing values.

## Overview

The current version of baseline provides two functions which facilitate the creation of baseline characteristics tables:

  - `summarise_categorical()` provides number and percentage of individuals in each group of a categorical  variable, as well as the P-value for a Chi-squared test between groups.
  - `summarise_continuous()` provides either mean±SD with the P-value for a T-test between groups for normally-distributed variables, or median (IQR) with the P-value for a Wilcoxon rank-sum test between groups for skewed variables.

## Usage

``` r
summarise_continuous(
    stroke_df, 
    bmi, 
    stroke, 
    normally_distributed = TRUE,
    set_name = "Age, years"
  )
#> # A tibble: 1 × 4
#>  Characteristic `0`      `1`      `P-value`
#>  <chr>          <chr>    <chr>    <chr>    
#> 1 Age, years     28.8±7.9 30.5±6.3 <0.001

summarise_categorical(
    stroke_df, 
    smoking_status, 
    stroke, 
    include_missing = TRUE,
    set_name = "Smoking status"
  )
#> # A tibble: 4 × 4
#>  Characteristic      `0`            `1`          `P-value`
#>  <chr>               <chr>          <chr>        <chr>    
#> 1 "Smoking status"    ""             ""           "0.003"  
#> 2 "  Never smoker"    "1802 (37.1%)" "90 (36.1%)" ""       
#> 3 "  Former smoker"   "815 (16.8%)"  "70 (28.1%)" ""       
#> 4 "  Current smoker"  "747 (15.4%)"  "42 (16.9%)" "" 
```

## Folder structure

Below is an overview of the folders in this repository that are actively synchronised with GitHub. It is structured roughly in-line with what is recommended in this [R best practices blog](https://kdestasio.github.io/post/r_best_practices/).

**baseline**   
     ├── code  
     ├── data  
     └── src  

### code

`code` contains the code used to import the data and test the functions.

### data

`data` contains the data used to test the functions, described in the introduction above.

### src

`src` contains any files that are called via `source()` in the scripts in `code`, i.e. functions.

## Contact

If you encounter a clear bug or have any questions/suggestions, please feel free to contact me via my email: [rob_fletcher@hotmail.co.uk](mailto:rob_fletcher@hotmail.co.uk?subject=Inquiry).

-----

## Credits

Rob Fletcher - Associate Biostatistician / Research Associate | The George Institute for Global Health

Tom Matcham - Head of Data Science | Intechnica