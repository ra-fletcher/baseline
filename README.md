# Functions for creating baseline characteristics tables

<!-- badges: start -->
![Languages](https://img.shields.io/badge/Languages-R-6498d3)
![Status](https://img.shields.io/badge/Status-In--Development-orange)
<!-- badges: end -->

This is the repository for a project to generate a set of functions to more easily create tables of baseline characteristics for epidemiological studies. I have written the basis for these functions (one to summarise normally-distributed, continuous variables, and one to summarise categorical variables) and they work well, but I would greatly appreciate collaboration and input from more experienced data scientists.

If you clone this repository, everything to get it working should be available, including the test data, which is a [stroke risk prediction dataset from Kaggle](https://www.kaggle.com/fedesoriano/stroke-prediction-dataset). I've pre-cleaned it so that categorical variables load-in as factors, but any missing data is still in there. This is by-design so that the functions for creating the baseline characteristics table can be tested on data with missing values.

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

Please feel free to contact me via my [email](mailto:rob_fletcher@hotmail.co.uk?subject=Inquiry) should you have any questions/suggestions, or notice any bugs in the code.