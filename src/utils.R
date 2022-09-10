#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    20-Sep-2021
# Author:  Rob Fletcher
# Purpose: Custom functions required by `summarise_continuous()` and 
#          `summarise_categorical()`
#
#*******************************************************************************

rnd = function(x, decimals) {
  # Correctly round numbers (`round()` function in R works weirdly)
  # 
  # Parameters
  # ----------
  # x : double (number to round)
  # decimals : integer (number of decimal places to round number)
  #
  # Returns
  # -------
  # double
  
  pos_neg = sign(x)
  y = abs(x) * 10^decimals
  y = y + 0.5 + sqrt(.Machine$double.eps)
  y = trunc(y)
  y = y / 10^decimals
  return(y * pos_neg)
}