#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Tom Matcham
# Purpose: Summarise continuous variables
#
#*******************************************************************************

summarise_continuous = function(df, 
                                summary_var, 
                                group_var, 
                                decimals = 1,
                                normally_distributed = TRUE,
                                set_name) {
  # Summarise continuous variables for baseline characteristics tables
  # 
  # Parameters
  # ----------
  # df : tibble
  # summary_var : variable to summarise
  # group_var : variable to group by
  # decimals : integer (number of decimal places to round numbers)
  # normally_distributed : boolean (whether summary_var is normally-distributed 
  #                                 or not, decides whether mean/SD with T-test 
  #                                 or median/IQR with Wilcoxon are printed)
  # set_name : character string (how characteristic is printed in the table)
  #
  # Returns
  # -------
  # tibble
  
  # Load function to correctly round numbers
  source("src/utils.R")
  
  # Unicode character for +/-
  plus_minus = bquote("\U00B1")
  
  # Create summary for normally-distributed variables (mean±SD with T-test
  # p-value)
  if(normally_distributed == TRUE) {
    
    summary = df %>%
      group_by({{ group_var }}) %>%
      drop_na({{ summary_var }}) %>% 
      summarise(mean = mean({{ summary_var }}), sd = sd({{ summary_var }})) %>%
      mutate(
        across(where(is.double), ~round_correctly(., {{ decimals }})),
        value = paste0(mean, plus_minus, sd)
      ) %>%
      select({{ group_var }}, value) %>%
      rename(!!set_name := value) %>% 
      pivot_longer(cols = !{{ group_var }}) %>% 
      pivot_wider(names_from = {{ group_var }}) %>% 
      mutate(
        `P-value` = t.test(
          filter(df, {{ group_var }} == 1) %>% pull({{ summary_var }}),
          filter(df, {{ group_var }} == 0) %>% pull({{ summary_var }})
        )$p.value,
        across(where(is.double), ~round_correctly(., 3)),
        `P-value` = if_else(
          `P-value` < 0.001, "<0.001", as.character(`P-value`)
        )
      ) %>% 
      rename(Characteristic = name)
    
  }
  
  # Create summary for skewed variables (median(IQR) with Wilcoxon rank-sum
  # p-value)
  if(normally_distributed == FALSE) {
    
    summary = df %>%
      group_by({{ group_var }}) %>%
      drop_na({{ summary_var }}) %>% 
      summarise(
        median = median({{ summary_var }}), 
        lower = quantile({{ summary_var }}, 0.25),
        upper = quantile({{ summary_var }}, 0.75)
      ) %>%
      mutate(
        across(where(is.double), ~round_correctly(., {{ decimals }})),
        value = paste0(median, " (", lower, ", ", upper, ")")
      ) %>%
      select({{ group_var }}, value) %>%
      rename(!!set_name := value) %>% 
      pivot_longer(cols = !{{ group_var }}) %>% 
      pivot_wider(names_from = {{ group_var }}) %>% 
      mutate(
        `P-value` = wilcox.test(
          filter(df, {{ group_var }} == 1) %>% pull({{ summary_var }}),
          filter(df, {{ group_var }} == 0) %>% pull({{ summary_var }})
        )$p.value,
        across(where(is.double), ~round_correctly(., 3)),
        `P-value` = if_else(
          `P-value` < 0.001, "<0.001", as.character(`P-value`)
        )
      ) %>% 
      rename(Characteristic = name)
    
  }
  
  return(summary)
}