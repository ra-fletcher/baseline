#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Tom Matcham
# Purpose: Summarise categorical variables
#
#*******************************************************************************

summarise_categorical = function(df, 
                                 summary_var, 
                                 group_var,
                                 decimals = 1,
                                 include_missing = TRUE,
                                 set_name) {
  # Summarise categorical variables for baseline characteristics tables
  # 
  # Parameters
  # ----------
  # df : tibble
  # summary_var : variable to summarise
  # group_var : variable to group by
  # decimals : integer (number of decimal places to round numbers)
  # include_missing : boolean (whether individuals with missing values should be 
  #                            included)
  # set_name : character string (how characteristic is printed in the table)
  #
  # Returns
  # -------
  # tibble
  
  # Load function to correctly round numbers
  source("src/utils.R")
  
  # Calculation which INCLUDES individuals with missing values in the percentage
  # value
  if(include_missing == TRUE) {
    
    calculation = df %>%
      group_by({{ group_var }}) %>%
      group_by({{ group_var }}, {{ summary_var }}) %>%
      summarise(n = n(), .groups = "keep") %>%
      group_by({{ group_var }}, is.na({{ summary_var }})) %>%
      group_by({{ group_var }}) %>%
      mutate(
        perc = 100 * n / sum(n),
        across(where(is.double), ~round_correctly(., {{ decimals }})),
        value = paste0(n, " (", perc, "%)")
      )
    
  }
  
  # Calculation which EXCLUDES individuals with missing values in the percentage
  # value
  if(include_missing == FALSE) {
    
    calculation = df %>%
      group_by({{ group_var }}) %>%
      group_by({{ group_var }}, {{ summary_var }}) %>%
      summarise(n = n(), .groups = "keep") %>%
      group_by({{ group_var }}, is.na({{ summary_var }})) %>%
      drop_na({{ summary_var }}) %>%
      group_by({{ group_var }}) %>%
      mutate(
        perc = 100 * n / sum(n),
        across(where(is.double), ~round_correctly(., {{ decimals }})),
        value = paste0(n, " (", perc, "%)")
      )
    
  }
    
  summary = calculation %>%
    filter(!is.na({{ summary_var }})) %>%
    select({{ group_var }}, {{ summary_var }}, value) %>%
    pivot_longer(cols = !c({{ group_var }}, {{ summary_var }})) %>%
    pivot_wider(names_from = {{ group_var }}) %>%
    mutate(
      across(c(`0`, `1`), ~ if_else(is.na(.), "0 (0.0%)", .)),
      `P-value` = chisq.test(
        table(
          df %>% pull({{ group_var }}),
          df %>% pull({{ summary_var }})
        )
      )$p.value,
      across(where(is.double), ~round_correctly(., 3)),
      `P-value` = if_else(
        `P-value` < 0.001, "<0.001", as.character(`P-value`)
      ),
      Characteristic = str_replace_all({{ summary_var }}, "_", " "),
      Characteristic = str_to_sentence(Characteristic),
      Characteristic = str_c("  ", Characteristic)
    ) %>%
    ungroup() %>% 
    select(Characteristic, `0`, `1`, `P-value`) %>% 
    add_row(
      Characteristic := !!{{ set_name }},
      `0` = "", `1` = "", `P-value` = NA,
      .before = 1
    ) %>% 
    fill(`P-value`, .direction = "up")
  
  summary_head = summary %>% slice(1)
  summary_tail = summary %>% 
    anti_join(summary_head, by = c("Characteristic", "0", "1", "P-value")) %>% 
    mutate(`P-value` = "")
  
  summary_clean = bind_rows(
    summary_head,
    summary_tail
  )
  
  return(summary_clean)
}