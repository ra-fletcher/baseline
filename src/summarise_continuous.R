#**************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    13-Sep-2021
# Author:  Rob Fletcher
# Purpose: Summarise normally-distributed continuous variables
#
#**************************************************************************

summarise_continuous = function(df, summary_var, group_var, set_name) {
  # Summarise normally-distributed continuous variables for baseline 
  # characteristics tables
  # 
  # Parameters
  # ----------
  # df : tibble
  # summary_var : variable to summarise
  # group_var : variable to group by
  # set_name : character string (how characteristic is printed in the table)
  #
  # Returns
  # -------
  # tibble
  
  df %>%
    group_by({{ group_var }}) %>%
    drop_na({{ summary_var }}) %>% 
    summarise(mean = mean({{ summary_var }}), sd = sd({{ summary_var }})) %>%
    mutate(value = sprintf("%0.1f±%0.1f", mean, sd)) %>%
    select({{ group_var }}, value) %>%
    rename(!!set_name := value) %>% 
    pivot_longer(cols = !{{ group_var }}) %>% 
    pivot_wider(names_from = {{ group_var }}) %>% 
    mutate(
      `P-value` = t.test(
        filter(df, {{ group_var }} == 1) %>% pull({{ summary_var }}),
        filter(df, {{ group_var }} == 0) %>% pull({{ summary_var }})
      )$p.value,
      across(where(is.double), ~ round(., digits = 3)),
      `P-value` = if_else(`P-value` < 0.001, "<0.001", as.character(`P-value`))
    ) %>% 
    rename(Characteristic = name)
}