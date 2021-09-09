#**************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    09-Sep-2021
# Author:  Rob Fletcher
# Purpose: Summarise categorical variables
#
#**************************************************************************

summarise_categorical = function(df, var, group_var, set_name) {
  # Summarise categorical variables for the baseline characteristics table
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
  
  summary = df %>%
    group_by({{ group_var }}) %>%
    group_by({{ group_var }}, {{ var }}) %>%
    summarise(N = n(), .groups = "keep") %>%
    group_by({{ group_var }}, is.na({{ var }})) %>%
    mutate(s1 = sum(N)) %>%
    group_by({{ group_var }}) %>%
    mutate(s2 = sum(N)) %>%
    mutate(
      value = sprintf("%d (%0.1f)", N, 100 * N / sum(N)),
      value = str_replace(value, "[)]$", "%)")
    ) %>%
    filter(!is.na({{ var }})) %>%
    select({{ group_var }}, {{ var }}, value) %>%
    pivot_longer(cols = !c({{ group_var }}, {{ var }})) %>%
    pivot_wider(names_from = {{ group_var }}) %>%
    mutate(
      across(c(`0`, `1`), ~ if_else(is.na(.), "0 (0.0%)", .)),
      `P-value` = chisq.test(
        table(
          df %>% pull({{ group_var }}),
          df %>% pull({{ var }})
        )
      )$p.value,
      across(where(is.double), ~ round(., digits = 3)),
      `P-value` = if_else(`P-value` < 0.001, "<0.001", as.character(`P-value`)),
      Characteristic = str_replace_all({{ var }}, "_", " "),
      Characteristic = str_to_sentence(Characteristic),
      Characteristic = str_c("   ", Characteristic)
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