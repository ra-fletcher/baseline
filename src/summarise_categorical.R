#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Patrick Rockenschaub, Tom Matcham
# Purpose: Summarise categorical variables
#
#*******************************************************************************

summarise_categorical = function(df,
                                 summary_vars,
                                 .group_by,
                                 decimals = 1,
                                 include_missing = TRUE) {
  # Wrapper for `generate_stats()`, allowing for multiple categorical 
  # `summary_vars` to be specified using a tidyselect interface
  # 
  # Arguments
  # ---------
  # df : tibble
  # summary_vars : variable(s) to summarise, i.e. x or c(x, y, z)
  # .group_by : [OPTIONAL] variable to group by, MUST be a binary variable with
  #             categories `0` and `1`
  # decimals : integer value (number of decimal places to round numbers 
  #            displayed in the output), default is 1 decimal place
  # include_missing : boolean (whether individuals with missing values should be 
  #                   included), default is TRUE
  #
  # Returns
  # -------
  # tibble
  
  generate_stats = function(df, 
                            summary_var, 
                            .group_by,
                            decimals = 1,
                            include_missing = TRUE) {
    # Generate summary statistics for one categorical variable for the purpose 
    # of building a baseline characteristics table
    
    # Load function to correctly round numbers
    source(here::here("src", "utils.R"))
    
    # Define characteristic name
    default_name = df %>% select({{ summary_var }}) %>% colnames()
    
    # Define group column names for grouping variable
    if (missing(.group_by)) {
      total_value = df %>% 
        mutate(
          n = n(),
          total = paste0("total (n = ", n, ")")
        ) %>% 
        pull(total) %>% 
        unique() %>% 
        as.character()
    } else {
      df = df %>%
        group_by({{ .group_by }}) %>% 
        mutate(
          n = n(),
          {{ .group_by }} := paste0({{.group_by}}, " (n = ", n, ")")
        )
      
      group_values = df %>% 
        pull({{ .group_by }}) %>% 
        unique() %>% 
        as.character()
    }
    
    if (missing(.group_by)) {
      # Calculation which INCLUDES individuals with missing values in the 
      # percentage value where the binary grouping variable is missing
      if (include_missing == TRUE) {
        calculation = df %>%
          group_by({{ summary_var }}) %>%
          summarise(n = n(), .groups = "keep") %>%
          drop_na({{ summary_var }}) %>%
          mutate(
            perc = 100 * n / df %>% nrow(),
            across(where(is.double), ~rnd(., {{ decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
        # Calculation which EXCLUDES individuals with missing values in the 
        # percentage value where the binary grouping variable is missing
      } else if (include_missing == FALSE) {
        calculation = df %>%
          group_by({{ summary_var }}) %>%
          summarise(n = n(), .groups = "keep") %>%
          drop_na({{ summary_var }}) %>%
          mutate(
            perc = 100 * n / df %>% drop_na({{ summary_var }}) %>% nrow(),
            across(where(is.double), ~rnd(., {{ decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
      }
      summary_clean = calculation %>%
        ungroup() %>% 
        mutate(
          Characteristic = str_replace_all({{ summary_var }}, "_", " "),
          Characteristic = str_to_sentence(Characteristic),
          Characteristic = str_c("  ", Characteristic)
        ) %>%
        select(Characteristic, value) %>%
        add_row(Characteristic := !!{{ default_name }}, value = "", .before = 1) %>% 
        rename(!!total_value := value)
    } else {
      # Calculation which INCLUDES individuals with missing values in the 
      # percentage value where the binary grouping variable is supplied
      if (include_missing == TRUE) {
        calculation = df %>%
          group_by({{ .group_by }}) %>%
          group_by({{ .group_by }}, {{ summary_var }}) %>%
          summarise(n = n(), .groups = "keep") %>%
          group_by({{ .group_by }}, is.na({{ summary_var }})) %>%
          group_by({{ .group_by }}) %>%
          mutate(
            perc = 100 * n / sum(n),
            across(where(is.double), ~rnd(., {{ decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
        # Calculation which EXCLUDES individuals with missing values in the 
        # percentage value where the binary grouping variable is supplied
      } else if (include_missing == FALSE) {
        calculation = df %>%
          group_by({{ .group_by }}) %>%
          group_by({{ .group_by }}, {{ summary_var }}) %>%
          summarise(n = n(), .groups = "keep") %>%
          group_by({{ .group_by }}, is.na({{ summary_var }})) %>%
          drop_na({{ summary_var }}) %>%
          group_by({{ .group_by }}) %>%
          mutate(
            perc = 100 * n / sum(n),
            across(where(is.double), ~rnd(., {{ decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
      }
      summary = calculation %>%
        filter(!is.na({{ summary_var }})) %>%
        select({{ .group_by }}, {{ summary_var }}, value) %>%
        pivot_longer(cols = !c({{ .group_by }}, {{ summary_var }})) %>%
        pivot_wider(names_from = {{ .group_by }}) %>%
        mutate(
          across(all_of(group_values), ~ if_else(is.na(.), "0 (0.0%)", .)),
          `P-value` = chisq.test(
            table(
              df %>% pull({{ .group_by }}),
              df %>% pull({{ summary_var }})
            )
          )$p.value,
          across(where(is.double), ~rnd(., 3)),
          `P-value` = if_else(
            `P-value` < 0.001, "<0.001", as.character(`P-value`)
          ),
          Characteristic = str_replace_all({{ summary_var }}, "_", " "),
          Characteristic = str_to_sentence(Characteristic),
          Characteristic = str_c("  ", Characteristic)
        ) %>%
        ungroup() %>% 
        select(Characteristic, all_of(group_values), `P-value`)
      
      summary = summary %>% 
        add_row(
          !!!c(default_name, "", "", NA) %>%
            set_names(c("Characteristic", group_values, "P-value")),
          .before = 1) %>% 
        fill(`P-value`, .direction = "up")
      summary_head = summary %>% slice(1)
      summary_tail = summary %>% 
        anti_join(summary_head, by = c("Characteristic", group_values, "P-value")) %>% 
        mutate(`P-value` = "")
      summary_clean = bind_rows(summary_head, summary_tail)
    }
    return(summary_clean)
  }
  
  # Defuse `summary_vars` function argument
  summary_vars = rlang::enquo(summary_vars)
  
  # Evaluate defused R code and return vector of locations for the selected 
  # elements
  selected = tidyselect::eval_select(summary_vars, df)
  
  if (missing(.group_by)) {
    # Loop function over selected `summary_vars` with the binary grouping 
    # variable not supplied
    names(selected) %>% 
      map( 
        ~generate_stats(
          df, !!sym(.), , decimals, include_missing
        )
      ) %>% 
      bind_rows() %>% 
      rename_with(str_to_sentence)
  } else {
    # Loop function over selected `summary_vars` with the binary grouping 
    # variable supplied
    names(selected) %>% 
      map( 
        ~generate_stats(
          df, !!sym(.), {{ .group_by }}, decimals, include_missing
        )
      ) %>% 
      bind_rows() %>% 
      rename_with(str_to_sentence)
  }
}