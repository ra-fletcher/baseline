#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Patrick Rockenschaub, Tom Matcham
# Purpose: Summarise continuous variables
#
#*******************************************************************************

summarise_continuous = function(df,
                                summary_vars,
                                .group_by,
                                decimals = 1,
                                normally_distributed = TRUE) {
  # Wrapper for `generate_stats()`, allowing for multiple continuous 
  # `summary_vars` to be specified using a tidyselect interface
  # 
  # Arguments
  # ---------
  # df : tibble
  # summary_vars : variable(s) to summarise, i.e. c(x, y, z)
  # .group_by : [OPTIONAL] variable to group by, MUST be a binary variable with
  #             categories `0` and `1`
  # decimals : integer value (number of decimal places to round numbers), 
  #            default is 1 decimal place
  # normally_distributed : boolean (whether summary_var is normally-distributed
  #                        [==TRUE] or skewed [==FALSE], decides whether mean/SD 
  #                        with T-test or median/IQR with Wilcoxon are 
  #                        returned), default is TRUE
  #
  # Returns
  # -------
  # tibble

  generate_stats = function(df, 
                            summary_var, 
                            .group_by, 
                            decimals = 1,
                            normally_distributed = TRUE) {
    # Generate summary statistics for one continuous variable for the purpose of
    # building a baseline characteristics table

    # Load function to correctly round numbers
    source(here::here("src", "utils.R"))
  
    # Unicode character for +/-
    plus_minus = bquote("\U00B1")
  
    # Define characteristic name
    default_name = df %>% select({{ summary_var }}) %>% colnames()
   
    if (missing(.group_by)) {
      # Create summary for normally-distributed variables (mean±SD with T-test
      # p-value) where the binary grouping variable is missing
      if (normally_distributed == TRUE) {
        summary = df %>%
          drop_na({{ summary_var }}) %>% 
          summarise(
            mean = mean({{ summary_var }}), 
            sd = sd({{ summary_var }})
          ) %>%
          mutate(
            across(where(is.double), ~round_correctly(., {{ decimals }})),
            value = paste0(mean, plus_minus, sd)
          ) %>%
          select(value) %>%
          rename(!!default_name := value) %>% 
          pivot_longer(everything()) %>% 
          rename(Characteristic = name, total = value)
      # Create summary for skewed variables (median(IQR) with Wilcoxon rank-sum
      # p-value) where the binary grouping variable is missing
      } else if (normally_distributed == FALSE) {
        summary = df %>%
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
          select(value) %>%
          rename(!!default_name := value) %>% 
          pivot_longer(everything()) %>% 
          rename(Characteristic = name, total = value)
      }
    } else {
      # Create summary for normally-distributed variables (mean±SD with T-test
      # p-value) where the binary grouping variable is supplied
      if (normally_distributed == TRUE) {
        summary = df %>%
          group_by({{ .group_by }}) %>%
          drop_na({{ summary_var }}) %>% 
          summarise(
            mean = mean({{ summary_var }}), 
            sd = sd({{ summary_var }})
          ) %>%
          mutate(
            across(where(is.double), ~round_correctly(., {{ decimals }})),
            value = paste0(mean, plus_minus, sd)
          ) %>%
          select({{ .group_by }}, value) %>%
          rename(!!default_name := value) %>% 
          pivot_longer(cols = !{{ .group_by }}) %>% 
          pivot_wider(names_from = {{ .group_by }}) %>% 
          mutate(
            `P-value` = t.test(
              filter(df, {{ .group_by }} == 1) %>% pull({{ summary_var }}),
              filter(df, {{ .group_by }} == 0) %>% pull({{ summary_var }})
            )$p.value,
            across(where(is.double), ~round_correctly(., 3)),
            `P-value` = if_else(
              `P-value` < 0.001, "<0.001", as.character(`P-value`)
            )
          ) %>% 
          rename(Characteristic = name)
      # Create summary for skewed variables (median(IQR) with Wilcoxon rank-sum
      # p-value) where the binary grouping variable is supplied
      } else if (normally_distributed == FALSE) {
        summary = df %>%
          group_by({{ .group_by }}) %>%
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
          select({{ .group_by }}, value) %>%
          rename(!!default_name := value) %>% 
          pivot_longer(cols = !{{ .group_by }}) %>% 
          pivot_wider(names_from = {{ .group_by }}) %>% 
          mutate(
            `P-value` = wilcox.test(
              filter(df, {{ .group_by }} == 1) %>% pull({{ summary_var }}),
              filter(df, {{ .group_by }} == 0) %>% pull({{ summary_var }})
            )$p.value,
            across(where(is.double), ~round_correctly(., 3)),
            `P-value` = if_else(
              `P-value` < 0.001, "<0.001", as.character(`P-value`)
            )
          ) %>% 
          rename(Characteristic = name)
      }
    } 
    return(summary)
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
          df, !!sym(.), , decimals, normally_distributed
        )
      ) %>% 
      bind_rows()
  } else {
    # Loop function over selected `summary_vars` with the binary grouping 
    # variable supplied
    names(selected) %>% 
      map( 
        ~generate_stats(
          df, !!sym(.), {{ .group_by }}, decimals, normally_distributed
        )
      ) %>% 
      bind_rows()
  }
}