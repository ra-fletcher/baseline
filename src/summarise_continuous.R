#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Patrick Rockenschaub, Tom Matcham
# Purpose: Summarise continuous variables
#
#*******************************************************************************

summarise_continuous = function(.data,
                                .cols,
                                .group_by,
                                .decimals = 1,
                                .normally_distributed = TRUE) {
  # Wrapper for `generate_stats()`, allowing for multiple continuous 
  # `.cols` to be specified using a tidyselect interface
  # 
  # Arguments
  # ---------
  # .data : tibble
  # .cols : variable(s) to summarise, i.e. x or c(x, y, z)
  # .group_by : [OPTIONAL] variable to group by, MUST be a binary variable
  # .decimals : integer value (number of decimal places to round numbers 
  #             displayed in the output), default is 1 decimal place
  # .normally_distributed : boolean (whether `.cols` are normally 
  #                         distributed [==TRUE] or skewed [==FALSE], decides 
  #                         whether mean/SD with T-test or median/IQR with 
  #                         Wilcoxon are returned), default is TRUE
  #
  # Returns
  # -------
  # tibble

  generate_stats = function(.data, 
                            .col, 
                            .group_by, 
                            .decimals = 1,
                            .normally_distributed = TRUE) {
    # Generate summary statistics for one continuous variable for the purpose of
    # building a baseline characteristics table

    # Load necessary functions
    source(here::here("src", "utils.R"))
  
    # Unicode character for +/-
    plus_minus = bquote("\U00B1")
    
    # Define data name
    df = .data
  
    # Define characteristic name
    default_name = df %>% dplyr::select({{ .col }}) %>% colnames()
    
    # Define group column names for grouping variable
    if (missing(.group_by)) {
      total_value = df %>% 
        dplyr::mutate(
          n = n(),
          total = paste0("total (n = ", n, ")")
        ) %>% 
        dplyr::pull(total) %>% 
        unique() %>% 
        as.character()
    } else {
      df = df %>%
        dplyr::group_by({{ .group_by }}) %>% 
        dplyr::mutate(
          n = n(),
          {{ .group_by }} := paste0({{.group_by}}, " (n = ", n, ")")
        )
      group_values = df %>% 
        dplyr::pull({{ .group_by }}) %>% 
        unique() %>% 
        as.character()
    }
   
    if (missing(.group_by)) {
      # Create summary for normally-distributed variables (mean±SD with T-test
      # p-value) where the binary grouping variable is missing
      if (.normally_distributed == TRUE) {
        summary = df %>%
          tidyr::drop_na({{ .col }}) %>% 
          dplyr::summarise(
            mean = mean({{ .col }}), 
            sd = sd({{ .col }})
          ) %>%
          dplyr::mutate(
            dplyr::across(c(mean, sd), ~ as.character(rnd(., {{ .decimals }}))),
            dplyr::across(
              c(mean, sd), 
              ~ dplyr::case_when(
                  !stringr::str_detect(., "[.]") ~ paste0(
                    ., ".", paste(rep("0", {{ .decimals }}), collapse = "")
                  ),
                TRUE ~ .
              )
            ),
            value = paste0(mean, plus_minus, sd)
          ) %>%
          dplyr::select(value) %>%
          dplyr::rename(!!default_name := value) %>% 
          tidyr::pivot_longer(tidyselect::everything()) %>% 
          dplyr::rename(Characteristic = name, !!total_value := value)
      # Create summary for skewed variables (median(IQR) with Wilcoxon rank-sum
      # p-value) where the binary grouping variable is missing
      } else if (.normally_distributed == FALSE) {
        summary = df %>%
          tidyr::drop_na({{ .col }}) %>% 
          dplyr::summarise(
            med = median({{ .col }}), 
            low = quantile({{ .col }}, 0.25),
            hgh = quantile({{ .col }}, 0.75)
          ) %>%
          dplyr::mutate(
            dplyr::across(
              c(med, low, hgh), ~ as.character(rnd(., {{ .decimals }}))
            ),
            dplyr::across(
              c(med, low, hgh), 
              ~ dplyr::case_when(
                  !stringr::str_detect(., "[.]") ~ paste0(
                    ., ".", paste(rep("0", {{ .decimals }}), collapse = "")
                  ),
                TRUE ~ .
              )
            ),
            value = paste0(med, " (", low, ", ", hgh, ")")
          ) %>%
          dplyr::select(value) %>%
          dplyr::rename(!!default_name := value) %>% 
          tidyr::pivot_longer(tidyselect::everything()) %>% 
          dplyr::rename(Characteristic = name, !!total_value := value)
      }
    } else {
      # Create summary for normally-distributed variables (mean±SD with T-test
      # p-value) where the binary grouping variable is supplied
      if (.normally_distributed == TRUE) {
        summary = df %>%
          dplyr::group_by({{ .group_by }}) %>%
          tidyr::drop_na({{ .col }}) %>% 
          dplyr::summarise(
            mean = mean({{ .col }}), 
            sd = sd({{ .col }})
          ) %>%
          dplyr::mutate(
            dplyr::across(c(mean, sd), ~ as.character(rnd(., {{ .decimals }}))),
            dplyr::across(
              c(mean, sd), 
              ~ dplyr::case_when(
                  !stringr::str_detect(., "[.]") ~ paste0(
                    ., ".", paste(rep("0", {{ .decimals }}), collapse = "")
                  ),
                TRUE ~ .
              )
            ),
            value = paste0(mean, plus_minus, sd)
          ) %>%
          dplyr::select({{ .group_by }}, value) %>%
          dplyr::rename(!!default_name := value) %>% 
          tidyr::pivot_longer(cols = !{{ .group_by }}) %>% 
          tidyr::pivot_wider(names_from = {{ .group_by }}) %>% 
          dplyr::mutate(
            `P-value` = t.test(
              dplyr::filter(df, {{ .group_by }} == min(group_values)) %>% 
                dplyr::pull({{ .col }}),
              dplyr::filter(df, {{ .group_by }} == max(group_values)) %>% 
                dplyr::pull({{ .col }})
            )$p.value,
            dplyr::across(where(is.double), ~rnd(., 3)),
            `P-value` = ifelse(
              `P-value` < 0.001, "<0.001", as.character(`P-value`)
            )
          ) %>% 
          dplyr::rename(Characteristic = name)
      # Create summary for skewed variables (median(IQR) with Wilcoxon rank-sum
      # p-value) where the binary grouping variable is supplied
      } else if (.normally_distributed == FALSE) {
        summary = df %>%
          dplyr::group_by({{ .group_by }}) %>%
          tidyr::drop_na({{ .col }}) %>% 
          dplyr::summarise(
            med = median({{ .col }}), 
            low = quantile({{ .col }}, 0.25),
            hgh = quantile({{ .col }}, 0.75)
          ) %>%
          dplyr::mutate(
            dplyr::across(
              c(med, low, hgh), ~ as.character(rnd(., {{ .decimals }}))
            ),
            dplyr::across(
              c(med, low, hgh), 
              ~ dplyr::case_when(
                  !stringr::str_detect(., "[.]") ~ paste0(
                    ., ".", paste(rep("0", {{ .decimals }}), collapse = "")
                  ),
                TRUE ~ .
              )
            ),
            value = paste0(med, " (", low, ", ", hgh, ")")
          ) %>%
          dplyr::select({{ .group_by }}, value) %>%
          dplyr::rename(!!default_name := value) %>% 
          tidyr::pivot_longer(cols = !{{ .group_by }}) %>% 
          tidyr::pivot_wider(names_from = {{ .group_by }}) %>% 
          dplyr::mutate(
            `P-value` = wilcox.test(
              dplyr::filter(df, {{ .group_by }} == min(group_values)) %>% 
                dplyr::pull({{ .col }}),
              dplyr::filter(df, {{ .group_by }} == max(group_values)) %>% 
                dplyr::pull({{ .col }})
            )$p.value,
            dplyr::across(where(is.double), ~ rnd(., 3)),
            `P-value` = ifelse(
              `P-value` < 0.001, "<0.001", as.character(`P-value`)
            )
          ) %>% 
          dplyr::rename(Characteristic = name)
      }
    } 
    return(summary)
  }
  
  # Defuse `.cols` function argument
  .cols = rlang::enquo(.cols)
  
  # Evaluate defused R code and return vector of locations for the selected 
  # elements
  selected = tidyselect::eval_select(.cols, .data)
  
  if (missing(.group_by)) {
    # Loop function over selected `.cols` with the binary grouping 
    # variable not supplied
    names(selected) %>% 
      purrr::map( 
        ~ generate_stats(
            .data, !!sym(.), , .decimals, .normally_distributed
        )
      ) %>% 
      dplyr::bind_rows() %>% 
      dplyr::rename_with(stringr::str_to_sentence)
  } else {
    # Loop function over selected `.cols` with the binary grouping 
    # variable supplied
    names(selected) %>% 
      purrr::map( 
        ~ generate_stats(
            .data, !!sym(.), {{ .group_by }}, .decimals, .normally_distributed
        )
      ) %>% 
      dplyr::bind_rows() %>% 
      dplyr::rename_with(stringr::str_to_sentence)
  }
}