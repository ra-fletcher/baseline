#*******************************************************************************
#
# Project: Functions for creating baseline characteristics tables
# Date:    17-Sep-2021
# Authors: Rob Fletcher, Patrick Rockenschaub, Tom Matcham
# Purpose: Summarise categorical variables
#
#*******************************************************************************

summarise_categorical = function(.data,
                                 .cols,
                                 .group_by,
                                 .decimals = 1,
                                 .include_missing = TRUE) {
  # Wrapper for `generate_stats()`, allowing for multiple categorical 
  # `.cols` to be specified using a tidyselect interface
  # 
  # Arguments
  # ---------
  # .data : tibble
  # .cols : variable(s) to summarise, i.e. x or c(x, y, z)
  # .group_by : [OPTIONAL] variable to group by, MUST be a binary variable
  # .decimals : integer value (number of decimal places to round numbers 
  #             displayed in the output), default is 1 decimal place
  # .include_missing : boolean (whether individuals with missing values should be 
  #                    included), default is TRUE
  #
  # Returns
  # -------
  # tibble
  
  generate_stats = function(.data, 
                            .col, 
                            .group_by,
                            .decimals = 1,
                            .include_missing = TRUE) {
    # Generate summary statistics for one categorical variable for the purpose 
    # of building a baseline characteristics table
    
    # Load function to correctly round numbers
    source(here::here("src", "utils.R"))
    
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
          {{ .group_by }} := paste0({{ .group_by }}, " (n = ", n, ")")
        )
      
      group_values = df %>% 
        dplyr::pull({{ .group_by }}) %>% 
        unique() %>% 
        as.character()
    }
    
    if (missing(.group_by)) {
      # Calculation which INCLUDES individuals with missing values in the 
      # percentage value where the binary grouping variable is missing
      if (.include_missing == TRUE) {
        calculation = df %>%
          dplyr::group_by({{ .col }}) %>%
          dplyr::summarise(n = n(), .groups = "keep") %>%
          tidyr::drop_na({{ .col }}) %>%
          dplyr::mutate(
            perc = 100 * n / df %>% nrow(),
            dplyr::across(where(is.double), ~ rnd(., {{ .decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
        # Calculation which EXCLUDES individuals with missing values in the 
        # percentage value where the binary grouping variable is missing
      } else if (.include_missing == FALSE) {
        calculation = df %>%
          dplyr::group_by({{ .col }}) %>%
          dplyr::summarise(n = n(), .groups = "keep") %>%
          tidyr::drop_na({{ .col }}) %>%
          dplyr::mutate(
            perc = 100 * n / df %>% tidyr::drop_na({{ .col }}) %>% nrow(),
            dplyr::across(where(is.double), ~ rnd(., {{ .decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
      }
      summary_clean = calculation %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          Characteristic = stringr::str_replace_all({{ .col }}, "_", " "),
          Characteristic = stringr:str_to_sentence(Characteristic),
          Characteristic = paste0("  ", Characteristic)
        ) %>%
        dplyr::select(Characteristic, value) %>%
        dplyr::add_row(Characteristic := !!{{ default_name }}, value = "", .before = 1) %>% 
        dplyr::rename(!!total_value := value)
    } else {
      # Calculation which INCLUDES individuals with missing values in the 
      # percentage value where the binary grouping variable is supplied
      if (.include_missing == TRUE) {
        calculation = df %>%
          dplyr::group_by({{ .group_by }}) %>%
          dplyr::group_by({{ .group_by }}, {{ .col }}) %>%
          dplyr::summarise(n = n(), .groups = "keep") %>%
          dplyr::group_by({{ .group_by }}, is.na({{ .col }})) %>%
          dplyr::group_by({{ .group_by }}) %>%
          dplyr::mutate(
            perc = 100 * n / sum(n),
            dplyr::across(where(is.double), ~ rnd(., {{ .decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
        # Calculation which EXCLUDES individuals with missing values in the 
        # percentage value where the binary grouping variable is supplied
      } else if (.include_missing == FALSE) {
        calculation = df %>%
          dplyr::group_by({{ .group_by }}) %>%
          dplyr::group_by({{ .group_by }}, {{ .col }}) %>%
          dplyr::summarise(n = n(), .groups = "keep") %>%
          dplyr::group_by({{ .group_by }}, is.na({{ .col }})) %>%
          tidyr::drop_na({{ .col }}) %>%
          dplyr::group_by({{ .group_by }}) %>%
          dplyr::mutate(
            perc = 100 * n / sum(n),
            dplyr::across(where(is.double), ~ rnd(., {{ .decimals }})),
            value = paste0(n, " (", perc, "%)")
          )
      }
      summary = calculation %>%
        dplyr::filter(!is.na({{ .col }})) %>%
        dplyr::select({{ .group_by }}, {{ .col }}, value) %>%
        tidyr::pivot_longer(cols = !c({{ .group_by }}, {{ .col }})) %>%
        tidyr::pivot_wider(names_from = {{ .group_by }}) %>%
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(group_values), ~ ifelse(is.na(.), "0 (0.0%)", .)),
          `P-value` = chisq.test(
            table(
              df %>% dplyr::pull({{ .group_by }}),
              df %>% dplyr::pull({{ .col }})
            )
          )$p.value,
          dplyr::across(where(is.double), ~rnd(., 3)),
          `P-value` = case_when(
            `P-value` < 0.001 ~ "<0.001", 
            str_detect(`P-value`, "[.][0-9]{2}$") ~ paste0(`P-value`, "0"),
            TRUE ~ as.character(`P-value`)
          ),
          Characteristic = stringr::str_replace_all({{ .col }}, "_", " "),
          Characteristic = stringr::str_to_sentence(Characteristic),
          Characteristic = paste0("  ", Characteristic)
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::select(Characteristic, all_of(group_values), `P-value`)
      
      summary = summary %>% 
        dplyr::add_row(
          !!!c(default_name, "", "", NA) %>%
            set_names(c("Characteristic", group_values, "P-value")),
          .before = 1
        ) %>% 
        tidyr::fill(`P-value`, .direction = "up")
      summary_head = summary %>% dplyr::slice(1)
      summary_tail = summary %>% 
        dplyr::anti_join(summary_head, by = c("Characteristic", group_values, "P-value")) %>% 
        dplyr::mutate(`P-value` = "")
      summary_clean = dplyr::bind_rows(summary_head, summary_tail)
    }
    return(summary_clean)
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
            .data, !!sym(.), , .decimals, .include_missing
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
          .data, !!sym(.), {{ .group_by }}, .decimals, .include_missing
        )
      ) %>% 
      dplyr::bind_rows() %>% 
      dplyr::rename_with(stringr::str_to_sentence)
  }
}