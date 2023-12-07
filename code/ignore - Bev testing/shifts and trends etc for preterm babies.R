         # outlier = if_else(measure_value > upper_control_limit | measure_value < lower_control_limit, 
         #                   measure_value, NA),
         # upper_outer_one_third = if_else(measure_value > upper_warning_limit,
         #                                 measure_value, NA),
         # lower_outer_one_third = if_else(measure_value < lower_warning_limit,
         #                                 measure_value,
         #                                 NA),
         # upper_inner_one_third = if_else(measure_value > mean & measure_value < upper_warning_limit,
         #                                 measure_value,
         #                                 NA),
         # lower_inner_one_third = if_else(measure_value < mean & measure_value > lower_warning_limit,
         #                                 measure_value,
         #                                 NA)

# Function to flag shifts and trends on run chart data
# Parameters:
# shift: the name for the new variable where shift is flagged
# trend: the name for the new variable where trend is flagged
# value: the name of the variable which contains the value being evaluated
# mean: the name of the variable which contains the mean against which value is tested

control_chart_flags <- function(dataset, shift, trend, value, mean) {
  
  dataset <- dataset %>%
    mutate(
      shift_i = tidytable::case_when(
        ({{value}} > {{mean}} & lag({{value}}, 1) > {{mean}} &
           lag({{value}}, 2) > {{mean}} & lag({{value}}, 3) > {{mean}} &
           lag({{value}}, 4) > {{mean}} & lag({{value}}, 5) > {{mean}} &
           lag({{value}}, 6) > {{mean}} & lag({{value}}, 7) > {{mean}} &
           lag({{value}}, 8) > {{mean}})
        | ({{value}} < {{mean}} & lag({{value}}, 1) < {{mean}} &
             lag({{value}}, 2) < {{mean}} & lag({{value}}, 3) < {{mean}} &
             lag({{value}}, 4) < {{mean}} & lag({{value}}, 5) < {{mean}} &
             lag({{value}}, 6) < {{mean}} & lag({{value}}, 7) < {{mean}} &
             lag({{value}}, 8) < {{mean}}) ~ TRUE,
        TRUE ~ FALSE),
      
      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE | lead(shift_i, 6) == TRUE
        | lead(shift_i, 7) == TRUE ~ TRUE,
        TRUE ~ FALSE),
      
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4) &
           lag({{value}}, 4) > lag({{value}}, 5)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4)
           & lag({{value}}, 4) < lag({{value}}, 5)) ~ TRUE,
        TRUE ~ FALSE),
      
      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE | lead(trend_i, 5) == TRUE ~ TRUE,
        TRUE ~ FALSE)
      ) %>%
    
    rename({{shift}}:=shift,{{trend}}:=trend) %>%
    select(-shift_i, -trend_i)
}

### ii - Mark SHIFTS and TRENDS ----

temp <- control_chart_flags(
  dataset = temp,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  mean = mean) 