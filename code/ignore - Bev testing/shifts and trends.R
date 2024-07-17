testdata <- read_xlsx(paste0(data_path, "/extremely_preterm_2024-03-14.xlsx"),
                      sheet = "Sheet3",
                      rows = c(1:68),
                      cols = c(1:9)
)

testdata <- testdata %>% 
  mutate(outlier = if_else(`Measure value` > `Upper control limit` |
                             `Measure value` < `Lower control limit`,
                           `Measure value`,
                           NA)
         )

testdata <- testdata %>% 
  mutate(flag_to_ignore = if_else(`Measure value` == lag(`Measure value`)
                                  | `Measure value` == `Centreline`,
                                  TRUE,
                                  FALSE)
         )

mark_shifts <- function(dataset, shift, trend, value, median) { # 8 or more consecutive points above or below centreline
  
  dataset <-
    dataset %>%
    
    # checks each value (shift_i) against the median and the preceding 7 values - if all above or all below median then shift_i is TRUE, it is the 8th consecutive value above the median, otherwise FALSE 

    mutate(
      shift_i = tidytable::case_when(
        (({{value}} > {{median}} & lag({{value}}, 1) > {{median}} &
            lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} &
            lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}} &
            lag({{value}}, 6) > {{median}} & lag({{value}}, 7) > {{median}})
         | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} &
              lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} &
              lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}} &
              lag({{value}}, 6) < {{median}} & lag({{value}}, 7) < {{median}})) ~ TRUE,
        TRUE ~ FALSE),
      
      # checks whether each value (shift_i) or any of the following 7 values are TRUE - identifies value (shift_i) as part of a shift

      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE | lead(shift_i, 6) == TRUE 
        | lead(shift_i, 7) == TRUE ~ TRUE,
        TRUE ~ FALSE)
    ) %>%

    rename({{shift}}:=shift) %>%
    select(- shift_i)

  dataset <-
    dataset %>%

    # Find whether sign has changed from preceding point

    mutate(median_diff_sign = sign({{value}} - {{median}}),
           sign_change = median_diff_sign != lag(median_diff_sign)
    ) %>%

    # Need these to find whether point is in the middle of a shift

    mutate(across(all_of(shift), ~lag(.x), .names = "shift_lag"),
           across(all_of(shift), ~lead(.x), .names = "shift_lead")
    ) %>%

    # Set new column to TRUE when all conditions are met

    mutate("{shift}.split" :=
             if_else(sign_change + .data[[shift]] +
                       shift_lag + shift_lead == 4,
                     TRUE, FALSE, missing = FALSE)
    ) #%>%

    # Remove columns that are no longer needed

    select(- all_of(c("median_diff_sign", "sign_change",
                      "shift_lag", "shift_lead"))
    )
}

testdata <- mark_shifts(
  dataset = testdata,
  shift = "orig_shift",
  trend = "orig_trend",
  value = `Measure value`,
  median = `Centreline`
  )

mark_trends <- function(dataset, shift, trend, value, median) { # 6 or more consecutive points increasing or decreasing  - observations that lie on the centreline or are equal to the precdign value are not counted
  
  dataset <- dataset %>%
    mutate(
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3) & lag({{value}}, 3) > lag({{value}}, 4) 
         & lag({{value}}, 4) > lag({{value}}, 5) & lag({{value}}, 5) > lag({{value}}, 6)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3) & lag({{value}}, 3) < lag({{value}}, 4) 
           & lag({{value}}, 4) < lag({{value}}, 5) & lag({{value}}, 5) < lag({{value}}, 6)) ~ TRUE,
        TRUE ~ FALSE),

      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE
        | lead(trend_i, 5) == TRUE | lead(trend_i, 6) == TRUE ~ TRUE,
        TRUE ~ FALSE)
    ) %>%

    rename({{trend}}:=trend) %>%
    select(- trend_i)
  
  # Two trends can sometimes run into each other. This can be problematic when
  # they are plotted using lines.
  #
  # Two adjacent trends where one point is in both trends is fine (think data
  # like \/) - the trend line should not be interrupted. But when the last point
  # in one trend is adjacent to the first point in the next (think \|\) we don't
  # want to connect both trend lines together.
  #
  # This code adds a column to the data that identifies the problematic cases,
  # so the lines can be split in the plotting function.
  #
  # Problematic points are surrounded by other trend points and are in a section
  # where the gradient changes sign twice in succession.
  
  dataset <-
    dataset %>%

    # For identifying two successive changes in gradient direction

    mutate(gradient = sign({{value}} - lag({{value}})),
           gradient_lag_change = lag(gradient) != gradient,
           gradient_lead_change = lead(gradient) != gradient
    ) %>%

    # Need these to find whether point is in the middle of a trend

    mutate(across(all_of(trend), ~lag(.x), .names = "trend_lag"),
           across(all_of(trend), ~lead(.x), .names = "trend_lead")
    ) %>%

    # Set new column to TRUE when all conditions are met

    mutate("{trend}.split" :=
             if_else(gradient_lag_change + gradient_lead_change +
                       .data[[trend]] + trend_lag + trend_lead == 5,
                     TRUE, FALSE, missing = FALSE)
    ) %>%

    # Remove columns that are no longer needed

    select(- all_of(c("gradient", "gradient_lag_change", "gradient_lead_change",
                      "trend_lag", "trend_lead"))
    )
}

testdata <- mark_trends(
  dataset = testdata,
  shift = "orig_shift",
  trend = "orig_trend",
  value = `Measure value`,
  median = `Centreline`
  )

testdata <- testdata %>% 
  mutate(
    trend = 
      if_else(orig_trend == TRUE, 
              `Measure value`, NA), # copies measure_value to plot as trend
    shift =
      if_else(orig_shift == TRUE,
              `Measure value`, NA) # copies measure_value to plot as shift
    )

testdata <- add_split_gaps(
  dataset = testdata,
  measure = "trend",
  split_col_prefix = "orig_trend"
  )

testdata <- add_split_gaps(
  dataset = testdata,
  measure = "shift",
  split_col_prefix = "orig_shift")  
