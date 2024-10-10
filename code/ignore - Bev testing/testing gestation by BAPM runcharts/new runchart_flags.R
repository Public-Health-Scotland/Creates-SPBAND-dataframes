### ii - Mark SHIFTS and TRENDS ----

# compares measure_value with extended to determine shifts
# compares consecutive measure_values to determine trends

# Function to flag shifts and trends on run chart data
# Parameters:
# shift: the name for the new variable where shift is flagged
# trend: the name for the new variable where trend is flagged
# value: the name of the variable which contains the value being evaluated
# median: the name of the variable which contains the median against which value is tested

runchart_flags <- function(dataset, shift, trend, value, median) {
  
  dataset <- dataset %>%
    mutate(
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4)) ~ TRUE,
        TRUE ~ FALSE),

      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE ~ TRUE,
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

# # Now calculate shifts (ensuring split medians, noted by median_period, are taken into account)

  dataset <-
    dataset %>%
    group_by(median_name, .add = TRUE) %>%

    mutate(
      shift_i = tidytable::case_when(
        (({{value}} > {{median}} & lag({{value}}, 1) > {{median}} &
            lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} &
            lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}})
         | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} &
              lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} &
              lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}})) ~ TRUE,
        TRUE ~ FALSE),

      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE  ~ TRUE,
        TRUE ~ FALSE)
    ) %>%

    rename({{shift}}:=shift) %>%
    select(- shift_i)

# There is a similar issue for shifts. There can't be one point in two shifts,
# but the last point in one can be adjacent to the first point in the next.
# Again we don't want to connect the lines.
#
# Problematic points are surrounded by other shift points and are on the
# opposite side of the median from the preceding point.

  dataset <-
    dataset %>%
    ungroup(median_name) |> 

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
    ) %>%

    # Remove columns that are no longer needed

    select(- all_of(c("median_diff_sign", "sign_change",
                      "shift_lag", "shift_lead"))
    )
}
