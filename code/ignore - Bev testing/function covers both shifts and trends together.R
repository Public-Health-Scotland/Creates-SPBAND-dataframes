# Function to flag shifts and trends on run chart data
# Parameters:
# shift: the name for the new variable where shift is flagged
# trend: the name for the new variable where trend is flagged
# value: the name of the variable which contains the value being evaluated
# median: the name of the variable which contains the median against which value is tested

controlchart_flags <- function(dataset, shift, trend, value, mean) {
  
    dataset <- dataset %>%
    
  # checks each value (shift_i) and the preceding 7 values against the median - if all above or all below median then shift_i is TRUE,  i.e. it is the 8th consecutive value above the median, otherwise FALSE
    
    mutate(
      shift_i = tidytable::case_when(
        ({{value}} > {{mean}} & lag({{value}}, 1) > {{mean}} &
           lag({{value}}, 2) > {{mean}} & lag({{value}}, 3) > {{mean}} &
           lag({{value}}, 4) > {{mean}} & lag({{value}}, 5) > {{mean}} &
           lag({{value}}, 6) > {{mean}} & lag({{value}}, 7) > {{mean}})
        | ({{value}} < {{mean}} & lag({{value}}, 1) < {{mean}} &
             lag({{value}}, 2) < {{mean}} & lag({{value}}, 3) < {{mean}} &
             lag({{value}}, 4) < {{mean}} & lag({{value}}, 5) < {{mean}} &
             lag({{value}}, 6) < {{mean}} & lag({{value}}, 7) < {{mean}}) ~ TRUE,
        TRUE ~ FALSE),
      
  # checks whether each value (shift_i) or any of the following 7 values are TRUE - identifies value (shift_i) as part of a shift
      
      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE | lead(shift_i, 6) == TRUE
        | lead(shift_i, 7) == TRUE ~ TRUE,
        TRUE ~ FALSE),
      
  # checks each value (trend_i) and the preceding 5 values against its preceding value - if all are bigger or all are smaller then trend_i is TRUE,  i.e. it is the 6th consecutive increasing or decreasing value, otherwise FALSE
      
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4) &
           lag({{value}}, 4) > lag({{value}}, 5)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4)
           & lag({{value}}, 4) < lag({{value}}, 5)) ~ TRUE,
        TRUE ~ FALSE),
      
  # checks whether each value (trend_i) or any of the following 5 values are TRUE - identifies value (trend_i) as part of a trend
      
      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE | lead(trend_i, 5) == TRUE ~ TRUE,
        TRUE ~ FALSE)
    ) %>%
    
    rename({{shift}}:=shift, {{trend}}:=trend) %>%
    select(-shift_i, -trend_i)

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
    ) #%>%

    # Remove columns that are no longer needed

    # select(- all_of(c("gradient", "gradient_lag_change", "gradient_lead_change",
    #                   "trend_lag", "trend_lead"))
    # )

# There is a similar issue for shifts. There can't be one point in two shifts,
# but the last point in one can be adjacent to the first point in the next.
# Again we don't want to connect the lines.
#
# Problematic points are surrounded by other shift points and are on the
# opposite side of the median from the preceding point.

  dataset <-
    dataset %>%

    # Find whether sign has changed from preceding point

    mutate(mean_diff_sign = sign({{value}} - {{mean}}),
           sign_change = mean_diff_sign != lag(mean_diff_sign)
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

    # select(- all_of(c("mean_diff_sign", "sign_change",
    #                   "shift_lag", "shift_lead"))
    # )
}

# Function to split adjacent shifts and trends that should not be connected
# i.e. the end point of one and the start point of the next should not be joined by 
# the green trend line (or yellow dots)?
# These were marked in the data during prep, now we just need to add a row with measure = NA 
# between the shifts/trends.
# Parameters:
# dataset: the dataframe with shift or trend data
# measure: the measure variable
# split_col_prefix: string prefix for column that identifies split locations
  
add_split_gaps <- function(dataset, measure, split_col_prefix){
  dataset <- 
      dataset %>%

      # How many times should each row be included? Splits should be included
      # twice, and FALSE + 1 = 1, TRUE + 1 = 2
      
      mutate(across(all_of(paste0(split_col_prefix, ".split")),
                    ~.x + 1, .names = "num_rows")
             ) %>%

      # If a row should be included more than once, duplicate it

      uncount(weights = num_rows, .id = "dup_row", .remove = FALSE) %>%

      # We want measure to be NA for the first row in each split
      mutate(across(all_of(measure),
                    ~if_else((num_rows == 2) & (dup_row == 1), NA_real_, .x))
             )
}
