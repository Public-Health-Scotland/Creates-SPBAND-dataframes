library(openxlsx2)
library(janitor)

source("code/1 - Housekeeping code to be updated each refresh.R")

orig_trend_label <-  
  paste0("trends: 6 or more consistently increasing", "<br>", "or decreasing points")
orig_shift_label <-  
  paste0("shifts: 8 or more consecutive points", "<br>", "above or below average")


testdata <- read_xlsx(paste0(data_path, "/extremely_preterm_2024-03-14.xlsx"),
                      sheet = "as is",
                      rows = c(1:77),
                      cols = c(1:12)
) |> 
  janitor::clean_names()

testdata <- testdata %>% 
  mutate(outlier = if_else(measure_value > upper_control_limit |
                             measure_value < lower_control_limit,
                           measure_value,
                           NA),
         point_on_mean = if_else(measure_value == centreline,
                                  TRUE,
                                  FALSE),
         same_as_previous = if_else(measure_value == lag(measure_value),
                                    TRUE,
                                    FALSE,
                                    missing = FALSE),
         in_outer_third = if_else(between(measure_value, upper_warning_limit, upper_control_limit) |
                                    between(measure_value, lower_control_limit,lower_warning_limit),
                                  TRUE,
                                  FALSE),
         in_inner_third = if_else(between(measure_value, lower_one_third_limit, upper_one_third_limit),
                                  TRUE,
                                  FALSE)
         # rule_4 = if_else(in_outer_third & lag(in_outer_third) | )
         )

testdata_unadjusted <- testdata

testdata_without_points_on_mean <- filter(testdata,
                                          point_on_mean == FALSE)

testdata_without_same_as_previous <- filter(testdata,
                                            same_as_previous == FALSE)

##########################################################################################################################

### SHIFTS ### ----

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
    ) %>%

    # Remove columns that are no longer needed

    select(- all_of(c("sign_change",
                      "shift_lag", "shift_lead"))
    )
}

testdata_unadjusted <- mark_shifts(
  dataset = testdata_unadjusted,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  median = centreline
  )

testdata_unadjusted <- testdata_unadjusted %>% 
  mutate(
    # trend = 
    #   if_else(orig_trend == TRUE, 
    #           `measure_value`, NA), # copies measure_value to plot as trend
    unadj_shift =
      if_else(orig_shift == TRUE,
              `measure_value`, NA) # copies measure_value to plot as shift
    )

testdata_unadjusted <- add_split_gaps( # look for split shifts
  dataset = testdata_unadjusted,
  measure = "unadj_shift",
  split_col_prefix = "orig_shift")

# create chart with shifts

creates_extremely_preterm_control_chart(plotdata = testdata_unadjusted,
                                        shift = "unadj_shift")

# misses last shift with sets of points on the median

# now rerun on data with points on the centreline removed

testdata_without_points_on_mean <- mark_shifts(
  dataset = testdata_without_points_on_mean,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  median = centreline
  )

testdata_without_points_on_mean <- testdata_without_points_on_mean %>% 
  mutate(
    # trend = 
    #   if_else(orig_trend == TRUE, 
    #           `measure_value`, NA), # copies measure_value to plot as trend
    rem_shift =
      if_else(orig_shift == TRUE,
              `measure_value`, NA) # copies measure_value to plot as shift
    )

testdata_without_points_on_mean <- add_split_gaps( # points on the centreline don't count so use this dataset to look for split shifts
  dataset = testdata_without_points_on_mean,
  measure = "rem_shift",
  split_col_prefix = "orig_shift")

# create chart with shifts to compare

creates_extremely_preterm_control_chart(plotdata = testdata_without_points_on_mean,
                                        shift = "rem_shift")

# correctly highlights last shift with several points on the median

# now add the missing points on the median back in

testdata_with_points_on_mean_added_back_in <- 
  left_join(select(testdata_unadjusted,
                   - c(orig_shift, orig_shift.split)),
            select(testdata_without_points_on_mean,
                   -c(median_diff_sign)),
            by = join_by(dataset, date_label, sub_category, measure_value, centreline, sd, lower_one_third_limit,
upper_one_third_limit, lower_warning_limit, upper_warning_limit, lower_control_limit, upper_control_limit, outlier, point_on_mean,
same_as_previous),
            multiple = "all" # to accommodate duplicate rows to split shifts
            ) 

# create a new_shift_value to pick up all points

testdata_with_points_on_mean_added_back_in <- testdata_with_points_on_mean_added_back_in |> 
  mutate(new_shift_value = if_else(point_on_mean &
                                     (point_on_mean != lag(point_on_mean)) &
                                     lag(median_diff_sign) == lead(median_diff_sign),
                                   measure_value,
                                   rem_shift,
                                   missing = FALSE)
  )

creates_extremely_preterm_control_chart(plotdata = testdata_with_points_on_mean_added_back_in,
                                        shift = "new_shift_value")

##########################################################################################################################

### TRENDS ### ----

mark_trends <- function(dataset, shift, trend, value, median) { # 6 or more consecutive points increasing or decreasing  - observations that lie on the centreline or are equal to the preceding value are not counted
  
  dataset <- dataset %>%
    mutate(
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3) & lag({{value}}, 3) > lag({{value}}, 4) 
         & lag({{value}}, 4) > lag({{value}}, 5) & lag({{value}}, 5)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3) & lag({{value}}, 3) < lag({{value}}, 4) 
           & lag({{value}}, 4) < lag({{value}}, 5) & lag({{value}}, 5)) ~ TRUE,
        TRUE ~ FALSE),

      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE
        | lead(trend_i, 5) == TRUE ~ TRUE,
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
    ) #%>%

    # Remove columns that are no longer needed

    # select(- all_of(c("gradient_lag_change", "gradient_lead_change",
    #                   "trend_lag", "trend_lead"))
    # )
}

testdata_unadjusted <- testdata |> 
  tibble::rowid_to_column() # creates a row number variable 

testdata_unadjusted <- mark_trends(
  dataset = testdata_unadjusted,
  shift = "orig_shift",
  trend = "orig_trend",
  value = `measure_value`,
  median = `centreline`
  )

testdata_unadjusted <- testdata_unadjusted %>% 
  mutate(
    # trend = 
    #   if_else(orig_trend == TRUE, 
    #           `measure_value`, NA), # copies measure_value to plot as trend
    unadj_trend =
      if_else(orig_trend == TRUE,
              `measure_value`, NA) # copies measure_value to plot as shift
    )

testdata_unadjusted <- add_split_gaps( # look for split trends
  dataset = testdata_unadjusted,
  measure = "unadj_trend",
  split_col_prefix = "orig_trend") 

# create chart with trends

creates_extremely_preterm_control_chart(plotdata = testdata_unadjusted,
                                        trend = "unadj_trend")

# correctly identifies three trends and splits the first two but misses last trend with sets of points same as previous

# now rerun on data with same as previous points removed 

testdata_without_same_as_previous <- filter(testdata_unadjusted,
                                            same_as_previous == FALSE)

testdata_without_same_as_previous <- mark_trends(
  dataset = testdata_without_same_as_previous,
  shift = "orig_shift",
  trend = "rev_orig_trend",
  value = `measure_value`,
  median = `centreline`
)

testdata_without_same_as_previous <- testdata_without_same_as_previous %>% 
  mutate(
    # trend = 
    #   if_else(orig_trend == TRUE, 
    #           `measure_value`, NA), # copies measure_value to plot as trend
    revised_trend =
      if_else(rev_orig_trend == TRUE,
              `measure_value`, NA) # copies measure_value to plot as shift
  )

testdata_without_same_as_previous <- add_split_gaps( # same as previous points don't count so use this dataset to look for split trends
  dataset = testdata_without_same_as_previous,
  measure = "revised_trend",
  split_col_prefix = "rev_orig_trend") |> 
  rename(revised_num_rows = num_rows,
         revised_dup_row = dup_row) |> 
  select(- c(gradient:trend_lead))

# create chart with trends with same as previous points removed

creates_extremely_preterm_control_chart(plotdata = testdata_without_same_as_previous,
                                        trend = "revised_trend")

# correctly picks up last trend with two sets of same as previous points

# now add the same as previous points back in

testdata_with_same_as_previous_added_back_in <- 
  left_join(testdata_unadjusted, testdata_without_same_as_previous,
            by = join_by(rowid, dataset, date_label, sub_category, measure_value, centreline, sd, lower_one_third_limit,
upper_one_third_limit, lower_warning_limit, upper_warning_limit, lower_control_limit, upper_control_limit, outlier, point_on_mean,
same_as_previous, in_outer_third, in_inner_third, orig_trend, orig_trend.split, unadj_trend),
            multiple = "all" # to accommodate duplicate rows to split shifts)
  )

testdata_with_same_as_previous_added_back_in <- 
  testdata_with_same_as_previous_added_back_in |> 
  mutate(new_dup_row = if_else(dup_row == 2 | revised_dup_row == 2, 2, NA))
  
# now recode new_trend_value to pick up all points

testdata_with_same_as_previous_added_back_in2 <- testdata_with_same_as_previous_added_back_in |> 
  mutate(
    new_trend_value = case_when(
      orig_trend.split == TRUE ~ "o1",
      rev_orig_trend.split == TRUE ~ "r1",
      orig_trend != rev_orig_trend & lead(gradient) == 0 & gradient != lead(gradient, 2) ~ "o2",
      orig_trend != rev_orig_trend ~ "r2",
      gradient == 0 & lag(gradient) == lead(gradient) ~ "MV",
      orig_trend != rev_orig_trend & lead(gradient) == 0 & gradient == lead(gradient, 2) ~ "o3",
      orig_trend == TRUE & is.na(rev_orig_trend) ~ "o4",
      orig_trend == rev_orig_trend ~ "o5",
      
      .default = "o6"
    # orig_trend != rev_orig_trend & lead(gradient) == 0 ~ unadj_trend,
    # orig_trend == TRUE & is.na(rev_orig_trend) ~ unadj_trend,
    # gradient == 0 & lag(gradient) == lead(gradient) ~ measure_value,
    # gradient == 0 & lead(gradient) == 0 & lag(gradient) != lead(gradient, 2) ~ unadj_trend,
    # #gradient == 0 & num_rows != 2 ~ measure_value,
    # orig_trend.split == TRUE ~ unadj_trend,
    # rev_orig_trend.split == TRUE ~ revised_trend,
    # orig_trend == rev_orig_trend ~ unadj_trend,
    # orig_trend != rev_orig_trend ~ revised_trend
    # (unadj_trend == revised_trend) & gradient != 0 ~ unadj_trend,
    ),
    value = case_match(
      new_trend_value,
      c("o1", "o2", "o3", "o4", "o5", "o6") ~ unadj_trend,
      "MV" ~ measure_value,
      c("r1", "r2") ~ revised_trend
      )
    )
    
creates_extremely_preterm_control_chart(plotdata = testdata_with_same_as_previous_added_back_in2,
                                        trend = "value")

# trends are now correct but is this code fit for future use?



