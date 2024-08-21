source("code/1 - Housekeeping code to be updated each refresh.R")

### 1 - SMR02 from .rds version ----

# condis = 3 (delivered)
# outcome1 = 1 (livebirth) 

births_raw <- 
  readRDS(SMR02_filename) %>%
  filter(year >= 2016 & condis == 3 & outcome1 == 1) %>%  # maternity record live births
  mutate(dataset = "SMR02",
         date = ymd(dodis),
         estgest = na_if(estgest, 99)) %>% 
  select(dataset, date, upi, numbir,
         outcome = outcome1, outcome_name = outcome1name, gestation_weeks = estgest)

births_raw <- births_raw %>% 
  mutate(gest_grp = case_when(
    gestation_weeks < 30 ~ 1,
    between(gestation_weeks, 30, 32) ~ 2, 
    between(gestation_weeks, 33, 36) ~ 3,
    between(gestation_weeks, 37, 41) ~ 4,
    between(gestation_weeks, 42, 44) ~ 5,
    TRUE ~ 9 # unknown
  )
  )

births_raw$gest_grp <- 
  factor(births_raw$gest_grp, levels = c(1, 2, 3, 4, 5, 9),
         labels = c("under 30 weeks",
                    "between 30 and 32 weeks (inclusive)",
                    "between 33 and 36 weeks (inclusive)",
                    "between 37 and 41 weeks (inclusive)",
                    "42 weeks and over (inclusive)",
                    "unknown gestation") 
  )

# pretend that the SMR02 discharge date is the admission date so we have gestation at admission

births_raw <- births_raw |> 
  arrange(date) |> 
  mutate(admission_Q = qtr(ymd(date), format = "short"),
         admission_Q = factor(admission_Q,
                              levels = c("Jan-Mar 2016", "Apr-Jun 2016", "Jul-Sep 2016", "Oct-Dec 2016",
                                         "Jan-Mar 2017", "Apr-Jun 2017", "Jul-Sep 2017", "Oct-Dec 2017",
                                         "Jan-Mar 2018", "Apr-Jun 2018", "Jul-Sep 2018", "Oct-Dec 2018",
                                         "Jan-Mar 2019", "Apr-Jun 2019", "Jul-Sep 2019", "Oct-Dec 2019",
                                         "Jan-Mar 2020", "Apr-Jun 2020", "Jul-Sep 2020", "Oct-Dec 2020",
                                         "Jan-Mar 2021", "Apr-Jun 2021", "Jul-Sep 2021", "Oct-Dec 2021",
                                         "Jan-Mar 2022", "Apr-Jun 2022", "Jul-Sep 2022", "Oct-Dec 2022",
                                         "Jan-Mar 2023", "Apr-Jun 2023", "Jul-Sep 2023", "Oct-Dec 2023",
                                         "Jan-Mar 2024", "Apr-Jun 2024", "Jul-Sep 2024", "Oct-Dec 2024"
                              ),
                              ordered = TRUE)
  )

numbers <- summarise(births_raw, .by = c(admission_Q, gest_grp), count = n()) |> 
  rename(discharge_quarter = admission_Q)

write.xlsx(numbers, file.path(data_path, "number of live births.xlsx"))

births_30_32 <- filter(births_raw, gest_grp == "between 30 and 32 weeks (inclusive)")

# generate a random number of days (0-6) to add to the gestation in weeks

count_rows <- nrow(births_30_32)

set.seed(1)

births_30_32 <- births_30_32 |> 
  mutate(gestation_days = round(runif(n = count_rows, min = 0, max = 6), 0),
         gestation = gestation_weeks + round(gestation_days/7, 2),
         days_in_neonatal = round(runif(n = count_rows, min = 40, max = 150), 0),
         corrected = round(gestation + days_in_neonatal/7, 2),
         discharge_date = date + days_in_neonatal
  )

births_30_32 <- births_30_32 |> 
  filter(discharge_date >= "2017-01-01") |> 
  arrange(discharge_date) |> 
  mutate(discharge_Q = qtr(ymd(discharge_date), format = "short"),
         discharge_Q = factor(discharge_Q,
                              levels = c("Jan-Mar 2017", "Apr-Jun 2017", "Jul-Sep 2017", "Oct-Dec 2017",
                                         "Jan-Mar 2018", "Apr-Jun 2018", "Jul-Sep 2018", "Oct-Dec 2018",
                                         "Jan-Mar 2019", "Apr-Jun 2019", "Jul-Sep 2019", "Oct-Dec 2019",
                                         "Jan-Mar 2020", "Apr-Jun 2020", "Jul-Sep 2020", "Oct-Dec 2020",
                                         "Jan-Mar 2021", "Apr-Jun 2021", "Jul-Sep 2021", "Oct-Dec 2021",
                                         "Jan-Mar 2022", "Apr-Jun 2022", "Jul-Sep 2022", "Oct-Dec 2022",
                                         "Jan-Mar 2023", "Apr-Jun 2023", "Jul-Sep 2023", "Oct-Dec 2023",
                                         "Jan-Mar 2024", "Apr-Jun 2024", "Jul-Sep 2024", "Oct-Dec 2024"
                              ),
                              ordered = TRUE)
  )

numbers <- summarise(births_30_32, .by = discharge_Q , count = n()) 

write.xlsx(numbers, file.path(data_path, "number of births at 30-32 weeks.xlsx"))

# calculate measure_value_mean and measure_value_median corrected gestation at discharge over quarters

births_30_32_median_mean <- births_30_32 |> 
  group_by(discharge_Q) %>% # quarters
    summarise(measure_value_median = round(median(corrected, na.rm = TRUE), 2),
           measure_value_mean = round(mean(corrected, na.rm = TRUE), 2),
           numbers = n()
           )

# flag pre/post-pandemic median periods

births_30_32_median_mean <- births_30_32_median_mean |> 
  mutate(median_name = case_when(
    discharge_Q <= "Oct-Dec 2019" ~ "pre-pandemic median",
    discharge_Q >= "Jul-Sep 2022" &  discharge_Q <= "Apr-Jun 2024" ~ "post-pandemic median",
    .default = NA
         )
  )

births_30_32_median_mean$median_name <- 
  factor(births_30_32_median_mean$median_name,
         levels = c("pre-pandemic median", "post-pandemic median"),
         labels = c("pre-pandemic median", # to Oct-Dec 2019 / to end Feb 2020
                    "post-pandemic median") # from Jul 2022 to end Jun 2024
  ) 

# calculate the MEDIAN of the measure_value variable over the relevant median_name - plotted as a solid line

births_30_32_median_mean <- births_30_32_median_mean |>
  group_by(median_name) |> 
  mutate(median_of_medians = 
           if_else(!is.na(median_name),
                   median(measure_value_median, na.rm = TRUE),
                   NA),
         median_of_means = 
           if_else(!is.na(median_name),
                   median(measure_value_mean, na.rm = TRUE),
                   NA),
         median_of_numbers = 
           if_else(!is.na(median_name),
                   median(numbers, na.rm = TRUE),
                   NA),
         # solid line on chart
         extended_median_of_medians = median_of_medians, 
         extended_median_of_means = median_of_means,
         extended_median_of_numbers = median_of_numbers
         ) |> 
  ungroup() |> 
  mutate(extended_median_of_medians = 
           na.locf(extended_median_of_medians, na.rm = FALSE),
         extended_median_of_means = 
           na.locf(extended_median_of_means, na.rm = FALSE),
         extended_median_of_numbers = 
           na.locf(extended_median_of_numbers, na.rm = FALSE)
         # extended is the same as median - dotted line on chart
         )

# fill-down median_name to ensure shifts are split in the correct places

births_30_32_median_mean <- births_30_32_median_mean |>
    mutate(median_name = na.locf(median_name, na.rm = FALSE)
           )

runchart_flags <- function(dataset, shift, trend, value, median) { # no flag
  
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

births_30_32_median <- runchart_flags( # looking for shifts and trends in measure_value_median
  dataset = births_30_32_median_mean,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value_median,
  median = extended_median_of_medians)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

births_30_32_median <- births_30_32_median %>% 
  mutate(
    trend = 
      if_else(orig_trend == TRUE, 
              measure_value_median, NA), # copies measure_value to plot as trend
    shift =
      if_else(orig_shift == TRUE,
              measure_value_median, NA) # copies measure_value to plot as shift
    )

# split adjacent shifts and trends that should not be connected

births_30_32_median <- add_split_gaps(
  dataset = births_30_32_median,
  measure = "trend",
  split_col_prefix = "orig_trend") %>% 
  rename(., c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

births_30_32_median <- add_split_gaps(
  dataset = births_30_32_median,
  measure = "shift",
  split_col_prefix = "orig_shift") %>% 
  rename(., c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

# reset extended values to NA where median values exist (bar last median value)

births_30_32_median <- births_30_32_median |>
  group_by(median_name) |> 
  mutate(extended_median_of_medians = if_else(
    !is.na(median_of_medians) & !is.na(median_of_medians) & is.na(lead(median_of_medians)),
    median_of_medians, NA),
    extended_median_of_medians = na.locf(extended_median_of_medians, na.rm = FALSE)
    )

# pivot wider to split median and extended into separate columns based on median_name

births_30_32_median <- births_30_32_median |>
  mutate(median_name2 = median_name) |> 
  pivot_wider(names_from = median_name2,
              values_from = median_of_medians,
              values_fill = NULL,
              names_sort = TRUE)

births_30_32_median <- births_30_32_median |> 
  pivot_wider(names_from = median_name, 
              values_from = extended_median_of_medians,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

births_30_32_median <- births_30_32_median |> 
  select(discharge_Q, measure_value_median, contains("pre-"), contains("post-"), trend, shift) |> 
  janitor::clean_names()

write.xlsx(births_30_32_median, file.path(data_path, "births at 30-32 weeks - median.xlsx"))

births_30_32_mean <- runchart_flags( # looking for shifts and trends in measure_value_mean
  dataset = births_30_32_median_mean,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value_mean,
  median = extended_median_of_means)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

births_30_32_mean <- births_30_32_mean %>% 
  mutate(
    trend = 
      if_else(orig_trend == TRUE, 
              measure_value_mean, NA), # copies measure_value to plot as trend
    shift =
      if_else(orig_shift == TRUE,
              measure_value_mean, NA) # copies measure_value to plot as shift
    )

# split adjacent shifts and trends that should not be connected

births_30_32_mean <- add_split_gaps(
  dataset = births_30_32_mean,
  measure = "trend",
  split_col_prefix = "orig_trend") %>% 
  rename(., c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

births_30_32_mean <- add_split_gaps(
  dataset = births_30_32_mean,
  measure = "shift",
  split_col_prefix = "orig_shift") %>% 
  rename(., c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

# reset extended values to NA where median values exist (bar last median value)

births_30_32_mean <- births_30_32_mean |>
  group_by(median_name) |> 
  mutate(extended_median_of_means = if_else(
    !is.na(median_of_means) & !is.na(median_of_means) & is.na(lead(median_of_means)),
    median_of_means, NA),
    extended_median_of_means = na.locf(extended_median_of_means, na.rm = FALSE)
    )

# pivot wider to split median and extended into separate columns based on median_name

births_30_32_mean <- births_30_32_mean |>
  mutate(median_name2 = median_name) |> 
  pivot_wider(names_from = median_name2,
              values_from = median_of_means,
              values_fill = NULL,
              names_sort = TRUE)

births_30_32_mean <- births_30_32_mean |> 
  pivot_wider(names_from = median_name, 
              values_from = extended_median_of_means,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

births_30_32_mean <- births_30_32_mean |> 
  select(discharge_Q, measure_value_mean, contains("pre-"), contains("post-"), trend, shift) |> 
  janitor::clean_names()


write.xlsx(births_30_32_mean, file.path(data_path, "births at 30-32 weeks - mean.xlsx"))

births_30_32_median_numbers <- births_30_32_median_mean |>
  group_by(median_name) |> 
  mutate(extended_median_of_numbers = if_else(
    !is.na(median_of_numbers) & !is.na(median_of_numbers) & is.na(lead(median_of_numbers)),
    median_of_numbers, NA),
    extended_median_of_numbers = na.locf(extended_median_of_numbers, na.rm = FALSE)
    ) |> 
  select(discharge_Q, median_name, contains("numbers"))

# pivot wider to split median and extended into separate columns based on median_name

births_30_32_median_numbers <- births_30_32_median_numbers |>
  mutate(median_name2 = median_name) |> 
  pivot_wider(names_from = median_name2,
              values_from = median_of_numbers,
              values_fill = NULL,
              names_sort = TRUE)

births_30_32_median_numbers <- births_30_32_median_numbers |> 
  pivot_wider(names_from = median_name, 
              values_from = extended_median_of_numbers,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

births_30_32_median_numbers <- births_30_32_median_numbers |> 
  select(discharge_Q, numbers, contains("pre-"), contains("post-")) |> 
  janitor::clean_names() |> 
  filter(discharge_q != "Oct-Dec 2024")

write.xlsx(births_30_32_median_numbers, file.path(data_path, "births at 30-32 weeks - numbers.xlsx"))

# buttons to remove (from plotly menu)

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

# sets default style of x and y axes

orig_xaxis_plots <- list(
  title = "",
  showticklabels = TRUE,
  tickfont = list(size = 12),                         
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE, 
  tickangle = -45 # angles the tick labels
  )
                         
orig_yaxis_plots <- list(
  title = list(text = "", font = list(size = 14), standoff = 30),
  showticklabels = TRUE,
  tickfont = list(size = 12),
  tickformat = ",d", # formats numbers with thousand separator if needed
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE  
  )

plotly_global_font <- list(
  color = "#3F3685" # phs-purple
  )

plot_ly(
  data = births_30_32_median_numbers,
  x = ~ discharge_q,
  y = ~ numbers,
  type = "scatter",
  mode = "lines+markers",
  line = list(
    color = "black", # black lines
    width = 1),
  marker = list(
    color = "black", # black dots
    size = 5),
  name = "number of babies" 
) |> 
  add_trace(
    y = ~ pre_pandemic_median, # solid blue line
    mode = "lines",
    line = list(
      color = phs_colours("phs-blue"),
      width = 1),
    marker = NULL,
    name = "average to Oct-Dec 2019"
  ) |> 
  add_trace(
    y = ~ extended_pre_pandemic_median, # dotted blue line
    mode = "lines",
    line = list(
      color = phs_colours("phs-blue"),
      width = 1,
      dash = "4"),
    marker = NULL,
    name = "projected average from Jan-Mar 2020 to Apr-Jun 2022"
  ) |> 
  add_trace(
    y = ~ post_pandemic_median, # solid magenta line
    mode = "lines",
    line = list(
      color = phs_colours("phs-magenta"),
      width = 1),
    marker = NULL,
    name = "average from Jul-Sep 2022 to Apr-Jun 2024"
  ) |> 
  add_trace(
    y = ~ extended_post_pandemic_median, # dotted magenta line
    mode = "lines",
    line = list(
      color = phs_colours("phs-magenta"),
      width = 1,
      dash = 4),
    marker = NULL,
    name = "average from Jul-Sep 2024"
  ) |> 
  layout(
    font = plotly_global_font,
    xaxis = orig_xaxis_plots,
    yaxis = orig_yaxis_plots,
    legend = list(title = list(text = "Scotland"),
                  tracegroupgap = 15,
                  orientation = "v",
                  x = 1.0,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  xanchor = "left",
                  itemclick = FALSE)
  ) |> 
  #config(modeBarButtons = list(list("zoomIn2d"), list("zoomOut2d"), list("pan3d")))
  config(displaylogo = F, displayModeBar = FALSE)
