library(openxlsx2)
library(janitor)

source("code/1 - Housekeeping code to be updated each refresh.R")

orig_trend_label <-  
  paste0("trends: 6 or more consistently increasing", "<br>", "or decreasing points")
orig_shift_label <-  
  paste0("shifts: 8 or more consecutive points", "<br>", "above or below average")

### i - read in testdata ----

testdata <- read_xlsx(paste0(data_path, "/extremely_preterm_2024-03-14.xlsx"),
                      sheet = "as is",
                      rows = c(1:77),
                      cols = c(1:12)
) %>% 
  janitor::clean_names()

testdata <- testdata %>% 
  mutate(outlier = if_else(measure_value < lower_control_limit, # can't be higher than 100%
                           measure_value,
                           NA),
         outer_one_third = dplyr::between(measure_value, lower_control_limit, lower_warning_limit),
         inner_one_third = dplyr::between(measure_value, lower_one_third_limit, upper_one_third_limit),
         on_centreline = measure_value == centreline,
         equals_following_value = measure_value == lead(measure_value, default = FALSE)
         )

### ii - Mark SHIFTS and TRENDS ----

testdata <- control_chart_flags(
  dataset = testdata,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  mean = centreline)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

testdata <- testdata %>% 
  mutate(
    trend = 
      if_else(orig_trend == TRUE, 
              measure_value, NA), # copies measure_value to plot as trend
    shift =
      if_else(orig_shift == TRUE,
              measure_value, NA) # copies measure_value to plot as shift
    )

# split adjacent shifts and trends that should not be connected

testdata <- add_split_gaps(
  dataset = testdata,
    measure = "shift",
  split_col_prefix = "orig_shift") %>% 
  rename(., c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

testdata <- add_split_gaps(
  dataset = testdata,
  measure = "trend",
  split_col_prefix = "orig_trend") %>% 
  rename(., c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

### iii - chart ----

extremely_preterm_control_chart <- 

  plot_ly(
    data = testdata,
    x = ~ date_label,
    y = ~ trend, # green trend line needs to be plotted first or it obliterates the others
    type = "scatter",
    mode = "lines",
    line = list(
      color = "lightgreen",
      width = 10
    ),
    name = orig_trend_label,
    legendgroup = "trend",
    legendrank = 1003,
    #showlegend = ~ include_trend_shift_legend,
    hovertext = "",
    hoverinfo = "none"
  ) %>%
  add_trace(
    y = ~ measure_value, # percentage
    mode = "lines+markers",
    line = list(
      color = "black", # black line
      width = 2
    ),
    marker = list(
      color = "black", # black dots
      size = 5
    ),
    name = ~ "percentage",
    hovertext = ~ paste0("Quarter: ",
                         date_label,
                         "<br>",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"
    ), # need to add hover text for percentage only
    hoverinfo = "text"
  ) %>%
  add_lines(
    y = ~ centreline, # mean (centreline)
    line = list(
      color = phs_colours("phs-blue"), # dotted blue line
      dash = "4",
      width = 2
    ),
    marker = NULL,
    name = "centreline",
    hovertext = ~ paste0("Centreline: ",
                         "<br>",
                         format(centreline,
                                digits = 1,
                                nsmall = 2),
                         "%"
    ),
    hoverinfo = "text"
  ) %>%
  add_lines(
    y = ~ lower_warning_limit, # lower warning limit
    line = list(
      color = selected_colours[11], # phs-blue-80 line
      dash = "1",
      width = 2
    ),
    marker = NULL,
    name = "warning limits",
    legendgroup = "warning limits",
    showlegend = TRUE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ upper_warning_limit, # upper warning limit
    line = list(
      color = selected_colours[11], # phs-blue-80 line
      dash = "1",
      width = 2
    ),
    marker = NULL,
    name = "warning limits",
    legendgroup = "warning limits",
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ lower_control_limit, # lower control limit
    line = list(
      color = "red", # red line
      dash = "2",
      width = 2
    ),
    marker = NULL,
    name = "control limits",
    legendgroup = "control limits",
    showlegend = TRUE,
    hoverinfo = "none"
  ) %>%
  add_lines(
    y = ~ upper_control_limit, # upper control limit
    line = list(
      color = "red", # red line
      dash = "2",
      width = 2
    ),
    marker = NULL,
    name = "control limits",
    legendgroup = "control limits",
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>% 
  add_trace(
    y = ~ shift, # orange lines
    mode = "lines",
    line = list(
      color = "orange", # orange lines (prevents missing data warning)
      width = 2),
    marker = NULL,
    name = orig_shift_label,
    legendgroup = "shift",
    legendrank = 1004,
    #showlegend = ~ include_trend_shift_legend,
    hovertext = "",
    hoverinfo = "none"
  ) %>%
  layout(xaxis = orig_xaxis_plots,
         yaxis = orig_yaxis_plots)

extremely_preterm_control_chart
