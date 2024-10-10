source("code/1 - Housekeeping code to be updated each refresh.R")

source("code/ignore - Bev testing/testing gestation by BAPM runcharts/new counts.R")
source("code/ignore - Bev testing/testing gestation by BAPM runcharts/new calculate_medians.R")
source("code/ignore - Bev testing/testing gestation by BAPM runcharts/new runchart_flags.R")

library(labelled)
library(data.table)
library(DT)
library(shiny)
library(shinymanager)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(phsstyles)
library(phsmethods)
library(purrr)
library(fresh)

### 1 - SMR02 from .rds version ----

# condis = 3 (delivered)
# outcome1 = 1 (livebirth) 

babies_raw <- 
  readRDS(SMR02_filename) |>
  filter(year >= 2018 & condis == 3 & outcome1 == 1) |>  # maternity record live births
  mutate(dataset = "NeoCare+",
         hbtype = "Treatment",
         hbname = "Scotland",
         date = ymd(dodis),
         estgest = na_if(estgest, 99),
         month_beginning = floor_date(date, unit = "month"),
         period = "Q",
         quarter = as.Date(as.yearqtr(date))
         ) |>  # quarter beginning)
  select(dataset, hbtype, hbname, date, month_beginning, period, quarter, upi, numbir,
         outcome = outcome1, outcome_name = outcome1name, gestation_weeks = estgest,
         induced = induce1,
         apgar5 = apgar,)

### 10 - Create new variables ----

babies <- babies_raw |>  # filter(births_raw, month_beginning <= cut_off_date) |> 
  mutate(median_name = case_when(
    quarter <= "2019-10-01" ~ "median",
    between(quarter, as.Date("2022-07-01"), as.Date("2024-06-01")) ~ "post-pandemic median",
    .default = NA
  ),
  babies = 1
  ) |> 
  janitor::remove_empty("cols")

# flag gestation periods (estgest has already been recoded
# (18 thru 44 = copy)(else = 99))

babies <- babies |> 
  mutate(gest_grp = case_when(
    between(gestation_weeks, 34, 36) ~ 1,
    between(gestation_weeks, 37, 42) ~ 2,
    TRUE ~ 9 # other
  )
  )

babies$gest_grp <- 
  factor(babies$gest_grp, levels = c(1, 2, 9),
         labels = c("between 34 and 36 weeks (inclusive)", # late pre-term
                    "between 37 and 42 weeks (inclusive)", # term and post-term
                    "other gestation") 
  )

# flag not induced, induced, not known in new_induced
# induced = 0 - not induced, 1 - induced, 2 - unknown

babies <- babies |> 
  mutate(new_induced = case_when(
    induced == 0 ~ 1, # not induced
    induced == 1 ~ 2, # induced
    TRUE ~ 9) # not known
  )

# live births, 37_42 weeks
# if still birth or any other gestation reset new_induced to NA

babies <- babies |>
  mutate(new_induced = if_else(numbir == 1 &
  outcome == 1 & between(gestation_weeks, 37, 42), new_induced, NA_real_)
  )

babies$new_induced <- 
  factor(babies$new_induced,
                                levels = c(1, 2, 9),
                                labels = c("not induced",
                                           "induced",
                                           "unknown whether induced")
                                )

# flag apgar 0-6, apgar 7-10, known apgar, unknown apgar in NEWAPGAR
# APGAR = '00' to '06' - low (<7) apgar score
# APGAR = '07' to '10' - apgar5 score of 7 or more
# APGAR = 'NR' - not recorded
# APGAR = 'RR' - not available as baby was being resuscitated

babies <- babies |> 
  mutate(new_apgar5 = case_when(
    apgar5 %in% c('00', '01', '02', '03', '04', '05', '06') ~ 1, # low (<7) apgar score
    apgar5 %in% c('07', '08', '09', '10') ~ 2, # apgar5 score of 7 or more
    TRUE ~ 9 # not recorded/not not available/not known
  )
  )

# live births, 37_42 weeks
# if stillbirth or any other gestation reset new_apgar5 to NA

babies <- babies |> 
  mutate(new_apgar5 = if_else(numbir == 1 &
  outcome == 1 & between(gestation_weeks, 37, 42), new_apgar5, NA_real_)
  )

babies$new_apgar5 <- 
  factor(babies$new_apgar5, levels = c(1, 2, 9),
         labels = c("low (<7) apgar5 score", "apgar5 score of 7 or more",
                    "apgar5 score unknown")
  )

numbers <- summarise(babies, .by = c(quarter, gest_grp), count = n())
write.xlsx(numbers, file.path(data_path, "number of live births by gestation group.xlsx"))

# generate a random BAPM level (1-3) to add to the dataset

count_rows <- nrow(babies)

babies <- babies |> 
  mutate(BAPM = round(runif(n = count_rows, min = 1, max = 100), 0)
  )

babies <- babies |> 
  mutate(BAPM = case_when( # not definitive, would depend on gestation - made up numbers
    between(BAPM, 1, 3) ~ 1, # roughly 3% intensive care
    between(BAPM, 4, 5) ~ 2, # roughly 2% high dependency care
    between(BAPM, 6, 13) ~ 3, # roughly 7% special care 
    .default = 9
  )
  )

babies$BAPM = factor(babies$BAPM, levels = c(1, 2, 3, 9),
                     labels = c("intensive care",
                                "high dependency care",
                                "special care",
                                "other or not needed"),
                     ordered = TRUE
                     )

numbers <- summarise(babies, .by = c(quarter, gest_grp, BAPM), count = n())
write.xlsx(numbers, file.path(data_path, "number of live births by gestation and BAPM.xlsx"))

# # and % of babies
# calculate NUMBER and PERCENTAGE of VARIABLE in SUBGROUP compared with total
# these are the points plotted on small multiple and runcharts (Q),
# not shown in multi indicator overview (FY, CY)

gestations <-
  counts(
    dataset = babies,
    variable = gest_grp,
    date = quarter,
    tally_var = babies,
    suffix = "%", # for hovertext
    measure = "BAPM GEST"
  )

# BAPM <- 
#     counts(
#     dataset = babies,
#     variable = BAPM,
#     date = quarter,
#     tally_var = babies,
#     suffix = "%", # for hovertext
#     measure = "BAPM BAPM"
#   )
# 
# BAPM_by_gestations <- 
#   counts(
#     dataset = babies,
#     variable = gest_grp,
#     date = quarter,
#     subgroup = BAPM,
#     tally_var = babies,
#     suffix = "%", # for hovertext
#     measure = "BAPM BAPM+GEST"
#   )

gestations_by_BAPM <- 
  counts(
    dataset = babies,
    variable = BAPM,
    date = quarter,
    subgroup = gest_grp,
    tally_var = babies,
    suffix = "%", # for hovertext
    measure = "BAPM GEST+BAPM"
  )

inductions <- # to test everything looks the same as before
  counts(
    dataset = babies,
    variable = new_induced,
    date = quarter,
    tally_var = babies,
    suffix = "%", # for hovertext
    measure = "INDUCTIONS"
  ) 

apgar5 <- # to test everything looks the same as before
  counts(
    dataset = babies,
    variable = new_apgar5,
    date = quarter,
    tally_var = babies,
    suffix = "%", # for hovertext
    measure = "APGAR5"
  ) 

runchart_categories <- c("induced", "low (<7) apgar5 score", "3rd or 4th degree tears",
                         "spontaneous vaginal births", "assisted vaginal births",
                         "all caesarean births", "planned caesarean births",
                         "unplanned caesarean births", "under 32 weeks",
                         "between 32 and 36 weeks (inclusive)", "under 37 weeks", "between 37 and 41 weeks (inclusive)",
                         "42 weeks and over (inclusive)", "between 18 and 44 weeks (inclusive)",
                         "all pregnancies booked", "all terminations", "average gestation",
                         "intensive care", "high dependency care", "special care")

subgroup_categories <- c("between 34 and 36 weeks (inclusive)", # late pre-term
                         "between 37 and 42 weeks (inclusive)") # term/post-term
                         

everything_dataframe <- bind_rows(#gestations,
                                  #BAPM,
                                  # BAPM_by_gestations,
                                  gestations_by_BAPM,
                                  inductions,
                                  apgar5)

saveRDS(everything_dataframe, paste0(data_path, "/", "everything_dataframe.rds"))

runchart_dataframe <- everything_dataframe |> 
  filter(measure_cat %in% runchart_categories) |> 
  filter(is.na(subgroup_cat) | subgroup_cat != "other gestation")
  
# set median_name as a factor to keep order

runchart_dataframe$median_name <- factor(runchart_dataframe$median_name,
                      levels = c("median", "revised median", "post-pandemic median"),
                      labels = c("median", # to Oct-Dec 2019 / to end Feb 2020
                                 "revised median", # FV/TAY (Gestation at booking)
                                 "post-pandemic median") # from Jul 2022 to end Jun 2024
                      ) 

### i - MEDIAN of measure_value ----

# calculate the MEDIAN of the measure_value variable over the relevant median_name - plotted as a solid line

runchart_dataframe <- calculate_medians(dataset = runchart_dataframe,
                                        date = quarter,
                                        measure_value = measure_value,
                                        subgroup_cat = subgroup_cat)

runchart_dataframe <- runchart_flags(
  dataset = runchart_dataframe,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  median = extended)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

runchart_dataframe <- runchart_dataframe |> 
  mutate(
    trend = 
      if_else(orig_trend == TRUE, 
              measure_value, NA), # copies measure_value to plot as trend
    shift =
      if_else(orig_shift == TRUE,
              measure_value, NA) # copies measure_value to plot as shift
    )

# split adjacent shifts and trends that should not be connected

runchart_dataframe <- add_split_gaps(
  dataset = runchart_dataframe,
  measure = "trend",
  split_col_prefix = "orig_trend") |> 
  rename(c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

runchart_dataframe <- add_split_gaps(
  dataset = runchart_dataframe,
  measure = "shift",
  split_col_prefix = "orig_shift") |> 
  rename(c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

# reset extended values to NA where median values exist (bar last median value)

runchart_dataframe <- runchart_dataframe |>
  group_by(dataset, hbtype, hbname, period, measure, measure_cat, subgroup_cat, median_name) |> 
  mutate(extended = if_else(
    !is.na(median) & !is.na(extended) & is.na(lead(median)),
    median, NA),
    extended = na.locf(extended, na.rm = FALSE)
    )

# pivot wider to split median and extended into separate columns based on median_name

runchart_dataframe <- runchart_dataframe |>
  mutate(median_name2 = median_name) |> 
  pivot_wider(names_from = median_name2,
              values_from = median,
              values_fill = NULL,
              names_sort = TRUE)

runchart_dataframe <- runchart_dataframe |> 
  pivot_wider(names_from = median_name, 
              values_from = extended,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

runchart_dataframe <- runchart_dataframe |> 
  janitor::clean_names()

# save off the median information to add to the download dataset created below

medians <- runchart_dataframe |> 
  ungroup() |> 
  select(dataset:measure_cat, subgroup_cat, median:extended_post_pandemic_median) |> 
  arrange(measure_cat) |> 
  mutate(across(c(median:extended_post_pandemic_median), ~ round(., 2))
         )|> 
  distinct() # removes duplicates created to split shifts and trends

### iii - Tidy up ----

# calculate overall range of dates - need to review these

date_range_Q <- as.Date(range(filter(everything_dataframe, period == "Q")$quarter))

date_range <- range(everything_dataframe$quarter)

# create a vector for the chart labels to force first label to Jan-Mar 2018

x_date_labels_Q <-
  seq(
    from = min(date_range_Q),
    to = max(date_range_Q),
    by = "3 months"
  )

x_date_labels_Q2 <- 
  qtr(x_date_labels_Q, format = "short")

# add quarter_label and round values

runchart_dataframe <- runchart_dataframe |>
  mutate(quarter_label = if_else(period == "Q",
                                 qtr(ymd(quarter), format = "short"),
                                 NA),
         quarter_label = factor(quarter_label,
                                levels = x_date_labels_Q2,
                                ordered = TRUE),
         across(c(measure_value, median:extended_post_pandemic_median, trend, shift), ~ round(., 3)),
         num = if_else(measure %in% c("BOOKINGS", "TERMINATIONS"), NA, num)
  ) |>
  select(any_of(c("dataset", "measure", "hbtype", "hbname", "period", "quarter", "quarter_label", "measure_cat", "subgroup", "subgroup_cat", "num", "den", "measure_value", "suffix", "median", "extended_median", "post_pandemic_median", "extended_post_pandemic_median", "trend", "shift")
  )
  ) |> 
  ungroup()

# buttons to remove (from plotly menu)

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

# create static labels for the runchart legends

orig_trend_label <-  
  paste0("trends: 5 or more consistently increasing", "<br>", "or decreasing points")
orig_shift_label <-  
  paste0("shifts: 6 or more consecutive points", "<br>", "above or below average")

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
 
# GESTATION BY BAPM SPECIFIC

# create a tibble with "nice" (superscript text) gestations for gestation by BAPM measure

# subgroup_cat_order

subgroup_cat_order <- c("between 34 and 36 weeks (inclusive)",
                        "between 37 and 42 weeks (inclusive)"
                        )

# formatted_name is the factor which controls the order in which the context charts legends should appear

short_formatted_name <- c("late pre-term", "term/post-term")

long_formatted_name <- c(paste0("late pre-term (34", "<sup>+0</sup>", " to 36", "<sup>+6</sup>", " weeks gestation)"),
                    paste0("term/post-term (37", "<sup>+0</sup>", " to 41", "<sup>+6</sup>", " weeks gestation)")
                    )

nicename <- tibble(subgroup_cat_order, short_formatted_name, long_formatted_name)

nicename$short_formatted_name <- factor(nicename$short_formatted_name, levels = short_formatted_name)

nicename$long_formatted_name <- factor(nicename$long_formatted_name, levels = long_formatted_name)

rm(subgroup_cat_order)

BAPM_plotListNames = c("intensive care", "high dependency care", "special care") # placeholder for plots

gest_by_BAPM_data <- filter(runchart_dataframe,
                            measure == "BAPM GEST+BAPM" &
                              measure_cat %in% BAPM_plotListNames &
                              subgroup_cat %in% c("between 34 and 36 weeks (inclusive)",
                                                  "between 37 and 42 weeks (inclusive)")
)

gest_by_BAPM_data <- left_join(gest_by_BAPM_data,
                               nicename,
                               by = c("subgroup_cat" = "subgroup_cat_order")
)

gest_by_BAPM_data$measure_cat <- factor(gest_by_BAPM_data$measure_cat,
                                        levels = BAPM_plotListNames)

gest_by_BAPM_data <- arrange(gest_by_BAPM_data, 
                             quarter,
                             subgroup_cat,
                             measure_cat)

  # set up x-axis chart labels

SMR02_date_range <- unique(gest_by_BAPM_data$quarter)
SMR02_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 2)]
SMR02_date_ticktext <- qtr(SMR02_date_tickvals, format = "short")

SMR02_multiples_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 4)]
SMR02_multiples_date_ticktext <- qtr(SMR02_multiples_date_tickvals, format = "short")


#gest_by_BAPM_runchart_data <- split(gest_by_BAPM_runchart_data, ~ measure_cat)

Date <-  "2019"
HBType <- "RESIDENCE"
HBName <- "NHS Borders"

Selected <- data.frame(Date, HBType, HBName)

Selected$BAPM_Subgroup_cat <- "between 34 and 36 weeks (inclusive)"
y_max_gestation_by_BAPM <- max(gest_by_BAPM_data$measure_value, na.rm = TRUE)

gest_by_BAPM_runchart_data <- ({
    # selects data
    
    #req(input$BAPM_subgroup_cat)
    
    data <- gest_by_BAPM_data |> 
      filter(subgroup_cat == Selected$BAPM_Subgroup_cat) |> 
      mutate(num_label = paste0("Number of ", short_formatted_name, " babies admitted to ", measure_cat, ": "),
             den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
             measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
             date = quarter
      ) |>  
      set_variable_labels(
        median = " average to Oct-Dec 2019",
        extended_median = " projected average from Jan-Mar 2020",
        post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
        extended_post_pandemic_median = "projected average from Jul 2024") 
    
    new_labels = c(unique(c(data$num_label, data$measure_label)), rep(unique(data$den_label), 3))
    
    new_max <- max(data$measure_value) # local maximum measure_value
    
    # observeEvent(new_max, {   # update local maximum measure_value when subgroup_cat changes
    #   y_max_gestation_by_BAPM(new_max)
    # }
    # )
 
    data <- data %>% 
      split(.$measure_cat)
    
    for (i in seq_along(data)){
      var_label(data[[i]]$num) <- new_labels[[i]]
      var_label(data[[i]]$measure_value) <- new_labels[[i+3]]
      var_label(data[[i]]$den) <- new_labels[[i+6]]
    }

    for (i in seq_along(data)){
      data[[i]]$mytext <- paste0("Quarter: ",
                                 data[[i]]$quarter_label,
                                 "<br>",
                                 var_label(data[[i]]$num),
                                 prettyNum(data[[i]]$num, big.mark = ","), # data[[i]]$num,
                                 "<br>",
                                 var_label(data[[i]]$den),
                                 prettyNum(data[[i]]$den, big.mark = ","), # data[[i]]$den,
                                 "<br>",
                                 "Percentage of babies: ", # not MEASURE_LABEL - too long
                                 format(data[[i]]$measure_value,
                                        digits = 1,
                                        nsmall = 1),
                                 "%")
    }
    
    data
    
    })
