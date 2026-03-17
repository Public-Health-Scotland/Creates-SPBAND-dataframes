####
# Code to update the download data that was republished on 15th February 2024 with data run on 15th Dec 2023
# To fix "new" median average gestation at booking for Forth Valley and Tayside
# Bev Dodds
# 28 February 2024
# Last update by Bev Dodds
# Latest update description: recreated files with corrected medians/shifts/trends
# Type of script - amendment of existing dataframes
# Written/run on R Studio Server
# Version of R - 4.1.2 - note use of dplyr 1.1.0
# Approximate run time - < 1 minute
####

### 1 - Housekeeping ----

rm(list = ls())

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R") # ensure refresh_date = 2023-12-15

print(refresh_date) # check!

# load 15 Dec 24 everything_dataframe to get date ranges

everything_dataframe <- readRDS("/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Publications/SPBAND/dataframes/Bev/data/2023-12-15 extract/everything_dataframe.rds")

# calculate date_range_Q

date_range_Q <- as.Date(range(filter(everything_dataframe, period == "Q")$date))

# create a vector for the chart labels to force first label to Jan-Mar 2017

x_date_labels_Q <-
  seq(
    from = min(date_range_Q),
    to = max(date_range_Q),
    by = "3 months"
  )

x_date_labels_Q2 <- 
  qtr(x_date_labels_Q, format = "short")

rm(everything_dataframe)

# load SMR02-ABC-Terminations-tweaked-2024-02-08.RData - loads annual_dataframe, download_dataframe and runchart_dataframe -
# this has the erroneous TERMINATIONS data removed (2024-01-17 revision to terminations download data.R)
# AND the tweaks to the measure_cat etc. (2024-02-08 tweaking content in downloads.R)

load("data/2023-12-15 extract/dashboard_dataframes/SMR02-ABC-Terminations-tweaked-2024-02-08.RData")

# annual_dataframe is unchanged

# amend runchart_dataframe

fixed_median_runchart_dataframe <- runchart_dataframe %>% # drop variables that will be recreated below
  select(- c(new_median:quarter_label)) #%>% 
  #filter(hbtype == "RESIDENCE" & hbname == "NHS Forth Valley" & measure == "GESTATION AT BOOKING")

fixed_median_runchart_dataframe <- unique(fixed_median_runchart_dataframe) # removes duplicate records where split shifts/trends were noted

# flags 12 months for new medians - was previously wrongly using 4 and 6 months

fixed_median_runchart_dataframe <- fixed_median_runchart_dataframe %>% 
  mutate(flag = if_else(measure == "GESTATION AT BOOKING" & period == "M" & # flags special months (FV/TAY)
                          ((hbname == "NHS Forth Valley" &
                              date >= "2021-03-01" & date <= "2022-02-01") |
                             (hbname == "NHS Tayside" &
                                date >= "2020-08-01" & date <= "2021-07-01")),
                        1, 0)
  ) %>% 
  tibble::rowid_to_column()

# CALCULATE NEW_MEDIAN FOR FORTH VALLEY/TAYSIDE FOR SELECTED DATES

add_medians <-
  filter(fixed_median_runchart_dataframe, flag == 1) %>%
  group_by(hbtype, hbname, period) %>% 
  mutate(new_median = median(measure_value, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(rowid, new_median)

# match new_median values onto runchart_dataframe

fixed_median_runchart_dataframe <-
  left_join(fixed_median_runchart_dataframe, add_medians, by = "rowid") %>% 
  select(-c(rowid, flag))

# for Gestation at booking Tayside and Forth Valley:

# calculate new_extended (median) (equals new_median) and fill down - plotted as dotted green line
# reset median to NA where new_median has a value
# reset extended to NA where new_extended has a value
# [reset new_extended to NA where there is a value for new_median - stops overplotting]
# [reset extended to NA where there is a value for median or new_extended - stops overplotting]

fixed_median_runchart_dataframe <- fixed_median_runchart_dataframe %>% 
  group_by(dataset, hbtype, hbname, period, measure, measure_cat) %>% 
  mutate(
    new_extended = new_median,
    new_extended = na.locf(new_extended, na.rm = FALSE),
    median = if_else(!is.na(new_median), NA_real_, median),
    extended = if_else(!is.na(new_extended), NA_real_, extended),
    flag = if_else(!is.na(new_extended), 1, 0))  # testing use of flag to split shifts by median

# save off the median information to add to the download dataset created below

medians <- select(fixed_median_runchart_dataframe,
                  c(dataset:measure_cat, median, extended, new_median, new_extended)) %>% 
  mutate(across(c(median:new_extended), ~ round(., 3)))

### ii - Mark SHIFTS and TRENDS ----

# update median with new_median for "special" cases (GESTATION AT BOOKING)
# update extended (median) - plotted as dotted blue line

fixed_median_runchart_dataframe <- fixed_median_runchart_dataframe %>%
  mutate(median = if_else(!is.na(new_median), new_median, median), # copies new_median into empty median, fills new_extended
         extended = na.locf(median, na.rm = FALSE))

# runchart_flags_shifts <- function(dataset, shift, value, median) {
#   
#   dataset <- dataset %>%
#     mutate(
#       shift_i = tidytable::case_when(
#         ({{value}} > {{median}} & lag({{value}}, 1) > {{median}} &
#            lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} &
#            lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}})
#         | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} &
#              lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} &
#              lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}}) ~ TRUE,
#         TRUE ~ FALSE),
#       
#       shift = tidytable::case_when(
#         shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
#         | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
#         | lead(shift_i, 5) == TRUE  ~ TRUE,
#         TRUE ~ FALSE),
#       ) %>%
#     
#     rename({{shift}}:=shift) %>%
#     select(-shift_i)
# }

# runchart_flags_trends <- function(dataset, trend, value) {
#   
#   dataset <- dataset %>%
#     mutate(
#      trend_i = tidytable::case_when(
#         ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
#          & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4)) |
#           ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
#            & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4)) ~ TRUE,
#         TRUE ~ FALSE),
#       
#       trend = tidytable::case_when(
#         trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
#         | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE ~ TRUE,
#         TRUE ~ FALSE)
#       ) %>%
#     
#     rename({{trend}}:=trend) %>%
#     select(-trend_i)
# }

fixed_median_runchart_dataframe1 <- runchart_flags(
  dataset = fixed_median_runchart_dataframe, # calculates trend over whole dataset, shifts over flag
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  median = extended)

# fixed_median_runchart_dataframe1 <- runchart_flags_shifts(
#   dataset = filter(fixed_median_runchart_dataframe, flag == 0), # doesn't calculate trend over whole dataset
#   shift = "orig_shift",
#   value = measure_value,
#   median = extended)
# 
# # shifts and trends for new_median and new_extended
# 
# fixed_median_runchart_dataframe2 <- runchart_flags_shifts(
#   dataset = filter(fixed_median_runchart_dataframe, flag == 1),
#   shift = "orig_shift2",
#   value = measure_value,
#   median = extended)
# 
# fixed_median_runchart_dataframe3 <- runchart_flags_trends(
#   dataset = fixed_median_runchart_dataframe, # doesn't calculate trend over whole dataset
#   trend = "orig_trend",
#   value = measure_value)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

fixed_median_runchart_dataframe1 <- fixed_median_runchart_dataframe1 %>% 
  mutate(
    #remove_shifts_and_trends = measure %in% c("BOOKINGS", "TERMINATIONS"), # not applicable
    trend = 
      if_else(orig_trend == TRUE, # & 
                #remove_shifts_and_trends == FALSE,
              measure_value, NA),
    shift =
      if_else(orig_shift == TRUE, # &
                #remove_shifts_and_trends == FALSE,
              measure_value, NA)
  )

# split adjacent shifts and trends that should not be connected

fixed_median_runchart_dataframe1 <- add_split_gaps(
  dataset = fixed_median_runchart_dataframe1,
  measure = "trend",
  split_col_prefix = "orig_trend") %>% 
  #select(- c("num_rows", "dup_row"))
  rename(., c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

fixed_median_runchart_dataframe1 <- add_split_gaps(
  dataset = fixed_median_runchart_dataframe1,
  measure = "shift",
  split_col_prefix = "orig_shift") %>% 
  rename(., c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

# reset median and extended (currently contain new_median and new_extended values)

fixed_median_runchart_dataframe1 <- fixed_median_runchart_dataframe1 %>% 
  mutate(median = if_else(!is.na(new_median), NA, median),
         extended = if_else(!is.na(new_extended), NA, extended)) 

# need to add suffix in as this is missing

fixed_median_runchart_dataframe1 <- fixed_median_runchart_dataframe1 %>% 
  mutate(suffix = recode_values(measure,
     c("BOOKINGS", "TERMINATIONS") ~ "",
     c("GESTATION AT BOOKING", "GESTATION AT TERMINATION") ~ " weeks",
     default = "%")
     )

### iii - Tidy up ----

# add quarter_label and round values

fixed_median_runchart_dataframe1 <- fixed_median_runchart_dataframe1 %>%
  ungroup(flag) %>% 
  select(c(dataset:hbname, period:measure_value, median, extended, new_median,
           new_extended, suffix, trend, shift,
           ends_with(c("num_rows", "dup_row")))
         ) %>% 
  mutate(quarter_label = if_else(period == "Q",
                                 qtr(ymd(date), format = "short"),
                                 NA),
         quarter_label = factor(quarter_label,
                                levels = x_date_labels_Q2,
                                ordered = TRUE),
         across(c(measure_value:new_extended, trend, shift), ~ round(., 3)),
         num = if_else(measure %in% c("BOOKINGS", "TERMINATIONS"), NA, num)
  ) %>%
  ungroup()

saveRDS(fixed_median_runchart_dataframe1, paste0(data_path, "/", "runchart_dataframe-2024-02-28.rds"))

# split 15 Dec 24 amended download data into the separate measure data using the function borrowed from dashboard code

builds_download_data <- function(measure) {
  
  downloaddata <- download_dataframe[[{{measure}}]] 
    
  return(downloaddata)
}

bookings <- builds_download_data("BOOKINGS")
av_gestation_booking <- builds_download_data("GESTATION AT BOOKING")
av_gestation_termination <- builds_download_data("GESTATION AT TERMINATION")
terminations <- builds_download_data("TERMINATIONS")
inductions <- builds_download_data("INDUCTIONS")
type_of_birth <- builds_download_data("TYPE OF BIRTH")
tears <- builds_download_data("TEARS")
gestation <- builds_download_data("GESTATION AT BIRTH")
apgar5 <- builds_download_data("APGAR5")

fixed_median_download_dataframe <- bind_rows(bookings,
                                av_gestation_booking,
                                av_gestation_termination,
                                terminations,
                                inductions,
                                type_of_birth,
                                tears,
                                gestation,
                                apgar5) %>% 
  select(c(dataset:measure_value, measure_value_description, suffix, key_measure_label, num, den, num_description, den_description, key_measure_label))

fixed_median_download_dataframe <- left_join(fixed_median_download_dataframe, medians) %>%  # copy median etc to grouped records
 select(c("dataset", "hbtype", "hbname", "period", "date", "measure", 
"measure_cat", "measure_value", "median", "extended", "measure_value_description", "new_median", "new_extended",
"suffix", "key_measure_label", "num", "den", "num_description", 
"den_description"))

fixed_median_download_dataframe <- fixed_median_download_dataframe %>% 
  split(.$measure) 

# remove empty columns

for (i in seq_along(fixed_median_download_dataframe)) {
  
  fixed_median_download_dataframe[[i]] <- 
    janitor::remove_empty(fixed_median_download_dataframe[[i]], which = c("cols"), quiet = TRUE)
}

saveRDS(fixed_median_download_dataframe, paste0(data_path, "/", "download_dataframe-2024-02-28.rds"))

annual_dataframe <- readRDS(paste0(data_path, "/", "annual_dataframe-2024-02-08.rds")) # unchanged
download_dataframe <- readRDS(paste0(data_path, "/", "download_dataframe-2024-02-28.rds")) # new
runchart_dataframe <- readRDS(paste0(data_path, "/", "runchart_dataframe-2024-02-28.rds")) # new

save(annual_dataframe, 
     download_dataframe,
     runchart_dataframe,
     factor_labels_year,
  file = paste0(dashboard_dataframes_folder, "/SMR02-ABC-Terminations-fixed-median-2024-02-28.RData")
)

## - END OF SCRIPT ----