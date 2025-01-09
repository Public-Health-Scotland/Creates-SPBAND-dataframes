###
# Late pre-term and term/post-term admissions to a neonatal unit by BAPM level of care 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from the Maternity Team's SMR02 data file [and the NeoCareIn+ datamart]
# Bev Dodds
# 10 October 2024
# Last update by Bev Dodds
# Latest update description: tidied up code
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on Posit Workbench
# Version of R - 4.1.2
# Reads in SMR02 live births and mocks up locations of care until NeoCareIn+ data becomes available
# Approximate run time - <5 minutes
###

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

### 2 - Initialise variables ----

BAPM_LOC_runchart_categories <- c("intensive care", "high dependency care", "special care")

BAPM_LOC_subgroup_categories <- c("between 34 and 36 weeks (inclusive)", # late pre-term
                                  "between 37 and 42 weeks (inclusive)") # term/post-term

### 3 - Read in source data ----

### 3a - real numerator will come from NeoCare+ ----

# The numerator contains a subset of the number of live born babies admitted to a neonatal unit (first admission only). These babies are categorised by their gestation at admission:

# 34+0 to 36+6 weeks (late pre-term)
# 37+0 to 42+6 weeks (term and post-term)
# and by the highest level of care they receive during this stay in the neonatal unit:

# Intensive care
# High dependency care
# Special care

# will need to select first admission (CHI/Baby ID and date of admission)
# at 34-36 weeks and 37-42 weeks gestation (gestation in weeks)

# Baby CHI or encrypted CHI (check no repeats)
# Date of birth
# Gestation at delivery (completed weeks? or completed weeks plus completed days?)
# Admission date (want first admission date) >= 2017
# Quarter - will be derived as quarter beginning (of admission)
# Gestation at admission - calculated from Gestation at delivery + days between(date of admission, date of delivery)
# Gestation group - calculated as below based on Gestation at admission
# Highest level of care (BAPM spec) - use BAPM (2011) level of care - want only the highest value 1 - INTENSIVE CARE, 2 - HIGH DEPENDENCY CARE, 3 - SPECIAL CARE (IGNORE 4 - NORMAL CARE)

# perform counts on this data (section 5)

### 3b - real denominator will come from SMR02 ----

# All live-born babies (condis = 3, delivered and outcome1 = 1, live birth)
# numbir not necessarily = 1
# gestation at delivery 34-36 weeks and 37-42 weeks

# year in the SMR02 extract is based on date of discharge >= 2018 & condis == 3 & outcome1 == 1 (as below)
# estgest = na_if 99 - this is gestation at delivery 
# Gestation group - calculated as below based on Gestation at discharge? /delivery?
# Quarter - based on quarter beginning (of discharge? delivery?)

# perform counts on this data (section 5)

###

# bind two dataframes together based on Quarter (section 5)
# calculate measure_value (section 5)
# create runcharts etc (section 6)

### Made up data ----

### 3c - SMR02 from .rds version ----
# to create charts for dashboard

# condis = 3 (delivered)
# outcome1 = 1 (livebirth) 

babies_raw <- 
  readRDS(SMR02_filename) %>% 
  filter(year >= 2018 & condis == 3 & outcome1 == 1) %>%   # maternity record live births
  mutate(dataset = "NeoCareIn+",
         hbtype = "Treatment",
         hbname = "Scotland",
         date_of_delivery = as.Date(date_of_delivery),
         quarter_of_delivery = as.Date(as.yearqtr(date_of_delivery)), # quarter beginning
         period = "Q",
         estgest = na_if(estgest, 99),
  ) %>%  
  filter(date_of_delivery >= "2018-01-01" & quarter_of_delivery <= cut_off_date_Qtrly) %>% # don't publish incomplete data
  select(dataset, hbtype, hbname, date_of_delivery, quarter_of_delivery, period, upi, numbir,
         outcome = outcome1, outcome_name = outcome1name, gestation_weeks = estgest)

### 4 - Create new variables ----

# no median for now - not enough quarters from Jul 2022 for post-pandemic median yet

# babies_raw <- babies_raw %>%  
#   mutate(
#     # median_name = case_when(
#     #   quarter <= "2019-10-01" ~ "pre-pandemic median",
#     #   #between(quarter, as.Date("2022-07-01"), as.Date("2024-06-01")) ~ "post-pandemic median",
#     #   .default = NA
#     # ),
#     date = quarter,
#     babies = 1
#   ) %>% 
#   janitor::remove_empty("cols") %>% 
#   select (- quarter)

# flag pertinent gestation periods (estgest has already been recoded
# (18 thru 44 = copy)(else = 99))

babies_raw <- babies_raw %>% 
  mutate(gest_grp = case_when(
    between(gestation_weeks, 34, 36) ~ 1,
    between(gestation_weeks, 37, 42) ~ 2
  ),
  babies = 1) %>% 
  filter(!is.na(gest_grp))

babies_raw$gest_grp <- 
  factor(babies_raw$gest_grp, levels = c(1, 2),
         labels = c("between 34 and 36 weeks (inclusive)", # late pre-term
                    "between 37 and 42 weeks (inclusive)") # term and post-term
  )

# generate a random BAPM level (1-3) to add to the dataset to create a subset (fake NeoCare dataset)

count_rows <- nrow(babies_raw)

BAPM_babies <- babies_raw %>% 
  mutate(BAPM_level_of_care = round(runif(n = count_rows, min = 1, max = 100), 0)
  )

BAPM_babies <- BAPM_babies %>% 
  mutate(BAPM_level_of_care = case_when( # not definitive, would depend on gestation - made up numbers
    between(BAPM_level_of_care, 1, 3) ~ 1, # roughly 3% intensive care
    between(BAPM_level_of_care, 4, 5) ~ 2, # roughly 2% high dependency care
    between(BAPM_level_of_care, 6, 13) ~ 3 # roughly 7% special care 
  )
  )

BAPM_babies <- BAPM_babies %>% 
  filter(!is.na(BAPM_level_of_care))

# generate a random number of days (0-7) to add to the date_of_delivery -> date of admission to neonatal care

count_rows <- nrow(BAPM_babies)

set.seed(1)

BAPM_babies <- BAPM_babies %>% 
  mutate(admission_delay_days = round(runif(n = count_rows, min = 0, max = 7), 0),
         admission_date = date_of_delivery + admission_delay_days) %>% 
  arrange(admission_date) %>% 
  mutate(quarter_of_admission = as.Date(as.yearqtr(admission_date)),
         quarter_of_admission_label = qtr(ymd(admission_date), format = "short"),
         quarter_of_admission_label = factor(quarter_of_admission_label,
                                          levels = unique(quarter_of_admission_label),
                                          ordered = TRUE)
         )

BAPM_babies <- BAPM_babies %>% 
  filter(quarter_of_admission <= cut_off_date_Qtrly) # don't publish incomplete data
  
# aggregate BAPM_babies by quarter, gest_grp and BAPM_level_of_care

admissions_to_neocare <- summarise(BAPM_babies,
                                   .by = c(dataset, hbtype, hbname, quarter_of_admission, period, gest_grp, BAPM_level_of_care),
                                   count = n())

# aggregate BAPM_babies by quarter and gest_grp to get total numbers admitted to neonatal care

all_admissions_to_neocare <- BAPM_babies %>%
  mutate(BAPM_level_of_care = 0) %>% 
  summarise(.,
            .by = c(dataset, hbtype, hbname, quarter_of_admission, period, gest_grp, BAPM_level_of_care),
            count = n())
  
# add the two files together - these are the numerators

admissions_to_neocare <- bind_rows(all_admissions_to_neocare, admissions_to_neocare) %>% 
  arrange(quarter_of_admission, gest_grp, BAPM_level_of_care)


# aggregate babies_raw by quarter and gest_grp to get denominators

live_babies <- summarise(babies_raw,
                        .by = c(dataset, hbtype, hbname, quarter_of_delivery, period, gest_grp),
                        babies = n())

# rename quarter_of_delivery to quarter_of_admission to enable matching

live_babies <- live_babies %>% 
  mutate(quarter_of_admission = quarter_of_delivery,
         quarter_of_delivery = NULL)

# append live_babies in same quarter to calculate percentages

gestation_by_BAPM_LOC <- left_join(admissions_to_neocare, live_babies)

# now add counts of live babies to BAPM_level_of_care for plotly charts in dashbboard

live_babies <- live_babies %>% 
  mutate(BAPM_level_of_care = 4) %>% 
  rename(count = babies)

# rename variables to standard names for download and dashboard code

gestation_by_BAPM_LOC <- bind_rows(gestation_by_BAPM_LOC, live_babies) %>% 
  rename(date = quarter_of_admission,
         subgroup_cat = gest_grp,
         den = babies,
         measure_cat = BAPM_level_of_care,
         num = count) %>% 
  mutate(measure_value = round(percentage(num, den), 3),
         suffix = "%",
         measure = "ADMISSIONS TO NEOCARE BY LEVEL OF CARE"
  ) %>% 
  arrange(date, subgroup_cat, measure_cat)

# set factor levels for measure_cat

gestation_by_BAPM_LOC$measure_cat =
  factor(gestation_by_BAPM_LOC$measure_cat, levels = c(0, 1, 2, 3, 4),
         labels = c("all admissions to a neonatal unit",
                    "intensive care",
                    "high dependency care",
                    "special care",
                    "babies born alive"),
         ordered = TRUE
  )

# set median_name as a factor to keep order

# gestation_by_BAPM_LOC$median_name <- factor(gestation_by_BAPM_LOC$median_name,
#                                             levels = c("pre-pandemic median", "post-pandemic median"),
#                                             labels = c("pre-pandemic median", # to Oct-Dec 2019 / to end Feb 2020
#                                                        "post-pandemic median") # from Jul 2022 to end Jun 2024
# )

saveRDS(gestation_by_BAPM_LOC, paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

gestation_by_BAPM_LOC <- readRDS(paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

# ### 6- Create data frames to be used in SPBAND ----
# 
# ### 6a - Create runchart dataframe ----
# 
# BAPM_LOC_runchart_dataframe <- gestation_by_BAPM_LOC %>% 
#   filter(measure_cat %in% BAPM_LOC_runchart_categories) 
# 
# ### i - MEDIAN of measure_value ----
# 
# # calculate the MEDIAN of the measure_value variable over the relevant median_name - plotted as a solid line
# 
# BAPM_LOC_runchart_dataframe <- calculate_medians(dataset = BAPM_LOC_runchart_dataframe,
#                                                  measure_value = measure_value,
#                                                  subgroup_cat = subgroup_cat)
# 
# ### ii - Mark SHIFTS and TRENDS ----
# 
# # compares measure_value with extended to determine shifts
# # compares consecutive measure_values to determine trends
# 
# BAPM_LOC_runchart_dataframe <- runchart_flags(
#   dataset = BAPM_LOC_runchart_dataframe,
#   shift = "orig_shift",
#   trend = "orig_trend",
#   value = measure_value,
#   median = extended)
# 
# # Set up data for "trend" and "shift" traces
# # We don't want to use this data to plot anything that is not part of a
# # trend or shift, so just set FALSE-flagged data to NA
# 
# BAPM_LOC_runchart_dataframe <- BAPM_LOC_runchart_dataframe %>% 
#   mutate(
#     trend = 
#       if_else(orig_trend == TRUE, 
#               measure_value, NA), # copies measure_value to plot as trend
#     shift =
#       if_else(orig_shift == TRUE,
#               measure_value, NA) # copies measure_value to plot as shift
#   )
# 
# # split adjacent shifts and trends that should not be connected
# 
# BAPM_LOC_runchart_dataframe <- add_split_gaps(
#   dataset = BAPM_LOC_runchart_dataframe,
#   measure = "trend",
#   split_col_prefix = "orig_trend") %>% 
#   rename(c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))
# 
# BAPM_LOC_runchart_dataframe <- add_split_gaps(
#   dataset = BAPM_LOC_runchart_dataframe,
#   measure = "shift",
#   split_col_prefix = "orig_shift") %>% 
#   rename(c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))
# 
# # reset extended values to NA where median values exist (bar last median value)
# 
# BAPM_LOC_runchart_dataframe <- BAPM_LOC_runchart_dataframe %>%
#   group_by(dataset, hbtype, hbname, period, measure, measure_cat, subgroup_cat, median_name) %>% 
#   mutate(extended = if_else(
#     !is.na(median) & !is.na(extended) & is.na(lead(median)),
#     median, NA),
#     extended = na.locf(extended, na.rm = FALSE)
#   )
# 
# # pivot wider to split median and extended into separate columns based on median_name
# 
# BAPM_LOC_runchart_dataframe <- BAPM_LOC_runchart_dataframe %>%
#   mutate(median_name2 = median_name) %>% 
#   pivot_wider(names_from = median_name2,
#               values_from = median,
#               values_fill = NULL,
#               names_sort = TRUE)
# 
# BAPM_LOC_runchart_dataframe <- BAPM_LOC_runchart_dataframe %>% 
#   pivot_wider(names_from = median_name, 
#               values_from = extended,
#               values_fill = NULL,
#               names_prefix = "extended ",
#               names_sort = TRUE)
# 
# BAPM_LOC_runchart_dataframe <- BAPM_LOC_runchart_dataframe %>% 
#   janitor::clean_names()
# 
# # to check whether any duplicate rows have been added to split trends or shifts - don't want these in the download data - wikl be removed in 6 - Create download dataframes.R anyway
# 
# print(max(c(BAPM_LOC_runchart_dataframe$trend_num_rows,
#                             BAPM_LOC_runchart_dataframe$shift_num_rows))
# )
# 
# saveRDS(BAPM_LOC_runchart_dataframe, paste0(data_path, "/BAPM_LOC_runchart_dataframe.rds"))
# 
# BAPM_LOC_runchart_dataframe <- readRDS(paste0(data_path, "/BAPM_LOC_runchart_dataframe.rds"))
# 
# ### 7 - Match runchart data to main dataframe ----
# 
# gestation_by_BAPM_LOC <- left_join(gestation_by_BAPM_LOC, BAPM_LOC_runchart_dataframe,
#                                    by = c("dataset", "hbtype", "hbname", "date", "period", "subgroup_cat", "den", "measure_cat", "num", "measure_value", "suffix", "measure", "subgroup"))

### 8 - Tidy up and save required variables ----

# add on the num, den, measure_value metadata for the data download

gestation_by_BAPM_LOC <-  merge(gestation_by_BAPM_LOC, metadata)

# Add "nicenames"

# formatted_name is the factor which controls the order in which the context charts legends should appear

short_formatted_name <- c("late pre-term", "term and post-term")

long_formatted_name <- c(paste0("late pre-term (34", "<sup>+0</sup>", " to 36", "<sup>+6</sup>", " weeks gestation)"),
                    paste0("term/post-term (37", "<sup>+0</sup>", " to 41", "<sup>+6</sup>", " weeks gestation)")
                    )

nicename <- tibble(BAPM_LOC_subgroup_categories, short_formatted_name, long_formatted_name)

nicename$BAPM_LOC_subgroup_categories <- factor(nicename$BAPM_LOC_subgroup_categories, levels = BAPM_LOC_subgroup_categories)

nicename$short_formatted_name <- factor(nicename$short_formatted_name, levels = short_formatted_name)

nicename$long_formatted_name <- factor(nicename$long_formatted_name, levels = long_formatted_name)

gestation_by_BAPM_LOC <- left_join(gestation_by_BAPM_LOC,
                                   nicename,
                                   by = c("subgroup_cat" = "BAPM_LOC_subgroup_categories")
)

# add date_label and round values, order variables for download dataframe

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  arrange(date) %>%
  mutate(date_label = qtr(ymd(date), format = "short"),
         date_label = factor(date_label, levels = unique(date_label), ordered = TRUE)
         #across(c(measure_value, pre_pandemic_median:extended_pre_pandemic_median, trend, shift), ~ round(., 3))
  ) %>%
  arrange(subgroup_cat, date, measure_cat) %>% 
  ungroup()

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>% 
  select("dataset", "measure", "hbtype", "hbname", "period", "date", "date_label", "subgroup_cat", "measure_cat", "num", "den", "measure_value", "suffix",  "short_formatted_name", "long_formatted_name", "num_description", "den_description", "measure_value_description", "plotted_on_charts", "shown_on_MIO") # "median_name", "pre_pandemic_median",  "extended_pre_pandemic_median", "trend", "shift", "post_pandemic_median", "extended_post_pandemic_median",

saveRDS(gestation_by_BAPM_LOC, paste0(dashboard_dataframes_folder, "/", "gestation-by-BAPM-level-of-care.rds"))

gestation_by_BAPM_LOC <- readRDS(paste0(dashboard_dataframes_folder, "/", "gestation-by-BAPM-level-of-care.rds"))

## - END OF SCRIPT ----