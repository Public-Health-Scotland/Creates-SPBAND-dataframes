###
# Late pre-term and term/post-term admissions to a specialist neonatal unit by highest level of care 
# Want to reduce the avoidable separation of mother and baby
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from the Maternity Team's SMR02 data file and the NeoCareIn+ datamart
# Bev Dodds
# 10 October 2024
# Last updated by Bev Dodds
# Last updated 18 February 2026
# Latest update description: revised code to use real NeoCare data
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on Posit Workbench
# Version of R - 4.4.2
# Reads in SMR02 live births for the denominator and NeoCareIn+ data for the numerator
# Approximate run time - <5 minutes
###

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

# Read in additional package libraries (every time) ----

library(DBI)
library(odbc)

# Open a connection to DVPROD (production environment) ----

denodo_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "DVPROD",
     uid = Sys.getenv("USER"),
    pwd = .rs.askForPassword("Enter your LDAP password"),
    bigint = "integer")
)

### 2 - Initialise grouping variables ----

bapm_level_of_care_runchart_categories <- c("intensive care", "high dependency care", "special care", "normal care")

bapm_level_of_care_subgroup_categories <- c("between 34 and 36 weeks (inclusive)", # late pre-term
                                            "between 37 and 42 weeks (inclusive)") # term/post-term

### 3 - Read in source data ----

### 3a - numerators come from NeoCareIn+ ----

# The numerator contains a subset of the number of live born babies admitted to a neonatal unit (first admission only). These babies are categorised by their gestation at birth:

# 34+0 to 36+6 weeks (late pre-term)
# 37+0 to 42+6 weeks (term and post-term)
# and by the highest level of care they receive during this stay in the neonatal unit:

# Intensive care
# High dependency care
# Special care
# Normal care (could be Special care + parent present)

# will need to select first admission (baby UPI and date of admission)
# where baby was born in Scotland at 34-36 weeks and 37-42 weeks gestation (gestation in weeks)

# Variables required from NeoCareIn+
# Born in Scotland = location_of_delivery_code [char] maps to Scottish NHS Board
# Baby CHI or encrypted CHI (check no repeats) = baby_national_id [char]
# Date of birth = baby_birth_date_time [DateTime] >= 01 Jan 2018 00:00
# Gestation at delivery (completed weeks) [integer]
# First admission date >= 01 Jan 2018 00:00 = date_time_of_admission [DateTime] not more # than 24 hours following birth to include BBA
# Episode = episode_number [integer]
# Discharged from neonatal care (dead or alive) = date_time_of_discharge [DateTime]
# Ward Location = highest_level_of_care [char]

# Quarter - will be derived as quarter beginning (of birth) = date
# Gestation group - derived based on Gestation at birth = gest_grp [derived]
# Highest level of care (BAPM spec) - use BAPM (2011) level of care - want only the highest value 1 - INTENSIVE CARE, 2 - HIGH DEPENDENCY CARE, 3 - SPECIAL CARE, 4 - NORMAL CARE = bapm2011_level_of_care [char] - will need to check over all days in episode

# Exclusions
# Babies born before 1 Jan 2018
# Babies born outwith Scotland
# Babies admitted from home (if they have already been discharged home) i.e. admitted more than 24 hours after birth
# Definitely don't want Ward Location = highest_level_of_care [char] 3, 8; if baby spends all days in > 1 (Neonatal unit) exclude
# Babies with a missing gestation at birth
# Babies with a missing BAPM2011 level of care (even one?)
# Babies with a missing discharge date
# Any subsequent episodes of care after discharge home/foster care
# These may be identified by admission from D201N if not already identified by a discharge home/to foster care

# Investigate how many babies are in Ward Location = 2 their entire stay
# Investigate whether Ward Location (highest_level_of_aare) correlates with bapm2011_level_of_care
# Investigate whether there are missing discharge dates where there are no recent daily records

# Units differ in practice - some babies transferred to transitional care are placed onto 
# BAdgerNet Maternity

# perform counts on this data (section 5)

# identify babies born at 34-36 and 37-42 weeks inclusive with a discharge_date and a valid baby_upi ----

initial_cohort_of_completed_episodes_loc <- as_tibble(dbGetQuery(denodo_connect,
  "SELECT entity_id, neocare_episode_unique_id, episode_number AS bn_episode_number, baby_upi, baby_birth_date_time, gestation_at_delivery_weeks, gestation_at_delivery_days, date_time_of_admission, date_time_of_discharge, location_of_delivery_matneo_key, location_of_delivery_code, location_of_delivery_name, admission_source_code, admission_source_desc, location_admitted_from_matneo_key, location_admitted_from_code, location_admitted_from_name, location_of_treatment_matneo_key, location_of_treatment_code, location_of_treatment_name, discharge_destination_code, discharge_destination_desc, location_discharged_to_matneo_key, location_discharged_to_code, location_discharged_to_name, discharge_destination_wardtype_code, discharge_destination_wardtype_desc
  
    FROM matneo.matneo_neocare_episode a
      LEFT OUTER JOIN matneo.matneo_deletions b
        ON a.entity_id = b.unique_identifier
          WHERE baby_birth_date >= '2018-01-01' AND date_of_discharge IS NOT NULL AND baby_upi IS NOT NULL AND (gestation_at_delivery_weeks BETWEEN '34' AND '36' OR gestation_at_delivery_weeks BETWEEN '37' AND '42')  AND dataset IS NULL
            ORDER BY baby_upi, date_time_of_admission, date_time_of_discharge
                       "))

nrow(initial_cohort_of_completed_episodes_loc)
length(unique(initial_cohort_of_completed_episodes_loc$baby_upi))

# add (new) episode_number, number_of_episodes, last episode flag 

initial_cohort_of_completed_episodes_loc <- 
  add_episode_vars(initial_cohort_of_completed_episodes_loc)

table(initial_cohort_of_completed_episodes_loc$episode_number)

# calculate delay between birth and admission to the first episode and the delay between episodes of care

initial_cohort_of_completed_episodes_loc <- 
  add_delay_vars(initial_cohort_of_completed_episodes_loc)

print(range(initial_cohort_of_completed_episodes_loc$delay_between_birth_and_admission, na.rm = TRUE))

print(range(initial_cohort_of_completed_episodes_loc$delay_between_episodes, na.rm = TRUE))

# reorder variables and drop bn_episode_number (prefer derived episode_number)

initial_cohort_of_completed_episodes_loc <- 
  initial_cohort_of_completed_episodes_loc %>% 
  select(neocare_episode_unique_id, episode_number, number_of_episodes, last_episode, baby_upi, baby_birth_date_time, delay_between_birth_and_admission, delay_between_episodes, date_time_of_admission, date_time_of_discharge, gestation_at_delivery_weeks, gestation_at_delivery_days, location_of_delivery_matneo_key, location_of_delivery_code, location_of_delivery_name, admission_source_code, admission_source_desc, location_admitted_from_matneo_key, location_admitted_from_code, location_admitted_from_name,  location_of_treatment_matneo_key, location_of_treatment_code, location_of_treatment_name, discharge_destination_code, discharge_destination_desc, location_discharged_to_matneo_key, location_discharged_to_code, location_discharged_to_name, discharge_destination_wardtype_code, 
         discharge_destination_wardtype_desc
  )

saveRDS(initial_cohort_of_completed_episodes_loc, paste0(data_path, "/", "initial_cohort_of_completed_episodes_loc.rds"))

# make a copy of the initial cohort ----

initial_cohort_of_completed_episodes_loc <- readRDS(paste0(data_path, "/", "initial_cohort_of_completed_episodes_loc.rds"))

completed_episodes_loc <- initial_cohort_of_completed_episodes_loc

# check for missing location codes in delivery, admitted from and treatment columns ----

completed_episodes_loc <- check_location_codes(completed_episodes_loc)

# check location_of_delivery is consistent ----

inconsistent_locations <- check_location_of_delivery(completed_episodes_loc)

# set inconsistent location_of_delivery to the first values (if not D299N)

completed_episodes_loc <- fix_location_of_delivery(completed_episodes_loc)

# recheck location_of_delivery is consistent ----

inconsistent_locations <- check_location_of_delivery(completed_episodes_loc)

# tidy up

rm(inconsistent_locations)

saveRDS(completed_episodes_loc, paste0(data_path, "/", "completed_episodes_loc.rds"))

# identify babies born in, admitted from a location in and discharged to a location in Scotland ---- 
# remove them from the cohort

completed_episodes_loc <- readRDS(paste0(data_path, "/", "completed_episodes_loc.rds"))

completed_episodes_loc <- identify_scottish_babies(completed_episodes_loc)

nrow(completed_episodes_loc)
length(unique(completed_episodes_loc$baby_upi))

table(completed_episodes_loc$episode_number)

saveRDS(completed_episodes_loc, paste0(data_path, "/", "completed_episodes_loc.rds"))

# flag episodes admitted from home or discharged to home/foster care ----

completed_episodes_loc <- readRDS(paste0(data_path, "/", "completed_episodes_loc.rds"))

completed_episodes_loc <-  completed_episodes_loc %>% 
  mutate(admitted_from_home = if_else(location_admitted_from_code == "D201N", episode_number, NA),
         discharged_home = if_else(discharge_destination_code %in% c(1, 4), episode_number, NA)
  )

table(completed_episodes_loc$admitted_from_home)
table(completed_episodes_loc$discharged_home)

# exclude babies admitted from home [to their first episode of care] more than 24 hours after birth ----

babies_admitted_from_home_over_24_hours_after_birth <- 
  filter(completed_episodes_loc, 
         admitted_from_home == 1 & delay_between_birth_and_admission > 24) %>% 
  select(baby_upi) %>% 
  distinct()

# remove these babies from the cohort

completed_episodes_loc <- 
  anti_join(completed_episodes_loc, babies_admitted_from_home_over_24_hours_after_birth)

nrow(completed_episodes_loc)
length(unique(completed_episodes_loc$baby_upi))

table(completed_episodes_loc$episode_number)

table(completed_episodes_loc$admitted_from_home)

table(completed_episodes_loc$discharged_home)

saveRDS(completed_episodes_loc, paste0(data_path, "/", "completed_episodes_loc.rds"))

rm(babies_admitted_from_home_over_24_hours_after_birth)

# identify first episode number where a baby was sent HOME or to FOSTER CARE ----

completed_episodes_loc <- readRDS(paste0(data_path, "/", "completed_episodes_loc.rds"))

babies_sent_home_foster_care <- identify_babies_sent_home_foster_care(completed_episodes_loc)

# if a baby has been discharged to home or foster care then subsequent episodes should be removed as the end of the first spell of care has been identified

completed_episodes_loc <- 
  left_join(completed_episodes_loc, babies_sent_home_foster_care) %>% 
  filter(is.na(first_discharged_home) | episode_number <= first_discharged_home)

nrow(completed_episodes_loc)
length(unique(completed_episodes_loc$baby_upi))

table(completed_episodes_loc$episode_number)

table(completed_episodes_loc$admitted_from_home)

table(completed_episodes_loc$discharged_home)

saveRDS(completed_episodes_loc, paste0(data_path, "/", "completed_episodes_loc.rds"))

rm(babies_sent_home_foster_care)

# remove subsequent episodes where a baby was admitted from HOME (D201N) ----
# if a baby has been admitted from home in the second (or higher) episode, these episodes (and subsequent episodes) should be removed as the baby has left specialist neonatal care for a period of time (possibly via POSTNATAL WARD or PICU)
# a few babies were admitted from home in the first and subsequent episodes so the later episodes need to be removed, hence the check for the same baby

completed_episodes_loc <- readRDS(paste0(data_path, "/", "completed_episodes_loc.rds"))

babies_admitted_from_home_subsequent_episode <- 
  select(completed_episodes_loc, c(baby_upi, admitted_from_home)) %>% 
  filter(!is.na(admitted_from_home)) %>% 
  group_by(baby_upi) %>% 
  mutate(same_baby = baby_upi == lag(baby_upi),
         first_admitted_from_home = min(admitted_from_home)
         ) %>%
  select(baby_upi, same_baby, first_admitted_from_home) %>% 
  distinct() 

table(babies_admitted_from_home_subsequent_episode$first_admitted_from_home)

babies_admitted_from_home_subsequent_episode <- 
  babies_admitted_from_home_subsequent_episode %>% 
  filter(same_baby == TRUE | first_admitted_from_home > 1) # first episodes will always be kept

table(babies_admitted_from_home_subsequent_episode$first_admitted_from_home)

# remove the subsequent episodes from the cohort
# keep all records where the baby wasn't admitted from home in any episode
# keep all first episodes regardless of where they were admitted from
# keep all episodes before the episode where the baby was admitted from home
# i.e. where episode_number < first_admitted_from_home

completed_episodes_loc <- 
  left_join(completed_episodes_loc, babies_admitted_from_home_subsequent_episode) %>% 
  filter(is.na(first_admitted_from_home) | episode_number == 1 | episode_number < first_admitted_from_home)

nrow(completed_episodes_loc)
length(unique(completed_episodes_loc$baby_upi))

table(completed_episodes_loc$episode_number)

table(completed_episodes_loc$admitted_from_home)

table(completed_episodes_loc$discharged_home)

# recalculate number_of_episodes and last_episode

completed_episodes_loc <- add_episode_vars(completed_episodes_loc)

table(completed_episodes_loc$number_of_episodes)

completed_episodes_loc <- completed_episodes_loc %>% 
  select(- same_baby)

saveRDS(completed_episodes_loc, paste0(data_path, "/", "completed_episodes_loc.rds"))

rm(babies_admitted_from_home_subsequent_episode)

# looking at discharge_destination_ward_type for babies with single episodes and multiple episodes ----

completed_episodes_loc <- readRDS(paste0(data_path, "/", "completed_episodes_loc.rds"))

# read in the day records ----

daily_data <- as_tibble(dbGetQuery(denodo_connect,
                                   "SELECT baby_upi, neocare_episode_unique_id, neocare_day_unique_id, date_of_care, bapm2011_level_of_care_code, bapm2011_level_of_care_desc, highest_level_of_care_code, highest_level_of_care_desc
  FROM matneo.matneo_neocare_day
    ORDER BY baby_upi, neocare_episode_unique_id, date_of_care"
))

# looking at highest_level_of_care (aka Ward Location)
# 1	NEONATAL UNIT, 2 TRANSITIONAL CARE, 3	POSTNATAL WARD, 8	OTHER OBSTETRIC AREA

# and bapm2011_level_of_care_code
# 1	INTENSIVE CARE, 2	HIGH DEPENDENCY CARE, 3	SPECIAL CARE, 4	NORMAL CARE

# match on the BAPM level of care and highest level of care fields ----
# to main dataset using baby_upi and neocare_episode_unique_id

completed_episodes_loc_with_daily_data <- left_join(completed_episodes_loc, daily_data) 
length(unique(completed_episodes_loc_with_daily_data$neocare_episode_unique_id))
length(unique(completed_episodes_loc_with_daily_data$baby_upi))

saveRDS(completed_episodes_loc_with_daily_data, paste0(data_path, "/", "completed_episodes_loc_with_daily_data"))

# group by baby_upi to determine minimum highest level of care codes ----
# remove babies with a missing minimum bapm2011_level_of_care_code

completed_episodes_loc_with_daily_data <- readRDS(paste0(data_path, "/", "completed_episodes_loc_with_daily_data"))

highest_level_of_care <- completed_episodes_loc_with_daily_data %>%
  group_by(baby_upi) %>%
  summarise(min_highest_level_of_care_code = min(highest_level_of_care_code, na.rm = TRUE),
            min_bapm2011_level_of_care_code = min(bapm2011_level_of_care_code, na.rm = TRUE),
            days_in_care = n()
  ) %>% 
  filter(!is.na(min_bapm2011_level_of_care_code))

# match min highest levels of care onto main cohort ----

completed_episodes_loc_with_daily_data <-
  left_join(completed_episodes_loc_with_daily_data, highest_level_of_care)

summary_highest_level_of_care <- completed_episodes_loc_with_daily_data %>% 
  select(baby_upi, number_of_episodes, min_highest_level_of_care_code, min_bapm2011_level_of_care_code) %>% 
  distinct() %>% 
  group_by(min_bapm2011_level_of_care_code, min_highest_level_of_care_code) %>% 
  summarise(count = n())

# remove the babies with missing bapm2011_level_of_care

completed_episodes_loc_with_daily_data <- 
  completed_episodes_loc_with_daily_data %>% 
  filter(!is.na(min_bapm2011_level_of_care_code))

length(unique(completed_episodes_loc_with_daily_data$neocare_episode_unique_id))
length(unique(completed_episodes_loc_with_daily_data$baby_upi))

# remove babies who ONLY spent time in TC or lower levels of care (2, 3, 8) ----

completed_episodes_loc_with_daily_data <- completed_episodes_loc_with_daily_data %>% 
  filter(min_highest_level_of_care_code == 1)

length(unique(completed_episodes_loc_with_daily_data$neocare_episode_unique_id))
length(unique(completed_episodes_loc_with_daily_data$baby_upi))

# reduce file down to babies for use in SPBAND ----

BAPM_babies <- completed_episodes_loc_with_daily_data %>% 
  group_by(baby_upi) %>%
  slice(1) %>% 
  ungroup() %>% 
  select(baby_birth_date_time, gestation_at_delivery_weeks,min_bapm2011_level_of_care_code) 

# add gestation groups ----

BAPM_babies <- BAPM_babies %>% 
  mutate(gest_grp = case_when(
    between(gestation_at_delivery_weeks, 34, 36) ~ 1,
    between(gestation_at_delivery_weeks, 37, 42) ~ 2),
    quarter_of_birth = as.Date(as.yearqtr(baby_birth_date_time)), # quarter beginning
    dataset = "NeoCareIn+"
  )

# aggregate BAPM_babies by quarter, gest_grp and BAPM_level_of_care

admissions_to_neocare <- summarise(BAPM_babies,
                                   .by = c(dataset, quarter_of_birth, gest_grp, min_bapm2011_level_of_care_code),
                                   bapm_count = n())

# aggregate BAPM_babies by quarter and gest_grp to get total numbers admitted to neonatal care

all_admissions_to_neocare <- BAPM_babies %>%
  mutate(min_bapm2011_level_of_care_code = '0') %>% 
  summarise(.,
            .by = c(dataset, quarter_of_birth, gest_grp, min_bapm2011_level_of_care_code),
            bapm_count = n())
  
# add the two files together - these are the numerators

admissions_to_neocare <- bind_rows(all_admissions_to_neocare, admissions_to_neocare) %>% 
  arrange(quarter_of_birth, gest_grp, min_bapm2011_level_of_care_code)

saveRDS(admissions_to_neocare, paste0(data_path, "/", "admissions_to_neocare.rds"))

### 3b - real denominator will come from SMR02 ----

# All live-born babies (condis = 3, delivered and outcome1 = 1, live birth)
# numbir not necessarily = 1
# gestation at delivery 34-36 weeks and 37-42 weeks

# date_of_delivery >= 01 Jan 2018 & condis == 3 & outcome1 == 1 (as below)
# estgest = na_if 99 - this is gestation at delivery 
# Gestation group - derived based on Gestation at delivery
# Quarter - based on quarter beginning (of delivery)

# perform counts on this data (section 5)

###

# bind two dataframes together based on Quarter (section 5)
# calculate measure_value (section 5)
# create runcharts etc (section 6)

### 3c - SMR02 from .rds version ----
# to create charts for dashboard

# condis = 3 (delivered)
# outcome1 = 1 (livebirth) 

SMR02_babies <- 
  readRDS(SMR02_filename) %>% 
  filter(date_of_delivery >= "2018-01-01" & condis == 3 & outcome1 == 1 & between(estgest, 34, 42)) %>%   # maternity record live births
  mutate(dataset = "SMR02",  
         date_of_birth = as.Date(date_of_delivery),
         quarter_of_birth = as.Date(as.yearqtr(date_of_birth)) # quarter beginning
) %>% 
  select(dataset, date_of_birth, quarter_of_birth, gestation_weeks = estgest)

# add gestation groups ----

SMR02_babies <- SMR02_babies %>% 
  mutate(gest_grp = case_when(
    between(gestation_weeks, 34, 36) ~ 1,
    between(gestation_weeks, 37, 42) ~ 2,
    .default = 3
  )
  )

saveRDS(SMR02_babies, paste0(data_path, "/", "SMR02_babies.rds"))

# aggregate babies_raw by quarter and gest_grp to get denominators

live_babies <- summarise(SMR02_babies,
                        .by = c(dataset, quarter_of_birth, gest_grp),
                        live_babies = n())

# append live_babies in same quarter to calculate percentages

admissions_to_neocare <- readRDS(paste0(data_path, "/", "admissions_to_neocare.rds"))

admissions_to_neocare <- left_join(admissions_to_neocare, select(live_babies, - dataset)
)

# now add total counts of live babies to min_bapm2011_level_of_care_code = 5 for plotly charts in dashboard

live_babies <- live_babies %>%
  mutate(min_bapm2011_level_of_care_code = '5',
         dataset = "SMR02"
  ) %>% 
  rename(bapm_count = live_babies) %>% 
  distinct()

# append total live births
# remove incomplete data
# add new and rename existing variables to standard names for download and dashboard code

gestation_by_BAPM_LOC <- 
  bind_rows(admissions_to_neocare, live_babies) %>% 
  filter(quarter_of_birth <= cut_off_date_Qtrly) %>% # don't publish incomplete data
  rename(date = quarter_of_birth,
         subgroup_cat = gest_grp,
         den = live_babies,
         measure_cat = min_bapm2011_level_of_care_code,
         num = bapm_count) %>% 
  mutate(hbtype = "Treatment",
         hbname = "Scotland",
         period = "Q",
         measure_value = round(percentage(num, den), 3),
         suffix = "%",
         measure = "ADMISSIONS TO NEOCARE BY LEVEL OF CARE"
  ) %>% 
  arrange(date) %>%
  select(dataset, measure, hbtype, hbname, period, date, subgroup_cat, measure_cat, num, den, measure_value, suffix) %>% 
  arrange(date, subgroup_cat, measure_cat)

# move babies born alive to denominator and measure_value (temporarily, to allow mwdian to be calculated)

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>% 
  mutate(den = if_else(measure_cat == '5', num, den),
         num = if_else(measure_cat == '5', NA, num),
         measure_value = if_else(measure_cat == '5', den, measure_value)
         )

# add post-pandemic median date range

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>% 
  mutate(
    median_name = if_else(between(date, as.Date("2022-07-01"), as.Date("2025-04-01")), "post-pandemic median", NA)
      )

saveRDS(gestation_by_BAPM_LOC, paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

gestation_by_BAPM_LOC <- readRDS(paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

### 4 - Create runchart data frame to be used in SPBAND ----

# gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
#   filter(measure_cat %in% bapm_level_of_care_runchart_categories)

### MEDIAN of measure_value ----

# calculate the MEDIAN of the measure_value variable over the relevant median_name - plotted as a solid line

gestation_by_BAPM_LOC <- calculate_medians(dataset = gestation_by_BAPM_LOC,
                                           measure_value = measure_value,
                                           subgroup_cat = subgroup_cat)

# reset extended values to NA where median values exist (bar last median value)

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  group_by(dataset, hbtype, hbname, period, measure, measure_cat, subgroup_cat, median_name) %>%
  mutate(extended = if_else(
    !is.na(median) & !is.na(extended) & is.na(lead(median)),
    median, NA),
    extended = na.locf(extended, na.rm = FALSE)
  )

# pivot wider to split median and extended into separate columns based on median_name

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  mutate(median_name2 = median_name) %>%
  pivot_wider(names_from = median_name2,
              values_from = median,
              values_fill = NULL,
              names_sort = TRUE)

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  pivot_wider(names_from = median_name,
              values_from = extended,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  janitor::clean_names() %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE) 

# reset measure_value for babies born alive

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>%
  mutate(measure_value = if_else(measure_cat == '5', NA, measure_value)
         )

# set factor levels for measure_cat

gestation_by_BAPM_LOC$measure_cat =
  factor(gestation_by_BAPM_LOC$measure_cat, levels = c(0, 1, 2, 3, 4, 5),
         labels = c("all admissions to a neonatal unit",
                    "intensive care",
                    "high dependency care",
                    "special care",
                    "normal care",
                    "babies born alive"),
         ordered = TRUE
  )

# set factor levels for subgroup_cat

gestation_by_BAPM_LOC$subgroup_cat =
  factor(gestation_by_BAPM_LOC$subgroup_cat, levels = c(1, 2),
         labels = c("between 34 and 36 weeks (inclusive)", # late pre-term
                    "between 37 and 42 weeks (inclusive)"), # term/post-term
         ordered = TRUE
  )

saveRDS(gestation_by_BAPM_LOC, paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

gestation_by_BAPM_LOC <- readRDS(paste0(data_path, "/", "gestation_by_BAPM_LOC.rds"))

### 5 - Tidy up and save required variables ----

# add on the num, den, measure_value metadata for the data download

gestation_by_BAPM_LOC <- merge(gestation_by_BAPM_LOC, metadata)

# Add "nicenames"

# formatted_name is the factor which controls the order in which the context charts legends should appear

short_formatted_name <- c("late pre-term", "term and post-term")

long_formatted_name <- c(paste0("late pre-term (34", "<sup>+0</sup>", " to 36", "<sup>+6</sup>", " weeks gestation)"),
                    paste0("term/post-term (37", "<sup>+0</sup>", " to 41", "<sup>+6</sup>", " weeks gestation)")
                    )

nicename <- tibble(bapm_level_of_care_subgroup_categories, short_formatted_name, long_formatted_name)

nicename$bapm_level_of_care_subgroup_categories <- factor(nicename$bapm_level_of_care_subgroup_categories, levels = bapm_level_of_care_subgroup_categories, ordered = TRUE)

nicename$short_formatted_name <- factor(nicename$short_formatted_name, levels = short_formatted_name, ordered = TRUE)

nicename$long_formatted_name <- factor(nicename$long_formatted_name, levels = long_formatted_name, ordered = TRUE)

gestation_by_BAPM_LOC <- left_join(gestation_by_BAPM_LOC,
                                   nicename,
                                   by = c("subgroup_cat" = "bapm_level_of_care_subgroup_categories")
)

# add date_label and round values, order variables for download dataframe

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>% 
  arrange(date) %>%
  mutate(date_label = qtr(ymd(date), format = "short"),
         date_label = factor(date_label, levels = unique(date_label), ordered = TRUE),
         across(c(measure_value, post_pandemic_median:extended_post_pandemic_median), ~ round(., 3))
  ) %>%
  arrange(date, subgroup_cat, measure_cat) %>% 
  ungroup()

gestation_by_BAPM_LOC <- gestation_by_BAPM_LOC %>% 
  select("dataset", "measure", "hbtype", "hbname", "period", "date", "date_label", "subgroup_cat", "measure_cat", "num", "den", "measure_value", "suffix",  "short_formatted_name", "long_formatted_name", "num_description", "den_description", "measure_value_description", "plotted_on_charts", "post_pandemic_median", "extended_post_pandemic_median", "shown_on_MIO")

saveRDS(gestation_by_BAPM_LOC, paste0(dashboard_dataframes_folder, "/", "gestation-by-BAPM-level-of-care.rds"))

# write.csv(gestation_by_BAPM_LOC, paste0(data_path, "/", "gestation_by_BAPM_LOC.csv"), row.names = FALSE)

## - END OF SCRIPT ----