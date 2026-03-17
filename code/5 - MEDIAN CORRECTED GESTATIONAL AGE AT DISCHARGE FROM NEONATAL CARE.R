###
# Median corrected gestational age at discharge from specialist neonatal care 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from NeoCareIn+ data views on the matneo data platform
# Bev Dodds
# 20 November 2024
# Last updated by Bev Dodds
# Last updated 14 November 2025
# Latest update description: updated code with real working code
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on Posit Workbench
# Version of R - 4.4.2
# Reads in NeoCareIn+ episodes and uses gestation at birth and LOS to calculate gestational age at discharge from the first spell of specialist neonatal care
# Approximate run time - <5 minutes
###

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

# install.packages("DBI")
# install.packages("odbc")

library(DBI)
library(odbc)

# Open a connection to the Denodo production environment

denodo_connect <- dbConnect(
    odbc(),
    dsn = "DVPROD",
    uid = Sys.getenv("USER"),
    pwd = .rs.askForPassword("Enter your LDAP password"),
    bigint = "integer"
    )

# using matneo.location view to match on scottish_health_board_curr

location_mapping <- as_tibble(dbGetQuery(denodo_connect,
                                         "SELECT location_code, health_board_code_9_curr, health_board_name_curr, location_type, scottish_health_board_curr
  FROM matneo.location"
))

### 2 - Initialise variables ----

### 3 - Read in source data ----

# identify babies born at 30-32 weeks' gestation inclusive with a discharge_date and a valid baby_upi
# note that the BadgerNet episode numbers for a baby may not always be in order so this is ignored and a new field called episode_number is added based on the date_time_of_admission and date_time_of_discharge for each episode for the baby.
# the count of the number_of_episodes for each baby is also derived, as is the delay between the discharge from one episode and the admission for the next episode for each baby.
# This code also checks for deleted records and removes them from the cohort.

initial_cohort_30_32_weeks <- as_tibble(dbGetQuery(denodo_connect,
                                       "SELECT
neocare_episode_unique_id, episode_id AS bn_episode_id, baby_upi, baby_birth_date_time, gestation_at_delivery_weeks, gestation_at_delivery_days, date_of_estimated_delivery, date_time_of_admission, date_time_of_discharge, baby_death_date, location_of_delivery_matneo_key, location_of_delivery_code, location_of_delivery_name, admission_category_code, admission_category_desc, admission_reason_code, admission_reason_desc, admission_source_code, admission_source_desc, admission_type_code, admission_type_desc, location_admitted_from_matneo_key, location_admitted_from_code, location_admitted_from_name, location_of_treatment_matneo_key, location_of_treatment_code, location_of_treatment_name, location_discharged_to_matneo_key, location_discharged_to_code, location_discharged_to_name, discharge_destination_code, discharge_destination_desc, discharge_destination_wardtype_code, discharge_destination_wardtype_desc, nhs_board_of_treatment_code_at_event, nhs_board_of_treatment_name_at_event

    FROM matneo.matneo_neocare_episode a
      LEFT OUTER JOIN matneo.matneo_deletions b
        ON a.entity_id = b.unique_identifier
          WHERE date_of_discharge IS NOT NULL AND baby_upi IS NOT NULL AND gestation_at_delivery_weeks BETWEEN '30' AND '32' AND dataset IS NULL
            ORDER BY baby_upi, date_time_of_admission, date_time_of_discharge
                       "))

nrow(initial_cohort_30_32_weeks) # count of completed episodes
length(unique(initial_cohort_30_32_weeks$baby_upi)) # count of individual babies

# add (new) episode_number, number_of_episodes, delay_between_episodes

initial_cohort_30_32_weeks <- initial_cohort_30_32_weeks %>% 
  group_by(baby_upi) %>% 
  mutate(episode_number =  row_number(),
         number_of_episodes = n(),
         delay_between_episodes = 
           trunc(difftime(date_time_of_admission, lag(date_time_of_discharge, 1), units = 'hours'))
  ) %>% 
  ungroup() %>% 
  select(neocare_episode_unique_id, bn_episode_id, episode_number, number_of_episodes, baby_upi:date_time_of_discharge, delay_between_episodes, baby_death_date:nhs_board_of_treatment_name_at_event)
  
initial_cohort_30_32_weeks %>% count(number_of_episodes)

saveRDS(initial_cohort_30_32_weeks, paste0(data_path, "/", "initial_cohort_30_32_weeks.rds"))

### 4 - Tidy up the dataset ----

# make a copy of the initial cohort ----

completed_episodes_30_32_weeks <- initial_cohort_30_32_weeks

# tidy up missing values in the "location" fields ----

# check for missing location codes in delivery, admitted from and treatment columns

location_code_vars <- 
  grep("^location.*code$", names(completed_episodes_30_32_weeks))[1:3]

location_code_vars <- names(completed_episodes_30_32_weeks[location_code_vars])

missing_locations <- completed_episodes_30_32_weeks %>%
  filter(
    is.na(.data[[location_code_vars[[1]]]]) |
      is.na(.data[[location_code_vars[[2]]]]) |
        is.na(.data[[location_code_vars[[3]]]])
  )

nrow(missing_locations) # number of episodes with a missing location_of

# replace missing locations with D299N

if(nrow(missing_locations > 0)) {
  
  completed_episodes_30_32_weeks <- 
    completed_episodes_30_32_weeks %>% 
    mutate(across(all_of(location_code_vars), ~ replace(., is.na(.), "D299N"))
    )
  
  missing_locations <- completed_episodes_30_32_weeks %>%
    filter(
      is.na(.data[[location_code_vars[[1]]]]) |
        is.na(.data[[location_code_vars[[2]]]]) |
        is.na(.data[[location_code_vars[[3]]]])
    )
  
  nrow(missing_locations) # should be 0
}

rm(missing_locations) # tidy up

# check location_of_delivery is consistent ----

# do all episodes in the spell have the same location_of_delivery? (They should, but some don't)

check_location_of_delivery <- filter(completed_episodes_30_32_weeks, number_of_episodes > 1) %>%
  group_by(baby_upi) %>% 
  reframe(same_location = baby_upi == lag(baby_upi) & location_of_delivery_code == lag(location_of_delivery_code)
         ) %>% 
  filter(same_location == FALSE) %>% 
  distinct()

nrow(check_location_of_delivery) # if not 0 then some babies have conflicting location_of_delivery

# select first location_of_delivery for each baby_upi (unless it is D299N) ----

if(nrow(check_location_of_delivery) != 0) {
  first_location_of_delivery <-  
    left_join(completed_episodes_30_32_weeks, check_location_of_delivery) %>% 
    filter(same_location == FALSE) %>% 
    group_by(baby_upi) %>% 
    select(baby_upi, contains("location_of_delivery"))
  
  condition <- function(x) x !="D299N" 
  
# apply the condition within each group and find the first value that meets the condition
  
  first_location_of_delivery <- first_location_of_delivery %>%
    group_by(baby_upi) %>%
    filter(condition(location_of_delivery_code)) %>%
    slice(1)
  
# append _new to the field names to identify them
  
  names(first_location_of_delivery)[2:4] <- paste0(names(first_location_of_delivery)[2:4], "_new")
  
# update location_of_delivery in the main dataset ----
  
  completed_episodes_30_32_weeks <- 
    left_join(completed_episodes_30_32_weeks, first_location_of_delivery) %>% 
    mutate(location_of_delivery_matneo_key = 
             if_else(!is.na(location_of_delivery_matneo_key_new), 
                     location_of_delivery_matneo_key_new,
                     location_of_delivery_matneo_key),
           location_of_delivery_code = 
             if_else(!is.na(location_of_delivery_code_new), 
                     location_of_delivery_code_new,
                     location_of_delivery_code),
           location_of_delivery_name = 
             if_else(!is.na(location_of_delivery_name_new), 
                     location_of_delivery_name_new,
                     location_of_delivery_name),
           
    ) %>% 
    select(- contains("_new"))
  
# check location of delivery again
  
  check_location_of_delivery <- completed_episodes_30_32_weeks %>%
    group_by(baby_upi) %>% 
    reframe(same_location = baby_upi == lag(baby_upi) & location_of_delivery_code == lag(location_of_delivery_code)
    ) %>% 
    filter(same_location == FALSE) %>% 
    distinct()
  
  nrow(check_location_of_delivery) # should now be 0
}

rm(check_location_of_delivery) # tidy up#

saveRDS(completed_episodes_30_32_weeks, paste0(data_path, "/", "completed_episodes_30_32_weeks.rds"))

### 5 - Refine cohort ----

# The cohort contains a subset of the number of babies born alive at 30-32 weeks gestation who were admitted to a specialist care neonatal unit. A baby may have had a stay in multiple neonatal units. This analysis looks for the first discharge to Home or to Foster Care. The LOS in specialist neonatal care is added to their birth gestation to calculate their [corrected] gestational age at this first discharge. The cohort is selected in this order:

# with the initial cohort which contains babies born at 30+0 to 32+6 weeks' gestation
# identify episode_number(s) where a baby was sent HOME or to FOSTER CARE i.e. (discharge_destination_code %in% c('1', '4'))
# look for repeat babies and identify the first spell of care (i.e. remove subsequent spells)
# identify and remove babies who had major surgery (in any spell)
# identify babies born in Scotland (assumes D201N means born at a home in Scotland), remove ones that weren't
# remove babies who were admitted or discharged from/to outwith Scotland
# identify highest level of care to exclude babies that received transitional care only
# remove spells where there is a delay between a discharge and the subsequent admission of more than 2 days; the baby may have been transferred out of specialist neonatal care and then back in before being sent home

# Variables required:

# from the Episode view

# neocare_episode_unique_id
# baby_upi
# baby_date_of_birth
# date_time_of_admission
# date_time_of_discharge
# location_of_delivery_code
# location_of_treatment_code
# location_admitted_from_code
# location_discharged_to_code
# discharge_destination_code
# gestation_at_delivery_weeks
# gestation_at_delivery_days

# from the Day view

# neocare_episode_unique_id
# baby_upi
# major_surgery
# highest_level_of_care_code

# from the Location view

# location_code
# scottish_health_board_curr

# Derived variables:

# episode_number - Badgernet episode numbers are not always in the correct order so these are derived by sorting the baby records by datetime of admission and datetime of discharge

# delay_between_episodes - time between discharge from one episode and admission of the subsequent episode for a baby

completed_episodes_30_32_weeks <- readRDS(paste0(data_path, "/", "completed_episodes_30_32_weeks.rds"))

# identify episodes where a baby was sent HOME or to FOSTER CARE ----

episodes_sent_home_foster_care <- completed_episodes_30_32_weeks %>% 
  filter(discharge_destination_code %in% c('1', '4')) %>% 
  select(episode_number, baby_upi, discharge_destination_code, discharge_destination_desc) %>%
  mutate(sent_home_foster_care = TRUE)

nrow(episodes_sent_home_foster_care) # number of episodes

length(unique(episodes_sent_home_foster_care$baby_upi)) # number of babies

episodes_sent_home_foster_care %>% count(discharge_destination_desc)

# look for repeat babies - only want the first spell of care then home ----
# look for the episode_number where baby was sent HOME or to FOSTER CARE

episodes_sent_home_foster_care <- episodes_sent_home_foster_care %>% 
  arrange(baby_upi, episode_number) %>% 
  group_by(baby_upi) %>% 
  mutate(repeat_baby = baby_upi == lag(baby_upi)) %>% 
  filter(is.na(repeat_baby)) %>% 
  mutate(sent_home_foster_care = episode_number) %>% 
  select(baby_upi, sent_home_foster_care)

nrow(episodes_sent_home_foster_care) # number of episodes

length(unique(episodes_sent_home_foster_care$baby_upi)) # number of babies (should match)

# match on the first episode number that baby went home ----

completed_episodes_30_32_weeks <- left_join(completed_episodes_30_32_weeks, episodes_sent_home_foster_care)

# remove unmatched babies and episodes after baby first went home ----

completed_episodes_30_32_weeks_first_spell <- completed_episodes_30_32_weeks %>% 
  filter(episode_number <= sent_home_foster_care)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(episodes_sent_home_foster_care) # tidy up

# recalculate number_of_episodes to reflect the number of episodes in the first spell of care only ----

completed_episodes_30_32_weeks_first_spell <- completed_episodes_30_32_weeks_first_spell %>% 
  group_by(baby_upi) %>% 
  mutate(number_of_episodes = n()) %>% 
  ungroup()

table(completed_episodes_30_32_weeks_first_spell$number_of_episodes)

# calculate last_episode flag ----

completed_episodes_30_32_weeks_first_spell <- 
  completed_episodes_30_32_weeks_first_spell %>% 
  mutate(last_episode = episode_number == number_of_episodes)

saveRDS(completed_episodes_30_32_weeks_first_spell, paste0(data_path, "/", "completed_episodes_30_32_weeks_first_spell.rds"))

# identify babies who had major surgery (in any spell) ----

babies_with_major_surgery <- as_tibble(dbGetQuery(denodo_connect, "SELECT DISTINCT baby_upi
  FROM matneo.matneo_neocare_day 
        WHERE major_surgery = '1' AND baby_upi IS NOT NULL"))

nrow(babies_with_major_surgery) # number of babies

# remove babies who had major surgery (in any spell) from cohort ----

completed_episodes_30_32_weeks_first_spell <- 
  anti_join(completed_episodes_30_32_weeks_first_spell, babies_with_major_surgery)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

# identify babies born in Scotland ----

babies_born_by_country <- 
  left_join(completed_episodes_30_32_weeks_first_spell, location_mapping,
            by = join_by(location_of_delivery_code == location_code)) %>% 
  mutate(scottish_health_board_curr = if_else(location_of_delivery_code == "D201N", "Y", scottish_health_board_curr)) %>%  # assume babies born at home are Scottish-born 
  select(baby_upi, born_in_scotland = scottish_health_board_curr) %>% 
  distinct()

nrow(babies_born_by_country)

# remove babies born outwith Scotland from cohort ----

completed_episodes_30_32_weeks_first_spell <- 
  left_join(completed_episodes_30_32_weeks_first_spell, babies_born_by_country) %>% 
  filter(born_in_scotland == "Y")

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(babies_born_by_country) # tidy up

# remove babies admitted from outwith Scotland ----
# this removed D299N - might be one episode not coded correctly within a spell

admitted_from_outwith_Scotland <- completed_episodes_30_32_weeks_first_spell %>%
  filter(substr(location_admitted_from_code, 1, 1) == "E") %>% 
  select(baby_upi) %>% 
  distinct()

completed_episodes_30_32_weeks_first_spell <- 
  anti_join(completed_episodes_30_32_weeks_first_spell, admitted_from_outwith_Scotland)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(admitted_from_outwith_Scotland) # tidy up

# remove babies discharged outwith Scotland ----
# have not removed D299N - might be one episode not coded correctly within a spell
# not all episodes have a location_discharged_to as this is only completed when 
# a baby is transferred to another hospital so would need to keep NAs

discharged_outwith_Scotland <- completed_episodes_30_32_weeks_first_spell %>%
  filter(substr(location_discharged_to_code, 1, 1) == "E") %>% 
  select(baby_upi) %>% 
  unique()

completed_episodes_30_32_weeks_first_spell <- 
  anti_join(completed_episodes_30_32_weeks_first_spell, discharged_outwith_Scotland)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(discharged_outwith_Scotland) # tidy up

saveRDS(completed_episodes_30_32_weeks_first_spell, paste0(data_path, "/", "completed_episodes_30_32_weeks_first_spell.rds"))

# identify highest level of care to exclude transitional care only ----

# select neocare_episode_unique_id and baby_upi from cohort to check highest level of care

completed_episodes_30_32_weeks_first_spell <- readRDS(paste0(data_path, "/", "completed_episodes_30_32_weeks_first_spell.rds"))

records_to_check <- select(completed_episodes_30_32_weeks_first_spell, neocare_episode_unique_id, baby_upi, number_of_episodes)

# pull all day records with a baby_upi and summarise episodes by neocare_episode_unique_id, baby_upi and highest_level_of_care

highest_level_of_care <- as_tibble(dbGetQuery(denodo_connect, 
"SELECT neocare_episode_unique_id, baby_upi, CAST(highest_level_of_care_code AS INTEGER) AS highest_level_of_care_code, highest_level_of_care_desc, COUNT(*)
  FROM matneo.matneo_neocare_day
    WHERE baby_upi IS NOT NULL
      GROUP BY neocare_episode_unique_id, baby_upi, highest_level_of_care_code, highest_level_of_care_desc"))

# match highest_level_of_care entries to the baby records in the cohort

records_to_check <- left_join(records_to_check, highest_level_of_care)

# look for the MINIMUM value in the highest_level_of_care_code; anything above 1 means the baby wasn't in Neonatal Care at all during the spell. Note that there are some NA values. These babies will be removed from the cohort.

records_to_check <- records_to_check %>% 
  group_by(baby_upi) %>% 
  mutate(min_highest_level_of_care_code = min(highest_level_of_care_code, na.rm = TRUE),
         min_highest_level_of_care_code = if_else(min_highest_level_of_care_code == Inf, NA, min_highest_level_of_care_code)
          ) %>% 
  filter(is.na(min_highest_level_of_care_code) | min_highest_level_of_care_code > 1) %>% 
  select(baby_upi) %>% 
  distinct()

completed_episodes_30_32_weeks_first_spell <- 
  anti_join(completed_episodes_30_32_weeks_first_spell, records_to_check)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(records_to_check, highest_level_of_care) # tidy up

# remove spells where there is a delay between discharge and subsequent admission of more than 2 days ----

# baby may have been transferred out of neonatal care and then back in before being sent home

range(completed_episodes_30_32_weeks_first_spell$delay_between_episodes, na.rm = TRUE)

table(completed_episodes_30_32_weeks_first_spell$delay_between_episodes, useNA = "ifany")

# negative values appear to belong to either duplicate or inaccurate entries
# NA values are attached to the first episode in a spell
# 0 means the baby was transferred on the same day
# 1 means the baby was transferred the next day (probably overnight)
# 2 could be a data entry error
# values > 2 suggest a baby may have been transferred out of neocare for a period of time before returning and then going home
# affects a very small number of records

# identify babies with delay > 48 hours ----

delays <- completed_episodes_30_32_weeks_first_spell %>% 
  filter(delay_between_episodes > 48) %>% 
  select(baby_upi)

completed_episodes_30_32_weeks_first_spell <- 
  anti_join(completed_episodes_30_32_weeks_first_spell, delays)

nrow(completed_episodes_30_32_weeks_first_spell) # number of episodes

length(unique(completed_episodes_30_32_weeks_first_spell$baby_upi)) # number of babies

rm(delays) # tidy up

### 6 - Analysis for SPBAND measure ----

# check episode_number of "last" episodes

filter(completed_episodes_30_32_weeks_first_spell, last_episode == TRUE) %>% 
  count(episode_number)

# select last episodes and add new variables needed for SPBAND ----

# quarter_of_discharge - derived as quarter beginning (of first discharge from specialist neonatal care)
# quarter_of_discharge_label - the short name of the month and year (to label axes)
# time_from_birth_to_discharge - calculated as the length of time between last date_time_of_discharge and baby_birth_date_time
# gestation_at_delivery - calculated as gestation_at_delivery_weeks + gestation_at_delivery_days
# gestation_at_discharge - calculated as gestation_at_delivery + time_from_birth_to_discharge

completed_episodes_30_32_weeks_last_episode <- completed_episodes_30_32_weeks_first_spell %>% 
  filter(last_episode == TRUE) %>% 
  arrange(date_time_of_discharge) %>% 
  mutate(quarter_of_discharge = as.Date(as.yearqtr(date_time_of_discharge)), # quarter beginning
         quarter_of_discharge_label = qtr(date_time_of_discharge, format = "short"),
         quarter_of_discharge_label = factor(quarter_of_discharge_label,
                                             levels = unique(quarter_of_discharge_label),
                                             ordered = TRUE),
         time_from_birth_to_discharge = difftime(date_time_of_discharge, baby_birth_date_time, units = "weeks"),
         gestation_at_delivery = as.integer(gestation_at_delivery_weeks) + as.integer(gestation_at_delivery_days)/7,
         gestation_at_discharge = gestation_at_delivery + time_from_birth_to_discharge
         ) %>% 
  filter(quarter_of_discharge <= cut_off_date_Qtrly)

saveRDS(completed_episodes_30_32_weeks_first_spell, paste0(data_path, "/", "completed_episodes_30_32_weeks_first_spell.rds"))

nrow(completed_episodes_30_32_weeks_last_episode) # number of episodes

length(unique(completed_episodes_30_32_weeks_last_episode$baby_upi)) # number of babies

completed_episodes_30_32_weeks_first_spell <- readRDS(paste0(data_path, "/", "completed_episodes_30_32_weeks_first_spell.rds"))

# add common fields for use in the SPBAND ----

babies_30_32_weeks_discharged_from_neocare <- completed_episodes_30_32_weeks_last_episode %>% 
  mutate(dataset = "NeoCareIn+",
         measure = "GESTATION AT DISCHARGE FROM NEONATAL CARE",
         hbtype = "Treatment",
         hbname = "Scotland",
         period = "Q"
  ) %>% 
  select(dataset, measure, hbtype, hbname, period, date = quarter_of_discharge, date_label = quarter_of_discharge_label, gestation_at_discharge)

# calculate median gestation_at_discharge and number of discharges per quarter ----

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare %>% 
  group_by(dataset, measure, hbtype, hbname, date, date_label, period) %>% # quarters
  summarise(`median corrected gestational age at first discharge` = as.numeric(round(median(gestation_at_discharge, na.rm = TRUE), 2)),
            `number of babies discharged` = n()
  ) %>% 
  pivot_longer(cols = `median corrected gestational age at first discharge`: `number of babies discharged`,
               names_to = "measure_cat",
               values_to = "measure_value"
  ) %>%
  mutate(suffix = if_else(measure_cat == "median corrected gestational age at first discharge", "weeks", NA)
  ) %>%
  select(dataset, measure, hbtype, hbname, period, date, date_label, measure_cat, measure_value, suffix) %>% 
  ungroup()

# mark dates for the post-pandemic median (Jul-Sep 2022 to Apr-Jun 2025) ----

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare %>%
  mutate(
    median_name = case_when(
      #date <= "2019-10-01" ~ "pre-pandemic median",
      between(date, as.Date("2022-07-01"), as.Date("2025-04-01")) ~ "post-pandemic median",
      .default = NA)
  )

# calculate post-pandemic medians and extend them ----

babies_30_32_weeks_discharged_from_neocare <- 
  calculate_medians(babies_30_32_weeks_discharged_from_neocare, measure_value = measure_value) 

# reset "extended" values to NA where "median" values exist (bar last "median" value)
# stops "extended" over plotting "median" which looks rubbish
# resets "extended" to NA when it is the last data point but still in the "median" period

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare |>
  group_by(dataset, hbtype, hbname, period, measure, measure_cat, median_name) |> 
  mutate(extended = if_else(
    !is.na(median) & !is.na(extended) & is.na(lead(median)),
    median, NA),
    extended = na.locf(extended, na.rm = FALSE),
    extended = case_when(
      date == max(date) & period == "Q" & median_name == "post-pandemic median" & date < as.Date("2025-07-01") ~ NA,
      .default = extended)
  )

# pivot wider to split median and extended into separate columns based on median_name ----

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare |>
  mutate(median_name2 = median_name) |> 
  pivot_wider(names_from = median_name2,
              values_from = median,
              values_fill = NULL,
              names_sort = TRUE)

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare |> 
  pivot_wider(names_from = median_name, 
              values_from = extended,
              values_fill = NULL,
              names_prefix = "extended ",
              names_sort = TRUE)

# tidy up ----

babies_30_32_weeks_discharged_from_neocare <- babies_30_32_weeks_discharged_from_neocare |> 
  janitor::clean_names() %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE) 

# add metadata for download ----

babies_30_32_weeks_discharged_from_neocare <- 
  left_join(babies_30_32_weeks_discharged_from_neocare, metadata, 
            by = c("measure", "measure_cat")) %>% 
  janitor::remove_empty(which = "cols") %>% 
  arrange(dataset, measure, hbtype, hbname, period, measure_cat, date, date_label) %>% 
  ungroup()

saveRDS(babies_30_32_weeks_discharged_from_neocare, paste0(data_path, "/", "babies_30_32_weeks_discharged_from_neocare_download_dataframe.rds")) # note NOT in dashboard_dataframes folder, only needed to create download file

# add number of babies to num to make charts work ----

babies_30_32_weeks_discharged_from_neocare <- readRDS(paste0(data_path, "/", "babies_30_32_weeks_discharged_from_neocare_download_dataframe.rds"))

number_of_babies <- 
  filter(babies_30_32_weeks_discharged_from_neocare, measure_cat == "number of babies discharged") %>% 
  select(dataset:date_label, num = measure_value)

babies_30_32_weeks_discharged_from_neocare <- 
  left_join(babies_30_32_weeks_discharged_from_neocare, number_of_babies) %>% 
  filter(measure_cat != "number of babies discharged") %>% 
  select(dataset:measure_cat, num, measure_value, suffix, contains("median"))

saveRDS(babies_30_32_weeks_discharged_from_neocare, paste0(dashboard_dataframes_folder, "/", "babies-30-32-weeks-discharged-from-neocare.rds")) # this is the file loaded into SPBAND

babies_30_32_weeks_discharged_from_neocare <- readRDS(paste0(dashboard_dataframes_folder, "/", "babies-30-32-weeks-discharged-from-neocare.rds"))

# compare with number of babies born at 30-32 week's gestation ----

babies_born_30_32_weeks <- 
  readRDS(SMR02_filename) %>% 
  filter(year >= 2017 & condis == 3 & outcome1 == 1) %>% # maternity record live births
  mutate(dataset = "NeoCareIn+",
         hbtype = "Treatment",
         hbname = "Scotland",
         period = "Q",
         date_of_delivery = as.Date(date_of_delivery),
         quarter_of_delivery = as.Date(as.yearqtr(date_of_delivery)), # quarter beginning
         estgest = na_if(estgest, 99)
         ) %>% 
  filter(date_of_delivery >= "2018-01-01" &
           between(estgest, 30, 32) & quarter_of_delivery <= cut_off_date_Qtrly) %>%  # don't publish incomplete data
  select(dataset, hbtype, hbname, period, date_of_delivery, quarter_of_delivery, upi, discharge_date, numbir, outcome = outcome1, outcome_name = outcome1name, gestation_weeks = estgest)

babies_born_by_quarter_of_delivery <- 
  summarise(babies_born_30_32_weeks, .by = c(quarter_of_delivery), count = n()) %>% 
  arrange(quarter_of_delivery)

# test charts as per SPBAND ----

server_folder <- "https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/refs/heads/new_neonatal_measures/"

source(paste0(server_folder,
                "functions.R"), local = FALSE)

# a) data ----

corrected_gest_age_at_discharge_runchart_data <- ({
  # selects data
  
  data <- babies_30_32_weeks_discharged_from_neocare %>% 
    #filter(measure_cat == "median gestational age at first discharge") %>% 
    set_variable_labels(
      num = "Number of babies discharged to home or foster care",
      measure_value = "Median corrected gestational age at discharge",
      #pre_pandemic_median = " average to Oct-Dec 2019",
      #extended_pre_pandemic_median = " projected average from Jan-Mar 2020",
      post_pandemic_median = paste0("average from Jul-Sep 2022", "<br>", "to Apr-Jun 2025"),
      extended_post_pandemic_median = "projected average from Jul-Sep 2025"
    ) %>% 
    mutate(mytext = paste0("Quarter: ",
                           date_label,
                           "<br>",
                           var_label(num),
                           ": ",
                           num,
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks"),
           trend = NA, # to prevent this line being plotted
           shift = NA # ditto
    ) %>% 
    ungroup()

  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
})

# b) chart ----

# tells plotly where to place x-axis tick marks and labels

NeoCare_date_range <- unique(corrected_gest_age_at_discharge_runchart_data$date)
NeoCare_date_tickvals <- NeoCare_date_range[seq(1, length(NeoCare_date_range), 2)]
NeoCare_date_ticktext <- qtr(NeoCare_date_tickvals, format = "short")

orig_xaxis_plots <- list(
  title = "",
  showticklabels = TRUE,
  tickfont = list(size = 12),                         
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE, 
  tickangle = -45 # angles the tick labels
  )

xaxis_plots <- orig_xaxis_plots
xaxis_plots[["tickmode"]] <- "array"
xaxis_plots[["tickvals"]] <- NeoCare_date_tickvals
xaxis_plots[["ticktext"]] <- NeoCare_date_ticktext

orig_yaxis_plots <- list(
  title = list(font = list(size = 14)),
  showticklabels = TRUE,
  tickfont = list(size = 12),
  tickformat = ",d", # formats numbers with thousand separator if needed
  fixedrange = FALSE, # allows zoom
  rangemode = "tozero", # show all non-negative values
  zeroline = FALSE  
  )
yaxis_plots <- orig_yaxis_plots
yaxis_plots[["range"]] <- list(30, 40)

runcharts <-

  plot_ly(
    data = corrected_gest_age_at_discharge_runchart_data,
    x = ~ date,
    y = ~ measure_value,
    type = "scatter",
    mode = "lines+markers",
    line = list(
      color = "black", # black lines
      width = 1),
    marker = list(
      color = "black", # black dots
      size = 5),
    name = str_to_lower(var_label(corrected_gest_age_at_discharge_runchart_data$measure_value)),
    legendgroup = "measure_value",
    legendrank = 100,
    showlegend = TRUE,
    hovertext = ~ mytext,
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~ post_pandemic_median, # solid magenta line
    mode = "lines",
    line = list(
      color = phs_colours("phs-magenta"),
      width = 1),
    marker = NULL,
    name = ~ paste0(var_label(corrected_gest_age_at_discharge_runchart_data$post_pandemic_median)), # retrieves label of variable
    legendgroup = "post_pandemic_median",
    legendrank = 200,
    showlegend = TRUE,
    hoverinfo = "y",
    yhoverformat = ".1f"
  ) %>%
  add_trace(
    y = ~ extended_post_pandemic_median, # dotted magenta line
    mode = "lines",
    line = list(
      color = phs_colours("phs-magenta"),
      width = 1,
      dash = "4"
    ),
    marker = NULL,
    name = ~ paste0(var_label(corrected_gest_age_at_discharge_runchart_data$extended_post_pandemic_median)), # retrieves label of variable
    legendgroup = "extended_post_pandemic_median",
    legendrank = 300,
    showlegend = TRUE,
    hoverinfo = "y",
    yhoverformat = ".1f"
  )

runcharts <- runcharts %>% 
    layout(
    # font = plotly_global_font,
    xaxis = xaxis_plots,
    yaxis = yaxis_plots)

runcharts

# tidy up

rm(initial_cohort_30_32_weeks, completed_episodes_30_32_weeks, completed_episodes_30_32_weeks_first_spell, completed_episodes_30_32_weeks_last_episode, babies_with_major_surgery, number_of_babies, babies_born_30_32_weeks)

## - END OF SCRIPT ----