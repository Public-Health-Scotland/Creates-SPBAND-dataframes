library(DBI)
library(odbc)
library(dplyr)
library(phsmethods)
library(zoo)
library(lubridate)

denodo_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "DVPROD",
     uid = Sys.getenv("USER"),
    pwd = .rs.askForPassword("Enter your LDAP password"),
    bigint = "integer")
)

cut_off_date_Qtrly <- ymd("2025-07-01") # quarter beginning (most complete) e.g. Jan-Mar, usually increments by 1 quarter

initial_cohort_of_completed_episodes_loc <- as_tibble(dbGetQuery(denodo_connect,
                       "SELECT a.entity_id, a.neocare_episode_unique_id, episode_number, a.baby_upi, baby_birth_date_time, gestation_at_delivery_weeks, gestation_at_delivery_days, date_time_of_admission, date_time_of_discharge, location_of_delivery_matneo_key, location_of_delivery_code, location_of_delivery_name, location_admitted_from_matneo_key, location_admitted_from_code, location_admitted_from_name, a.location_of_treatment_matneo_key, a.location_of_treatment_code, a.location_of_treatment_name,  discharge_destination_code, discharge_destination_desc, location_discharged_to_matneo_key, location_discharged_to_code, location_discharged_to_name, discharge_destination_wardtype_code, discharge_destination_wardtype_desc, neocare_day_unique_id, date_of_care, bapm2011_level_of_care_code, bapm2011_level_of_care_desc, highest_level_of_care_code, highest_level_of_care_desc
                       
    FROM matneo.matneo_neocare_episode a
      LEFT OUTER JOIN matneo.matneo_neocare_day b 
        ON a.neocare_episode_unique_id = b.neocare_episode_unique_id 
          WHERE date_of_discharge IS NOT NULL AND a.baby_upi IS NOT NULL AND (gestation_at_delivery_weeks BETWEEN '34' AND '36' OR gestation_at_delivery_weeks BETWEEN '37' AND '42')
                       "))


initial_cohort_of_completed_episodes_loc <- initial_cohort_of_completed_episodes_loc %>% 
  mutate(gest_grp = case_when(
    between(gestation_at_delivery_weeks, 34, 36) ~ 1,
    between(gestation_at_delivery_weeks, 37, 42) ~ 2),
    quarter_of_birth = as.Date(as.yearqtr(baby_birth_date_time)), # quarter beginning
    dataset = "NeoCareIn+"
  ) %>%
   filter(quarter_of_birth <= cut_off_date_Qtrly)

highest_level_of_care <- initial_cohort_of_completed_episodes_loc %>%
  group_by(baby_upi, gest_grp) %>%
  summarise(min_highest_level_of_care_code = min(highest_level_of_care_code, na.rm = TRUE),
            min_bapm2011_level_of_care_code = min(bapm2011_level_of_care_code, na.rm = TRUE),
            days_in_care = n()
  ) %>% 
  filter(!is.na(min_bapm2011_level_of_care_code)) %>% 
  filter(min_highest_level_of_care_code == 1)

levels_of_care <- highest_level_of_care %>% 
  ungroup() %>% 
  count(gest_grp, min_bapm2011_level_of_care_code, min_highest_level_of_care_code)



