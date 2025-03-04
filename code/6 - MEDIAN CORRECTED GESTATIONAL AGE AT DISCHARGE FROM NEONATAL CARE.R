###
# Median corrected gestational age at discharge from neonatal care 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from the [NeoCareIn+ datamart] Maternity Team's SMR02 data file and SMR01?
# Bev Dodds
# 20 November 2024
# Last update by Bev Dodds
# Latest update description: initialised test code
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on Posit Workbench
# Version of R - 4.1.2
# Reads in SMR02 live births and mocks up lengths of stay to calculate corrected gestational ages
# Approximate run time - <5 minutes
###

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

### 2 - Initialise variables ----

### 3 - Read in source data ----

### 3a - real numbers, gestations and LOS will come from NeoCareIn+ ----

# The cohort contains a subset of the number of babies born alive at 30-32 weeks gestation who were admitted to a neonatal unit. The baby may have had a stay in multiple neonatal units. The LOS in neonatal care is added to their birth gestation to calculate their corrected gestational age at discharge:

# 30+0 to 32+6 weeks at birth
# calculate LOS (days between date of delivery and date of discharge from neonatal care)
# calculate corrected gestational age (gestation at birth plus LOS at discharge)
# calculate the median corrected gestational age per quarter of discharge from neonatal care
# want to capture first admissions only
# include only babies who stayed within neonatal care from first admission to discharge home/foster care 
# exclude babies who were transferred to paediatrics
# exclude babies who had surgery (exclusions to be defined)
# exclude babies who died

# need to select first admission (CHI/Baby ID and date of admission)
# at 30-32 weeks (gestation in weeks, but need gestation in days as well)

# Still to understand how continuous stays work - a baby may have episodes in more than one unit during their stay, hence a single episode may not contain the baby's final discharge details

# Variables required:

# NeoCareIn+ variable name = BadgerNet variable name = description [type]


# Identifiers:

# unique_episode_id = BadgerUniqueID = A unique identifier for the episode [char(20)]

# episode_number = EpisodeNumber = The number of this episode within the baby's episode of care (stay) [number]

# baby_national_id = NationalIDBaby = A unique identifier for the baby; will be CHI if available else NHS number/system generated unique number [char(20)]

# date_of_care = NULL = The date care was given to the baby [DateTime] (daily record variable)

# *** unique_episode_id + date_of_care *** will form the unique record id (key) for processing the daily data. Will be used to join the daily and episode data.


# Birth details:

# baby_birth_date_time = BirthTimebaby = The baby's date and time of birth [DateTime]

# Gestation at delivery (in completed weeks plus completed days):

# gestation_at_delivery_weeks = GestWeeks = Number of full weeks of gestation at delivery [number]

# gestation_at_delivery_days = GestDays = Number of additional days of gestation at delivery [number (0-6)]

# Admission details:

# Admission date (want first admission date) >= Jan 2018:

# date_time_of_admission = AdmitTime = The date time the baby was admitted [DateTime]

# ? admission_source = AdmissionSource = The source/type of location the baby was admitted from [char(2)]

# ? admission_type = Referral = Type of admission based on place of booking, place of birth and previous admission history [char(1)]

# ? admission_reason = AdmitPrincipalReason = The primary clinical reason for the episode of care [char(2)]

# ? admission_category = AdmitType = Category of care given at the start of the episode [char(2)]

# ? location_of_treatment_code = ProviderNHSCode = The code to identify the baby's location of treatment [char(10)]

# ? location_of_treatment_name = ProviderName = The name of the baby's location of treatment [char(125)]

 
# Discharge details:

# date_time_of_discharge = DischTime = The date time the baby was discharged [DateTime]

# discharge_destination = DischargeDestination = The destination of the baby at discharge at the end of the episode [char(2)]

# exclude baby if 3 = DIED

# include 1 = HOME, 4 = FOSTER CARE in FINAL episode (i.e. last date_time_of_discharge)

# ? exclude 12 = TRANSFERRED TO ANOTHER HOSPITAL FOR SURGICAL CARE 
# ? exclude any of the remaining codes for non-final episodes (baby no)

# location_discharged_to_code = DischargeHospitalCode = The code of the hospital the baby has been transferred to [char(15)] - only populated if baby is transferred to another hospital but this is not validated.

# location_discharged_to_name = DischargeHospitalName = The name of the location the baby has been transferred to [char(150)] - only populated if baby is transferred to another hospital but this is not validated.

# discharge_destination_wardtype = DischargeDestinationWard = The type of ward the baby was discharged to at the end of the episode [char(1)] - only populated if baby is transferred to another hospital but this is not validated.

# ? include 1 = POSTNATAL, 2 = TRANSITIONAL CARE, 3 = OTHER NEONATAL UNIT
# ? exclude 4 = PICU, 5 = OTHER WARD

# operation_procedure_performed = PrincipleProceduresDuringStay = The procedures selected at the time of discharge to be the principal procedures performed during the episode [char(4000)] *

# Daily records:

# operation_procedure_performed = OperationsToday = "Operations procedures performed during the 24 hour period [char(4000)] *

# ? major_surgery = MajorSurgeryToday = Indicates if the baby had major surgery on this day [char(1) (Y/N)] - exclude if Y?

# ? ecmo = ECMO = Indicates if the baby received ECMO in this day [char(1) (Y/N)] - exclude if Y?

# **** There could be other procedures that indicate this baby should be excluded from the cohort ****


# **** Procedures will be in a comma separated list; where possible they will be mapped to OPCS4; if we need to exclude babies based on anything in this field this needs to be specified and agreed; will need to see what is contained in this field. ****


# Derived variables:

# Quarter - will be derived as quarter beginning (of discharge from neonatal care) [Date]

# Gestation at discharge - will calculated from Gestation at delivery + days between(date of discharge, date of delivery) [number]




# perform counts on this data (section 5)
# calculate measure_value (section 5)
# create runcharts etc (section 6)

### Made up data ----

### 3c - SMR02 from .rds version ----
# to create charts for dashboard

# condis = 3 (delivered)
# outcome1 = 1 (livebirth) 

# have assumed all discharges have a delivery date 
# should cut_off_date_Qtrly be applied to the discharge dates?

babies_raw <- 
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

### 4 - Create new variables ----

# pre-pandemic median for now - not enough quarters from Jul 2022 for post-pandemic median yet

# babies_raw <- babies_raw %>%   
#   mutate(
    # median_name = case_when(
    #   quarter <= "2019-10-01" ~ "pre-pandemic median",
    #   #between(quarter, as.Date("2022-07-01"), as.Date("2024-06-01")) ~ "post-pandemic median",
    #   .default = NA
    # ),
    #date = quarter,
  # ) %>%  
  # janitor::remove_empty("cols") %>% 
  # select (- quarter)

# babies born this early are almost always admitted to neocare

count_rows <- nrow(babies_raw)

numbers <- summarise(babies_raw, .by = c(quarter_of_delivery, gestation_weeks), count = n()
                                         )

write.xlsx(numbers, file.path(data_path, "number of babies born at 30-32 weeks admitted to neonatal care.xlsx"))

# generate a random number of days (0-6) to add to the gestation in weeks

set.seed(1)

babies_30_32_admitted <- babies_raw %>% 
  mutate(gestation_days = round(runif(n = count_rows, min = 0, max = 6), 0),
         gestation = gestation_weeks + round(gestation_days/7, 2),
         days_in_neonatal = round(runif(n = count_rows, min = 4, max = 50), 0),
         corrected = round(gestation + days_in_neonatal/7, 2),
         discharge_date = date_of_delivery + days_in_neonatal
  ) %>% 
  arrange(discharge_date) %>% 
  mutate(quarter_of_discharge = as.Date(as.yearqtr(discharge_date)),
         quarter_of_discharge_label = qtr(ymd(discharge_date), format = "short"),
         quarter_of_discharge_label = factor(quarter_of_discharge_label,
                                             levels = unique(quarter_of_discharge_label),
                                             ordered = TRUE)
  )

dates <- babies_30_32_admitted %>% 
  group_by(quarter_of_delivery, quarter_of_discharge) %>% 
  summarise(count = n())

# calculate measure_value_mean and measure_value_median corrected gestation at discharge over quarters

# babies_30_32_discharged_from_neocare <- babies_30_32_admitted %>% 
#   group_by(dataset, hbtype, hbname, quarter_of_discharge, quarter_of_discharge_label, period, measure_cat = "discharged from neocare") %>% # quarters
#   summarise(measure = "MEDIAN CORRECTED GEST AGE",
#             measure_value_median = round(median(corrected, na.rm = TRUE), 2),
#             measure_value_mean = round(mean(corrected, na.rm = TRUE), 2),
#             num = n(),
#             suffix = "weeks"
#   ) %>% 
#   select(dataset, measure, hbtype, hbname, period, quarter_of_discharge, quarter_of_discharge_label, measure_cat, num, measure_value = measure_value_median, suffix, measure_value_mean) %>% 
#   ungroup()

babies_30_32_discharged_from_neocare <- babies_30_32_admitted %>% 
  group_by(dataset, hbtype, hbname, date = quarter_of_discharge, date_label = quarter_of_discharge_label, period) %>%  
  summarise(measure = "MEDIAN CORRECTED GEST AGE",
            `median corrected gestational age` = round(median(corrected, na.rm = TRUE), 2),
            #measure_value_mean = round(mean(corrected, na.rm = TRUE), 2),
            `number of babies` = n()
            #suffix = "weeks"
  ) %>% 
  pivot_longer(cols = `median corrected gestational age`: `number of babies`,
               names_to = "measure_cat",
               values_to = "measure_value"
               ) %>% 
  mutate(suffix = if_else(measure_cat == "median corrected gestational age", "weeks", NA)
         ) %>% 
  select(dataset, measure, hbtype, hbname, period, date, date_label, measure_cat, measure_value, suffix) %>% 
  ungroup()

babies_30_32_discharged_from_neocare <- left_join(babies_30_32_discharged_from_neocare, metadata, 
                                                  by = c("measure", "measure_cat")) %>% 
  janitor::remove_empty(which = "cols")

saveRDS(babies_30_32_discharged_from_neocare, paste0(dashboard_dataframes_folder, "/", "babies-30-32-discharged-from-neocare.rds"))

babies_30_32_discharged_from_neocare <- readRDS(paste0(dashboard_dataframes_folder, "/", "babies-30-32-discharged-from-neocare.rds"))

## - END OF SCRIPT ----