####
# SMR02 measures for the Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Inductions, Type of birth, Gestation at birth, Apgar5 scores and Perineal tears, 
# Pregnancies booked, Average gestation at booking, Terminations, Average gestation at termination
# Sourced from the Maternity Team's SMR02 data file, the ABC base file and the Terminations data file
# Bev Dodds
# Last update: 21 May 2024
# Last update by: Bev Dodds
# Latest update description: Finalised the "grouped" Island Board data for"Gestation at termination" and
# added a working "miniapp" to view data in charts before exporting it to the dashboard project 
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on R Studio Server
# Version of R - 4.1.2 - note use of dplyr 1.1.0
# Reads in SMR02 SPSS singleton live births and produces a variety of tables/charts 
# Approximate run time - 5 minutes
####

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

### 2 - Read in source data ----

### 2a - SMR02 from .rds version ----

# condis = 3 (delivered)
# numbir = 1 (singletons)
# outcome1 = 1 (livebirth) or 2 (stillbirth)
# model1 is based on model which is based on MODE_OF_DELIVERY_BABY_1:

# 0 Normal, spontaneous vertex vaginal delivery, occipito-anterior.
# 1 Cephalic vaginal delivery, with abnormal presentation of the head at delivery, without instruments, with or without manipulation
# 2 Low forceps, no rotation, forceps NOS (incl. Wrigleys).
# 5 Breech delivery, spontaneous, assisted or unspecified partial breech extraction.
# 6 Breech extraction, NOS. Version with breech extraction.
# 7 Elective (planned) caesarean section.
# 8 Emergency and unspecified caesarean section.
# 9 Other and unspecified method of delivery.
# A Mid cavity forceps, no rotation (incl. Haig Fergusson, Neville-Barnes etc).
# B Rotational forceps (incl Kiellands)
# C Ventouse, no rotation or unspecified
# D Ventouse with rotation
# E Other forceps delivery (includes ‘high-cavity’, high forceps)

# where model1 is coded from model as

# 0, 1 = "1, spontaneous"
# 2, 3, A, B, E = "2, forceps"
# 4, C, D = "3, vacuum"
# 5, 6 = "4, breech"
# 7 = "5, caesarean - elective (planned)"
# 8 = "6, caesarean - emergency (unplanned)"
# 9 = "9, other and unspecified"

births_raw <- 
  readRDS(SMR02_filename) %>%
  filter(year>= 2017 & condis == 3 & numbir == 1) %>%  # maternity record singleton births
  mutate(dataset = "SMR02",
         date = ymd(dodis),
         estgest = na_if(estgest, 99)) %>% 
  select(dataset, date, hbrcode = hb2019, hbrname = hb2019name, hbtcipher = hbt,
         outcome = outcome1, outcome_name = outcome1name,
         type_of_birth = model1, type_of_birth_name = model1name, induced = induce1,
         apgar5 = apgar, tears = tears, gestation = estgest)

# add hbtcode and hbtname, drop hbtcipher

births_raw <- left_join(births_raw, 
                        select(hbcipher, c(HBCIPHER, HBCODE, HBNAME)),
                        by = c("hbtcipher" = "HBCIPHER")) %>% 
  rename(hbtcode = HBCODE, hbtname = HBNAME) %>% 
  select (- hbtcipher)

### 2b - ABC data from .rds version ----

bookings_raw <-
  readRDS(ABC_filename) %>%
  filter(chi != "" & hbt != "E") %>% # exclude cases booked in England
  arrange(mothersfirstforename, motherssurname, mothers_dob) %>% 
  # tibble::rowid_to_column() %>%
  mutate(dataset = "ABC",
         date = ymd(booking_date),
         gestation_at_booking = na_if(gestation_at_booking, 99)) %>% 
  select(dataset, date, hbrcode = hb2019, hbrname = hb2019name, 
         hbtcode = hbt2019code, hbtname = hbt2019name, gestation = gestation_at_booking)

bookings_raw <- bookings_raw %>%
  mutate(gestation = if_else(gestation < 2 | gestation > 44, NA_real_, gestation))

### 2c - Terminations data from .rds version ----
# ensure the correct file is initialised in the housekeeping code!

terminations_raw <- readRDS(terminations_filename) %>%
  filter(year>= 2017 & STATUS != "D" & HB_TREATMENT_CYPHER != "E") %>% # temporarily exclude Draft records
  mutate(dataset = "TERMINATIONS",
         date = ymd(DATE_OF_TERMINATION)) %>% 
  select(dataset, date, hbrcode = hb2019, hbrname = hb2019name,
         hbtcipher = HB_TREATMENT_CYPHER, gestation = ESTIMATED_GESTATION)

# add hbtcode and hbtname, drop hbtcipher

terminations_raw <- left_join(terminations_raw, 
                          select(hbcipher, c(HBCIPHER, HBCODE, HBNAME)),
                 by = c("hbtcipher" = "HBCIPHER")) %>% 
  rename(hbtcode = HBCODE, hbtname = HBNAME) %>% 
  select (- hbtcipher)
  
### 3 - Bind births_raw, bookings_raw and terminations_raw tables together ----
# set blank hbname to "Unknown", change "and" to "&"

analysis_raw <- bind_rows(births_raw, bookings_raw, terminations_raw) %>% 
  mutate(hbrname = if_else(hbrname == "" | is.na(hbrname), "Unknown", hbrname),
         hbtname = if_else(hbtname == "" | is.na(hbtname), "Unknown", hbtname),
         hbrcode = if_else(hbrcode == "" | hbrcode == "RA2704" | is.na(hbrcode), "Unknown", hbrcode),
         hbtcode = if_else(hbtcode == "" | is.na(hbtcode), "Unknown", hbtcode),
         hbrname = sub(" and ", " & ", hbrname),
         hbtname = sub(" and ", " & ", hbtname)
         )

# rm(births_raw, bookings_raw, terminations_raw) # tidy up

### 4 - Recode all data to Scotland ----

# Scotland

scotland <- analysis_raw %>%
  mutate(
    hbrcode = "S92000003",
    hbrname = "Scotland",
    hbtcode = "S92000003",
    hbtname = "Scotland")

# add Scotland data to original dataset

analysis_raw <- bind_rows(scotland, analysis_raw) 

# rm(scotland)

### 5 - Create useful date parameters ----
         
analysis_raw <- analysis_raw %>% 
  mutate(fin_year = extract_fin_year(date),
         calendar_year = format(ymd(date), "%Y"),
         month_beginning = floor_date(date, unit = "month"),
         quarter = as.Date(as.yearqtr(date)) # quarter beginning
         ) %>% 
  select(dataset, date, fin_year:quarter, hbrcode,
         hbrname, hbtcode, hbtname, outcome, outcome_name, type_of_birth, type_of_birth_name, induced,
         apgar5, tears, gestation)
         
### 6 - Create summary tables for checking denominators ----

births_summary <- filter(analysis_raw, dataset == "SMR02") %>%
  group_by(hbrname, hbtname, month_beginning, quarter, fin_year, calendar_year, outcome_name) %>% 
  summarise(
    births_18_44 = sum(between(gestation, 18, 44), na.rm = TRUE),
    births_32_36 = sum(between(gestation, 32, 36), na.rm = TRUE),
    births_37_41 = sum (between(gestation, 37, 41), na.rm = TRUE),
    births_37_42 = sum(between(gestation, 37, 42), na.rm = TRUE),
    births_under32 = sum(gestation < 32, na.rm = TRUE),
    births_under37 = sum(gestation < 37, na.rm = TRUE),
    births_42plus = sum (between(gestation, 42, 44), na.rm = TRUE),
    births_gest_unknown = sum(is.na(gestation)),
    births_all = n() # births_ALL includes NAs (outwith 18-44 and unknown)
    )

write.csv(births_summary, row.names = FALSE,
          file.path(data_path, paste0("births_summary_", refresh_date, ".csv")))

rm(births_summary)

bookings_summary <- filter(analysis_raw, dataset == "ABC") %>%
  group_by(hbrname, hbtname, month_beginning, quarter, fin_year, calendar_year) %>% 
  summarise(
    under10wks = sum(gestation <= 9, na.rm = TRUE),
    between10and12wks = sum(between(gestation, 10, 12), na.rm = TRUE),
    `13wksandover` = sum(gestation >= 13, na.rm = TRUE),
    unknown = sum(is.na(gestation)),
    allbookings = n())

write.csv(bookings_summary, row.names = FALSE,
          file.path(data_path, paste0("bookings_summary_", refresh_date, ".csv")))

rm(bookings_summary)

terminations_summary <-
  filter(analysis_raw, dataset == "TERMINATIONS") %>%
  group_by(hbrname, hbtname, month_beginning, quarter, fin_year, calendar_year) %>% 
  summarise(
    under10wks = sum(gestation <= 9, na.rm = TRUE),
    between10and12wks = sum(between(gestation, 10, 12), na.rm = TRUE),
    `13wksandover` = sum(gestation >= 13, na.rm = TRUE),
    unknown = sum(is.na(gestation)),
    allterminations = n()) 

write.csv(terminations_summary, row.names = FALSE,
          file.path(data_path, paste0("terminations_summary_", refresh_date, ".csv")))

rm(terminations_summary)

### 7 - Check completeness of data ---- 
# each month versus the average of the previous 12 months
# BOARD OF RESIDENCE ONLY, excludes "Unknown"

completeness <- filter(analysis_raw, hbrname != "Unknown") %>% 
  mutate(date = 
           if_else(
             dataset == "SMR02",
             quarter,
             month_beginning
           )
         ) %>% 
  group_by(dataset, hbrname, calendar_year, date) %>% 
  summarise(count = n()) %>%
  group_by(dataset, hbrname) %>%
  arrange(dataset, calendar_year, date, .by_group = TRUE) %>%
  mutate(lag_count = lag(count, 1),
         `12_month_mean` = 
           if_else(dataset == "SMR02",
                   zoo::rollmean(lag_count, 4, fill = NA, align = "right"),
                   zoo::rollmean(lag_count, 12, fill = NA, align = "right")
           ),
         perc_complete = round(percentage(count, `12_month_mean`), 1)) %>%
  ungroup() %>% 
  mutate(date = 
           if_else(
             dataset == "SMR02",
             substr(qtr(date, format = "short"), 1, 7),
             month(date, label = TRUE)
             )
  ) %>% 
  arrange(desc(dataset))

percent_complete <- select(
  completeness, c(dataset, hbrname, YEAR = calendar_year, date, perc_complete)) %>% 
  filter(!is.na(perc_complete)) %>% 
  pivot_wider(names_from = date, values_from = perc_complete)

count <- select(
  completeness, c(dataset, hbrname, YEAR = calendar_year, date, count)) %>%
  pivot_wider(names_from = date, values_from = count)

# write out completeness stats

list_of_datasets <- list("percent_complete" = percent_complete, "count" = count)

write.xlsx(list_of_datasets, file.path(data_path, paste0("completeness_", refresh_date, ".xlsx")))

rm(completeness, list_of_datasets, count, percent_complete)

### 8 - Create RESIDENCE and TREATMENT files ----

# create BOARD of RESIDENCE file

analysis_R <- select(analysis_raw, - c(hbtname, hbtcode)) %>% 
  rename(hbname = hbrname, hbcode = hbrcode) %>%  
  mutate(hbtype = "RESIDENCE")

# create BOARD of TREATMENT file

analysis_T <- select(analysis_raw, - c(hbrname, hbrcode)) %>%
  rename(hbname = hbtname, hbcode = hbtcode) %>%  
  mutate(hbtype = "TREATMENT")

# add them together

analysis_both <- rbind(analysis_R, analysis_T)

rm(analysis_R, analysis_T)

### 9 - Create files by PERIOD ----

# create MONTHLY FILE - set date to month_beginning etc. date is a character to allow FY and CY formats
# e.g. 2020/21 and 2022

analysis_M <-
  filter(analysis_both, dataset != "SMR02") %>% # only need Q for SMR02 measures
  mutate(period = "M",
         date = as.character(month_beginning))

# create QUARTERLY FILE - set date to quarter

analysis_Q <- filter(analysis_both, dataset =="SMR02") %>% 
  mutate(period = "Q",
         date = as.character(quarter))

# create FINANCIAL YEAR FILE - set date to fin_year

analysis_FY <- analysis_both %>% 
  mutate(period = "FY",
         date = as.character(fin_year))

# create CALENDAR YEAR FILE - set date to calendar_year

analysis_CY <- analysis_both %>% 
  mutate(period = "CY",
         date = as.character(calendar_year))

# add them together

analysis_ALL <- bind_rows(
  analysis_M, 
  analysis_Q,
  analysis_FY,
  analysis_CY) #%>% 
  # tibble::rowid_to_column()

rm(analysis_both, analysis_M, analysis_Q, analysis_FY, analysis_CY)

### 10 - Create new variables ----

# 10a - Split analysis_ALL into component parts ----

# remove data that will be incomplete using cut off dates

births <- filter(analysis_ALL,
                 dataset == "SMR02",
                 month_beginning <= cut_off_date) |>
  mutate(median_name = if_else(date <= "2019-12-31" &
                                 period == "Q", "pre-pandemic median", NA)) |>  # pre-pandemic median only (for now)
  janitor::remove_empty("cols")

bookings_terminations <-
  bind_rows(
    filter(
      analysis_ALL,
      dataset == "ABC" &
        month_beginning <= cut_off_date_ABC
    ),
    filter(
      analysis_ALL,
      dataset == "TERMINATIONS" &
        month_beginning <= cut_off_date
    )
  ) %>%
  mutate(
    median_name = case_when(
      month_beginning <= "2020-02-01" & period == "M" ~ "pre-pandemic median",
      between(
        month_beginning,
        as.Date("2022-07-01"),
        as.Date("2024-06-01")
      ) & period == "M" ~ "post-pandemic median",
      .default = NA
    )
  ) |>
  janitor::remove_empty("cols")

# rm(analysis_raw, analysis_ALL) # tidy up

# 10b - Add BIRTHS measures ----
# all based on singletons

# flag spontaneous vaginal, assisted, planned caesarean, unplanned caesarean, unknown
# type_of_birth is my name for model1 (see section 2a)

births <- births %>% 
  mutate(new_type_of_birth_name = case_when(
    type_of_birth == 1 ~ 1, # "spontaneous"
    type_of_birth %in% c(2, 3, 4) ~ 2, # "forceps", "breech", "vacuum" = "assisted"
    type_of_birth == 5 ~ 3, # planned caesarean
    type_of_birth == 6 ~ 4, # unplanned caesarean
    TRUE ~ 9) # unknown
  )

# live births, any gestation
# if still birth, reset new_type_of_birth_name to NA

births <- births %>%
  mutate(
    new_type_of_birth_name = if_else(outcome == 1, new_type_of_birth_name, NA_real_)
  )

births$new_type_of_birth_name <- 
  factor(births$new_type_of_birth_name,
                                  levels = c(1, 2, 3, 4, 9),
                                  labels = c("spontaneous vaginal births",
                                             "assisted vaginal births",
                                             "planned caesarean births",
                                             "unplanned caesarean births",
                                             "unknown type of birth")
)

# flag all caesarean births

births <- births %>% 
  mutate(new_type_of_birth_name2 = case_when(
    type_of_birth %in% c(5, 6) ~ 1, # all caesarean births
    TRUE ~ NA_real_)
  )

# live births, any gestation
# if still birth, reset new_type_of_birth_name2 to NA

births <- births %>%
  mutate(
    new_type_of_birth_name2 = if_else(outcome == 1, new_type_of_birth_name2, NA_real_)
  )

births$new_type_of_birth_name2 <- 
  factor(births$new_type_of_birth_name2,
         levels = 1,
         labels = "caesarean births"
  )

# flag not induced, induced, not known in new_induced
# induced = 0 - not induced, 1 - induced, 2 - unknown

births <- births %>% 
  mutate(new_induced = case_when(
    induced == 0 ~ 1, # not induced
    induced == 1 ~ 2, # induced
    TRUE ~ 9) # not known
  )

# live births, 37_42 weeks
# if still birth or any other gestation reset new_induced to NA

births <- births %>%
  mutate(new_induced = if_else(
  outcome == 1 & between(gestation, 37, 42), new_induced, NA_real_)
  )

births$new_induced <- 
  factor(births$new_induced,
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

births <- births %>% 
  mutate(new_apgar5 = case_when(
    apgar5 %in% c('00', '01', '02', '03', '04', '05', '06') ~ 1, # low (<7) apgar score
    apgar5 %in% c('07', '08', '09', '10') ~ 2, # apgar5 score of 7 or more
    TRUE ~ 9 # not recorded/not not available/not known
  )
  )

# live births, 37_42 weeks
# if stillbirth or any other gestation reset new_apgar5 to NA

births <- births %>% 
  mutate(new_apgar5 = if_else(
  outcome == 1 & between(gestation, 37, 42), new_apgar5, NA_real_)
  )

births$new_apgar5 <- 
  factor(births$new_apgar5, levels = c(1, 2, 9),
         labels = c("low (<7) apgar5 score", "apgar5 score of 7 or more",
                    "apgar5 score unknown")
  )

# flag no tear, 1st and 2nd degree, 3rd and 4th degree, unspecified, 
# and unknown perineal tears
# type_of_birth = 1 - SVCB, 2 - forceps, 3 - ventouse, 4 - breech, 5 - planned caesarean, 
# 6 - unplanned caesarean
# tears = 0 - no tear, 1,2 - 1st and 2nd degree, 3,4 - 3rd and 4th degree,
# 8 - unspecified tear, 9 - unknown tear status

births <- births %>% 
  mutate(new_tears = case_when(
    tears == 0 ~ 0, # no tears
    tears %in% c(1, 2) ~ 1, # 1st and 2nd degree tears 
    tears %in% c(3, 4) ~ 2, # 3rd and 4th degree tears
    tears == 8 ~ 3, # unspecified tear
    TRUE ~ 9) # unknown tear status
  )

# live or still births, 37_42 weeks, born vaginally 
# (spontaneous vaginal plus forceps/ventouse but NOT breech)
# any other gestation or type of birth reset new_tears to NA

births <- births %>% 
  mutate(new_tears = if_else(
    between(gestation, 37, 42) & type_of_birth %in% c(1, 2, 3), new_tears, NA_real_)
    )

births$new_tears <- 
  factor(births$new_tears, levels = c(0, 1, 2, 3, 9),
         labels = c("no tears",
                    "1st or 2nd degree tears",
                    "3rd or 4th degree tears", 
                    "unspecified tears",
                    "unknown tear status")
  )

# flag gestation periods (estgest has already been recoded
# (18 thru 44 = copy)(else = 99))

births <- births %>% 
  mutate(gest_grp = case_when(
    gestation < 32 ~ 1,
    between(gestation, 32, 36) ~ 2,
    between(gestation, 37, 41) ~ 3,
    between(gestation, 42, 44) ~ 4,
    TRUE ~ 9 # unknown
  )
  )

# live births
# if still birth reset gest_grp to NA

births <- births %>% 
  mutate(gest_grp = if_else(
    outcome == 1, gest_grp, NA_real_
  )
  )

births$gest_grp <- 
  factor(births$gest_grp, levels = c(1, 2, 3, 4, 9),
         labels = c("under 32 weeks",
                    "between 32 and 36 weeks (inclusive)",
                    "between 37 and 41 weeks (inclusive)",
                    "42 weeks and over (inclusive)",
                    "unknown gestation") 
  )

# flag 'other' gestation categories (overlapping the 'main' categories) - 

births <- births %>% 
  mutate(gest_grp2 = case_when(
         gestation < 37 ~ 1,
         between(gestation, 37, 42) ~ 2,
         gestation > 42 ~ 3,
         TRUE ~ 9 # "unknown" to make up to 100%
         )
  )

# live births
# if still birth reset gest_grp2 to NA

births <- births %>% 
  mutate(gest_grp2 = if_else(
    outcome == 1, gest_grp2, NA
    )
  )

births$gest_grp2 <-
  factor(births$gest_grp2, levels = c(1, 2, 3, 9), 
         labels = c("under 37 weeks",
                    "between 37 and 42 weeks (inclusive)",
                    "over 42 weeks gestation (inclusive)",
                    "other or unknown gestation")
         )

births <- births %>% 
  mutate(lb_gest_18_44 = if_else(between(gestation, 18, 44) & outcome == 1, 1, 0),
         lb_gest_unknown = if_else(gestation == 99 & outcome == 1, 1, 0),
         births = 1
         )

# 10c - Add BOOKINGS and TERMINATIONS measures ----

bookings_terminations <- bookings_terminations %>%
  mutate(gest_grp3 = case_when(
  gestation <= 9 ~ 1,
  between(gestation, 10, 12) ~ 2,
  between(gestation, 13, 98) ~ 3,
  TRUE ~ 9))

# set factor levels for gest_grp3

bookings_terminations$gest_grp3 <- factor(
  bookings_terminations$gest_grp3,
  levels = c(1, 2, 3, 9),
  labels = c("under 10 weeks",
             "between 10 and 12 weeks (inclusive)",
             "13 weeks and over (inclusive)",
             "unknown"
             )
  )

# group island boards in the average gestation terminations dataset 

bookings_terminations <- bookings_terminations %>% 
  mutate(count = 1,
         hbname = if_else(dataset == "TERMINATIONS" & hbname %in% island_boards,
                          "NHS Orkney, NHS Shetland and NHS Western Isles", hbname)
         ) 

### 11 - TABLES of counts, percentages, averages ----

### 11a - BIRTHS measures ----

# and % of births
# calculate NUMBER and PERCENTAGE of VARIABLE compared with births
# by HB - these are the points plotted on small multiple and runcharts (Q),
# shown in multi indicator overview (FY, CY)
# breakdowns are made available in the data download only

# MIGHT WANT TO SHORTEN LABELS AS THEY DOMINATE THE CHART

inductions <- # to test everything looks the same as before
  counts(
    variable = new_induced,
    suffix = "%", # for hovertext
    measure = "INDUCTIONS"
  ) 

type_of_birth <-
  counts(
    variable = new_type_of_birth_name,
    suffix = "%", # for hovertext
    measure = "TYPE OF BIRTH"
  )

tears <-
  counts(
    variable = new_tears,
    suffix = "%", # for hovertext
    measure = "TEARS"
  )

gestation1 <-
  counts(
    variable = gest_grp,
    suffix = "%", # for hovertext
    measure = "GESTATION AT BIRTH"
  ) 

gestation2 <-
  counts(
    variable = gest_grp2,
    suffix = "%", # for hovertext
    measure = "GESTATION AT BIRTH"
  ) %>% 
    filter(measure_cat %in% c("under 37 weeks")
           ) # only want this category

# add gestation grouped files together

gestation <- rbind(gestation1, gestation2)

# recode "total_exc._unknown" to "between 18 and 44 weeks"

gestation <- gestation %>% 
  mutate(measure_cat = 
           if_else(measure_cat == "total exc. unknown", "between 18 and 44 weeks (inclusive)", measure_cat)
  )

apgar5 <-
  counts(
    variable = new_apgar5,
    suffix = "%", # for hovertext
    measure = "APGAR5"
  ) 

### 11b - BOOKINGS and TERMINATIONS measures ----

# and % of BOOKINGS/TERMINATIONS
# calculate NUMBER and PERCENTAGE of VARIABLE compared with BOOKINGS/TERMINATIONS
# by HB - these are the points plotted on small multiple and runcharts (Q),
# NOT shown in multi indicator overview
# breakdowns are made available in the data download only (not TERMINATIONS)

bookings <-
  counts(
    dataset = filter(bookings_terminations, dataset == "ABC"),
    variable = gest_grp3,
    tally_var = count,
    suffix = "", # for hovertext
    measure = "BOOKINGS"
    ) %>% 
  mutate(measure_cat = 
           if_else(measure_cat == "total", "all pregnancies booked", measure_cat)
  )

# remove gestation breakdowns for Island Boards and financial/calendar year values as they aren't used

bookings <- bookings %>%
  mutate(measure_value = 
           if_else(hbname %in% island_boards &
                     measure_cat != "all pregnancies booked", NA, measure_value)) %>% 
  filter(!is.na(measure_value) & !period %in% c("CY", "FY")
         )

# remove post-pandemic median category (not needed for this measure)

bookings <- bookings %>%
  mutate(median_name = 
           if_else(median_name == "post-pandemic median", NA, median_name))

terminations <- # function needs all categories but only produces the totals until disclosure ready
  counts(
    dataset = filter(bookings_terminations,
                     dataset == "TERMINATIONS" & 
                       hbname != "NHS Orkney, NHS Shetland and NHS Western Isles"),
    variable = gest_grp3, # not used but needed for function
    tally_var = count,
    suffix = "", # for hovertext
    measure = "TERMINATIONS"
    ) %>% 
  mutate(measure_value = 
           if_else(measure_cat == "total" & measure_value < 5, NA, measure_value), # disclosure control total 
         measure_cat = 
           if_else(measure_cat == "total", "all terminations", measure_cat)
  )

# remove financial/calendar year values as they aren't used

terminations <- terminations %>%
  filter(!period %in% c("CY", "FY")
         )

# remove post-pandemic median category (not needed for this measure)

terminations <- terminations %>%
  mutate(median_name = 
           if_else(median_name == "post-pandemic median", NA, median_name))

### 11c - Average gestation at BOOKING/TERMINATION ----

# by HB - these are the points plotted on charts
# CY/FY (multi-indicator overview) or M (individual measures)

av_gestation <- bookings_terminations %>% 
  mutate(measure_cat = "average gestation",
         measure = if_else(dataset == "ABC", 
                             "GESTATION AT BOOKING",
                             "GESTATION AT TERMINATION"),
         suffix = " weeks") %>% 
  select(- median_name)

# mark "special" FV and TAY months - these need additional medians, shifts and trends

av_gestation <- av_gestation |> 
  mutate(median_name = 
           case_when(
             month_beginning <= "2020-02-01" & period == "M" ~ "pre-pandemic median",
             between(month_beginning, as.Date("2022-07-01"), as.Date("2024-06-01")) & period == "M" ~ "post-pandemic median",
             dataset == "ABC" & period == "M" & 
                     ((hbname == "NHS Forth Valley" &
                         date >= "2021-03-01" & date <= "2022-02-01") | # 12 months
                        (hbname == "NHS Tayside" &
                           date >= "2020-08-01" & date <= "2021-07-01")) ~ "revised median", # 12 months
             .default = NA
             )
           )

# calculate average gestation each month

av_gestation <- av_gestation |>  
  group_by(dataset, hbtype, hbname, date, period, median_name, measure, measure_cat, suffix) %>% 
  summarise(measure_value = mean(gestation, na.rm = TRUE))

### 12 - Create data frames to be used in SPBAND ----

### 12a - Create everything data frame to perform common calculations on ----

everything_dataframe <- bind_rows(inductions,
                                  apgar5,
                                  tears,
                                  type_of_birth,
                                  gestation,
                                  bookings,
                                  terminations,
                                  av_gestation) %>% 
  filter(hbname != "Unknown")

# add on the num, den, measure_value metadata for the data download

everything_dataframe <- left_join(everything_dataframe, metadata, 
                                  by = c("measure", "measure_cat"))

saveRDS(everything_dataframe, paste0(data_path, "/", "everything_dataframe.rds"))

# calculate overall range of dates - need to review these

date_range_M <- as.Date(range(filter(everything_dataframe, period == "M")$date))

date_range_Q <- as.Date(range(filter(everything_dataframe, period == "Q")$date))

date_range <- range(everything_dataframe$date)

# create a vector for the chart labels to force first label to Jan-Mar 2017

x_date_labels_M <-
  seq(
    from = min(date_range_M),
    to = max(date_range_M),
    by = "2 months"
  )

x_date_labels_Q <-
  seq(
    from = min(date_range_Q),
    to = max(date_range_Q),
    by = "3 months"
  )

x_date_labels_Q2 <- 
  qtr(x_date_labels_Q, format = "short")

### 12b - Create annual data frame - this doesn't need runcharts etc. ----

annual_dataframe <- filter(everything_dataframe,
                           period %in% c("CY", "FY") &
                             date %in% factor_labels_year &
                             shown_on_MIO == "Y") %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE) %>%
  arrange(hbtype, hbname, MIO_measure_ref, period, date) %>% 
  group_by(MIO_measure_ref, measure, period, hbtype, date) %>% 
  mutate(
    measure_cat = case_match(measure,
                             "GESTATION AT BOOKING" ~ "average gestation at booking",
                             "GESTATION AT TERMINATION" ~ "average gestation at termination",
                             .default = measure_cat
                             ),
    measure_value = round(measure_value, 2),
    MIN = min(measure_value, na.rm = TRUE),
    MAX = max(measure_value, na.rm = TRUE),
    RANGE = MAX - MIN,
    RESCALED = round((measure_value - MIN) / RANGE, 2), # to stretch across entire x axis
    MIN_RS = min(RESCALED, na.rm = TRUE),
    MAX_RS = max(RESCALED, na.rm = TRUE),
    plotlylabel = sapply(MIO_measure_label, # wraps label text
                         FUN = function(x) {
                           paste(strwrap(x, width = 50), collapse = "<br>")})) %>%
  ungroup() %>% 
  select(c(dataset, measure, hbtype, hbname, period, date, measure_cat, num, den, measure_value, suffix,
           MIO_measure_ref, MIO_measure_label, MIN, MAX, RANGE, RESCALED, MIN_RS, MAX_RS,
           plotlylabel)
         )

saveRDS(annual_dataframe, paste0(data_path, "/", "annual_dataframe.rds"))

### 12c - Save monthly and quarterly data - this will be used for download and runcharts ----

remaining_dataframe <- filter(everything_dataframe,
                           period %in% c("Q", "M")) %>% 
  mutate(date = ymd(date)) %>%
  select("dataset", "hbtype", "hbname", "median_name", "period", "date", 
  "measure", "measure_cat", "num", "den", "measure_value", "suffix",
  "num_description", "den_description", "measure_value_description", "plotted_on_charts",
  "shown_on_MIO", "MIO_measure_label", "MIO_measure_ref") %>% 
  ungroup()

# drop incomplete quarter data

remaining_dataframe <- remaining_dataframe %>% 
  mutate(drop = period == "Q" & date > cut_off_date_Qtrly) %>% 
  filter(drop == FALSE) %>% 
  select(- drop)

# temporarily copy some data to check extended post-pandemic median 

# temp <- filter(remaining_dataframe, between(date, as.Date("2020-02-01"), as.Date("2020-08-01")) &
#                  period == "M" & measure %in% c("GESTATION AT BOOKING", "GESTATION AT TERMINATION")
# ) |> 
#   mutate(date = if_else(dataset == "ABC", 
#                         date %m+% months(52),
#                         date %m+% months(50)),
#          median_name = if_else(date <= "2024-06-01", 
#                                "post-pandemic median",
#                                NA)
#   )
# 
# remaining_dataframe <- bind_rows(remaining_dataframe, temp)

### 12d - Create run chart data frame ----

runchart_dataframe <- filter(remaining_dataframe, measure_cat %in% runchart_categories) %>% 
  select(- c("num_description", "den_description", "measure_value_description", "plotted_on_charts", contains("MIO")))

# set median_name as a factor to keep order

runchart_dataframe$median_name <- factor(runchart_dataframe$median_name,
                      levels = c("pre-pandemic median", "revised median", "post-pandemic median"),
                      labels = c("pre-pandemic median", # to Oct-Dec 2019 / to end Feb 2020
                                 "revised median", # FV/TAY (Gestation at booking)
                                 "post-pandemic median") # from Jul 2022 to end Jun 2024
                      ) 

### i - MEDIAN of measure_value ----

# calculate the MEDIAN of the measure_value variable over the relevant median_name - plotted as a solid line

runchart_dataframe <- calculate_medians(dataset = runchart_dataframe,
                                        measure_value = measure_value)

### ii - Mark SHIFTS and TRENDS ----

# compares measure_value with extended to determine shifts
# compares consecutive measure_values to determine trends

runchart_dataframe <- runchart_flags(
  dataset = runchart_dataframe,
  shift = "orig_shift",
  trend = "orig_trend",
  value = measure_value,
  median = extended)

# Set up data for "trend" and "shift" traces
# We don't want to use this data to plot anything that is not part of a
# trend or shift, so just set FALSE-flagged data to NA

runchart_dataframe <- runchart_dataframe %>% 
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
  split_col_prefix = "orig_trend") %>% 
  rename(., c("trend_num_rows" = "num_rows", "trend_dup_row" = "dup_row"))

runchart_dataframe <- add_split_gaps(
  dataset = runchart_dataframe,
  measure = "shift",
  split_col_prefix = "orig_shift") %>% 
  rename(., c("shift_num_rows" = "num_rows", "shift_dup_row" = "dup_row"))

# reset "extended" values to NA where "median" values exist (bar last "median" value)
# stops "extended" over plotting "median" which looks rubbish

runchart_dataframe <- runchart_dataframe |>
  group_by(dataset, hbtype, hbname, period, measure, measure_cat, median_name) |> 
  mutate(extended = if_else(
    !is.na(median) & !is.na(extended) & is.na(lead(median)),
    median, NA),
    extended = na.locf(extended, na.rm = FALSE),
    extended = if_else(median_name == "post-pandemic median" & date < as.Date("2024-06-01"),
                       NA,
                       extended)
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

medians <- runchart_dataframe %>% 
  ungroup() %>% 
  select(dataset:measure_cat, pre_pandemic_median:extended_post_pandemic_median) |> 
  mutate(across(c(pre_pandemic_median:extended_post_pandemic_median), ~ round(., 2))) |> 
  distinct() # removes duplicates created to split shifts and trends

### iii - Tidy up ----

# add quarter_label and round values

runchart_dataframe <- runchart_dataframe %>%
  mutate(quarter_label = if_else(period == "Q",
                                 qtr(ymd(date), format = "short"),
                                 NA),
         quarter_label = factor(quarter_label,
                                levels = x_date_labels_Q2,
                                ordered = TRUE),
         across(c(measure_value, pre_pandemic_median:extended_post_pandemic_median, trend, shift), ~ round(., 3)),
         num = if_else(measure %in% c("BOOKINGS", "TERMINATIONS"), NA, num)
  ) %>%
  select(dataset, measure, hbtype, hbname, period, date, quarter_label, measure_cat:suffix, pre_pandemic_median:extended_post_pandemic_median, trend, shift, 
         ) %>% 
  ungroup()

saveRDS(runchart_dataframe, paste0(data_path, "/", "runchart_dataframe.rds"))

### 12e - Create download data frame ----

download_dataframe <- left_join(
  select(remaining_dataframe,
         - "median_name"),
  medians) %>%  # copy median etc to grouped records
  mutate(date_label = 
           if_else(period == "Q",
                   qtr(ymd(date), format = "short"),
                   format(date, "%b %Y")),
         measure_value = round(measure_value, 3),
         #hbname = str_remove(hbname, "[*]")
         ) %>% 
  arrange(dataset, measure, hbtype, hbname, period, date, date_label) |> 
  select(dataset, measure, hbtype, hbname, period, date, date_label, measure_cat, num, den, measure_value,  suffix, plotted_on_charts, pre_pandemic_median:extended_post_pandemic_median, shown_on_MIO)

download_dataframe <- download_dataframe %>% 
  split(.$measure) 

# remove empty columns

for (i in seq_along(download_dataframe)) {

  download_dataframe[[i]] <-
    janitor::remove_empty(download_dataframe[[i]], which = c("cols"), quiet = TRUE)
}

# sort GESTATION AT BIRTH in ascending measure_cat order

download_dataframe[["GESTATION AT BIRTH"]]$measure_cat <-
  factor(download_dataframe[["GESTATION AT BIRTH"]]$measure_cat,
         levels = c("under 32 weeks",
                    "between 32 and 36 weeks (inclusive)",
                    "under 37 weeks",
                    "between 37 and 41 weeks (inclusive)",
                    "42 weeks and over (inclusive)",
                    "between 18 and 44 weeks (inclusive)",
                    "unknown gestation",
                    "total")
  )

download_dataframe[["GESTATION AT BIRTH"]] <-
  arrange(download_dataframe[["GESTATION AT BIRTH"]],
          dataset, measure, hbtype, hbname, period, date, date_label, measure_cat)

saveRDS(download_dataframe, paste0(data_path, "/", "download_dataframe.rds"))

### 13 - Save data for SPBAND ----

save(annual_dataframe, 
     runchart_dataframe,
     factor_labels_year,
  file = paste0(dashboard_dataframes_folder, "/SMR02-ABC-Terminations.RData")
)

### - END OF SCRIPT ----
