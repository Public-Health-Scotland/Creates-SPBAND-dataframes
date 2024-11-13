###
# Location of extremely pre-term births, 22-26 weeks inclusive for the
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from SMRA SMR02
# Bev Dodds
# 13 November 2024
# Last update by Bev Dodds
# Latest update description: removed admission reason = 26 (transfer after delivery in another hospital)
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on Posit Workbench
# Version of R - 4.1.2
# Reads in SMRA SMR02_PI live births and produces a variety of tables/charts 
# Approximate run time - <5 minutes
###

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

library(odbc) # required to read in SMRA data

# set up odbc connection to query SMRA

smra_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = rstudioapi::showPrompt(title = "Username", message = "Username:", default = "beverd01"),
    pwd = .rs.askForPassword("SMRA Password:"),
    port = "1527",
    host = "nssstats01.csa.scot.nhs.uk",
    SVC = "SMRA.nss.scot.nhs.uk"))

### 2 - Read in source data from SMRA SMR02_PI ----

# Select records where the mother was discharged on or after 1 Jan 2018, 
# at least one baby was delivered (CONDITION_ON_DISCHARGE = 3),
# with an estimated gestation between 22 and 26 weeks (inclusive) - note difference from WI dashboard
# have added location = "D201N" - home births (although these tend to be term babies anyway) ### stops query in it's tracks
# there are no discharge dates for home births so would need to use delivery date

preterm <- as_tibble (dbGetQuery(smra_connect, statement = "SELECT 
                            DATE_OF_DELIVERY, DISCHARGE_DATE, ESTIMATED_GESTATION, LOCATION, 
                            CONDITION_ON_DISCHARGE, NUM_OF_BIRTHS_THIS_PREGNANCY,  OUTCOME_OF_PREGNANCY_BABY_1,
                            OUTCOME_OF_PREGNANCY_BABY_2, OUTCOME_OF_PREGNANCY_BABY_3,
                            ADMISSION_REASON, ADMISSION_TRANSFER_FROM,
                            ADMISSION_TYPE, DISCHARGE_TYPE, DISCHARGE_TRANSFER_TO 
                            FROM ANALYSIS.SMR02_PI
                            WHERE (DISCHARGE_DATE >= To_date('2018-01-01', 'YYYY-MM-DD') AND
                                  CONDITION_ON_DISCHARGE = 3 AND
                                    ESTIMATED_GESTATION BETWEEN 22 and 26)
                            OR 
                            (LOCATION = 'D201N' AND DATE_OF_DELIVERY >= To_date('2018-01-01', 'YYYY-MM-DD') AND
                              CONDITION_ON_DISCHARGE = 3 AND
                              ESTIMATED_GESTATION BETWEEN 22 and 26)"))

names(preterm) <- to_snake_case(names(preterm)) # convert variable names to snake_case

# Home births (very unlikely at 22-26 weeks) do not have a discharge date. Recode discharge date = 
# date of delivery.

preterm <- preterm %>% 
  mutate(discharge_date = if_else(location == 'D201N', date_of_delivery, discharge_date))

# select deliveries where at least one baby was born alive 
# (OUTCOME_OF_PREGNANCY_BABY_1,2,3 in (1, 3, 4, 5)) (2 = still born)

preterm <- preterm %>% 
mutate(date = ymd(discharge_date),
       date = as.Date(as.yearqtr(date)), # first day of quarter (e.g. 2020-01-01, 2020-04-01)
       date_label = qtr(date, format = "short"), # e.g. Jan-Mar 2020
       period = "Q",
       dataset = "SMR02",
       hbtype = "TREATMENT",
       hbname = "Scotland",
       measure = "EXTREMELY PRE-TERM BIRTHS"
       ) %>% 
  filter(date <= cut_off_date_Qtrly & admission_reason != 26 &
           (outcome_of_pregnancy_baby_1 %in% c(1, 3, 4, 5) |
              outcome_of_pregnancy_baby_2 %in% c(1, 3, 4, 5) |
              outcome_of_pregnancy_baby_3 %in% c(1, 3, 4, 5))
         ) %>% 
  arrange(date)

# flag NICU units

preterm <- preterm %>% 
  mutate(NICU = case_when(
    location == "A111H" & discharge_date < "2019-10-01" ~ 1,
    location == "F704H" & discharge_date < "2019-10-01" ~ 1, 
    location == "F705H" & discharge_date < "2019-10-01" ~ 1, 
    location == "G515H" & discharge_date < "2019-10-01" ~ 1, 
    location == "G108H" ~ 1, 
    location == "G405H" ~ 1,
    location == "G513H" ~ 1,
    location == "L308H" ~ 1,
    location == "N101H" ~ 1,
    location == "N161H" ~ 1, 
    location == "S314H" ~ 1, 
    location == "T101H" ~ 1,
    TRUE ~ 0)
  )

# calculate numerator - mothers with extremely pre-term deliveries in a location with a NICU who 
# were not transferred in having given birth at home or transferred after delivery in another hospital 
# i.e. ADMISSION_TYPE != 41, ADMISSION_TRANSFER_FROM != 70, DISCHARGE_TYPE != 70, DISCHARGE_TRANSFER_TO != 70
# AND who were not admitted having delivered elsewhere 
# i.e. ADMISSION_REASON != 20 (not admitted - home birth), 24 (born before arrival), 25 (admitted after home birth), # as of Nov 2024 removed 26 (admitted after delivery in another hospital)
# ADMISSION_TRANSFER_FROM_LOC is not used as it is not a mandatory item and so not populated
# calculate denominator - all deliveries at between 22-26 weeks gestation

preterm <- preterm %>% 
  mutate(
    NICU_22_26 = 1,
    excluded_from_NICU_22_26 = 
      if_else(admission_reason %in% c("20", "24", "25") | # removed "26" (transfer where birth occurred in another hospital) as of January 2025 publication
                admission_type == "41" | 
                admission_transfer_from == "70" |
                discharge_type == "70" |
                discharge_transfer_to == "70" |
                NICU == 0,
              1, 0),
    NICU_22_26 = NICU_22_26 - excluded_from_NICU_22_26,
    all_22_26 = 1
  )

preterm$date_label = factor(preterm$date_label,
                            levels = unique(preterm$date_label),
                            ordered = TRUE)

# useful if trying things out - no risk of refreshed data 

saveRDS(preterm, paste0(data_path, "/", "raw_preterm.rds"))  

preterm <- readRDS(paste0(data_path, "/", "raw_preterm.rds"))

### 3 - Wrangle SMR02 data ----

# sum up to quarterly periods, create total and den

extremely_preterm_data <- preterm %>% 
  group_by(dataset, hbtype, hbname, period, date, date_label, measure) %>% 
  summarise(NICU_22_26 = sum(NICU_22_26),
            excluded_from_NICU_22_26 = sum(excluded_from_NICU_22_26),
            total = sum(all_22_26),
            den = total
  ) %>% 
  ungroup()

# make date_label a factor to ensure date order is correct

extremely_preterm_data$date_label <- factor(extremely_preterm_data$date_label,
                                               levels = extremely_preterm_data$date_label,
                                               ordered = TRUE)

# pivot longer to make measure_cat and num variables - in keeping with other measures
# reset den to NA for the total, calculates measure_value, i.e. percentage
  
extremely_preterm_data <- extremely_preterm_data %>%
  pivot_longer(cols = c(NICU_22_26:total), names_to = "measure_cat", values_to = "num") %>%
  mutate(den = if_else(measure_cat != "total", den, NA),
         measure_value = percentage(num, den)
  )

# calculate centreline (mean), standard deviation, upper and lower warning and control limits for the percentage
# 22-26 weeks born in a hospital with a nicu unit, then drop standard deviation

temp <- filter(extremely_preterm_data,
               measure_cat == "NICU_22_26") %>%
  group_by(dataset, hbtype, hbname, period, measure) %>%
  mutate(centreline = sum(num)/sum(den) * 100,
         sd = sqrt(centreline * (100 - centreline) / den),
         lower_warning_limit = if_else(centreline - (sd * 2) < 0, 0,
                                       centreline - (sd * 2)),
         upper_warning_limit = if_else(centreline + (sd * 2) > 100, 100,
                                       centreline + (sd * 2)),
         lower_control_limit = if_else(centreline - (sd * 3) < 0, 0,
                                       centreline - (sd * 3)),
         upper_control_limit = if_else(centreline + (sd * 3) > 100, 100,
                                       centreline + (sd * 3))
         )

# join centreline etc back onto the main preterm_Q data file, round values

extremely_preterm_data <- left_join(extremely_preterm_data, temp,
                                    by = join_by(dataset, hbtype, hbname, period, date, date_label,
                                                 measure, measure_cat, num, den, measure_value)
                                    ) %>%
  select(- sd) %>% 
  mutate(across(c(centreline:upper_control_limit), ~ round(., 2)),
         measure_value = round(measure_value, 3)
  )
  

rm(temp) # tidy up

# add on suffix and metadata for num, den, measure

extremely_preterm_data <- left_join(extremely_preterm_data, metadata, 
                                  by = c("measure", "measure_cat")) %>% 
  mutate(suffix = "%") %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE) %>% 
  select(dataset, measure, hbtype:date_label, measure_cat, num, den, measure_value, plotted_on_charts, suffix, centreline:upper_control_limit,
         shown_on_MIO, contains("description")
         )
  
### 4 - Save extremely_preterm_data for use in dashboard ----

saveRDS(extremely_preterm_data,
        paste0(dashboard_dataframes_folder, "/extremely-preterm-data.rds")
        )

# if wanting to load again for testing

extremely_preterm_data <- readRDS(paste0(dashboard_dataframes_folder, "/extremely-preterm-data.rds")
                                  )
### 5 - Testing charts look OK ----

server_folder <- "https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/refs/heads/new_neonatal_measures/"

source(paste0(server_folder,
                "functions.R"), local = FALSE)

# tells plotly where to place x-axis tick marks and labels

SMR02_date_range <- unique(extremely_preterm_data$date)
SMR02_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 2)]
SMR02_date_ticktext <- qtr(SMR02_date_tickvals, format = "short")

# 5i - control chart ----

# plot percentage of deliveries in nicu by quarter, with mean, LCL, UCL, LWL and UWL

# a) data ----

# pivot data to fit plotly preference, create groupname to prevent legends appearing twice

extremely_preterm_control_chart_data <- filter(extremely_preterm_data,
                                               measure_cat== "NICU_22_26"
                                               ) 

# b) chart ---- 

extremely_preterm_control_chart <- 

  plot_ly(
    data = extremely_preterm_control_chart_data,
    x = ~ date_label,
    y = ~ measure_value, # percentage
    type = "scatter",
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
                         "Percentage",
                         ": ",
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
    hoverinfo = "none"
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
  layout(xaxis = orig_xaxis_plots,
         yaxis = orig_yaxis_plots)

extremely_preterm_control_chart <- extremely_preterm_control_chart %>% 
    layout(
      legend = list(orientation = "v",
                    groupclick = "togglegroup"
                    )
      ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

extremely_preterm_control_chart

# 5ii - context chart ----

# a) data ----

extremely_preterm_context_data <- 
  
  extremely_preterm_data %>% 
  filter(measure_cat== "NICU_22_26"
         ) %>% 
  set_variable_labels(
    num = "Births at 22-26 weeks in a hospital with a NICU",
    den = "All births at 22-26 weeks"
  ) %>% 
  mutate(mytext1 = paste0("Quarter: ", 
                         date_label,
                         "<br>",
                         var_label(num), ": ", prettyNum(num, big.mark = ",")),
         mytext2 = paste0("Quarter: ", 
                         date_label,
                         "<br>",
                         var_label(den), ": ", prettyNum(den, big.mark = ","))
         )

# b) chart ---- 

extremely_preterm_context_chart <- 
  
  creates_context_charts(plotdata = extremely_preterm_context_data,
                         date = "date",
                         num = "num",
                         den = "den"
                    )

extremely_preterm_context_chart 


# 6 ---- extract details of the location of non-NICU births (and admission/discharge info)

preterm <- readRDS(paste0(data_path, "/", "raw_preterm.rds"))

# temporary - should be saved in next version #

preterm$date_label = factor(preterm$date_label,
                            levels = unique(preterm$date_label),
                            ordered = TRUE)

non_NICU_preterm <- preterm %>% 
  filter(excluded_from_NICU_22_26 == 1) %>% 
  select(c("date_of_delivery", "discharge_date", "estimated_gestation", "location",
           "condition_on_discharge", "num_of_births_this_pregnancy", 
           "outcome_of_pregnancy_baby_1", "outcome_of_pregnancy_baby_2", 
           "outcome_of_pregnancy_baby_3", "admission_reason", "admission_transfer_from", 
           "admission_type", "discharge_type", "discharge_transfer_to",
           "date", "date_label", "NICU", "NICU_22_26", "excluded_from_NICU_22_26", 
           "all_22_26"))

write.csv(non_NICU_preterm, row.names = FALSE,
          file.path(data_path, paste0("non-NICU births at 22-26 weeks to ", 
                                   max(non_NICU_preterm$date_label), ".csv"))
          )

summary <- preterm %>% 
  group_by(date_label, admission_reason) %>%
  pivot_wider(names_from = admission_reason,
              values_from = all_22_26,
              values_fill = 0) %>% 
  reframe(`Born at NICU` = sum(NICU_22_26),
          `24 (Born before arrival)` = sum(`24`),
          `25 (Born at home)` = sum(`25`),
          #`26 (Admitted after delivery in another hospital)` = sum(`26`),
          `All births at 22-26 weeks` = n()
  )

write.csv(summary, row.names = FALSE,
          file.path(data_path, paste0("summary of births at 22-26 weeks to ", 
                                   max(summary$date_label), ".csv"))
          )

### - END OF SCRIPT ----

