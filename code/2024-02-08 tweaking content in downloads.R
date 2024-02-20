####
# Code to update the download data that was published on 16th January 2024 with data run on 15th Dec 2023
# To match the tweaks to wording on the dashboard (titles etc.)
# Most affected measures are Apgar5 scores and Gestation at birth
# Bev Dodds
# 08 February 2024
# Last update by Bev Dodds
# Latest update description: created code
# Type of script - amendment of existing dataframes
# Written/run on R Studio Server
# Version of R - 4.1.2 - note use of dplyr 1.1.0
# Approximate run time - < 1 minute
####

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

refresh_date <- as.Date("2024-02-20") # change this each time the data is updated

# set local output folder for data - dated automatically

data_path <- paste0("./data/", refresh_date, " extract")

# create this folder if it doesn't already exist

if (!dir.exists(data_path)) {
  dir.create(data_path, showWarnings = TRUE, recursive = TRUE, mode = "2770")
}

# create the dashboard_dataframes folder

dashboard_dataframes_folder <- paste0(data_path, "/dashboard_dataframes")

# create this folder if it doesn't already exist

if (!dir.exists(dashboard_dataframes_folder)) {
  dir.create(dashboard_dataframes_folder, showWarnings = TRUE, recursive = TRUE, mode = "2770")
}

metadata <- read.xlsx("data/measure metadata_old.xlsx", sheet = 1) 

# create a vector with "complete" years (for multi indicator overview)
# update as necessary

factor_labels_year <- c("2022", "2021", "2020", "2019", "2018", "2017",
                        "2022/23", "2021/22", "2020/21", "2019/20", "2018/19", "2017/18"
                        )

# create a vector containing "measure_cat" that will have a timeseries or runchart

runchart_categories <- c("induced", "low (<7) apgar5 scores", "3rd or 4th degree tears",
                          "spontaneous vaginal births", "assisted births",
                          "all caesarean births", "planned caesarean births",
                          "unplanned caesarean births", "under 32 weeks",
                          "between 32 and 36 weeks", "under 37 weeks", "between 37 and 41 weeks",
                          "42 weeks and over", "between 18 and 44 weeks",
                          "all pregnancies booked", "all terminations", "average gestation")

# create a vector of Island Boards to remove them from the outputs if necessary

island_boards <- c("NHS Orkney", "NHS Western Isles", "NHS Shetland")

### 2 - Read in data to be amended ----

load("data/2023-12-15 extract/dashboard_dataframes/SMR02-ABC-Terminations-revised.RData")

### 2a - annual data frame ----

published_Jan_24_annual_dataframe <- annual_dataframe %>% 
  mutate(measure_cat = if_else(
    measure_cat == "low apgar5 scores", "low (<7) apgar5 score", measure_cat)
  )

published_Jan_24_annual_dataframe <- 
  published_Jan_24_annual_dataframe %>% 
  select(-c(contains("key"), plotlylabel)) # drops off existing key_measure_label, plotlylabel

published_Jan_24_annual_dataframe <- 
  left_join(published_Jan_24_annual_dataframe, metadata, 
            by = c("measure", "measure_cat")) %>% 
  select(c(dataset:suffix, key_measure_ref, key_measure_label, MIN:MAX_RS)) %>% 
  mutate(plotlylabel = sapply(key_measure_label, # wraps label text
                              FUN = function(x) {
                                paste(strwrap(x, width = 50), collapse = "<br>")})
         )

saveRDS(published_Jan_24_annual_dataframe, paste0(data_path, "/", "annual_dataframe.rds"))

### 2b - runchart data frame ----

published_Jan_24_runchart_dataframe <- runchart_dataframe %>% 
  mutate(measure_cat = if_else(
    measure_cat == "low apgar5 scores", "low (<7) apgar5 score", measure_cat)
  )

saveRDS(published_Jan_24_runchart_dataframe, paste0(data_path, "/", "runchart_dataframe.rds"))

### 2c - download data frame ----

published_Jan_24_download_dataframe <- download_dataframe 

published_Jan_24_download_dataframe[["INDUCTIONS"]] <-  published_Jan_24_download_dataframe[["INDUCTIONS"]] %>% 
    mutate(measure_cat = case_when(
    measure_cat == "unknown if induced" ~ "unknown whether induced",
    .default = measure_cat)
  )

published_Jan_24_download_dataframe[["APGAR5"]] <- 
  published_Jan_24_download_dataframe[["APGAR5"]] %>% 
    mutate(measure_cat = case_when(
    measure_cat == "low apgar5 scores" ~ "low (<7) apgar5 score",
    measure_cat == "desirable apgar5 scores" ~ "apgar5 score of 7 or more",
    .default = measure_cat)
  )

published_Jan_24_download_dataframe[["GESTATION AT BIRTH"]] <- published_Jan_24_download_dataframe[["GESTATION AT BIRTH"]] %>% 
  filter(measure_cat != "between 37 and 42 weeks")

# sort GESTATION AT BIRTH in ascending measure_cat order

published_Jan_24_download_dataframe[["GESTATION AT BIRTH"]] <- published_Jan_24_download_dataframe[["GESTATION AT BIRTH"]] %>% 
  mutate(measure_cat = factor(measure_cat,
         levels = c("under 32 weeks",
                    "between 32 and 36 weeks",
                    "under 37 weeks",
                    "between 37 and 41 weeks",
                    #"between 37 and 42 weeks",
                    "42 weeks and over",
                    "between 18 and 44 weeks",
                    "unknown gestation",
                    "total")
         )
  ) %>% 
  arrange(.,
          dataset, measure, hbtype, hbname, period, date, measure_cat)

for (i in c(1, 3, 6, 7, 9)) {
  
  published_Jan_24_download_dataframe[[i]] <- 
    select(published_Jan_24_download_dataframe[[i]], -c("key_measure_label", contains("description"))) %>%
  left_join(., metadata,
            by = c("measure", "measure_cat")) %>%
  select(dataset:extended, measure_value_description, suffix, key_measure_label, num, den, num_description, den_description)
}

saveRDS(published_Jan_24_download_dataframe, paste0(data_path, "/", "download_dataframe.rds"))

### 3 - Save data for SPBAND ----

annual_dataframe <- readRDS(paste0(data_path, "/", "annual_dataframe.rds"))
download_dataframe <- readRDS(paste0(data_path, "/", "download_dataframe.rds"))
runchart_dataframe <- readRDS(paste0(data_path, "/", "runchart_dataframe.rds"))

save(annual_dataframe, 
     download_dataframe,
     runchart_dataframe,
     factor_labels_year,
  file = paste0(dashboard_dataframes_folder, "/SMR02-ABC-Terminations.RData")
)

# once the data has been checked in the PRA dashboard it should be copied to the SPBAND data folder 
# as the published dashboard is a self-contained entity

### - END OF SCRIPT ----

## END OF SCRIPT
