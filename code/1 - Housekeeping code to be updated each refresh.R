####
# Administrative code to produce the data files for the 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Bev Dodds
# 12 July 2023
# Last update: 07 May 2024
# Last update by: Bev Dodds
# Latest update description: Amended code to deal with grouped Island Boards in average gestation at termination measure
# Type of script - preparation
# Written/run on Posit Workbench
# Version of R - 4.1.2
# Initialises packages to be used, date parameters and file locations
# Will be called in relevant data creation code
# # Approximate run time - <1 minute
####

### Housekeeping ----
# This section should be the only section of the script which requires manual changes 
# for future updates and includes:

library(here)

Sys.umask("002") # ensures new folders are created with the correct group permissions

# loading packages

# source("code/packages.R")

source(here("code", "packages.R"))

# define functions

# source("code/functions.R")

source(here("code", "functions.R"))

# set refresh date (based on when this code is run rather than when the files were created)


refresh_date <- as.Date("2024-09-05") # change this each time the data is updated

# set cut-off date - what month are we happy to publish to?

cut_off_date <- ymd("2024-06-01") # month beginning, usually increments by 3 months
cut_off_date_ABC <- ymd("2024-08-01") # month beginning (ABC more timely than SMR02)
cut_off_date_Qtrly <- ymd("2024-04-01") # quarter beginning (most complete) e.g. Jan-Mar, usually increments by 1 quarter

# metadata file - for num, den, measure_value descriptions

metadata <- read.xlsx(here("../basefiles/measure metadata.xlsx"), sheet = 1) 

# Maternity team's SMR02 location and filename

SMR02_filename <- "/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Data/R_smr02ext97on.rds"

file.exists(SMR02_filename)

# ABC base location and filename

ABC_filename <- "/PHI_conf/MaternityBirths/Topics/AntenatalBooking/Analysis/Analysis Files/abc_base.rds"

file.exists(ABC_filename)

# update Terminations data loction and filename here

terminations_filename <- "../basefiles/Terminations/topss_data_extract.rds"

file.exists(terminations_filename)

# update file name that contains NRS quarterly data (published)

NRS_filename <- "../basefiles/NRS/Births deaths and other vital events - 2024 Q2 - Table Q1.xlsx"

file.exists(NRS_filename)

# create a vector with "complete" years (for multi indicator overview)
# update as necessary

factor_labels_year <- c("2023", "2022", "2021", "2020", "2019", "2018", "2017",
                        "2023/24", "2022/23", "2021/22", "2020/21", "2019/20", "2018/19", "2017/18"
                        )

# create a vector containing "measure_cat" that will have a timeseries or runchart

runchart_categories <- c("induced", "low (<7) apgar5 score", "3rd or 4th degree tears",
                          "spontaneous vaginal births", "assisted vaginal births",
                          "all caesarean births", "planned caesarean births",
                          "unplanned caesarean births", "under 32 weeks",
                          "between 32 and 36 weeks (inclusive)", "under 37 weeks", "between 37 and 41 weeks (inclusive)",
                          "42 weeks and over (inclusive)", "between 18 and 44 weeks (inclusive)",
                          "all pregnancies booked", "all terminations", "average gestation")

# create a vector of Island Boards to remove them from the outputs if necessary

island_boards <- c("NHS Orkney", "NHS Western Isles", "NHS Shetland")

# read in HBNAME cipher names (for HBNAME of Treatment) in WI dashboard format,
# also for HBNAME labels

hbcipher <- read.csv(here("../basefiles/hb14_hb19.csv"), stringsAsFactors = FALSE) %>% 
  filter(is.na(HBDateArchived)) %>% # want the latest NHS Board codes
  select(HBCIPHER, HBCODE, HBNAME, HBLABEL)

# set local output folder for data - dated automatically

data_path <- here(paste0("data/", refresh_date, " extract"))

# create this folder if it doesn't already exist

if (!dir.exists(data_path)) {
  dir.create(data_path, showWarnings = TRUE, recursive = TRUE, mode = "2770")
}

# create the dashboard_dataframes folder

dashboard_dataframes_folder <- here(paste0(data_path, "/dashboard_dataframes"))

# create this folder if it doesn't already exist

if (!dir.exists(dashboard_dataframes_folder)) {
  dir.create(dashboard_dataframes_folder, showWarnings = TRUE, recursive = TRUE, mode = "2770")
}

# map the Excel templates folder (for accessible downloads)

excel_templates_folder <- here("../basefiles/excel templates/")

# create the Excel downloads folder

excel_downloads_folder <- here(paste0(dashboard_dataframes_folder, "/excel downloads"))

# create this folder if it doesn't already exist

if (!dir.exists(excel_downloads_folder)) {
  dir.create(excel_downloads_folder, showWarnings = TRUE, recursive = TRUE, mode = "2770")
}

# sets colour palette to the PHS colour scheme

selected_colours <-
  as.character(c(phs_colours()[1:8],
                 phs_colours(str_subset(
                   names(phs_colours()), "-80"
                 ))))

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

## - END OF SCRIPT ----

