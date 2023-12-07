####
# Administrative code to produce the data files for the 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Bev Dodds
# 12 July 2023
# Last update: 19 September 2023
# Last update by: Bev Dodds
# Latest update description: September refresh for October first public release
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

Sys.umask("002") # ensures new folders are created with the correct group permissions

# loading packages

source("code/packages.R")

# define functions

source("code/functions.R")

# set refresh date (based on when this code is run rather than when the files were created)

refresh_date <- as.Date("2023-12-07") # change this each time the data is updated

# set cut-off date - what month are we happy to publish to?

cut_off_date <- ymd("2023-06-01") # month beginning
cut_off_date_ABC <- ymd("2023-08-01") # month beginning (ABC more timely than SMR02)
cut_off_date_Qtrly <- ymd("2023-04-01") # quarter beginning (most complete) e.g. Jan-Mar

# metadata file - for num, den, measure_value descriptions

metadata <- read.xlsx("data/basefiles/measure metadata.xlsx", sheet = 1) %>% 
  select(-c("key_measure_cat", "key_measure_ref", "key_measure_label")
         ) # for use after refresh in January 2023

# Maternity team's SMR02 location and filename

SMR02_filename <- "/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Data/R_smr02ext97on.rds"

file.exists(SMR02_filename)

# ABC base location and filename

ABC_filename <- "/PHI_conf/MaternityBirths/Topics/AntenatalBooking/Analysis/Analysis Files/abc_base.rds"

file.exists(ABC_filename)

# update Terminations data loction and filename here

terminations_filename <- 
  "//PHI_conf/SexualHealth/Topics/Terminations/Projects/20200505-Covid19/Analysis/AAS-2017-onwards-covid-20230914.rds"

file.exists(terminations_filename)

# update file name that contains NRS quarterly data (published)

NRS_filename <- "./data/basefiles/NRS/Births deaths and other vital events - 2023 Q2 - Table Q1.xlsx"

file.exists(NRS_filename)

# set local output folder for data - dated automatically

data_path <- paste0("./data/output/", refresh_date, " extract")

# create this folder if it doesn't already exist

if (!dir.exists(data_path)) {
  dir.create(data_path, showWarnings = TRUE, recursive = FALSE, mode = "2770")
}

# create the dashboard_dataframes folder

dashboard_dataframes_folder <- paste0(data_path, "/dashboard_dataframes")

# create this folder if it doesn't already exist

if (!dir.exists(dashboard_dataframes_folder)) {
  dir.create(dashboard_dataframes_folder, showWarnings = TRUE, recursive = FALSE, mode = "2770")
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

