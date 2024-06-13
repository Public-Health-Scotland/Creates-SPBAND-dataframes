# Code to remove gestation breakdowns from TERMINATIONS download

# load 15 Dec 2023 published 16 Jan 2024 SMR02-ABC-TERMINATIONS.RData - loads annual_dataframe, download_dataframe and runchart_dataframe

load("data/2023-12-15 extract/dashboard_dataframes/SMR02-ABC-Terminations.RData")

# split out latest download data into the separate measure data using the function borrowed from dashboard code

builds_download_data <- function(measure) {
  
  downloaddata <- download_dataframe[[{{measure}}]] 
    
  return(downloaddata)
}

bookings <- builds_download_data("BOOKINGS")
av_gestation_booking <- builds_download_data("GESTATION AT BOOKING")
av_gestation_termination <- builds_download_data("GESTATION AT TERMINATION")
terminations <- builds_download_data("TERMINATIONS")
inductions <- builds_download_data("INDUCTIONS")
type_of_birth <- builds_download_data("TYPE OF BIRTH")
tears <- builds_download_data("TEARS")
gestation <- builds_download_data("GESTATION AT BIRTH")
apgar5 <- builds_download_data("APGAR5")

# do a quick check to see number of rows and columns agrees with download_data structure

View(download_dataframe)

# check terminations data 

View(terminations)

str(terminations)

# remove gestation breakdowns from terminations download data

terminations <- filter(terminations, measure_cat == "all terminations")

View(terminations)

str(terminations)

download_dataframe2 <- bind_rows(bookings, av_gestation_booking, av_gestation_termination, terminations,
                                 inductions, type_of_birth, tears, gestation, apgar5)

download_dataframe2 <- download_dataframe2 %>% 
  split(.$measure) 

# check download_dataframe2 looks right

View(download_dataframe2)

# remove empty columns

for (i in seq_along(download_dataframe2)) {
  
  download_dataframe2[[i]] <- 
    janitor::remove_empty(download_dataframe2[[i]], which = c("cols"), quiet = TRUE)
}

View(download_dataframe2)

download_dataframe <- download_dataframe2

### 13 - Save data for SPBAND ----

factor_labels_year <- c("2022", "2021", "2020", "2019", "2018", "2017",
                        "2022/23", "2021/22", "2020/21", "2019/20", "2018/19", "2017/18"
                        )

save(annual_dataframe, 
     download_dataframe,
     runchart_dataframe,
     factor_labels_year,
  file = paste0(dashboard_dataframes_folder, "/SMR02-ABC-Terminations-revised.RData")
)

rm(list = ls())

# final check

load("data/2023-12-15 extract/dashboard_dataframes/SMR02-ABC-Terminations-revised.RData")

View(download_dataframe)

