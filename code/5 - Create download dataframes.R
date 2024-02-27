####
# Pulls together all the download data for the  
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# and reformats it using pre-built Excel templates 
# Bev Dodds
# 24 January 2024
# Last update by Bev Dodds
# Latest update description: initialised code
# Type of script - preparation and data extraction for dashboards
# Written/run on R Studio Server
# Version of R - 4.2.1
# Reads pre-created download data from 2, 3 and 4 and
# produces Excel spreadsheets in accessible format as well as csv versions
# Approximate run time - <5 minutes
####

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

# Read in the data to be reformatted

download_dataframe <- readRDS(
  paste0(data_path, "/download_dataframe.rds"))

annual_dataframe <- readRDS(
  paste0(data_path, "/annual_dataframe.rds"))

download_dataframe <- list_assign(download_dataframe,
                                   "STILLBIRTHS AND INFANT DEATHS" = readRDS(paste0(dashboard_dataframes_folder, "/stillbirths-infant-deaths-data.rds")),
                                   "EXTREMELY PRETERM" = readRDS(paste0(dashboard_dataframes_folder, "/extremely-preterm-data.rds")),
                                   "MULTI INDICATOR OVERVIEW" = annual_dataframe)

# Make each dataset presentable ----

# this function will change each variable name in the download_dataframe into a presentable form

tidy_data_download <- function(measure_selection) {
  data <- download_dataframe[[measure_selection]]
  
  # modify entries in `Board of` and Measure columns
  data <- data %>% 
    mutate(`Board of` = str_to_title(hbtype),
           Measure = case_when(
             measure == "BOOKINGS" ~ "Number of pregnancies booked",
             measure == "TERMINATIONS" ~ "Number of terminations",
             measure == "INDUCTIONS" ~ "Induction of labour",
             measure == "TEARS" ~ "Third- and fourth-degree perineal tears",
             measure == "GESTATION AT BIRTH" ~ "Gestation at birth: pre- and post- term births",
             measure == "APGAR5" ~ "Apgar scores",
             measure == "EXTREMELY PRETERM" ~ "Location of extremely pre-term births", 
             TRUE ~ str_to_sentence(measure)),
           .keep = "unused") %>% 
    
    # rename the columns that are present in all datasets
    
    #     data <- data %>% 
    
    rename(Dataset = dataset,
           `Health Board` = hbname,
           Period = period,
           Date = date,
           `Measure value`  = measure_value,
           `Sub-category` = measure_cat
    ) %>%
    
    # delete unused columns
    
    #         data <- data %>% 
    
    select(-any_of(c("num_description", "den_description", "measure_value_description", "quarter_label", "use_for_mean", "measure_cat2", "MIO_measure_label", "MIO_measure_ref", "MIN", "MAX", "RANGE", "RESCALED", "MIN_RS", "MAX_RS", "plotlylabel")
    )
    )
  
  # some indicators are not sub-categorised so we delete sub-cat and key measure columns
  if (length(unique(data$`Sub-category`)) == 1) {
    data$`Sub-category` = NULL
  }
  
  # modify names of columns for the datasets where they occur
  
  if ("date_label" %in% names(data)) {
    data <- data %>%
      rename(`Date label` = date_label)
  }
  
  if ("num" %in% names(data)) {
    data <- data %>%
      rename(Numerator = num,
             Denominator = den)
  }
  
  if ("suffix" %in% names(data)) {
    data <- data %>%
      rename(Suffix = suffix)
  }
  
  if ("median" %in% names(data)) {
    data <- data %>%
      rename(Median = median,
             `Extended median` = extended)
  }
  
  if ("new_median" %in% names(data)) {
    data <- data %>%
      rename(`New median` = new_median,
             `Extended new median` = new_extended)
  }
  
  if ("mean" %in% names(data)) {
    data <- data %>%
      rename(Mean = mean)
    
    if ("extended" %in% names(data)) {
      data <- data %>% 
        rename(`Extended mean` = extended)
    }
  }
  
  if("lower_warning_limit" %in% names(data)) {
    data <- data %>%
      rename(
        Centreline = centreline,
        `Lower warning limit` = lower_warning_limit,
        `Upper warning limit` = upper_warning_limit,
        `Lower control limit` = lower_control_limit,
        `Upper control limit` = upper_control_limit,
      )
  }
  
  if ("shown_on_MIO" %in% names(data)) {
    data <- data %>%
      rename(`Shown on Multi indicator overview` = shown_on_MIO)
  }
  
  if ("plotted_on_charts" %in% names(data)) {
    data <- data %>%
      rename(`Plotted on dashboard charts` = plotted_on_charts)
  }
  
  # make sure columns are in right order
  data <- data %>%
    relocate(any_of(c("Dataset","Measure", "Board of", "Health Board", "Period", "Date", "Date label", "Sub-category", "Numerator", "Denominator", "Measure value", "Suffix", "Plotted on dashboard charts",  "Median", "Extended median", "New median", "Extended new median", "Mean", "Extended mean", "Centreline", "Lower warning limit", "Upper warning limit", "Lower control limit", "Upper control limit", "Shown on Multi indicator overview")
    )
    )
  
  return(data)  
}

# run the function across all of download_dataframe

map(names(download_dataframe), tidy_data_download) -> nice_download
names(nice_download) <- janitor::make_clean_names(names(download_dataframe))

# function to take each dataset and write it into the correct template

write_to_excel <- function(index) {
  typeof(names(nice_download)[index])
  
  rownum <- ifelse(
    first(nice_download[[index]]$Measure) %in% c("Third- and fourth-degree perineal tears", 
                                                 "Gestation at booking"),
    8, 7)

# load in the correct template
  
  wb <- loadWorkbook(
    paste0(excel_templates_folder, names(nice_download)[index],
           "_template.xlsx"
           )
    )

# write the data into the workbook as a table, putting it in an accessible table style

  writeDataTable(wb,
                 sheet = 1,
                 x = nice_download[[index]],
                 startRow = rownum,
                 tableStyle = "TableStyleLight1",
                 tableName = paste0(str_to_title(names(nice_download)[index]), "_data"),
  )
  
# set header row height

  setRowHeights(wb,
                sheet = 1,
                rows = rownum,
                heights = 60)

# set table row heights (excluding header row)

  setRowHeights(wb,
                sheet = 1,
                rows = rownum + 1:(length(nice_download[[index]]$Dataset) + rownum + 1),
                heights = 30)

# make whole table vertically centred and text wrapped

  addStyle(wb,
           sheet = 1,
           rows = rownum:(length(nice_download[[index]]$Dataset) + rownum),
           cols = 1:length(nice_download[[index]]),
           gridExpand = TRUE,
           stack = TRUE,
           style = createStyle(wrapText = TRUE, valign = "center") #, numFmt = "0.000")
  )

# format the date field - default is mm/dd/yyyy

  addStyle(wb,
           sheet = 1,
           rows = rownum:(length(nice_download[[index]]$Dataset) + rownum),
           cols = 6,
           gridExpand = TRUE,
           stack = TRUE,
           style = createStyle(numFmt = "dd/mm/yyyy", halign = "left")
  )

# save the workbook as an xlsx spreadsheet with appropriate title

  saveWorkbook(wb,
               paste0(excel_downloads_folder, "/",
                      names(nice_download)[index],
                      "_" ,
                      refresh_date, ".xlsx"),
               overwrite = TRUE
               )

}

# run this function on all indicators

walk(1:length(download_dataframe), write_to_excel)

# save outputs into a zipped file ready for copying to the dashboard folder

zipfile <- "Excel-downloads.zip"

files2zip <- dir(excel_downloads_folder, full.names = FALSE)

zip::zip(zipfile,
    files = files2zip,
    include_directories = FALSE,
    root = excel_downloads_folder)

# once the content of the Excel downloads has been checked run this:

# moves zip file to dashboard_dataframes folder and deletes excel_downloads folder

file.rename(from = paste0(excel_downloads_folder, "/Excel-downloads.zip"),
            to = paste0(dashboard_dataframes_folder, "/Excel-downloads.zip")
)

unlink(excel_downloads_folder, recursive = TRUE)

# End of Script ----
                            