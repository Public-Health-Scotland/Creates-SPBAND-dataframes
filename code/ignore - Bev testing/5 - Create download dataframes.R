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

decimals <- read.xlsx("../basefiles/excel templates/decimals.xlsx")

# Read in the data to be reformatted

download_dataframe <- readRDS(paste0(data_path, "/", "download_dataframe.rds"))

annual_dataframe <- readRDS(paste0(data_path, "/", "annual_dataframe.rds"))

#load(paste0(dashboard_dataframes_folder, "/SMR02-ABC-Terminations.RData"))

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
    
    select(- any_of(c("num_description", "den_description", "measure_value_description", "quarter_label", "use_for_mean", "measure_cat2", "MIO_measure_label", "MIO_measure_ref", "MIN", "MAX", "RANGE", "RESCALED", "MIN_RS", "MAX_RS", "plotlylabel")
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
#   
# # create styles for no decimal places and two decimal places
#   
#   no_dp <- createStyle(numFmt = "0")

 two_dp <- createStyle(numFmt = "0.00")
  
# define row number from which to print table
  
  header_row <- ifelse(
    names(nice_download) %in% 
      c("tears", "gestation_at_booking"), 8, 7)
  
  all_cols <- c(1:length(nice_download[[1]]))
  all_rows <- c(header_row[1]: length(nice_download[[1]]$Dataset)) # 7:4050
  data_rows <- c(header_row[1] + 1: length(nice_download[[1]]$Dataset)) # 8:4057
  tabledata <- rowcol_to_dims(data_rows, all_cols) # "A8:N4057"
  first_data_row <- min(data_rows) # 8
  last_data_row <- max(data_rows) # 4057

  
  wb_dims(rows = data_rows, cols = all_cols)

  # colnum_no_dp <- decimals[index, "no_dp"]
  
  #colnum_two_dp[index] <- decimals[index, "two_dp"]

  # load in the correct template
  
  wb <- wb_load(
    paste0(excel_templates_folder, names(nice_download)[1],
           "_template.xlsx")
  ) %>%
    
    # write the data into the workbook as a table, putting it in an accessible table style    
    wb_add_data_table(sheet = 1,
                      x = nice_download[[1]],
                      start_col = 1,
                      start_row = header_row[1],
                      table_style = "TableStyleLight1",
                      table_name = paste0(str_to_title(names(nice_download)[1]), "_data"),
                      na.strings = ""
    ) %>% 
  
    # set header row height
    
    wb_set_row_heights(
      sheet = 1,
      rows = header_row[1],
      heights = 60
    ) %>% 
    
    # set table row heights (excluding header row)
    
    wb_set_row_heights(
      sheet = 1,
      rows = c(header_row[1] + 1: length(nice_download[[1]]$Dataset)),
      heights = 30
    ) %>% 
    
    # make whole table vertically centred and text wrapped
    
  wb_add_cell_style(
    sheet = 1,
    dims = rowcol_to_dims(all_rows, all_cols),
    wrap_text = 1,
    vertical = "center"
    )
  
  wb_add_conditional_formatting(
    wb, 
    sheet = 1,
    dims = wb_dims(cols = "A", rows = 1:4), rule = ">2")
  

  
  
    )
  
  wb_save(wb, paste0(dashboard_dataframes_folder, "/", "out_file.xlsx"), overwrite = TRUE)

  wb_dims(x = nice_download[[1]])

int2col(all_cols)

dput(all_cols)

rowcol_to_dims(data_rows, all_cols)

dims_to_rowcol(data_rows, as_integer = FALSE)

data_rows2 <- wb_dims(x =  nice_download[[1]], select = "data")
wb_dims(x =  nice_download[[1]], select = "col_names")
wb_dims(x =  nice_download[[1]], row_names = TRUE, select = "row_names")
z <- wb_dims(x =  nice_download[[1]], cols = "Measure value") 
z + wb_dims(header_row[1])

  # 
  # wb <- loadWorkbook(
  #   paste0(excel_templates_folder, names(nice_download)[1],
  #          "_template.xlsx"
  #          )
  #   )
  
# write the data into the workbook as a table, putting it in an accessible table style
    
  # writeDataTable(wb,
  #                sheet = 1,
  #                x = nice_download[[index]],
  #                startRow = rownum[index],
  #                tableStyle = "TableStyleLight1",
  #                tableName = paste0(str_to_title(names(nice_download)[index]), "_data"),
  # )
  
# set header row height
    
    wb_set_row_heights(
      wb,
      sheet = "Data",
      rows = rownum[index],
      heights = 60
    ) 

  # setRowHeights(wb,
  #               sheet = 1,
  #               rows = rownum[index],
  #               heights = 60)

# set table row heights (excluding header row)
    
    wb_set_row_heights(
      wb,
      sheet = "Data",
      rows = rownum[index] + 1:(length(nice_download[[index]]$Dataset) + rownum[index] + 1),
      heights = 30
    ) 

  # setRowHeights(wb,
  #               sheet = 1,
  #               rows = rownum[index] + 1:(length(nice_download[[index]]$Dataset) + rownum[index] + 1),
  #               heights = 30)

# make whole table vertically centred and text wrapped
    
    wb_set_cell_style_across(
      wb,
      sheet = 1,
      style = create_cell_style(wrap_text = TRUE, vertical = "center"),
      rows = rownum[index]:(length(nice_download[[index]]$Dataset) + rownum[index]),
      cols = 1:length(nice_download[[index]])
    )

  # addStyle(wb,
  #          sheet = 1,
  #          rows = rownum[index]:(length(nice_download[[index]]$Dataset) + rownum[index]),
  #          cols = 1:length(nice_download[[index]]),
  #          gridExpand = TRUE,
  #          stack = TRUE,
  #          style = createStyle(wrapText = TRUE, valign = "center") #, numFmt = "0.000")
  # )

# format the date field - default is mm/dd/yyyy
    
    wb_set_cell_style_across(
      wb,
      sheet = 1,
      style = create_cell_style(numFmt = "dd/mm/yyyy", horizontal = "left"),
      rows = rownum[index]:(length(nice_download[[index]]$Dataset) + rownum[index]),
      cols = 6
    )
    
  # addStyle(wb,
  #          sheet = 1,
  #          rows = rownum[index]:(length(nice_download[[index]]$Dataset) + rownum[index]),
  #          cols = 6,
  #          gridExpand = TRUE,
  #          stack = TRUE,
  #          style = createStyle(numFmt = "dd/mm/yyyy", halign = "left")
  # )
  
# format 2dp columns
  
  wb_add_cell_style(wb,
                    sheet = 1,
                    dims = "Measure_value",
                    num_fmt_id = two_dp
    )
  
# save the workbook as an xlsx spreadsheet with appropriate title
  
  wb_save(wb,
          file = paste0(dashboard_dataframes_folder, "/",
                        names(nice_download)[index],
                        "_" ,
                        refresh_date, ".xlsx"),
          overwrite = TRUE
          )

  # saveWorkbook(wb,
  #              paste0(dashboard_dataframes_folder, "/",
  #                     names(nice_download)[index],
  #                     "_" ,
  #                     refresh_date, ".xlsx"),
  #              overwrite = TRUE
  #              )

}

# run this function on all indicators

walk(1:length(download_dataframe), write_to_excel(1))

wb_save(wb, paste0(dashboard_dataframes_folder, "/", "out_file.xlsx"), overwrite = TRUE)

# End of Script ----
                            