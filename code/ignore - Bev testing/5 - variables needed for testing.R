# load latest SMR02 ABC Terminations data

load(paste0(dashboard_dataframes_folder, "/SMR02_ABC_Terminations.RData")) 
# for SPBAND dashboard - cannot connect to server, needs self-contained dataset

# load latest extreme pre-term data

extremely_preterm_data <- readRDS(paste0(dashboard_dataframes_folder, "/extremely_preterm_data.rds")) 

# load latest NRS data

NRS_timeseries <- readRDS(paste0(dashboard_dataframes_folder, "/stillbirths_infant_deaths_data.rds")) 


# split runchart_dataframe into individual indicator dataframes

bookings_data <- load_and_split_dataframe("BOOKINGS")
gest_at_booking_data <- load_and_split_dataframe("GESTATION AT BOOKING")
terminations_data <- load_and_split_dataframe("TERMINATIONS")
gest_at_termination_data <- load_and_split_dataframe("GESTATION AT TERMINATION")
inductions_data <- load_and_split_dataframe("INDUCTIONS")
type_of_birth_data <- load_and_split_dataframe("TYPE OF BIRTH")
tears_data <- load_and_split_dataframe("TEARS")
gest_at_birth_data <- load_and_split_dataframe("GESTATION AT BIRTH")
apgar5_data <- load_and_split_dataframe("APGAR5")

# set up x-axis chart labels

bookings_date_range <- unique(bookings_data$date)
bookings_date_tickvals <- bookings_date_range[seq(1, length(bookings_date_range), 2)]
bookings_date_ticktext <- format(bookings_date_tickvals,"%b %Y")

terminations_date_range <- unique(terminations_data$date)
terminations_date_tickvals <- terminations_date_range[seq(1, length(terminations_date_range), 3)]
terminations_date_ticktext <- format(terminations_date_tickvals, "%b %Y")

SMR02_date_range <- unique(inductions_data$date)
SMR02_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 2)]
SMR02_date_ticktext <- qtr(SMR02_date_tickvals, format = "short")

# SMR02_date_ticktext <- paste0(substr(SMR02_date_ticktext, 1,7),
#                               "<br>",
#                               substr(SMR02_date_ticktext, 9, 13))


SMR02_multiples_date_tickvals <- SMR02_date_range[seq(1, length(SMR02_date_range), 4)]
SMR02_multiples_date_ticktext <- qtr(SMR02_multiples_date_tickvals, format = "short")

# SMR02_multiples_date_ticktext <- paste0(substr(SMR02_multiples_date_ticktext, 1,7),
#                                         "<br>",
#                                         substr(SMR02_multiples_date_ticktext, 9, 13))

y_max_type_of_birth <- max(type_of_birth_data$measure_value, na.rm = TRUE) # not sure this is still needed

# STLLBIRTHS SPECIFIC

date_range_NRS <- as.character(unique(NRS_timeseries$quarter_label))

NRS_date_tickvals <- c(date_range_NRS[seq(1, 16, 2)], "2020", " ", " ", # balances x-axis dates
                       date_range_NRS[seq(22, length(date_range_NRS), 2)])

NRS_date_ticktext <- NRS_date_tickvals

stillbirths_download <- NRS_timeseries %>% 
  select("dataset", "hbtype", "hbname", "period", "date", "measure", "measure_cat", "num", "den", 
         "measure_value", "mean", "extended", "suffix", "num_description", "den_description",
         "measure_value_description")

y_max_NRS <- max(NRS_timeseries$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis

# GESTATION AT BIRTH SPECIFIC

# create a tibble with "nice" (superscript text) gestations for gestation at birth measure

# measure_cat_order

measure_cat_order <- c("between 18 and 44 weeks",
                     "between 37 and 41 weeks",
                     "between 32 and 36 weeks",
                     "42 weeks and over",
                     "under 32 weeks",
                     "under 37 weeks"
)

# formatted_name is the factor which controls the order in which the context charts legends should appear

formatted_name <- c(paste0("all known gestations (18", "<sup>+0</sup>", " to 44", "<sup>+6</sup>", " weeks)"),
                    paste0("37", "<sup>+0</sup>", " to 41", "<sup>+6</sup>", " weeks"),
                    paste0("32", "<sup>+0</sup>", " to 36", "<sup>+6</sup>", " weeks"),
                    paste0("42", "<sup>+0</sup>", " weeks and over"),
                    "under 32 weeks",
                    "under 37 weeks"
                    )

nicename <- tibble(measure_cat_order, formatted_name)

nicename$formatted_name <- factor(nicename$formatted_name, levels = formatted_name)

rm(measure_cat_order)

gest_at_birth_data <- left_join(gest_at_birth_data,
                                nicename,
                                by = c("measure_cat" = "measure_cat_order")
                                )

# create static labels for the runchart legends

orig_trend_label <-  
  paste0("trends: 5 or more consistently increasing", "<br>", "or decreasing points")

orig_shift_label <-  
  paste0("shifts: 6 or more consecutive points", "<br>", "above or below average")

# useful groupings for telling Shiny when to show the different drop-down filters

tabnames <- 1:13

names(tabnames) <- 
  c("home", "multi_indicator_overview", "pregnancies_booked",
    "terminations", "gestation_at_booking", "gestation_at_termination",
    "location_of_ex_pre_term", "inductions", "type_of_birth",
    "perineal_tears", "gestation_at_birth", "stillbirths",
    "apgar_scores")

show_org <- names(tabnames[!tabnames %in% c(1, 7, 12)]) # don't show organisation selection in "home",
                                                     # "location_of_ex_pre_term", "stillbirths" 

show_HBname <- names(tabnames[tabnames %in% c(2, 3, 4)]) # show HB selection in "multi_indicator_overview",
                                                        # "pregnancies_booked", "terminations"

show_HBname2 <- names(tabnames[!tabnames %in% c(1, 2, 3, 4, 7, 12)]) # the remaining indicators

island_names <- c("NHS Orkney", "NHS Shetland", "NHS Western Isles"
                  )

# order for HB dropdown filter and small multiple charts

HBnames <- list("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
             "NHS Shetland", "NHS Western Isles"
             )

# order for small multiple charts in average gestation at termination

HBnames_alternative <- list("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
                          "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde", 
                          "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside",
                          "NHS Orkney, NHS Shetland and NHS Western Isles"
                          )

# grouped island board name

HBName_terminations <- "NHS Orkney, NHS Shetland and NHS Western Isles"

# sets colour palette to the PHS colour scheme

selected_colours <-
  as.character(c(phs_colours()[1:8],
                 phs_colours(str_subset(
                   names(phs_colours()), "-80"
                 ))))

# buttons to remove (from plotly menu)

bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

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

# customise features and interactivity of DT table:
# create a list of options used to format the DT table

my.options <- list(
  dom = "t",
  scrollY = TRUE,
  scrollX = FALSE,
  autoWidth = TRUE, # smart width handling
  searching = FALSE, # search box above table
  ordering = FALSE, # whether columns can be sorted
  lengthChange = FALSE, # ability to change number rows shown on page in table
  lengthMenu = FALSE, # options lengthChange can be changed to
  pageLength = 10, # initial number of rows per page of table
  paging = FALSE, # whether to do pagination
  info = FALSE) # notes whether or not table is filtered

# create HTML formatting code for header and overall table HTML container
# create header style HTML code

header.style <- "th { font-family: 'Arial'; font-weight: bold; color: #3F3685; background-color: white;}"

### END OF SCRIPT ###
