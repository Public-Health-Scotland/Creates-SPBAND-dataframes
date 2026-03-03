####
# Stillbirths and infant deaths for the 
# Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Sourced from pre-release data provided by NRS - do not make available until after 
# NRS publication
# Bev Dodds
# 12 July 2023
# Last update by Bev Dodds
# Latest update description: initialised code
# Type of script - preparation, visualisation, data extraction for dashboards
# Written/run on R Studio Server
# Version of R - 4.4.2
# Reads in NRS Excel spreadsheet with pre-release stillbirth and infant death data and
# produces tables and charts - Scotland only, Jan-Mar 2016 - latest quarter
# Note that 2020 quarterly data is not available so the annual figure is used
# Approximate run time - <5 minutes
####

### 1 - Housekeeping ----

# Import the common housekeeping code - this MUST be updated BEFORE this code is run

source("code/1 - Housekeeping code to be updated each refresh.R")

# set groups to calculate correct denominators for rates

use_live_births <- c("neonatal deaths", "post-neonatal deaths", "infant deaths")
use_live_plus_still_births <- c("stillbirths", "extended perinatal deaths")

### 2 - Read in source data ----
# from 2016 onwards

## NRS_filename is initialised in the Housekeeping code

NRS <- read.xlsx(NRS_filename,
                 sheet = 1, startRow = 6, colNames = TRUE, cols = c(1:3, 10:21)) %>% # NRS added an extra line at the top of the table
  filter(Year >= 2016) %>% 
  mutate(Quarter = str_trim(Quarter), # removes trailing white space in some entries
         across(where(is.character), ~ sub("\\[l]", "", .)), # NRS introduced a footnote in the data table
         across(ends_with("rate"), ~ as.numeric(.)) # change rate columns from character to numeric
         )

names(NRS) <- to_snake_case(names(NRS)) # convert variable names to snake_case

### 3 - Wrangle NRS table ----

# add total for live births plus stillbirths

NRS <- NRS %>% 
  mutate(live_births_plus_stillbirths_total_number = live_births_total_number + stillbirths_number) %>% 
  relocate(live_births_plus_stillbirths_total_number,
           .after = "quarter")

# create date from year and quarter

NRS$date <- 
  as.Date(
    zoo::as.yearqtr(
      paste0(NRS$year,
             " ",
             substr(NRS$quarter, 1, 1)
      ),
      format = "%Y %q"
    )
  )

NRS$date <- if_else(NRS$year == 2020 & NRS$quarter == "Full year", "2020", as.character(NRS$date))

# create date_label - based on year and quarter 

NRS <- NRS %>% 
  mutate(date_label = recode_values(quarter,
                                    "1st" ~ paste("Jan-Mar", year, sep = " "),
                                    "2nd" ~ paste("Apr-Jun", year, sep = " "),
                                    "3rd" ~ paste("Jul-Sep", year, sep = " "),
                                    "4th" ~ paste("Oct-Dec", year, sep = " "),
                                    default = as.character(year)
                                    )
         )

# create "marker" variables

# "drop" marks "Full year" rows (apart from 2020) - these are dropped
# "flag_NA" marks 2020 quarterly values to reset to NA (not valid, but kept to make chart look balanced)
# "use_for_mean" marks the rows that contribute to the average (currently the pre-pandemic period to Oct-Dec 2019)

NRS <- NRS %>%
  mutate(drop = (year != 2020 & quarter == "Full year"), # | date_label == "Apr-Jun 2020"),
         flag_NA = (year == 2020 & quarter %in% c("1st", "2nd", "3rd", "4th")),
         use_for_mean = (year <= 2019 & quarter != "Full year")
         ) %>% 
  filter(drop == FALSE) %>%
  select(- drop) %>% 
  tibble::rowid_to_column() # creates a row number variable 

# reorder 2020 quarter values 1st, 2nd, Full year, 3rd, 4th - for chart x-axis label layout - to ensure break in line

NRS$rowid <- recode_values(NRS$rowid,
                           17 ~ 19,
                           18 ~ 17,
                           19 ~ 18,
                           default = NRS$rowid)

NRS <- arrange(NRS, rowid)

# extract all unique values of date_label to set factor levels in correct order

NRS$date_label = factor(NRS$date_label,
                           levels = NRS$date_label,
                           ordered = TRUE)

### 4 - Create timeseries ----

# create a time series subset with quarterly data points (except for 2020, only annual data point available)

# Stillbirths are classed as babies born dead from 24+0 weeks gestation onwards, rate per 1000 total
# (live + still) births
# Neonatal deaths are those that occur < 28 days (within first 4 weeks), rate per 1000 live births
# extended perinatal deaths = stillbirths + neonatal deaths, rate per 1000 total (live + still) births
# Post-neonatal deaths are those that occur >= 28 days and < 365 days (after 4 weeks but within the first year),
# rate is per 1000 live births
# Infant deaths are those that occur < 365 days, rate per 1000 live births

NRS_timeseries <- NRS %>%
  select(year, quarter, date_label, date, ends_with("number"), ends_with("rate"), use_for_mean,
         flag_NA, - starts_with("perinatal")
         ) %>%
  rename_with(., ~ sub("_number", "", .x, fixed = TRUE)) %>% 
  rename_with(., ~ sub("_rate", "s_rate", .x, fixed = TRUE)) %>% # to make the same for measure_cat
  # previously only showed rates, not numbers in download file
  # rename_with(., ~ gsub(".", " ", .x, fixed = TRUE)) %>% 
  rename_with(., ~ sub("post_neonatal", "post-neonatal", .x, fixed = TRUE)) # post-neonatal term is used in text for this indicator

# pivot longer to match standard measure format

numbers <- NRS_timeseries %>%
  select(year:infant_deaths, use_for_mean:flag_NA) %>% 
  pivot_longer(
    cols = c(stillbirths:infant_deaths),
    names_to = "measure_cat",
    values_to = "num"
  )

# pivot longer to match standard measure format

rates <- NRS_timeseries %>%
  select(year:date, stillbirths_rate: infant_deaths_rate, use_for_mean:flag_NA) %>% 
  rename_with(., ~ sub("_rate", "", .x, fixed = TRUE)) %>%
  pivot_longer(
    cols = c(stillbirths:infant_deaths),
    names_to = "measure_cat",
    values_to = "measure_value"
  )

# pivot longer to match standard measure format

denominators <- NRS_timeseries %>%
  select(year:live_births_total, use_for_mean:flag_NA
         ) %>% 
  pivot_longer(
    cols = c(live_births_plus_stillbirths_total:live_births_total),
    names_to = "measure_cat",
    values_to = "num"
  )

# match rates to numbers

NRS_timeseries <- left_join(numbers, rates,
                            by = join_by(year, quarter, date_label, date, use_for_mean,
                                         flag_NA, measure_cat)
                            )

# add denominators to main dataframe

NRS_timeseries <- bind_rows(denominators, NRS_timeseries)

# remove underscores from measure_cat

NRS_timeseries$measure_cat <- str_replace_all(NRS_timeseries$measure_cat, "_", " ")

# populate den with correct denominator value
# i.e. if measure_cat in use_live_births, den = live_births_total,
# if measure_cat in use_live_plus_still_births, den = live_births_plus_stillbirths_total

NRS_timeseries <- NRS_timeseries %>% 
  mutate(den = recode_values(measure_cat,
                             use_live_births ~ live_births_total,
                             use_live_plus_still_births ~ live_births_plus_stillbirths_total,
                             default = NA)) %>% 
  relocate(den,
           .after = num) %>% 
  select(- starts_with("live")) # removes unnecessary columns

# filter on use_for_mean to calculate mean rate per measure_cat

mean_rates <- NRS_timeseries %>% 
  filter(use_for_mean == TRUE & !is.na(den)) %>%
  summarise(mean = round(mean(measure_value, na.rm = TRUE), 2),
            .by = measure_cat
            )

# match mean_rates to main dataframe and drop year, quarter variables 

NRS_timeseries <- left_join(NRS_timeseries, mean_rates) %>%
  select(- c(year, quarter)
         )

# reset mean to NA after 2019, set extended to mean after 2019, remove 2020 quarter values

NRS_timeseries <- NRS_timeseries %>% 
  mutate(extended = mean, 
         mean =
           if_else(use_for_mean == FALSE, NA, mean),
         extended = if_else(!is.na(mean), NA, extended), # prevents extended line overlaying mean line on chart
         num = if_else(flag_NA, NA_real_, num), # removes the incomplete numbers for 2020 quarters
         den = if_else(flag_NA, NA_real_, den), # removes the incomplete numbers for 2020 quarters
         measure_value = if_else(flag_NA, NA_real_, measure_value) # removes the data points for the 2020 quarters, not plotted
  ) |> 
  group_by(measure_cat) |> 
  mutate(extended = if_else(date_label == "Oct-Dec 2019", lag(mean), extended)) # makes extended line run on from mean line on chart 

# add metadata labels for download file

NRS_timeseries <- NRS_timeseries %>% 
  mutate(measure = "STILLBIRTHS AND INFANT DEATHS",
         period = "Q",
         dataset = "NRS VITAL EVENTS",
         hbname = "SCOTLAND",
         hbtype = "REGISTRATION",
         suffix = recode_values(measure_cat,
                                use_live_births ~ "rate per 1,000 live births",
                                use_live_plus_still_births ~ "rate per 1,000 total (live + still) births",
                                default = NA)
         ) %>% 
  left_join(., metadata,
            by = c("measure", "measure_cat")
            ) %>% 
  select(- flag_NA)

# set 2020 label to annual rate

NRS_timeseries <- NRS_timeseries %>% 
  mutate(measure_value_description = if_else(date_label == "2020",
                                       str_replace_all(measure_value_description, "quarterly", "annual"),
                                       measure_value_description)
         )

# subset of measure_cat required to plot the five charts in the correct order

NRS_timeseries$measure_cat2 <- factor(NRS_timeseries$measure_cat,
                                       levels = c("stillbirths", 
                                                  "neonatal deaths",
                                                  "extended perinatal deaths",
                                                  "post-neonatal deaths",
                                                  "infant deaths")
)

NRS_timeseries <- arrange(NRS_timeseries, measure_cat2, date_label) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE) %>% 
  select(dataset, measure, hbtype, hbname, period, date, date_label, measure_cat, num, den, measure_value, plotted_on_charts, suffix, mean, extended, shown_on_MIO, measure_cat2, contains("description")
         )

### 5 - Save NRS_timeseries data for use in dashboard ----

saveRDS(NRS_timeseries,
  file = paste0(dashboard_dataframes_folder, "/stillbirths-infant-deaths-data.rds")
  )

# if wanting to load again for testing

NRS_timeseries <- readRDS(paste0(dashboard_dataframes_folder, "/stillbirths-infant-deaths-data.rds"))

### 6 - Testing charts look OK ----

# remove "Apr-Jun 2020" row (to balance chart)

NRS_timeseries <- NRS_timeseries |> 
  filter(date_label != "Apr-Jun 2020") |> 
  mutate(date = if_else(date_label == "2020", as.Date("2020-04-01"), as.Date(date)))

# set y-axis labels for charts

yaxislabel1 <- list(title = list(text = "rate per 1,000 total (live + still) births",
                                 font = list(size = 12)))

yaxislabel2 <- list(title = list(text = "rate per 1,000 live births",
                                 font = list(size = 12)))

# calculates maximum value on y-axis (to set a common scale)

y_max <- max(NRS_timeseries$measure_value, na.rm = TRUE) 

date_range_NRS <- as.character(unique(NRS_timeseries$date))

date_label_range_NRS <- as.character(unique(NRS_timeseries$date_label))

# tells plotly where to place x-axis tick marks and labels

NRS_date_tickvals <- c(date_range_NRS[seq(1, 16, 2)], date_range_NRS[18], # only mark "Apr-Jun 2020" which is the annual figure
                       date_range_NRS[seq(21, length(date_range_NRS), 2)])

NRS_date_ticktext <- c(date_label_range_NRS[seq(1, 16, 2)], "2020", # labels to match marks
                       date_label_range_NRS[seq(21, length(date_label_range_NRS), 2)])

xaxis_plots <- orig_xaxis_plots
xaxis_plots[["tickmode"]] = "array" # makes non-date x-axis labels follow correct order
xaxis_plots[["ticktext"]] = NRS_date_ticktext
xaxis_plots[["tickvals"]] = NRS_date_tickvals

# tells plotly how to format y-axis

yaxis_plots <- orig_yaxis_plots
yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs

# selects data for charts

stillbirths_runchart_data <-

NRS_timeseries %>%
  filter(!measure_cat %like% "total") |> # don't want the "total" values
  mutate(measure_label = paste0("Rate per 1000 ", den_description),
         hover_date_label = if_else(date_label == "2020",
                              paste0("Year: ", date_label),
                              paste0("Quarter: ", date_label)
         )
         ) %>% 
  set_variable_labels(
    mean = " average to Oct-Dec 2019",
    extended = " projected average from Jan-Mar 2020"
    ) %>% 
    mutate(mytext = paste0(hover_date_label,
                           "<br>",
                           str_to_sentence(measure_cat),
                           "<br>",
                           measure_label,
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1)
                           )
    )

# builds plotly charts as per dashboard

stillbirth_charts <- stillbirths_runchart_data %>%
  split(.$measure_cat2) %>% 
  lapply(
    function(d)
      plot_ly(d, 
              x = ~ date,
              y = ~ measure_value,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "black", # black lines
                    width = 1,
                    dash = "solid"),
        marker = list(color = "black", # black dots
                      size = 5),
        name = "rate per 1,000 related births", # retrieves label of variable
        legendrank = 100,
        legendgroup = "measure_value",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hovertext = ~ mytext,
        hoverinfo = "text"
        ) |> 
      add_trace(
        y = ~ mean, # solid blue line
        type = "scatter",
        mode = "lines",
        line = list(color = phs_colours("phs-blue"), 
                    width = 1, dash = "solid"),
        marker = NULL,
        name = "average to Oct-Dec 2019", # label of variable
        legendrank = 200,
        legendgroup = "mean",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hovertext = ""
        ) |> 
      add_trace(
        y = ~ extended, # dotted blue line # this line first as plotting last leads to overrun 
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-blue"), 
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = "projected average from Jan-Mar 2020", # label of variable
        legendrank = 300, 
        legendgroup = "extended",
        showlegend = ~ unique(measure_cat) == "infant deaths",
        hovertext = "",
        hoverinfo = "none"
      ) |> 
      layout(xaxis = xaxis_plots,
             yaxis = yaxis_plots,
             annotations = list(
               x = 0.5,
               y = 1.0,
               text = ~ unique(measure_cat),
               font = list(size = 16),
               xref = "paper",
               yref = "paper",
               xanchor = "center",
               yanchor = "bottom",
               showarrow = FALSE
               )
             )
    )

stillbirth_charts <- stillbirth_charts %>% 
  subplot(nrows = 2,
          heights = c(0.45, 0.45),
          margin = c(0.01, 0.03, 0.075, 0.075),
          shareX = FALSE,
          shareY = FALSE,
          titleY = TRUE
          ) %>%
  layout(
      yaxis = yaxislabel1,
      yaxis2 = yaxislabel2,
      yaxis3 = yaxislabel1,
      yaxis4 = yaxislabel2,
      yaxis5 = yaxislabel2
  )
  
stillbirth_charts <- stillbirth_charts %>% 
    layout(
      legend = list(orientation = "v",
                    x = 0.9,
                    xanchor = "auto",
                    y = 0.25,
                    groupclick = "togglegroup"
                    )
      ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

stillbirth_charts

# tidy up

rm(NRS, numbers, denominators, rates, mean_rates)

### - END OF SCRIPT ----
