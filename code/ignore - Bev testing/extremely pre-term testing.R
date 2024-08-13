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
         lower_one_third_limit = if_else(centreline - (sd * 1) < 0, 0,
                                         centreline - (sd * 1)),
         upper_one_third_limit = if_else(centreline + (sd * 1) > 100, 100,
                                         centreline + (sd * 1)),
         lower_warning_limit = if_else(centreline - (sd * 2) < 0, 0,
                                       centreline - (sd * 2)),
         upper_warning_limit = if_else(centreline + (sd * 2) > 100, 100,
                                       centreline + (sd * 2)),
         lower_control_limit = if_else(centreline - (sd * 3) < 0, 0,
                                       centreline - (sd * 3)),
         upper_control_limit = if_else(centreline + (sd * 3) > 100, 100,
                                       centreline + (sd * 3))
         )

temp <- temp |>
  mutate(outlier = if_else(measure_value > upper_control_limit | measure_value < lower_control_limit,
                           measure_value,
                           NA))

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
  select(dataset, measure, hbtype:date_label, measure_cat, num, den, measure_value, plotted_on_charts, suffix, centreline:outlier,
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

server_folder <- "https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/"

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

### - END OF SCRIPT ----

