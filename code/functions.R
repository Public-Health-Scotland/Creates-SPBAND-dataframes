# Function to calculate the percentage of 2 numbers
# Parameters:
# x: numerator
# y: denominator

percentage = function(x, y, na.rm = TRUE) {
  x / y * 100
}

# Function to tally the number of a given variable
# Parameters:
# dataset: defaults to births (can be modified)
# variable: name of the variable to be counted
# pre_pan: used if counting pre-pandemic values only (set to NULL otherwise)
# pre_pan_date: used if counting pre-pandemic values only (set to NULL otherwise)
# subgroup: name of a subgroup (e.g. SIMD), default is NULL = no subgroup
# tally_var: name of the denominator variable (defaults to births)
# suffix: a character describing the nature of the measure (e.g. "%")
# measure: the name of the measure (e.g. "GESTATION AT BIRTH")
# key: the name of the key (e.g. "B1") - relates to the multi indicator overview display order
# key_measure_cat: the name of the "key" measure_cat (e.g. "induced") - relates to the multi indicator overview display
# key_measure_label: the text to be shown in the multi indicator overview chart/table

counts <- 
  function(dataset = births, variable, pre_pan = pre_pan, pre_pan_date = pre_pan_date,
           subgroup = NULL, tally_var = births, suffix, measure, key, 
           key_measure_cat, key_measure_label){
    
    name <- substitute(subgroup)
    #print(name)
    
    data <- filter({{dataset}}, !is.na({{variable}})) # selects and filters dataset (removes NAs)
    
    # if({{measure}} %in% c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
    #                          "GESTATION AT TERMINATION")) { # only shown monthly
    #   
    #   data <- filter(data, period != "Q") # removes quarterly data
    #     
    #   } else { 
    
    # if({{measure}} != "TYPE OF BIRTH") { # only shown quarterly except TOB for MCQIC
    #     
    #     data <- filter(data, period != "M") # removes monthly data except for TYPE OF BIRTH
    #     }
    #   } ##### redundant
    
    data <- data %>% 
      
      # aggregates numerator (num) over specified group 
      
      select(dataset, hbtype, hbname, {{pre_pan}}, {{pre_pan_date}}, date,
             period, {{subgroup}}, measure_cat := {{variable}}, {{tally_var}}) %>%
      group_by(dataset, hbtype, hbname, date, period, {{pre_pan}}, {{pre_pan_date}},
               {{subgroup}}, measure_cat) %>% 
      summarise(num = sum({{tally_var}}))

      data <- data %>% 
        
        # pivots numerators from measure_cat to calculate totals
        
        pivot_wider(names_from = measure_cat,
                    names_prefix = "num_",
                    values_from = num,
                    values_fill = 0) %>%
        mutate(`num_total` = sum(across(where(is.numeric))),
               den = `num_total` - sum(if_any(contains("unknown"))),
               `num_total exc. unknown` = den) %>%
        relocate(`num_total exc. unknown`, 
                 .before = contains("unknown"))

      if({{measure}} == "TYPE OF BIRTH"){ 
        
        data <- data %>%
          
          # calculates "all" caesarean births
          
          mutate(`num_all caesarean births` =
                 `num_planned caesarean births` +
                 `num_unplanned caesarean births`) %>%
          relocate(`num_all caesarean births`,
                   .before = `num_planned caesarean births`)
      }

      data <- data %>% 
        
        # pivots longer again
        
        pivot_longer(cols = starts_with("num_"),
                     names_to = "measure_cat",
                     names_prefix = "num_",
                     values_to = "num")
      
      if(!{{measure}} %in% c("BOOKINGS", "TERMINATIONS")) {
        
        data <- data %>% 
          
          # calculates percentages (based on "known" values in denominator)
          mutate(measure_value = if_else(!grepl("unknown", measure_cat) &
                                     !grepl("total", measure_cat),
                                   percentage(num, den),
                                   NA_real_),
                 # sets "den" to NA for "unknown" and "total" values
                 den = if_else(!grepl("unknown", measure_cat) &
                                 !grepl("total", measure_cat),
                               den, NA_real_)
          )
      } else {
          data <- data %>% 
            mutate(measure_value = num, # for BOOKINGS AND TERMINATIONS where there is no percentage measure
                   num = NA,
                   den = NA)
        }
      
      data <- data %>% 
        
        # add variables to the dataframe
        
        mutate(
        suffix = if_else(suffix == "", NA, suffix),
        measure = measure,
        key_measure_cat = measure_cat == key_measure_cat,
        key_measure_ref = if_else(key_measure_cat == TRUE, key, NA),
        key_measure_label = if_else(key_measure_cat == TRUE, key_measure_label, NA)) %>%
      rename(subgroup_cat = {{subgroup}})

    if(!is.null(name)) data$subgroup = as.character(name)
    
    return(data)
  }

# counts2 <- # without complete, removes unknowns
#   function(dataset = births, variable, pre_pan = pre_pan, 
#            pre_pan_date = pre_pan_date, subgroup = NULL, tally_var = births, 
#            den_label, suffix, measure, key){
#     
#     name <- substitute(subgroup)
#     #print(name)
#     
#     data <- filter({{dataset}}, !is.na({{variable}}) & !grepl("known", {{variable}})) %>%
#       select(dataset, hbtype, hbname, {{pre_pan}}, {{pre_pan_date}}, date, 
#              period, {{subgroup}}, measure_cat := {{variable}}, {{tally_var}}) %>%
#       group_by(dataset, hbtype, hbname, {{pre_pan}}, {{pre_pan_date}},
#                date, period, {{subgroup}},
#                measure_cat) %>%
#       summarise(num = sum({{tally_var}})) %>%
#       #complete(., measure_cat, fill = list(num = 0)) %>% 
#       mutate(den = sum(num),
#              measure_value = round(percentage(num, den), 1),
#              den_LABEL = den_label,
#              suffix = suffix,
#              measure = measure,
#              measure_REF = key) %>%
#       rename(subgroup_cat = {{subgroup}}) #%>% 
#     # ungroup() %>% 
#     # select(- pre_pan, - pre_pan_date)
#     
#     if(!is.null(name)) data$subgroup = as.character(name)
#     
#     return(data)
#   }

# Function to calculate the median of the measure variable over the pre-pandemic period
# Parameters:
# dataset: the name of the input dataframe
# measure_value: fixed (is the measure_value in every dataframe)

calculate_medians <- function(dataset, measure_value){
  
  data <- {{dataset}} %>% 
    group_by(dataset, hbtype, hbname, pre_pan, period, measure, measure_cat) %>% # PRE-PANDEMIC period
    mutate(median =
             unique(if_else(pre_pan == TRUE,
                            median({{measure_value}}, na.rm = TRUE), 
                            NA_real_))) %>% # median of measure_value
    group_by(dataset, hbtype, hbname, period, measure, measure_cat) %>%
    arrange(pre_pan_date, .by_group = TRUE) %>%
    mutate(extended = median,
           extended = na.locf(extended, na.rm = FALSE))
  
  return(data)
  
}

# Function to flag shifts and trends on run chart data
# Parameters:
# shift: the name for the new variable where shift is flagged
# trend: the name for the new variable where trend is flagged
# value: the name of the variable which contains the value being evaluated
# median: the name of the variable which contains the median against which value is tested

runchart_flags <- function(dataset, shift, trend, value, median) {
  
  dataset <- dataset %>%
    mutate(
      shift_i = tidytable::case_when(
        ({{value}} > {{median}} & lag({{value}}, 1) > {{median}} &
           lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} &
           lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}})
        | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} &
             lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} &
             lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}}) ~ TRUE,
        TRUE ~ FALSE),
      
      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE  ~ TRUE,
        TRUE ~ FALSE),
      
      trend_i = tidytable::case_when(
        ({{value}} > lag({{value}}, 1) & lag({{value}}, 1) > lag({{value}}, 2)
         & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4)) |
          ({{value}} < lag({{value}}, 1) & lag({{value}}, 1) < lag({{value}}, 2)
           & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4)) ~ TRUE,
        TRUE ~ FALSE),
      
      trend = tidytable::case_when(
        trend_i == TRUE | lead(trend_i, 1) == TRUE | lead(trend_i, 2) == TRUE
        | lead(trend_i, 3) == TRUE | lead(trend_i, 4) == TRUE ~ TRUE,
        TRUE ~ FALSE)
      ) %>%
    
    rename({{shift}}:=shift,{{trend}}:=trend) %>%
    select(-shift_i, -trend_i)

# Two trends can sometimes run into each other. This can be problematic when
# they are plotted using lines.
#
# Two adjacent trends where one point is in both trends is fine (think data
# like \/) - the trend line should not be interrupted. But when the last point
# in one trend is adjacent to the first point in the next (think \|\) we don't
# want to connect both trend lines together.
#
# This code adds a column to the data that identifies the problematic cases,
# so the lines can be split in the plotting function.
#
# Problematic points are surrounded by other trend points and are in a section
# where the gradient changes sign twice in succession.
  
dataset <-
  dataset %>%

  # For identifying two successive changes in gradient direction

  mutate(gradient = sign({{value}} - lag({{value}})),
         gradient_lag_change = lag(gradient) != gradient,
         gradient_lead_change = lead(gradient) != gradient
         ) %>%

  # Need these to find whether point is in the middle of a trend

  mutate(across(all_of(trend), ~lag(.x), .names = "trend_lag"),
         across(all_of(trend), ~lead(.x), .names = "trend_lead")
         ) %>%

  # Set new column to TRUE when all conditions are met

  mutate("{trend}.split" :=
           if_else(gradient_lag_change + gradient_lead_change +
                     .data[[trend]] + trend_lag + trend_lead == 5,
                   TRUE, FALSE, missing = FALSE)
         ) %>%

  # Remove columns that are no longer needed

  select(- all_of(c("gradient", "gradient_lag_change", "gradient_lead_change",
                   "trend_lag", "trend_lead"))
         )

# There is a similar issue for shifts. There can't be one point in two shifts,
# but the last point in one can be adjacent to the first point in the next.
# Again we don't want to connect the lines.
#
# Problematic points are surrounded by other shift points and are on the
# opposite side of the median from the preceding point.
  
dataset <-
  dataset %>%

  # Find whether sign has changed from preceding point

  mutate(median_diff_sign = sign({{value}} - {{median}}),
         sign_change = median_diff_sign != lag(median_diff_sign)
  ) %>%

  # Need these to find whether point is in the middle of a shift

  mutate(across(all_of(shift), ~lag(.x), .names = "shift_lag"),
         across(all_of(shift), ~lead(.x), .names = "shift_lead")
  ) %>%

  # Set new column to TRUE when all conditions are met

  mutate("{shift}.split" :=
           if_else(sign_change + .data[[shift]] +
                     shift_lag + shift_lead == 4,
                   TRUE, FALSE, missing = FALSE)
  ) %>%

  # Remove columns that are no longer needed

  select(- all_of(c("median_diff_sign", "sign_change",
                    "shift_lag", "shift_lead"))
  )
}

# Function to split adjacent shifts and trends that should not be connected
# i.e. the end point of one and the start point of the next should not be joined by 
# the green trend line (or yellow dots)?
# These were marked in the data during prep, now we just need to add a row with measure = NA 
# between the shifts/trends.
# Parameters:
# dataset: the dataframe with shift or trend data
# measure: the measure variable
# split_col_prefix: string prefix for column that identifies split locations
  
add_split_gaps <- function(dataset, measure, split_col_prefix){
  dataset <- 
      dataset %>%

      # How many times should each row be included? Splits should be included
      # twice, and FALSE + 1 = 1, TRUE + 1 = 2
      
      mutate(across(all_of(paste0(split_col_prefix, ".split")),
                    ~.x + 1, .names = "num_rows")
             ) %>%
      
      # If a row should be included more than once, duplicate it
    
      uncount(weights = num_rows, .id = "dup_row", .remove = FALSE) %>%
      
      # We want measure to be NA for the first row in each split
      mutate(across(all_of(measure),
                    ~if_else((num_rows == 2) & (dup_row == 1), NA_real_, .x))
             )
}

# Function to read in data and split by measure, remove redundant columns
# Parameters:
# measure: 

load_and_split_dataframe <- function(measure) {
  
  data <- filter(runchart_dataframe, measure == {{measure}}) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE)
  
  return(data)
}

# Function to create empty plot when no data available
# Parameters:
# height_plot: height of the empty chart in pixels
# text_nodata: What text will show when no data is available

plot_nodata <- function(height_plot = 450, text_nodata) {
  
  text_na <- list(align = "centre",
                  text = text_nodata,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE,
                  name = "_no_data_plot_marker_") # so can check later if plot object has no data

  plot_ly(height = height_plot) %>%
    add_trace(x = 0,
              y = 0,
              visible = FALSE,
              type = "scatter",
              mode = "lines") %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE,
                        fixedrange = TRUE),
           xaxis = list(showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE,
                        fixedrange = TRUE),
           font = list(size = 14)) %>%
    config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
}

# Function to create plotly small multiple overview charts with median
# Parameters:
# plotdata: the dataframe containing the data to be plotted
# measure: the "measure" variable to be plotted as black dots/lines
# hover: the hovertext for the measure trace
# centreline: the variable representing the median to be plotted as a solid blue line
# dottedline: the variable representing the extended median to be plotted as a dotted blue line
# yaxislabel: the text for the y-axis label

creates_overview_charts_with_median <- function(plotdata,
                                                measure = "measure",
                                                hover = "mytext",
                                                centreline = "median",
                                                dottedline = "extended",
                                                yaxislabel = yaxislabel){
  
  y_max <- max(plotdata$measure)
  
  xaxis_plots <- orig_xaxis_plots
  #xaxis_plots[["showticklabels"]] <- if_else(plotdata$hbname %in% island_names, TRUE, FALSE)
  xaxis_plots[["dtick"]] <-  case_when(plotdata$period == "Q" ~ "2",
                                       TRUE ~ "M6")
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(-1.0, y_max * 1.05)
  
  # annotates the "10 week" dot on the GESTATION AT BOOKING charts
  
  # annotations
  a <- list(
    x = max(plotdata$date), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(
      family = 'Arial',
      size = 14,
      color = phs_colours("phs-blue")
      ),
    showarrow = FALSE
  )
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING"){
    
    overview <- plotdata %>%
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ get(centreline), # solid blue line
            type = "scatter",
            mode = "lines",
            line = list( 
              color = phs_colours("phs-blue"), 
              width = 1), 
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(dottedline), # dotted blue line
            type = "scatter",
            mode = "lines",
            line = list( 
              color = phs_colours("phs-blue"),
              width = 1,
              dash = "4" 
            ),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(
              color = "black", # black lines 
              width = 1
              ),
            marker = list(
              color = "black", # black dots
              size = 5
              ),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          add_trace(
            x = ~ max(plotdata$date), # dot for 10 weeks
            y = 10,
            type = 'scatter',
            mode = "lines+markers", 
            line = list(opacity = 0),
            marker = list(
              color = phs_colours("phs-blue"),
              size = 5
              ),
            hovertext = ""
          ) %>%
          add_trace(
            data = filter(gest_at_booking_small_multiples_data,!is.na(new_median)),
            y = ~ new_median, # green line (where applicable)
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-green"),
              width = 1
            ),
            marker = NULL,
            name = ~ paste0(case_when(
              hbname == "NHS Tayside" ~
                paste0(hbname, " average from Aug 2020 to end Dec 2020"),
              hbname == "NHS Forth Valley" ~
                paste0(hbname, " average from Mar 2021 to end Jun 2021"),
              TRUE ~ ""
            )
            ),
            legendrank = 400,
            hovertext = ""
          ) %>%
          add_trace(
            data = filter(gest_at_booking_small_multiples_data,!is.na(new_extended)),
            y = ~ new_extended, # dotted green line (where applicable)
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-green"),
              width = 1,
              dash = "4"
            ),
            marker = NULL,
            name = ~ paste0(case_when(
              hbname == "NHS Tayside" ~
                paste0(hbname, " projected average from Jan 2021"),
              hbname == "NHS Forth Valley" ~
                paste0(hbname, " projected average from Jul 2021"),
              TRUE ~ ""
            )
            ),
            legendrank = 500,
            hovertext = ""
          ) %>% 
          layout(annotations = a) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(hbname2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
    
  } else {
    overview <- plotdata %>% 
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ get(centreline), # solid blue line
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-blue"),
              width = 1
              ),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(dottedline), # dotted blue line
            type = "scatter",
            mode = "lines",
            line = list(
              color = phs_colours("phs-blue"),
              width = 1,
              dash = "4"
            ),
            hovertext = ""
          ) %>%
          add_trace(
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(
              color = "black", # black lines
              width = 1
            ),
            marker = list(
              color = "black", # black dots
              size = 5
            ),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(hbname2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  }
  
  overview <- overview %>%   
    subplot(nrows = 5,
            heights = c(0.15, 0.2, 0.2, 0.2, 0.18),
            margin = c(0.01, 0.01, 0.05, 0.02),
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(
      annotations = list(
        x = 0,
        y = 0.5,
        text = ~ yaxislabel,
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

# Function to create plotly small multiple overview charts without the median
# Parameters:
# plotdata: the dataframe containing the data to be plotted
# measure: the "measure" variable to be plotted as black dots/lines
# hover: the hovertext for the measure trace
# yaxislabel: the text for the y-axis label

creates_overview_charts_without_median <- function(plotdata,
                                                   measure = "measure",
                                                   hover = "mytext",
                                                   yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$measure)
  
  xaxis_plots <- orig_xaxis_plots
  #xaxis_plots[["showticklabels"]] <- if_else(plotdata$hbname %in% island_names, TRUE, FALSE)
  xaxis_plots[["dtick"]] <- case_when(plotdata$period == "Q" ~ "3",
                                      TRUE ~ "M6")
  #xaxis_plots[["tickangle"]] <- -45
  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05)
  
  # annotates the "10 week" dot on the GESTATION AT BOOKING charts
  
  # annotations
  a <- list(
    x = max(plotdata$date), 
    y = 10,
    xanchor = 'left',
    yanchor = 'middle',
    text = " 10 weeks",
    font = list(
      family = 'Arial',
      size = 14,
      color = phs_colours("phs-blue")
    ),
    showarrow = FALSE
  )
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING"){
    
    overview <- plotdata %>%
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(
              color = "black", # black lines
              width = 1
            ),
            marker = list(
              color = "black", # black dots
              size = 5
            ),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          add_trace(
            x = ~ max(plotdata$date), # dot for 10 weeks
            y = 10,
            type = 'scatter',
            mode = "lines+markers",
            line = list(opacity = 0),
            marker = list(
              color = phs_colours("phs-blue"),
              size = 5
            ),
            hovertext = ""
          )
        %>% 
          layout(annotations = a) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(hbname2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  } else {
    
    overview <- plotdata %>% 
      split(.$hbname2) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ get(measure),
            type = "scatter",
            mode = "lines+markers",
            line = list(
              color = "black", # black lines
              width = 1
            ),
            marker = list(
              color = "black", # black dots
              size = 5
            ),
            hovertext = ~ get(hover),
            hoverinfo = "text"
          ) %>%
          layout(
            #font = list(size = 12),
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 1.0,
              text = ~ unique(hbname2),
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
      )
  }
  
  overview <- overview %>%
    subplot(nrows = 5,
            heights = c(0.15, 0.2, 0.2, 0.2, 0.18),
            margin = c(0.01, 0.01, 0.05, 0.02), 
            shareX = TRUE,
            shareY = TRUE) %>% 
    layout(
      annotations = list(
        x = 0,
        y = 0.5,
        text = ~ yaxislabel,
        xshift = -50,
        textangle = 270,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper"
      )
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

add_variable_labels <- function(plotdata) {
  
plotdata <- plotdata %>% 
  mutate(
  num_label = #unique(#first(
    case_match(
      measure,
      c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
        "GESTATION AT TERMINATION") ~ "",
      "INDUCTIONS" ~ "Number of births following induction: ",
      "TYPE OF BIRTH" ~
        if_else(measure_cat == "all caesarean births",
                paste0("Number of births that were caesarean births: "),
                paste0("Number of births that were ", measure_cat, ": ")
        ),
      "TEARS" ~ "Number of women who have a third or fourth degree perineal tear: ",
      "GESTATION AT BIRTH" ~ paste0("Number of babies born at ", formatted_name, ": "),
      "APGAR5" ~ "Number of births that have a 5 minute Apgar score of <7: "
    ),
    #)
  
  den_label = #unique(#first(
    case_match(
      measure,
      c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
        "GESTATION AT TERMINATION") ~ "",
      "TEARS" ~ "Total number of women: ",
      #"GESTATION AT BIRTH" ~ paste0("Number of babies born at ", plotdata$formatted_name, ": "),
      .default = "Total number of births:"
    ),
  #)
  
  measure_label = #unique(#first(
    case_match(
      measure,
      "BOOKINGS" ~ "Number of pregnancies booked",
      "TERMINATIONS" ~ "Number of terminations",
      "GESTATION AT BOOKING" ~ "Average gestation at booking",
      "GESTATION AT TERMINATION" ~ "Average gestation at termination",
      "TEARS" ~ "Percentage of women (%)",
      .default = "Percentage of births (%)"
    ),
  #)
  
  median_label = #unique(#first(
    case_match(
      measure,
      c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
        "GESTATION AT TERMINATION") ~ "average to end Feb 2020",
      .default = "average to Oct-Dec 2019"
    ),
  #)
  
  extended_label = #unique(#first(
    case_match(
      measure,
      c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
        "GESTATION AT TERMINATION") ~ "projected average from Mar 2020",
      .default = "projected average from Jan-Mar 2020"
    ),
  #)
  
  yaxislabel = #unique(#first(
    case_match(
      measure,
      c("GESTATION AT BOOKING", "GESTATION AT TERMINATION") ~
        paste0(measure_label, " (weeks)"),
      .default = measure_label
    )
  )

return(plotdata)
  
  #return(c(num_label, den_label, measure_label, median_label, extended_label, yaxislabel))
  
}

add_hovertext <- function(plotdata) {
  
  plotdata <- plotdata %>% 
    mutate(
      mytext = case_match(
        measure,
        c("BOOKINGS", "TERMINATIONS") ~ paste0(
          "Month: ",
          format(date, "%b %Y"),
          "<br>",
          measure_label,
          ": ",
          prettyNum(measure, big.mark = ",")
        ),
        c("GESTATION AT BOOKING", "GESTATION AT TERMINATION") ~ paste0(
          "Month: ",
          format(date, "%b %Y"),
          "<br>",
          measure_label,
          ": ",
          format(measure,
                 digits = 1,
                 nsmall = 1),
          " weeks"
        ),
        "TEARS" ~ paste0(
          "Quarter: ", 
          quarter_label,
          "<br>",
          num_label,
          prettyNum(num, big.mark = ","),
          "<br>",
          den_label,
          prettyNum(den, big.mark = ","),
          "<br>",
          "Percentage of women",
          ": ",
          format(measure,
                 digits = 1,
                 nsmall = 1),
          "%"),
        c("INDUCTIONS", "TYPE OF BIRTH", "GESTATION AT BIRTH", "APGAR5")
        ~ paste0(
          "Quarter: ", 
          plotdata$quarter_label,
          "<br>",
          plotdata$num_label,
          prettyNum(num, big.mark = ","),
          "<br>",
          plotdata$den_label,
          prettyNum(den, big.mark = ","),
          "<br>",
          "Percentage of births",
          ": ",
          format(measure,
                 digits = 1,
                 nsmall = 1),
          "%")
      )
    )
  
  return(plotdata)
  
}


# Function to create the runcharts/timeseries charts
# Parameters:
# plotdata: dataframe with data to be plotted
# measure_value: variable to be plotted as black dots/lines
# hover: hovertext for the measure
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# trend: 5 or more points going up or going down are highlighted with a green line
# shift: orange circles for 6 or more points above or below the median
# yaxislabel: text to appear on y axis

creates_runcharts <- function(plotdata,
                              measure_value,
                              #hover = "mytext",
                              centreline = "median",
                              dottedline = "extended"){
                              #trend = "orig_trend",
                              #shift = "orig_shift",
                              #yaxislabel = "Percentage of births (%)"){
  
  y_max <- max(plotdata$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # temp fix
  
  # plotdata <-  
  #   plotdata %>% 
  #   mutate(shift = if_else(orig_shift == TRUE, measure, NA),
  #          trend = if_else(orig_trend == TRUE, measure, NA)
  #   )
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != "between 32 and 36 weeks" ~ FALSE,
    TRUE ~ TRUE)
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- case_when(
    first(plotdata$measure_cat) == "spontaneous vaginal births" ~ FALSE,
          first(plotdata$measure_cat) == "between 32 and 36 weeks" ~ FALSE,
    TRUE ~ include_legend)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
    # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = bookings_date_tickvals,
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals
   ) 
  
  select_date_ticktext <- switch( # tells plotly what text to show on ticks
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = bookings_date_ticktext,
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "INDUCTIONS" = SMR02_date_ticktext,
   "TYPE OF BIRTH" = SMR02_multiples_date_ticktext,
   "TEARS" = SMR02_date_ticktext,
   "GESTATION AT BIRTH" = SMR02_multiples_date_ticktext,
   "APGAR5" = SMR02_date_ticktext
   )
  
  #  select_date_tickvals <- case_match( # tells plotly where ticks will show
  #  first(plotdata$measure), 
  #  c("BOOKINGS", "GESTATION AT BOOKING") ~ bookings_date_tickvals,
  #  c("TERMINATIONS", "GESTATION AT TERMINATION") ~ terminations_date_tickvals,
  #  c("TYPE OF BIRTH", "GESTATION AT BIRTH") ~ SMR02_multiples_date_tickvals,
  #  .default = SMR02_date_tickvals
  #  )
  # 
  # select_date_ticktext <- case_match( # tells plotly what text to show on ticks
  #  first(plotdata$measure),
  #   c("BOOKINGS", "GESTATION AT BOOKING") ~ bookings_date_ticktext,
  #  c("TERMINATIONS", "GESTATION AT TERMINATION") ~ terminations_date_ticktext,
  #  c("TYPE OF BIRTH", "GESTATION AT BIRTH") ~ SMR02_multiples_date_ticktext,
  #  .default = SMR02_date_ticktext
  #  )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  
  yaxis_plots[["title"]] <- list(
    text = ~ #if_else(first(plotdata$measure) == "TEARS",
                     #"Number of women",
                     plotdata$yaxislabel,
    standoff = 30) # distance between axis and chart
  
  yaxis_plots[["tickformat"]] <- 
    if_else(first(plotdata$measure) %in% c("APGAR5", "TEARS"),
            ".1f",
            ",d")
  
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  runcharts <-
    plot_ly(
    data = plotdata,
    x = ~ date,
    y = ~ trend, # green trend line needs to be plotted first or it obliterates the others
    type = "scatter",
    mode = "lines",
    line = list(
      color = "lightgreen",
      width = 10
    ),
    name = orig_trend_label, # legend entry
    legendgroup = "trend",
    legendrank = 1003,
    showlegend = ~ include_trend_shift_legend,
    hovertext = "",
    hoverinfo = "none"
  ) %>%
    add_trace(
      y = ~ measure_value,
      mode = "lines+markers",
      line = list(
        color = "black", # black lines
        width = 1),
      marker = list(
        color = "black", # black dots
        size = 5),
      name = ~ str_to_lower(measure_label), # legend entry
      legendgroup = "measure",
      legendrank = 100,
      showlegend = include_legend,
      hovertext = ~ mytext,
      hoverinfo = "text"
    ) %>%
    add_trace(
      y = ~ get(centreline), # solid blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1),
      marker = NULL,
      name = ~ median_label, # legend entry
      legendgroup = "median",
      legendrank = 200,
      showlegend = ~ include_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    add_trace(
      y = ~ get(dottedline), # dotted blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1,
        dash = "4"
      ),
      marker = NULL,
      name = ~ extended_label, # legend entry
      legendgroup = "extended",
      legendrank = 300,
      showlegend = ~ include_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
      add_trace(
      y = ~ shift, # orange circles
      mode = "lines",
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 2),
      marker = NULL,
      name = orig_shift_label, # legend entry
      legendgroup = "shift",
      legendrank = 1004,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(title = list(text = paste0(plotdata$hbname, "<br>")),
                    tracegroupgap = 15,
                    orientation = "v",
                    x = 1.0,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    itemclick = FALSE)
    ) %>%
    config(displaylogo = F, displayModeBar = FALSE)

# adds "dummy" traces for multiple runcharts to force shift and trend legends to appear even if there
# are none in these charts

if(first(plotdata$measure_cat) %in% c("spontaneous vaginal births",
                                        "between 32 and 36 weeks")) {
  runcharts <- runcharts %>%
    add_trace(
    data = plotdata,
    x = ~ min(date), # fake trend to show legend even when no trend exists on chart
    y = ~ -5,
    mode = "lines",
    line = list(
      color = "lightgreen",
      width = 10
    ),
    marker = NULL,
    name = orig_trend_label, # legend entry
    legendgroup = "trend",
    legendrank = 600,
    showlegend = TRUE,
    hovertext = "",
    hoverinfo = "none"
    ) %>%
    add_trace(
      data = plotdata,
      x = ~ max(date), # fake shift to show legend even when no shift exists on chart
      y = ~ -5,
      mode = "lines",
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 1),
      marker = NULL,
      name = orig_shift_label, # legend entry
      legendgroup = "shift",
      legendrank = 700,
      showlegend = TRUE,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    layout(
      legend = list(tracegroupgap = 15
                  )
    )
}

# additional traces for the "special" Boards in GESTATION AT BOOKING measure

  if(first(plotdata$measure) == "GESTATION AT BOOKING" &
     first(plotdata$hbname) %in% c("NHS Forth Valley", "NHS Tayside")) {

    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata, !is.na(new_median)),
        y = ~ new_median, # green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1
        ),
        marker = NULL,
        name = ~ case_when(
          hbname == "NHS Forth Valley" ~
            paste0("average gestation from Mar 2021", "<br>", "to end Feb 2022"),
          hbname == "NHS Tayside" ~
            paste0("average gestation from Aug 2020", "<br>", "to end Jul 2021"),
          TRUE ~ ""
        ),
        legendrank = 1001,
        legendgroup = "additional median",
        hovertext = "",
        hoverinfo = "none"
      ) %>%
      add_trace(
        data = filter(plotdata, !is.na(new_extended)),
        y = ~ new_extended, # dotted green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ case_when(
          hbname == "NHS Forth Valley" ~ "projected average gestation from Mar 2022",
          hbname == "NHS Tayside" ~ "projected average gestation from Aug 2021",
          TRUE ~ ""
        ),
        legendrank = 1002,
        legendgroup = "additional extended",
        hovertext = "",
        hoverinfo = "none"
      ) %>%
      layout(legend = list(tracegroupgap = 15
                           )
      )
  }

  return(runcharts)
}

# Function to create the context charts (overall numbers relevant to the measure)
# Parameters:
# plotdata: the dataframe containing the data to be plotted
# date: the name of the "date" variable (may be "QUARTER" rather than "date")
# num: the main measure variable to be plotted as line (e.g. number of Apgar5 scores < 7)
# num_hover: the hovertext for the num
# den: the "total" measure variable to be plotted as a line (e.g. the total number of Apgar5 
# scores recorded)
# den_hover: the hovertext for the den
# yaxislabel: text to appear on y axis

creates_context_charts <- function(plotdata,
                                   date,
                                   num,
                                   num_hover = "mytext1",
                                   den,
                                   den_hover = "mytext2",
                                   yaxislabel = "Number of births"){
  
  y_max <- max(plotdata$den, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != ">= 32 and <= 36 weeks" ~ FALSE,
    TRUE ~ TRUE)
  
  # ensures ticks and tick labels correspond (different for ABC, TERMINATIONS, SMR02)
  
   select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = bookings_date_tickvals,
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "EXTREMELY PRE-TERM BIRTHS" = SMR02_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals
   ) 
  
  select_date_ticktext <- switch( # tells plotly what text to show on ticks
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = bookings_date_ticktext,
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "EXTREMELY PRE-TERM BIRTHS" = SMR02_date_ticktext,
   "INDUCTIONS" = SMR02_date_ticktext,
   "TYPE OF BIRTH" = SMR02_multiples_date_ticktext,
   "TEARS" = SMR02_date_ticktext,
   "GESTATION AT BIRTH" = SMR02_multiples_date_ticktext,
   "APGAR5" = SMR02_date_ticktext
   )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  yaxis_plots[["title"]] <- list(
    text = ~ if_else(first(plotdata$measure) == "TEARS",
                     "Number of women",
                     yaxislabel),
    standoff = 30) # distance between axis and chart

context_charts <-
    plot_ly(
      data = plotdata,
      x = ~ date,
      y = ~ num,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = selected_colours[2], # magenta line with x
        width = 2
      ),
      marker = list(
        color = selected_colours[2],
        symbol = "square-x-open"
      ),
      name = ~ case_match( # retrieves label of variable
        first(plotdata$measure),
        c("TYPE OF BIRTH", "GESTATION AT BIRTH") ~ "number of births",
        "APGAR5" ~ "babies with an Apgar5 score less than 7",
        "EXTREMELY PRE-TERM BIRTHS" ~ "births at 22-26 weeks in a hospital with a NICU",
      .default = str_to_lower(var_label(num))
      ),
      #legendgroup = "measure"
      legendrank = 200,
      showlegend = include_legend,
      hovertext = ~ get(num_hover),
      hoverinfo = "text"
    ) %>%
    add_trace(
      y = ~ den, # dashed purple line
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = selected_colours[1],
        width = 2
      ),
      marker = list(
        color = selected_colours[1],
        symbol = "circle"
      ),
      name = ~ case_match( # retrieves label of variable
        first(plotdata$measure),
        "APGAR5" ~ "babies with a known Apgar5 score",
        .default = str_to_lower(var_label(den))
      ), 
      #legendgroup = "median"
      legendrank = 100,
      showlegend = ~ include_legend,
      hovertext = ~ get(den_hover),
      hovertext = "text"
    ) %>%
    layout(
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(
        title = list(
          text = paste0(plotdata$hbname, "<br>")
          ),
        orientation = "v",
        x = 1.0,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        itemclick = FALSE),
        # groupclick = "togglegroup") 
      margin = list(pad = 30) # distance between axis and first data point
    ) %>% 
    config(displaylogo = F, displayModeBar = FALSE)

  return(context_charts)
}
