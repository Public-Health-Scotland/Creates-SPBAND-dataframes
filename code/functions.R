# Function to calculate the percentage of 2 numbers
# Parameters:
# x: numerator
# y: denominator

percentage  <- function(x, y, na.rm = TRUE) {
  x / y * 100
}

# Function to tally the number of a given variable
# Parameters:
# dataset: defaults to births (can be modified)
# variable: name of the variable to be counted
# date: date
# median_name: the name of the median period to calculate the median over
# subgroup: name of a subgroup (e.g. SIMD, gestation_group), default is NULL = no subgroup
# tally_var: name of the denominator variable (defaults to births)
# suffix: a character describing the nature of the measure (e.g. "%")
# measure: the name of the measure (e.g. "GESTATION AT BIRTH")

counts <- 
  function(dataset = births, variable, date = date, median_name, 
           subgroup = NULL, tally_var = births, suffix, measure){ 
    
    # key, key_measure_cat, key_measure_label){
    
    name <- substitute(subgroup)
    #print(name)

    data <- filter({{dataset}}, !is.na({{variable}})) # selects and filters dataset (removes NAs)
    
    if({{measure}} == "TERMINATIONS") { # TERMINATIONS has no grouping variable
      
      data <- data %>% 
        
        # aggregates numerator (num)
        
        select(dataset, hbtype, hbname, median_name, {{date}},
               period, {{subgroup}},
               {{tally_var}}) %>%
        group_by(dataset, hbtype, hbname, {{date}}, period, median_name, {{subgroup}}) %>%
        summarise(num = sum({{tally_var}})) %>% 
        mutate(measure_cat = "total")
      
    } else {
      
    # 
    # # if({{measure}} %in% c("BOOKINGS", "TERMINATIONS", "GESTATION AT BOOKING",
    # #                          "GESTATION AT TERMINATION")) { # only shown monthly
    # #   
    # #   data <- filter(data, period != "Q") # removes quarterly data
    # #     
    # #   } else { 
    # 
    # # if({{measure}} != "TYPE OF BIRTH") { # only shown quarterly except TOB for MCQIC
    # #     
    # #     data <- filter(data, period != "M") # removes monthly data except for TYPE OF BIRTH
    # #     }
    # #   } ##### redundant
    # 
      data <- data %>%
        
        # aggregates numerator (num) over specified group
        
        select(dataset, hbtype, hbname, median_name, {{date}},
               period, {{subgroup}}, measure_cat := {{variable}}, {{tally_var}}) %>%
        group_by(dataset, hbtype, hbname, {{date}}, period, median_name,
                 {{subgroup}}, measure_cat) %>%
        summarise(num = sum({{tally_var}}))
      
      data <- data %>%

        # pivots numerators from measure_cat to calculate totals

        pivot_wider(names_from = measure_cat,
                    names_prefix = "num_",
                    values_from = num,
                    values_fill = 0
                    ) %>%
        mutate(`num_total` = sum(across(where(is.numeric))),
               den = `num_total` - sum(across(contains("unknown"))), # den only includes "known" values - but this does include "other" gestations
               flag = sum(across(contains("unknown"))) == 0, # if there is no "unknown" column flag = TRUE
               `num_total exc. unknown` = den,
               ) %>%
        relocate(`num_total exc. unknown`,
                 .before = contains("unknown")
                 )
      
      if(sum(data$flag == 0)) { # i.e. there are no valid "unknowns"

        data <- select(data, - flag)
        
      } else {
        
        data <- select(data, -c(`num_total exc. unknown`, flag))
      }

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
    }

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
        measure = measure) %>%
        # key_measure_cat = measure_cat == key_measure_cat,
        # key_measure_ref = if_else(key_measure_cat == TRUE, key, NA),
        # key_measure_label = if_else(key_measure_cat == TRUE, key_measure_label, NA)) %>%
      rename(subgroup_cat = {{subgroup}}) |> 
      ungroup()

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
# date: date
# subgroup: name of a subgroup (e.g. SIMD, gestation_group), default is NULL = no subgroup
# measure_value: fixed (is the measure_value in every dataframe)

calculate_medians <- function(dataset, date = date, subgroup_cat = NULL, measure_value){ 
  
  data <- {{dataset}} |>  
    group_by(dataset, hbtype, hbname, median_name, period, measure, measure_cat, {{subgroup_cat}}) |>  # over different median periods
    mutate(median = if_else(!is.na(median_name),
                            median({{measure_value}}, na.rm = TRUE), 
                            NA), # median of measure_value - solid line on chart
           extended = median # extended median - dotted line on chart
           ) |> 
  ungroup(median_name) |> 
  mutate(extended = na.locf(extended, na.rm = FALSE) # extended is the same as median - dotted line on chart
         )
  
  # extended is used to compare measure_value to the median in the runchart_flags code
  # extended is then reset to NA where median exists (except for changeover point) to prevent double-plotting
  # the lines
  
  # fill-down median_name to ensure shifts are split in the correct places
  
  data <- data |>
    #group_by(dataset, hbtype, hbname, period, measure, measure_cat) %>%
    arrange({{date}}, .by_group = TRUE) %>%
    mutate(median_name = na.locf(median_name, na.rm = FALSE)
    )

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

    rename({{trend}}:=trend) %>%
    select(- trend_i)
  
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
   
# # Now calculate shifts (ensuring split medians, noted by median_period, are taken into account)

  dataset <-
    dataset %>%
    group_by(median_name, .add = TRUE) %>%

    mutate(
      shift_i = tidytable::case_when(
        (({{value}} > {{median}} & lag({{value}}, 1) > {{median}} &
            lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} &
            lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}})
         | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} &
              lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} &
              lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}})) ~ TRUE,
        TRUE ~ FALSE),

      shift = tidytable::case_when(
        shift_i == TRUE | lead(shift_i, 1) == TRUE | lead(shift_i, 2) == TRUE
        | lead(shift_i, 3) == TRUE | lead(shift_i, 4) == TRUE
        | lead(shift_i, 5) == TRUE  ~ TRUE,
        TRUE ~ FALSE)
    ) %>%

    rename({{shift}}:=shift) %>%
    select(- shift_i)

# There is a similar issue for shifts. There can't be one point in two shifts,
# but the last point in one can be adjacent to the first point in the next.
# Again we don't want to connect the lines.
#
# Problematic points are surrounded by other shift points and are on the
# opposite side of the median from the preceding point.

  dataset <-
    dataset %>%
    ungroup(median_name) |> 

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