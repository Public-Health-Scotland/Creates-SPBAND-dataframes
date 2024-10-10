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
