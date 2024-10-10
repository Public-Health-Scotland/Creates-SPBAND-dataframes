# Function to calculate the median of the measure variable over the different median_names
# Parameters:
# dataset: the name of the input dataframe
# date: the name of the grouping date variable (e.g. date, quarter)
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
  
  # fill-down median_name to ensure shifts are split in the correct places
  
  data <- data |>
    #group_by(dataset, hbtype, hbname, period, measure, measure_cat) %>%
    arrange({{date}}, .by_group = TRUE) %>%
    mutate(median_name = na.locf(median_name, na.rm = FALSE)
    )

  return(data)
  
}
