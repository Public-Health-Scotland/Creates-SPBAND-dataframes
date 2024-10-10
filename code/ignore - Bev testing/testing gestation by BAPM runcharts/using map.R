library(purrr)

p <-
  mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ {
    plot_ly(
      data = .x,
      x = ~ hp,
      y = ~ mpg,
      type = "scatter"
    )
  }
  ) %>%
  subplot(margin = .05) 

library(dplyr)
iris %>%
  group_by(.$Species) %>%
  group_map(
    ~ plot_ly(
      data = .,
      x = ~ Sepal.Length,
      y = ~ Sepal.Width,
      color = ~ Species,
      type = "scatter",
      mode = "markers"
    )
  ) %>%
  subplot(nrows = 1,
          shareX = TRUE,
          shareY = TRUE)

gest_by_BAPM_runchart_data %>%
  group_by(.$measure_cat) %>%
  group_map(
    ~ plot_ly(
      data = .,
      x = ~ date,
      y = ~ measure_value,
      color = ~ measure_cat,
      type = "scatter",
      mode = "markers"
    )
  ) %>%
  subplot(nrows = 1,
          shareX = TRUE,
          shareY = TRUE)


my_function <- function(plotdata,
                        measure_value,
                        hover = "mytext",
                        centreline = "median",
                        dottedline = "extended_median",
                        yaxislabel = "Percentage of births (%)") {
  
  
  plotdata <- droplevels(plotdata) # drop unused factor levels
  
  y_max <- max(plotdata$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis
  
  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != "between 32 and 36 weeks (inclusive)" ~ FALSE,
    first(plotdata$measure) == "BAPM GEST+BAPM" &
      first(plotdata$measure_cat) != "special care" ~ FALSE,
    TRUE ~ TRUE)
  
  plot_ly(
    data = plotdata,
    x = ~ date,
    y = ~ measure_value,
    color = ~ measure_cat,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", # black lines
                width = 1),
    marker = list(color = "black", # black dots
                  size = 5),
    name = ~ case_match(
      first(plotdata$measure),
      "TYPE OF BIRTH" ~ "percentage of births (%)",
      "GESTATION AT BIRTH" ~ "percentage of births (%)",
      "BAPM GEST+BAPM" ~ "percentage of babies (%)"
      #.default = str_to_lower(var_label(measure_value))
    ),
    legendgroup = "measure_value",
    legendrank = 100,
    showlegend = include_legend,
    hovertext = ~ mytext,
    hoverinfo = "text"
  )
}

my_function(gest_by_BAPM_runchart_data[[1]])

gest_by_BAPM_runchart_data |> 
  map(~ {my_function(.x,)}) |> 
  subplot(nrows = 1,
          shareX = FALSE,
          shareY = TRUE)

droplevels(gest_by_BAPM_runchart_data[[1]])

