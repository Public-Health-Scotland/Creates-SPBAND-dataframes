# a) data ----

max_plots_type_of_neonatal_care <- 3

BAPM_plotListNames = c("intensive care", "high dependency care", "special care") # placeholder for plots

gest_by_BAPM_runchart_data <- gest_by_BAPM_data |> 
  filter(subgroup_cat == "between 34 and 36 weeks (inclusive)") |> 
  mutate(num_label = paste0("Number of ", short_formatted_name, " babies admitted to ", measure_cat, ": "),
         den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
         measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
  ) %>% 
  set_variable_labels(
    median = " average to Oct-Dec 2019",
    extended_median = " projected average from Jan-Mar 2020",
    post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
    extended_post_pandemic_median = "projected average from Jul 2024"
  ) %>% 
  mutate(mytext = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         num_label,
                         prettyNum(num, big.mark = ","),
                         "<br>",
                         den_label,
                         prettyNum(den, big.mark = ","),
                         "<br>",
                         "Percentage of babies",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"),
         date = quarter
  ) |> 
  janitor::remove_empty("cols")

gest_by_BAPM_runchart_data <- ({ #reactive({
  # selects data
  
  #req(input$BAPM_gestation)
  
  data <- gest_by_BAPM_data |> 
    filter(subgroup_cat == Selected$BAPM_gestation) |> 
    mutate(num_label = paste0("Number of ", short_formatted_name, " babies admitted to ", measure_cat, ": "),
           den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
           measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
    ) |> 
    set_variable_labels(
      median = " average to Oct-Dec 2019",
      extended_median = " projected average from Jan-Mar 2020",
      post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
      extended_post_pandemic_median = "projected average from Jul 2024"
    ) |> 
    mutate(mytext = paste0("Quarter: ", 
                           quarter_label,
                           "<br>",
                           num_label,
                           prettyNum(num, big.mark = ","),
                           "<br>",
                           den_label,
                           prettyNum(den, big.mark = ","),
                           "<br>",
                           "Percentage of babies",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           "%"),
           date = quarter
    ) |> 
    janitor::remove_empty("cols")
  
  new_labels = unique(c(data$num_label, data$measure_label))
  
  data <- data %>% 
    split(.$measure_cat)
  
  for (i in seq_along(data)){
    var_label(data[[i]]$num) <- new_labels[[i]]
    var_label(data[[i]]$measure_value) <- new_labels[[i+3]]
  }
  
  for (i in seq_along(data)){
    data[[i]]$mytext <- paste0("Quarter: ",
                               data[[i]]$quarter_label,
                               "<br>",
                               var_label(data[[i]]$num), 
                               prettyNum(data[[i]]$num, big.mark = ","), # data[[i]]$num, 
                               "<br>",
                               var_label(data[[i]]$den),
                               prettyNum(data[[i]]$den, big.mark = ","), # data[[i]]$den,
                               "<br>",
                               "Percentage of births: ", # not MEASURE_LABEL - too long
                               format(data[[i]]$measure_value,
                                      digits = 1,
                                      nsmall = 1),
                               "%")
  }
  
  if (is.null(data)) {
    return()
  } else {
    data
  }
})

# b) chart ----

# Insert the right number of plot output objects into the web page

output$gestation_by_BAPM_runcharts <- renderUI({
  
  tagList(
    fluidRow(
      column(4, 
             h4("high dependency care"),
             plotlyOutput(BAPM_plotListNames[1])
      ),
      column(4, 
             h4("intensive care"),
             plotlyOutput(BAPM_plotListNames[2])
      ),
      column(4, 
             h4("special care"),
             plotlyOutput(BAPM_plotListNames[3])
      )
    ) # fluidRow
  )
})

for (i in 1:max_plots_type_of_neonatal_care) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    my_i <- i
    plotname <- BAPM_plotListNames[my_i]
    
    plot[[plotname]] <- #renderPlotly({
      creates_runcharts(plotdata = gest_by_BAPM_runchart_data[[my_i]]) %>%
        layout(xaxis = list(#dtick = "6",
          tickangle = -45),
          yaxis = list(range = c(0, y_max * 1.05)), # forces y axis to same value on all charts
          legend = list(orientation = "v",
                        x = 1.2,
                        y = 0.5, 
                        xref = "container",
                        xanchor = "left"))
    })
  #})
}

# c) chart title ----

output$type_of_birth_runcharts_title <- renderText({
  
  if_else(input$hbname == "NHS Borders",
          paste0("Board of ",
                 str_to_sentence(input$organisation),
                 ": ",
                 input$hbname,
                 "*"),
          paste0("Board of ",
                 str_to_sentence(input$organisation),
                 ": ",
                 input$hbname)
  )
})

