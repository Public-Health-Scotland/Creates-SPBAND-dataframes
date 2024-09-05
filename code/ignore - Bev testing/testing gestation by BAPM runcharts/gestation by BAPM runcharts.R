#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# source("functions.R")
# source("as original runcharts.R")

# Define UI for application that draws a histogram
ui <- fluidPage( # ui ---- 
                 
                 # Application title
                 titlePanel("gestation by BAPM runcharts"),
                 
                 # Sidebar
                 sidebarLayout(
                   sidebarPanel(width = 2,
                                uiOutput("BAPM_subgroupControl")
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     fluidRow(
                       column(12,
                              h1("Scotland"
                              ),
                              
                              uiOutput("mytext"),
                              
                              br(),
                              
                              #DTOutput("table"),
                              
                              #plotlyOutput("chart")
                              
                              # plotlyOutput("small_multiples",
                              #              height = "35em"
                       )
                       
                       
                     ),
                     
                     column(12,
                            loading(
                              uiOutput("gestation_by_BAPM_runcharts"
                              )
                            ),
                            
                            br()
                            
                     )
                     
                     # column(12,
                     #        p(textOutput("gest_at_termination_runcharts_footnote2") %>%
                     #            tagAppendAttributes(style = "font-size:14px;
                     #                             text-align: left;")
                     #        )
                   )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "NHS Ayrshire & Arran",
                             Measure = "BOOKINGS",
                             Date = "Jun 2019",
                             BAPM_Subgroup_cat = "between 34 and 36 weeks (inclusive)"
  )
  
  #observeEvent(input$hbname, Selected$HBName <- input$hbname)
  #observeEvent(input$organisation, Selected$HBType <- input$organisation)
  observeEvent(input$BAPM_subgroup_cat, Selected$BAPM_Subgroup_cat <- input$BAPM_subgroup_cat)
  
  output$mytext <- renderText({ # for testing
    paste0("input = ", input$BAPM_subgroup_cat,
           br(),
           "Selected = ", Selected$BAPM_Subgroup_cat)
  })
  
  # select BAPM_gestation
  
  output$BAPM_subgroupControl <- renderUI({
    radioButtons(
      inputId = "BAPM_subgroup_cat",
      label = "Select gestation group",
      choiceNames = list("late pre-term",
                         "term and post-term"),
      choiceValues = list("between 34 and 36 weeks (inclusive)", "between 37 and 42 weeks (inclusive)"),
      selected = "between 34 and 36 weeks (inclusive)",
      inline = FALSE
    )
  })
  
  # a) data ----
  
  y_max_gestation_by_BAPM <- reactiveVal(0) # initialise y_max_gestation_by_BAPM
  
  max_plots_gestation_by_BAPM <- 3
  
  BAPM_plotListNames = c("intensive care", "high dependency care", "special care") # placeholder for plots
  
  gest_by_BAPM_runchart_data <- reactive({
    # selects data
    
    req(input$BAPM_subgroup_cat)
    
    data <- gest_by_BAPM_data |> 
      filter(subgroup_cat == Selected$BAPM_Subgroup_cat) |> 
      mutate(num_label = paste0("Number of ", short_formatted_name, " babies admitted to ", measure_cat, ": "),
             den_label = paste0("Total number of ", short_formatted_name, " babies: "), 
             measure_label = paste0("Percentage of ", short_formatted_name, " babies admitted to ", measure_cat, " (%)"),
             date = quarter
      ) |>  
      set_variable_labels(
        median = " average to Oct-Dec 2019",
        extended_median = " projected average from Jan-Mar 2020",
        post_pandemic_median = paste0("average from Jul 2022", "<br>", "to end Jun 2024"),
        extended_post_pandemic_median = "projected average from Jul 2024") 
    
    new_labels = c(unique(c(data$num_label, data$measure_label)), rep(unique(data$den_label), 3))
    
    new_max <- max(data$measure_value) # local maximum measure_value
    
    observeEvent(new_max, {   # update local maximum measure_value when subgroup_cat changes
      y_max_gestation_by_BAPM(new_max)
    }
    )
 
    data <- data %>% 
      split(.$measure_cat)
    
    for (i in seq_along(data)){
      var_label(data[[i]]$num) <- new_labels[[i]]
      var_label(data[[i]]$measure_value) <- new_labels[[i+3]]
      var_label(data[[i]]$den) <- new_labels[[i+6]]
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
                                 "Percentage of babies: ", # not MEASURE_LABEL - too long
                                 format(data[[i]]$measure_value,
                                        digits = 1,
                                        nsmall = 1),
                                 "%")
    }
    
    if (is.null(data()))
    {
      return()
    }
    
    else {
      data
    }
    })
    
    # b) chart ----
    
    # Insert the right number of plot output objects into the web page
    
  output$gestation_by_BAPM_runcharts <- renderUI({
    
    tagList(
      fluidRow(
        column(4,
               h4("intensive care"),
               plotlyOutput(BAPM_plotListNames[1])
               ),
        column(1),
        column(4,
               h4("high dependency care"),
               plotlyOutput(BAPM_plotListNames[2])
               )
        ), # fluidRow

        br(),

        fluidRow(
          column(7,
                 h4("special care"),
                 plotlyOutput(BAPM_plotListNames[3])
          )
        ) # fluidRow
      )
      })
      
  for (i in 1:max_plots_gestation_by_BAPM) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- BAPM_plotListNames[my_i]
      
      output[[plotname]] <- renderPlotly({
        creates_runcharts(plotdata = gest_by_BAPM_runchart_data()[[my_i]]) |> 
        layout(xaxis = list(#dtick = "6",
          tickangle = -45),
          yaxis = list(range = c(0, y_max_gestation_by_BAPM() * 1.05)), # forces y axis to same value on all charts
          legend = list(orientation = "v",
                        x = 1.2,
                        y = 0.5,
                        xref = "container",
                        xanchor = "left"))
      }) 
    }) 
  }

  }

# Run the application 
shinyApp(ui = ui, server = server)
