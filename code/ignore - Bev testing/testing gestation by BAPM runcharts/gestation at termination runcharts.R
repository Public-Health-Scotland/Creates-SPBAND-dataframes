#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
source("../functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage( # ui ---- 
                 
                 # Application title
                 titlePanel("Average gestation at termination runcharts"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                   sidebarPanel(width = 2,
                                uiOutput("hbnameControl"), 
                                uiOutput("organisationControl"),
                                #uiOutput("measureControl")
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     #DTOutput("table")
                     
                     # plotlyOutput("small_multiples",
                     #              height = "35em"
                     # )
                     # 
                     fluidRow(
                       column(12,
                              textOutput("gest_at_terminaton_runcharts_title"
                              ),
                              
                              br()
                              
                       ),
                       
                       column(12,
                              loading(
                                plotlyOutput("gest_at_termination_runcharts",
                                             height = "30em"
                                )
                              ),
                              
                              br()
                              
                       ),
                       
                       column(12,
                              p(textOutput("gest_at_termination_runcharts_footnote2") %>%
                                  tagAppendAttributes(style = "font-size:14px;
                                                   text-align: left;")
                              )
                       )
                     )
                   )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "NHS Ayrshire & Arran",
                             Measure = "BOOKINGS",
                             Date = "Jun 2019"
  )
  
  observeEvent(input$hbname, Selected$HBName <- input$hbname)
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  #observeEvent(input$measure, Selected$Measure <- input$measure)
  
  # select ORGANISATION (RESIDENCE or TREATMENT)

  output$organisationControl <-renderUI({ 
      radioButtons(
      inputId = "organisation",
      label = "View analyses by Board of",
      choiceNames = list("Residence", "Treatment"),
      choiceValues = list("RESIDENCE", "TREATMENT"),
      selected = "RESIDENCE",
      inline = FALSE
    )
    })
  
  # select hbname
  
  output$hbnameControl <- renderUI({
    pickerInput(
      #session = session,
      inputId = "hbname",
      label = "Select Board",
      choices = c("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
                  "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
                  "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
                  "NHS Shetland", "NHS Western Isles"),
      selected = "Scotland",
      options = pickerOptions(size = 10), # shows 10 boards and a scroll bar - will drop up and not get hidden?
      choicesOpt = list(
        style = rep("color: #3F3685;", 15) # PHS-purple text
      )
    )
  })
  
  #   # footnote for MIO table and Av. gestation at termination runcharts (Island Boards)
  # 
  # output$gest_at_termination_runcharts_footnote1 <- 
  #   output$gest_at_termination_runcharts_footnote2 <- renderText({
  #     if(input$hbname %in% island_names) {
  #       "* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for ‘Average gestation at termination’ are based on the data for those three Boards combined."
  #     }
  #   })
  
 gest_at_termination_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  Selected$HBName_terminations <- if_else(input$hbname %in% island_names,
                                          "NHS Orkney, NHS Shetland and NHS Western Isles*",
                                          input$hbname)
  
  data <- gest_at_termination_data %>%
    filter(hbname == Selected$HBName_terminations &
             hbtype == Selected$HBType) %>%
    set_variable_labels(
      measure_value = "Average gestation at termination",
      median = " average gestation to end Feb 2020",
      extended_median = " projected average gestation from Mar 2020"
    ) %>% 
    mutate(mytext = paste0("Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks")
    )

  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})
  
  output$table <- renderDT(server = FALSE,
                             datatable(inductions_small_multiples_data(),
                                       extensions = "Buttons",
                                       options = list(
                                         dom = "Bfrtip",
                                         buttons = "copyHtml5"
                                         )
                                       )
    )

output$gest_at_termination_runcharts <- renderPlotly({
  
creates_runcharts(plotdata = gest_at_termination_runchart_data(),
                  yaxislabel = "Average gestation at termination (weeks)") %>% 
  layout(xaxis = list(range =
                        range(gest_at_termination_runchart_data()$date) + c(months(-1), months(1)))
         )
})

output$gest_at_terminaton_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         Selected$HBName_terminations
  )
})

}

# Run the application 
shinyApp(ui = ui, server = server)
