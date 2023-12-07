HBnames <- unique(runchart_dataframe$hbname)
Measure_List <- unique(runchart_dataframe$measure)

ui <- fluidPage(
  tags$style(
  "table {
  font-family: Arial, Helvetica, sans-serif;
  font-size: small;
  }"
  ),
  
  # This is the dynamic UI for the table and plots
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("hbnameControl"), 
      uiOutput("organisationControl"),
      uiOutput("measureControl")
    ),
  
   mainPanel(
     DTOutput("table")
   )
  )
    
    #plotlyOutput("gest_at_booking_runcharts")
  )

server <- function(input, output) {
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "NHS Ayrshire & Arran",
                             Measure = "BOOKINGS",
                             Date = "Jun 2019"
                             )
  
  observeEvent(input$hbname, Selected$HBName <- input$hbname)
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  observeEvent(input$measure, Selected$Measure <- input$measure)
  
  output$hbnameControl <- renderUI({ # select hbname
    pickerInput(
      #session = session,
      inputId = "hbname",
      label = "Select Board",
      choices = HBnames,
      selected = "NHS Ayrshire & Arran"
    )
  })
  
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
  
  output$measureControl<- renderUI({ # select indicator
    pickerInput(
      #session = session,
      inputId = "measure",
      label = "Select Measure",
      choices = Measure_List,
      selected = "BOOKINGS"
    )
  })
  
    runchart_data <- reactive({
    # selects data

    #req(input$period)

    data <- runchart_dataframe %>%
      filter(hbname == Selected$HBName &
               hbtype == Selected$HBType &
               measure == Selected$Measure) %>% 
      arrange(desc(date))
      
  if (is.null(data()))
  {
  return()
  }

  else {
    data
  }
    })

    output$table <- renderDT(server = FALSE,
                             datatable(runchart_data(),
                                       extensions = "Buttons",
                                       options = list(
                                         dom = "Bfrtip",
                                         buttons = "copyHtml5"
                                         )
                                       )
    )
}

shinyApp(ui, server)