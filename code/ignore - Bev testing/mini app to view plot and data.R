# source global settings
#source("/PHI_conf/MaternityBirths/Topics/MaternityHospitalSubmissions/Projects/20201028-WiderImpactsDashboardDeliveries/WI deliveries/code/SPBAND data code/global.R")


HBnames <- unique(runchart_dataframe$hbname)
Measure_List <- unique(runchart_dataframe$measure) 

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  tags$style(
    "table {
  font-family: Arial, Helvetica, sans-serif;
  font-size: small;
  }"
  ),
  
  # This is the dynamic UI for the table and plots
  
  sidebarLayout(
    
    sidebarPanel(width = 2,
      uiOutput("hbnameControl"), 
      uiOutput("organisationControl"),
      uiOutput("measureControl")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotlyOutput("runchart")),
        tabPanel("Data", DT::DTOutput("table"))
        )
      )
    )
  )

# Define server ----------------------------------------------------------------

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
  
  observeEvent(input$measure, print(paste0("Topic = ", input$measure)))
  
  runchart_data <- reactive({
    # selects data
    
    plotdata <- 
      data <- runchart_dataframe %>%
      filter(hbname == Selected$HBName &
               hbtype == Selected$HBType & 
               measure == Selected$Measure)
    
    if(length(plotdata > 0)) {
    
    if(first(plotdata$measure == "GESTATION AT BIRTH")) {
      
      plotdata <- filter(plotdata,
                         !measure_cat %in% c("between 37 and 41 weeks", "between 18 and 44 weeks")
      )
      
      plotdata <- left_join(
        plotdata,
        nicename,
        by = c("measure_cat" = "measure_cat_order")
      ) %>% 
        mutate(measure_cat = factor(measure_cat,
                                    levels = c("under 32 weeks",
                                               "between 32 and 36 weeks",
                                               "under 37 weeks",
                                               "42 weeks and over")
        )
        )
      
    } else {
      
      if(first(plotdata$measure == "TYPE OF BIRTH")) {
        
        plotdata <- plotdata %>% 
          mutate(measure_cat = factor(measure_cat,
                                      levels = c("all caesarean births",
                                                 "planned caesarean births",
                                                 "unplanned caesarean births",
                                                 "assisted births",
                                                 "spontaneous vaginal births")
          )
          )
        
      }
    }
    
    plotdata <- plotdata %>% 
      arrange(measure_cat) %>% 
      mutate(formatted_name = "")
    
    plotdata <- add_variable_labels(plotdata)
    
    plotdata <-  add_hovertext(plotdata) %>% 
      mutate(
        trend = if_else(
          measure %in% c("BOOKINGS", "TERMINATIONS"), # to prevent this line being plotted
          NA,
          trend
        ),
        shift = if_else(
          measure %in% c("BOOKINGS", "TERMINATIONS"), # ditto
          NA,
          shift
        )
      )
    } else {
      plotdata <- NULL
    }
    
    validate(
      need(!is.null(plotdata), "No data to show")
    )
    
    return(plotdata)

    # if (is.null(data()))
    # {
    #   return()
    # }
    # 
    # else {
    #   data
    # }
  })
  
  table_data <- reactive({
    
    runchart_data() %>% 
      select("dataset":"suffix")
    
  })
  
  output$runcharts <- renderUI({
    
    if (first(
      runchart_data()$indicator) %in% c("TYPE OF BIRTH", "GESTATION AT BIRTH")) { 
      
      chartdata <- runchart_data() %>% 
        split(.$indicator_cat)
      
      max_plots <- length(chartdata)
      plotnames <- c(names(chartdata))
      
      tagList(
        fluidRow(
          column(4, 
                 h4("under 32 weeks"),
                 plotlyOutput(plotList[[3]])
          ),
          column(1),
          column(7, 
                 h4(HTML(paste0("32", tags$sup("+0"),
                                " to 36", tags$sup("+6"), " weeks"))),
                 plotlyOutput(plotList[[2]])
          )
        ), # fluidRow
        
        br(),
        
        fluidRow(
          column(4,
                 h4( "under 37 weeks"),
                 plotlyOutput(plotList[[4]])
          ),
          column(1),
          column(4,
                 h4(HTML(paste0("42", tags$sup("+0"), " weeks and over"))),
                 plotlyOutput(plotList[[1]])
          )
        ) # fluidRow
      )
    }
  })
  
# define header names for the table
header.names <-
  c("Data source", "Board type", "Period", "Date", "Measure", "Sub-category", "Numerator", "Denominator",
    "Measure value", "Median", "Extended median", "New median", "New extended median", "Suffix"
  )

# the container parameter allows us to design the header of the table using CSS

my.container <- ({ #reactive
  withTags(
    table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th) #, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
          )
        )
      )
    )
})
# 
#   output$table <- 
#   renderDT({
#     my.table <- datatable(
#       table_data(),
#       #container = my.container,
#       options = my.options,
#       caption = htmltools::tags$caption("View the data for the measures by Board and Board Type: use the filters on the left",
#                   style = "color: #3F3685; margin-bottom: 0px;"
#                   ),
#       rownames = FALSE, # do not treat table row names as separate column
#       width = '100%', # ensure table remains within the dimensions of the container
#       height = '100%' # ensure table remains within the dimensions of the container
#     )
#     
#     # create specific table formatting customizations for table (round numbers to 1 d.p., font)
#     
#     my.table <- 
#       formatStyle(
#         my.table,
#         columns = colnames(table_data()),
#         color = "#3F3685",
#         fontFamily = "Arial",
#         fontSize = "14px")
#     
#     my.table <- formatRound(
#       my.table,
#       columns = c("measure_value", "median", "extended", "new_median", "new_extended"), 2)
#   })
  
  output$table <-
    renderDT(server = FALSE,
             datatable(table_data(),
                       container = my.container,
                       extensions = "Buttons",
                       options = list(
                         dom = "Bfrtip",
                         buttons = "copyHtml5"
                       ),
                       caption = htmltools::tags$caption("View the data for the measures by Board and Board Type: use the filters on the left",
                                                         style = "color: #3F3685; margin-bottom: 0px; font-size: 20px;"
                       ),
                       rownames = FALSE, # do not treat table row names as separate column
                       width = "100%", # ensure table remains within the dimensions of the container
                       height = "100%" # ensure table remains within the dimensions of the container
             )
    )
      
  output$plotList <- renderPlotly({
    
    if (first(
      runchart_data()$indicator) %in% c("TYPE OF BIRTH", "GESTATION AT BIRTH")) { 
      
      # chartdata <- runchart_data() %>% 
      #   split(.$indicator_cat)
      # 
      # max_plots <- length(chartdata)
      # plotnames <- c(names(chartdata))
      
      # save plots in a list object - can then refer to them separately
      
      chartdata %>% 
        map(~{
          creates_runcharts(plotdata = .x)
        })
      
    }
  }
  )
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
