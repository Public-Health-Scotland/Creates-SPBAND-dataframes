####
# Provides a quick way to view the charts as seen in the full dashboard 
# via mini Scottish Pregnancy, Births and Neonatal Data dashboard (SPBAND)
# Bev Dodds
# 21 May 2024
# Latest update description: initialised code
# Type of script - Shiny app code linking to functions in dashboard project to reduce duplication
# Written/run on R Studio Server
# Version of R - 4.2.1
# Asks user for their name (to link to correct server folder) and refersh_date (to pull in the correct data)
# Produces a cut-down version of the SPBAND to look for inconsistencies
####

library(here)

here::here("code/miniapp", "miniapp.R")

# rstudioapi::executeCommand('activateConsole')
# 
# username <- readline("What is your name? ")

server_folder <- "https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/"

source(paste0(server_folder,
                "functions.R"), local = FALSE)

source(here::here("code/miniapp", "global.R"))

# TITLE ----

# appears top left

dashboardtitle <- tags$a(href = "https://www.publichealthscotland.scot/",
                         target="_blank",
                         tags$imag(src = "phs-logo.png",
                                   alt = "Public Health Scotland logo",
                                   width = 120)
)

# HEADER BAR ----

# forces dashboard name to top right

header <- dashboardHeader(
  title = dashboardtitle,
  #titleWidth = 290,
  tags$li(class = "dropdown",
          tags$p("SPBAND for checking") # this is a cut-down version to check quickly everything looks ok
  )
)

# MENU ----

topicmenu <- sidebarMenu(
  id = "topics",
  menuItem("Home",
           tabName = "home",
           icon = icon("info-circle", verify_fa = FALSE) %>% rem_aria_label()
           ),
  menuItem("Multi indicator overview",
           tabName = "multi_indicator_overview",
           icon = icon("tachometer-alt", verify_fa = FALSE) %>% rem_aria_label()
  ),
  menuItem("Pregnancy",
           icon = icon("person-pregnant", verify_fa = FALSE) %>% rem_aria_label(),
           menuSubItem("Number of pregnancies booked",
                       tabName = "pregnancies_booked",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Number of terminations",
                       tabName = "terminations",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at booking",
                       tabName = "gestation_at_booking",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at termination",
                       tabName = "gestation_at_termination",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           )
  ) %>% rem_menu_aria_label(),
  menuItem("Births and babies",
           icon = icon("baby", verify_fa = FALSE) %>% rem_aria_label(),
           menuSubItem("Location of extremely pre-term births",
                       tabName = "location_of_ex_pre_term",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Induction of labour",
                       tabName = "inductions",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Type of birth",
                       tabName = "type_of_birth",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Third- and fourth-degree perineal tears",
                       tabName = "perineal_tears",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at birth: pre- and post-term births",
                       tabName = "gestation_at_birth",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
  menuSubItem("Stillbirths and infant deaths",
              tabName = "stillbirths",
              icon = shiny::icon("angle-double-right") %>% rem_aria_label()
  ),
  menuSubItem("Apgar scores",
              tabName = "apgar_scores",
              icon = shiny::icon("angle-double-right") %>% rem_aria_label()
  )
  # ) %>% rem_menu_aria_label(),
  # menuItem("Infant feeding",
  #          tabName = "infant_feeding",
  #          icon = icon("person-breastfeeding", verify_fa = FALSE) |> rem_aria_label()
  # ) %>% rem_menu_aria_label()
  )
)

# SIDEBAR ----

sidebar <- dashboardSidebar(#width = 280,
  useShinyjs(),
  accessible_menu(topicmenu),
  uiOutput("organisationControl"), # Board of Residence/Treatment
  uiOutput("hbnameControl"), # Board name
  uiOutput("dateControl"), # FY/CY
  hidden(
    textInput(inputId = "topics",
              label = "",
              value = "home") # forces input$topics to initialise as "home" to make filters appear correctly
  )
)

# HOME ----

home <- tabItem(
  tabName = "home",
  
  fluidRow(
    
    h1("Welcome to the Scottish Pregnancy, Births and Neonatal Data dashboard",
       class = "smaller--h1"
    ),
    
    hr(),
    
    tabBox(title = "Home",
           
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset00",
           width = 12,
           
           tabPanel(title = "Information about this release",
             
             fluidRow(
               
               p(paste0("The data shown in this dashboard were extracted on ", pretty_refresh_date)
               ),
               
               p("The Excel downloads are: "),
               
               tags$ul(class = "bullet-points",
                       tags$li(excel_filenames[[1]]),
                       tags$li(excel_filenames[[2]]),
                       tags$li(excel_filenames[[3]]),
                       tags$li(excel_filenames[[4]]),
                       tags$li(excel_filenames[[5]]),
                       tags$li(excel_filenames[[6]]),
                       tags$li(excel_filenames[[7]]),
                       tags$li(excel_filenames[[8]]),
                       tags$li(excel_filenames[[9]]),
                       tags$li(excel_filenames[[10]]),
                       tags$li(excel_filenames[[11]]),
                       tags$li(excel_filenames[[12]])
                       
               ) # tags$ul
               
             ) # fluidRow
             
           ) # tabPanel
           
    ) # tabBox("Home")
    
  ) # fluidRow
  
) # tabItem("Home")

# MULTI INDICATOR OVERVIEW ----

multi_indicator_overview <- tabItem(
  tabName = "multi_indicator_overview",
  
  fluidRow(
    tabBox(title = "Multiple indicator overview",
           
           # The id lets us use input$tabset01 on the server to find the current tab
           id = "tabset01",
           width = 12,
           
           # "bullet" chart tab
           
           tabPanel(title = "Board comparison",
                    
                    fluidRow(
                      column(12,
                             textOutput("multi_indicator_chart_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Compare Boards by measure: hover your mouse over the dots to see individual Board values"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("multi_indicator_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("multi_indicator_chart",
                                                  height = "40em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for  ‘Average gestation at termination’ are based on the data for those three Boards combined.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("^ Shortened label for clarity. Full label is: % of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                             )
                      
                    ) # fluidRow
                    
           ), # tabPanel("Board comparison")
           
           # table tab
           
           tabPanel(title = "Individual Board",
                    
                    fluidRow(
                      column(12,
                             textOutput("multi_indicator_table_title"
                             ), 
                             
                             br()
                             
                      ),
                      
                      column(1, offset = 10,
                             downloadButton("multi_indicator_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      )
                    ), # fluidRow
                    
                    br(),
                    
                    fluidRow(
                      column(11,
                             loading(DTOutput("mytable"
                             )
                             ),
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for  ‘Average gestation at termination’ are based on the data for those three Boards combined.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("^ Shortened label for clarity. Full label is: % of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at between 37-42 weeks gestation who had a third- or fourth-degree perineal tear.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                             )
                      ) # fluidRow
                    
           ) # tabPanel ("Individual Board")
           
    ) # tabBox ("Multi indicator overview")
    
  ) # fluidRow
  
) # tabItem ("multi_indicator_overview")

# NUMBER OF PREGNANCIES BOOKED ----

pregnancies_booked <- tabItem(
  tabName = "pregnancies_booked",
  
  fluidRow(
    tabBox(title = "Number of pregnancies booked",
           
           # The id lets us use input$tabset10 on the server to find the current tab
           id = "tabset10",
           width = 12,
           
           # Timeseries
           
           tabPanel(title = "Individual Board", #value = "bookings_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("bookings_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Number of pregnancies booked for antenatal care"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("bookings_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("bookings_runcharts", # timeseries not runchart
                                                  height = "30em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                             
                      ) # column
                      
                    ) # fluidRow
                    
           ) # tabPanel("bookings_board")
           
    ) # tabBox ("Number of pregnancies booked")
    
  ) # fluidRow
  
) # tabItem("pregnancies_booked")

# NUMBER OF TERMINATIONS ----

terminations <- tabItem(
  tabName = "terminations",
  
  fluidRow(
    tabBox(title = "Number of terminations",
           
           # The id lets us use input$tabset11 on the server to find the current tab
           id = "tabset11",
           width = 12,
           
           # Timeseries
           
           tabPanel(title = "Individual Board", #value = "terminations_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("terminations_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Number of terminations"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("terminations_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("terminations_runcharts", # timeseries not runchart
                                                  height = "30em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"                             
                             )
                      )

                    ) # fluidRow
                    
           ) # tabPanel("terminations_board")
           
    ) # tabBox ("Number of terminations")
    
  ) #fluidRow
  
) # tabItem("terminations")

# GESTATION AT BOOKING ----

gestation_at_booking <- tabItem(
  tabName = "gestation_at_booking",
  
  fluidRow(
    tabBox(title = "Average gestation at booking",
           
           # The id lets us use input$tabset12 on the server to find the current tab
           id = "tabset12",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "gest_at_booking_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_booking_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at booking (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_booking_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("gest_at_booking_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_booking_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "gest_at_booking_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_booking_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at booking (based on completed weeks of
                               pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_booking_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_booking_runcharts",
                                            height = "35em"
                               )
                             ),
                             
                             br()
                             
                      ),

                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )

                      ) # fluidRow
                    
           ) # tabPanel("gest_at_booking_board")
           
    ) # tabBox ("Average gestation at booking")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_booking")

# GESTATION AT TERMINATION ----

gestation_at_termination <- tabItem(
  tabName = "gestation_at_termination",
  
  fluidRow(
    tabBox(title = "Average gestation at termination",
           
           # The id lets us use input$tabset13 on the server to find the current tab
           id = "tabset13",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "gest_at_termination_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_termination_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at termination (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_termination_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("gest_at_termination_small_multiples_mainland",  
                                            height = "32em"
                               )
                             )
                      ),
                      
                      br(),
                      
                      column(4,
                             loading(
                               plotlyOutput("gest_at_termination_small_multiples_island",  
                                            height = "13em"
                               )
                             )
                      ),
                      
                      column(8,
                             br(),
                             
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for ‘Average gestation at termination’ are based on the data for those three boards combined.",
                               class = "notes-style"
                             ),
                             
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_termination_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "gest_at_termination_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_terminaton_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at termination (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_termination_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
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
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                             )
                      
                      ) # fluidRow
                    
           ) # tabPanel("gest_at_termination_board")
           
    ) # tabBox ("Average gestation at termination")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_termination")

# PRE-TERM BIRTHS ----

location_of_ex_pre_term <- tabItem(
  tabName = "location_of_ex_pre_term",
  
  fluidRow(
    tabBox(title = "Location of extremely pre-term births",
           
           # The id lets us use input$tabset20 on the server to find the current tab
           id = "tabset20",
           width = 12,
           
           # Control chart and context chart
           
           tabPanel(title = "Scotland", #value = "pre-term_births_control_chart",
                    
                    fluidRow(
                      column(12,
                             textOutput("extremely_preterm_control_chart_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of births at 22-26 weeks gestation resulting in a live born baby that occurred in a hospital with a neonatal intensive care unit (NICU) on site"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("extremely_preterm_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("extremely_preterm_control_chart",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of births at 22-26 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("extremely_preterm_context_chart",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )

                    ) # fluidRow
                    
           ) # tabPanel("pre-term_births_control_chart")
           
    ) # tabBox("Location of extremely pre-term births")
    
  ) # fluidRow
  
) # tabItem ("location_of_ex_pre_term")

# INDUCTIONS ----

inductions <- tabItem(
  tabName = "inductions",
  
  fluidRow(
    tabBox(title = "Induction of labour",
           
           # The id lets us use input$tabset21 on the server to find the current tab
           id = "tabset21",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "induction_of_labour_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("inductions_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at 37-42 weeks gestation that followed induction of labour"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("inductions_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("inductions_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )

                    ) # fluidRow
                    
           ), # tabPanel("induction_of_labour_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "induction_of_labour_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("inductions_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at 37-42 weeks gestation
                                that followed induction of labour"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("inductions_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("inductions_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                      ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("inductions_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel("induction_of_labour_board")
           
    ) # tabBox("Induction of labour")
    
  ) # fluidRow
  
) # tabItem ("inductions")

# TYPE OF BIRTH ----
# previously METHOD OF DELIVERY

type_of_birth <- tabItem(
  tabName = "type_of_birth",
  
  fluidRow(
    tabBox(title = "Type of birth",
           
           # The id lets us use input$tabset22 on the server to find the current tab
           id = "tabset22",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "type_of_birth_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("type_of_birth_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p(htmlOutput("type_of_birth_small_multiples_sub_title"
                             )
                             )
                      ),
                      
                      column(1,
                             downloadButton("type_of_birth_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(10,
                             loading(
                               plotlyOutput("type_of_birth_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(2,
                             br(),
                             uiOutput("typeofbirthControl"
                             )
                      ),
                      
                      column(12,
                             p(textOutput("Borders_caesarean_footnote1") %>%
                                 tagAppendAttributes(style = "font-size:14px;
                                                   text-align: left;")
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("type_of_birth_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board",  #value = "type_of_birth_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("type_of_birth_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at any gestation that were"
                             ),
                             
                             br()
                      ),
                      
                      column(1, 
                             downloadButton("type_of_birth_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("type_of_birth_runcharts",
                                            height = "50em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(textOutput("Borders_caesarean_footnote2") %>%
                                 tagAppendAttributes(style = "font-size:14px;
                                                   text-align: left;")
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )

                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births at any gestation by type of birth"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("type_of_birth_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(textOutput("Borders_caesarean_footnote3") %>%
                                 tagAppendAttributes(style = "font-size:14px;
                                                   text-align: left;")
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )

                    ) # fluidRow
                    
           ), # tabPanel("type_of_birth_board")
           
    ) # tabBox("Type of birth")
    
  ) # fluidRow
  
) # tabItem ("type_of_birth")

# PERINEAL TEARS ----

perineal_tears <- tabItem(
  tabName = "perineal_tears",
  
  fluidRow(
    tabBox(title = "Perineal tears",
           
           # The id lets us use input$tabset23 on the server to find the current tab
           id = "tabset23",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "tears_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("tears_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear"
                             )
                      ),
                      
                      column(1,
                             downloadButton("tears_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("tears_small_multiples",  
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("tears_overview")
           
           # Individual Board
           
           tabPanel("Individual Board", #value = "tears_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("tears_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear"
                             )
                      ),
                      
                      column(1,
                             downloadButton("tears_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("tears_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("tears_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel("tears_board")
           
    ) # tabBox ("Perineal tears")
    
  ) # fluidRow
  
) # tabItem ("perineal_tears")

# GESTATION AT BIRTH ----

gestation_at_birth <- tabItem(
  tabName = "gestation_at_birth",
  
  fluidRow(
    tabBox(title = "Gestation at birth",
           
           # The id lets us use input$tabset24 on the server to find the current tab
           id = "tabset24",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", # value = "gest_at_birth_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_birth_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p(htmlOutput("gest_at_birth_small_multiples_sub_title"
                             )
                             )
                      ),
                      
                      column(1,
                             downloadButton("gest_at_birth_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(10,
                             loading(
                               plotlyOutput("gest_at_birth_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(2,
                             br(),
                             uiOutput("gestationControl"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_birth_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board",  #value = "gest_at_birth_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_birth_runcharts_title"
                             ),
                             
                             br()
                             
                      ),

                      column(10,
                             p("Percentage of singleton live births that were at the stated gestation",
                               style = "font-weight: normal;
                                         text-align: left;"
                             )
                      ),
                      
                      column(1,
                             downloadButton("gest_at_birth_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_birth_runcharts",
                                            height = "50em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births that were at the stated gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_birth_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_birth_board")
           
    ) # tabBox("Gestation at birth")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_birth")

# STILLBIRTHS AND INFANT DEATHS ----

stillbirths <- tabItem(
  tabName = "stillbirths",
  
  fluidRow(
    tabBox(title = "Stillbirths and infant deaths",
           
           # The id lets us use input$tabset25 on the server to find the current tab
           id = "tabset25",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Scotland", # value = "stillbirths_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("stillbirths_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Quarterly rates of stillbirths and infant deaths*"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("stillbirths_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("stillbirths_runcharts",
                                            height = "60em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Rates are per 1,000 live births except for stillbirths and extended perinatal deaths, where the rates are per 1,000 total (live and still) births.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data first published by National Records of Scotland (NRS) on ", NRS_published_date),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel("stillbirths_overview")
           
    ) # tabBox("Stillbirths and infant deaths")
    
  ) # fluidRow
  
) # tabItem ("stillbirths")

# APGAR5 SCORES ----

apgar_scores <- tabItem(
  tabName = "apgar_scores",
  
  fluidRow(
    tabBox(title = "Low Apgar5 scores",
           
           # The id lets us use input$tabset26 on the server to find the current tab
           id = "tabset26",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "apgar5_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("apgar5_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton babies born alive at 37-42 weeks gestation that had a 5-minute Apgar score of less than 7"
                             )
                      ),
                      
                      column(1,
                             downloadButton("apgar5_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("apgar5_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("apgar5_overview")
           
           # Individual Board
           
           tabPanel("Individual Board", #value = "apgar5_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("apgar5_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton babies born alive at 37-42 weeks gestation that had a 5-minute Apgar score of less than 7"
                             )
                      ),
                      
                      column(1,
                             downloadButton("apgar5_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("apgar5_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton babies born alive at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("apgar5_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel("apgar5_board")
           
    ) # tabBox("Low Apgar5 scores")
    
  ) # fluidRow
  
) # tabItem ("apgar_scores")


# BODY ----

body <- dashboardBody(
  
  use_theme(mytheme), # <-- use the theme to change colours
  tags$head(includeCSS("www/styles.css")),
  
  tabItems(
    home,
    multi_indicator_overview,
    pregnancies_booked,
    terminations,
    gestation_at_booking,
    gestation_at_termination,
    location_of_ex_pre_term,
    inductions,
    type_of_birth,
    perineal_tears,
    gestation_at_birth,
    stillbirths,
    apgar_scores
    #infant_feeding
  ) # tabItems
  
) # dashboardBody

# ui ----

ui <- 
  #secure_app( # uncomment if want password protection
  tagList( #needed for shinyjs
    #useShinyjs(),  # Include shinyjs
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0/css/all.css);"),
    tags$head(
      HTML(
        "<html lang='en'>"),
      tags$link(rel="shortcut icon",
                href="favicon_phs.ico"), # Icon for browser tab
      tags$title("Scottish Pregnancy, Births and Neonatal Dashboard"),
    ),
    # Including Google analytics
    # includeScript("google-analytics.js")),
    
    dashboardPage(

      header,
      
      sidebar,
      
      body
      
    ) # dashboardPage
  
    
   ) # tagList

#) # secure_app # uncomment if want password protection

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #cdata <- session$clientData
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "Scotland",
                             Date = "2020/21",
                             Subgroup = "Age group",
                             Measure_cat = "all caesarean births",
                             Gestation = "under 32 weeks",
                             Nicename = "under 32 weeks")
  
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  
  observeEvent(input$hbname, Selected$HBName <- input$hbname)
  
  observeEvent(input$date, Selected$Date <- input$date)
  
  observeEvent(input$gestation, Selected$Gestation <- input$gestation)
  
  # observeEvent(input$link_to_patterns, {
  #   updateTabsetPanel(getDefaultReactiveDomain(),
  #                     "tabset00",
  #                     "patterns")
  # })
  
  # this observeEvent sets the current tabset back to the first tabPanel when a new tabset is selected from the
  # menu - this is needed to trigger the filter selections correctly
  
  observeEvent(input$topics,

               if (input$topics %in% names(tabnames)) {

                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset00", # home
                 #                   "instructions") 
                 
                 # removed as this was switching back to "instructions" if you clicked on another tab too soon after opening the dashboard - should not affect the workings as filters are not dependent on the tab selected here

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset01", # multi_indicator_overview
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset10", # pregnancies_booked
                                   "Individual Board")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset11", # terminations
                                   "Individual Board")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset12", # gestation_at_booking
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset13", # gestation_at_termination
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset20", # location_of_ex_pre_term
                                   "Scotland")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset21", # inductions
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset22", # type_of_birth
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset23", # perineal_tears
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset24", # gestation_at_birth
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset25", # stillbirths
                                   "Scotland")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset26", # apgar_scores
                                   "Board comparison")

                 Selected$Tabset <- "Board comparison" # forces reset for HBname filter where there is a Board comparison tab
               }
  )
  
  observeEvent(input$tabset00, Selected$Tabset <- input$tabset00)
  observeEvent(input$tabset01, Selected$Tabset <- input$tabset01)
  observeEvent(input$tabset10, Selected$Tabset <- input$tabset10)
  observeEvent(input$tabset11, Selected$Tabset <- input$tabset11)
  observeEvent(input$tabset12, Selected$Tabset <- input$tabset12)
  observeEvent(input$tabset13, Selected$Tabset <- input$tabset13)
  observeEvent(input$tabset20, Selected$Tabset <- input$tabset20)
  observeEvent(input$tabset21, Selected$Tabset <- input$tabset21)
  observeEvent(input$tabset22, Selected$Tabset <- input$tabset22)
  observeEvent(input$tabset23, Selected$Tabset <- input$tabset23)
  observeEvent(input$tabset24, Selected$Tabset <- input$tabset24)
  observeEvent(input$tabset25, Selected$Tabset <- input$tabset25)
  observeEvent(input$tabset26, Selected$Tabset <- input$tabset26)

    # observeEvent(input$tabset31, Selected$Tabset <- input$tabset31)  # testing whether can jump to a tabset
  
  # select ORGANISATION (RESIDENCE or TREATMENT)
  
  output$organisationControl <- renderUI({ 
    hidden(
      radioButtons(
        inputId = "organisation",
        label = "View analyses by Board of",
        choiceNames = list("Residence", "Treatment"),
        choiceValues = list("RESIDENCE", "TREATMENT"),
        selected = "RESIDENCE",
        inline = FALSE
      )
    )
  })
  
  # determines whether the ORGANISATION filter should show or not
  
  observe({
    toggleElement(id = "organisation",
                  condition = (input$topics %in% show_org &
                                 Selected$Tabset != "About this measure")
    )
  })
  
  # select hbname
  
  output$hbnameControl <- renderUI({
    hidden(
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
    )
  })
  
  # determines whether the hbname filter should show or not
  
  observe(toggleElement(id = "hbname",
                        condition = ((input$topics %in% show_HBname &
                                        Selected$Tabset != "About this measure") |
                                       (input$topics %in% show_HBname2 &
                                          !Selected$Tabset %in% c("Board comparison", "About this measure"))
                        )
  )
  )
  
  # select date (financial year or calendar year)
  
  output$dateControl <- renderUI({ 
    hidden(
      pickerInput(
        inputId = "date",
        label = "Choose time period",
        choices = factor_labels_year,
        selected = "2023",
        width = "fit",
        choicesOpt = list(
          style = rep("color: #3F3685;",
                      length(factor_labels_year))
        )
      )
    )
  })
  
  # determines whether the date filter should show or not
  
  observe(
    toggleElement(id = "date",
                  condition = (input$topics == "multi_indicator_overview")
    )
  )
  
  # select TYPEOFBIRTH - appears on Type of Birth "Board comparison" view only
  
  output$typeofbirthControl <- renderUI({ 
    radioButtons(
      inputId = "tob",
      label = "Choose type of birth",
      choiceNames = list("all caesarean", "planned caesarean", "unplanned caesarean",
                         "assisted", "spontaneous vaginal"),
      choiceValues = list("all caesarean births", "planned caesarean births",
                          "unplanned caesarean births", "assisted vaginal births",
                          "spontaneous vaginal births"),
      selected = "all caesarean births",
      inline = FALSE
    )
  })
  
  # select Gestation at Birth - appears on Gestation at Birth "Board comparison" view only
  
  output$gestationControl <- renderUI({ 
    radioButtons(
      inputId = "gestation",
      label = "Choose gestation at birth",
      choiceNames = list("under 32 weeks",
                         HTML(
                           paste0("32", tags$sup("+0"), " to 36", tags$sup("+6"), " weeks"
                           )
                         ),
                         "under 37 weeks",
                         HTML(
                           paste0("42", tags$sup("+0"), " weeks and over"
                           )
                         )
      ),
      choiceValues = list("under 32 weeks", "between 32 and 36 weeks (inclusive)", "under 37 weeks", "42 weeks and over (inclusive)"
      ),
      selected = "under 32 weeks",
      inline = FALSE
    )
  })
  
  # builds Version table
  
  `Version` <- c("1.0", "1.1", "1.2", "1.3", "1.4")
  `Date` <- c("3 Oct 2023", "9 Nov 2023", "15 Feb 2024", "2 Apr 2024", "2 Jul 2024")
  `Change` <- c("First public release of SPBAND",
                "Amended Home - How to use this dashboard",
                "Updated links and standardised titles, labels and metadata",
                "Corrected the medians and shifts for NHS Forth Valley and NHS Tayside in the ‘Gestation at booking’ measure;
              replaced CSV download files with accessible Excel download files;
              updated links and standardised titles, labels, legends and metadata",
              "Added aggregated values for the Island Boards in the ‘Gestation at termination’ measure - these Boards are now also represented on the Multi Indicator Overview for this measure;
              revised the y-axis scales for the Island Boards in the small multiple charts (where possible) to make the mainland Boards' variation easier to see;
              removed the 'dots' from the monthly small multiple charts (i.e. the ‘Gestation at booking’ and ‘Gestation at termination’ measures); added notes describing the issue with NHS Borders planned and unplanned caesarean birth rates"
  )
  
  version_info <- tibble(`Version`, `Date`, `Change`)
  
  output$version_tbl <- renderTable(version_info, 
                                    striped = TRUE,
                                    bordered = TRUE)
  
# footnote text for Forth Valley and Tayside revised medians in Gestation at booking measure
  
gest_at_booking_revised_median_text <-  # was gest_at_booking_correction_text

  tags$li(class= "bullet-points",
          "A green line shows a revised overall average (median) of the mean gestation at booking each month during a period after changes were made to the process for recording booking. The green line is dashed where the revised average is projected outside that time range."
  )

  output$gest_at_booking_footnote <- renderUI({
    tagList(gest_at_booking_revised_median_text)
  })

  observeEvent(input$hbname,

               toggleElement(id = "gest_at_booking_footnote",
                             condition = input$hbname %in% c("NHS Forth Valley", "NHS Tayside"))
  )
  
  # footnote Av. gestation at termination runcharts (when Island Boards are selected)

  output$gest_at_termination_runcharts_footnote1 <- renderText({
      if(input$hbname %in% island_names) {
        "* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for ‘Average gestation at termination’ are based on the data for those three Boards combined."
      }
    })
  
 # footnote for Type of Birth - Board comparison - Borders caesarean anomalies
  
  observeEvent(input$tob,
  
  output$Borders_caesarean_footnote1 <- renderText({
    if(grepl("planned", input$tob)) {
      "* Data for NHS Borders for planned and unplanned caesarean births show some unusual patterns from April 2022 to date. We have been liaising with NHS Borders and believe this to be a recording issue rather than a true reflection of the numbers. We are working with the Board to try to further understand and rectify the issue."
    }
  })
  )
  
  # footnote for Type of Birth - Individual Board - Borders caesarean anomalies

  output$Borders_caesarean_footnote2 <- output$Borders_caesarean_footnote3 <- renderText({
      if(input$hbname == "NHS Borders") {
        "* Data for NHS Borders for planned and unplanned caesarean births show some unusual patterns from April 2022 to date. We have been liaising with NHS Borders and believe this to be a recording issue rather than a true reflection of the numbers. We are working with the Board to try to further understand and rectify the issue."
      }
    })

  # output$mytext <- renderText({ # for testing
  #   paste0("Topic = ", input$topics) 
  # })

  # observeEvent(input$link_to_patterns, print(input$link_to_patterns))
  # observeEvent(input$topics, print(paste0("Topic = ", input$topics)))
  # observe(print(paste0("Selected Tabset = ", Selected$Tabset)))
  # observe(print(paste0("Measure_cat = ", Selected$Measure_cat)))
  # observe(print(paste0("Home: ", input$tabset00)))
  # observe(print(paste0("MIO: ", input$tabset01)))
  # observe(print(paste0("Pregnancies booked: ", input$tabset10)))
  # observe(print(paste0("Terminations: ", input$tabset11)))
  # observe(print(paste0("Gestation at booking: ", input$tabset12)))
  # observe(print(paste0("Gestation at termination: ", input$tabset13)))
  # observe(print(paste0("Location of extremely pre-term births: ", input$tabset20)))
  # observe(print(paste0("Induction of labour: ", input$tabset21)))
  # observe(print(paste0("Type of birth: ", input$tabset22)))
  # observe(print(paste0("Perineal tears: ", input$tabset23)))
  # observe(print(paste0("Gestation at birth: ", input$tabset24)))
  # observe(print(paste0("Stillbirths: ", input$tabset25)))
  # observe(print(paste0("Apgar scores: ", input$tabset26)))
  
  # this section tells the app where to find the code for each tab
  
  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Multi%20indicator%20overview/Multi%20indicator%20overview%20chart.R", local = environment())
  
  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Multi%20indicator%20overview/Multi%20indicator%20overview%20table.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Multi%20indicator%20overview/Multi%20indicator%20overview%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Antenatal%20booking/Antenatal%20bookings%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Terminations/Terminations%20runcharts.R", local = environment())
  
  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Antenatal%20booking/Average%20gestation%20at%20booking%20small%20multiples.R", local = environment())
  
  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Antenatal%20booking/Average%20gestation%20at%20booking%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Antenatal%20booking/Average%20gestation%20at%20booking%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Terminations/Average%20gestation%20at%20termination%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Terminations/Average%20gestation%20at%20termination%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Terminations/Average%20gestation%20at%20termination%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Extremely%20preterm/Extremely%20preterm%20control%20chart.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Extremely%20preterm/Extremely%20preterm%20context%20chart.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Extremely%20preterm/Extremely%20preterm%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Inductions/Inductions%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Inductions/Inductions%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Inductions/Inductions%20context%20charts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Inductions/Inductions%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Type%20of%20birth/Type%20of%20birth%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Type%20of%20birth/Type%20of%20birth%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Type%20of%20birth/Type%20of%20birth%20context%20charts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Type%20of%20birth/Type%20of%20birth%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Tears/Tears%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Tears/Tears%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Tears/Tears%20context%20charts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Tears/Tears%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Gestation%20at%20birth/Gestation%20at%20birth%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Gestation%20at%20birth/Gestation%20at%20birth%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Gestation%20at%20birth/Gestation%20at%20birth%20context%20charts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Gestation%20at%20birth/Gestation%20at%20birth%20download%20data.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Stillbirths%20and%20infant%20deaths/Stillbirths%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Apgar5/Apgar5%20small%20multiples.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Apgar5/Apgar5%20runcharts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Apgar5/Apgar5%20context%20charts.R", local = environment())

  source("https://raw.githubusercontent.com/Public-Health-Scotland/SPBAND/main/Apgar5/Apgar5%20download%20data.R", local = environment())
  
}

shinyApp(ui, server)  
