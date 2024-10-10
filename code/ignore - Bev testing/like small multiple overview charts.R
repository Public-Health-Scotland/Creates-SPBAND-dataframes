# Function to read in data and split by measure, remove redundant columns

load_and_split_dataframe <- function(measure) {
  
  data <- filter(runchart_dataframe, measure == {{measure}}) %>% 
  janitor::remove_empty(., which = c("cols"), quiet = TRUE)
  
  return(data)
}

gest_by_BAPM_data <- load_and_split_dataframe("BAPM GEST+BAPM")

# a) data ----

# map input$BAPMgestation to visually nicer gestation group names for title and hover text

# initialise Selected$Nicename

Selected$Nicename <- "Late pre-term"
Selected$BAPMgestation <- "under 32 weeks"

observeEvent(input$gestation, Selected$Nicename <- case_when(
  input$gestation == "under 32 weeks" ~ "under 32 weeks gestation",
  input$gestation == "between 32 and 36 weeks (inclusive)" ~ paste0("32", "<sup>+0</sup>",
                                                         " to 36", "<sup>+6</sup>", " weeks gestation"),
  input$gestation == "under 37 weeks" ~ "under 37 weeks gestation",
  input$gestation == "42 weeks and over (inclusive)" ~ paste0("42", "<sup>+0</sup>", " weeks gestation and over")
  )
)

# map input$BAPMgestation to the values in subgroup_cat to enable filtering of the data frame

observeEvent(input$BAPMgestation, Selected$Subgroup_cat <- input$BAPMgestation)

gestation_by_BAPM_small_multiples_data <- reactive({
  # selects data
  
  req(input$BAPMgestation)
  
  data <- gest_by_BAPM_data %>%
    filter(subgroup_cat == Selected$Subgroup_cat) %>%
    mutate(mytext = paste0("Quarter: ", 
                           quarter_label,
                           "<br>",
                           "Number of ", string_to_lower()
                           measure_cat,
                           "<br>",
                           
                           "Percentage of births",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           "%"),
           date = quarter_label,
           hbgroup = factor(if_else(hbname %in% island_names, "island", "mainland"),
                            levels = c("mainland", "island"), ordered = TRUE)
    ) %>% 
    group_by(hbgroup, hbtype) %>% 
    mutate(y_max = max(measure_value)
    ) %>%
    ungroup()
  
  
  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
  
})