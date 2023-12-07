# Code to test function to create all runchart dataframes and all runcharts

Date <-  "Jun 2019"
HBType <- "RESIDENCE"
HBName <- "NHS ORKNEY"
Measure = "TERMINATIONS"

Selected <- data.frame(Date, HBType, HBName, Measure)

plotdata <- 
  data <- runchart_dataframe %>%
  filter(hbname == Selected$HBName &
           hbtype == Selected$HBType & 
           measure == Selected$Measure)

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

if (first(plotdata$measure) %in% c("TYPE OF BIRTH", "GESTATION AT BIRTH")) { 
  
  chartdata <- plotdata %>% 
    split(.$measure_cat)
  
  max_plots <- length(chartdata)
  plotnames <- c(names(chartdata))
  
  # save plots in a list object - can then refer to them separately
  
  plotList <- chartdata %>% 
    map(~{
      creates_runcharts(plotdata = .x)
    }) 

} else { 
  plotList <- creates_runcharts(plotdata)
  
}

plotList






  
