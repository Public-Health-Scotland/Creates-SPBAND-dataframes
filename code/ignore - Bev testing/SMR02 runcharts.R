
Selected <- tibble(HBType = "RESIDENCE",
                   HBName = "Scotland",
                   Date = "2020/21",
                   Subgroup = "Age group",
                   Measure_cat = "all caesarean births",
                   Gestation = "under 32 weeks",
                   Nicename = "under 32 weeks")

measure <- switch(runchart_data,
                  bookings_data = "Number of pregnancies booked",
                  gest_at_booking_data = "Average gestation at booking in weeks")



# a) data ----

runchart_data <- bookings_data

  data <- runchart_data %>%
  filter(hbname == Selected$HBName &
           hbtype == Selected$HBType) %>%
    set_variable_labels(
    measure = "measure",
    median = " average to end Feb 2020",
    extended = " projected average from Mar 2020"
  ) %>% 
  mutate(mytext = paste0("Month: ", 
                         format(date, "%b %Y"),
                         "<br>",
                         var_label(measure),
                         ": ",
                         prettyNum(measure, big.mark = ",")),
         orig_trend = FALSE, # to prevent this line being plotted
         orig_shift = FALSE # ditto
         )
  
  if (is.null(data()))
  {
  return()
  }


#   else {
#     data
#   }
# }

# b) chart ----

bookings_runcharts <- 
  
creates_runcharts(plotdata = runchart_data)

