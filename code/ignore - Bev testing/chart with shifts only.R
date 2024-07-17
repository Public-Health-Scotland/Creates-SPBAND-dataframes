extremely_preterm_control_chart <- 

  plot_ly(
    data = testdata_for_shifts3,
    x = ~ date_label,
    #y = ~ trend, # green trend line needs to be plotted first or it obliterates the others
    type = "scatter",
  #   mode = "lines",
  #   line = list(
  #     color = "lightgreen",
  #     width = 10
  #   ),
  #   name = orig_trend_label,
  #   legendgroup = "trend",
  #   legendrank = 1003,
  #   #showlegend = ~ include_trend_shift_legend,
  #   hovertext = "",
  #   hoverinfo = "none"
  # ) %>%
  #add_trace(
    y = ~ measure_value, # percentage
    mode = "lines+markers",
    line = list(
      color = "black", # black line
      width = 2
    ),
    marker = list(
      color = "black", # black dots
      size = 5
    ),
    name = ~ "percentage",
    hovertext = ~ paste0("Quarter: ",
                         date_label,
                         "<br>",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"
    ), # need to add hover text for percentage only
    hoverinfo = "text"
  ) %>%
  add_lines(
    y = ~ centreline, # mean (centreline)
    line = list(
      color = phs_colours("phs-blue"), # dotted blue line
      dash = "4",
      width = 2
    ),
    marker = NULL,
    name = "centreline",
    hovertext = ~ paste0("Centreline: ",
                         "<br>",
                         format(centreline,
                                digits = 1,
                                nsmall = 2),
                         "%"
    ),
    hoverinfo = "text"
  ) %>%
  # add_lines(
  #   y = ~ lower_warning_limit, # lower warning limit
  #   line = list(
  #     color = selected_colours[11], # phs-blue-80 line
  #     dash = "1",
  #     width = 2
  #   ),
  #   marker = NULL,
  #   name = "warning limits",
  #   legendgroup = "warning limits",
  #   showlegend = TRUE,
  #   hoverinfo = "none"
  # ) %>%
  # add_lines(
  #   y = ~ upper_warning_limit, # upper warning limit
  #   line = list(
  #     color = selected_colours[11], # phs-blue-80 line
  #     dash = "1",
  #     width = 2
  #   ),
  #   marker = NULL,
  #   name = "warning limits",
  #   legendgroup = "warning limits",
  #   showlegend = FALSE,
  #   hoverinfo = "none"
  # ) %>%
  # add_lines(
  #   y = ~ lower_control_limit, # lower control limit
  #   line = list(
  #     color = "red", # red line
  #     dash = "2",
  #     width = 2
  #   ),
  #   marker = NULL,
  #   name = "control limits",
  #   legendgroup = "control limits",
  #   showlegend = TRUE,
  #   hoverinfo = "none"
  # ) %>%
  # add_lines(
  #   y = ~ upper_control_limit, # upper control limit
  #   line = list(
  #     color = "red", # red line
  #     dash = "2",
  #     width = 2
  #   ),
  #   marker = NULL,
  #   name = "control limits",
  #   legendgroup = "control limits",
  #   showlegend = FALSE,
  #   hoverinfo = "none"
  # ) %>% 
  add_trace(
    y = ~ shift, # orange lines
    mode = "lines",
    line = list(
      color = "orange", # orange lines (prevents missing data warning)
      width = 2),
    marker = NULL,
    name = orig_shift_label,
    legendgroup = "shift",
    legendrank = 1004,
    #showlegend = ~ include_trend_shift_legend,
    hovertext = "",
    hoverinfo = "none"
  ) %>%
  layout(xaxis = orig_xaxis_plots,
         yaxis = orig_yaxis_plots)

extremely_preterm_control_chart
