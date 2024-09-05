# Function to create the runcharts/timeseries charts
# Parameters:
# plotdata: dataframe with data to be plotted
# measure_value: variable to be plotted as black dots/lines
# hover: hovertext for the measure_value
# centreline: variable to be plotted as a solid blue line
# dottedline: variable to be plotted as a dotted blue line
# trend: green squares for 5 or more points going up or going down
# shift: orange circles for 6 or more points above or below the median
# yaxislabel: text to appear on y axis

creates_runcharts <- function(plotdata,
                              measure_value,
                              hover = "mytext",
                              centreline = "median",
                              dottedline = "extended_median",
                              yaxislabel = "Percentage of births (%)"){
  
  plotdata <- droplevels(plotdata) # drop unused factor levels
  
  y_max <- max(plotdata$measure_value, na.rm = TRUE) # allows a margin to be set around y-axis
  
  # temp fix
  
  # plotdata <-  
  #   plotdata %>% 
  #   mutate(shift = if_else(orig_shift == TRUE, measure_value, NA),
  #          trend = if_else(orig_trend == TRUE, measure_value, NA)
  #   )
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)

  include_legend <- case_when(
    first(plotdata$measure) == "TYPE OF BIRTH" &
      first(plotdata$measure_cat) != "spontaneous vaginal births" ~ FALSE,
    first(plotdata$measure) == "GESTATION AT BIRTH" &
      first(plotdata$measure_cat) != "between 32 and 36 weeks (inclusive)" ~ FALSE,
    TRUE ~ TRUE)
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- case_when(
    first(plotdata$measure_cat) == "spontaneous vaginal births" ~ FALSE,
          first(plotdata$measure_cat) == "between 32 and 36 weeks (inclusive)" ~ FALSE,
    TRUE ~ include_legend)
  
  select_date_tickvals <- switch( # tells plotly where ticks will show
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_tickvals,
   "GESTATION AT BOOKING" = gest_at_booking_date_tickvals, # temp
   "TERMINATIONS" = terminations_date_tickvals,
   "GESTATION AT TERMINATION" = terminations_date_tickvals,
   "INDUCTIONS" = SMR02_date_tickvals,
   "TYPE OF BIRTH" = SMR02_multiples_date_tickvals,
   "TEARS" = SMR02_date_tickvals,
   "GESTATION AT BIRTH" = SMR02_multiples_date_tickvals,
   "APGAR5" = SMR02_date_tickvals
   ) 
  
  select_date_ticktext <- switch( # tells plotly what text to show on ticks
   first(plotdata$measure), 
   "BOOKINGS" = bookings_date_ticktext,
   "GESTATION AT BOOKING" = gest_at_booking_date_ticktext, # temp
   "TERMINATIONS" = terminations_date_ticktext,
   "GESTATION AT TERMINATION" = terminations_date_ticktext,
   "INDUCTIONS" = SMR02_date_ticktext,
   "TYPE OF BIRTH" = SMR02_multiples_date_ticktext,
   "TEARS" = SMR02_date_ticktext,
   "GESTATION AT BIRTH" = SMR02_multiples_date_ticktext,
   "APGAR5" = SMR02_date_ticktext
   )
  
  # adds an asterisk to these Board names when there is a related footnote to show
  
  legend_board_name <- if_else(
    (first(plotdata$measure == "TYPE OF BIRTH") &
       first(plotdata$hbname == "NHS Borders")
     ) |
      (first(plotdata$measure == "GESTATION AT BOOKING") &
      first(plotdata$hbname %in% c("NHS Forth Valley", "NHS Tayside"))
      ) |
      (first(plotdata$measure == "GESTATION AT TERMINATION") &
      first(plotdata$hbname == "NHS Orkney, NHS Shetland and NHS Western Isles")
      ),
    paste0(first(plotdata$hbname), "*"),
    first(plotdata$hbname)
    )
  
  hoverinfo_format <- switch( # tells plotly how to format median hoverinfo
    first(plotdata$measure), 
   "BOOKINGS" = ",.0f",
   "GESTATION AT BOOKING" = ".1f",
   "TERMINATIONS" = ",.0f",
   "GESTATION AT TERMINATION" = ".1f",
   "INDUCTIONS" = ".1f",
   "TYPE OF BIRTH" = ".1f",
   "TEARS" = ".2f",
   "GESTATION AT BIRTH" = ".2f",
   "APGAR5" = ".2f"
   )

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  
  yaxis_plots[["title"]] <- list(
    text = ~ case_match(
      first(plotdata$measure),
      "TEARS" ~ "Percentage of women (%)",
      "APGAR5" ~ "Percentage of babies (%)",
      .default = yaxislabel
      )
    )
  
  yaxis_plots[["tickformat"]] <- 
    if_else(first(plotdata$measure) %in% c("APGAR5", "TEARS"),
            ".1f",
            ",d")
  
  yaxis_plots[["range"]] <- list(0, y_max * 1.05) # expands the y-axis range to prevent cut-offs
  
  runcharts <-
    plot_ly(
      data = plotdata,
      x = ~ date,
      y = ~ trend, # green trend line needs to be plotted first or it obliterates the others
      type = "scatter",
      mode = "lines",
      line = list(
        color = "lightgreen",
        width = 10
      ),
      name = orig_trend_label,
      legendgroup = "trend",
      legendrank = 1200,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>% 
    add_trace(
      y = ~ measure_value,
      mode = "lines+markers",
      line = list(
        color = "black", # black lines
        width = 1),
      marker = list(
        color = "black", # black dots
        size = 5),
      name = ~ case_match(
        first(plotdata$measure),
        "TYPE OF BIRTH" ~ "percentage of births (%)",
        "GESTATION AT BIRTH" ~ "percentage of births (%)",
        .default = str_to_lower(var_label(measure_value))
      ),
      legendgroup = "measure_value",
      legendrank = 100,
      showlegend = include_legend,
      hovertext = ~ mytext,
      hoverinfo = "text"
    ) %>% 
    add_trace(
      y = ~ get(centreline), # solid blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1),
      marker = NULL,
      name = ~ paste0(var_label(get(centreline))), # retrieves label of variable
      legendgroup = "median",
      legendrank = 200,
      showlegend = ~ include_legend,
      hoverinfo = "y",
      yhoverformat = hoverinfo_format
      # hovertext = "",
      # hoverinfo = "none"
    ) %>%
    add_trace(
      y = ~ get(dottedline), # dotted blue line
      type = "scatter",
      mode = "lines",
      line = list(
        color = phs_colours("phs-blue"),
        width = 1,
        dash = "4"
      ),
      marker = NULL,
      name = ~ paste0(var_label(get(dottedline))), # retrieves label of variable
      legendgroup = "extended_median",
      legendrank = 300,
      showlegend = ~ include_legend,
      hoverinfo = "y",
      yhoverformat = hoverinfo_format
    ) %>%
    add_trace(
      y = ~ shift, # orange lines
      mode = "lines",
      line = list(
        color = "orange", # orange lines (prevents missing data warning)
        width = 2),
      marker = NULL,
      # marker = list(
      #   color = "orange", # orange dots
      #   size = 6,
      #   symbol = "circle"
      # ),
      name = orig_shift_label,
      legendgroup = "shift",
      legendrank = 1300,
      showlegend = ~ include_trend_shift_legend,
      hovertext = "",
      hoverinfo = "none"
    ) %>%
    layout(
      font = plotly_global_font,
      xaxis = xaxis_plots,
      yaxis = yaxis_plots,
      legend = list(title = list(text = paste0(legend_board_name, "<br>")),
                    tracegroupgap = 15,
                    orientation = "v",
                    x = 1.0,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    itemclick = FALSE)
    ) %>%
    #config(modeBarButtons = list(list("zoomIn2d"), list("zoomOut2d"), list("pan3d")))
    config(displaylogo = F, displayModeBar = FALSE)
  
  # adds "dummy" traces for multiple runcharts to force shift and trend legends to appear even if there
  # are none in these charts
  
  if(first(plotdata$measure_cat) %in% c("spontaneous vaginal births",
                                        "between 32 and 36 weeks (inclusive)")) {
    runcharts <- runcharts %>%
      add_trace(
        data = plotdata,
        x = ~ min(date), # fake trend to show legend even when no trend exists on chart
        y = ~ -5,
        mode = "lines",
        line = list(
          color = "lightgreen",
          width = 10
        ),
        #marker = NULL,
        name = orig_trend_label, # retrieves label of variable
        legendgroup = "trend",
        legendrank = 800,
        showlegend = TRUE,
        #line = NULL,
        hovertext = "",
        hoverinfo = "none"
      ) %>%
      add_trace(
        data = plotdata,
        x = ~ max(date), # fake shift to show legend even when no shift exists on chart
        y = ~ -5,
        mode = "lines",
        marker = NULL,
        line = list(
          color = "orange", # orange lines (prevents missing data warning)
          width = 2),
        # marker = list(
        #   color = "orange", # orange dots
        #   size = 6,
        #   symbol = "circle"
        # ),
        name = orig_shift_label, # retrieves label of variable
        legendgroup = "shift",
        legendrank = 900,
        showlegend = TRUE,
        #line = NULL,
        hovertext = "",
        hoverinfo = "none"
      )
  }
  
  # post-pandemic traces for the GESTATION AT BOOKING measure
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING") {
    
    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(post_pandemic_median)),
        y = ~ post_pandemic_median, # magenta line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-magenta"),
          width = 1
        ),
        marker = NULL,
        name = ~ paste0(var_label(post_pandemic_median)
                        ),
        legendrank = 1000,
        showlegend = include_legend,
        legendgroup = "post-pandemic median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
        #hovertext = ""
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(extended_post_pandemic_median)),
        y = ~ extended_post_pandemic_median, # dotted magenta line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-magenta"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(var_label(extended_post_pandemic_median)
                        ),
        legendrank = 1100,
        showlegend = include_legend,
        legendgroup = "extended post-pandemic median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      )
  }
  
  # additional traces for the "special" Boards in GESTATION AT BOOKING measure
  
  if(first(plotdata$measure) == "GESTATION AT BOOKING" &
     first(plotdata$hbname) %in% c("NHS Forth Valley", "NHS Tayside")) {
    
    runcharts <- runcharts %>%
      add_trace(
        data = filter(plotdata,!is.na(revised_median)),
        y = ~ revised_median, # green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1
        ),
        marker = NULL,
        name = ~ case_when(
          hbname == "NHS Forth Valley" ~
            paste0("average gestation from Mar 2021", "<br>", "to end Feb 2022"),
          hbname == "NHS Tayside" ~
            paste0("average gestation from Aug 2020", "<br>", "to end Jul 2021"),
          TRUE ~ ""
        ),
        legendrank = 400,
        showlegend = include_legend,
        legendgroup = "revised median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      ) %>%
      add_trace(
        data = filter(plotdata,!is.na(extended_revised_median)),
        y = ~ extended_revised_median, # dotted green line
        type = "scatter",
        mode = "lines",
        line = list(
          color = phs_colours("phs-green"),
          width = 1,
          dash = "4"
        ),
        marker = NULL,
        name = ~ paste0(case_when(
          hbname == "NHS Forth Valley" ~ 
            paste0("projected average gestation from Mar 2022", "<br>", "to end Jun 2022"),
          hbname == "NHS Tayside" ~ 
            paste0("projected average gestation from Aug 2021", "<br>", "to end Jun 2022"),
          TRUE ~ ""
        )
        ),
        legendrank = 500,
        showlegend = include_legend,
        legendgroup = "extended_revised_median",
        hoverinfo = "y",
        yhoverformat = hoverinfo_format
      )
  }
  
  return(runcharts)
}
