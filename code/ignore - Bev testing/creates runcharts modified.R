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
  
  # include_legend = TRUE for ONE of multiple runcharts (otherwise the legends get repeated) 
  # need to see if a different method can utilise the subgroup function (will need to reformat the
  # dataframe fed into plotly)
  
  include_legend <- if_else(
    first(plotdata$measure_cat) == "special care", TRUE, FALSE)

  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- include_legend
  
  select_date_tickvals <- SMR02_multiples_date_tickvals # tells plotly where ticks will show

  select_date_ticktext <- SMR02_multiples_date_ticktext # tells plotly what text to show on ticks

  hoverinfo_format <- ".1f" # tells plotly how to format median hoverinfo

  xaxis_plots <- orig_xaxis_plots
  xaxis_plots[["tickmode"]] <- "array"
  xaxis_plots[["tickvals"]] <- select_date_tickvals
  xaxis_plots[["ticktext"]] <- select_date_ticktext

  yaxis_plots <- orig_yaxis_plots
  
  yaxis_plots[["title"]] <- "Percentage of babies (%)"

  #yaxis_plots[["tickformat"]] <- ",d"

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
      name = "percentage of babies (%)",
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
      legend = list(title = list(text = paste0("Scotland", "<br>")),
                    tracegroupgap = 15,
                    orientation = "v",
                    x = 1.0,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    itemclick = FALSE)
    ) %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
  # adds "dummy" traces for multiple runcharts to force shift and trend legends to appear even if there
  # are none in these charts
  
  if(first(plotdata$measure_cat) == "special care") {
    
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
      name = orig_trend_label, # retrieves label of variable
      legendgroup = "trend",
      legendrank = 800,
      showlegend = TRUE,
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
      name = orig_shift_label, # retrieves label of variable
      legendgroup = "shift",
      legendrank = 900,
      showlegend = TRUE,
      hovertext = "",
      hoverinfo = "none"
    )
  }
  
  # post-pandemic traces for the GESTATION AT BOOKING and GESTATION AT TERMINATION measures
  
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

  return(runcharts)
}

creates_runcharts(plotdata = filter(gest_by_BAPM_runchart_data,
                                    measure_cat == "special care")
                  )
