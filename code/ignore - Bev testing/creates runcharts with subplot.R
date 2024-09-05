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
  
  #include_legend <- unique(plotdata$measure_cat) == "special care"
  
  # include_trend_shift_legend = TRUE ensures that the shift and trend legends appear even when the 
  # chart "linked" to the legend doesn't have any shifts or trends 
  
  include_trend_shift_legend <- TRUE
  
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
  
 plotdata <- split(plotdata, ~ measure_cat)
 
lapply(plotdata, function(d){
        include_legend <- if (d == "special care") {TRUE} else {FALSE}
        plot_ly(
          d,
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
          name = ~ "percentage of babies (%)",
          hovertext = ~ mytext,
          hoverinfo = "text",
          legendgroup = "measure_value",
          legendrank = 100,
          showlegend = ~ include_legend
          #   (function(){
          #   # echo to console
          #   print(include_legend)
          #   if(include_legend){
          #     # at first plot this will run
          #     # set to FALSE for subsequent plots
          #     include_legend <<- FALSE
          #     return(TRUE)
          #   } else {
          #     # runs for all except first
          #     return(FALSE)
          #   }
          # })()
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
          #plot_bgcolor='#ecebf3',
          annotations = list(
            x = 0.5,
            y = 1.0,
            text = ~ unique(measure_cat),
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      }
    )
  
  runcharts <- runcharts %>%
    subplot(nrows = 1,
            shareX = TRUE,
            shareY = FALSE) %>%
    # layout(annotations = list(text = ~ yaxislabelswitch,
    #                           font = list(size = 14),
    #                           x = 0,
    #                           y = ~ yaxislabelposition,
    #                           xshift = -60,
    #                           textangle = 270,
    #                           showarrow = FALSE,
    #                           xref = "paper",
    #                           yref = "paper"
    # )
    # ) %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(runcharts)
}

  creates_runcharts(plotdata = gest_by_BAPM_runchart_data[1]
                  )

  
  creates_runcharts(plotdata = gest_by_BAPM_runchart_data[[3]])
  