creates_small_multiple_charts_without_median <- function(plotdata,
                                                         measure_value,
                                                         hover = "mytext"){
  
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
  
  
overview <- plotdata %>% 
      split(.$measure_cat) %>% 
      lapply(
        function(d)
          plot_ly(
            d,
            x = ~ date,
            y = ~ measure_value,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "black", # black lines
                        width = 1),
            marker = list(color = "black", # black dots
                          size = 5),
            hovertext = ~ get(hover),
            hoverinfo = "text",
            color = I("black")
          ) %>%
          layout(
            font = plotly_global_font,
            xaxis = xaxis_plots,
            yaxis = yaxis_plots,
            showlegend = FALSE,
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
      )

  overview <- overview %>%
    subplot(nrows = 1,
            #heights = plot_heights,
            #margin = c(0.01, 0.01, 0.05, 0.05), # gives more room between plots
            shareX = TRUE,
            shareY = TRUE) %>%
    layout(annotations = list(text = "Percentage of babies (%)",
                              font = list(size = 14),
                              x = 0,
                              y = 0.5,
                              xshift = -60,
                              textangle = 270,
                              showarrow = FALSE,
                              xref = "paper",
                              yref = "paper"
    )
    ) %>%
    config(displaylogo = F, displayModeBar = FALSE)
  
  return(overview)
}

creates_small_multiple_charts_without_median(plotdata = gest_by_BAPM_runchart_data)
