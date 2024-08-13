library(plotly)
df <- diamonds[sample(nrow(diamonds), 1000), ]
fig <- plot_ly(df, x = ~carat, y = ~price, text = ~paste("Clarity: ", clarity),
             mode = "markers", color = ~carat, size = ~carat)
fig <- layout(fig, title = 'Highlighting Regions with Circles',
            shapes = list(
              list(type = 'circle',
                   xref = 'x', x0 = .2, x1 = .7,
                   yref = 'y', y0 = 20, y1 = 3000,
                   fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = .75, x1 = 1.5,
                   yref = 'y', y0 = 2500, y1 = 7500,
                   fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                   opacity = 0.2),
               list(type = 'circle',
                   xref = 'x', x0 = 1.6, x1 = 2.5,
                   yref = 'y', y0 = 12500, y1 = 18500,
                   fillcolor = 'rgb(90, 200, 75)', line = list(color = 'rgb(90, 200, 75)'),
                   opacity = 0.2)))

fig
