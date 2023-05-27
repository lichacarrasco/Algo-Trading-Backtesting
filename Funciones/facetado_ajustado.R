#' Generar un facetado de precio ajustado
#'
#' @description 
#' `facetado_ajustado()` genera un grafico de areas facetado para las 10 acciones
#' seleccionadas para el proyecto actual
#' 
#' @param data. Dataframe. El dataframe original, que debe contener 
#' las columas Date, Adjusted, Ticker
#' @param fecha_desde. String. La fecha desde la cual comienzan los graficos
#' @return Una figura de plotly 
facetado_ajustado <- function(data, fecha_desde) {
  
  data_pivot <- data %>%
    select(Ticker, Date, Adjusted) %>%
    pivot_wider(names_from = Ticker, values_from = Adjusted) %>% 
    filter(Date >= fecha_desde)
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE
  )
  
  fig1 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~GOOG, name = 'GOOG')%>%
    layout(xaxis = ax)
  
  
  fig2 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~AAPL, name = 'AAPL')%>%
    layout(xaxis = ax)
  
  
  fig3 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~AMZN, name = 'AMZN')%>%
    layout(xaxis = ax)
  
  
  fig4 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~META, name = 'META')%>%
    layout(xaxis = ax)
  
  
  fig5 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~TSLA, name = 'TSLA')%>%
    layout(xaxis = ax)
  
  fig6 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~MSFT, name = 'MSFT')%>%
    layout(xaxis = ax)
  
  fig7 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~`BRK-A`, name = 'BRK-A')%>%
    layout(xaxis = ax)
  
  fig8 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~NVDA, name = 'NVDA')%>%
    layout(xaxis = ax)
  
  fig9 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~V, name = 'V')%>%
    layout(xaxis = list(title = 'Date'))
  
  fig10 <- plot_ly(data_pivot, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
    add_trace(x = ~Date, y = ~XOM, name = 'XOM')%>%
    layout(xaxis = list(title = 'Date'))
  
  annotations = list(
    list(
      x = 0.225,
      y = 1.0,
      font = list(size = 10),
      text = " GOOG",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.775,
      y = 1.0,
      font = list(size = 10),
      text = " AAPL",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.225,
      y = 0.78,
      font = list(size = 10),
      text = " AMZN",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.775,
      y = 0.78,
      font = list(size = 10),
      text = " META",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.225,
      y = 0.58,
      font = list(size = 10),
      text = " TSLA",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.775,
      y = 0.58,
      font = list(size = 10),
      text = "MSFT",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.225,
      y = 0.375,
      font = list(size = 10),
      text = " BRK-A",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.775,
      y = 0.375,
      font = list(size = 10),
      text = " NVDA",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.225,
      y = 0.18,
      font = list(size = 10),
      text = " V",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.775,
      y = 0.18,
      font = list(size = 10),
      text = " XOM",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    )
  )
  
  
  fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10,
                 nrows = 5, titleY = F, titleX = TRUE) %>% 
    layout(xaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff'),
           plot_bgcolor='#e5ecf6') %>%
    layout(width = 900, annotations = annotations, title = "Evolución del precio ajustado")
  
  return(fig)  
}