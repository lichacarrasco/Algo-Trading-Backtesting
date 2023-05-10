
# 1 - Objetivo ------------------------------------------------------------

# Análisis exploratorio de datos para entrega 2


# 2 - Setup ---------------------------------------------------------------

path <- here::here()

path_output <- glue::glue(path, "/Output")
# Aca deberia ir la parte de renv

library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(plotly)
library(tsibble)
library(forecast)
library(feasts)
library(prophet)
library(descr)

options(
  encoding = 'UTF-8',
  scipen = 9999
  )

# 3 - Data ----------------------------------------------------------------

data <- read.csv(glue::glue('{path}/Output/data_entera.csv'))

test <- data %>% 
  filter(Ticker == "MSFT") %>% 
  arrange(desc(Date)) %>% 
  mutate(Days = n():1)

# 4 - Analisis ------------------------------------------------------------

# * 1. Analisis basico ----------------------------------------------------

# > * 1. Faltantes --------------------------------------------------------

any(is.na(data)) #No hay faltantes en la data original

# Pero no todas las acciones cotizan desde la misma fecha

data %>% 
  group_by(Ticker) %>% 
  summarise(Start = min(Date)) %>% 
  arrange(Start)

# > * 2. Desvio standar ---------------------------------------------------

data %>% 
  group_by(Ticker) %>% 
  summarise(
    Desvio = sd(Close),
    CV     = sd(Close) / mean(Close) * 100,
    Start = min(Date)
  ) %>% 
  arrange(CV)

# > * 5. Distribucion volumen ---------------------------------------------

ggplot(data, aes(x = Adjusted, fill = Ticker)) +
   geom_density(alpha = 0.5) +
  facet_wrap(~ Ticker, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Close por Ticker", x = "Close", y = "Frecuencia")

# * 2. Visualizaciones ----------------------------------------------------

# > * 1. Tendencia corto --------------------------------------------------

mes <- test %>% 
  filter(Date >= as.Date(max(test$Date))-31)

plot_ly(mes, type = "candlestick",
        x = ~Date, open = ~Open, high = ~High,
        low = ~Low, close = ~Close, name = mes$Ticker,
        increasing = list(line = list(color = "green")),
        decreasing = list(line = list(color = "red"))) %>%
  layout(title = "Evolución del precio de MSFT en el último mes",
         xaxis = list(title = "Fecha", rangeslider = list(visible = F)),
         yaxis = list(title = "Precio"))

# > * 3. Tendencia largo --------------------------------------------------

source(glue::glue('{path}/Funciones/facetado_ajustado.R'))

facetado_ajustado(data = data, fecha_desde = '2018-04-04')

# * 3. Volatilidad --------------------------------------------------------

# > * 1. Funcion ----------------------------------------------------------

calcular_volatilidad <- function(df) {
  df <- df %>% 
    arrange(Date) %>% 
    mutate(Return = log(Close / lag(Close))) %>% 
    na.omit()
  
  volatilidad <- df %>% 
    summarize(VolatilidadHistorica = sd(Return) * sqrt(252))
  
  sp500 <- read.csv("https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=946713600&period2=1938341200&interval=1d&events=history&includeAdjustedClose=true")
  sp500 <- sp500 %>% 
    select(Date, Close) %>% 
    mutate(ReturnSP500 = log(Close / lag(Close))) %>% 
    na.omit()
  
  data_sp500 <- left_join(df, sp500, by = "Date")
  
  valor_beta <- data_sp500 %>% 
    summarize(ValorBeta = cov(Return, ReturnSP500) / var(ReturnSP500))
  
  data.frame(Ticker = unique(df$Ticker), Volatilidad = volatilidad, ValorBeta = valor_beta)
}

calcular_volatilidad(test)

# > * 2. Calculo ----------------------------------------------------------

data_volatilidad <- data %>%
  group_split(Ticker) %>%
  purrr::map_dfr(~calcular_volatilidad(.x))

#write.csv(data_volatilidad, glue::glue('{path}/Output/data_volatilidad.csv'))

# > * 3. Barplot ----------------------------------------------------------

plot_ly(
  data   = data_volatilidad %>% arrange(VolatilidadHistorica), 
  x      = ~Ticker, 
  y      = ~VolatilidadHistorica, 
  type   = 'bar', 
  name   = 'Volatilidad Histórica',
  marker = list(color = 'royalblue3')
  ) %>% 
  add_trace(y = ~ValorBeta, name = 'Valor Beta', marker = list(color = 'red')) %>%
  layout(yaxis = list(title = 'Volatilidad'), barmode = 'group') %>% 
  layout(xaxis = list(categoryorder = "total ascending")) %>% 
  layout(title = "Comparación de la volatilidad de cada acción")

# * 4. Serie de tiempo ----------------------------------------------------

# > * 1. Autocorrelacion  -------------------------------------------------



# > * 2. Estacionalidad ---------------------------------------------------



