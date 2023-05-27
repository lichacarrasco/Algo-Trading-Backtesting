
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
library(corrplot)
library(GGally)

options(
  encoding = 'UTF-8',
  scipen = 9999
  )

# 3 - Data ----------------------------------------------------------------

data <- read.csv(glue::glue('{path}/Output/data_entera.csv')) %>% 
  mutate(Date = as.Date(Date))

test <- data %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Ticker == "MSFT") %>% 
  arrange(desc(Date)) %>% 
  mutate(Days = n():1)

data_pivot <- data %>% 
  select(Date, Ticker, Adjusted) %>% 
  mutate(Date = as.Date(Date)) %>% 
  pivot_wider(names_from = Ticker, values_from = Adjusted)

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

# > * 3. Distribucion volumen ---------------------------------------------

ggplot(data, aes(x = Volume, fill = Ticker)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Ticker, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Close por Ticker", x = "Close", y = "Frecuencia")

# > * 4. Correlacion ------------------------------------------------------

data_pivot <- data_pivot %>% 
  filter(year(Date) > 2017)

cor(data_pivot %>% select(-Date))

### Esto hace el corplot para todas
bi_test %>% 
  GGally::ggpairs(columns = 2:5)

# Pasar a plotly y poner colores a cada ticker

data_pivot %>% 
  ggplot(aes(x = AAPL, y = MSFT)) +
  geom_point()

data_pivot %>% 
  ggplot(aes(x = XOM, y = MSFT)) +
  geom_point()

# > * 5. Dispersion -------------------------------------------------------

data %>% 
  filter(year(Date) > 2017) %>% 
  mutate(Month = zoo::as.yearmon(Date, "%Y-%m")) %>% 
  group_by(Ticker, Month) %>% 
  summarise(Adj_mean = mean(Adjusted)) %>% 
  ggplot(aes(x = Month, y = Adj_mean)) +
  geom_line() +
  facet_grid(vars(Ticker), scales = "free_y") +
  labs(title = "Evolución comparada del precio ajustado",
       y= "Precio ajustado") + 
  theme_minimal()

# * 2. Volatilidad --------------------------------------------------------

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
    mutate(
      Date        = as.Date(Date),
      Close       = as.numeric(Close),
      ReturnSP500 = log(Close / lag(Close))
    ) %>%
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

data_volatilidad

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


# > * 4. Corplot ----------------------------------------------------------

# Esto creo que puede salir

volumen_medio <- data %>% 
  group_by(Ticker) %>% 
  summarise(VolumenMedio = mean(Volume))

data_volatilidad <- data_volatilidad %>% 
  left_join(
    volumen_medio,
    by = c ("Ticker")
  ) 

matriz_correlacion <- data_volatilidad %>% 
  arrange(desc(ValorBeta), VolumenMedio)

# corrplot(
#   cor(matriz_correlacion %>% select(-Ticker)), 
#   method = "circle", 
#   type   = "lower", 
#   tl.col = "black", 
#   tl.srt = 30
# )

# * 3. Serie de tiempo ----------------------------------------------------

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

# > * 2. Tendencia largo --------------------------------------------------

source(glue::glue('{path}/Funciones/facetado_ajustado.R'))

# Usa el valor ajustado. Explicar qué es

facetado_ajustado(data = data, fecha_desde = '2018-04-04')

# > * 3. Autocorrelacion  -------------------------------------------------

acf(test %>% select(Volume),type = "correlation", na.action = na.pass )

pacf(test %>% select(Adjusted))

data_pivot %>%
  as_tsibble(index = Date) %>%
  tsibble::fill_gaps() %>% 
  ACF(MSFT, lag_max = 52, type = "correlation", na.action = na.pass) %>% 
  autoplot()+
  labs(title = "Autocorrelacion", subtitle = "MSFT")

# > * 4. Estacionalidad ---------------------------------------------------

test %>% 
  tsibble(index = "Date") %>% 
  filter(Date > '2017-12-31') %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(Adjusted, .direction = "down") %>% 
  gg_season(Adjusted, labels = "both", na.rm = TRUE) +
  labs(y = "Valor ajustado", title = "Gráfico estacional", subtitle = 'MSFT') +
  theme_minimal()

# > * 5. Descomposicion de la serie ---------------------------------------

serie_tiempo <- data %>%
  filter(Ticker == "MSFT") %>% 
  select(ds = Date, y = Close) %>% 
  mutate(ds = as.Date(ds)) %>% 
  filter(ds > '2018-10-01' & ds < '2023-12-01') %>% 
  group_by(ds = floor_date(ds, unit = "week")) %>% 
  summarise(y = mean(y, na.rm = TRUE)) %>% 
  as_tsibble(index = ds)

plot(decompose(ts(serie_tiempo$y, frequency = 52)))

## Forma del libro

test_ts <- test %>% 
  tsibble(index = Date) %>% 
  filter(Date > '2018-10-01') %>% 
  select(Date, Ticker, Adjusted) 
str(test_ts)
any(is.na(test))

autoplot(test_ts, Adjusted)

#Esto forma un "dable", el tipo de df descompuesto
# Donde Adjusted mantiene el valor original
# Y las otras columnas son los componentes estimados
dcmp_test <- test_ts %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(Adjusted, .direction = "down") %>%
  model(stl = STL(Adjusted ~ trend(window = 21), robust = TRUE))
components(dcmp_test) %>% 
  autoplot()

# > * 6. Fuerza de la tendencia y estacionalidad --------------------------

test %>% 
  as_tsibble(index = Date) %>% 
  filter(Date > '2018-10-01') %>% 
  select(Date, Ticker, Adjusted) %>% 
  features(Adjusted, feat_stl) 



##### HASTA ACÁ ESTÁ CHEQUEADO



# 5. Prophet --------------------------------------------------------------

test_prophet <- test %>% 
  select(ds = Date, y = Close) %>% 
  mutate(ds = as.Date(ds))

covid <- data.frame(
  holiday      = 'covid',
  ds           = seq(as.Date("2020-02-19"), as.Date("2020-08-18"), by = "day"),
  lower_window = 0,
  upper_window = 0
)

prophet <- prophet(holidays = covid)

prophet <- add_seasonality(prophet, name = 'monthly', period = 30.5, fourier.order = 4)

prophet <- fit.prophet(m = prophet, test_prophet)

plot(prophet, fcst = predict(prophet, test_prophet)) +
  theme_bw()
