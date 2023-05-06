
# 1 - Objetivo ------------------------------------------------------------

# Análisis exploratorio de datos para entrega 2


# 2 - Setup ---------------------------------------------------------------

path <- here::here()

path_output <- glue::glue(path, "/Output")
# Aca deberia ir la parte de renv

library(dplyr)
library(stringr)
library(tibble)
library(lubridate)
library(ggplot2)

# 3 - Data ----------------------------------------------------------------

data <- read.csv(glue::glue('{path}/Output/data_entera.csv'))

test <- data %>% 
  filter(Ticker == "MSFT") %>% 
  arrange(desc(Date)) %>% 
  mutate(Days = n():1)

# 4 - Analisis ------------------------------------------------------------

any(is.na(data))


# * 1. Analisis basico ----------------------------------------------------


# > * 1. Faltantes --------------------------------------------------------


# > * 2. Desvio standar ---------------------------------------------------


# > * 3. ¿Desvio de intradiario? ------------------------------------------


# > * 4. Precio promedio anual --------------------------------------------


# > * 5. Distribucion volumen ---------------------------------------------


# * 2. Visualizaciones ----------------------------------------------------

# # > * 1. Tenencia corto -------------------------------------------------



# * 3. Volatilidad --------------------------------------------------------

# > * 1. Funcion ----------------------------------------------------------

volatilidad_historica <- function(df) {
  df <- df %>% 
    mutate(Return = (Close - lag(Close))/lag(Close)) %>% 
    na.omit()
  volatilidad <- sd(df$Return) * sqrt(252)
  
  return(volatilidad)
}

volatilidad_historica(test)

volatilidad_relativa <- function(df) {
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

volatilidad_relativa(test)

# > * 2. Calculo ----------------------------------------------------------

data_volatilidad <- data %>%
  group_split(Ticker) %>%
  purrr::map_dfr(~volatilidad_relativa(.x))

write.csv(data_volatilidad, glue::glue('{path}/Output/data_volatilidad.csv'))


# > * 3. Dotplot ----------------------------------------------------------


# * 4. Serie de tiempo ----------------------------------------------------

# > * 1. Autocorrelacion  -------------------------------------------------



# > * 2. Estacionalidad ---------------------------------------------------



