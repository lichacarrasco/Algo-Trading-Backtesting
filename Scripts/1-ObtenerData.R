
# 1 - Objetivo ------------------------------------------------------------

# Obtener la data basica de cotizacion de las 10 acciones de mayor market cap en EEUU


# 2 - Setup ---------------------------------------------------------------

path <- here::here()

path_output <- glue::glue(path, "/Output")
# Aca deberia ir la parte de renv

library(dplyr)
library(stringr)
library(quantmod)
library(zoo)
library(tibble)


# 3 - Funciones -----------------------------------------------------------

obtener_acciones <- function(ticker){
  data <- quantmod::getSymbols(Symbols = ticker, auto.assign = FALSE) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Date") %>% 
    mutate(
      Ticker = ticker,
      Date   = as.Date(Date)
    ) %>% 
    rename_with(
      .x,
      .cols = starts_with(ticker),
      .fn   = ~ str_replace(.x, glue::glue(ticker, "\\."), "")
    )
  
  return(data)
}

# 4 - Data ----------------------------------------------------------------

acciones <- c('AAPL', 'MSFT', 'GOOG', 'AMZN', 'BRK-A', 'NVDA', 'TSLA', 'META', 'V', 'XOM')

data <- purrr::map_df(
  .x = acciones,
  .f = ~ obtener_acciones(.x)
)

# 5 - Guardo la data ------------------------------------------------------

write.csv(data, file = glue::glue(path_output,"/data_entera.csv"), row.names = FALSE)

