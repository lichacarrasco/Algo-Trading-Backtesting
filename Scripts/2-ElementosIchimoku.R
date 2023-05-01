
# 1 - Objetivo ------------------------------------------------------------

# Obtener los elementos que componen el sistema de Ichimoku.

# 2 - Setup ---------------------------------------------------------------

path <- here::here()

path_output <- glue::glue(path, "/Output")
# Aca deberia ir la parte de renv

library(dplyr)
library(stringr)
library(tibble)
library(lubridate)
library(RcppRoll)

# 3 - Data ----------------------------------------------------------------

data <- read.csv(glue::glue('{path}/Output/data_entera.csv'))

test <- data %>% 
  filter(Ticker == "MSFT") %>% 
  arrange(desc(Date)) %>% 
  mutate(Days = n():1)

# 4 - Elementos -----------------------------------------------------------

# * 1. Tenkan -------------------------------------------------------------

test <- test %>%
  arrange(Days) %>%
  mutate(
    Tenkan = (
      RcppRoll::roll_max(High, 9, fill = NA, align = "right") +
      RcppRoll::roll_min(Low, 9, fill = NA, align = "right")
      ) / 2
  )

# * 2. Kijun --------------------------------------------------------------

test <-  test %>%
  arrange(Days) %>%
  mutate(
    Kijun = (
      RcppRoll::roll_max(High, 26, fill = NA, align = "right") +
      RcppRoll::roll_min(Low, 26, fill = NA, align = "right")
    ) / 2
  )

# * 3. Chikou -------------------------------------------------------------

test <- test %>% 
  arrange(Days) %>%
  mutate(Chikou = lead(Close, 26))

# * 4. Span A -------------------------------------------------------------

# * 5. Span B -------------------------------------------------------------

# * 6. Kumo ---------------------------------------------------------------


