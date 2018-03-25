library(tidyverse)
library(ggridges)
library(lubridate)

fma <- function(ur, ppt) {
  n <- length(ur)
  fma <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {

    if (i == 1) {
      fma[i] <- 0
      next()
    }
    
    if (is.na(ur[i - 1])) {
      fma[i] <- 0
      next()
    }
    
    fma[i] <- case_when(
      ppt[i] < 2.5 ~ (100 / ur[i]) + fma[i - 1] * 1 ,
      ppt[i] >= 2.5 & ppt[i] < 5  ~ (100 / ur[i]) + fma[i - 1] * 0.7,
      ppt[i] >= 5   & ppt[i] < 10 ~ (100 / ur[i]) + fma[i - 1] * 0.4,
      ppt[i] >= 10  & ppt[i] < 13 ~ (100 / ur[i]) + fma[i - 1] * 0.2,
      ppt[i] >= 13 ~ 0
    )
  }
  
  fma
}

fma_classe <- function(fma) {
  case_when(
    fma <= 1 ~ 1,
    fma > 1 & fma <= 3 ~ 2,
    fma > 3 & fma <= 8 ~ 3,
    fma > 8 & fma <= 20 ~ 4,
    fma > 20 ~ 5
  )
}

dados <- read_csv2(
  "https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/clima_pira.csv",
  col_types = cols(data = col_date(format = "%d/%m/%Y"))
  ) %>% 
  filter(!is.na(data))

dados

dados_fma <- dados %>% 
  mutate(
    fma = fma(ur13, ppt),
    classe = fma_classe(fma),
    ano = year(data),
    mes = month(data)
  )


dados_fma  %>% 
  ggplot(aes(data, classe)) +
    geom_col() +
    coord_polar()

dados_fma %>% 
  ggplot(aes(fma, factor(mes))) +
  geom_density_ridges() +
  theme_bw()

dados_fma %>% 
  ggplot(aes(fma, factor(ano))) +
  geom_density_ridges() +
  theme_bw()

dados_fma %>%
  mutate(
    ano = year(data),
    doy = yday(data)
  ) %>% 
  ggplot(aes(doy, ano, fill = ur13)) +
    geom_tile() +
    scale_fill_distiller(palette = "Blues")

dados_fma %>%
  mutate(
    ano = year(data),
    doy = yday(data)
  ) %>% 
  ggplot(aes(doy, factor(ano), fill = fma)) +
  geom_tile() +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_bw()

dados_fma %>%
  mutate(
    ano = year(data),
    doy = yday(data),
    doy_data = as.Date("1999-12-31") + days(doy)
  ) %>% 
  ggplot(aes(doy, factor(ano), fill = fma)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw()

dados_fma %>%
  mutate(
    ano = year(data),
    doy = yday(data),
    doy_data = as.Date("1999-12-31") + days(doy)
  ) %>% 
  ggplot(aes(doy_data, factor(ano), fill = classe)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  scale_x_date(date_labels = "%B") +
  theme_bw()
