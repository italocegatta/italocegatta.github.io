library(tidyverse)
library(ggridges)
library(lubridate)

ppt <- head(dados, n = 20)$ppt
ur <-  head(dados, n = 20)$ur13
fma[1] <- 0

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
      ppt[i] >= 2.5 & ppt[i] < 5 ~ (100 / ur[i]) + fma[i - 1] * 0.7,
      ppt[i] >= 5   & ppt[i] < 10 ~ (100 / ur[i]) + fma[i - 1] * 0.4,
      ppt[i] >= 10  & ppt[i] < 13 ~ (100 / ur[i]) + fma[i - 1] * 0.2,
      ppt[i] >= 13 ~ 0
    )
  }
  
  fma
}

teste <- function(x, y) {
  x0 <- lag(x)
  y0 <- lag(y)
  
  z[1] <- 0
  
  z <- case_when(
    x < 2.5 ~ (100 / y) + fma[i - 1] * 1 ,
    ppt[i] >= 2.5 & ppt[i] < 5 ~ (100 / ur[i]) + fma[i - 1] * 0.7,
    ppt[i] >= 5   & ppt[i] < 10 ~ (100 / ur[i]) + fma[i - 1] * 0.4,
    ppt[i] >= 10  & ppt[i] < 13 ~ (100 / ur[i]) + fma[i - 1] * 0.2,
    ppt[i] >= 13 ~ 0
  )
  
  
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

fma2 <- function(ur, ppt) {
  
}

dados <- read_csv2(
  "https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/clima_pira.csv",
  col_types = cols(data = col_date(format = "%d/%m/%Y"))
)

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
  scale_fill_distiller(palette = "GnBu", direction = 1) +
  theme_bw()
