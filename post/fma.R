library(tidyverse)
library(ggjoy)
library(lubridate)

f_ppt <- function(ppt) {
  case_when(
    ppt < 2.5 ~ 1,
    ppt >= 2.5 & ppt < 5 ~ 0.7,
    ppt >= 5 & ppt < 10 ~ 0.4,
    ppt >= 10 & ppt < 12 ~ 0.2,
    TRUE ~ NA_real_
  )
}

fma <- function(ur, ppt) {
  n <- length(ur)
  fma <- numeric(n)
  
  for (i in seq_len(n)) {
    f_i <- f_ppt(ppt[i])
    
    if (is.na(f_i)) {
      fma[i] <- 0
    } else {
      fma[i] <- fma[i] * f_i + (100 / ur[i]) 
    }
  }
  
  fma
}

fma2 <- function(ur, ppt) {
  
}




dados <- read_csv2("https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/clima_pira.csv")

dados

dados_fma <- dados %>% 
  mutate(
    data = dmy(data),
    fma = fma(ur, ppt)
  )


dados_fma %>% 
  mutate(ano = year(data)) %>% 
  ggplot(aes(ur, ano, group = ano)) +
  geom_joy() +
  theme_bw()

dados_fma %>%
  mutate(
    ano = year(data),
    doy = yday(data)
  ) %>% 
  ggplot(aes(doy, ano, fill = ur)) +
    geom_tile() +
    scale_fill_viridis_c()
