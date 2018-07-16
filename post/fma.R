library(tidyverse)
library(ggridges)
library(lubridate)



dados_fma  %>% 
  filter(!is.na(risco)) %>% 
  group_by(ano = year(data), risco) %>% 
  tally() %>% 
  ggplot(aes(ano, n, fill = risco)) +
  geom_col(position = "fill", alpha = 0.8, show.legend = FALSE) +
  labs(x = "Ano", y = "Frequência", fill = "Risco") +
  scale_y_continuous(breaks = seq(0.1, 1, 0.1), labels = scales::percent) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_void() +
  ggsave("fma.png", width = 10, height = 6)


fma <- function(data, ur, ppt) {
  
  if (any(data != sort(data))) {
    stop("data precisa estar em ordem crescente")
  }
  
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

fma_classe <- function(fma, limites = c(1, 3, 8, 20)) {
  case_when(
    fma <= 1 ~ "1",
    fma > 1 & fma <= 3 ~"2",
    fma > 3 & fma <= 8 ~ "3",
    fma > 8 & fma <= 20 ~ "4",
    fma > 20 ~ "5"
  )
}

x$ppt %>% fma_classe()


dados <- read_csv2(
  "https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/clima_pira.csv",
  col_types = cols(data = col_date(format = "%d/%m/%Y"))
  ) %>% 
  filter(!is.na(data))


x <- dados %>% head()

any(x$data[c(1, 2, 4, 3, 5, 6)] != sort(x$data))
any(x$data != sort(x$data))

sum(dados$data != sort(dados$data))

dados_fma <- dados %>% 
  arrange(ppt) %>% 
  mutate(
    fma = fma(data, ur13, ppt),
    classe = fma_classe(fma),
    ano = year(data),
    mes = month(data)
  )


dados_fma  %>% 
  group_by(ano_mes = floor_date(data, "year"), classe) %>% 
  tally() %>% 
  ggplot(aes(n, fill = classe)) +
    geom_bar(position = "fill") +
    coord_polar() +
    scale_fill_brewer(palette = "Spectral") 

dados_fma  %>% 
  group_by(ano_mes = floor_date(data, "year"), classe) %>% 
  tally() %>% 
  ggplot(aes(ano_mes, n, fill = classe)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette = "Spectral") 


geom_col()dados_fma %>% 
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
