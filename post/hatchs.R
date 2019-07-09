library(tidyverse)
library(fipe)
library(lubridate)
library(deflateBR)

# limpa_nome <- function(x) {
#   x %>%
#     stringr::str_to_lower() %>%
#     iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
# }

hatchs <- c(
  "^kwid", "^mobi", "^up!",
  "^hb20[\\s|x]", "^march",
  "^gol\\s", "^onix", "^sandero",
  "^ka\\s", "^argo", "^etios (?!.*sed)",
  "^polo", "^fox",
  "^picanto", "^yaris (?!.*sed)",
  "^fit"
)

base_hatch <- fipe_carro(
  modelo = hatchs,
  ano = c(0, 2018, 2017, 2016, 2015),
  data_referencia = seq.Date(as.Date("2018-01-01"), as.Date("2018-12-01"), by = "1 months"),
  .progress = TRUE
  ) %>% 
  mutate(modelo_ano = paste(modelo, ano)) %>% 
  group_by(modelo_ano) %>% 
  add_count() %>% 
  filter(n == 12) %>% 
  select(marca, modelo, ano, modelo_ano, data_referencia, valor)

base_hatch %>% 
  select(marca, modelo, ano, modelo_ano, data_referencia, valor) %>% 
  write_excel_csv2("content/dados/base_hatch.csv", na = "")

base_hatch <- read_csv2("content/dados/base_hatch.csv")

base_hatch_ipca <- base_hatch %>% 
  mutate(valor_ipca = deflate(valor, data_referencia, "12/2017", "ipca"))

base_hatch_ipca %>% 
  left_join(filter(., ano == "0 km") %>% select(modelo, data_referencia, valor_0km = valor_ipca)) %>% 
  #left_join(filter(., ano == "0 km") %>% group_by(modelo) %>% summarise(valor_0km_media = mean(valor))) %>% 
  filter(ano != "0 km") %>% 
  mutate(delta_novo = valor_0km - valor_ipca) %>% 
  group_by(modelo_ano) %>% 
  mutate(delta_media = mean(delta_novo)) %>% 
  ungroup() %>% 
  filter(dense_rank(delta_media) <= 5 | dense_rank(-delta_media) <= 5) %>%
  mutate(modelo_ano = fct_reorder(modelo_ano, valor_0km)) %>% 
  ggplot(aes(data_referencia)) +
    geom_line(aes(y = valor_ipca), color = "blue") +
    geom_line(aes(y = valor_0km), color = "red") +
    facet_wrap(~modelo_ano, ncol = 5)


# variacao entre jan e dez ------------------------------------------------

calc <- base_hatch_ipca %>% 
  filter(
    data_referencia %in% c(ymd("2018-01-01"), ymd("2018-12-01")),
    ano %in% c("0 km", "2018")
  ) %>% 
  mutate(data_referencia = month(data_referencia)) %>% 
  mutate(id = str_glue("{ano}_{data_referencia}")) %>% 
  select(modelo, marca, id, valor_ipca ) %>% 
  spread(id, valor_ipca ) %>% 
  drop_na() %>% 
  mutate(
    delta_novo = `0 km_12` - `0 km_1`,
    delta_seminovo = `2018_12` - `2018_1`,
    depreciacao_abs = `0 km_1` - `2018_12`,
    depreciacao_p = (1 - `2018_12` / `0 km_1`) * 100 
  )



calc %>% 
  ggplot(aes(depreciacao_p)) +
    geom_histogram(binwidth = 1, color = "grey")

calc %>% 
  ggplot(aes(`0 km_12`, depreciacao_p)) +
    geom_point()

calc %>% 
  filter(row_number(depreciacao_p) <= 5 | row_number(-depreciacao_p) <= 5) %>% 
  mutate(modelo = fct_reorder(modelo, depreciacao_p)) %>% 
  ggplot(aes(depreciacao_p, modelo)) +
    geom_segment(aes(xend = 0, yend = modelo)) +
    geom_point(size = 3)

