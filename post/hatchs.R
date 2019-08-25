library(tidyverse)
library(fipe)
library(lubridate)
library(broom)
library(deflateBR)

limpa_nome <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
}

# hatchs <- c(
#   "^kwid", "^mobi", "^up!",
#   "^hb20[\\s|x]", "^march",
#   "^gol\\s", "^onix", "^sandero",
#   "^ka\\s", "^argo", "^etios (?!.*sed)",
#   "^polo", "^fox",
#   "^yaris (?!.*sed)",
#   "^fit"
# )
# 
# base_hatch <- fipe_carro(
#   modelo = hatchs,
#   ano = c(0, 2019, 2018, 2017, 2016, 2015),
#   data_referencia = seq.Date(as.Date("2017-01-01"), as.Date("2019-08-01"), by = "1 months"),
#   .progress = TRUE
#   )
# 
# base_hatch %>% 
#   write_excel_csv2("content/dados/base_hatch.csv", na = "")

base_hatch <- read_csv2("content/dados/base_hatch.csv")

base_hatch_deflate <- base_hatch %>% 
  mutate(valor_deflate = deflate(valor, data_referencia, "07/2019", "ipca"))

base_0km <- base_hatch_deflate %>% 
  filter(ano == "0 km") %>% 
  mutate(ano = as.character(year(data_referencia))) %>% 
  select(modelo, marca, ano, data_referencia_0km = data_referencia, valor_deflate_0km = valor_deflate) 
  

base_seminovo <- base_hatch_deflate %>% 
  filter(ano != "0 km") %>% 
  select(-valor)

base_modelo <- base_seminovo %>% 
  left_join(base_0km) %>% 
  filter(data_referencia >= data_referencia_0km) %>% 
  mutate(
    delta_ano = time_length(data_referencia - data_referencia_0km, "year"),
    delta_valor = valor_deflate - valor_deflate_0km
  ) %>% 
  group_by(modelo, marca, ano) %>% 
  nest() %>% 
  crossing(tibble(ano_uso = c(1, 2, 3))) %>% 
  mutate(
    data_filter_ano_uso = map2(data, ano_uso, ~filter(.x, delta_ano <= .y)),
    n = map_dbl(data_filter_ano_uso, nrow)
  ) %>% 
  distinct(modelo, marca, ano, n, .keep_all = TRUE) %>% 
  mutate(
    mod_ano_uso = map(data_filter_ano_uso, ~lm(valor_deflate ~ delta_ano, data = .x)),
    resumo = map(mod_ano_uso, glance),
    coef = map(mod_ano_uso, tidy),
    resid = map(mod_ano_uso, augment)
  )

base_modelo$data[[4]] %>% 
  arrange(data_referencia, data_referencia_0km)

base_modelo %>% 
  unnest(resid) %>% 
  ggplot(aes(delta_ano, .fitted, group = interaction(modelo, ano, ano_uso))) +
    geom_line(alpha = 0.1) +
    geom_smooth(aes(group = 1)) +
    facet_wrap(~ano_uso, scales = "free_x", ncol = 1)


base_modelo %>% 
  unnest(data_filter_ano_uso) %>% 
  #filter(marca == "Kia Motors") %>% 
  ggplot(aes(delta_ano, delta_valor)) +
    geom_line(aes(group = interaction(modelo, ano, data_referencia)), alpha = 0.1) +
    #geom_point(alpha = 0.1) +
    geom_smooth() +
    facet_wrap(~marca) +
    labs(x = "Anos de uso", y = "Desvalorização") +
    theme_bw()


base_seminovo %>% 
  group_by(modelo, ano) %>% 
  nest() %>% 
  mutate(
    valor_0km = map2_dbl(modelo, ano, ~filter(base_0km, modelo == .x, year(data_referencia))
  )

base_hatch_ipca %>% 
  filter(modelo == "ARGO 1.0 6V Flex.") %>% 
  group_by(modelo, ano) %>% 
  nest() %>% 
  
  group_by()

base_hatch_ipca$modelo %>% unique()

base_hatch_ipca %>% 
  left_join(filter(., ano == "0 km") %>% select(modelo, data_referencia, valor_0km = valor_corrigido)) %>% 
  #left_join(filter(., ano == "0 km") %>% group_by(modelo) %>% summarise(valor_0km_media = mean(valor))) %>% 
  filter(ano != "0 km") %>% 
  mutate(delta_novo = valor_0km - valor_corrigido) %>% 
  group_by(modelo_ano) %>% 
  mutate(delta_media = mean(delta_novo)) %>% 
  ungroup() %>% 
  filter(dense_rank(delta_media) <= 5 | dense_rank(-delta_media) <= 5) %>%
  mutate(modelo_ano = fct_reorder(modelo_ano, valor_0km)) %>% 
  ggplot(aes(data_referencia)) +
    geom_line(aes(y = valor_corrigido), color = "blue") +
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

