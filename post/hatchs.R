library(tidyverse)
library(fipe)
library(lubridate)

# limpa_nome <- function(x) {
#   x %>%
#     stringr::str_to_lower() %>%
#     iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
# }

hatchs <- c(
  "^kwid", "^mobi", "^up!",
  "^hb20[\\s|x]", "^march",
  "^gol\\s", "^onix", "^sandero",
  "^ka\\s", "^argo", "^etios (?!.*sedan)",
  "^polo", "^fox",
  "^picanto", "^yaris (?!.*sedan)",
  "^fit"
)

base_hatch <- fipe_carro(
  modelo = hatchs,
  ano = c(0, 2018),
  data_referencia = seq.Date(as.Date("2018-01-01"), as.Date("2018-12-01"), by = "1 months"),
  .progress = TRUE
)

write_excel_csv2(base_hatch, "content/dados/base_hatch_2018.csv")

base_hatch <- read_csv2("content/dados/base_hatch_2018.csv")

base_hatch %>% 
  group_by(modelo, ano) %>% 
  tally() %>% 
  filter(n == 12) %>% 
  select(-n)

base_hatch %>% 
  filter(data_referencia %in% c(ymd("2018-01-01"), ymd("2018-12-01"))) %>% 
  mutate(id = str_glue("{ano}-{format.Date(data_referencia, '%Y')}")) %>% 
  select(modelo, marca, id, valor) %>% 
  spread(id, valor) 

calc <- base_hatch %>% 
  filter(data_referencia %in% c(ymd("2018-01-01"), ymd("2018-12-01"))) %>% 
  mutate(data_referencia = month(data_referencia)) %>% 
  pivot(., pivot_spec_wide(., c("ano","data_referencia"), valor)) %>% 
  na.omit() %>% 
  mutate(
    delta_novo = `0 km_12` - `0 km_1`,
    delta_seminovo = `2018_12` - `2018_1`,
    depreciacao_abs = `0 km_1` - `2018_12`,
    depreciacao_p = (1 - `2018_12` / `0 km_1`) * 100 
  )

# grafico ternario para valor, depre_abs, depre_p