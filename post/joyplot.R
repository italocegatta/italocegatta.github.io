if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, RcppRoll, lubridate, stringr, ggplot2, ggjoy, ggthemes)




clima <- read_csv2("https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/posto_esalq.csv") %>% 
  mutate(
    data = dmy(data),
    ano = year(data),
    decada_label = cut(ano, breaks = seq(1910, 2020, by = 10), dig.lab = 100, right = FALSE),
    decada = as.numeric(str_extract(decada_label, "[0-9]+"))
  )

summary(clima)



ggplot(clima, aes(t_max)) +
  geom_density() +
  labs(
    x = "Temperatura máxima do mês",
    y = "Densidade"
  ) +
  theme_bw()



ggplot(clima, aes(t_max, group = ano, fill = ano)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Temperatura máxima do mês",
    y = "Densidade",
    fill = "Ano"
  ) +
  scale_fill_viridis_c() +
  theme_bw()


ggplot(clima, aes(t_max)) +
  geom_density() +
  facet_wrap(~ano) +
  labs(
    x = "Temperatura máxima do mês",
    y = "Densidade"
  ) +
  theme_few(base_size = 9)


ggplot(clima, aes(t_med, factor(ano))) +
  geom_joy() +
  labs(
    x = "Temperatura máxima do mês",
    y = "Ano"
  ) +
  theme_few(base_size = 9)



aux_mediana <- clima %>% 
  group_by(decada) %>% 
  summarise(mediana = median(t_med, na.rm = TRUE))

ggplot(clima, aes(t_med, factor(decada), fill = ..x..)) +
  geom_joy_gradient(show.legend = FALSE, color = "white") +
  # geom_path(
  #   data = aux_mediana,
  #   aes(mediana, factor(decada))
  # ) +
  labs(
    x = "Temperatura máxima do mês",
    y = "Ano"
  ) +
  scale_fill_viridis_c() +
  theme_few(base_size = 9)



clima %>% 
  mutate(mes = month(data)) %>% 
  ggplot(aes(t_med, rev(factor(mes)),  fill = ..x..)) +
  geom_joy_gradient(color = "white", show.legend = FALSE) +
  labs(
    x = "Temperatura média",
    y = "Mês"
  ) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0,40, 4)) +
  scale_y_discrete(labels = format(ISOdate(2000, 12:1, 1), "%b"))



clima %>% 
  mutate(mes = month(data)) %>% 
  ggplot(aes(prec, rev(factor(mes)),  fill = ..x..)) +
  geom_joy_gradient(color = "white", show.legend = FALSE) +
  labs(
    x = "Chuva mensal (mm)",
    y = "Mês"
  ) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0,700, 100)) +
  scale_y_discrete(labels = format(ISOdate(2000, 12:1, 1), "%b"))


clima_normal <- clima %>%
  filter(!is.na(t_med)) %>% 
  group_by(ano = year(data)) %>%
  summarise(t_med = mean(t_med, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(media_movel = roll_mean(t_med, 30, align = "right", fill = NA))

clima_normal %>% 
  filter(!is.na(media_movel)) %>% 
  ggplot(aes(media_movel, ano)) +
  geom_path() +
  geom_point() +
  scale_y_reverse(breaks = seq(1940, 2017, by = 5))




