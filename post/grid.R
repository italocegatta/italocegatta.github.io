
 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, sf)
pacman::p_load_gh("italocegatta/brmap")
 


 
grid <- st_make_grid(brmap_brasil, cellsize = 5) %>% 
  st_sf()

ggplot() + 
  geom_sf(data = brmap_brasil) +
  geom_sf(data = grid, fill = NA) +
  theme_bw()
 

 
intersects <- st_intersects(grid, brmap_brasil, F) %>% as.vector()

grid_intersects <- filter(grid, intersects) %>% 
  st_cast("POLYGON") 

ggplot() + 
  geom_sf(data = brmap_brasil) +
  geom_sf(data = grid_intersects, fill =NA) +
  theme_bw()
 

 
grid_intersection <- st_intersection(grid_intersects, brmap_brasil) %>% 
  st_cast("MULTIPOLYGON")

ggplot() + 
  geom_sf(data = brmap_brasil) +
  geom_sf(data = grid_intersection, fill =NA) +
  theme_bw()
 

 
focos <- read_csv2("https://raw.githubusercontent.com/italocegatta/italocegatta.github.io_source/master/content/dados/base_incendios.csv") %>% 
  filter(ano == 2016)

focos
 
 
focos_sf <- focos %>% 
  left_join(brmap_estado, by = "sigla") %>% 
  st_sf()

ggplot(focos_sf) +
  geom_sf(aes(fill = focos)) +
  geom_sf(data = grid_intersection, fill =NA, color = "black") +
  scale_fill_viridis_c() +
  theme_bw()
 

 
# focos_interpolado <- st_interpolate_aw(focos_sf["focos"], grid_intersection, extensive = FALSE)
# 
# ggplot(focos_interpolado ) +
#   geom_sf(aes(fill = focos)) +
#   scale_fill_viridis_c() +
#   theme_bw()
 

