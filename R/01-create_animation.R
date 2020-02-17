pkgs = c(
  "devtools",       # for install_github
  "sf",             # spatial data classes
  "rnaturalearth",  # world map data
  "dplyr",          # data manipulation
  "purrr",          # data manipulation
  "ggplot2",        # visualisation
  "gganimate"       # animation
)
to_install = !pkgs %in% installed.packages()
if(any(to_install)) {
  install.packages(pkgs[to_install])
}

library(sf)             
library(rnaturalearth)  
library(dplyr)          
library(purrr)          
library(ggplot2)        
library(gganimate)  

world_orig = ne_countries(returnclass = "sf")

world_sel = world_orig %>% 
  select()

world_poly = world_sel %>% 
  st_cast("POLYGON")

world = world_poly %>% 
  st_transform(crs = 3857)

world_areas = world %>% 
  st_transform(crs = "+proj=moll") %>% 
  st_area()

map_areas = world %>% 
  st_set_crs(NA) %>% 
  st_area()

world_scaled = world %>% 
  mutate(scale = 1 / (sqrt(map_areas / world_areas))) %>% 
  mutate(scale = as.numeric(scale / max(scale)))

scaler = function(x, y, z) {
  (x - z) * y + z
}

world_geom = st_geometry(world) 
world_center = st_centroid(world_geom)

world_transf = pmap(list(world_geom, world_scaled$scale, world_center), scaler) %>% 
  st_sfc(crs = st_crs(world)) %>% 
  st_sf()

world$state = 1
world_transf$state = 2
worlds = rbind(world, world_transf)

worlds_anim = ggplot() +
  geom_sf(data = worlds, fill = "grey50") +
  transition_states(state, transition_length = 5, state_length = 2) + 
  ease_aes("cubic-in-out")

worlds_animate = animate(worlds_anim, nframes = 20,
                         width = 645, height = 645)

worlds_anim_void = worlds_anim +
  theme_void() 

worlds_animate_void = animate(worlds_anim_void, nframes = 20,
                              width = 645, height = 645)

anim_save("worlds_animate.gif", worlds_animate)
anim_save("worlds_animate2.gif", worlds_animate_void)

