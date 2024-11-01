########################################
## 01_map_of_data_sites.R
##
## This code makes some maps that we will use to report
##   the locations of where data was collected.


library(tidyverse)

library(sf)
library(ggthemes)
library(ggspatial)

#################################
## Site Data

load("fully_processed_data.RData")



## Filter out the winter data, get the latitude & longitude
##   along with characteristics of the sites

site_data <- full_light_data %>%
  dplyr::select(SiteID, lat, long, BlagraveID) %>%
  distinct()


site_data <- st_as_sf(site_data, coords = c("long", "lat"),
                      crs = "NAD27")

################################
### Get State and Province outlines

#devtools::install_github("UrbanInstitute/urbnmapr")

library(urbnmapr)

usa_map <- fortify(get_urbn_map("states", sf = TRUE)) %>%
  filter(state_abbv %in% c("MN", "IA", "WI", "IL", "MI", "IN", "OH", "PA", "NY", "MO", "NJ", "CT", "MA", "VT"))
detach("package:urbnmapr", unload=TRUE)

### Canadian Province outlines are in the PROV dataset in canadianmaps
###    it is an sf object
### The PROV object is huge and slows down the computer. Get what we need
###    from it, and then detach the package
library(canadianmaps)
can_prov <- fortify(PROV) %>% filter(PRENAME %in% c("Ontario", "Quebec", "Manitoba"))
detach("package:canadianmaps", unload = TRUE)
can_prov <- st_transform(can_prov, crs="NAD27")

#### Lastly, some labels for the map

# label_df <- data.frame(Lake = c("Superior", "Huron", "Erie", "Ontario", "Michigan"),
#                        x = c(-87.40, -82.575, -80.5, -75.3, -85.54),
#                        y = c(47.78, 44.55, 42.2, 43.5, 43.5))
label_df <- data.frame(Lake = c("Superior", "Huron", "Erie", "Ontario"),
                       x = c(-87.40, -82.575, -80.5, -76.2),
                       y = c(47.78, 44.55, 42.2, 43.5))


p_map_sites <- ggplot() + 
  geom_sf(data=usa_map,  fill="gray98", color="gray75", linewidth=0.35) + 
  geom_sf(data=can_prov, fill="gray98", color="gray75", linewidth=0.35) +
  geom_sf(data=site_data,  size=0.95, color="gray30") + 
  geom_label(data=label_df, aes(x=x, y=y, label=Lake), color="gray40", size=3) +
  scale_color_manual(values=c("gray20", "gray50"), name="Sampling Design") +
  scale_shape_manual(name="Sampling Design", values=c(16, 17)) + 
  theme_map() +
  coord_sf(xlim = c(-92.31, -73.5), ylim = c(40.5, 50), expand = FALSE, crs="NAD27") +
  theme(legend.position.inside=c(0.8, 0.8),
        legend.background = element_rect(color="gray20"),
        legend.justification = "center") +
  annotation_scale(bar_cols=c("gray20", "gray90"), 
                   location="br", width_hint =0.4) + 
  annotation_north_arrow(height=unit(1, "cm"),
                         width=unit(1, "cm"),
                         style=north_arrow_nautical(fill=c("gray20", "gray90") ) )

p_map_sites

ggsave(filename="plots/map_of_sites.png", p_map_sites, bg="white",
       width=6, height=4)


