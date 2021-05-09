################################################################################
# 0. settings
################################################################################

# load packages
require(tidyverse)
require(scales)
require(sf)
require(janitor)
require(cowplot)
# devtools::install_github("sboysel/murdock")
require(murdock)
require(raster)
require(sp)
require(GISTools)
require(Hmisc)
require(broom)
require(scico)
require(viridis)
require(patchwork)
require(labels)

# set working directory
setwd("H:/TidyTuesday")

################################################################################
# 1. data preparation 
################################################################################

#-------------------------------------------------------------------------------
# 1.1 regions
#-------------------------------------------------------------------------------

# load Murdock's ethnographic regions for Africa
# see: http://worldmap.harvard.edu/data/geonode:murdock_ea_2010_3
murdock_pol <- murdock
# proj4string(murdock_pol)
murdock_pol_tidy <- tidy(murdock_pol) # get a tidy version.

# extract information of different groups
dat_group_aux <- murdock_pol$NAME %>% as.data.frame() %>% 
  rename(id = 1) %>% bind_cols(
    # murdock_pol$Cultgrp_30 %>% as.data.frame() %>% 
    murdock_pol$V30 %>% as.data.frame() %>% 
      rename(cul_group = 1))

murdock_pol_group <- left_join(murdock_pol_tidy, dat_group_aux, by = "id")



#-------------------------------------------------------------------------------
# 1.2 water data
#-------------------------------------------------------------------------------

# read data https://www.waterpointdata.org/analyze-data/
data_water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv') %>% 
  # rename variables
  rename(lat = lat_deg, 
         lon = lon_deg) %>%
  # keep only coordinates within Africa
  filter(between(lat,-37, 35),
         between(lon, -40,60))

# check levels + frequencies
unique(data_water$water_source)
data_water %>% group_by(water_source) %>% dplyr::summarize(N = n()) %>% top_n(20) %>% arrange(desc(N))

# adjust levels
data_water <- data_water %>% 
  mutate(water_source = case_when(
    str_detect(water_source, "Surface Water") ~ "Surface Water",
    str_detect(water_source, "Shallow Well") ~ "Shallow Well",
    str_detect(water_source, "Spring") ~ "Spring",
    TRUE ~ water_source))

# check frequencies
data_water %>% group_by(water_source) %>% dplyr::summarize(N = n()) %>% top_n(10) %>% arrange(desc(N))  

# extract over a loop (only over the four most common sources)
type_sources <- c("Borehole","Shallow Well","Spring","Surface Water")

# https://gis.stackexchange.com/questions/110117/counting-number-of-points-in-polygon-using-r
# https://stackoverflow.com/questions/35141094/attribute-values-to-polygons-in-spatialpolygonsdataframe
for (i in 1:length(type_sources)) {
  
  water_aux <- data_water %>% dplyr::select(row_id,lon,lat,water_source) %>% filter(water_source == type_sources[i]) # current source
  water_aux_xy <- water_aux[,c(2,3)] # current coordinates 
  
  spdf_aux <- SpatialPointsDataFrame(coords = water_aux_xy, data = water_aux[,c(2,3)],
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # proj4string(spdf_aux)
  
  # count observation per polygon
  count_obs_aux <- poly.counts(spdf_aux, murdock_pol) 
  count_obs_aux2 <- setNames(count_obs_aux, murdock_pol@data$NAME) %>% as.data.frame() %>% 
    rename(count_water = 1) %>% 
    tibble::rownames_to_column("id")
  
  if (i==1) {
    count_obs <- count_obs_aux2 %>% rename(setNames("count_water",as.character(type_sources[i])))
  } else{
    count_obs <- bind_cols(count_obs, 
                           count_obs_aux2 %>%  dplyr::select(count_water) %>% rename(setNames("count_water",as.character(type_sources[i]))))
  }
  
}


# pivot long and add factor variable 
murdock_pol_tidy <- left_join(murdock_pol_tidy,count_obs, by = "id") %>%
  pivot_longer(!long:id, names_to = "water_source_id", values_to = "count_water") %>% 
  mutate(levels_water = cut(count_water, 
                            breaks = c(-10,0,10,100,1000,1000000),
                            labels = c("0 or NA",
                                       "1-10",
                                       "11-100",
                                       "101-1000",
                                       ">1000")))

################################################################################
# 2. visualization
################################################################################

#-------------------------------------------------------------------------------
# 2.1 regions
#-------------------------------------------------------------------------------

fig_1 <-
  ggplot() +
  geom_polygon(data = murdock_pol_group %>% 
                 mutate(cul_group = as.character(cul_group, add.non.labelled  = T),
                        cul_group = 
                          ifelse(cul_group == "",NA,
                                 ifelse(cul_group == "missing data",NA,cul_group)),
                        cul_group = case_when(
                          cul_group == "Neighborhoods of dispersed family homesteads" ~ "Neighborhoods of dispersed\nfamily homesteads",
                          cul_group == "Separated hamlets, forming a single community" ~ "Separated hamlets, forming\na single community",
                          TRUE ~ cul_group
                        )),
               
               
               aes(x=long, y=lat,
                   group = group,
                   fill = factor(cul_group)),
               color = "black")  +
  geom_path() +
  coord_fixed() +
  theme_void() +
  scale_fill_scico_d(palette = 'nuuk') +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(1.5,1,1,1), "cm"),
    text=element_text(family="Fira Sans Condensed"),
    plot.title = element_text(size=20, hjust = 0.5),
    strip.text  = element_text(size = 14, margin = margin(0, 0, 5, 0), color = "#58514B"),
    legend.text = element_text( size = 12, color = "black"),
    legend.title = element_text( size = 14, color = "black"),
    legend.key.height = unit(1, "cm"),
    legend.key.width  = unit(0.75, "cm")) +
  labs(fill = "Settlement pattern:",
       title = "A) The (historic) prevailing type of settlement pattern") +
  guides(fill=guide_legend(ncol=2,title.position="top", title.hjust = 0.5))
fig_1

#-------------------------------------------------------------------------------
# 2.2 water 
#-------------------------------------------------------------------------------

col <- scico(4, palette = 'batlow')
fig_2 <- ggplot() +
  geom_polygon(data = murdock_pol_tidy, aes(x=long, y=lat,
                                            group = group,
                                            fill = factor(levels_water)),
               color = "black")  +
  facet_wrap(~water_source_id)+
  geom_path() +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values = c("white",col)) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(1,1,1,1), "cm"),
    text=element_text(family="Fira Sans Condensed"),
    plot.title = element_text(size=20, hjust = 0.5),
    strip.text  = element_text(size = 14, margin = margin(0, 0, 5, 0), color = "#58514B"),
    legend.text = element_text( size = 12, color = "black"),
    legend.title = element_text( size = 14, color = "black"),
    legend.key.height = unit(1, "cm"),
    legend.key.width  = unit(0.75, "cm")
  ) +
  labs(fill = "Number of sources:",
       title = "B) Water Sources\n") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
fig_2

fig_1 + fig_2 + 
  plot_annotation(
    title = "Murdock's (historic) ethnographic regions, settlement  pattern\n& water sources in Africa",
    caption = "Source 1: Water Point Data Exchange, Source 2: Murdock, Blier, & Nunn.
  By: Sergei Schaub",
    theme = theme(plot.title = element_text(family = "Fira Sans Condensed SemiBold", size = 24,hjust = 0.5,face="bold")))+
  ggsave("tidy_water.png", dpi = 320, width = 14, height = 12)






