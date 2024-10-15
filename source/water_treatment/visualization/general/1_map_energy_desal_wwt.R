library(dplyr)
library(ggplot2)
library(vroom)
library(ggthemes)
library(patchwork)
library(ggtext)

#### inputs ####
inputDirWorld <- '../../../../input/global_data/'
inputDirData <- '../../../../output/water_treatment/model/'
outputDir <- '../../../../output/water_treatment/visualization/general/'

wg <- map_data("world")

countries.raster <- vroom(paste0(inputDirWorld, 'countries_raster_5arcmin.csv'))

energy.plants.desal <- read.csv(paste0(
  inputDirData, 
  'dwt/1_volumes/0_drinking_desalination/desaldata_drinking_2015_gridcells.csv')) %>% 
  mutate(ej.log = log10(ej.y.mean))

energy.plants.wwt <- read.csv(paste0(inputDirData, 
                                     'wwt/1_energy/energy_plants_2015_wwt.csv')) %>% 
  mutate(ej.log = log10(ej.y.mean))

my.palette <- c("#7FC97F", "#AFF80A","#BEAED4", 
                "#FDC086","#386CB0", "#F0027F")

#### aggregate to 5 arcmin ####

energy.5arcmin.desal <- energy.plants.desal %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(ej.y.mean = sum(ej.y.mean),
            ej.log = sum(ej.log)) %>% 
  inner_join(countries.raster %>% 
               select(cell_ID, cell_lon, cell_lat) %>% 
               rename(pcrglobwb_cellID = cell_ID))

energy.5arcmin.wwt <- energy.plants.wwt %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(ej.y.mean = sum(ej.y.mean),
            ej.log = sum(ej.log)) %>% 
  inner_join(countries.raster %>% 
               select(cell_ID, cell_lon, cell_lat) %>% 
               rename(pcrglobwb_cellID = cell_ID))


#### plot ####
energy.plot.desal <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.desal, aes(x = cell_lon, y = cell_lat, 
                                      color = ej.y.mean, fill= ej.y.mean),
             shape = 22, 
             size = 2, stroke=0) +
  scale_fill_gradientn(colours = my.palette, na.value = 'transparent',
                     name = 'EJ y<sup>-1</sup>', trans = 'log10', limits = c(1e-10,1e-04)) +
  scale_color_gradientn(colours = my.palette,  na.value = 'transparent',
                      guide = 'none',trans = 'log10', limits = c(1e-10,1e-04)) +
  theme_map() +
  ggtitle('Desalination') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

energy.plot.wwt <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.wwt, aes(x = cell_lon, y = cell_lat, 
                                    color = ej.y.mean, fill=ej.y.mean),
             shape = 22, 
             size = 1, stroke=0) +
  scale_fill_gradientn(colours = my.palette, na.value = 'transparent',
                     name = 'EJ y<sup>-1</sup>', trans = 'log10', limits = c(1e-10,1e-04)) +
  scale_color_gradientn(colours = my.palette,  na.value = 'transparent',
                      guide = 'none',trans = 'log10', limits = c(1e-10,1e-04))  + 
  theme_map() +
  ggtitle('Wastewater treatment') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

pp.energy <- (energy.plot.desal / energy.plot.wwt) + #/ pop.plot) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = '5 arcmin average modelled energy consumption (2015)\n',
                  theme = theme(plot.title = element_text(hjust = 0.5 , size = 24))) &
  theme(legend.position = 'right',
        legend.direction = 'vertical',
        legend.spacing= unit(4, 'cm'))

#
ggsave(paste0(outputDir, 'energy_spatial_desal_wwt.png'), pp.energy,
       height=13, width=12, units='in', dpi=300, bg='white')
#
file.show(paste0(outputDir, 'energy_spatial_desal_wwt.png'))

