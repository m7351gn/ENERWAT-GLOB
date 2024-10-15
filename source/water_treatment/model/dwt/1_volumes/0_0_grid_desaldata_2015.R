library(dplyr)
library(vroom)
library(FNN)
library(terra)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../../input/water_treatment/dwt/'
inputDirWorld <- '../../../../../input/global_data/'
outputDir <- '../../../../../output/water_treatment/model/dwt/1_volumes/0_drinking_desalination/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### part 1 :::: attributes each plant to the nearest gridcell (centroid) ####
#load point data
data <- read.csv(paste0(inputDir, 'DesalDataEnergy_online_2015.csv')) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(Customer.type == "Municipalities as drinking water (TDS 10ppm - <1000ppm)"  
         | Customer.type == "Tourist facilities as drinking water (TDS 10ppm - <1000ppm)")

#load text file of gridcells with cell ID and coordinates
gridcells <- vroom(paste0(inputDirWorld, 'countries_raster_5arcmin.csv'),
                   show_col_types = F)

#get non-NA cell IDs
not.na.gridcells <- gridcells[!is.na(gridcells$Country),]

#### assign cell IDs to plants ####
#loop over countries to constrain assignment to same country
data.countries <- intersect(unique(gridcells$Country), unique(data$Country))


#empty list for appending
data.with.cellids.list <- list()

for(country in 1:length(data.countries)){
  
  replaceMessage(paste0('Country ', country, '/', length(data.countries)))
  
  #get country plants
  country.plants <- data %>% 
    filter(Country==data.countries[country]) 
  
  #get country pixels
  gridcells.country <- not.na.gridcells %>% 
    # filter(Country == data.countries[country])
    filter(Country == data.countries[country])
  
  #make dataframe of plant coordinates
  coord.plants.country <- as.data.frame(cbind(
    country.plants$Longitude, country.plants$Latitude))
  
  #make dataframe of gridcells coordinates
  coord.cells.country <- as.data.frame(cbind(
    gridcells.country$cell_lon, gridcells.country$cell_lat))
  
  #find nearest cell to plants
  matches <- knnx.index(coord.cells.country, coord.plants.country, k = 1)
  
  #find nearest neighbour cell ID
  nearest.cell.id <- as.data.frame(
    gridcells.country$cell_ID[matches[,1]])
  colnames(nearest.cell.id) <- 'pcrglobwb_cellID'
  
  #bind to country plants
  country.plants.cellid <- cbind(country.plants, nearest.cell.id)
  
  #append to global list 
  data.with.cellids.list[[country]] <- country.plants.cellid
  
}

#explode list
data.with.cellids <- do.call(rbind, data.with.cellids.list)

#save intermediate output
write.csv(data.with.cellids, 
          paste0(outputDir, 'desaldata_drinking_2015_gridcells.csv'), row.names = F)


#### add desalination capacity to 5 arcmin country table ####
desal.drinking.5arc <- data.with.cellids %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(drinking.desalination.m3.y = sum(Capacity..m3.d.) * 365) %>% 
  rename(cell_ID = pcrglobwb_cellID)

desal.drinking.5arc.all <- merge(gridcells, desal.drinking.5arc, all = T)

desal.drinking.5arc.clean <- desal.drinking.5arc.all %>% 
  select(cell_ID, cell_lon, cell_lat, drinking.desalination.m3.y) %>% 
  rename(lon = cell_lon,
         lat = cell_lat) %>% 
  replace(is.na(.), 0)

vroom_write(desal.drinking.5arc.clean, 
            paste0(outputDir, 'desaldata_drinking_5arcmin_2015.csv'), ',')
