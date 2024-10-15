library(vroom)
library(dplyr)

countries.raster <- vroom('countries_raster_5arcmin.csv')

desaldata <- read.csv('DesalData_2019.csv')

#### add cell ID ####

PLANT_ID <- seq(1, 20321, 1)

desaldata.id <- cbind(PLANT_ID, desaldata)

write.csv(desaldata.id, 'DesalData_2019_ID.csv', row.names = F)

### rename countries in desaldata ####

# get names
countries.desaldata <- unique(desaldata$Country)

countries.raster.names <- unique(countries.raster$Country)
countries.raster.names <- countries.raster.names[order(countries.raster.names)]

diff.names <- setdiff(countries.desaldata, countries.raster.names)
diff.names

#rename
desaldata.id$Country[
  which(desaldata.id$Country == "Anguilla")] <- 
  'United Kingdom'

desaldata.id$Country[
  which(desaldata.id$Country == "Curaçao")] <- 
  'Curacao'

desaldata.id$Country[
  which(desaldata.id$Country == "Democratic Republic of the Congo")] <- 
  'Congo DRC'

desaldata.id$Country[
  which(desaldata.id$Country == "Guadeloupe")] <- 
  'France'

desaldata.id$Country[
  which(desaldata.id$Country == "Guernsey (UK)")] <- 
  'United Kingdom'

desaldata.id$Country[
  which(desaldata.id$Country == "Jersey (UK)")] <- 
  'United Kingdom'

desaldata.id$Country[
  which(desaldata.id$Country == "Mayotte")] <- 
  'France'

desaldata.id$Country[
  which(desaldata.id$Country == "Puerto Rico")] <- 
  'United States of America'

desaldata.id$Country[
  which(desaldata.id$Country == "Republic of Congo")] <- 
  'Congo Republic'

desaldata.id$Country[
  which(desaldata.id$Country == "Republic of Djibouti")] <- 
  'Djibouti'

desaldata.id$Country[
  which(desaldata.id$Country == "Saint Barthelemy")] <- 
  'France'

desaldata.id$Country[
  which(desaldata.id$Country == "Saint Helena")] <- 
  'United Kingdom'

desaldata.id$Country[
  which(desaldata.id$Country == "U.K.")] <- 
  'United Kingdom'

desaldata.id$Country[
  which(desaldata.id$Country == "U.S. Virgin Islands")] <- 
  'US Virgin Islands'

desaldata.id$Country[
  which(desaldata.id$Country == "U.S.A.")] <- 
  'United States of America'

desaldata.id$Country[
  which(desaldata.id$Country == "United States Minor Outlying Islands")] <- 
  'United States of America'



#check differences
countries.desaldata <- unique(desaldata.id$Country)

countries.raster.names <- unique(countries.raster$Country)
countries.raster.names <- countries.raster.names[order(countries.raster.names)]

diff.names <- setdiff(countries.desaldata, countries.raster.names)
diff.names


write.csv(desaldata.id, 'DesalData_2019_ID_renamed_countries.csv', row.names = F)
