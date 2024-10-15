library(dplyr)
library(vroom)

#### input files ####
inputDirWorld <- '../../../../input/global_data/'
inputDirFlows <- '../../../../output/water_treatment/model/wwt/0_volumes/3_flows_model_output/'
inputDirEnergyData <- '../../../../input/water_treatment/wwt/'
outputDir <- '../../../../output/water_treatment/model/wwt/1_energy/'
dir.create(outputDir, showWarnings = F, recursive = T)

country.region.data <- read.csv(paste0(inputDirWorld, 'countries_id_regions.csv'))

#primary energy 
eia.primary.energy <- read.csv(paste0(
  inputDirWorld, 'eia_total_energy_consumption_2015.csv'))

#electricity 
eia.electricity <- read.csv(paste0(
  inputDirWorld, 'eia_electricity_net_consumption_TWh_2015.csv'))

#modelled data
plants.online <- read.csv(
  paste0(inputDirFlows, 'wwtp_modelled_flows.csv')) 

energy.data <- read.csv(
  paste0(inputDirEnergyData, 'Longo2016_secondary_wwt.csv'))

#### generate COD parameter per plant #### 
# start with simple 60 g cod / d capita
plants.online$COD <- plants.online$population_5arcmin * 60

# 120 g cod / d capita for US and Canada
plants.online$COD[
  plants.online$Country == 'Canada' | plants.online$Country == 'United States'] <- 
  plants.online$population_5arcmin[
    plants.online$Country == 'Canada' | plants.online$Country == 'United States'] * 120

#### standardize units in plants.online flows to day: volume [m3/d], COD [g/d] ####
plants.online$Average.flowrate..m3.d. <- plants.online$flow_5arcmin * 10^9 / 365 
plants.online$Influent..COD..g.d <- plants.online$COD 

#set dataframe to work with energy values
plants.online.energy <- plants.online



#### all the following are in kwh/d until further conversion
#### primary wwt ####
# energy use: 4.3 e-5 - 7.1 e-5 kwh/m3 

primary.intensity.low <- 0.000072
primary.intensity.high <- 0.013
primary.intensity.mean <- ( primary.intensity.low + primary.intensity.high ) / 2

plants.online.energy$primary.low <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.low

plants.online.energy$primary.mean <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.mean

plants.online.energy$primary.high <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.high


#### secondary wwt ####
#multiple linear regression 
#fit mlr using log
mlr.model.ro <- lm(log(Total.electricity.consumption..kWh.d.) ~
                     log(Average.flowrate..m3.d.) + log(Influent..COD..g.d),
                   # log(Influent..COD..g.d) + log(Effluent..COD..g.d),
                   # log(Influent..COD..g.d) + COD..removed..ratio,
                   # COD..removed..g.d,
                   data=energy.data)

# summary(mlr.model.ro)

#### predict energy use values for SECONDARY TREATMENT (log!!!) ####
predict.wwt.energy <- as_tibble(predict(mlr.model.ro, plants.online,
                                        interval = "confidence", level = 0.95))

#bind to plant -> ELEVATING EXP(PREDICTED)
plants.online.energy$secondary.low <- exp(predict.wwt.energy$lwr)
plants.online.energy$secondary.mean <- exp(predict.wwt.energy$fit)
plants.online.energy$secondary.high <- exp(predict.wwt.energy$upr)


#### advanced wwt - phosphorus and nitrogen removal ####
#nitrogen : 0.4-0.5 kwh/m3
advanced.n.low <- 0.4
advanced.n.high <- 0.5
advanced.n.mean <- ( advanced.n.low + advanced.n.high ) / 2

#phosphorus : 0.06 - 1.6 kWh/m3
advanced.p.low <- 0.06
advanced.p.high <- 1.6
advanced.p.mean <- ( advanced.p.low + advanced.p.high ) / 2

#nitrogen
plants.online.energy$advanced.n.low <- 
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.low
plants.online.energy$advanced.n.mean <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.mean
plants.online.energy$advanced.n.high <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.high

#phosphorus
plants.online.energy$advanced.p.low <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.low
plants.online.energy$advanced.p.mean <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.mean
plants.online.energy$advanced.p.high <- 
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.high



# #### divide by level ###
# plants.online.primary <- plants.online %>% 
#   filter(LEVEL == 'Primary')
# plants.online.secondary <- plants.online %>% 
#   filter(LEVEL == 'Secondary')
# plants.online.advanced <- plants.online %>% 
#   filter(LEVEL == 'Advanced')



#### assign energy use depending on level in plants.online ####
#empty arrays
plants.online.energy$kwh.d.low <- NA
plants.online.energy$kwh.d.mean <- NA
plants.online.energy$kwh.d.high <- NA

#primary 
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.low[plants.online.energy$LEVEL == 'Primary']
plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.mean[plants.online.energy$LEVEL == 'Primary']
plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.high[plants.online.energy$LEVEL == 'Primary']

#secondary
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.low[plants.online.energy$LEVEL == 'Secondary']
plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.mean[plants.online.energy$LEVEL == 'Secondary']
plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.high[plants.online.energy$LEVEL == 'Secondary']

#advanced : add both nitrogen and phosphorus uncertainties
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.low[plants.online.energy$LEVEL == 'Advanced'] + 
  plants.online.energy$advanced.n.low[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.low[plants.online.energy$LEVEL == 'Advanced']

plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.mean[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.n.mean[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.mean[plants.online.energy$LEVEL == 'Advanced']

plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.high[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.n.high[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.high[plants.online.energy$LEVEL == 'Advanced']


#### make kwh/d -> kwh/y ####
plants.online.energy$kwh.y.low <- NA
plants.online.energy$kwh.y.mean <- NA
plants.online.energy$kwh.y.high <- NA

plants.online.energy$kwh.y.low <- plants.online.energy$kwh.d.low * 365
plants.online.energy$kwh.y.mean <- plants.online.energy$kwh.d.mean * 365
plants.online.energy$kwh.y.high <- plants.online.energy$kwh.d.high * 365

plants.online.energy$twh.y.low <- plants.online.energy$kwh.y.low / (10^9)
plants.online.energy$twh.y.mean <- plants.online.energy$kwh.y.mean / (10^9)
plants.online.energy$twh.y.high <- plants.online.energy$kwh.y.high / (10^9)

plants.online.energy$ej.y.low <- plants.online.energy$twh.y.low * 3.6 * 10^(-3)
plants.online.energy$ej.y.mean <- plants.online.energy$twh.y.mean * 3.6 * 10^(-3)
plants.online.energy$ej.y.high <- plants.online.energy$twh.y.high * 3.6 * 10^(-3)


#### country energy use #### 
energy.country.use <- plants.online.energy %>%
  group_by(Country) %>%
  summarise(wwt.km3.y = sum(flow_5arcmin),
            pop.connected.2015 = sum(population_5arcmin),
            COD.kg.y = sum(COD * 365 / 1000),
            kwh.y.low = sum(kwh.y.low),
            kwh.y.mean = sum(kwh.y.mean),
            kwh.y.high = sum(kwh.y.high),
            twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high),
            ej.y.low = sum(ej.y.low),
            ej.y.mean = sum(ej.y.mean),
            ej.y.high = sum(ej.y.high)
            
            ) %>%
  
  mutate(kwh.m3.low = kwh.y.low / (wwt.km3.y * 1000000000),
         kwh.m3.mean = kwh.y.mean / (wwt.km3.y * 1000000000),
         kwh.m3.high = kwh.y.high / (wwt.km3.y * 1000000000),
         kwh.kgCODin.low = kwh.y.low / (COD.kg.y),
         kwh.kgCODin.mean = kwh.y.mean / (COD.kg.y),
         kwh.kgCODin.high = kwh.y.high / (COD.kg.y)
         
         ) 

#### add world total volumes and data ####
world.sums <- t(as.data.frame(
  colSums(energy.country.use %>% select(-Country))))
world.means <- t(as.data.frame(
  colMeans(energy.country.use %>% select(-Country))))
world.sums.to.use <- t(as.data.frame(world.sums[, 1:12]))
world.means.to.use <- t(as.data.frame(world.means[, 13:18]))
world.string <- 'World'

world.row <- cbind(world.string, world.sums.to.use, world.means.to.use)

row.names(world.row) <- NULL
colnames(world.row)[1] <- 'Country'

energy.country.use.world <- rbind(world.row, energy.country.use)

#### save plants and countries tables ####
vroom_write(
  plants.online.energy, paste0(outputDir, 'energy_plants_2015_wwt.csv'),
  delim = ',' )
vroom_write(
  energy.country.use.world, paste0(outputDir, 'energy_countries_2015_wwt.csv'),
  delim = ',' )


#### ratios ####
country.energy.model <- read.csv(paste0(outputDir, 'energy_countries_2015_wwt.csv'))

countries.with.data.primary <- inner_join(country.energy.model, eia.primary.energy)
countries.with.data.electricity <- inner_join(country.energy.model, eia.electricity)

countries.ratios <- inner_join(countries.with.data.primary,
                               countries.with.data.electricity) %>%
  mutate(ratio.primary.energy.low = (ej.y.low / X2015.primary.ej) * 100,
         ratio.primary.energy.mean = (ej.y.mean / X2015.primary.ej) * 100,
         ratio.primary.energy.high = (ej.y.high / X2015.primary.ej) * 100,
         ratio.electricity.low = (twh.y.low / X2015.net.electricity.twh) * 100,
         ratio.electricity.mean = (twh.y.mean / X2015.net.electricity.twh) * 100,
         ratio.electricity.high = (twh.y.high / X2015.net.electricity.twh) * 100)


write.csv(countries.ratios, paste0(outputDir, 'energy_countries_2015_wwt.csv'),
          row.names = F)
