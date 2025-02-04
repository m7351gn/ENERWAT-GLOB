library(dplyr)
library(tidyr)
library(vroom)

inputDir   <- '../../../../output/water_treatment/model/desalination/0_tech_categories/'
outputDir <- '../../../../output/water_treatment/model/desalination/1_cumulative/'

#### load ####
desalData.energy <- read.csv(paste0(inputDir, 'DesalDataEnergy_online_2019.csv')) %>%
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) 

desalData.energy.RO <- vroom(
  paste0(inputDir,'DesalDataEnergy_RO_time_evolving.csv'), show_col_types = F) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  mutate(year.exponential=as.Date(paste(year.exponential, 1, 1, sep = "-"))) %>% 
  filter(!grepl('Construction', Plant.status)) %>%  
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status)) 

Pure <- desalData.energy[grepl("Pure", desalData.energy$Feedwater), ]

River <-  desalData.energy[grepl("River", desalData.energy$Feedwater), ]

Brackish <- desalData.energy[grepl("Brackish", desalData.energy$Feedwater), ]

Wastewater <-  desalData.energy[grepl("Wastewater", desalData.energy$Feedwater), ]

Seawater <-  desalData.energy[grepl("Seawater", desalData.energy$Feedwater), ]

Brine <-  desalData.energy[grepl("Brine", desalData.energy$Feedwater), ]


#### calculate cumulative capacities (m3.d) ####

#calculate cumulative capacity
cum.capacity.all.tech.frame <- desalData.energy %>% group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Total')

#create vector with the whole timeframe of DesalData
years_range_desalData <- as.data.frame(
  as.Date(
    seq.Date(
      cum.capacity.all.tech.frame$Online.date[1],
      cum.capacity.all.tech.frame$Online.date[nrow(cum.capacity.all.tech.frame)],
      by='year'))) %>% dplyr::rename(Online.date=1)

cum.capacity.all.tech.frame <- cum.capacity.all.tech.frame %>%
  merge(years_range_desalData, ., all=T)
cum.capacity.all.tech.frame$m3.d[2] <- 0
cum.capacity.all.tech.frame$cum.m3.d[2] <- cum.capacity.all.tech.frame$cum.m3.d[1]
cum.capacity.all.tech.frame$cum.m3.y[2] <- cum.capacity.all.tech.frame$cum.m3.y[1]
cum.capacity.all.tech.frame$tech[2] <- cum.capacity.all.tech.frame$tech[1]

#calculate capacities of indibidual feeds
Pure.C <- Pure %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Pure')

River.C <- River %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='River')

Brackish.C <- Brackish %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Brackish')

Wastewater.C <- Wastewater %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Wastewater')

Seawater.C <- Seawater %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Seawater')

Brine.C <- Brine %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Brine')

#checksum
cum.capacity.all.feeds <- Pure.C$cum.m3.d + River.C$cum.m3.d +
  Brackish.C$cum.m3.d + Wastewater.C$cum.m3.d + 
  Seawater.C$cum.m3.d +
  Brine.C$cum.m3.d

#### calculate energy evolving per subregion for RO ####
source('fun_1_2_capacity_energy_evolving_RO_per_feed.R')

#### calculate cumulative energy consumption (kwh.y) ####
#assume wastewater salinity is between brackish and seawater
Pure.E <- Pure %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Pure.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Pure.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Pure.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Pure')

River.E <- River %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + River.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + River.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + River.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='River')

Brackish.E <- Brackish %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Brackish.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Brackish.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Brackish.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Brackish')

Wastewater.E <- Wastewater %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Wastewater.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Wastewater.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Wastewater.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Wastewater')

Seawater.E <- Seawater %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Seawater.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Seawater.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Seawater.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Seawater')

Brine.E <- Brine %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Brine.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Brine.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Brine.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Brine')


#years
cum.energy.years <- years_range_desalData

#kwh.d
cum.all.tech.kwh.d.low <- Pure.E$kwh.d.low + River.E$kwh.d.low + Brackish.E$kwh.d.low +
  Wastewater.E$kwh.d.low +
  Seawater.E$kwh.d.low + Brine.E$kwh.d.low 
cum.all.tech.kwh.d.high <- Pure.E$kwh.d.high + River.E$kwh.d.high + Brackish.E$kwh.d.high +
  Wastewater.E$kwh.d.high +
  Seawater.E$kwh.d.high + Brine.E$kwh.d.high 
cum.all.tech.kwh.d.mean <- Pure.E$kwh.d.mean + River.E$kwh.d.mean + Brackish.E$kwh.d.mean +
  Wastewater.E$kwh.d.mean +
  Seawater.E$kwh.d.mean + Brine.E$kwh.d.mean 

#cum kwh.d
cum.all.tech.cum.kwh.d.low <- Pure.E$cum.kwh.d.low + River.E$cum.kwh.d.low + Brackish.E$cum.kwh.d.low +
  Wastewater.E$cum.kwh.d.low +
  Seawater.E$cum.kwh.d.low + Brine.E$cum.kwh.d.low 
cum.all.tech.cum.kwh.d.high <- Pure.E$cum.kwh.d.high + River.E$cum.kwh.d.high + Brackish.E$cum.kwh.d.high +
  Wastewater.E$cum.kwh.d.high +
  Seawater.E$cum.kwh.d.high + Brine.E$cum.kwh.d.high 
cum.all.tech.cum.kwh.d.mean <- Pure.E$cum.kwh.d.mean + River.E$cum.kwh.d.mean + Brackish.E$cum.kwh.d.mean +
  Wastewater.E$cum.kwh.d.mean +
  Seawater.E$cum.kwh.d.mean + Brine.E$cum.kwh.d.mean  

#total energy
cum.energy.all.tech.low <- Pure.E$cum.kwh.y.low + River.E$cum.kwh.y.low + Brackish.E$cum.kwh.y.low +
  Wastewater.E$cum.kwh.y.low +
  Seawater.E$cum.kwh.y.low + Brine.E$cum.kwh.y.low  
cum.energy.all.tech.high <- Pure.E$cum.kwh.y.high + River.E$cum.kwh.y.high + Brackish.E$cum.kwh.y.high +
  Wastewater.E$cum.kwh.y.high +
  Seawater.E$cum.kwh.y.high + Brine.E$cum.kwh.y.high  
cum.energy.all.tech.mean <- Pure.E$cum.kwh.y.mean + River.E$cum.kwh.y.mean + Brackish.E$cum.kwh.y.mean +
  Wastewater.E$cum.kwh.y.mean +
  Seawater.E$cum.kwh.y.mean + Brine.E$cum.kwh.y.mean  

cum.energy.all.tech.frame <- data.frame(
  cum.energy.years,
  cum.all.tech.kwh.d.low, cum.all.tech.kwh.d.mean, cum.all.tech.kwh.d.high,
  cum.all.tech.cum.kwh.d.low, cum.all.tech.cum.kwh.d.mean, cum.all.tech.cum.kwh.d.high,
  cum.energy.all.tech.low, cum.energy.all.tech.mean, cum.energy.all.tech.high) %>% 
  mutate(tech='Total')

# colnames(cum.energy.all.tech.frame) <- c('Online.date', 'kwh.d', 'cum.kwh.d', 'cum.kwh.y', 'tech')

colnames(cum.energy.all.tech.frame) <- colnames(Pure.E)

#### make dataframes for plotting ####
#### capacity
capacity.plot.data <- rbind(cum.capacity.all.tech.frame %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Pure.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            River.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Brackish.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Wastewater.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Seawater.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Brine.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds)) %>%
  mutate(cum.million.m3.d = cum.m3.d / 10^6) %>%
  mutate(tech = factor(tech, levels=c('Total',  'Pure', 'River', 'Wastewater',
                                      'Brackish', 'Seawater', 'Brine')))

#### energy
energy.plot.data <- rbind(cum.energy.all.tech.frame %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Pure.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          River.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          Brackish.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Wastewater.E %>%
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Seawater.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          Brine.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean)
) %>%
  mutate(cum.twh.y.low = cum.kwh.y.low / 10^9) %>% 
  mutate(cum.twh.y.mean = cum.kwh.y.mean / 10^9) %>% 
  mutate(cum.twh.y.high = cum.kwh.y.high / 10^9) %>% 
  mutate(tech = factor(tech, levels=c('Total',  'Pure', 'River', 'Wastewater',
                                      'Brackish', 'Seawater', 'Brine')))


#### save ####
#tables
capacity.table <- capacity.plot.data %>%
  mutate(Online.date = format(as.Date(Online.date, format="%d/%m/%Y"),"%Y")) %>% 
  relocate(ratio.capacity, .after = cum.million.m3.d) %>% 
  relocate(tech, .after = ratio.capacity)

energy.table <- energy.plot.data %>%
  mutate(Online.date = format(as.Date(Online.date, format="%d/%m/%Y"),"%Y")) %>% 
  relocate(ratio.energy.mean, .after = cum.twh.y.high) %>% 
  relocate(tech, .after = ratio.energy.mean)

write.csv(capacity.table,
           paste0(outputDir,'cumulative_capacity_1945_2019_feed.csv'),
          row.names = F)
write.csv(energy.table,
           paste0(outputDir,'cumulative_energy_1945_2019_feed.csv'),
          row.names = F)


