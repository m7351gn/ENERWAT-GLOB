library(dplyr)
library(stringr)


insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


#primary energy Quad BTU  = 1.055 EJ = 293 TWh
eia.primary.energy <- read.csv(paste0('eia_total_energy_consumption.csv'))

country.list <- list()
total.energy.quadbtu.2015.list <- list() 

for(idx in seq(1,231)){
  
  if(idx == 1){
    
    
    #get country names
    find = 1 
    
    country.name.idx <- eia.primary.energy$Country[find]
    
    country.list[[idx]] <- country.name.idx
    
    #get total energy quadbtu
    find.energy = 2 
    
    country.energy.2015.idx <- eia.primary.energy$X2015[find.energy]
    
    total.energy.quadbtu.2015.list[[idx]] <- country.energy.2015.idx
    
  }
  
  else{
    
    country.name.idx <- eia.primary.energy$Country[find]
    
    country.list[[idx]] <- country.name.idx
    
    country.energy.2015.idx <- eia.primary.energy$X2015[find.energy]
    
    total.energy.quadbtu.2015.list[[idx]] <- country.energy.2015.idx

  }
  
  find = find + 8
  find.energy = find.energy + 8
  
}

countries <- do.call(rbind, country.list)
energy.quabtu.2015 <- do.call(rbind, total.energy.quadbtu.2015.list)

  
energy.df <- as.data.frame(
  cbind(countries, energy.quabtu.2015))

colnames(energy.df) <- c('Country','X2015.quadbtu')
energy.df$X2015.quadbtu <- as.numeric(energy.df$X2015.quadbtu)


energy.df.converter <- energy.df %>% 
  filter(!is.na(X2015.quadbtu)) %>% 
  arrange(Country) %>%
  mutate(X2015.ej = X2015.quadbtu * 1.055,
         X2015.twh = X2015.ej * 277.778) 

world.total <- energy.df.converter %>%
  filter(Country == 'World') 

energy.df.save <- energy.df.converter %>%
  filter(Country != 'World')
  
energy.df.save.forreal <- insertRow(
  energy.df.save,
  world.total, 1)

write.csv(energy.df.save.forreal, 'eia_total_energy_consumption_2015.csv',
          row.names = F)

#electricity is in billion kwh = TWh
eia.electricity <- read.csv(paste0('eia_electricity_net_consumption_TWh.csv'))

eia.electricity.2015 <- eia.electricity %>%
  select(Country, X2015)
eia.electricity.2015$Country <- str_trim(eia.electricity.2015$Country)
eia.electricity.2015$X2015 <- as.numeric(eia.electricity.2015$X2015)

eia.electricity.2015.clean <- eia.electricity.2015 %>%
  filter(!is.na(X2015)) %>%
  arrange(Country) %>% 
  rename(X2015.net.electricity = X2015) %>% 
  filter(Country != 'World') 

eia.electricity.2015.save <- insertRow(
  eia.electricity.2015.clean,
  eia.electricity.2015[eia.electricity.2015$Country == 'World', ], 1)

write.csv(eia.electricity.2015.save, 'eia_electricity_net_consumption_TWh_2015.csv',
          row.names = F)
