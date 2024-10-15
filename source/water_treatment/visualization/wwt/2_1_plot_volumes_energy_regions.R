library(dplyr)
library(vroom)
library(ggplot2)
library(reshape2)
library(patchwork)
library(ggtext)

inputDirWorld <- '../../../../input/global_data/'
inputDirRaw <- '../../../../input/water_treatment/wwt/'
inputDirEnergy <- '../../../../output/water_treatment/model/wwt/1_energy/'
outputDir <- '../../../../output/water_treatment/visualization/wwt/'
outputDirTables <- '../../../../output/water_treatment/model/wwt/1_energy/'


#### files #### 

wwp.data <- vroom(paste0(inputDirRaw, 'WWP_Jones2021_5arcmin.csv'))
wwt.data <- read.csv(paste0(inputDirEnergy, 'energy_countries_2015_wwt.csv'))

countries.raster <- vroom(
  paste0(inputDirWorld, 'countries_raster_5arcmin.csv')) %>% 
  select(cell_ID, country_ID) %>% 
  filter(country_ID != -9999)

countries.regions <- read.csv(
  paste0(inputDirWorld, 'countries_id_regions.csv')) 

regions.prod <- wwp.data %>% 
  inner_join(countries.raster) %>% 
  rename(ID = country_ID) %>%
  inner_join(., countries.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(wwp.km3.2015 = sum(wwp))

wwt.data.regions <- inner_join(wwt.data, countries.regions)

#### processing and plotting ####
regions.summarise <- wwt.data.regions %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(wwt.km3.y = sum(wwt.km3.y),
            COD.kg.y = sum(COD.kg.y),
            ej.y.low = sum(ej.y.low),
            ej.y.mean = sum(ej.y.mean),
            ej.y.high = sum(ej.y.high)
  ) %>% 
  inner_join(regions.prod) %>% 
  mutate(ratio.treated.2015 = wwt.km3.y / wwp.km3.2015 * 100)

untreated <- data.frame(regions.summarise$REGION_WB_SHORT,
                        regions.summarise$wwt.km3.y,
                        regions.summarise$wwp.km3.2015,
                        regions.summarise$wwp.km3.2015 - 
                          regions.summarise$wwt.km3.y)
colnames(untreated) <- c('REGION_WB_SHORT','wwt.km3.y',
                         'wwp.km3.2015','wwu.km3.2015')

#### plot volumes #### 

#side by side bar plot with point ratio
var1 <- regions.summarise$wwt.km3.y
var2 <- regions.summarise$wwp.km3.2015
var3 <- regions.summarise$ratio.treated.2015
x <- regions.summarise$REGION_WB_SHORT

plot.df <- data.frame(var1,var2,var3,x) %>% 
  # sort your dataframe
  arrange(desc(var3)) %>%          
  # reset your factor-column based on that order
  mutate(x = factor(x, unique(x))) %>%  
  select(-var3)

colnames(plot.df) <- c('Treated wastewater',
                       'Produced wastewater',
                       'Subregion')
plot.df.melted <- melt(plot.df, id.vars = 'Subregion')

plot.df2 <- data.frame(x, var3)
colnames(plot.df2) <- c('Subregion',
                        'var3')

scaling.factor.vol <- 1.2 #scaling for second variable

p.vol <- ggplot(plot.df.melted, aes(x=Subregion, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  geom_point(data = plot.df2 %>% 
               mutate(var = 'Ratio treated (%)'), aes(x=Subregion, 
                                                  y=var3 * scaling.factor.vol,
                                                  color = var),
             inherit.aes = F) +
  geom_hline(
    yintercept = mean(regions.summarise$ratio.treated.2015) * scaling.factor.vol,
    linetype = 'dashed') +
  ggtitle('A. Wastewater volumes (2015)') +
  ylab(expression(paste("Volume", " (km"^"3", ")"))) +
  scale_y_continuous(
    sec.axis = sec_axis(~./scaling.factor.vol, name = "\nRatio treated (%)")) +
  scale_fill_manual(values = c("#00BFC4", "#A6761D")) +
  scale_color_manual(values = '#000000') +
  theme_light() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
# p.vol


#### processing ####
regions.summarise <- inner_join(wwt.data,
                                countries.regions) %>% 
  
  group_by(REGION_WB_SHORT) %>% 
  summarise(wwt.km3.y = sum(wwt.km3.y),
            twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high),
            kwh.m3.low = mean(kwh.m3.low),
            kwh.m3.mean = mean(kwh.m3.mean),
            kwh.m3.high = mean(kwh.m3.high),
            kwh.kgCODin.low = mean(kwh.kgCODin.low),
            kwh.kgCODin.mean = mean(kwh.kgCODin.mean),
            kwh.kgCODin.high = mean(kwh.kgCODin.high),
            ratio.primary.energy.low = mean(ratio.primary.energy.low),
            ratio.primary.energy.mean = mean(ratio.primary.energy.mean),
            ratio.primary.energy.high = mean(ratio.primary.energy.high),
            ratio.electricity.low = mean(ratio.electricity.low),
            ratio.electricity.mean = mean(ratio.electricity.mean),
            ratio.electricity.high = mean(ratio.electricity.high)
            
  )



#### plot total energy use per group ####
#side by side bar plot with point ratio

scaling.factor.intensity <- 10 #scaling for second variable

var1 <- data.frame(regions.summarise$twh.y.low,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption (TWh)')
var2 <- data.frame(regions.summarise$twh.y.mean,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption (TWh)')
var3 <- data.frame(regions.summarise$twh.y.high,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption (TWh)')
var4 <- data.frame(regions.summarise$kwh.m3.low * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / m3)')
var5 <- data.frame(regions.summarise$kwh.m3.mean * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / m3)')
var6 <- data.frame(regions.summarise$kwh.m3.high * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / m3)')
var7 <- data.frame(regions.summarise$kwh.kgCODin.low * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / kgCODin)')
var8 <- data.frame(regions.summarise$kwh.kgCODin.mean * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / kgCODin)')
var9 <- data.frame(regions.summarise$kwh.kgCODin.high * scaling.factor.intensity,
                   regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Intensity (kWh / kgCODin)')


colnames(var1)[1] <- 'low'
colnames(var2)[1] <- 'mean'
colnames(var3)[1] <- 'high'
colnames(var4)[1] <- 'low'
colnames(var5)[1] <- 'mean'
colnames(var6)[1] <- 'high'
colnames(var7)[1] <- 'low'
colnames(var8)[1] <- 'mean'
colnames(var9)[1] <- 'high'
colnames(var1)[2] <- 'REGION_WB_SHORT'
colnames(var2)[2] <- 'REGION_WB_SHORT'
colnames(var3)[2] <- 'REGION_WB_SHORT'
colnames(var4)[2] <- 'REGION_WB_SHORT'
colnames(var5)[2] <- 'REGION_WB_SHORT'
colnames(var6)[2] <- 'REGION_WB_SHORT'
colnames(var7)[2] <- 'REGION_WB_SHORT'
colnames(var8)[2] <- 'REGION_WB_SHORT'
colnames(var9)[2] <- 'REGION_WB_SHORT'


df.low <- data.frame(rbind(var1, var4, var7))
df.mean <- data.frame(rbind(var2, var5, var8))
df.high <- data.frame(rbind(var3, var6, var9))


df.plot <- cbind(df.low, df.mean$mean, df.high$high)
colnames(df.plot)[4:5] <- c('mean','high')
df.plot.cleaned <- df.plot %>%
  relocate(mean, .after=low) %>%
  relocate(high, .after=mean) %>%
  relocate(REGION_WB_SHORT, .before = low) %>% 
  mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT,
                                  levels=c('EAP',  'NAM', 'WE', 'MENA',
                                           'EECA', 'LAC', 'SAS', 'SSA')))


df.plot.cleaned$cat <- factor(df.plot.cleaned$cat, 
                              levels = c('Total energy consumption (TWh)',
                                         'Intensity (kWh / m3)',
                                         'Intensity (kWh / kgCODin)'
))

p.energy <- ggplot(data = df.plot.cleaned, aes(x=REGION_WB_SHORT, y=mean, fill=cat)) +
  geom_bar(stat="identity", position='dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(
    sec.axis = sec_axis(~./scaling.factor.intensity, 
                        name = expression(
                          paste("kWh", " m"^"-3 ", "| kWh", " kgCODin"^"-1 ")))) +
  scale_fill_manual(values = c('#648FFF', '#FE6100', '#FFB000'),
                    labels = c('Total energy consumption (TWh)',
                               'Intensity (kWh m<sup>-3</sup>)',
                               'Intensity (kWh kgCODin<sup>-1</sup>)')) +
  theme_light() +
  xlab('Subregion') +
  ylab("Total energy consumption (TWh)") +
  ggtitle('B. Energy consumption (2015)') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = 'bottom')
# p.energy


#### patch ####
p.patch <- (p.vol + p.energy) +
  plot_layout(guides='collect') &
  theme(legend.position="bottom",
        legend.direction = 'vertical',
        legend.spacing = unit(1,"cm"),
        legend.text = element_markdown(size=11),
        axis.title.x = element_text(size = 12,
                                    margin=margin(t=20)),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size=9),
        plot.title = element_text(hjust=0.5, size=16)
        
  )

#### save things ####
ggsave(paste0(outputDir,'wwt_vol_energy.png'), p.patch,
       height=5, width=9, units='in', dpi=300)

write.csv(regions.summarise, paste0(outputDirTables, 'energy_regions_2015_wwt.csv'),
          row.names = F)

file.show(paste0(outputDir,'wwt_vol_energy.png'))