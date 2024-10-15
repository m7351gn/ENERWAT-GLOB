library(dplyr)
library(ggplot2)
library(patchwork)


#### files ####
inputDirWorld <- '../../../../input/global_data/'
inputDirData <- '../../../../output/water_treatment/model/'
outputDir <- '../../../../output/water_treatment/visualization/general/'
dir.create(outputDir, recursive = T, showWarnings = F)


world.regions <- read.csv(paste0(inputDirWorld, 'countries_id_regions.csv'))
electricity.2015.countries <- read.csv(paste0(
  inputDirWorld, 'eia_electricity_net_consumption_TWh_2015.csv'))
primary.energy.2015.countries <- read.csv(paste0(
  inputDirWorld, 'eia_total_energy_consumption_2015.csv'))

ratios.desal <- read.csv(paste0(inputDirData, 'desalination/energy_countries_2015_desalination.csv'))
ratios.wwt <- read.csv(paste0(inputDirData, 'wwt/1_energy/energy_countries_2015_wwt.csv'))
ratios.dwt <- read.csv(paste0(inputDirData, 'dwt/2_energy/energy_countries_2015_dwt.csv'))

#### process ####
#summarise regional energy consumption (primary and electricity)

energy.2015.countries <- inner_join(electricity.2015.countries,
                                    primary.energy.2015.countries)

energy.2015.countries.regions <- inner_join(energy.2015.countries,
                                            world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(X2015.net.electricity.twh = sum(X2015.net.electricity.twh),
            X2015.primary.quadbtu = sum(X2015.primary.quadbtu),
            X2015.primary.ej = sum(X2015.primary.ej),
            X2015.primary.twh = sum(X2015.primary.twh)
  )

#summarise technologies 
regions.summarise.desal <- ratios.desal %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high)
  ) %>% 
  mutate(tech = 'Desalination') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100)

regions.summarise.wwt <- ratios.wwt %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high)
  ) %>% 
  mutate(tech = 'Wastewater treatment') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100)

regions.summarise.dwt <- ratios.dwt %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(energy.total.low.twh),
            twh.y.mean = sum(energy.total.mean.twh),
            twh.y.high = sum(energy.total.high.twh)
  ) %>% 
  mutate(tech = 'Conventional drinking water treatment') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100)


#### prepare plot data ####
#desalination
var1 <- data.frame(regions.summarise.desal$ratio.primary.energy.low,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var2 <- data.frame(regions.summarise.desal$ratio.primary.energy.mean,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var3 <- data.frame(regions.summarise.desal$ratio.primary.energy.high,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var4 <- data.frame(regions.summarise.desal$ratio.electricity.low,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var5 <- data.frame(regions.summarise.desal$ratio.electricity.mean,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var6 <- data.frame(regions.summarise.desal$ratio.electricity.high,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')


#wwt
var7 <- data.frame(regions.summarise.wwt$ratio.primary.energy.low,
                   regions.summarise.wwt$tech,
                   regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var8 <- data.frame(regions.summarise.wwt$ratio.primary.energy.mean,
                   regions.summarise.wwt$tech,
                   regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var9 <- data.frame(regions.summarise.wwt$ratio.primary.energy.high,
                   regions.summarise.wwt$tech,
                   regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var10 <- data.frame(regions.summarise.wwt$ratio.electricity.low,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var11 <- data.frame(regions.summarise.wwt$ratio.electricity.mean,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var12 <- data.frame(regions.summarise.wwt$ratio.electricity.high,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')


#cdwt

var13 <- data.frame(low = regions.summarise.dwt$ratio.primary.energy.low,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var14 <- data.frame(mean = regions.summarise.dwt$ratio.primary.energy.mean,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var15 <- data.frame(high = regions.summarise.dwt$ratio.primary.energy.high,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var16 <- data.frame(low = regions.summarise.dwt$ratio.electricity.low,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var17 <- data.frame(mean = regions.summarise.dwt$ratio.electricity.mean,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var18 <- data.frame(high = regions.summarise.dwt$ratio.electricity.high,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')


#desal
colnames(var1)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var2)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var3)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var4)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var5)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var6)[1:3] <- c('high', 'Technology', 'Subregion')

#wwt
colnames(var7)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var8)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var9)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var10)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var11)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var12)[1:3] <- c('high', 'Technology', 'Subregion')

#drinking water treatment
colnames(var13)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var14)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var15)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var16)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var17)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var18)[1:3] <- c('high', 'Technology', 'Subregion')


df.low <- data.frame(rbind(var1, var4, var7, var10, var13, var16))
df.mean <- data.frame(rbind(var2, var5, var8, var11, var14, var17))
df.high <- data.frame(rbind(var3, var6, var9, var12, var15, var18))

df.plot <- cbind(df.low, df.mean$mean, df.high$high)
colnames(df.plot)[5:6] <- c('mean','high')

df.plot.cleaned <- df.plot %>%
  relocate(mean, .after=low) %>%
  relocate(high, .after=mean) %>% 
  mutate(Technology = factor(Technology, 
                             levels = c('Desalination',
                                        'Wastewater treatment',
                                        'Conventional drinking water treatment')))




#### plot ####
df.electricity <- df.plot.cleaned %>% 
  filter(type == 'Electricity') 

df.electricity.order <- df.electricity %>% 
  group_by(Subregion) %>% 
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>% 
  arrange(desc(sum.mean))

df.primary <- df.plot.cleaned %>% 
  filter(type == 'Primary energy')

df.primary.order <- df.primary %>% 
  group_by(Subregion) %>% 
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>% 
  arrange(desc(sum.mean))


p.electricity <- ggplot(data = df.electricity %>% 
                          mutate(Subregion = factor(
                            Subregion, levels = df.electricity.order$Subregion)), #%>% 
                          # filter(Technology != 'Drinking water treatment'),
                        aes(x=Subregion, y=mean, 
                            color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  theme_light() +
  ylab('Ratio (%)\n') +
  ggtitle('Electricity') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE')) +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  scale_y_continuous(position = "right")  +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) 

p.primary <- ggplot(data = df.primary %>% 
                      mutate(Subregion = factor(
                        Subregion, levels = df.primary.order$Subregion)) ,
                    aes(x=Subregion, y=mean, color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(position = "left") +
  theme_light() +
  ylab('Ratio (%)\n') +
  ggtitle('Primary energy') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE')) +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) 

p.patch <- (p.primary | p.electricity )  +
  plot_annotation(
    title = 'Regional energetic toll of water treatment processes (2015)',
    subtitle = '% of total energy consumption') +
  plot_layout(guides='collect') &
  theme(legend.position="bottom",
        legend.spacing = unit(7,"cm"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title.y  = element_text(size = 13),
        axis.title.x  = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                     size = 13),
      
        axis.text = element_text(size = 10),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 11),
        legend.title = element_blank()
  ) 

ggsave(paste0(outputDir,'water_treatment_energy_ratio_regions.png'), p.patch,
       height=5, width=10, units='in', dpi=300)

file.show(paste0(outputDir
                 
                 ,'water_treatment_energy_ratio_regions.png'))