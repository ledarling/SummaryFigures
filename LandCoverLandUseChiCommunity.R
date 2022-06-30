#Calculate municipal land cover by land use and create figures
#5/10/2022
#Lindsay Darling


#Load libraries----------

library(tidyverse)
library(tidylog)
library(magrittr)
library(readxl)
library(units)
library(scales)
library(sf)
library(gt)
library(gtExtras)
library(webshot)

#While I attempted to run this entire process in R, it was not up for the tabulate area
#for every muni. I instead created tables in model builder in arc pro. This
#code reads those tables, cleans them up, and creates figures and tables.

# Load muni layer-------------

muni <- st_read("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/City of Chicago Shapefiles/Boundaries_-_Community_Areas__current_/CommAreas.shp") %>% 
  mutate(area_ft2 =st_area(.)) %>% #add area
  drop_units(.) %>% #drop units
  mutate(areaAc = `area_ft2` / 43560) %>% #Convert to acres
  st_drop_geometry() %>% #Drop geometry
  arrange(., COMMUNITY) %>% #Sort by name
  mutate(NAME = COMMUNITY)
  #mutate(NAME = str_replace(COMMUNITY, "_", " "))


#Create color palette

#colors <- c('#458352', '#b6d886', '#e9c252', '#2c6495', '#d15d9a', '#000002', '#b2b2b2')
#colors2 <- c('#b2b2b2', '#000002', '#d15d9a', '#2c6495', '#e9c252', '#b6d886', '#458352')
colors <- c('#598128', '#e1cc11', '#613a1e', '#405f59', '#912427', '#000002', '#b2b2b2')
colors2 <- c('#b2b2b2', '#000002', '#912427', '#405f59', '#613a1e', '#e1cc11', '#598128')

target <- muni[53,'NAME'] #Change first value to run another muni

area <- muni[53,'areaAc'] #Make sure first value is the same as above

#create folder to catch figures

dir.create(paste0('D:/Dropbox/chicago region trees initiative/Data and Maps/CanopyRecommendations/MunicipalCanopySummaries/2022/Chicago/', target))
  

lulc <-  read_xls(paste0('D:/Dropbox/Forest Composition/composition/Maps/shapefiles/CommunityLandCover/2017/', target, '.xls')) %>% 
  rename(LandUse = `NEW`,
         Canopy = `VALUE_1`,
         Vegetation = `VALUE_2`,
         BareSoil = `VALUE_3`,
         Water =`VALUE_4`,
         Building = `VALUE_5`,
         RoadRail = `VALUE_6`,
         OtherPaved = `VALUE_7`) %>% 
  mutate(TotalCover = `Canopy` + `Vegetation` + `BareSoil` + `Water` + 
           `Building` + `RoadRail` + `OtherPaved`) %>% 
  mutate(pCanopy = `Canopy`/`TotalCover`,
         pVegetation = `Vegetation`/`TotalCover`,
         pBareSoil = `BareSoil`/`TotalCover`,
         pWater = `Water`/`TotalCover`,
         pBuilding = `Building`/`TotalCover`,
         pRoadRail = `RoadRail`/`TotalCover`,
         pOtherPaved = `OtherPaved`/`TotalCover`)

lulc%<>%
  mutate(areaCanopy = `Canopy` * area / as.numeric(colSums(lulc[,10])),
         areaVegetation = `Vegetation` * area / as.numeric(colSums(lulc[,10])),
         areaBareSoil = `BareSoil` * area / as.numeric(colSums(lulc[,10])),
         areaWater = `Water` * area / as.numeric(colSums(lulc[,10])),
         areaBuilding = `Building` * area / as.numeric(colSums(lulc[,10])),
         areaRoadRail = `RoadRail` * area / as.numeric(colSums(lulc[,10])),
         areaOtherPaved = `OtherPaved` * area / as.numeric(colSums(lulc[,10])))

#Change wd to place where we want to write files

setwd(paste0('D:/Dropbox/chicago region trees initiative/Data and Maps/CanopyRecommendations/MunicipalCanopySummaries/2022/Chicago/', target))


#Now that our table is lovely, make figures

#Pie charts

pie <- lulc %>% 
  pivot_longer(cols = c(areaCanopy, areaVegetation, areaBareSoil, areaWater, 
                        areaBuilding, areaRoadRail, areaOtherPaved)) %>% 
  dplyr::select(LandUse, name, value)  %>%  # drop extra column
  group_by(name) %>% 
  summarize(totArea = sum(value)) %>% 
  mutate(prop = round(`totArea` / sum(`totArea`) *100)) %>% 
  mutate(name=recode(name, 'areaCanopy' = '1', 'areaVegetation'= '2', 
                     'areaBareSoil' = '3', 'areaWater' = '4','areaBuilding' = '5', 
                     'areaRoadRail' = '6', 'areaOtherPaved' = '7')) %>% 
  arrange(name)

#Land cover pie-------------
png(filename= 'LCP.png', width=6, height=6, units="in", res = 300, bg = "transparent") #sets up to save the image
pie(pie$totArea, labels = paste(pie$prop, "%", sep = ""),
    col = colors, border = 'white')
dev.off() 


#Plantable pie-----------

pie2 <- data.frame('name' = c('Canopy', 'Plantable space', 'Not suitable'),
               'area' = c(as.numeric(pie[1,3]), as.numeric(pie[2,3] + pie[3,3] + pie[7,3]), 
                          as.numeric(pie[4,3] + pie[5,3] +pie[6,3])))

png(filename= 'PSP.png', width=6, height=6, units="in", res = 300, bg = "transparent") #sets up to save the image
pie(pie2$area, labels = paste(pie2$area, "%", sep = ""),
    col = c('#598128', '#e1cc11', '#b2b2b2'), border = 'white')
dev.off()

#Prep for percent cover

lulcFilt <- lulc %>% 
  filter(TotalCover >= 43560) #Remove land use types that are < 1 acre

pCov <- lulcFilt %>% 
  pivot_longer(cols = c(pCanopy, pVegetation, pBareSoil, pWater, 
                                       pBuilding, pRoadRail, pOtherPaved)) %>% 
  dplyr::select(LandUse, name, value) %>%  # drop extra columns
  mutate(sort = rep(1:7, nrow(lulcFilt)), #Add column to sort chart
         value = `value` * 100) 

#PercentCover--------------

png(filename = 'PC.png', width=6, height=4, units="in", res = 300)
pCov %>% mutate(name = fct_reorder(name, sort, .desc = TRUE)) %>%
                  ggplot(aes(y = LandUse, x = value, fill = name)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Percent cover") +
  ylab("Land use type") +
  scale_fill_manual(values = colors2,
                    name = '',
                    labels = c('Other paved', 'Road/rail', 'Building', 'Water',
                               'Bare soil', 'Vegetation', 'Canopy')) +
  #theme(legend.position="none") +
  theme(text = element_text(size = 12))
dev.off()
    
#Area cover-----------

aCov <- lulcFilt %>% pivot_longer(cols = c(areaCanopy, areaVegetation, areaBareSoil, areaWater, 
                                       areaBuilding, areaRoadRail, areaOtherPaved)) %>% 
  dplyr::select(LandUse, name, value) %>%  # drop extra columns
  mutate(sort = rep(1:7, nrow(lulcFilt))) %>%  #Add column to sort chart
  rename('area' = 'value',
         'cover' = 'name')

png(filename = 'AC.png', width=6, height=4, units="in", res = 300)
aCov %>% mutate(cover = fct_reorder(cover, sort, .desc = TRUE)) %>%
  ggplot(aes(y = LandUse, x = area, fill = cover)) + 
  geom_bar(position = 'stack', stat = 'identity') +
  theme_bw() +
  xlab("Acres of cover") +
  ylab("Land use type") +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = colors2,
                    name = '',
                    labels = c('Other paved', 'Road/rail', 'Building', 'Water',
                               'Bare soil', 'Vegetation', 'Canopy')) +
  theme(legend.position="none") +
  theme(text = element_text(size = 12))
dev.off()

#Plantable percent-------------

pPlant <- lulcFilt %>% 
  select(LandUse, pCanopy:pOtherPaved) %>% 
  gather(Cover, Value, pCanopy:pOtherPaved) %>% 
  spread(LandUse, Value) %>% 
  mutate(Type = c('Plantable', 'Not suitable', 'Canopy', 'Plantable', 
                  'Not suitable', 'Plantable', 'Not suitable')) 
  
pPlantLong <- pPlant %>% group_by(Type) %>% 
  summarize(#Agriculture = sum(Agriculture),
            Commercial = sum(Commercial),
            Residential = sum(Residential),
            #Golf = sum(Golf),
            Industrial = sum(Industrial),
            Institutional = sum(Institutional),          
            Vacant = sum(Vacant),
            #`Natural area` = sum(`Natural area`),
            Park = sum(Park),
            Other = sum(Other),
            Transit = sum(Transit),
            Utility = sum(Utility)) %>% 
  pivot_longer(cols = c(Commercial:Utility)) %>% 
  dplyr::select(Type, name, value) %>% 
  mutate(sort = rep(c(3,2,1), each = 9), #Add column to sort chart
         value = `value` * 100) 

png(filename = 'PP.png', width=6, height=4, units="in", res = 300)
pPlantLong %>% mutate(Type = fct_reorder(Type, sort, .desc = TRUE)) %>%
  ggplot(aes(y = name, x = value, fill = Type)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Percent cover") +
  ylab("Land use type") +
  scale_fill_manual(values = c('#598128', '#b2b2b2', '#e1cc11'),
                    name = '') +
  theme(legend.position="none") +
  theme(text = element_text(size = 12))
dev.off()

#Plantable space cover-----------

aPlant <- lulcFilt %>% 
  select(LandUse, areaCanopy:areaOtherPaved) %>% 
  gather(Cover, Value, areaCanopy:areaOtherPaved) %>% 
  spread(LandUse, Value) %>% 
  mutate(Type = c('Plantable', 'Not suitable', 'Canopy', 'Plantable', 
                  'Not suitable', 'Plantable', 'Not suitable')) 


aPlantLong <- aPlant %>% group_by(Type) %>% 
  summarize(Commercial = sum(Commercial),
            Residential = sum(Residential),
            #Golf = sum(Golf),
            Industrial = sum(Industrial),
            Institutional = sum(Institutional),          
            Vacant = sum(Vacant),
            #`Natural area` = sum(`Natural area`),
            Park = sum(Park),
            Other = sum(Other),
            Transit = sum(Transit),
            Utility = sum(Utility)) %>% 
  pivot_longer(cols = c(Commercial:Utility)) %>% 
  dplyr::select(Type, name, value) %>% 
  mutate(sort = rep(c(3,2,1), each = 9)) #Add column to sort chart

png(filename = 'AP.png', width=6, height=4, units="in", res = 300)
aPlantLong %>% mutate(Type = fct_reorder(Type, sort, .desc = TRUE)) %>%
  ggplot(aes(y = name, x = value, fill = Type)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Acres of cover") +
  ylab("Land use type") +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c('#598128', '#b2b2b2', '#e1cc11'),
                    name = '') +
  theme(legend.position="none") +
  theme(text = element_text(size = 12))
dev.off()

#Table----------

table <- lulcFilt %>% 
  dplyr::select('LandUse', 'areaCanopy', 'pCanopy', 'areaVegetation', 
                'pVegetation', 'areaBareSoil', 'pBareSoil', 'areaWater', 'pWater', 
                'areaBuilding', 'pBuilding', 'areaRoadRail', 'pRoadRail', 
                'areaOtherPaved', 'pOtherPaved') %>% 
  arrange(., LandUse)

webshot::install_phantomjs()

tab <- table %>% 
  gt() %>% 
  gt_theme_espn() %>% 
  fmt_number(columns = c(areaCanopy, areaVegetation, areaBareSoil, areaWater,
                         areaBuilding, areaRoadRail, areaOtherPaved), 
             decimals = 1) %>% 
  fmt_percent(columns = c(pCanopy, pVegetation, pBareSoil, pWater, pBuilding,
                          pRoadRail, pOtherPaved), decimals = 1) %>% 
  tab_spanner(label = "Canopy", columns = matches("Canopy")) %>% 
  tab_spanner(label = "Vegetation", columns = matches("Vegetation")) %>% 
  tab_spanner(label = "Bare Soil", columns = matches("BareSoil")) %>%   
  tab_spanner(label = "Water", columns = matches("Water")) %>% 
  tab_spanner(label = "Buildings", columns = matches("Building")) %>% 
  tab_spanner(label = "Roads/rail", columns = matches("RoadRail")) %>% 
  tab_spanner(label = "Other paved", columns = matches("OtherPaved")) %>% 
  cols_label(areaCanopy ="Acres", pCanopy = "Percent",
             areaVegetation ="Acres", pVegetation = "Percent",
             areaBareSoil ="Acres", pBareSoil = "Percent",
             areaWater ="Acres", pWater = "Percent",
             areaBuilding ="Acres", pBuilding = "Percent",
             areaRoadRail ="Acres", pRoadRail = "Percent",
             areaOtherPaved ="Acres", pOtherPaved = "Percent",
             LandUse = "Land use") %>% 
  gt_color_rows(c(areaCanopy, pCanopy, areaBareSoil, pBareSoil, areaBuilding,
                  pBuilding, areaOtherPaved, pOtherPaved), palette = '#9d9e92')

gtsave(tab, filename = "TB.png")

#Neighbor cover------

muni2 <- st_read("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/MuniMinusWillFullName.shp")  %>% 
  dplyr::select(NAME)

cover <- read_csv('D:/Dropbox/Forest Composition/composition/Maps/LandCover/2017/MuniCover.csv') %>% 
  rename(NAME = `NAME,C,19`)
  
muniSelect<- muni2 %>% 
  filter(NAME == target)

getNeighbor <- muniSelect %>% 
  st_touches(., muni2) %>% 
  unlist(.) 

Neighborlulc <- muni2[c(getNeighbor),] %>% 
  st_drop_geometry() %>% 
  left_join(., cover) %>% 
  rbind(filter(cover, NAME == target)) %>% 
  rename(Canopy = `VALUE_1,N,19,11`,
         Vegetation = `VALUE_2,N,19,11`,
         BareSoil = `VALUE_3,N,19,11`,
         Water =`VALUE_4,N,19,11`,
         Building = `VALUE_5,N,19,11`,
         RoadRail = `VALUE_6,N,19,11`,
         OtherPaved = `VALUE_7,N,19,11`) %>% 
  mutate(TotalCover = `Canopy` + `Vegetation` + `BareSoil` + `Water` + 
           `Building` + `RoadRail` + `OtherPaved`) %>% 
  mutate(pCanopy = `Canopy`/`TotalCover`,
         pVegetation = `Vegetation`/`TotalCover`,
         pBareSoil = `BareSoil`/`TotalCover`,
         pWater = `Water`/`TotalCover`,
         pBuilding = `Building`/`TotalCover`,
         pRoadRail = `RoadRail`/`TotalCover`,
         pOtherPaved = `OtherPaved`/`TotalCover`) 

Neighborlulc %<>%
  pivot_longer(cols = c(pCanopy, pVegetation, pBareSoil, pWater, 
                        pBuilding, pRoadRail, pOtherPaved)) %>% 
  dplyr::select(NAME, name, value) %>%  # drop extra columns
  mutate(sort = rep(1:7, nrow(Neighborlulc)), #Add column to sort chart
         value = round(`value`,digits = 2),
         label = paste0(value * 100, "%")) 
  

png(filename = 'NC.png', width=6, height=4, units="in", res = 300)
Neighborlulc %>% mutate(name = fct_reorder(name, sort, .desc = TRUE)) %>%
  ggplot(aes(y = NAME, x = value, fill = name, 
             label = ifelse(value > .02, label, ""))) + 
  geom_bar(stat = 'identity', position = 'fill') +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  xlab("Percent cover") +
  ylab("Municipality") +
  scale_fill_manual(values = colors2,
                    name = '',
                    labels = c('Other paved', 'Road/rail', 'Building', 'Water',
                               'Bare soil', 'Vegetation', 'Canopy')) +
  theme(legend.position="none") +
  geom_text(size = 3.5, 
            position = position_stack(vjust = 0.5), col = 'white') +
theme(text = element_text(size = 12))
dev.off()


#Woody invasive--------
# 
# inv <- read_csv('D:/Dropbox/Forest Composition/composition/iTree/WoodyInvasiveCompare.csv') %>% 
#   mutate(Year = as.character(Year), 
#          Value = round(`Percent`,digits = 2),
#          label = paste0(Value * 100, "%")) 
# 
# 
# ggplot(inv,                         # Draw barplot with grouping & stacking
#        aes(x = Year,
#            y = Percent,
#            fill = Species,
#            label= ifelse(Value > .01, label, ""))) + 
#   geom_bar(stat = "identity",
#            position = "stack") +
#   facet_grid(~ County) +
#   geom_text(size = 3.5, 
#             position = position_stack(vjust = 0.5), col = 'white') +
#   theme(text = element_text(size = 12)) +
#   scale_fill_manual(values = colors[c(1,4)]) +
#   theme_bw() +
#   ylab("Percent Abundance") +
#   scale_y_continuous(labels = scales::percent) +
#   theme(legend.position = 'top',
#         legend.title = element_blank())
# 
