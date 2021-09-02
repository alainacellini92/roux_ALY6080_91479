#############################
###### ALY 6080 #############
## Forest & Inventory Data ##
######## EDA ################ 
#############################

#______ Install Packages and libraries________# 

install.packages('rFIA')
install.packages ('dplyr')
install.packages('devtools')
install.packages('rgdal')
install.packages('tigris')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages("sp")
install.packages("rgeos")
install.packages('sf')
install.packages('spdep')
install.packages('FNN')
install.packages("rpart.plot")
library(rFIA)
library(dplyr)
library(devtools)
library(rgdal)
library(tigris)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(fuzzyjoin)
library(sp)
library(rgeos)
library(sf)
library(spdep)
library(FNN)
library(ROSE)
library(rpart)
library(rpart.plot)

#____________ Load Data __________________# 

## Download the ME subset from the FIA database online 
## Save as an object
## If files already loaded, use readFIA instead 
me <- getFIA(states="ME")

# Load dataset me_plot.csv & me_tree.csv
#me_tree <- read.table(file.choose(), sep = ",", head = TRUE)
#me_plot <- read.table(file.choose(), sep = ",", head = TRUE)

## Download county name data for ME 
meCounties <- counties("Maine", cb = TRUE)

# load positive EAB findings
me_EAB_POS <- read.table(file.choose(), sep = ",", head = TRUE, nrows=47) #Load CSV ME_positive_finds.csv
options(scipen = 999)

#____________ Data cleaning ____________# 

#Access PLOT table 
me_plot <- me$PLOT
# number of unique plots from the sample
Unique_CN_ME <- unique(me_plot$CN)
length(Unique_CN_ME)

#Access COND table 
me_COND <- select(me$COND, PLT_CN, CONDID, OWNCD, OWNGRPCD, FORTYPCD,
                  STDSZCD, SITECLCD, CONDPROP_UNADJ, MICRPROP_UNADJ, 
                  SUBPPROP_UNADJ, PHYSCLCD, GSSTKCD, ALSTKCD, BALIVE,
                  CARBON_LITTER, CARBON_SOIL_ORG, CARBON_STANDING_DEAD,
                  CARBON_UNDERSTORY_AG, CARBON_UNDERSTORY_BG)


# Join me_plot with me_COND 
# The foreign key for the COND table is PLT_CN.
me_plot_2 <- sqldf("select * from me_plot left join me_COND on me_plot.CN = me_COND.PLT_CN")

## Access the tree table, remove duplicate variables  
me_tree<- select(me$TREE, -INVYR, -STATECD, -UNITCD, -COUNTYCD, -PLOT, -CYCLE, 
                 -SUBCYCLE)

#Check number of unique records 
Unique_SPCD <- unique(me_tree$SPCD)
length(Unique_SPCD)
prop.table(sort(table(me_tree$SPCD), decreasing = TRUE)) *100
# SPCD 12 = Balsam Fir
# 316 = Red Maple
# 97 = Red Spruce
# 241 = Northern White Cedar
# 375 = paper Birch 

# Join me_plot_2 with tree on PLT_CN 
me_plot_3 <- sqldf("Select * from me_tree left join me_plot_2 on me_tree.PLT_CN = me_plot_2.PLT_CN") 
# identify % of plots in each ecosub region
prop.table(sort(table(me_plot_3$ECOSUBCD), decreasing = TRUE))*100


#Keep only relevant variables
me_plot_4<- me_plot_3 %>% select("CN", "CTY_CN", "INVYR", "PLOT","PLOT_STATUS_CD", "MEASYEAR",
                                 "MEASMON","KINDCD", "WATERCD","LAT","LON","ELEV","ECOSUBCD", 
                                 "INVASIVE_SAMPLING_STATUS_CD", "PLT_CN","COUNTYCD","PLOT", 
                                 "TREE", "CONDID","STATUSCD", "SPCD", "SPGRPCD", "DIA", "HT",
                                 "HTCD", "ACTUALHT","TREECLCD", "CR", "CCLCD", "TREEGRCD", "AGENTCD", 
                                 "VOLCFNET","VOLCSNET", "VOLBFNET", "GROWCFGS", "GROWBFSL", "MORTCFGS", 
                                 "MORTCFGS", "MORTBFSL", "REMVCFGS", "REMVBFSL", "TPA_UNADJ", "TPAMORT_UNADJ",
                                 "TPAREMV_UNADJ", "TPAGROW_UNADJ", "DRYBIO_BOLE", "DRYBIO_TOP","DRYBIO_STUMP", 
                                 "DRYBIO_SAPLING", "DRYBIO_BG", "CARBON_AG", "CARBON_BG", "CONDID", 
                                 "OWNCD", "OWNGRPCD", "FORTYPCD", "STDSZCD",
                                 "SITECLCD", "CONDPROP_UNADJ", "PHYSCLCD", "GSSTKCD", "ALSTKCD", 
                                 "BALIVE", "CARBON_LITTER", "CARBON_SOIL_ORG", 
                                 "CARBON_STANDING_DEAD", "CARBON_UNDERSTORY_AG","CARBON_UNDERSTORY_BG")

# Filter for Ash trees 
# subset to SPCD = c(541, 543, 544) (these are the ash tree codes)
me_plot_5 <- subset(me_plot_4, SPCD %in% c(541, 543, 544))
prop.table(sort(table(me_plot_5$ECOSUBCD), decreasing = TRUE))*100

#Identify years in data 
#plot(me_plot_5$MEASYEAR)

## Filter for 2018 & 2019 
me_plot_6 <- subset(me_plot_5, MEASYEAR == 2018 |
                            MEASYEAR == 2019 )

#___________ Plot FIA + EAB Data __________# 

# Note: Data will be joined later on 

# Rename Lat and Lon fields to Match 
me_plot_6 <- rename(me_plot_6, latitude=LAT, 
                    longitude=LON)

me_EAB_POS <-rename(me_EAB_POS, latitude=Latitude, 
                    longitude=Longitude)



#Plot FIA data with EAB Data 
ggplot()+
        geom_point(aes(x= me_plot_6$longitude, y = me_plot_6$latitude)) +
        geom_point(aes(x=me_EAB_POS$longitude, y = me_EAB_POS$latitude, color= me_EAB_POS$Year)) +
        coord_fixed(ratio = 1) + ggtitle("FIA Survey Plots with ASh Trees vs areas with Postive infestations") +
  xlab("Latitude Coordinates") + ylab("Longitud Coordinates")+labs(color = "Postive EAB Identification")

#____________________ EDA _____________________# 

#Note - Majority of EDA done using the rFIA package 

## Check spatial coverage of plots held in FIA 
PlotMap <- plotFIA(me, 
        plot.title = "Spatial coverage of plots in Maine",
        text.size = .8)
PlotMap

##Plot of TPA (Abundance) over Time 
# TPA = Trees per acre
tpaME <- tpa(me)

TimeSeries <- plotFIA(tpaME, TPA, 
        plot.title= "ME TPA (Abundance) over time", 
        text.size = .8) 
TimeSeries

## BAA over time, grouped by county
tpaME_County <- tpa(me, grpBy = COUNTYCD)

plotFIA(tpaME_County, y = BAA, grp = COUNTYCD, text.size = .55 )

## TPA by county, Maine 
tpaRecent<- tpa(clipFIA(me), polys = meCounties, returnSpatial = TRUE)


## Produce animated plot of TPA bt county 
PlotStanding <- plotFIA(tpaRecent, y = TPA, animate = FALSE, 
        plot.title = 'Estimated abundance of Standing Trees',
        text.size=.8, 
        legend.height=.4)
PlotStanding

## BAA by size class (not a time series) grouped by species
tpaME_sc<- tpa(clipFIA(me), bySpecies = TRUE, bySizeClass = TRUE)
plotFIA(tpaME_sc, y = BAA, grp = COMMON_NAME, x = sizeClass, n.max = 5)# Only the top 5
# }

## BAA by county, Maine 
tpaRecent<- tpa(clipFIA(me), polys = meCounties, returnSpatial = TRUE)
str(tpaRecent)

## Produce animated plot
plotFIA(tpaRecent, y = BAA, animate = FALSE, 
        plot.title = 'Estimated Basal Area, by acre')

##Filter for Ash Trees 
## White, black, green ash SPCD codes
ash_codes <- c(541, 543, 544)

## Ash TPA over time
tpaMEAsh <- tpa(me,
                   treeDomain = SPCD %in% ash_codes & DIA > 5)

TimeSeriesAsh <- plotFIA(tpaMEAsh, TPA, 
                      plot.title= "ME TPA Ash (Abundance) over time", 
                      text.size = .8) 
TimeSeriesAsh

## Ash TPA by county
tpa_ash_county <- tpa(me, polys = meCounties,
               treeDomain = SPCD %in% ash_codes & DIA > 5)


## Ash mortality over time
gmMEAsh <- growMort(me,
                treeDomain = SPCD %in% ash_codes & DIA > 5)

## Ash mortality by county
gm_ash_county <- growMort(me, polys = meCounties,
                   treeDomain = SPCD %in% ash_codes & DIA > 5)


## TPA by county, Maine - Filtered for Ash 
tpaRecent<- tpa(clipFIA(me, COMMON_NAME %in% Ash_Names), polys = meCounties, returnSpatial = TRUE)
str(tpaRecent)
## Produce animated plot
plotFIA(tpaRecent, y = BAA, animate = FALSE, 
        plot.title = 'Estimated Basal Area, by acre, Ash Trees')

str(me_tree)

#Most Recent Year 
meRecent <- clipFIA(me, mostRecent=TRUE)
summary(meRecent)

## TPA & BAA for the most recent inventory year
#abundance of standing trees
tpaME_MR <- tpa(meRecent, totals=TRUE)
tpaME_MR

## All Inventory Years Available (i.e., returns a time series)
tpaME <- tpa(me, totals=TRUE)
tpaME

## Plot-level
tpaME_plot <- tpa(meRecent, byPlot = TRUE)
head(tpaME_plot)

## Subplot-level
tpaME_subp <- tpa(meRecent, byPlot = TRUE, grpBy = SUBP)

## Tree-level
tpaME_tree <- tpa(meRecent, byPlot = TRUE, grpBy = TREE)
tpaME_tree

#Group by Species - Recent
tpaME_species <- tpa(meRecent, bySpecies= TRUE, totals=TRUE)
tpaME_species
View(tpaME_species)

#Group by Species - All time 
tpaME_AshOverTime <- tpa()

#Group by size class 
tpaME_SizeClass <- tpa(meRecent, bySizeClass=TRUE, totals=TRUE)
View(tpaME_SizeClass)

#Group by species & Size Class 
tpaME_Species_Size<- tpa(meRecent, bySpecies=TRUE ,bySizeClass=TRUE, totals=TRUE)
View(tpaME_Species_Size)

#Plot all inventory years in ME 
str(tpaME)
plotFIA(tpaME, y=TPA, plot.title ='Total Trees Per Acre Over Time')

## Group estimates by the areal units, and return as a dataframe
tpa_polys <- tpa(meRecent, polys = )

## Same as above, but return an sf mulitpolygon object (spatially enabled)
tpa_polysSF <- tpa(riMR, polys = countiesRI, returnSpatial = TRUE)

## Group by species and size class, and plot the distribution 
##  for the most recent inventory year

Ash_Names <- c("white ash", "black ash", "green ash")
AshTrees <- filter(tpaME_Species_Size, COMMON_NAME %in% Ash_Names)
AshTrees

#All Trees Size Distribution 
plotFIA(tpaME_Species_Size, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distributions of BAA by species', 
        x.lab = 'Size Class (inches)', text.size = .75,
        n.max = 5) # Only want the top 5 species, try n.max = -5 for bottom 5

#Ash Tree Size Distribution 
PlotAshDist <-plotFIA(AshTrees, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distributions of Ash Trees', 
        x.lab = 'Size Class (inches)', text.size = .8)
PlotAshDist


grid.arrange(PlotMap, TimeSeries, nrow = 1)
grid.arrange(PlotStanding, PlotAshDist, nrow = 1)


#_________ Join FIA + EAB Data ____________# 

# Convert FIA data frame to sf object
sf_me_plot <- st_as_sf(x = me_plot_6, 
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# Check in plot 
#plot(sf_me_plot)

# convert to spatial object 
me_sp <- as(sf_me_plot, "Spatial")
class(me_sp)
class(me_EAB_POS)

# Convert EAB data frame to sf object
sf_EAB<- st_as_sf(x = me_EAB_POS, 
                  coords = c("longitude", "latitude"),
                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Convert to spatial object 
EAB_sp <- as(sf_EAB, "Spatial")

# Check the class
class(EAB_sp)

#Check number of unique records 
Unique_CN <- unique(me_plot_6$CN)
length(Unique_CN)

# FIA coordinates do not exactly match EAB coordinates
# We need to find the nearest neighbor in EAB to ME FIA points 
d <- get.knnx(coordinates(me_sp), coordinates(EAB_sp), 1)

#Matrix with a column for each nearest neighbor 
d_index <- d$nn.index[,1]
me_sp[d_index,1]

FIA_index <- c(unique(me_sp[d_index,1]$CN))
EAB_pos <-  me_plot_6$CN %in% FIA_index
FIA_index <- as.data.frame(FIA_index)

str(FIA_index)
names(FIA_index)[1] <- c('CN')
FIA_index$EAB <- c("1")

# Join me_plot_6 with the EAB Y/A data 
me_plot_7 <- dplyr::left_join(me_plot_6, FIA_index, by="CN")

#check the class 
class(me_plot_7)

# Change NA in column EAB to NO 
me_plot_7$EAB[is.na(me_plot_7$EAB)]<- "0"

# Convert to a factor 
me_plot_7$EAB <- as.factor(me_plot_7$EAB)

# Summary and Prop table to see class imbalance 
summary(me_plot_7$EAB)
prop.table(table(me_plot_7$EAB))
table(me_plot_7$EAB)

#Test FIA + EAB Join 
me_plot_EAB_TRUE <- subset(me_plot_7, EAB == "1")

ggplot()+
        geom_point(aes(x=me_plot_EAB_TRUE$LON, y = me_plot_EAB_TRUE$LAT)) +
        geom_point(aes(x=me_EAB_POS$longitude, y = me_EAB_POS$latitude, color= me_EAB_POS$Year)) +
        coord_fixed(ratio = 1) + ggtitle("FIA Survey Plots with ASh Trees vs areas with Postive infestations") +
  xlab("Latitude Coordinates") + ylab("Longitud Coordinates")+labs(color = "Postive EAB Identification")


# ECOSUBCD proptional tablbles 
prop.table(sort(table(me_plot$ECOSUBCD), decreasing = TRUE))*100



