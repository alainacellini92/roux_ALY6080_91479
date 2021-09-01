#############################
###### ALY 6080 #############
## Forest & Inventory Data ## 
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
library(caret)

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
me_EAB_POS <- read.table("~/School/ALY6080/EAB USA/ME_EAB_Positive_finds.csv", sep = ",", head = TRUE, nrows=47) #Load CSV not EXCEL

options(scipen = 999)

#____________ Data cleaning ____________# 

#Access PLOT table 
me_plot <- me$PLOT

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

# Join me_plot_2 with tree on PLT_CN 
me_plot_3 <- sqldf("Select * from me_tree left join me_plot_2 on me_tree.PLT_CN = me_plot_2.PLT_CN") 


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

#Identify years in data 
#plot(me_plot_5$MEASYEAR)

## Filter for 2018 & 2019 
me_plot_6 <- subset(me_plot_5, MEASYEAR == 2018 |
                               MEASYEAR == 2019 )

#___________ Plot FIA + EAB Data __________# 

# Rename Lat and Lon fields to Match 
me_plot_6 <- rename(me_plot_6, latitude=LAT, 
                    longitude=LON)

me_EAB_POS <-rename(me_EAB_POS, latitude=Latitude, 
                    longitude=Longitude)

#Plot Lat & Lon for FIA data 
plot(me_plot_6$latitude, me_plot_6$longitude)

#Plot FIA data with EAB Data 
ggplot()+
  geom_point(aes(x= me_plot_6$longitude, y = me_plot_6$latitude)) +
  geom_point(aes(x=me_EAB_POS$longitude, y = me_EAB_POS$latitude, color= me_EAB_POS$Year)) +
  coord_fixed(ratio = 1)

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
  coord_fixed(ratio = 1)

#______________ Imbalanced Model _______________#

# separate the data into training (80%) and test (20%) sets ----
str(me_plot_7)

#Count NAs per column 
colSums(is.na(me_plot_7))

#Keep columns with few NAs (<1000)
me_plot_8 <- select(me_plot_7, PLOT_STATUS_CD, MEASYEAR, MEASMON, KINDCD,
                    WATERCD, ELEV, ECOSUBCD,  INVASIVE_SAMPLING_STATUS_CD, 
                    STATUSCD, SPCD, DIA, HT, TREECLCD, CR, CCLCD,
                    VOLCFNET, GROWCFGS, GROWBFSL, MORTCFGS, MORTBFSL, 
                    REMVCFGS, REMVBFSL, TPA_UNADJ, TPAMORT_UNADJ, TPAREMV_UNADJ,
                    TPAGROW_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, 
                    DRYBIO_BG, CARBON_AG, CARBON_BG, OWNCD, FORTYPCD, CONDPROP_UNADJ, 
                    EAB) 

str(me_plot_8)

#Change ECOSUBCD to be a factor 
me_plot_8$ECOSUBCD <- as.factor(me_plot_8$ECOSUBCD)

str(me_plot_8)

# Build training & testing datasets
set.seed(123)
train_indices <- sample(1:nrow(me_plot_8),
                        size = ceiling(0.8* nrow(me_plot_8)),
                        replace = FALSE)
# Training dataset
train <- me_plot_8[train_indices, ]

# Test dataset
test <- me_plot_8[-train_indices, ]

#Build a model on imbalanced data 
imbalance <- rpart(EAB ~., data=train)

pred.imbalance <- predict(imbalance, newdata = test, type = "class")

#Check the accuracy of the prediction 
accuracy.meas(test$EAB, pred.imbalance)
confusionMatrix(pred.imbalance, test$EAB)

#Check accuracy using ROC curve
roc.curve(test$EAB, pred.imbalance, plotit = T)

# Visualize
rpart.plot(imbalance, nn=TRUE)

#________ Class Balancing __________# 

### Use sampling techniques to try and improve prediction

# Over sampling 
data_balanced_over <- ovun.sample(EAB ~., data=train, 
                                  method = "over", 
                                  N = 1960)$data
table(data_balanced_over$EAB)

# Under sampling 
data_balanced_under <- ovun.sample(EAB ~ ., data=train, 
                                   method = "under", 
                                   N= 40, 
                                   seed=1)$data 

table(data_balanced_under$EAB)

# Both over and under sampling 
data_balanced_both <- ovun.sample(EAB ~., data = train, 
                                  method = "both", 
                                  p=.5, 
                                  N=1000, 
                                  seed=1)$data

table(data_balanced_both$EAB)

# Rose method based on original data 
data.rose <- ROSE(EAB ~ ., data = train, 
                  seed = 1)$data



#____________Build Decision Tree Models_________# 

# Rose - ERROR
tree.rose <- rpart(EAB ~ ., data = data.rose)
rpart.plot(tree.rose, nn=TRUE)
# Oversampling 
tree.over <- rpart(EAB ~ ., data = data_balanced_over)

#Under Sampling 
tree.under <- rpart(EAB ~ ., data = data_balanced_under)

#Both over and under 
tree.both <- rpart(EAB ~ ., data = data_balanced_both)
rpart.plot(tree.both, nn=TRUE)

#________ Predictions on Test Data ______________# 

# ERROR _ ROSE METHOD 
pred.tree.rose <- predict(tree.rose, newdata = test, type = "class")

# Oversampling 
pred.tree.over <- predict(tree.over, newdata = test, type = "class")

# Under sampling 
pred.tree.under <- predict(tree.under, newdata = test, type = "class")

# Both over and under sampling 
pred.tree.both <- predict(tree.both, newdata = test, type = "class")

#AUC ROSE - highest accuracy 
roc.curve(test$EAB, pred.tree.rose, main = "ROC curve (ROSE)")
confusionMatrix(pred.tree.rose, test$EAB)
accuracy.meas(test$EAB, pred.tree.rose)

#AUC Oversampling
roc.curve(test$EAB, pred.tree.over, main = "ROC curve (oversampling)")
confusionMatrix(pred.tree.over, test$EAB)

#AUC Undersampling
roc.curve(test$EAB, pred.tree.under, main = "ROC curve (undersampling)")
confusionMatrix(pred.tree.under, test$EAB)

#AUC Both 
roc.curve(test$EAB, pred.tree.both, main = "ROC curve (both)")
confusionMatrix(pred.tree.both, test$EAB)
