###################################################
##   ALY 6070 - Ssummer 2021 XN                  ##
##   AFinal Deliverable Code - Weather/Revenue   ##
##   Joe Reynolds & Thomas Stahlhuth             ##
##   September 2nd, 2021                         ##
###################################################


library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(nhstplot)
library(corrplot)
library(RColorBrewer)
library(leafs)
library(MASS)
library(ISLR)
library(caret)
library(gridExtra)
library(pRoc)
library(plyr)
library(caTools)
library(pscl)
library(car)
library(class)
library(gt)

getwd()
setwd()

Cutler <- read.csv(file="C:/Users/thoma/OneDrive/Desktop/Cutler2.1.csv")
KBHB <- read.csv(file="C:/Users/thoma/OneDrive/Desktop/ForecastingDataTrialKBHB.csv")
KBXM <- read.csv(file="C:/Users/thoma/OneDrive/Desktop/ForecastingDataTrial1.csv")

str(Cutler)
str(KBHB)
str(KBXM)

summary(Cutler)
count(Cutler$one_hour_without_gust)
count(Cutler$three_hour_without_gust)
summary(KBHB)
summary(KBXM)

names(Cutler)[31] <- "launch"
Cutler$launch <- as.factor(Cutler$launch)
Cutler$one_hour_without_gust <- as.factor(Cutler$one_hour_without_gust)

plot(Cutler$wind_speed~Cutler$temp, col = Cutler$launch)
abline(lm(Cutler$wind_speed~Cutler$temp))

#New Cutler EDA to find out basic information 
LaunchWindowsByYear <- data.frame(Cutler$year,Cutler$launch)
summary(LaunchWindowsByYear)
Year2010 <- filter(LaunchWindowsByYear, Cutler.year== 2010)
summary(Year2010)
Year2011 <- filter(LaunchWindowsByYear, Cutler.year== 2011)
summary(Year2011)
Year2012 <- filter(LaunchWindowsByYear, Cutler.year== 2012)
summary(Year2012)
Year2013 <- filter(LaunchWindowsByYear, Cutler.year== 2013)
summary(Year2013)
Year2014 <- filter(LaunchWindowsByYear, Cutler.year== 2014)
summary(Year2014)
Year2015 <- filter(LaunchWindowsByYear, Cutler.year== 2015)
summary(Year2015)
Year2016 <- filter(LaunchWindowsByYear, Cutler.year== 2016)
summary(Year2016)
Year2017 <- filter(LaunchWindowsByYear, Cutler.year== 2017)
summary(Year2017)
Year2018 <- filter(LaunchWindowsByYear, Cutler.year== 2018)
summary(Year2018)
Year2019 <- filter(LaunchWindowsByYear, Cutler.year== 2019)
summary(Year2019)
Year2020 <- filter(LaunchWindowsByYear, Cutler.year== 2020)
summary(Year2020)
Year2021 <- filter(LaunchWindowsByYear, Cutler.year== 2021)
summary(Year2021)


#********* Creating The SVM Model *************#


#Consolidating models to fit the binary factors, and for the numeric values themselves 

#binary Cutler 
CutlerBinaryData <- data.frame(Cutler$wind_go,
                               Cutler$temp_go,
                               Cutler$condition_go,
                               Cutler$wind_direction_go,
                               Cutler$one_hour_without_gust,
                               Cutler$three_hour_without_gust)
#Binary BarHarbor
BarHarborBinaryData <- data.frame(KBHB$wind_go,
                        KBHB$temp_go,
                        KBHB$condition_go,
                        KBHB$wind_direction_go,
                        KBHB$one_hour_window,
                        KBHB$three_hour_window)
#Binary Brunswick
BrunswickBinary <- data.frame(KBXM$wind_go,
                              KBXM$temp_go,
                              KBXM$condition_go,
                              KBXM$wind_direction_go,
                              KBXM$one_hour_window,
                              KBXM$three_hour_window)

CutlerBinaryData$Cutler.three_hour_without_gust <- as.factor(CutlerBinaryData$Cutler.three_hour_without_gust)
BarHarborBinaryData$KBHB.three_hour_window <- as.factor(BarHarborBinaryData$KBHB.three_hour_window)
BrunswickBinary$KBXM.three_hour_window <- as.factor(BrunswickBinary$KBXM.three_hour_window)


names(CutlerBinaryData)[1] <- "wind_go"
names(BarHarborBinaryData)[1] <- "wind_go"
names(BrunswickBinary)[1] <- "wind_go"

names(CutlerBinaryData)[2] <- "temp_go"
names(BarHarborBinaryData)[2] <- "temp_go"
names(BrunswickBinary)[2] <- "temp_go"

names(CutlerBinaryData)[3] <- "conditions_go"
names(BarHarborBinaryData)[3] <- "conditions_go"
names(BrunswickBinary)[3] <- "conditions_go"

names(CutlerBinaryData)[4] <- "wind_direction_go"
names(BarHarborBinaryData)[4] <- "wind_direction_go"
names(BrunswickBinary)[4] <- "wind_direction_go"

names(CutlerBinaryData)[5] <- "one_hour_window"
names(BarHarborBinaryData)[5] <- "one_hour_window"
names(BrunswickBinary)[5] <- "one_hour_window"

names(CutlerBinaryData)[6] <- "launch"
names(BarHarborBinaryData)[6] <- "launch"
names(BrunswickBinary)[6] <- "launch"

str(CutlerBinaryData)
str(BarHarborBinaryData)
str(BrunswickBinary)

#Attempting to create a SVM Model 
intrain <- createDataPartition(y=CutlerBinaryData$launch, p=0.99, list=FALSE)
train <- CutlerBinaryData[intrain,]


test1 <- BrunswickBinary
test2 <- BarHarborBinaryData

#checking the dimensions of the model
dim(train);
dim(test1);
dim(test2);



traincontrol<- trainControl(method = "repeatedcv", number = 5, repeats = 3)
#To Fine tune the model walk it through more iterations!

svmModel <- train(launch~., data=train, method = "svmLinear",
                  trControl=traincontrol,
                  preProcess= c("center","scale"),
                  tuneLength = 8)

svmModel
TestPrediction <- predict(svmModel, newdata = train)
TestPrediction
confusionMatrix(table(TestPrediction, train$launch))

confusionMatrix(table(svmModel,train$launch))

#Test Data set Brunswick
TestPrediction <- predict(svmModel, newdata = test1)
TestPrediction
confusionMatrix(table(TestPrediction, test1$launch))


#Test Data set Bar Harbor
TestPrediction <- predict(svmModel, newdata = test2)
TestPrediction
confusionMatrix(table(TestPrediction, test2$launch))



grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(launch ~., data = train, method = "svmLinear",
                         trControl=traincontrol,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 8)

svm_Linear_Grid
plot(svm_Linear_Grid)

TestingLinearGrid <- predict(svm_Linear_Grid, newdata = test)
TestingLinearGrid

confusionMatrix(table(TestingLinearGrid, test$heartdisease))


#Logistic Regression Model 
Model7 <- glm(launch ~ wind_go + temp_go + conditions_go + wind_direction_go + one_hour_window, data= train, family = 'binomial')
summary(Model7)

#Pushing data going through models
#Test
res2 <- predict(Model7, test2, type = "response")
res2

res1 <- predict(Model7, test1, type = "response")
res1
#Train
res <- predict(Model7, train, type = "response")
res

#Checking the fit of the model. r squared value. 
pR2(Model7)["McFadden"]

#Checking for variable importance. Higher values, more important to model. 
varImp(Model7)

#Checking for multicollinearity, values above 5 show severe MC
vif(Model7)

#Validating the models accuracy 
ConfusionMatrix <- table(Actual_Value= train$launch, Predicted_Value = res > 0.5)
ConfusionMatrix
#Test model 
ConfusionMatrix <- table(Actual_Value= test1$launch, Predicted_Value = res1 > 0.5)
ConfusionMatrix

#Test model 
ConfusionMatrix <- table(Actual_Value= test2$launch, Predicted_Value = res1 > 0.5)
ConfusionMatrix




#Numeric Dataframes 
CutlerNumeric <- data.frame(Cutler$time_of_day,
                            Cutler$temp,
                            Cutler$wind_speed,
                            Cutler$wind_direction,
                            Cutler$conditions,
                            Cutler$three_hour_without_gust)
CutlerNumeric$Cutler.conditions <- as.factor(CutlerNumeric$Cutler.conditions)
CutlerNumeric$Cutler.three_hour_without_gust <- as.factor(CutlerNumeric$Cutler.three_hour_without_gust)
CutlerNumeric$Cutler.time_of_day <- as.numeric(CutlerNumeric$Cutler.time_of_day)

BarHarborNumeric <- data.frame(KBHB$Time_of_day,
                               KBHB$temp,
                               KBHB$wind_speed,
                               KBHB$wind_direction,
                               KBHB$conditions,
                               KBHB$three_hour_window)
BarHarborNumeric$KBHB.conditions <- as.factor(BarHarborNumeric$KBHB.conditions)
BarHarborNumeric$KBHB.three_hour_window <- as.factor(BarHarborNumeric$KBHB.three_hour_window)

BrunswickNumeric <- data.frame(KBXM$Time_of_day,
                               KBXM$temp,
                               KBXM$wind_speed,
                               KBXM$wind_direction,
                               KBXM$condition_go,
                               KBXM$three_hour_window)
BrunswickNumeric$KBXM.condition_go <- as.factor(BrunswickNumeric$KBXM.condition_go)
BrunswickNumeric$KBXM.three_hour_window <- as.factor(BrunswickNumeric$KBXM.three_hour_window)

#Correcting names to be the same
names(CutlerNumeric)[1] <- "time_of_day"
names(BarHarborNumeric)[1] <- "time_of_day"
names(BrunswickNumeric)[1] <- "time_of_day"

names(CutlerNumeric)[2] <- "temp"
names(BarHarborNumeric)[2] <- "temp"
names(BrunswickNumeric)[2] <- "temp"

names(CutlerNumeric)[3] <- "wind_speed"
names(BarHarborNumeric)[3] <- "wind_speed"
names(BrunswickNumeric)[3] <- "wind_speed"

names(CutlerNumeric)[4] <- "wind_direction"
names(BarHarborNumeric)[4] <- "wind_direction"
names(BrunswickNumeric)[4] <- "wind_direction"

names(CutlerNumeric)[5] <- "conditions"
names(BarHarborNumeric)[5] <- "conditions"
names(BrunswickNumeric)[5] <- "conditions"

names(CutlerNumeric)[6] <- "launch"
names(BarHarborNumeric)[6] <- "launch"
names(BrunswickNumeric)[6] <- "launch"

str(CutlerNumeric)
str(BarHarborNumeric)
str(BrunswickNumeric)


#Attempting to create a SVM Model 
intrain <- createDataPartition(y=CutlerNumeric$launch, p=0.70, list=FALSE)
train <- CutlerNumeric[intrain,]


test1 <- BrunswickNumeric
test2 <- BarHarborNumeric

#checking the dimensions of the model
dim(train);
dim(test1);
dim(test2);


traincontrol<- trainControl(method = "repeatedcv", number = 5, repeats = 3)
#To Fine tune the model walk it through more iterations!

svmModel <- train(launch~., data=train, method = "svmLinear",
                  trControl=traincontrol,
                  preProcess= c("center","scale"),
                  tuneLength = 8)

svmModel

#Test Data set Brunswick
TestPrediction <- predict(svmModel, newdata = test1)
TestPrediction
confusionMatrix(table(TestPrediction, test1$launch))


#Test Data set Bar Harbor
TestPrediction <- predict(svmModel, newdata = test2)
TestPrediction
confusionMatrix(table(TestPrediction, test2$launch))



# ********  Graphing the Data ************#

#Wind direction by compass degrees Graphic 
breaks <- c(0,60,210,270,360)
tags <- c("1-60", "61-210","211-270","271-360")

group_tags <- cut(Cutler$wind_direction,
                  breaks = breaks,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)
group_tags <- na.omit(group_tags)
summary(group_tags)

group_tags2 <- as_tibble(group_tags)
group_tags3 <- as.data.frame(table(group_tags))
rownames(group_tags3) <- c(1:5)
colnames(group_tags3)[2] <- "Frequency"
group_tags3 <- na.omit(group_tags3)

ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill=ifelse(group_tags3$group_tags =="61-210", 'gray', '#38b000'),color="white",alpha=0.7) + 
  stat_count(geom="text", 
             aes(label=sprintf("%.2f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='Wind Direction in Degrees', y='Frequency') +
  theme_minimal() +
  ggtitle("Wind Direction 2010-2021")+
  theme(axis.line.x = element_line(colour = "gray"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title=element_text( hjust=.50, face='bold', size = 15)) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))


# Recode and Reclass Culter "Launch Decision" variable

colnames(Cutler2.1)[31] <- "LaunchDecision"
?colnames
class(Cutler2.1$LaunchDecision)
Cutler2.1$LaunchDecision <- as.factor(Cutler2.1$LaunchDecision)
Cutler2.1 <- Cutler2.1 %>% 
  mutate(LaunchDecision = recode(LaunchDecision, 
                                 `0` = "No Launch", 
                                 `1` = "Launch"))
unique(Cutler2.1$LaunchDecision)



#Scatterplot of Temperature and Wind speed, launch windows in green

ggplot(Cutler2.1, aes(x = wind_speed, y = temp, col=LaunchDecision)) +
  geom_point()
wind_temp_scatter <- ggplot(Cutler2.1, aes(x = temp, y = wind_speed, 
                     col=LaunchDecision))
wind_temp_scatter + geom_point() +
  scale_color_manual(breaks = c("Launch", "No Launch"), 
                     values = c("#38b000", "#bcb8b1")) +
  xlab("Air Temperature (F)") +
  ylab("Wind Speed (kts)") +
  theme_minimal() +
  ggtitle("Wind & Temperature Launch Window")+
  theme(plot.title=element_text( hjust=.75, face='bold', size = 15))


#table of annual launch windows w/ average 3-hour launch windows per year
LW_byYear <- as.data.frame(table(Go_for_Launch$year)) 
LW_byYear
colnames(LW_byYear) <- c("Year", "LaunchWindows") 
LW_byYear <- LW_byYear[-12, ]
mean(LW_byYear$LaunchWindows)
#2169.2

colnames(LW_byYear) <- c("Year", "Launch Windows") 

LWgt <- LW_byYear %>% gt() %>% 
  tab_header(
    title = "3 Hour Launch Windows ('10-'20)") %>% 
  tab_source_note(source_note = "Annual Average  =  2169 ") %>%
  tab_style(
    style = list(cell_text(align = "right"),
                 cell_text(weight = "bold"),
                 cell_text(size = "large"),
                 cell_fill(color = "lightgreen")),
    locations = cells_source_notes()) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title())
LWgt


#What percentage of row(hours) in the dataset contain a launch window
nrow(Go_for_Launch)/nrow(Cutler2.1)
## answer:  24.35%

# Condense things down to the launches
Go_for_Launch <- filter(Cutler2.1, LaunchDecision == "Launch")



#Time of Day launch ability 
AM <- filter(Cutler2.1, morning_evening == "AM")
PM <- filter(Cutler2.1, morning_evening == "PM")

# creating a military time variable 

Go_for_Launch$M_Time <- 
  format(as.POSIXlt(Go_for_Launch$OG_Date_Time,format="%m/%d/%Y %H:%M"),
         "%H")

# Barplot of Launch Windows by Hour of the Day over 5 years

Time_Barplot <- ggplot(Go_for_Launch, 
                       aes(x = M_Time, 
                           col = "LaunchDecision",
                           fill = "LaunchDecision"))
Time_Barplot + geom_bar(show.legend = FALSE) +
  scale_color_manual(breaks = c("Launch"), values = "white") +
  scale_fill_manual(values = c("LaunchDecision" = "lightgreen"))+
  xlab("Hour of the Day on 24 Hour Clock ") + ylab("Count of Launch Windows") +
  theme(axis.line.x = element_line(colour = "gray"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Launch Windows by Hour ('10-'21)") +
  theme(plot.title=element_text( hjust=.55, face='bold', size = 15))



# Maine Industries w/Spaceport DataFrame

one_million = 1000000
maine_df <-data.frame("Industry" = c("Lobster", "Potatoes", 
                                     "Cannabis", "Spaceport"),
                      "Revenue"= c("$485 M", "$167 M", "$112 M", "$55-60M"))
maine_df
gt(maine_df)  %>%  
  tab_header(
    title = "Is space Maine's new big industry?",
    subtitle = "") %>%
  tab_style(
    style = list(cell_text(style = "italic"),
                 cell_text(weight = "bold"),
                 cell_fill(color = "lightgreen")),
    locations = cells_body(
      columns = c(Revenue, Industry),
      rows = 4
    ))


# Temp and Wind Speed Boxplot 
wind_temp_scatter + geom_boxplot() +
  scale_color_manual(breaks = c("Launch", "No Launch"), 
                     values = c("#38b000", "#bcb8b1")) +
  xlab("Air Temperature (F)") +
  ylab("Wind Speed (kts)") +
  theme_minimal() +
  ggtitle("Wind & Temperature Launch Window")+ 
  theme(axis.title = element_text(size=11, face=3)) + 
  theme(plot.title=element_text( hjust=.50, face='bold', size = 15)) +
  scale_y_continuous(breaks=seq(0,35,5)) + ylim(0 ,35) +
  scale_x_continuous(breaks=seq(0,70,10))
