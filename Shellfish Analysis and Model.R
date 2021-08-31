
### Initial Analysis EDA ###

# Load libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggthemes)
library(patchwork) 
library(hrbrthemes)


# Decision Tree libraries
library(rpart)
library(rpart.plot)
library(caret)

data = read.csv(file.choose())

# Check Structure
str(data)
# Fix date to R date
data$Date = as.Date(data$Date, format = "%m/%d/%Y")

## EDA on mean temperatures and scoring by month ##

### Group Averages by month ###
ScorebyMonth = data %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(Score))

TempbyMonth = data %>%
  filter(!is.na(TempFahrenheit)) %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(TempFahrenheit))

### Convert to Time Series ###
score.ts = ts(ScorebyMonth$avg, start = c(2013,1), freq = 12)
temp.ts = ts(TempbyMonth$avg, start = c(2013,1), freq = 12)

### Explore with a few base plots ###
# check line plot (base r)
layout(1:2)
plot(score.ts, ylab = "Number of Fecal Coliform/100ml Sea Water")
plot(temp.ts, ylab = "Temperature in Fahrenheit")


# check distbution plot (base r)
boxplot(score.ts ~ cycle(score.ts),
        ylab = "Number of Fecal Coliform/100ml Sea Water",
        xlab = "Fecal Score Time Series by Month")

boxplot(temp.ts ~ cycle(temp.ts),
        ylab = "Temperature in Fahrenheit",
        xlab = "Temperature Time Series by Month")


## Intersection ##
layout(1:1)
plot(as.vector(score.ts), as.vector(temp.ts),
     xlab = "Average Number of Fecal Coliform/100ml Sea Water",
     ylab = "Average Temperature in Fahrenheit",
     ylim = c(30,67))

abline(v = mean(score.ts))
abline(h = mean(temp.ts))

abline(reg = lm(temp.ts ~ score.ts))

# Check correlation of temp/scores
r = cor(score.ts,temp.ts)
fit = lm(score.ts ~ temp.ts)
R2 = summary(fit)$r.squared
R2

##### This value determined via decision tree analysis below ######
## Median values salinity ##
d1 = filter(data, data$Salinity...24.5 == "Yes")
d2 = filter(data, data$Salinity...24.5 == "No")
median(d1$Score)
median(d2$Score)

## median values with/without precip ##
d3 = filter(data, data$Adversity == "P")
d4 = filter(data, data$Adversity != "P")

mean(d3$Score)
median(d3$Score)
median(d4$Score)
mean(d4$Score)



##### Appendix plots #####
layout(1:1)
mar = c(5.1, 4.1, 4.1, 2.1) 

## 3.01 ##

# Filter only investgatory and random strategy
sampleStrategy = filter(data, Strategy == "Investigatory" | Strategy == "Random")

# Build Plot 3.01
fig.01 <- ggplot(data=sampleStrategy, aes(x=Score, group=Strategy, fill=Strategy)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlim(1.5,35) +
  xlab("Number of Fecal Coliform/100ml Wea Water Score") +
  ylab("Density") + 
  theme_ipsum()
fig.01

## 3.02 ##
fig.02 = ggplot(data, aes(x=Score)) + 
  geom_histogram() +
  theme_tufte() +
  ggtitle("Histogram of Fecal Contamination Scores") +
  ylab("Count of Score") +
  xlab("Number of Fecal Coliform/100ml Wea Water Score") 
fig.02



## 3.03 ##
Score = score.ts
Temperature = temp.ts
plot(cbind(Score, Temperature),
     main = "Time Series of Average Score and Temperature")


## 3.04 ##
fig.04 = ggplot(data, aes(x=Score, y=TempFahrenheit)) +
  geom_point() +
  theme_minimal() +
  ylab("Temperature in Fahrenheit") + 
  xlab("Number of Fecal Coliform/100ml Sea Water Score") + 
  geom_smooth(method='lm', formula= y~x)
fig.04

## PowerPoint ##
fig.05 = ggplot(data, aes(x=Score, y=Salinity_PartsPerThousand)) +
  geom_point() +
  theme_minimal() +
  ylab("Salinity Parts Per Thousand") + 
  xlab("Number of Fecal Coliform/100ml Sea Water Score") + 
  geom_smooth(method='lm', formula= y~x)
fig.05



##########################
### Deision Tree Model ###
##########################

# Remove N/A values in data for lat/long.
dataTree = data %>%
  filter(!is.na(Lat))

# From eda of the data, need to remove commas, spaces, &, and slashes from adversity
dataTree$Adversity = gsub(",","",as.character(dataTree$Adversity))
dataTree$Adversity = gsub(" ","",as.character(dataTree$Adversity))
dataTree$Adversity = gsub("/","",as.character(dataTree$Adversity))
dataTree$Adversity = gsub("&","",as.character(dataTree$Adversity))

# Overall model using all potential data (Removed items like score, Salinity >24.5 indicator etc.)
Tree = rpart(Binary ~ GrowArea + Date + CATEGORY + Samp_Meth + Class + Strategy + Tide + TempFahrenheit +
               Adversity + Salinity_PartsPerThousand + WindDirection + Method + Long + Lat,
             data = dataTree,
             method = "class")

# Plot the decision tree:
# Used 4 digits for accuracy if lat/long found to be valuable within tree analysis.
rpart.plot(Tree, nn=TRUE, digits = 4)
# Salinity level followed by adversity documented during testing are only splits.

# Adversity as followed:
# 9 & R are Unknown variables within the key provided by the state.
# H = Habitation: Seasonal habitation of homes
# N = Nonpoint: Flowing streams, stormwater pipes, or overland runoff
# P = Precipitation: Rain or mixed precipitation anytime within past 2 days (i.e. thunderstorms, rainfall more than a drizzle)
# W = Wildlife: Waterfowl (10 or more), domestic or wild animals (i.e. at the station or in close enough proximity to have a possible impact)

#### Interpretation ####
# Node 1: 77.65% of the data has a low fecal score (binary classifer was 3rd quartile score)  (Conversely 22.35% are high)
# Node 2: 12.25% of the data < 24.5 salinity but 57.69% of those scores are low (Conversely, 42.24% are high) 
# Node 3: 87.75% of the data has salinity > 24.5 & 80.43% of the time, the score is low (modest improvement of the 77.65% overall).
# Node 4: Adversities listed are present and salinity < 24.5 is 2.65% of the data with 40.83% having a high score.
# Node 5: Adversities listed are not presents and salinity < 24.5 is 9.59% of the data, with 62.35% having a low score.

###### Split data into test/train set and test for accuracy #######

set.seed(1123)
train <- sample(1:nrow(dataTree),size = ceiling(0.80*nrow(dataTree)),replace = FALSE)

# training set
tree_train <- dataTree[train,]
# test set
tree_test <- dataTree[-train,]

# Rebuild tree with training set
tree <- rpart(Binary ~ GrowArea + Date + CATEGORY + Samp_Meth + Class + Strategy + Tide + TempFahrenheit +
              Adversity + Salinity_PartsPerThousand + WindDirection + Method + Long + Lat,
              data = tree_train,
              method = "class")


# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
# Introduces temperature and grow area to the splits.  


# Prune the tree with CP optimization
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree <- prune(tree, cp=cp.optim)

#Test the model
pred <- predict(object=tree,tree_test[-20],type="class")

#Calculating accuracy
accuracy <- table(tree_test$Binary,pred) 
confusionMatrix(accuracy) 

# Left column label is actual values
# Top column label is predicted values

# To sum totals
table(tree_test$Binary)
# In the testing model, 2,499 scores are high and 8,563 are low.

# Interpretation #
# Overall model classification accuracy is a misleading 78.22%
  # The model perfoms very well at predicting low values but not as well on high values
  # Predicted high when high 257 times but miscategorized as low 2242 times - 10.28% accuracy
  # Predicted low when low 8,396 times and miscategorized as high 167 times - 98% accuracy
# The sensitivity of the model of 60.613% is based on a prediction of 257 high values out of 424 as correct.
  # This means that it predicts yes when it is in fact yes 60% of the time.

# Model has a very high false negative rate of 89.72%.  In other words, the model would predict
# an area as low when it is in fact high, 89.72% of the time.

# When the model predicts a site as low, is's correct 78.925% of the time.  With that, you could theoretically
# target sites the model predicts as high and 60% of the sites you test will be high.  The same tests at random
# would only find high scores 22.36% of the time.  In other words, by using the model you can make a small number
# of targeted inspections 3x more effective than random testing.  This would be specific to grow area,
# water temperatures and expected adversity rates when salinity is low (after precipitation).


############################
### Kmeans Cluster Model ###
############################

# See what we are working with for numerics.
head(data)

# select numerics identified in DT and later convert adversity to binary 1/0 value if rain present.
kclusterData = data %>%
  select(TempFahrenheit, Salinity_PartsPerThousand, Score,Adversity)

# create true/false column for when rain is present as a binary value
kclusterData$AdversityRain = grepl("P", kclusterData$Adversity)
# Make true/false numeric (this works for kmeans)
kclusterData$AdversityRain = ifelse(kclusterData$AdversityRain == "TRUE",1,0)

# rebuild data
kclusterData = kclusterData %>%
  select(Score, Salinity_PartsPerThousand, TempFahrenheit, AdversityRain)

summary(kclusterData)

# remove n/a's.
kclusterData = kclusterData %>%
  filter(!is.na(TempFahrenheit), !is.na(Salinity_PartsPerThousand))

# After trying several clusters, settled in on 4.
clusterK = kmeans(kclusterData, 4)
options(digits=2)
finalCluster = data.frame(clusterK$size,clusterK$centers)
finalCluster %>% arrange(desc(Score))

# A higher rain adversity score indicates that there are more scenarios where rain occurred within the cluster.








