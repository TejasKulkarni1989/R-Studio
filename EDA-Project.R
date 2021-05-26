require(Sleuth3)
require(mosaic)
require(knitr)


cars.df <- read.csv("Projectdata.csv", header=T, na.strings=c("","NA"))
dim(cars.df)  # find the dimension of data frame
head(cars.df)  # show the first six rows
View(cars.df)
summary(cars.df)

# Missing values

colSums(is.na(cars.df))


# Drop column Market Cat

cars.df_new <- cars.df[, -10]

# Drop Missing rows

cars.df_new = na.omit(cars.df_new)
colSums(is.na(cars.df_new))

nums = select_if(cars.df_new,is.numeric)

# number of brands 
Rel.Freq<- cars.df_new %>%
  group_by(`Make`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq


# # transmission type
Rel.Freq<- cars.df_new %>%
  group_by(`Transmission.Type`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq

## Transmission type into 2 cats

cars.df_new$Transmission.Type[cars.df_new$Transmission.Type=='AUTOMATED_MANUAL'] = "AUTOMATIC"

cars.df_new$Transmission.Type[cars.df_new$Transmission.Type=='DIRECT_DRIVE'] = "MANUAL"


# # Driven wheels
Rel.Freq<- cars.df_new %>%
  group_by(`Driven_Wheels`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq

# All wheel drive to four wheel drive
cars.df_new$Driven_Wheels[cars.df_new$Driven_Wheels== 'all wheel drive'] = "four wheel drive"

# # Vehicle Size
Rel.Freq<- cars.df_new %>%
  group_by(`Vehicle.Size`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq

# Compact = midsize
cars.df_new$Vehicle.Size[cars.df_new$Vehicle.Size== 'Compact'] = "Midsize"

# # Vehicle Style

Rel.Freq<- cars.df_new %>%
  group_by(`Vehicle.Style`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq

cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== '2dr Hatchback'] = "Hatchback"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== '4dr Hatchback'] = "Hatchback"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Coupe'] = "Convertible"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Extended Cab Pickup'] = "Cab"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Regular Cab Pickup'] = "Cab"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Crew Cab Pickup'] = "Cab"

cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Passenger Minivan'] = "Van"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Passenger Van'] = "Van"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Cargo Minivan'] = "Van"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Cargo Van'] = "Van"

cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== '4dr SUV'] = "SUV"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== '2dr SUV'] = "SUV"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Convertible SUV'] = "SUV"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Wagon'] = "Sedan"
cars.df_new$Vehicle.Style[cars.df_new$Vehicle.Style== 'Convertible SUV'] = "SUV"



# # Engine fuel type

Rel.Freq<- cars.df_new_log %>%
  group_by(`Engine.Fuel.Type`) %>%
  summarise(Frequency1 = n()) %>%
  arrange(desc(Frequency1)) %>%
  mutate(Relative_Frequency = Frequency1/sum(Frequency1)); Rel.Freq

# All unleaded into one Engine fuel type

cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'regular unleaded'] = "unleaded"
cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'premium unleaded (recommended)'] = "unleaded"
cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'premium unleaded (required)'] = "unleaded"
cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'flex-fuel (premium unleaded required/E85)'] = "unleaded"
cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'flex-fuel (premium unleaded recommended/E85)'] = "unleaded"
cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'flex-fuel (unleaded/E85)'] = "unleaded"

cars.df_new$ Engine.Fuel.Type[cars.df_new$Engine.Fuel.Type== 'natural gas'] = "diesel"
# Convert Cat columns to facotrs

cars.df_new$Engine.Fuel.Type =as.factor(cars.df_new$Engine.Fuel.Type)

cars.df_new$Make = as.factor(cars.df_new$Make)

cars.df_new$Model = as.factor(cars.df_new$Model)


# Convert to factor

cars.df_new$Transmission.Type = as.factor(cars.df_new$Transmission.Type)

cars.df_new$Driven_Wheels = as.factor(cars.df_new$Driven_Wheels)
cars.df_new$Vehicle.Size = as.factor(cars.df_new$Vehicle.Size)
cars.df_new$Vehicle.Style = as.factor(cars.df_new$Vehicle.Style)

dim(cars.df_new)


## install.packages('tidyverse')
require('tidyverse')

table(cars.df_new$Transmission.Type)


# histogram

histogram( ~cars.df_new$MSRP, data=cars.df_new,xlab="MSRP",freq=FALSE,breaks =100)

# density plot
densityplot(~MSRP, data=cars.df_new)

## qq plot
gf_qq(~ MSRP, col="red", data=cars.df_new) %>% 
  gf_qqline(col="blue", lwd=1.2)

## transmission vs city mpg
boxplot(cars.df_new$city.mpg ~ cars.df_new$Transmission.Type, main = "City Mileage vs Transmission",xlab="Transmission",ylab="Mileage",las =1 )

## boxplot highway mpg vs transmission

boxplot(cars.df_new$highway.MPG ~ cars.df_new$Transmission.Type, main = "City Mileage vs Transmission",xlab="Transmission",ylab="Mileage",las =1 )

## box plot MSRP 
boxplot(cars.df_new$MSRP, main = "MSRP",xlab="MSRP",las =1 )

## log transform

cars.df_new_log = transform(cars.df_new, logMSRP=log(MSRP))

## Normalize data

cars.df.norm <- sapply(cars.df_new[,-c(1,2,4,7,8,10,11)], scale)


##

histogram( ~logMSRP, data=cars.df_new_log,xlab="MSRP",freq=FALSE,breaks =100)

##
densityplot(~logMSRP, data=cars.df_new_log)

## box log
boxplot(cars.df_new_log$logMSRP, main = "MSRP",xlab="MSRP",las =1 )

## time series 

myts <- ts(cars.df_new$MSRP, 
           start=c(2011), 
           end=c(2017),  
           frequency=12,    
); myts

plot(myts, 
     main="Time Series Graph: Car Price", 
     ylab="MSRP",
     col="red",
     type="l", #line. Other options: p, o, b, h, s, S
     lwd = 2, # line thickness 
     cex=1.2, cex.lab=1.2, cex.axis=1.2,
     las=1, # rotate the value of y-axis
)


## Run linear regression on Normal Data

##
library(rpart)
library(rpart.plot)
library(caret)
library(forecast)

# randomly generate training and validation sets
cars.df_new$ID <- seq.int(nrow(cars.df_new))

training <- sample(cars.df_new$ID,dim(cars.df_new_log)[1]*0.6)
validation <- sample(setdiff(cars.df_new$ID, training),dim(cars.df_new_log)[1]*0.4)


# run linear regression model
reg <- lm(MSRP~., data=cars.df_new[,-c(1,2,3,7,9,16,cars.df_new$Engine.Fuel.Typenaturalgas)], subset=training,
          na.action=na.exclude)
summary(reg)


pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=cars.df_new[validation,-c(1,2,16)],
                  na.action=na.pass)


accuracy(pred_t, cars.df_new[training,]$MSRP)

# validation
accuracy(pred_v, cars.df_new[validation,]$MSRP)

options(scipen=999)

library(gains)
gain <- gains(cars.df_new[validation,]$MSRP[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# we will compute the gain relative to price
price <- cars.df_new[validation,]$MSRP[!is.na(cars.df_new[validation,]$MSRP)]
plot(c(0,gain$cume.pct.of.total*sum(cars.df_new$MSRP))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(cars.df_new$MSRP))~c(0,dim(cars.df_new[validation,])[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(cars.df_new$MSRP), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

cars.df_new.test1 = cbind(cars.df_new[validation,],Predicted = pred_v,na.rm=TRUE)        

## Run linear regression on Log Data


library(rpart)
library(rpart.plot)
library(caret)
library(forecast)

# randomly generate training and validation sets
cars.df_new_log$ID <- seq.int(nrow(cars.df_new_log))

training_log <- sample(cars.df_new_log$ID,dim(cars.df_new_log)[1]*0.6)
validation_log <- sample(setdiff(cars.df_new_log$ID, training_log),dim(cars.df_new_log)[1]*0.4)


# run linear regression model
reg_log <- lm(logMSRP~., data=cars.df_new_log[,-c(1,2,7,9,15,17)], subset=training_log,
          na.action=na.exclude)
summary(reg_log)


pred_t_log <- predict(reg_log, na.action=na.pass)
pred_v_log <- predict(reg_log, newdata=cars.df_new_log[validation_log,-c(1,2)],
                  na.action=na.pass)

# prediction

accuracy(pred_t_log, cars.df_new_log[training_log,]$logMSRP)

# validation
accuracy(pred_v_log, cars.df_new_log[validation_log,]$logMSRP)

options(scipen=999)

library(gains)
gain <- gains(cars.df_new_log[validation_log,]$logMSRP[!is.na(pred_v_log)], pred_v_log[!is.na(pred_v_log)])

# we will compute the gain relative to price
price <- cars.df_new_log[validation_log,]$logMSRP[!is.na(cars.df_new_log[validation_log,]$logMSRP)]
plt = plot(c(0,gain$cume.pct.of.total*sum(cars.df_new_log$logMSRP))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")


# baseline
bl = lines(c(0,sum(cars.df_new_log$logMSRP))~c(0,dim(cars.df_new_log[validation_log,])[1]), col="gray", lty=2)

# Decile-wise lift chart

barplot(gain$mean.resp/mean(cars.df_new_log$logMSRP), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# i was trying to compare values

cars.df_new.test = cbind(cars.df_new_log[validation_log,],Predicted = exp(pred_v_log),na.rm=TRUE)        

## Kmeans Clustering


set.seed(42)
km <- kmeans(cars.df.norm[,-c(1)], 4)
km$size
km$centers
km$cluster

## wss plot
install.packages("ggplot2")
install.packages("ggfortify")
library(dplyr)
library(stats)
library(ggplot2)
library(ggfortify)
wssplot(cars.df.norm[,-c(1)])

# plot centroids
# plot an empty scatter plot

plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))
colnames(cars.df.norm)
# label x-axes
axis(1, at = c(1:7), labels = colnames(cars.df.norm[,-c(1)]))

for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 3, col = ifelse(i %in% c(1, 3),
                                                       "black", "dark grey"))
# name clusters
text(x =0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:4)))


## Heirachal Clustering 

row.names(cars.df.norm) <- row.names(cars.df.norm) 

# c 
c.norm <- dist(cars.df.norm, method = "euclidean")

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(c.norm, method = "complete")
plot(hc1, hang = -1, ann = FALSE)
plot(hc1, cex = 1)
rect.hclust(hc1, k = 6, border = 2:5)

#d

memb <- cutree(hc1, k = 11)
memb

#e

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc2 <- hclust(c.norm, method = "complete")
plot(hc2, hang = -1, ann = FALSE)
plot(hc2, cex = 1)
rect.hclust(hc2, k = 6, border = 2:5)


#f 
memb <- cutree(hc2, k = 10)
memb









# wss plot function
wssplot <- function(data,nc=15,seed =1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type='b',xlab="Number of clusters",ylab = "Withing Groups sum of squares")
}

