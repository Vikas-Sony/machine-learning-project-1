setwd("C:/Users/Vikas/Desktop/Assignments/Project-1")
getwd()
data = read.csv("day.csv")
summary(data)
data
head(data)
# now , we will remove rows that are not essential
data2 = subset(data, select = -c(instant, casual, registered, temp))
head(data2)
data3 = subset(data2, select = -c(dteday))
head(data3)
#Now, we have our selected features which are scaled already.So, we will jump into EDA now
summary(data3)
str(data)
# we can see that some variables need to be changed to categorical
data3$season = as.factor(data3$season)
data3$yr = as.factor(data3$yr)
data3$mnth = as.factor(data3$mnth)
data3$holiday = as.factor(data3$holiday)
data3$weekday = as.factor(data3$weekday)
data3$workingday = as.factor(data3$workingday)
data3$weathersit = as.factor(data3$weathersit)
str(data3)
library(ggplot2)
miss_value = data.frame(apply(data3,2, function(x){sum(is.na(x))}))
boxplot(data3$atemp, main = "atemp", boxwex = 0.2)
boxplot(data3$hum, main = "hum", boxwex = 0.2)                        
boxplot(data3$windspeed, main = "wspeed", boxwex = 0.2)
# SO , as given my the graphs . only windspeed and hum has some outliers.
x <- data3$hum
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data3$hum <- x
boxplot(data3$hum, main = "hum", boxwex = 0.2)
x <- data3$windspeed
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data3$windspeed <- x
boxplot(data3$windspeed, main = "wspeed", boxwex = 0.2)
#So, as we can see outliers for both the variables are removed
#Now, we can try and figure out some trends in data
ggplot(data3,aes(season,cnt)) + 
  geom_boxplot(aes(color=season),alpha=0.2) + 
  theme_bw()
# as we can see , count is more in winter which is a bit unintuitive.
ggplot(data3,aes(weekday,cnt)) + 
  geom_boxplot(aes(color=weekday),alpha=0.2) + 
  theme_bw()
#boxplots from weekday
ggplot(data3,aes(workingday,cnt)) + 
  geom_boxplot(aes(color=workingday),alpha=0.2) + 
  theme_bw()
#boxplots from mnth
ggplot(data3,aes(mnth,cnt)) + 
  geom_boxplot(aes(color=mnth),alpha=0.2) + 
  theme_bw()
ggplot(data3,aes(atemp,cnt)) + 
  geom_point(aes(color=atemp),alpha=0.2) + theme_bw()
# correlation with temperature is quite linear. SO, we can try a linear regression model first.
ggplot(data3,aes(hum,cnt)) + 
  geom_point(aes(color=hum),alpha=0.2) + theme_bw()
# correlation with humidity seems to be a bit low.BUt, still it is observed that count is less either in high or low hum.
ggplot(data3,aes(windspeed,cnt)) + 
  geom_point(aes(color=windspeed),alpha=0.2) + theme_bw()
#correlation is a bit low here as well.But, count is more when average windspeed which is kind of obvious
cor(data3)
str(data3)
# Now, as we have some intuition about data, we can start to train a model.
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data3), size = floor(.75*nrow(data)), replace = F)
train <- data3[sample, ]
test  <- data3[-sample, ]
# Now, as our data is split into two parts as train and test , we can start training.
str(train)
install.packages("randomForest")
library('randomForest')
install.packages('e1071')
library('e1071')
#So, we are going to use three most common ML algorithms. All libraries are successfully installed.
str(train)
str(test)
regressor = lm(formula = cnt ~ . , data = train)
regressor = step(regressor)
y_pred = predict(regressor, test)

results <- data.frame(yr = test$yr,mnth = test$mnth, weekday = test$weekday ,count = y_pred)
write.csv(results, file = 'BikeRenting_LR.csv', row.names = FALSE, quote=FALSE)
# NOw, trying support vector regressor
regressor = svm(formula = cnt ~ .,
                data = train,
                type = 'eps-regression')
y_pred2 = predict(regressor, test)
results2 <- data.frame(yr = test$yr,mnth = test$mnth, weekday = test$weekday ,count = y_pred2)
write.csv(results2, file = 'BikeRenting_SVR.csv', row.names = FALSE, quote = FALSE)
#NOw, trying for random forest
regressor3 = randomForest(x = train[,-which(names(train)=="cnt")],
                          y = train$cnt)
y_pred3 = predict(regressor3, test)
results3 <- data.frame(yr = test$yr, mnth = test$mnth, weekday = test$weekday,count = y_pred)
write.csv(results3, file = 'BikeRenting_RF.csv', row.names = FALSE, quote = FALSE)
# Error metrics calculations

install.packages('Metrics')
library('Metrics')
MAE = mean(abs(y_pred- test$cnt))
MAE2 = mean(abs(y_pred2- test$cnt))
MAE3 = mean(abs(y_pred3 - test$cnt))
# SO, as we can see random forest worked the best.

