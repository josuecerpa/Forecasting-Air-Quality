rm(list=ls())
require(dplyr)
require(MASS)
library(data.table)
library(lubridate)
library(ggplot2)
library(car)
library(caret)
library(tidyverse)
library(tidyr)

########################################################
### MACHINE LEARNING TOOLS FROM LINE 571 ###############
########################################################

#As taking the data from the API ropenaq took much time, we have stored it 
#in order to avoid again the expensive computational process. 

load("C:/ASIGNATURAS UC3M/FOURTH TERM/ADVANCED REGRESSION/PART 1/8mayo.RData")

#########################################################################
##DATA SET WORLD MET ####################################################
#########################################################################

#worldmet: Functions to import data from more than 30,000 surface 
#meteorological sites around the world managed by the National Oceanic 
#and Atmospheric Administration (NOAA) Integrated Surface
library(worldmet)
getMeta()
predictors <- importNOAA(code="037720-99999",year = 2017,precip=TRUE)

#As predictors we select lat, long, wd, ws, ceil_hgt, visibility,
#air_temp, dew_point, atmos_press, RH and Precip. 
#We take the date as element of joint between the predictors and 
#the response

x=na.omit(dplyr::select(predictors,wd,ws,ceil_hgt,visibility,air_temp,
                 dew_point,atmos_pres,RH,date))

summary(x) #we do not seem to have wrong values

#As we have enough observations, we decide to remove directly the outliers
boxplot(x$air_temp,main="Outliers air temperature")
boxplot(x$dew_point,main="Outliers dew point")
boxplot(x$atmos_pres,main="Outliers atmosphere pressure")
boxplot(x$RH,main="Outliers relative humidity")
boxplot(x$visibility,main="Outliers visibility") #No outliers
boxplot(x$ceil_hgt,main="Outliers ceil_hgt") #No outliers
boxplot(x$wd,main="Outliers wind direction")
boxplot(x$ws,main="Outliers wind speed")

outair=boxplot.stats(x$air_temp)$stats
x=dplyr::filter(x,air_temp>outair[1] & air_temp<outair[5])

outdew=boxplot.stats(x$dew_point)$stats
x=dplyr::filter(x,dew_point>outdew[1] & dew_point<outdew[5])

outatmos=boxplot.stats(x$atmos_pres)$stats
x=dplyr::filter(x,atmos_pres>outatmos[1] & atmos_pres<outatmos[5])

outrh=boxplot.stats(x$RH)$stats
x=dplyr::filter(x,RH>outrh[1] & RH<outrh[5])

outwd=boxplot.stats(x$wd)$stats
x=dplyr::filter(x,wd>outwd[1] & wd<outwd[5])

outws=boxplot.stats(x$ws)$stats
x=dplyr::filter(x,ws>outws[1] & ws<outws[5])

#########################################################################
##DATA SET ROPENAQ ######################################################
#########################################################################

#Air quality data from the API of the OpenAQ, using ropenaq package
#,any location
library(ropenaq)
countries_table <- aq_countries()

#GB:United kingdom / We choose London
cities<- aq_cities(country="GB") 

#From the different locations we choose London Harlington (Row 8)
location=aq_locations(city = "London") 

from=date("2017-01-01")
to=date("2017-12-31")
dates=seq(from,to,1)
dates=as.character(dates)
pollutants=data.frame()
http="https://openaq-data.s3.amazonaws.com/"

for(i in 1:length(dates)){
  file=read.csv(paste0(http,dates[i],".csv"),stringsAsFactors = F)
  file=select(file,location,utc,parameter,value,latitude,longitude)
  file=filter(file,location=="London Harlington")
  file$utc=gsub("T"," ",file$utc)
  file$utc=substr(file$utc,1,19)
  file$utc=as.POSIXct(file$utc,tz="GMT")
  pollutants=rbind(pollutants,file)
}

airquality=pollutants %>% group_by(utc) %>% summarise(value=mean(value))
summary(airquality) #No wrong values
boxplot(airquality$value)
outvalue=boxplot.stats(airquality$value)$stats
airquality=dplyr::filter(airquality,value>outvalue[1] & value<outvalue[5])

#########################################################################
##JOIN BOTH DATA SETS ###################################################
#########################################################################
head(airquality)
head(x)
colnames(airquality)[1]="date"
conc=merge(airquality,x,by=c("date"),all = FALSE)
conc$date=date(conc$date)
conc=aggregate(.~ date, conc, mean)
conc$date=factor(month(conc$date))
colnames(conc)[1]=c("month")
pairs(conc)
conc=dplyr::select(conc,2:ncol(conc),1)

#dew point and air temp negative values, we convert them to kelvin
conc$dew_point=conc$dew_point+273
conc$air_temp=conc$air_temp+273
#########################################################################
##SPLIT AND DESCRIPTIVE ANALISYS ########################################
#########################################################################
# Split data into training and testing sets using the caret package
in_train <- createDataPartition(conc[,1], p = 0.8, list = FALSE)  # 80% for training
training <- conc[in_train,]
testing <- conc[-in_train,]
nrow(training)
nrow(testing)

##########
#Densities
##########
#Pollutants (response)
ggplot(training, aes(value))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("pollutants concentration")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("µg/m³")

#wind direction (wd)
ggplot(training, aes(wd))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Wind direction in degrees º")+
  theme(plot.title = element_text(hjust = 0.5))

#wind speed(ws)
ggplot(training, aes(ws))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Wind speed in m/s")+
  theme(plot.title = element_text(hjust = 0.5))

#height above ground level (ceil_hgt)
ggplot(training, aes(ceil_hgt))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Height above ground level")+
  theme(plot.title = element_text(hjust = 0.5))

#Visibility 
ggplot(training, aes(visibility))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Visibility in metres")+
  theme(plot.title = element_text(hjust = 0.5))

#Relative humidity (RH)
ggplot(training, aes(RH))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Relative humidity (%)")+
  theme(plot.title = element_text(hjust = 0.5))

#Air temperature (air_temp)
ggplot(training, aes(air_temp))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Air temperature in kelvin")+
  theme(plot.title = element_text(hjust = 0.5))

#Dew point temperature (dew_point)
ggplot(training, aes(dew_point))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Dew point temperature in kelvin")+
  theme(plot.title = element_text(hjust = 0.5))

#Atmosphere pressure (atmos_pres)
ggplot(training, aes(atmos_pres))+
  geom_density(color="darkblue",fill="lightblue")+
  ggtitle("Sea level pressure in millibars")+
  theme(plot.title = element_text(hjust = 0.5))

#################################
#response-predictors
#################################
ggplot(training,aes(month,value))+
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("months")

ggplot(training,aes(wd,value))+
  geom_point()+
  ggtitle("Air quality-Wind Direction")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("wind direction (º)")

ggplot(training,aes(ws,value))+
  geom_point()+
  ggtitle("Air quality - Wind speed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("wind speed (m/s)")

ggplot(training,aes(ceil_hgt,value))+
  geom_point()+
  ggtitle("Air quality - ceil_hgt")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Height above ground level")

ggplot(training,aes(visibility,value))+
  geom_point()+
  ggtitle("Air quality - visibility")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Visibility (m)")

ggplot(training,aes(air_temp,value))+
  geom_point()+
  ggtitle("Air quality - Air_temp")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Air temperature (ºC)")

ggplot(training,aes(dew_point,value))+
  geom_point()+
  ggtitle("Air quality - Dew_point")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Dew point temperature (ºC)")

ggplot(training,aes(atmos_pres,value))+
  geom_point()+
  ggtitle("Air quality - Atmos_pres")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Dew point temperature (ºC)")

ggplot(training,aes(RH,value))+
  geom_point()+
  ggtitle("Air quality - RH")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Air quality (µg/m³)")+
  xlab("Relative humidity (%)")

cor_value <- sort(cor(training[,1:9])["value",], decreasing = T)
corr=data.frame(cor_value)
ggplot(corr,aes(x = row.names(corr), y = cor_value)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  scale_x_discrete(limits= row.names(corr)) +
  labs(x = "Predictors", y = "Pollutants concentration", title = "Correlations") + 
  theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust = 1))

################################################################
###################### REGRESSION ##############################
################################################################
pairs(training)

#All the variables
linFit <- lm(value ~., data=training)
summary(linFit)#R^2 0.6182
par(mfrow=c(2,2)) 
plot(linFit, pch=23 ,bg='orange',cex=2) 


#All the variables but including air_temp*dew_point
linFit <- lm(value ~wd+ws+ceil_hgt+visibility+dew_point*air_temp+atmos_pres+RH+month, data=training)
summary(linFit) #R^2 0.6212
par(mfrow=c(2,2))
plot(linFit, pch=23 ,bg='orange',cex=2)

#We remove dew_point but we add the interaction dew_point:air_temp
linFit <- lm(value ~wd+ws+ceil_hgt+visibility+air_temp+air_temp:dew_point+atmos_pres+RH+month, data=training)
summary(linFit)#R^2 0.620
par(mfrow=c(2,2))
plot(linFit, pch=23 ,bg='orange',cex=2)



#We add lag1 of the response (we appreciate a patten in boxplot month-value)
linFit <- lm(value ~wd+ws+ceil_hgt+visibility+atmos_pres+RH+month+air_temp+air_temp:dew_point+lag(value), data=training)
summary(linFit) #R^2 0.715
par(mfrow=c(2,2))
plot(linFit, pch=23 ,bg='orange',cex=2)

training = training %>% dplyr::mutate(lag1value=lag(value)) %>% drop_na()
testing = testing %>% dplyr::mutate(lag1value=lag(value)) %>% drop_na()

#A priori the last model is the best. We define the models:

################################################################
###################### MODEL SELECTION #########################
################################################################

mod0=value ~.
mod1=value ~wd+ws+ceil_hgt+visibility+air_temp+air_temp:dew_point+atmos_pres+RH+month
mod2=value ~wd+ws+ceil_hgt+visibility+atmos_pres+RH+month+air_temp+air_temp:dew_point+lag1value

# Try a model selection tool to simplify the model:
library(leaps)
exhaustive <- regsubsets(mod2, data=training)
summary(exhaustive)
par(mfrow=c(1,1))
plot(summary(exhaustive)$bic, type = 'l') 
#6 or 8 to predict better

### Prediction
### Advanced regression models Caret package

# Each model can be automatically tuned and evaluated 
# In this case, we are goint to use 2 repeats of 10-fold cross validation
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, repeats = 2)

# Finally, create a data frame with all the predictors 
test_results <- data.frame(value = testing$value)
measurements=data.frame()
# Linear regression
mod0lm_tune <- train(mod0, data = training, 
                 method = "lm", 
                 preProc=c('scale', 'center'),
                 trControl = ctrl)
mod0lm_tune
test_results$mod0lm <- predict(mod0lm_tune, testing)
postResample(pred = test_results$mod0lm,  obs = test_results$value)
#RMSE       Rsquared       MAE 
#4.3812695 0.5135213 3.5071343 

qplot(test_results$mod0lm, test_results$value) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
#a bit of bias and variance

mod1lm_tune <- train(mod1, data = training, 
                     method = "lm", 
                     preProc=c('scale', 'center'),
                     trControl = ctrl)
mod1lm_tune
test_results$mod1lm <- predict(mod1lm_tune, testing)
postResample(pred = test_results$mod1lm,  obs = test_results$value)
#RMSE       Rsquared       MAE 
#4.3553447 0.5204763 3.5249867 

qplot(test_results$mod1lm, test_results$value) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

mod2lm_tune <- train(mod2, data = training, 
                     method = "lm", 
                     preProc=c('scale', 'center'),
                     trControl = ctrl)
mod2lm_tune
test_results$mod2lm <- predict(mod2lm_tune, testing)
postResample(pred = test_results$mod2lm,  obs = test_results$value)
#RMSE  Rsquared       MAE 
#4.3381054 0.5195958 3.4743279
qplot(test_results$mod2lm, test_results$value) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

#With mod2 we get the lower MAE and MRSE, and better Rsquared, thus that is the model
#we will take
lm=postResample(pred = test_results$mod2lm,  obs = test_results$value)
measurements=rbind(measurements,lm)

for_tune <- train(mod2, data = training, 
                  method = "leapForward", #instead of original package 
                  preProc=c('scale', 'center'),
                  tuneGrid = expand.grid(nvmax = 2:21),
                  trControl = ctrl)
for_tune
plot(for_tune)
# which variables are selected?
coef(for_tune$finalModel, for_tune$bestTune$nvmax)
test_results$frw <- predict(for_tune, testing)
frwp=postResample(pred = test_results$frw,  obs = test_results$value)
measurements=rbind(measurements,frwp)

qplot(test_results$frw, test_results$value) + 
  labs(title="Forward Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40))+
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
#a bit of bias and variance, very similar to lm

# Backward regression
back_tune <- train(mod2, data = training, 
                   method = "leapBackward", #backward regression in pack. leap
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 2:21),
                   trControl = ctrl)
back_tune
plot(back_tune)
# which variables are selected? 
coef(back_tune$finalModel, back_tune$bestTune$nvmax)
test_results$bw <- predict(back_tune, testing)
bwp=postResample(pred = test_results$bw,  obs = test_results$value)
measurements=rbind(measurements,bwp)

qplot(test_results$bw, test_results$value) + 
  labs(title="Backward Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40))+
  geom_abline(intercept = 0, slope = 1, colour = "blue")+
  theme_bw()
# #a bit of bias and variance, very similar to lm

# Stepwise regression
step_tune <- train(mod2, data = training, 
                   method = "leapSeq", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 2:21),
                   trControl = ctrl)
plot(step_tune)

# which variables are selected?(wd,ws,ceil_hgt,visibility,air_temp)
coef(step_tune$finalModel, step_tune$bestTune$nvmax)
test_results$seq <- predict(step_tune, testing)
seqp=postResample(pred = test_results$seq,  obs = test_results$value)
measurements=rbind(measurements,seqp)

qplot(test_results$seq, test_results$value) + 
  labs(title="Stepwise regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue")+
  theme_bw()
# #a bit of bias and variance, very similar to lm

#Ridge regression

ridge_grid <- expand.grid(lambda = seq(0, .1, length = 20))
ridge_tune <- train(mod2, data = training,
                    method='ridge',
                    preProc=c('scale','center'),
                    tuneGrid = ridge_grid,
                    trControl=ctrl)
plot(ridge_tune)
ridge_tune$bestTune
test_results$ridge <- predict(ridge_tune, testing)
ridgep=postResample(pred = test_results$ridge,  obs = test_results$value)
measurements=rbind(measurements,ridgep)

qplot(test_results$ridge, test_results$value) + 
  labs(title="Ridge regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue")+
  theme_bw()
# #a bit of bias and variance, very similar to lm

##Lasso

lasso_grid <- expand.grid(fraction = seq(.01, 1, length = 20))
lasso_tune <- train(mod2, data = training,
                    method='lasso',
                    preProc=c('scale','center'),
                    tuneGrid = lasso_grid,
                    trControl=ctrl)
#rho for lasso around 0.2
plot(lasso_tune)
lasso_tune$bestTune
test_results$lasso <- predict(lasso_tune, testing)
lassop=postResample(pred = test_results$lasso,  obs = test_results$value)
measurements=rbind(measurements,lassop)

qplot(test_results$lasso, test_results$value) + 
  labs(title="Lasso regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue")+
  theme_bw()
# #a bit of bias and variance, very similar to lm

# Elastic net
modelLookup('glmnet')
elastic_grid = expand.grid(alpha = seq(0, .2, 0.01), lambda = seq(0, .1, 0.01)) 
glmnet_tune <- train(mod2, data = training,
                     method='glmnet',
                     preProc=c('scale','center'),
                     tuneGrid = elastic_grid,
                     trControl=ctrl)
plot(glmnet_tune)
glmnet_tune$bestTune
test_results$glmnet <- predict(glmnet_tune, testing)
elanetp=postResample(pred = test_results$glmnet,  obs = test_results$value)
measurements=rbind(measurements,elanetp)
# PCR

pcr_tune <- train(mod2, data = training,
                  method='pcr',
                  preProc=c('scale','center'),
                  tuneGrid = expand.grid(ncomp = 2:10),
                  trControl=ctrl)
plot(pcr_tune)
test_results$pcr <- predict(pcr_tune, testing)
pcrp=postResample(pred = test_results$pcr,  obs = test_results$value)
measurements=rbind(measurements,pcrp)

# PLS
pls_tune <- train(mod2, data = training,
                  method='pls',
                  preProc=c('scale','center'),
                  tuneGrid = expand.grid(ncomp = 2:10),
                  trControl=ctrl)
plot(pls_tune)
test_results$pls <- predict(pls_tune, testing)
plsp=postResample(pred = test_results$pls,  obs = test_results$value)
measurements=rbind(measurements,plsp)

#Robust regression
linfit.huber <- rlm(mod2, data=training, psi=psi.huber)
summary(linfit.huber)
test_results$robust <- predict(linfit.huber, testing)
robustp=postResample(pred = test_results$robust,  obs = test_results$value)
measurements=rbind(measurements,robustp)

colnames(measurements)=c("MSRE","Rsquared","MAE")
rownames(measurements)=c("lm","frw","bw","stw","ridge","lasso","elasnet","PCR","PLS","robust")
measurements
which.min(measurements[,1])
which.min(measurements[,3])
which.max(measurements[,2])

#Backward is the best model lowest MSRE y best Rsquare

mean(training$value)
# This is equivalent to
benchFit <- lm(value ~ 1, data=training)
predictions <- predict(benchFit, newdata=testing)
cor(testing$value, predictions)^2
RMSE <- sqrt(mean((predictions - testing$value)^2))
RMSE #benchmark

################################################################
###################### PREDICTION INTERVALS ####################
################################################################

# Final predictions:
yhat = test_results$bw
head(yhat) # show the prediction for 6 home prices
hist(yhat, col="lightblue")
# take care: asymmetric distribution

y = test_results$value
error = y-yhat
hist(error, col="lightblue")

noise = error[1:30]
sd.noise = sd(noise)

# If noise is a normal variable, then a 90%-CI can be computed as
lwr = yhat[31:length(yhat)] - 1.65*sd.noise
upr = yhat[31:length(yhat)] + 1.65*sd.noise

predictions = data.frame(real=y[31:length(y)], fit=yhat[31:length(yhat)], lwr=lwr, upr=upr)
predictions = predictions %>% mutate(out=factor(if_else(real<lwr | real>upr,1,0)))
mean(predictions$out==1)

ggplot(predictions, aes(x=fit, y=real))+theme_minimal()+
  geom_point(aes(color=out)) + theme(legend.position="none") +
  lims(x = c(10.5,30), y = c(-20, 40)) +
  geom_ribbon(data=predictions,aes(ymin=lwr,ymax=upr),alpha=0.3) +
  labs(title = "Prediction intervals 90%-CI", x = "prediction",y="Real value")

################################################################
###################### MACHINE LEARNING TOOLS ##################
################################################################

measurements2=data.frame()

#knn 
# Automatic optimization of hyper-parameter k with Caret:
modelLookup('kknn')
knn_tune <- train(mod2, data = training,
                  method = "kknn",   
                  preProc=c('scale','center'),
                  tuneLength = 10,
                  trControl = ctrl)
plot(knn_tune)
test_results$knn <- predict(knn_tune, testing)
neighbours=postResample(pred = test_results$knn,  obs = test_results$value)
measurements2=rbind(measurements2,neighbours)

#SVM 
modelLookup('svmRadial')
svm_tune <- train(mod2, data = training,
                  method = "svmRadial",
                  tuneGrid = data.frame(.C = seq(.1, 2, 0.1), .sigma = seq(0.01, 0.05, 0.01)), 
                  preProc=c('scale','center'),
                  trControl = ctrl)
plot(svm_tune)
test_results$svmR <- predict(svm_tune, testing)
svm=postResample(pred = test_results$svmR,  obs = test_results$value)
measurements2=rbind(measurements2,svm)

#Decision Trees

library(rpart)
# Trees cannot handle interaction terms directly (dew_point:air_temp)
training$dewair = training$air_temp*training$dew_point
testing$dewair = testing$air_temp*testing$dew_point

ModelTree = value ~ wd+ws+ceil_hgt+visibility+atmos_pres+RH+month+dewair+lag1value
rpart.fit <- rpart(ModelTree, data = training)
summary(rpart.fit)
# For each node in the tree, the number of examples reaching the decision point is listed
plot(rpart.fit,margin=.25)
text(rpart.fit, use.n=T)
rpart.fit

# Visualizing decision trees
library(rpart.plot)

rpart.plot(rpart.fit, digits = 3, fallen.leaves = TRUE,
           type = 3, extra=101)

test_results$tree <- predict(rpart.fit, testing)
dt=postResample(pred = test_results$tree,  obs=test_results$value)
measurements2=rbind(measurements2,dt)

#Random forest

modelLookup('rf')
# easy to tune
rf_tune <- train(mod2, data = training,
                 method = "rf",
                 preProc=c('scale','center'),
                 trControl = ctrl,
                 ntree = 250,
                 tuneGrid = expand.grid(mtry = c(10,20,30)), 
                 verbose = FALSE)
plot(rf_tune)
test_results$rf <- predict(rf_tune, testing)
rf=postResample(pred = test_results$rf,  obs = test_results$value)
measurements2=rbind(measurements2,dt)

# Gradient Boosting
modelLookup('gbm')
# not so easy to tune...
gbmGrid<-expand.grid(n.trees=seq(100,400,by=100),interaction.depth=seq(1,4,by=1),shrinkage=c(.001,.01,.1), n.minobsinnode=10)
gbm_tune <- train(mod2, data = training,
                  method = "gbm", # Generalized Boosted Regression Models
                  preProc=c('scale','center'),
                  tuneGrid = gbmGrid,
                  trControl = ctrl, 
                  verbose = FALSE)
plot(gbm_tune)
test_results$gbm <- predict(gbm_tune, testing)
gb=postResample(pred = test_results$gbm,  obs = test_results$value)
measurements2=rbind(measurements2,gb)

qplot(test_results$gbm, test_results$value) + 
  labs(title="Gradient Boosting Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(-20,40), y = c(-20, 40))+
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

#Combining models

# Let's look again at the performance for all models:
sort(apply(test_results[-1], 2, function(x) cor(x,test_results$value)^2))
sort(apply(test_results[-1], 2, function(x) mean(abs(x - test_results$value))))

library(GGally)
test_results %>%
  dplyr::select(-value) %>%
  ggcorr(palette = "RdBu", label = TRUE) + labs(title = "Correlations between different models")

test_results$comb = (test_results$svmR + test_results$knn + test_results$robust)/3
combination=postResample(pred = test_results$comb,  obs = test_results$value)
measurements2=rbind(measurements2,combination)

colnames(measurements2)=c("MSRE","Rsquared","MAE")
rownames(measurements2)=c("knn","svm","dtree","rforest","gradboost","combination")

which.min(measurements2[,1])
which.min(measurements2[,3])
which.max(measurements2[,2])

################################################################
###################### PREDICTION INTERVALS 2 ##################
################################################################

# Final predictions:
yhat = test_results$comb
head(yhat) # show the prediction for 6 home prices
hist(yhat, col="lightblue")
# take care: asymmetric distribution

y = test_results$value
error = y-yhat
hist(error, col="lightblue")

noise = error[1:30]
sd.noise = sd(noise)

# If noise is a normal variable, then a 90%-CI can be computed as
lwr = yhat[31:length(yhat)] - 1.65*sd.noise
upr = yhat[31:length(yhat)] + 1.65*sd.noise

predictions = data.frame(real=y[31:length(y)], fit=yhat[31:length(yhat)], lwr=lwr, upr=upr)
predictions = predictions %>% mutate(out=factor(if_else(real<lwr | real>upr,1,0)))
mean(predictions$out==1)

ggplot(predictions, aes(x=fit, y=real))+theme_minimal()+
  geom_point(aes(color=out)) + theme(legend.position="none") +
  lims(x = c(10.5,30), y = c(-20, 40)) +
  geom_ribbon(data=predictions,aes(ymin=lwr,ymax=upr),alpha=0.3) +
  labs(title = "Prediction intervals 90%-CI", x = "prediction",y="Real value")

