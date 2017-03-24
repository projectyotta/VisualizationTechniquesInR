#remove everything from global environment 
rm(list=ls())

# set working directory 
setwd("C:/Users/SOURAV/Desktop/stat")

#read data using read.csv 
data<-read.csv("C:/Users/SOURAV/Desktop/stat/data.csv")

#1.a. 

# this part of the question has been finished using tableau and excel. 


#1.b. 

# open csv file and see what all coulmns have numeric data and store in an array
integercoulmns <- c(5,6,9,10,12,13,16,17,18)
#use the data of those coulmns 
numericdata<-data[,integercoulmns]
#correlation
x <- cor(numericdata,data$FARE)
#distance is the best predictor, obtained from correlation table 
x
#plot the correlation to indicate best predictor graphically
plot(x,main=" best single predictor of FARE")
#scatter plots 
plot(numericdata,pch="*",bg='black',col='black')


#1.c 

#computing mean value of fare according to each category 
vacation<- aggregate(FARE~VACATION, data=data, FUN=mean)
sw<- aggregate(FARE~SW, data=data, FUN=mean)
slot<- aggregate(FARE~SLOT, data=data, FUN=mean)
gate<- aggregate(FARE~GATE, data=data, FUN=mean)
# mean value of fare according to each category 
# for vacation  
vacation 
# for southwest 
sw
#for slot
slot
# for gate
gate

# mean difference 
# for vacation
abs(diff(vacation[,2])) 
# for southwest 
abs(diff(sw[,2]))
#for slot
abs(diff(slot[,2]))
#for gate 
abs(diff(gate[,2]))


# 1.d 

#to get the number of rows 
dim(data[2])
set.seed(12345)
# 60 % of 638 is 382 
trainingindex <- sample(638, 382, replace=FALSE)
var <- c(8,16,18)
#data model consisting of the variables 
datamodel <- data[,var]
# training set data 
training = datamodel[trainingindex,]
# validation set data 
validation = datamodel[-trainingindex,]
#find out the dimensions of the training and the validation set data 
dim(training)
dim(validation)
# build the linear model 
trainingfit <- lm(FARE~SW+DISTANCE, data= training)
summary(trainingfit)
BIC(trainingfit)
#plot for the predicted and residual values 
predi <- fitted(trainingfit)
resid<- residuals(trainingfit)
plot(predi,resid)
abline(h=0,v=175)
#coefficients are parameter estimates 


# 1.e. 

# generate for backware regression 
back<-step(lm(FARE~COUPON+NEW+VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data = data[trainingindex,]),direction = 'backward')
#Coefficient values for the above model 
coefficients(back)
#BIC value for the above model 
BIC(back)
#Summary values for the above model 
summary(back)

# 1.f 
# findx the AIC values for both the models 
AIC(trainingfit)	
AIC(back)

#1.g. 

gdata=data.frame(COUPON=1.202,NEW=3,VACATION="No",
SW="No",HI=4442.41,S_INCOME=28760,E_INCOME=27664,
S_POP=4557004,E_POP=3195503,SLOT="Free",GATE="Free",PAX=12782,DISTANCE=1976)
pred_val<-predict(back,gdata,se.fit = TRUE,terms=NULL,scale=NULL)
#average fare of model
pred_val$fit

gdata_SW_YES=data.frame(COUPON=1.202,NEW=3,VACATION="No",
SW="Yes",HI=4442.41,S_INCOME=28760,E_INCOME=27664,
S_POP=4557004,E_POP=3195503,SLOT="Free",GATE="Free",PAX=12782,DISTANCE=1976)
pred_val_SW_YES<-predict(back,gdata_SW_YES,se.fit = TRUE,terms=NULL,scale=NULL)
#average fare of model if route is covered by southwest airlines 
pred_val_SW_YES$fit
#reduction in fare when southwest operates  
y <- pred_val$fit - pred_val_SW_YES$fit 
y
# to get the regression coefficient of sw 
coefficients(back)

# part i 
# exclude Coupon  , Vacation , Hi , Distance , Pax 
trainingfit_i=lm(FARE~S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+NEW+SW,data = data[trainingindex,])
summary(trainingfit_i)
summary(back)
AIC(trainingfit_i)
AIC(back)
BIC(trainingfit_i)
BIC(back)
