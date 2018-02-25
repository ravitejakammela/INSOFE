##### DATA HACK ROUND 3

## Team name   : Cyber Sharks
## Team members 1) Punit Agrawal
##              2) Raviteja Kammela




## 1. Setting the working directory and clearing the R environment


rm(list=ls(all=T))
setwd("C:/Users/Punit/Desktop")


## 2. Loading the required libraries 


library(RColorBrewer)
library(rattle)
library(ipred)
library(ROSE)
library(ada)
library(rpart.plot)
library(rpart)
library(randomForest)
library(C50)
library(factoextra)
library(xgboost)
library(glmnet)
library(mice)
library(dplyr)
library(ROCR)
library(DMwR)
library(car)
library(MASS)
library(vegan)
library(dummies)
library(infotheo)
library(caTools)
library(caret)
library(e1071)
library(corrplot)
library(ggplot2)


## 3. Reading the data in R


housing= read.csv("housing.csv", header=T, sep=",")
View(housing)


## 4. Data understanding, exploration


dim(housing)
# 477833 x 10

sum(is.na(housing))
# no missing values in the dataset 

str(housing)


table(housing$town)
#26 towns in all

length(unique(housing$town))

class(housing$town)

housing$month= as.character(housing$month)
housing$year_of_sale= lapply(housing$month, FUN= function(x){substring(x,1,4)})
housing$year_of_sale= as.numeric(housing$year_of_sale)
housing$lease_remaining= 98-(housing$year_of_sale-housing$lease_commence_date)
str(housing)
housing$month_of_sale= lapply(housing$month, FUN= function(x){substring(x,6,7)})
housing$month_of_sale= as.numeric(housing$month_of_sale)
# Extracting year of sale and month of sale from the month column

check= function(x){
  if(x==1|x==2|x==3){
    x=1
  }else{
    if(x==4|x==5|x==6){
      x=2
    }else{
      if(x==7|x==8|x==9){
        x=3
      }else{
        x=4
      }
    }
  }
}

check2= function(x){
  if(x==1|x==2|x==3){
    x=1
  }else{
    if(x==4|x==5|x==6){
      x=1
    }else{
      if(x==7|x==8|x==9){
        x=2
      }else{
        x=2
      }
    }
  }
}


housing$quarter= lapply(housing$month_of_sale, check)
housing$half_year= lapply(housing$month_of_sale,check2)
housing$quarter= as.numeric(housing$quarter)
housing$half_year= as.numeric(housing$half_year)

housing= housing[,-1]

## 1. Animated Plot of year wise change in prices across various flats
install.packages("cowplot")
install.packages("gganimate")
install.packages("gapminder")
library(gganimate)
library(gapminder)
devtools::install_github("dgrtwo/gganimate")
install.packages("magick")
library(magick)
install.packages("installr")
library(installr)
install.ImageMagick()

theme_set(theme_bw())
g <- ggplot(housing, aes(floor_area_sqm, resale_price,  frame = year_of_sale)) +
  geom_point() +
  
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)

## 2. Flipped axis, town wise price average, standardized
new= tapply(housing$resale_price, housing$town, mean)
new=data.frame(new)
View(new)
new$town= row.names(new)
new$new= scale(new$new, center= T, scale=T)
theme_set(theme_bw())
data("housing")
new$type= ifelse(new$new<0,"below","above")
new= new[order(new$new),]
new$town= factor(new$town, levels= new$town)
new$Town= new$town
new$Standardized_prices= new$new
str(new)
new$Standardized_prices= as.numeric(new$Standardized_prices)
new$Town= as.factor(new$Town)
ggplot(new, aes(x=Town, y=Standardized_prices, label=Town)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Resale Price", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised resale price from 'House resale data'", 
       title= "Town vs Resale Price") + 
  coord_flip()

## 3. Barplot of flat_type vs resale price, ascending order
new2=tapply(housing$resale_price, housing$flat_type, mean)
new2= data.frame(new2)
View(new2)
new2$room= row.names(new2)
new2= new2[order(new2$new2),]
new2$room= factor(new2$room, levels= new2$room)
new2$Flat_type= new2$room
new2$Resale_price= new2$new2
new2$Flat_type= as.factor(new2$Flat_type)
new2$Resale_price=as.numeric(new2$Resale_price)
theme_set(theme_bw())
ggplot(new2, aes(x=Flat_type, y=Resale_price)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Flat type vs Resale Price", 
       subtitle="Avg Resale Price for different flat types") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

-
## 4. Flat model vs resale price, descending order
new3=tapply(housing$resale_price, housing$flat_model, mean)
new3= data.frame("Resale_price"=new3)
View(new3)
new3$Flat_model= row.names(new3)
str(new3)
new3$room=NULL
new3$Resale_price=as.numeric(new3$Resale_price)
new3$Flat_model= as.factor(new3$Flat_model)
new3= new3[order(new3$Resale_price),]
new3$Flat_model= factor(new3$Flat_model, levels= new3$Flat_model)
new2
theme_set(theme_bw())
ggplot(new3, aes(x=Flat_model, y=Resale_price)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Flat model vs Resale Price", 
       subtitle="Avg. Resale Price for different flat models") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


## 5. Density plots of floor area across flat_types
theme_set(theme_classic())
g <- ggplot(housing, aes(floor_area_sqm))
g + geom_density(aes(fill=factor(flat_type)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")


## 6. Scatterplot of lease remaining vs resale price
theme_set(theme_bw())  # pre-set the bw theme.
housing$lease_remaining= as.factor(housing$lease_remaining)
lease= tapply(housing$resale_price, housing$lease_remaining, mean)
lease= data.frame("Resale_price"=lease)
lease$Remaining_lease_years= row.names(lease)
str(lease)
lease$Remaining_lease_years= as.numeric(lease$Remaining_lease_years)
lease$Resale_price= as.numeric(lease$Resale_price)

gg <- ggplot(lease, aes(x=Remaining_lease_years, y=Resale_price)) + 
  geom_point() + 
  geom_smooth(method="loess", se=F)+
  labs(subtitle="Avg Resale Prices for different lease years", 
       y="Resale price", 
       x="Remaining lease years", 
       title="Remaining Lease Years Vs Price")

plot(gg)


# Basic exploration
db= subset(housing, flat_model=="DBSS")
dim(db)
min(db$year_of_sale)
max(db$year_of_sale)

s1= subset(housing, flat_model=="Type S1")
dim(s1)
min(s1$year_of_sale)
max(s1$year_of_sale)

s2= subset(housing, flat_model=="Type S2")
dim(s2)
min(s2$year_of_sale)

prem= subset(housing, flat_model=="Premium Maisonette")
dim(prem)

## 7. Boxplot of binned area vs resale price
## Discretizing the floor area into 6 bins based on equal frequency

sq_bin= discretize(housing$floor_area_sqm, "equalfreq", nbins=6)
View(sq_bin)
sq= data.frame("Resale_price"=housing$resale_price, "Area(sqm)"=sq_bin)
View(sq)
sq$Area= as.factor(sq$X)
sq$Resale_price= as.numeric(sq$Resale_price)
str(sq)
dim(sq)
mi=tapply(housing$floor_area_sqm, sq_bin, min)
ma=tapply(housing$floor_area_sqm, sq_bin, max)
mi
ma

str(sq)
sq$Area= as.character(sq$Area)
sq$X[sq$X==1]="28-67"
sq$X[sq$X==2]="68-84"
sq$X[sq$X==3]="85-99"
sq$X[sq$X==4]="100-106"
sq$X[sq$X==5]="107-122"
sq$X[sq$X==6]="123-297"
str(sq)
sq$Area= as.factor(sq$X)


sq= sq[order(sq$Resale_price),]
sq$X= factor(sq$X, levels=c("28-67","68-84","85-99","100-106","107-122","123-297"))

table(sq$X)

max(housing$floor_area_sqm)
sq
View(sq)
min(housing$floor_area_sqm)
max(housing$floor_area_sqm)
min(housing$resale_price)
max(housing$resale_price)
str(sq)
theme_set(theme_classic())

g <- ggplot(sq, aes(Area, Resale_price))
g + geom_boxplot(varwidth=T, fill="lightblue") + 
  labs(title="Binned Area(in sqm) vs Resale Price", 
       subtitle="Resale Price vs binned area",
       
       x="Binned Area",
       y="Resale Price")
table(sq$X)


## 8. Correlation plot
housing$lease_remaining= as.numeric(housing$lease_remaining)
nums = sapply(housing, is.numeric)
nums_data= housing[,nums]
str(nums_data)

correlations = cor(nums_data)
# only want the columns that show strong correlations with SalePrice
corr.SalePrice = as.matrix(sort(correlations[,'resale_price'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.00 | x < -0.00))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)


housing2= housing[,1:11]

table(housing$storey_range)
str(housing)
unique(housing$storey_range)
housing$storey_range= as.character(housing$storey_range)

## Not binning, letting the levels be
checking= function(x){
  if(x=="01 TO 03"|x=="01 TO 05"|x=="04 TO 06"|x=="06 TO 10"|x=="07 TO 09"|x=="10 TO 12"|x=="11 TO 15"|x=="13 TO 15"){
    x=x
  }else{
    x=">16"
  }
}
housing$new_floor= lapply(housing$storey_range, checking)
## 


housing$storey_range= as.factor(housing$storey_range)
housing$new_floor= as.factor(as.character(housing$new_floor))

##---------------##
str(housing)
class(storey)
storey= as.data.frame(storey)
names(storey)=c("resaleprice","storey")
str(storey)
storey$`resaleprice`= as.numeric(storey$`resaleprice`)
storey$storey= as.factor(storey$storey)
housing$new_floor= factor(housing$new_floor, levels=c("01 TO 03", "01 TO 05","04 TO 06","06 TO 10","07 TO 09","10 TO 12","11 TO 15","13 TO 15",">16"))
##---------------##

## 9. Boxplot range for floor levels, resale price
str(housing)
g <- ggplot(housing, aes(storey_range, resale_price))
g + geom_boxplot(varwidth=T, fill="lightblue") + 
  labs(title="Storey Range vs Resale Price", 
       subtitle="Resale prices for different storeys",
       
       x="Storey range",
       y="Resale price")
table(sq$X)


## 10. Checking if the quarter affects the resale price
housing$quarter= as.factor(housing$quarter)
g <- ggplot(housing, aes(quarter, resale_price))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")
table(sq$X)

## 11. Checking if month affects the resale price
housing$quarter= as.factor(housing$month_of_sale)
g = ggplot(housing, aes(quarter, resale_price))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


## 12. Flipped co-ordinates, price per new floor(original, not binned)
storey= tapply(housing$resale_price, housing$storey_range, mean)
storey= data.frame("Resale_price"=storey)
storey$Storey_range= row.names(storey)
storey$Resale_price=scale(storey$Resale_price, center = T, scale = T)
storey$Resale_price=as.numeric(storey$Resale_price)
storey= storey[order(storey$Resale_price),]
storey$Storey_range= factor(storey$Storey_range, levels= storey$Storey_range)
str(storey)
storey$storey= as.numeric(storey$storey)
class(storey)
storey$type= ifelse(storey$Resale_price<0,"below","above")
ggplot(storey, aes(x=Storey_range, y=Resale_price, label=Storey_range)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Resale Price", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Avg resale prices for various storey_ranges", 
       title= "Normalised resale price vs Storey_range") + 
  coord_flip()
theme_set(theme_bw())


## 13. Per city average sqm of houses
sqm_town= tapply(housing$floor_area_sqm, housing$town,mean)
dim(sqm_town)
sqm_town= data.frame("Floor_area"=sqm_town)
sqm_town$Town= row.names(sqm_town)
str(sqm_town)
sqm_town$sqm_town= round(sqm_town$sqm_town)
sqm_town$Floor_area=as.numeric(sqm_town$Floor_area)
sqm_town$Town=as.factor(sqm_town$Town)
str(sqm_town)
sqm_town= sqm_town[order(sqm_town$Floor_area),]
sqm_town$Town= factor(sqm_town$Town, levels= sqm_town$Town)
new2
theme_set(theme_bw())
ggplot(sqm_town, aes(x=Town, y=Floor_area)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  
  labs(title="Floor area vs town", 
       subtitle="Avg floor area for various towns") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))





## Finalizing the data to be used in the models
housing2= housing[,-c(3,4,5,12,13,14)]
str(housing2)

lease_bin= function(x){
  if(x>90){
    x=4
  }else{
    if(x>75 & x<91){
      x=3
    }else{
      if(x>60 & x<76){
        x=2
      }else{
        x=1
      }
    }
  }
}


housing2$lease_bin= lapply(housing2$lease_remaining, lease_bin)
housing2$lease_bin= as.factor(as.character(housing2$lease_bin))

## 5. Dvision of data into train, validation and test

dup= which(duplicated(housing2))
length(dup)
dim(housing2)
housing2=housing2[-dup,]
housing2=read.csv("housing2.csv")

set.seed(123)
rows= sample.split(housing2$resale_price, 0.7)
train= housing2[rows==T,]
untrain= housing2[rows==F,]

set.seed(123)
rows= sample.split(untrain$resale_price, 0.67)
validation= untrain[rows==T,]
test= untrain[rows==F,]

dim(train)
dim(validation)
dim(test)


## 6. LINEAR REGRESSION 

#MODEL 1: Direct LINREG
model1= lm(resale_price~., data=train)
summary(model1)
# Adjusted R Squared of 0.8255 
pred1= predict(model1, newdata= validation)
error1= regr.eval(pred1, validation$resale_price)
error1

# mape of 16.06%
pred1_train= predict(model1)
error1_train= regr.eval(pred1_train, train$resale_price)
error1_train
# train rmse of 16.17%
par(mfrow=c(2,2))
plot(model1)

hist(housing2$resale_price)
hist(log(housing$resale_price))
hist(sqrt(housing2$resale_price))

# Model 2: Log transformation of target

str(train)
model2= lm(log(resale_price)~., data=train)
summary(model2)
# Adjusted R Squared of 85.2% 
pred2= predict(model2, newdata= validation)
pred2= exp(pred2)
error2= regr.eval(pred2, validation$resale_price)
error2
# mape of 13.53%
pred2_train= predict(model2)
pred2_train= exp(pred2_train)
error2_train= regr.eval(pred2_train, train$resale_price)
error2_train
# train rmse of 13.54%
par(mfrow=c(2,2))
plot(model1)

length(which(duplicated(housing2)))


# Model 3: Sqrt transformation of target

str(train)
model2= lm(sqrt(resale_price)~., data=train2)
summary(model2)
# Adjusted R Squared of 85.04% 
pred2= predict(model2, newdata= validation)
pred2= pred2*pred2
error2= regr.eval(pred2, validation$resale_price)
error2
# mape of 13.37%
pred2_train= predict(model2)
pred2_train= pred2_train*pred2_train
error2_train= regr.eval(pred2_train, train$resale_price)
error2_train
# train rmse of 13.36%
par(mfrow=c(2,2))
plot(model1)
cooksd <- cooks.distance(model1)
View(cooksd)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 0.0019, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>0.0019,names(cooksd),""), col="red")  # add labels
length(which(duplicated(housing2)))

points=c(204682,387851,207278,390258,156339,211896,382076,197998,209641,394665,83895,475175,471147,437678)
length(points)
dim(train)
dim(train2)
train2=train[-c(points),]

## Regularization, feature selection

preProc<-preProcess(train[,setdiff(names(train),"resale_price")],method = c("center", "scale"))
train_reg<-predict(preProc,train)
validation_reg<-predict(preProc,validation)
mean(train$resale_price)

dummies <- dummyVars(resale_price~., data = train_reg)

x.train=predict(dummies, newdata = train_reg)
y.train=train$resale_price
x.test = predict(dummies, newdata = validation_reg)
y.test = test$resale_price

fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)

fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="gaussian",nfolds=10,parallel=TRUE)

plot(fit.lasso, xvar="lambda")
plot(fit.lasso.cv)
sum(is.na(train))
coef(fit.lasso.cv,s = fit.lasso.cv$lambda.min)
pred.lasso.cv.train <- predict(fit.lasso.cv,x.train,s = fit.lasso.cv$lambda.min)
pred.lasso.cv.test <- predict(fit.lasso.cv,x.test,s = fit.lasso.cv$lambda.min)

regr.eval(y.train,pred.lasso.cv.train)
# train 0.1496
regr.eval(y.test,pred.lasso.cv.test)
# test 0.1017

## decision trees

# rpart
train_rpart_1<-rpart(resale_price~.,data=train)
rpart(train_rpart_1)
plot(train_rpart_1)
text(train_rpart_1)
train_rpart_1
train_rpart_valid<-predict(train_rpart_1,validation)
train_rpart_train<-predict(train_rpart_1,train)
regr.eval(train_rpart_train,train$resale_price)
fancyRpartPlot(train_rpart_1)

# mape on validation for rpart: 1.413434e-01
# mape on train for rpart: 1.407799e-01

# Variables actually used in tree construction:
# flat_model, flat_type, floor_area_sqm, town, year_of_sale  





## Random Forest

model_rf = randomForest(resale_price ~ . , train, ntree = 10,mtry = 3)
summary(model_rf)
plot(model_rf)
varImpPlot(model_rf)
rf_train_pred = predict(model_rf, train)
preds_rf_test <- predict(model_rf, validation)
error= regr.eval(preds_rf_test, validation$resale_price)
error
# 5.7%
error2= regr.eval(rf_train_pred, train$resale_price)
error2
# 4.55%

## XgBoost

# XGboost 
xgb.ctrl <- trainControl(method = "cv", number = 5,
                         search='random')
set.seed(123)
xgb.tune <-train(resale_price~.,
                 data = train,
                 method="xgbLinear",
                 trControl=xgb.ctrl,
                 tuneLength=5)
XgB_final_valid= predict(xgb.tune, newdata= validation)
XgB_final_train= predict(xgb.tune, newdata= train)

length(XgB_final)
table(XgB_final)
predictions= data.frame("ID"= te_id, "Purchase"= XgB_final)
write.csv(predictions, "predictions7.csv", row.names = F)
dim(predictions)

regr.eval(XgB_final_valid,validation$resale_price)

# mape on train for XGboost: 6.221307e-02
# mape on validation for XGboost: 6.304313e-02
