

library(mice)
data <- read.csv("https://contentwithgraphics2.s3-eu-west-1.amazonaws.com/online_shoppers_intention.csv")
dim(data)

train <- head(data, 10330)
test <- tail(data,2000)

# sprawdzalem czy przy podziale nasza zmienna celu nie zostala tylko w jednym datasecie
summary(train$Revenue)
summary(test$Revenue)

# usuwam zmienna celu z datasetu testowego, odpowiedzi bedziemy trzymac w excelu
test <- test[,1:17]
dim(test)


# usuniecie brak?w danych
imputed_train<-na.omit(train)

summary(imputed_train)

#zamiana zmiennych tekstowych na numeryczne
imputed_train$Month = factor(imputed_train$Month, levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul', 'Aug','Sep','Oct','Nov','Dec'),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
imputed_train$VisitorType = factor(imputed_train$VisitorType, levels = c('Returning_Visitor','New_Visitor', 'Other'),  labels = c(0,1,2))
imputed_train$Weekend = factor(imputed_train$Weekend, levels = c('FALSE','TRUE'),  labels = c(0,1))
imputed_train$Revenue = factor(imputed_train$Revenue, levels = c('FALSE','TRUE'),  labels = c(0,1))

summary(imputed_train)
dim(imputed_train)

# usuniecie brak?w danych test
imputed_test<-na.omit(test)
summary(imputed_test)

#zamiana zmiennych tekstowych na numeryczne
imputed_test$Month = factor(imputed_test$Month, levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul', 'Aug','Sep','Oct','Nov','Dec'),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
imputed_test$VisitorType = factor(imputed_test$VisitorType, levels = c('Returning_Visitor','New_Visitor', 'Other'),  labels = c(0,1,2))
imputed_test$Weekend = factor(imputed_test$Weekend, levels = c('FALSE','TRUE'),  labels = c(0,1))
# imputed_test$Revenue = factor(imputed_test$Revenue, levels = c('FALSE','TRUE'),  labels = c(0,1))

summary(imputed_test)
dim(imputed_test)


# Sprawdzam zawartosc poszczeg?lnych zmiennych

library(ISLR)
str(test)

# Wyznaczam statystyki opisowe zmiennych liczbowych, w celu okreslenia rozkladu zmiennych 


library(dlookr)
# describe(test)

#wyznaczam wspolczynnik korelacji wszystkich kombinacji
correlate(test)

#macierz koleracji przedstawiona graficznie
plot_correlate(test)

#Wykresy dla naszych zmiennych cz1.
par(mfrow=c(2,2))
hist(test$Informational, xlab = "Informational", ylab = "Frequency",main = "Informational pages", col = "darkblue")
hist(test$Administrative, xlab = "Administrative", ylab = "Frequency",main = "Administrative pages", col = "darkblue")
hist(test$ProductRelated, xlab = "Product Related", ylab = "Frequency",main = "Product Related pages", col = "darkblue")
hist(test$TrafficType, xlab = "Traffic Type", ylab = "Frequency",main = "Traffic Type", col = "darkblue")

	
library(rpart)
library(rpart.plot)



fit <- rpart(Revenue~., data = imputed_train, method = 'class')
rpart.plot(fit, extra = 106)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(imputed_train))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(imputed_train)), size = smp_size)

train <- imputed_train[train_ind, ]
valid <- imputed_train[-train_ind, ]


library(rpart)


library("e1071")

library("caret")

drzewo_ucz<-rpart(Revenue~.,train)
print(drzewo_ucz)
plot(drzewo_ucz,margin = 0.1)
text(drzewo_ucz)
dev.off()

klasyfikacja_ucz<-predict(drzewo_ucz, train,type="class")
klasyfikacja_ucz


caret::confusionMatrix(klasyfikacja_ucz,train$Revenue)


klasyfikacja_walid<-predict(drzewo_ucz,valid, type = "class")

klasyfikacja_walid
valid$wynik_klasyfikacji<-klasyfikacja_walid

caret::confusionMatrix(klasyfikacja_walid,valid$Revenue)

summary(imputed_train$Month)
unique(imputed_train$Month)

imputed_train2<-na.omit(imputed_train)

unique(imputed_train2$Month)

#import the package
library(randomForest)

dim(train)
dim(test)

# Perform training:
set.seed(123)
rf_classifier = randomForest(Revenue ~., data=imputed_train2, ntree=150)

print(rf_classifier)
#caret::confusionMatrix(preds,valid$Revenue)

dim(train)

columns = names(imputed_train2)
str(imputed_train2)

columns[1:17]

imputed_train2$Month = as.numeric(imputed_train2$Month)
imputed_train2$VisitorType = as.numeric(imputed_train2$VisitorType)
imputed_train2$Weekend = as.numeric(imputed_train2$Weekend)
imputed_train2$Revenue = as.numeric(imputed_train2$Revenue)

str(imputed_train2)

unique(imputed_train2$Month)
unique(train$Month)

require(neuralnet)

# fit neural network
nn=neuralnet(Revenue~Administrative+Administrative_Duration
             +Informational+Informational_Duration
             +ProductRelated+ProductRelated_Duration+BounceRates+ExitRates+PageValues+
               SpecialDay+Month+OperatingSystems+Browser+Region+TrafficType+VisitorType+Weekend
            ,data=imputed_train2, hidden=4,act.fct = "logistic",
             linear.output = FALSE)

# plot neural network
plot(nn)


imputed_test$Month = as.numeric(imputed_test$Month)
imputed_test$VisitorType = as.numeric(imputed_test$VisitorType)
imputed_test$Weekend = as.numeric(imputed_test$Weekend)

str(imputed_test)

predict=compute(nn, imputed_test)
predict$net.result


prob <- predict$net.result
pred <- ifelse(prob>0.5, 1, 0)

unique(pred)
