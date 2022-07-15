library(stargazer)

train = read.csv("RetailMartTrain.csv", sep = ";", header = T)
test = read.csv("RetailMartTest.csv", sep = ";", header = T)

train$Male = ifelse(train$Implied.Gender=="M",1,0)
train$Female = ifelse(train$Implied.Gender=="F",1,0)
train$Implied.Gender = NULL

train$Home = ifelse(train$Home.Apt..PO.Box=="H",1,0)
train$Apt = ifelse(train$Home.Apt..PO.Box=="A",1,0)
train$Home.Apt..PO.Box = NULL

#Specifying models
model1 = glm(PREGNANT ~ Ginger.Ale + Birth.Control + Wine + Cigarettes,
             data = train, family = "binomial")
summary(model1)

model2 = glm(PREGNANT ~ Ginger.Ale + Folic.Acid + Sea.Bands + Pregnancy.Test, family = "binomial", data = train)
summary(model2)

model3 = glm(PREGNANT ~ ., family = "binomial", data = train)
summary(model3)

stargazer(model1,model2,model3,type = "text")

#Creating predictions
test$preds1 = predict(model1, newdata = test, type = "response")
summary(test$preds1)
test$preds2 = predict(model2, newdata = test, type = "response")
summary(test$preds2)
test$preds3 = predict(model3, newdata = test, type = "response")
summary(test$preds3)

#install.packages("ROCR")
library(ROCR)
p1 = prediction(test$preds1, test$PREGNANT)
perf1 = performance(p1, measure = "tpr", x.measure = "fpr")

p2 = prediction(test$preds2,test$PREGNANT)
perf2 = performance(p2, measure = "tpr", x.measure = "fpr")

p3 = prediction(test$preds3,test$PREGNANT)
perf3 = performance(p3, measure = "tpr", x.measure = "fpr")

#Plotting ROC curves
plot(perf1, col = "black")
plot(perf2, col = "red", add = T)
plot(perf3, col = "blue", add = T)

#Calculating area under the curve
auc1 = performance(p1, measure = "auc")
auc1 = auc1@y.values[[1]]
auc1
auc2 = performance(p2, measure = "auc")
auc2 = auc2@y.values[[1]]
auc2
auc3 = performance(p3, measure = "auc")
auc3 = auc3@y.values[[1]]
auc3

#Calculating thresholds
Thresholds1 = data.frame(cut=perf1@alpha.values[[1]], fpr=perf1@x.values[[1]], 
                         tpr=perf1@y.values[[1]])

Thresholds2 = data.frame(cut=perf2@alpha.values[[1]], fpr=perf2@x.values[[1]], 
                         tpr=perf2@y.values[[1]])

Thresholds3 = data.frame(cut=perf3@alpha.values[[1]], fpr=perf3@x.values[[1]],  tpr=perf3@y.values[[1]])

Thresholds1 
Thresholds2
Thresholds3 


#QWE Case

QWE = read.csv("QWE_data.csv", sep = ",", header = T)

#1
boxplot(CustomerAge ~ Churn, data = QWE)

CSum = aggregate(Churn ~ CustomerAge, data = QWE, FUN = sum)
barplot(CSum$Churn, xlab = "Customer Age", ylab = "Churn Sums", 
        main = "Churn Sum by Customer Age")
axis(1, at = CSum$CustomerAge)

#2
model1 = glm(Churn ~ . -Customer_ID, data = QWE, family = "binomial")
summary(model1)

step(model1)

model2 = step(model1)
summary(model2)

#3
x=2553.1-2441.0
x
qchisq(.05, df = 7, lower.tail = F)

x>qchisq(.05, df = 7, lower.tail = F)

#4
-
  
#5
exp(coef(model2))

#6
QWE$ChurnProb2 = predict(model2, newdata = QWE, type = "response")
plot(QWE$CustomerAge, QWE$ChurnProb2)

model3 = glm(Churn ~ CustomerAge + I(CustomerAge^2)+ CHI0 + CHI01 + Support0 + Support01
             + Views01 + LastLogin01, data = QWE, family = "binomial") 
summary(model3)

QWE$ChurnProb3 = predict(model3, newdata = QWE, type = "response")
plot(QWE$CustomerAge, QWE$ChurnProb3)

#7
CritCust = QWE[order(-QWE$ChurnProb3),][1:100,]


#ROC curves for models 2 and 3
p1 = prediction(QWE$ChurnProb2,QWE$Churn)
perf1 = performance(p1,measure = "tpr", x.measure = "fpr")
plot(perf1,xlim=c(0,1),ylim=c(0,1))

p2 = prediction(QWE$ChurnProb3,QWE$Churn)
perf2 = performance(p2,measure = "tpr", x.measure = "fpr")
plot(perf2,xlim=c(0,1),ylim=c(0,1),lty=2)

plot(perf1,xlim=c(0,1),ylim=c(0,1))
plot(perf2,xlim=c(0,1),ylim=c(0,1), lty=2, add=TRUE)
legend(.9, .8, legend=c("Model 2", "Model 3"), lty=1:2)





#Hierarchical Cluster Analysis
BeersRaw = read.csv("Beers.csv", sep = ",", header = T)

str(BeersRaw)

Beers = BeersRaw

#standardizing the data
Beers$Calories = round(scale(Beers$Calories, center = T, scale = T),2)
Beers$Sodium = round(scale(Beers$Sodium, center = T, scale = T),2)
Beers$Alcohol = round(scale(Beers$Alcohol, center = T, scale = T),2)
Beers$WS.Price = round(scale(Beers$WS.Price, center = T, scale = T),2)

cor(Beers[,2:5])

BeerDistances = (dist(Beers[,2:5], method = "euclidean", diag = T))^2
BeerDistances

#Hierarchical clustering
BeerClusters = hclust(BeerDistances, method = "centroid")

#Building agglomeration schedule
Agglomeration = data.frame(ClustersRemaining = 
                             seq(from = nrow(Beers)-1, to=1, by=-1),
                           CentroidDistances = BeerClusters$height,
                           Item1 = BeerClusters$merge[,1],
                           Item2 = BeerClusters$merge[,2])
Agglomeration

plot(BeerClusters)

rect.hclust(BeerClusters, k = 4, border = 2:4)

#Adding cluster to original dataset
Beers$Cluster = cutree(BeerClusters, k = 4)

#Creating subsets for each cluster for further analysis
C1 = subset(Beers, Cluster == 1)


#Nonhierarchical Cluster Analysis

winedata = read.csv("Wine.csv", sep = ",", header = T)
str(winedata)

#replacing NA's with zeros
winedata[is.na(winedata)] = 0

#install.packages("skmeans")
library(skmeans)

ncol(winedata)

winedata.transposed = t(winedata[,8:107])

#Nonhierarchical clustering
set.seed(4)
winedata.clusters = skmeans(winedata.transposed, k=3, method = "genetic")
winedata.clusters

#show everyone who is in cluster 3
winedata.clusters$cluster[winedata.clusters$cluster == 3]

#sum purchases of offers by cluster
winedata.clustercounts = aggregate(winedata.transposed, by = list(winedata.clusters$cluster), FUN = sum)[,2:33]

#Creating final dataset
winedata.complete = cbind(winedata[,1:7], t(winedata.clustercounts))
