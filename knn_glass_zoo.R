install.packages('xlsx')
library(xlsx)
data<-read.csv("D://r workings//knn classification//glass.csv")
View(data)
str(data)
# custom function for normalization
data_norm<- function(x){
  ((x-min(x))/max(x)-min(x))
}

# normalizing all colunns but not type column
glass_norm<-as.data.frame(lapply(data[ ,-10], data_norm))

# before normalization
summary(data)
#after normalizaion 
summary(glass_norm)
# appending normalized data column "type"
glass<-append(glass_norm,data[10])
View(glass)

model_data<- as.data.frame(glass)
View(model_data)

attach(model_data)

#spliting into training and testing data 
split_data<-sample(2,nrow(model_data),prob = c(0.7,0.3), replace = TRUE)

# training data
train_glass<-model_data[split_data ==1,] 
nrow(train_glass)
View(train_glass)

#testing data
test_glass<-model_data[split_data==2,]
nrow(test_glass)

library(class)

# knn model k=20
knn_model<-knn(train_glass, test_glass, cl= data[split_data==1,10], k=20)
confusion<-table(knn_model,data[split_data==2,10])
confusion

accuracy<- sum(diag(confusion))/sum(confusion)
accuracy ###89.23%

########### zoo data set###########
install.packages("ggplot2")
library(xlsx)
library("ggplot2")
zoo.df<-read.csv("D://r workings//knn classification//zoo.csv")
View(zoo.df)
str(zoo.df)

# Split dataset in training and test dataset
# Ignoring first and last columns as these columns contain non numeric values animal names and class tyeps respectively
# 
zoo.train <- zoo.df[1:70,2:17]
zoo.test <- zoo.df[71:101,2:17]

# Splitting last column whcih contains animal class types
zoo.train.labels <- zoo.df[1:70,18]
zoo.test.labels <- zoo.df[71:101,18]

# Applying KNN model on the training data keeping k value 9
zooClassification <- knn(train = zoo.train,test = zoo.test,cl=zoo.train.labels,k=9)

# Plotting result of KNN model
plot(zooClassification, col = rainbow(7),main="Classification of Animlas",xlab="Types of Animals")
legend(x=6.5,y=10,legend = class.df$Class_Type,cex=1,fill = rainbow(7),bty="n")
confusion <- table(zooClassification,zoo.test.labels)
confusion
accuracy<- sum(diag(confusion))/sum(confusion)
accuracy
