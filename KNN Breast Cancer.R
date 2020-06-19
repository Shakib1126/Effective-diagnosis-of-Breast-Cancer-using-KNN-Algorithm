data=read.csv("C:/Users/shaki/Desktop/python/BREAST CANCER/breastCancer.csv")

data

str(data)
summary(data)

#PRE PROCESSING

data$Clump_Thickness=as.numeric(data$Clump_Thickness)

data$Size_Uniformity=as.numeric(data$Size_Uniformity)

data$Shape_Uniformity=as.numeric(data$Shape_Uniformity)

data$Marginal_Adhesion=as.numeric(data$Marginal_Adhesion)

data$Single_Epithelial_Size=as.numeric(data$Single_Epithelial_Size)

data$Bare_Nucleoli=as.numeric(data$Bare_Nucleoli)

data$Bland_Chromatin=as.numeric(data$Bland_Chromatin)

data$Normal_Nucleoli=as.numeric(data$Normal_Nucleoli)

data$Mitoses=as.numeric(data$Mitoses)

data$Class=as.factor(data$Class)




data[is.na(data$Clump_Thickness)]<-mean(data$Clump_Thickness,na.rm=TRUE)

data[is.na(data$Size_Uniformity)]<-mean(data$Size_Uniformity,na.rm = TRUE)

data[is.na(data$Shape_Uniformity)]<-mean(data$Shape_Uniformity,na.rm = TRUE)

data[is.na(data$Marginal_Adhesion)]<-mean(data$Marginal_Adhesion,na.rm = TRUE)

data[is.na(data$Single_Epithelial_Size)]<-mean(data$Single_Epithelial_Size,na.rm = TRUE)

data[is.na(data$Bare_Nucleoli)]<-mean(data$Bare_Nucleoli,na.rm = TRUE)

data[is.na(data$Bland_Chromatin)]<-mean(data$Bland_Chromatin,na.rm = TRUE)

data[is.na(data$Normal_Nucleoli)]<-mean(data$Normal_Nucleoli,na.rm = TRUE)

data[is.na(data$Mitoses)]<-mean(data$Mitoses,na.rm = TRUE)





sub <- sample(nrow(data), floor(nrow(data) * 0.75))

training <- data[sub,(2:10) ]

training.class=data[sub,11]

testing <- data[-sub, (2:10)]

testing.class<- data[-sub,11]



library("class")

predict<-knn(training, testing,training.class,k = 10,prob=FALSE)
cm = as.matrix(table(Actual = testing.class, Predicted = predict))
accuracy=sum(diag(cm))/length(testing.class)
print(accuracy)

#Precision = TP/ TP+FP
Precision = 114/(114+2)
print(Precision)


#Recall = TP / TP+FN
Recall = 114 / (114+8)
print(Recall)



#Fmeasure = 2 * recall * precision / recall + precision
Fmeasure = (2 * 0.9344 * 0.9827 )/ (0.9344 +0.9827)
print(Fmeasure)




