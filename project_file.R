#TO IMPLEMENT NAIVE-BAYES CLASSIFICATION ON THEE GIVEN SET OF DATA

#we need to download this library first to implment naive bayes
install.packages("e1071")
install.packages("ggplot2")
library(e1071)
library(ggplot2)

#to check the working directory
getwd()
#to change to the present working directory
setwd("C:\\Users\\hp\\Desktop\\R DOC\\project_report")

#for retrieving the data from the csv file
df<- read.csv("nassCDS.csv",header=TRUE)
print(df)
#this will create a separate pane for the dataset
View(df)

#we need dead,airbag,seatbelt and sex in our dataset, all the other data in the given dataset is irrelevant
#for our project, hence we create a new dataset from the existing one.

#DATA PREPARATION

dataframe1<- data.frame(dead=df$dead,airbag=df$airbag,seatbelt=df$seatbelt,sex=df$sex)
dataframe1
write.csv(dataframe1,file="dataset1.csv",row.names = FALSE)
df1<-read.csv("dataset1.csv",header=TRUE)
print(df1)


#Visualisation of data

#we can have a visualisation for the year of accidents and severity of the injury.This will help us in unerstanding the data.
counts<-table(df$injSeverity,df$yearacc)
#legend is used to give description about the severity level
barplot(counts,main = "YEAR OF ACCIDENT VS SEVERITY OF INJURY",xlab ="year of accident",ylab = "severity",col = c("#0000FFFF","#0080FFFF"),legend=rownames(counts),beside = TRUE)
#plot function 
p1<-plot(df$dead,df$ageOFocc)
p<-ggplot(df,aes(x=df$dead,y=df$ageOFocc))+geom_boxplot()+geom_jitter()+geom_boxplot(outlier.size = 0,alpha=0.8)+ggtitle("Dead Vs Age Of the Person involved in acc")+guides(colours=TRUE)
p
#DATA MANIPULATION
#checking the levels or the categories lying under each column

levels(df1$dead)
levels(df1$airbag)
levels(df1$seatbelt)
levels(df1$sex)

#APPLYING NAIVE_BAYES THEOREM(CREATING A MODEL)

naive_bayes_model = naiveBayes(df1$dead~.,data=df1)
#to print the model summary
naive_bayes_model
#the above also prints the apriori probability which predicts the distribaution of our data

#PREDICTION

prediction=predict(naive_bayes_model,dataframe1)
#creation of confusion matrix to check accuracy of the prediction
table(prediction,df1$dead)

#the below given command is used to create a probability table
prop.table(table(prediction,df1$dead),1)
