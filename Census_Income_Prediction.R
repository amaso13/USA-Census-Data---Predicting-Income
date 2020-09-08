# Data Science Projec: Census Income Prediction


# 1.	Data Preprocessing:
# a)	Replace all the missing values with NA.
 censusData<-read.csv("C:/Users/ADMIN/Desktop/DSU/Semesters/Learning_Data_science/Data Science with Intellipaat/Porject sessions/census-income.csv", stringsAsFactors = T)
 library(plotly)
 plot_ly(data=censusData, x = ~capital.gains, y = ~hours.per.week)
 View(censusData)
 str(censusData)
 # convert column into character. Because the dataset was imported as Factor, we need to change the columns into characters so we can manipulate the data
 censusData$workclass<-as.character(censusData$workclass)
 censusData$occupation<-as.character(censusData$occupation)
 censusData$native.country<-as.character(censusData$native.country)
 censusData$education<-as.character(censusData$education)
 censusData$marital.status<-as.character(censusData$marital.status)
 censusData$relationship<-as.character(censusData$relationship)
 censusData$race<-as.character(censusData$race)
 censusData$sex<-as.character(censusData$sex)
 censusData$X<-as.character(censusData$X)
 str(censusData)
colSums(is.na(censusData))
censusData[censusData==" ?"]<- NA
 View(censusData)
 
# b)	Remove all the rows that contain NA values.
 table(is.na(censusData)) 
 censusData <-na.omit(censusData)

 # c)	Remove all whitespaces from the columns.
 install.packages("stringr")
 install.packages("dplyr")
library(stringr) 
library(dplyr)

censusData<-mutate_if(censusData, is.character, str_trim)

# in order to work with models, vizualize, we need to convert them into factors
censusData$workclass<-as.factor(censusData$workclass)
censusData$occupation<-as.factor(censusData$occupation)
censusData$native.country<-as.factor(censusData$native.country)
censusData$education<-as.factor(censusData$education)
censusData$marital.status<-as.factor(censusData$marital.status)
censusData$relationship<-as.factor(censusData$relationship)
censusData$race<-as.factor(censusData$race)
censusData$sex<-as.factor(censusData$sex)
censusData$X<-as.factor(censusData$X)
str(censusData)
plot(censusData$X)


# 2.	Data Manipulation:
#   Your task is to perform data manipulation to analyze the data set using various functions from the dplyr package.
summary(censusData)

# a)	Extract the "education" column and store it in "census_ed" .
census_ed<-censusData$education
View(census_ed)
class(census_ed)
head(census_ed)

# b)	Extract all the columns from "age" to "relationship" and store it in "census_seq".
census_seq<-censusData%>%
select(age:relationship)
View(census_seq)
#censusData[1:8]->c
#View(c)
# c)	Extract the column number "5", "8", "11" and store it in "census_col".
census_col<-censusData[,c(5,8,11)]
# we could also use 'Select' as #select(censusData, 5, 8, 11)
View(census_col)
head(census_col)

# d)	Extract all the male employees who work in state-gov and store it in "male_gov".
male_gov<-censusData%>% filter(sex == "Male" & workclass=="State-gov")
View(male_gov)

# e)	Extract all the 39 year olds who either have a bachelor's degree 
#    or who are native of United States and store the result in "census_us".

table(censusData$native.country)
table(censusData$education)
census_us<-censusData%>%filter(age==39&(education=="Bachelors"|native.country=="United-States"))
View(census_us)

# f)	Extract 200 random rows from the "census" data frame and store it in "census_200".
census_200<-sample_n(censusData,200)
View(census_200)

# g)	Get the count of different levels of the "workclass" column.
install.packages("plyr")
library(plyr)
countWcls<-count(censusData$workclass)
countWcls
table(censusData$workclass)

# h)	Calculate the mean of "capital.gain" column grouped according to "workclass".

tapply(censusData$capital.gain,censusData$workclass,mean)



# 3.	Data Visualization:
#
install.packages("ggplot2")
library(ggplot2)

# a)	Build a bar-plot for the "relationship" column and fill the bars according to the "race"
# column.
ggplot(censusData,aes(x=relationship,fill=race))+
  geom_bar()
# i.	Set x-axis label to 'Categories of Relationships'
# ii.	Set y-axis label to 'Count of Categories'
ggplot(censusData,aes(x=relationship,fill=race))+
  geom_bar()+
  labs(x="Categories of Relationships",y="Count of Categories")
# iii.	Fill the bars according to "sex"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar()+
  labs(x="Categories of Relationships",y="Count of Categories")
# iv.	Set the position of the bars to "dodge"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories")
# v.	Set the title of plot to be 'Distribution of Relationships by Sex"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of Relationships by Sex")

# b)	Build a Histogram for the "age" column with number of bins equal to 50.
ggplot(censusData,aes(x=age))+geom_histogram(bins = 90)
table(censusData$age)
# i)	Fill the bars of the histogram according to yearly income column i.e., "X"
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 90)
# ii)	Set the title of the plot to "Distribution of Age".
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 90)+
  labs(title = "Distribution of Age")
# iii)Set the legend title to "Yearly income".
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 90)+
  labs(title = "Distribution of Age",fill='Yearly income')
# iv) Set the theme of the plot to black and white.
ggplot(censusData,aes(x=age))+geom_histogram(bins =90)+
  labs(title = "Distribution of Age")+theme()

# c)	Build a scatter-plot between "capital.gain" and "hours.per.week".
#     Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.
ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+geom_point()
# i)	Set the transparency of the points to 40% and size as 2.
ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+
  geom_point(alpha=0.6,size=2)
# ii)	Set the color of the points according to the "X" (yearly income) column. 
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,fill=X))+geom_point()
# iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
# to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,fill=X))+
  geom_point(alpha=0.6,size=2)+
labs(x="Capital Gain",y="Hours per Week",
     title = "Capital Gain vs Hours per Week by Income", fill="Yearly Income") 
install.packages("plotly")
library(plotly)
plot_ly(data=censusData, x = ~capital.gain, y = ~hours.per.week, color = ~X, type='scatter')

# d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
# "age" on the y-axis.
 ggplot(censusData,aes(x=education,y=age))+geom_boxplot()
# Fill the box-plots according to the "sex" column.
 ggplot(censusData,aes(x=education,y=age,fill=sex))+geom_boxplot()
# Set the title to "Box-Plot of age by Education and Sex".
 ggplot(censusData,aes(x=education,y=age,fill=sex))+
   geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex") 

# 4.	Prediction guilding a Linear Regression Model:
# Build a simple linear regression model as follows: # Divide the dataset into training and test sets in 70:30 ratio.
 set.seed(98)  #any integer number no problem
 install.packages("caTools")
 library("caTools")
 split_data<-sample.split(censusData$hours.per.week,SplitRatio = 0.70)
 View(split_data)
 censusTrain<-subset(censusData,split_data==T)
 censusTest<-subset(censusData,split_data==F)
 View(censusTrain)
 View(censusTest)
 nrow(censusTrain)
 nrow(censusTest)
View(split_data)
 # Build a linear model on the train set where the dependent variable is
# "hours.per.week" and independent variable is "education.num".
 #dependent~independ
 View(censusData[c('hours.per.week','education.num')])
 
 LR_model<-lm(hours.per.week~education.num,data=censusTrain)
 summary(LR_model)

# Predicting the values on the test set and find the error in prediction. 
 ## Find the root-mean-square error (RMSE).
 censusP<-predict(LR_model,newdata=censusTest)
 head(censusP)
 View(censusP)
 censusD<-cbind(Actual=censusTest$hours.per.week,Predicted=censusP)
 View(censusD)
 class(censusD)
 censusD<-as.data.frame(censusD)
 Error<-censusD$Actual-censusD$Predicted
 View(Error)
Data<-cbind(censusD,Error)
 View(Data)
 
 sqrt(mean((Data$Error)^2))
library(caret)
RMSE(censusP, censusTest$hours.per.week)


# 5. Prediction building a 	Logistic Regression

# let's divide the dataset into training and test sets in 65:35 ratio.
 split_data1<-sample.split(censusData$X,SplitRatio = 0.65)
 censusTrain1<-subset(censusData,split_data1==T)
 censusTest1<-subset(censusData,split_data1==F)
 nrow(censusTrain1)
 nrow(censusTest1)
 
# now let's build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".
log_mod<-glm(X~occupation,data=censusTrain1,family = "binomial")
summary(log_mod)
# Predicting the values on the test set.
pred_val<-predict(log_mod,newdata =censusTest1,type = "response")#probability
head(pred_val)
range(pred_val)
install.packages("ROCR")
library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest1$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)## Check for which valve accuracy get constant
table(censusData$X)
# Plot accuracy vs cut-off and pick an ideal value for cut-off.
lm.pred<-ifelse(pred_val>0.47,">50K","<=50K")  
lm.pred
# Build a confusion matrix and find the accuracy.
tab<-table(lm.pred,censusTest1$X)
tab
#TP FP
#FN TN
#TP TN -correctly predicted
#FP FN - wrongly predicted

(7188+660)/(7188+660+1968+741)
accuracy<-sum(diag(tab))/sum(tab)
accuracy

# Plot the ROC curve and find the auc(Area Under Curve). 
roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc
split_data1<-sample.split(censusData$X,SplitRatio = 0.80)
censusTrain2<-subset(censusData,split_data1==T)
censusTest2<-subset(censusData,split_data1==F)
log_mod2<-glm(X~age+workclass+education,data=censusTrain2,family = "binomial")
summary(log_mod2)
pred_val<-predict(log_mod2,newdata =censusTest2,type = "response")
head(pred_val)

library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest2$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)
lm.pred<-ifelse(pred_val>0.45,">50K","<=50K")  
lm.pred

tab<-table(lm.pred,censusTest2$X)
tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy

roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc


# 6.  Prediction building a  Decision Tree Model:
# Divide the dataset into training and test sets in 70:30 ratio.
set.seed(123)
split_data<-sample.split(censusData,SplitRatio = 0.70)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)
# Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables
library(rpart)
library(rpart.plot) 

census_model<-rpart(formula = X~.,
                    data = censusTrain,
                    method = "class")

# Plot the decision tree 
rpart.plot(x= census_model, type= 5, extra = 0,tweak = 1.5)

# Predict the values on the test set
class_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")
#TP FP
#FN TN
#TP TN -correctly predicted
#FP FN - wrongly predicted
# Build a confusion matrix and calculate the accuracy
tab<-table(class_prediction,censusTest$X)
tab
sum(diag(tab))/sum(tab)
#Precision


# 7. Prediction building a random Forest:
#
# let's build a random forest model
# Divide the dataset into training and test sets in 80:20 ratio
set.seed(123)
split_data<-sample.split(censusData$X,SplitRatio = 0.8)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)
# now let's build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
library(randomForest)

census_model<-randomForest(formula=X~.,
                           data=censusTrain,
                           ntree=300)

plot(census_model)
text(census_model)

# Predict values on the test set
cenus_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")
# Build a confusion matrix and calculate the accuracy
tab<-table(cenus_prediction,censusTest$X)
tab
sum(diag(tab))/sum(tab)

