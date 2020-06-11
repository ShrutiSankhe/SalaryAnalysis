library(corrplot)
library(caret)
library(dplyr)
library(ggplot2)
library(readr)
library(nnet)
library(MASS)
library(Metrics)


filepath_w= "/home/yvaska/Desktop/Assignment/DA/german_salary_data_english_V1.csv"

german_sal_data <- read.csv(filepath_w)


dim(german_sal_data)
colnames(german_sal_data)
summary(german_sal_data)

# Normalize
hist(german_sal_data$yearly_income)
hist(log(german_sal_data$yearly_income)) ### this doesn't help

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Removing the lower 10th percentile IQR
sum(german_sal_data$yearly_income<=5000)
german_sal_data <- subset(german_sal_data,yearly_income > quantile(german_sal_data$yearly_income, c(.10)))
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Remove NA Columns

colnames(german_sal_data)[ apply(german_sal_data, 2, anyNA) ]
german_sal_data <- subset(german_sal_data, select = -c(monthlyORhourly,tax_grade,salary_type,skill_type,paid_holiday_days))
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

str(german_sal_data)
names(german_sal_data)
### Find top 5 salary by industry
finding1 <- german_sal_data %>% group_by(Industry_type) %>% summarize(MEANSAL=round(mean(yearly_income),0),MEANEDU=round(mean(education) ,0),MEANMALEPERCENT=round(mean(male_percent) ,0),MEANFEMALEPERCENT=round(mean(female_percent) ,0)) %>% arrange(desc(MEANSAL)) %>% head(n=5) 
### Find top 5 salary by skill type,
finding2 <- german_sal_data %>% group_by(skill_type) %>% summarize(MEANSAL=round(mean(yearly_income),0),MEANAGE=round(mean(Age) ,0),MEANEXPERIENCE=round(mean(yrs_of_Exp) ,0),MEANWORKHR=round(mean(weekly_work_hour) ,0)) %>% arrange(desc(MEANSAL)) %>% head(n=5) 
## Find top 5 salary by education
finding3 <- german_sal_data %>% group_by(education) %>% summarize(MEANSAL=round(mean(yearly_income),0)) %>% arrange(desc(MEANSAL)) 

ggplot(data=finding1, aes(x=(as.factor(Industry_type)), y=MEANSAL, fill=factor(Industry_type))) +   geom_bar(stat="identity") +   geom_text(aes(x=as.factor(Industry_type), y=MEANSAL, label=paste0("$",MEANSAL))) +  scale_fill_brewer(palette = "Green")
ggplot(data=finding2, aes(x=as.factor(skill_type), y=MEANSAL, fill=factor(skill_type))) +   geom_bar(stat="identity") +   geom_text(aes(x=as.factor(skill_type), y=MEANSAL, label=paste0("$",MEANSAL))) + scale_fill_brewer(palette = "PuBu")
ggplot(data=finding3, aes(x=as.factor(education), y=MEANSAL, fill=factor(education))) +   geom_bar(stat="identity") +   geom_text(aes(x=as.factor(education), y=MEANSAL, label=paste0("$",MEANSAL))) +   scale_fill_brewer(palette = "Purples")
## Find salary by age
Age_bin <-  seq(15, 55, by=5)
Salary <- round(tapply(as.numeric(german_sal_data$yearly_income), cut(german_sal_data$Age, seq(15, 60, by=5)), mean),0)
newdf <- data.frame(Age_bin,Salary )

ggplot(data=newdf, aes(x=Age_bin, y=Salary, fill=factor(Age_bin))) +geom_bar(stat="identity") +  
  geom_text(aes(x=Age_bin, y=Salary, label=Salary)) +theme_bw() + geom_text(aes(label = Salary), position = position_dodge(0.6)) + scale_fill_brewer(palette = "Greens")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plot(sort(german_sal_data$yearly_income), main = "Sorted Base Pay")

#Iearning more than 50K Euro.
table(german_sal_data$yearly_income > 50000)
#Histograms provide a quick view of the distribution of your data. In this instance, the histogram 
hist(german_sal_data$yearly_income, breaks = 100)
ggplot(german_sal_data, aes(x=yearly_income)) + geom_histogram(aes(y=..density..),binwidth = 2500,colour="purple", fill="white") + theme_bw()+geom_density(alpha=.2, fill="blue")

#Next, we try boxplots, 
bp <- boxplot(german_sal_data$yearly_income, main = "Base Pay Box Plot") 
str(bp)
bp$stats

## Lets cpmpare between skilled worker(2) and ranked(3) employees
select_rank <- c(2, 3)
german_sal_data_tmp <- german_sal_data[german_sal_data$rank %in% select_rank,]
dim(german_sal_data_tmp)
boxplot(german_sal_data_tmp$yearly_income ~ german_sal_data_tmp$rank)
ggplot(german_sal_data_tmp,aes(x=factor(rank),y=yearly_income)) +geom_boxplot(fill = "grey", colour = "blue", alpha = 0.7)+ theme_bw()

#for rank 3 is much higher than rank 2


##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Version 2: Convert integer columnns to factors

numericCol <- german_sal_data[ ,(colnames(german_sal_data) %in% c("Age","yearly_income","male_percent","weekly_work_hour","female_percent","yrs_of_Exp"))]
factorcCol <-  german_sal_data[ ,!(colnames(german_sal_data) %in% c("Age","yearly_income","male_percent","weekly_work_hour","female_percent","yrs_of_Exp"))] %>%  lapply(factor) %>% data.frame() 
german_sal_data <-cbind(numericCol, factorcCol)

str(german_sal_data)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CORRELATION only for numeeric values

cor(numericCol)
corrplot(cor(numericCol), type="upper")

#Annova for Categorical Variables
AnovaTest <- aov( yearly_income ~ Age + education +gender + job + rank + employment_type + male_percent + Industry_type + yrs_of_Exp + REGION, data = german_sal_data )
summary(AnovaTest)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

### Break data into test and training
set.seed(12345)
TrainSet <- createDataPartition(y = german_sal_data$yearly_income, p = 0.70, list = FALSE)
training <- german_sal_data[TrainSet,]
testing <- german_sal_data[-TrainSet,]
set.seed(12345)

# Linear Model
#Try linear model using all features
Model1 <- lm(yearly_income~.,data = training)
## Regression model
#Model1 <- lm(yearly_income ~ REGION + employee_type + Industry_type + skill_type + gender + birth_year + job_start_year + no_of_child + job + rank + education + employment_type + weekly_work_hour + paid_holiday_days, data=german_sal_data_tmp)
summary(Model1)

plot(Model1)
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Version 2: Remove the features with high P Values

german_sal_data<-german_sal_data[ ,!(colnames(german_sal_data) %in% c("male_percent","female_percent"))]
training<-training[ ,!(colnames(training) %in% c("male_percent","female_percent"))]
testing<-testing[ ,!(colnames(testing) %in% c("male_percent","female_percent"))]
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

set.seed(12345)
Model2 <- lm(yearly_income~.,data = training)
summary(Model2)
str(Model2)
plot(Model2)
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fitted Values and Accuracy

predictedValues <- predict(Model2,testing,interval="confidence")
RMSE <- rmse(testing$yearly_income,predictedValues[,1]); RMSE

#mnglmModel <- glm(yearly_income ~., data=training, family=binomial())
#MnglmPredict <- format(predict(mnglmModel,testing))
#MnglmPdf <- data.frame(MnglmPredict)


#############################################################################################################

# Multi nominal prediction

str(factorcCol)

# Chi-Test
chitest <- data.frame()
for(i in 1:(dim(factorcCol)[2]))
   {X <- mapply(function(x, y) chisq.test(x, y)$p.value, factorcCol[, -i], MoreArgs=list(factorcCol[,i]))
    chitest <- rbind(chitest,X)
     }

# Model, fitted Values and COnfusion Matrix
mnModel2 <- multinom(no_of_child ~., training,maxit=1000)
MnPredict2 <- predict(mnModel2,testing)
confusionMatrix(testing$no_of_child,MnPredict2)


