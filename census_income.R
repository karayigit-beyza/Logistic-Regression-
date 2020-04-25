#Set working directory 
setwd('C:/Users/busra/Documents/Beyza')
getwd()

#Installing Libraries 
install.packages('tidyverse')
install.packages("ROCR")

#Libraries 
library(tidyverse)
library(ROCR)
library(caTools)


#Importing the datasets
testdata <- read.csv('adult.test', sep =',', header = FALSE, skip = 1, col.names = c('age','work_status', 'final_weight', 'edu_level', 'edu_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'response'), stringsAsFactors = FALSE)
mydata <- read.csv('adult.data', sep =',', header = FALSE,  col.names = c('age','work_status', 'final_weight', 'edu_level', 'edu_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'response'), stringsAsFactors = FALSE)

#Combining two files 
all_data <- rbind(mydata, testdata)

#Exploring the dataset
str(all_data)
summary(all_data)
#Checking the null values
colSums(is.na(all_data))

#Changing response variable
all_data$response[all_data$response == " <=50K"] <- "0"
all_data$response[all_data$response == " >50K"] <- "1"
all_data$response[all_data$response == " <=50K."] <- "0"
all_data$response[all_data$response == " >50K."] <- "1"

#Replacing ? with NA
all_data$age[all_data$age == " ?"] <- NA
all_data$work_status[all_data$work_status == " ?"] <- NA
all_data$final_weight[all_data$final_weight == " ?"] <- NA
all_data$edu_level[all_data$edu_level == " ?"] <- NA
all_data$edu_num[all_data$edu_num == " ?"] <- NA
all_data$marital_status[all_data$marital_status == " ?"] <- NA
all_data$occupation[all_data$occupation == " ?"] <- NA
all_data$relationship[all_data$relationship == " ?"] <- NA
all_data$race[all_data$race == " ?"] <- NA
all_data$sex[all_data$sex == " ?"] <- NA
all_data$capital_gain[all_data$capital_gain == " ?"] <- NA
all_data$capital_loss[all_data$capital_loss == " ?"] <- NA
all_data$hours_per_week[all_data$hours_per_week == " ?"] <- NA
all_data$native_country[all_data$native_country == " ?"] <- NA
all_data$response[all_data$response == " ?"] <- NA


#Creating new dataset with unique rows
unique_alldata <- unique(all_data)
summary(unique_alldata)
colSums(is.na(unique_alldata))

#Comparing edu_level and edu num
education <-  unique_alldata %>% select(, 4:5) 
education <- group_by(education, edu_level, edu_num)
summarise(education)
education %>% count(edu_level)
## We redesigned the edu_level column as preschool, 1st-4th, 5th-8th, 
## High School Grad, Some College, Associate, Bachelors, Grad School
unique_alldata$edu_level <- ifelse(unique_alldata$edu_level %in% c(" 5th-6th"," 7th-8th"), "Middle-School",
                                   ifelse(unique_alldata$edu_level %in% c(" 9th", " 10th" , " 11th" , " 12th", " HS-grad"), "HS-Grad",
                                          ifelse(unique_alldata$edu_level %in% c(" Assoc-voc", " Assoc-acdm"), "Assc-Deg",
                                                 ifelse(unique_alldata$edu_level %in% c(" Doctorate" , " Masters", " Prof-school"), "Grad-Deg",
                                                        ifelse(unique_alldata$edu_level %in% c(" Preschool"), "Preschool",
                                                               ifelse(unique_alldata$edu_level %in% c(" Bachelors"), "Bachelors",
                                                                      ifelse(unique_alldata$edu_level %in% c(" Some-college"), "Some-college",
                                                                             ifelse(unique_alldata$edu_level %in% c(" 1st-4th"), "Primary-Sch", "NA"))))))))
#Native Country Column Check
country_numbers <- unique_alldata %>% count(native_country)
unique_alldata$native_country[unique_alldata$native_country == " Holand-Netherlands"]  <-  NA 


# Setting bins for age column
#unique_alldata$age <- ifelse(unique_alldata$age < 23, "17-22",
#                                   ifelse(unique_alldata$age < 29, "23-28",
#                                          ifelse(unique_alldata$age < 35, "29-34",
#                                                 ifelse(unique_alldata$age < 41, "35-40",
#                                                        ifelse(unique_alldata$age < 47, "41-46",
#                                                               ifelse(unique_alldata$age < 53, "47-52",
#                                                                      ifelse(unique_alldata$age < 59, "53-58",
#                                                                             ifelse(unique_alldata$age < 65, "59-64", "Senior"))))))))
#work_status column 
unique_alldata$work_status[unique_alldata$work_status == " Never-worked"] <- " Without-pay"


#Drop the columns that we will not use// edu_num and final_weight
#Assessing which variables are useful 
## Edu_num and final_weight variables will be eliminated because edu_num is a numerical
##value of edu_level column, final_weight is not related to analysis
sub_unique <- select(unique_alldata, -c(3,5))
summary(sub_unique)

#Checking the unique values 
sapply(sub_unique, function(x)length(table(x)))
unique(sub_unique$age)
unique(sub_unique$work_status)
unique(sub_unique$edu_level)
unique(sub_unique$marital_status)
unique(sub_unique$occupation)
unique(sub_unique$relationship)
unique(sub_unique$race)
unique(sub_unique$sex)
unique(sub_unique$capital_gain)
unique(sub_unique$capital_loss)
unique(sub_unique$hours_per_week)
unique(sub_unique$native_country)

#Checking Null Values in subset data
colSums(is.na(sub_unique))


#Imputing the missing values for work_status, native_country, and occupation 
#We will impute them with the most frequent ones

sub_unique$work_status[is.na(sub_unique$work_status)] <- " Private"
sub_unique$native_country[is.na(sub_unique$native_country)] <- " United-States"
sub_unique$occupation[is.na(sub_unique$occupation)]<- " Prof-specialty"
colSums(is.na(sub_unique))

#Visualizations 

#Histograms of Categorical Variables 
ggplot(sub_unique, aes(response)) + geom_histogram(stat = "count")
ggplot(sub_unique, aes(sex)) + geom_histogram(stat = "count")
ggplot(sub_unique, aes(race)) + geom_histogram(stat = "count")
ggplot(sub_unique, aes(native_country)) + geom_histogram(stat = "count") + coord_flip()
ggplot(sub_unique, aes(occupation)) + geom_histogram(stat = "count")+ coord_flip()
ggplot(sub_unique, aes(marital_status)) + geom_histogram(stat = "count")+coord_flip()
ggplot(sub_unique, aes(edu_level)) + geom_histogram(stat = "count")+coord_flip()
ggplot(sub_unique, aes(work_status)) + geom_histogram(stat = "count")+coord_flip()
ggplot(sub_unique, aes(relationship)) + geom_histogram(stat = "count")+coord_flip()


#Histograms of Numerical Variables 
sub_unique %>% ggplot(aes(age)) + geom_histogram(binwidth = 5)
ggplot(sub_unique, aes(capital_gain, fill = response)) + geom_histogram(binwidth = 15000, position = "dodge")
ggplot(sub_unique, aes(capital_loss, fill = response)) + geom_histogram(binwidth = 400, position = "dodge")
ggplot(sub_unique, aes(hours_per_week)) + geom_histogram(binwidth = 10)


#Plots overlapping 
ggplot(sub_unique, aes(sex, fill = response)) + geom_histogram(stat = "count", position = 'dodge')
ggplot(sub_unique, aes(work_status, fill = response)) + geom_histogram(stat = "count", position = 'dodge')+
  coord_flip()
ggplot(sub_unique, aes(occupation, fill = response)) + geom_histogram(stat = "count", position = 'dodge')+
  coord_flip()
ggplot(sub_unique, aes(age, fill = response)) + geom_histogram(binwidth = 5, position = 'dodge')
  

#BoxPlot
ggplot(sub_unique, aes(response, age)) + geom_boxplot() 
ggplot(sub_unique, aes(response, hours_per_week)) + geom_boxplot() 
#Fixing Race Column as White and Others
sub_unique$race <- ifelse(sub_unique$race==" White", "White", "Other")


#Correlations among variables/// Continuous variables are nit correlated to each other
corMat = cor(sub_unique[, c("age", "capital_loss", "capital_gain", "hours_per_week")])
corMat


#Converting chr variables to factors  
model_data <- sub_unique
unique(model_data$age)
unique(model_data$edu_level)



#model_data$age <- factor(model_data$age, ordered = TRUE, levels = c("17-22","23-28","29-34","35-40","41-46","47-52","53-58","59-64","Senior"))
model_data$work_status <- factor(model_data$work_status, ordered = FALSE)
model_data$edu_level <- factor(model_data$edu_level, ordered = TRUE, levels = c("Preschool", "Primary-Sch", 
                                                                                "Middle-School","HS-Grad","Some-college",
                                                                                "Assc-Deg" , "Bachelors", "Grad-Deg"))         

                                                                               
model_data$marital_status <- factor(model_data$marital_status, ordered = FALSE)
model_data$occupation <- factor(model_data$occupation, ordered = FALSE)
model_data$relationship <- factor(model_data$relationship, ordered = FALSE)
model_data$race <- factor(model_data$race, ordered = FALSE)
model_data$sex <- factor(model_data$sex, ordered = FALSE)
model_data$native_country <- factor(model_data$native_country, ordered = FALSE)
model_data$response <- factor(model_data$response, ordered = TRUE, levels = c(0,1))
summary(model_data)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(model_data$response, SplitRatio = 0.70)
training_set = subset(model_data, split == TRUE)
test_set = subset(model_data, split == FALSE)

##Applying Feature Scaling to the numeric variables
training_set[c(1,9,10,11)] = scale(training_set[c(1,9,10,11)])
test_set[c(1,9,10,11)] = scale(test_set[c(1,9,10,11)])


# Fitting Logistic Regression to the Training set

classifier.null = glm(response ~ 1,
                 family = binomial,
                 data = training_set)
summary(classifier.null)

classifier = glm(response ~ age + work_status + edu_level + marital_status +
                   occupation + relationship + race + sex  + capital_gain +
                   capital_loss + hours_per_week + native_country,
                 family = binomial,
                 data = training_set)
summary(classifier)


# We can see that AIC Value dropped from 37599 to 21569

#Pedicting the test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-13])
summary(prob_pred)

y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[, 13], y_pred > 0.5)
cm
accuracy <- (10373+2062)/14637
accuracy # Model accuracy is %84.9

#Roc curve for training set
prob_pred_train = predict(classifier, type = 'response', newdata = training_set[-13])
ROCRpred <- prediction(prob_pred_train, training_set$response)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Roc curve for test set
ROCpredTest <- prediction(prob_pred, test_set$response)
ROCRperfTest = performance(ROCpredTest, "tpr", "fpr")
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Auc_TrainingSet
auc.txt <- performance(ROCRpred, "auc")
auc <- as.numeric(auc.txt@y.values)

#Auc Test_Set
auc.tst <- performance(ROCpredTest, "auc")
auc.test <- as.numeric(auc.tst@y.values)



