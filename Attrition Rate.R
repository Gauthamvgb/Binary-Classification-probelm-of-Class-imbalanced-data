path <- "C:/Users/7000320/Desktop/Backup IT011/Pijush Resources/Allsec Attrition rate"
setwd(path)

#load libraries
library(data.table)
library(eeptools)
library(tibble)
library(ggplot2)
library(caret)
library(lmtest)


#load Data
Test_1<- read.csv(file.choose(),header= TRUE,na.strings =c("",NA),stringsAsFactors = FALSE)
dim(Test_1)
summary(Test_1)
str(Test_1)

##Getting rid off blank columns
##Test_2 <-Test_1[,1:13]
##dim(Test_2)
##summary(Test_2)

table(duplicated(Test_1$Employee.ID))
which(duplicated(Test_1$Employee.ID))
Test_1 <- Test_1[!duplicated(Test_1$Employee.ID),]
## NA value Information/status
NA_Value <- table(is.na(Test_1))
NA_Value_percent <- sapply(Test_1, function(x) sum(is.na(x))/length(x))*100
NA_Value
NA_Value_percent
##Band Change is having 99% NA values, discarding them as well
Test_2 <- subset(Test_1,select = -c(12))

## Managing Gender Information
as.factor(Test_2$Gender)
Test_2$Gender <- sapply(Test_2$Gender,function(x) suppressWarnings(levels(x)<-sub("NULL", NA, x)))
table(Test_2$Gender)
table(is.na(Test_2$Gender)) ## checking NA value status
Test_2$Gender[is.na(Test_2$Gender)] <- 'F'

##noquote(paste(Test_3$Gender,collapse = ", "))
##Managing Mariatl Status
Test_2$Marital.Status[Test_2$Marital.Status == 0] <- 'Married'
Test_2$Marital.Status[Test_2$Marital.Status == 'Divorcee'] <- 'Married'
Test_2$Marital.Status[Test_2$Marital.Status == 'Divorced'] <- 'Married'
Test_2$Marital.Status[is.na(Test_2$Marital.Status)] <- 'Married'
Test_2$Marital.Status[Test_2$Marital.Status == 'SELF'] <- 'Married'
Test_2$Marital.Status[Test_2$Marital.Status == 'Unmarried'] <- 'Single'

table(Test_2$Gender, Test_2$Marital.Status)
summary(Test_2)

## Working wih Date 
DOB<- as.Date(Test_2$D.O.B,
              format = "%d-%B-%y")
DOB

correct_DOB <- as.Date(ifelse(DOB> Sys.Date(), 
                                 format(DOB, "19%y-%m-%d"), 
                                 format(DOB)))
View(correct_DOB)
summary(correct_DOB)

which(is.na(correct_DOB))

##Integrating with existing dataset
Test_3<- data.frame(Test_2,correct_DOB)
Test_4 <- Test_3[-c(464,6497,6499,6501,6503,6505,6507,6509,6511,6513,6515,6517,6519,6521,6523,6525,6527),]
## Age Calculation
##Test5$Age <- age_calc(Test5$correct_DOB,enddate = Sys.Date(),units = "years")

Age <- age_calc(na.omit(Test_4$correct_DOB), units = "years")
Age
Final_Age <-round(Age,digits = 0)
View(Final_Age)

##Test5$Age[!is.na(Test5$correct_DOB)] <- ages
##Integrating with Dataframe
Test_5 <-data.frame(Test_4,Final_Age)

##Making New Variable
##Resigned = ifelse(is.na(Test_5$Termination.type..voluntary.or.involuntary), 0 , 1) ## 0 <- Retained, 1 <- terminated/resigned

##Retained1 <- Test_7[which(Test_7$Resigned == 0),]
##Terminated1 <- Test_7[which(Test_7$Resigned == 1),]
##Incorporating with existing dataframe
Test_6 <- data.frame(Test_5,Resigned)
Test_7 <- subset(Test_6,select = -c(4,12,13))

## NA Value Treatment
NA_Value <- table(is.na(Test_7))
NA_Value_percent <- sapply(Test_7, function(x) sum(is.na(x))/length(x))*100
NA_Value
NA_Value_percent

##"Supporting market" is having 14% of Missing value, assuming this as Customer care executive we're imputing this.
Test_7$Supporting.market[is.na(Test_7$Supporting.market)] <- 'Customer Care Executive'

## "highest Educational qualification is having 4.5 % of missing value, assuming the minimum education needed to join is HS, we're imputing this.
Test_7$Highest.Educational.Qualification[is.na(Test_7$Highest.Educational.Qualification)] <- 'HSC'

## Featue engineering 2
## Managing Overall Experience Data into binary class.

summary(as.factor(Test_7$Overall.Experience))
str(Test_7$Overall.Experience)
## as.numeric(Test_7$Overall.Experience)
Test_7$Overall.Experience[is.na(Test_7$Overall.Experience)] <- 2 ## mean(na.omit(as.numeric(Test_7$Overall.Experience)))= 1.72 Years (imputing that, as round)
Test_7$Overall.Experience[Test_7$Overall.Experience == 'RT Nagar'] <- 0
Test_7$Overall.Experience[Test_7$Overall.Experience == 'Nil'] <- 0
Test_7$Overall.Experience[Test_7$Overall.Experience == '0 Yrs'] <- 0
Test_7$Overall.Experience[Test_7$Overall.Experience == '1 Yrs'] <- 1
Test_7$Overall.Experience[Test_7$Overall.Experience == '2 Yrs'] <- 2
Test_7$Overall.Experience[Test_7$Overall.Experience == '3 Yrs'] <- 3
Test_7$Overall.Experience[Test_7$Overall.Experience == '4 Yrs'] <- 4
Test_7$Overall.Experience[Test_7$Overall.Experience == '5 Yrs'] <- 5
Test_7$Overall.Experience[Test_7$Overall.Experience == '6 Yrs'] <- 6
Test_7$Overall.Experience[Test_7$Overall.Experience == '7 Yrs'] <- 7
Test_7$Overall.Experience[Test_7$Overall.Experience == '8 Yrs'] <- 8
Test_7$Overall.Experience[Test_7$Overall.Experience == '9 Yrs'] <- 9
Test_7$Overall.Experience[Test_7$Overall.Experience == '10 Yrs'] <- 10
Test_7$Overall.Experience[Test_7$Overall.Experience == '11 Yrs'] <- 11
Test_7$Overall.Experience[Test_7$Overall.Experience == '12 Yrs'] <- 12
Test_7$Overall.Experience[Test_7$Overall.Experience == '13 Yrs'] <- 13
Test_7$Overall.Experience[Test_7$Overall.Experience == '14 Yrs'] <- 14
Test_7$Overall.Experience[Test_7$Overall.Experience == '15 Yrs'] <- 15
Test_7$Overall.Experience[Test_7$Overall.Experience == '19 Yrs'] <- 19
Test_7$Overall.Experience <-as.factor(Test_7$Overall.Experience)
str(Test_7$Overall.Experience)
summary(Test_7$Overall.Experience)

## Managing length of Service into binary class
Test_7$Length.of.Service[Test_7$Length.of.Service == '0 Yrs'] <- 0
Test_7$Length.of.Service[Test_7$Length.of.Service == '1 Yrs'] <- 1
Test_7$Length.of.Service[Test_7$Length.of.Service == '2 Yrs'] <- 2
Test_7$Length.of.Service[Test_7$Length.of.Service == '3 Yrs'] <- 3
Test_7$Length.of.Service[Test_7$Length.of.Service == '4 Yrs'] <- 4
Test_7$Length.of.Service[Test_7$Length.of.Service == '5 Yrs'] <- 5
Test_7$Length.of.Service[Test_7$Length.of.Service == '6 Yrs'] <- 6
Test_7$Length.of.Service[Test_7$Length.of.Service == '7 Yrs'] <- 7
Test_7$Length.of.Service[Test_7$Length.of.Service == '8 Yrs'] <- 8
Test_7$Length.of.Service[Test_7$Length.of.Service == '9 Yrs'] <- 9
Test_7$Length.of.Service[Test_7$Length.of.Service == '10 Yrs'] <- 10
Test_7$Length.of.Service[Test_7$Length.of.Service == '11 Yrs'] <- 11
Test_7$Length.of.Service[Test_7$Length.of.Service == '12 Yrs'] <- 12
Test_7$Length.of.Service[Test_7$Length.of.Service == '13 Yrs'] <- 13
Test_7$Length.of.Service[Test_7$Length.of.Service == '14 Yrs'] <- 14
Test_7$Length.of.Service[Test_7$Length.of.Service == '15 Yrs'] <- 15
Test_7$Length.of.Service[Test_7$Length.of.Service == '16 Yrs'] <- 16
Test_7$Length.of.Service[Test_7$Length.of.Service == '17 Yrs'] <- 17
Test_7$Length.of.Service[Test_7$Length.of.Service == '18 Yrs'] <- 18
Test_7$Length.of.Service[is.na(Test_7$Length.of.Service)] <- 1 ## mean(na.omit(as.numeric(Test_7$Length.of.Service)))

summary(Test_7$Length.of.Service)
Test_7$Length.of.Service <- as.factor(Test_7$Length.of.Service)
summary(Test_7$Length.of.Service)

## Checking correlation between Length of service and Overall Experience.
cor.test(as.numeric(Test_7$Length.of.Service),as.numeric(Test_7$Overall.Experience)) ## 19.55% of correlation which means these two attributes doesn't have same impact on target variable.

str(Test_7$City.Origin)
summary(Test_7$City.Origin)
##Test_7$City.Origin[is.na(Test_7$City.Origin)] <- "Chennai"
##Location <- ifelse(Test_7$City.Origin %in% "Chennai", "Chennai",
##                   ifelse(Test_7$City.Origin %in% "CHENNAI","Chennai",
##                          ifelse(Test_7$City.Origin %in% "chennai","Chennai",
##                                 ifelse(Test_7$City.Origin %in% "New Delhi","Delhi",
##                                          ifelse(Test_7$City.Origin %in% "Delhi","Delhi",
##                                              ifelse(Test_7$City.Origin %in% "Bangalore","Bangalore",
##                                                  ifelse(Test_7$City.Origin %in% "new delhi","Delhi",
##                                                      ifelse(Test_7$City.Origin %in% "delhi","Delhi",
##                                                             ifelse(Test_7$City.Origin %in% "Trichy","Chennai",0)))))))))


## table(Location)

## Need to process NA value field on location.

## Managing Heighest educational qualification
table(Test_7$Highest.Educational.Qualification)
Test_7$Highest.Educational.Qualification [Test_7$Highest.Educational.Qualification == "Dinnur Main Road"] <- 'Diploma'
table(is.na(Test_7$Highest.Educational.Qualification))


## Managing Departemnt/Technology
table(Test_7$Department.Technology)


## Managing Job Title
table(Test_7$Job.title)
table(is.na(Test_7$Job.title))
Test_7$Job.title[is.na(Test_7$Job.title)] <- "Customer Care Executive"

## checking the employee attrition information
## importaing that information

P1 <- read.csv(file.choose(),stringsAsFactors = FALSE, header = TRUE)
P1$A[P1$A == "i"] <- 'I'
table(as.factor(P1$A))
colnames(P1)[1] <- "Employee.ID"
colnames(P1)[2] <- "Resigned"
Dataframe_11 <- merge(Test_7,P1,by = "Employee.ID")

##After this point new coding has been done##

## Importing Salary data 
Salary <-  read.csv(file.choose(),header = TRUE,na.strings =c("","NA"))
dim(Salary)
sort(Salary$EMPLOYEE_CODE, decreasing = F)
View(Salary)
str(Salary)
summary(Salary)

## Importing old database 

Old_database <- read.csv(file.choose(), header = T, na.strings =c("","NA"),sep = "\t")
dim(Old_database)
## Integrating with Primary dataset.

##Salary1 <- ifelse(as.numeric(Test_7$Employee.ID) == as.numeric(Salary$EMPLOYEE_CODE), as.numeric(Salary$Gross_pay), 
##                  ifelse(as.numeric(Test_7$Employee.ID) == as.numeric(Old_database$Empcode), as.numeric(Salary$Gross_pay),NA))
colnames(Salary)[1] <- "Employee.ID"
colnames(Old_database)[1] <- "Employee.ID"

Dataframe1 <- merge(Dataframe_11,Salary, by= "Employee.ID")
table(is.na(Dataframe1$Gross_pay))

## Binning the salary data
## X <- Salary$Gross_pay
## y <- 200000
## Sal_bin <- binning(counts = X, breaks = y)
##Test_8$Salary1[is.na(Test_8$Salary1)] <- mean(na.omit(Test_8$Salary1)) ## Imputation with Mean/Median/central tendencies
##table(is.na(Test_8$Salary1))

Dataframe1$Sal_bin <- cut(Dataframe1$Gross_pay, breaks = c(0, seq(200000.0, 1400000.0, by = 200000.0)), labels = 0:6)
table(is.na(Dataframe1$Sal_bin))
Dataframe1$Sal_bin[is.na(Dataframe1$Sal_bin)] <- 6
Test_8 <- subset(Dataframe1,select = -c(12,14,15))
## need to work.
## Check NA values from entire Dataset

table(is.na(Test_8))
NA_Value_percent1 <- sapply(Test_8, function(x) sum(is.na(x))/length(x))*100
NA_Value_percent1

## Incorporating unit location data from old database

Region_1 <- ifelse(Test_8$Employee.ID %in% Old_database$Employee.ID, as.character(Old_database$Unit_Name),
            ifelse(Test_8$Employee.ID %in% Salary$Employee.ID,as.character(Old_database$Unit_Name), NA))
table(is.na((Region_1)))
table(Test_Final$Region_1)
Test_Final <- data.frame(Test_8,Region_1)
Test_Final$Region_1[Test_Final$Region_1 == "Manila"] <- 'Chennai'
Test_Final$Region_1[Test_Final$Region_1 == "Dallas"] <- 'Chennai'
## Test_Final$Region_1[is.na(Test_Final$Region_1)] <- "Chennai"

Test_Final_1 <- Test_Final[c(-4)]

table(is.na(Test_Final_1))
na.omit(Test_Final_1)
summary(Test_Final_1)
str(Test_Final_1)

##Test_clean <- na.omit(Test_8)
##Final_data <- data.frame(Test_clean,Region_clean)
##Region2

## Now data is ready for visualization and other modeling purpose

table(Test_Final_1$Gender,Test_Final_1$Marital.Status)
table(Test_Final_1$Highest.Educational.Qualification,Test_Final_1$Marital.Status)
table(Test_Final_1$Highest.Educational.Qualification,Test_Final_1$Gender)
table(Test_Final_1$Overall.Experience,Test_Final_1$Gender)
table(Test_Final_1$Overall.Experience,Test_Final_1$Marital.Status)
table(Test_Final_1$Region_1,Test_Final_1$Sal_bin)
table(Test_Final_1$Resigned.y,Test_Final_1$Region_1)
table(Test_Final_1$Resigned.y,Test_Final_1$Sal_bin)
table(Test_Final_1$Highest.Educational.Qualification,Test_Final_1$Resigned)
table(Test_Final_1$Overall.Experience,Test_Final_1$Resigned.y)
table(Test_Final_1$Length.of.Service,Test_Final_1$Resigned.y)
table(Test_Final_1$Length.of.Service,Test_Final_1$Sal_bin)
table(Test_Final_1$Overall.Experience,Test_Final_1$Sal_bin)

##writing csv file.
write.csv(Dataframe_1,file = "Dataframe_1.csv")
Test.data_3 <- as.tibble(fread('C:/Users/7000320/Desktop/Backup IT011/Pijush Resources/Allsec Attrition rate/Dataframe_1.csv', nrows= 6000))
write.csv(Test.data_3, "Test.data_3.csv")

## Visualization

ggplot(Dataframe111) +
  geom_point(aes(Sal_bin1, Marital.Status)) +
  geom_point(data = Dataframe111, aes(Sal_bin1, mean(as.numeric(Overall.Experience))), colour = 'red', size = 3)


Test_Final_1$Overall.Experience <- factor(Test_Final_1$Overall.Experience, levels = c(1,2,3),
                      labels=c("1Year","2Year","3Year")) 
Test_Final_1$Resigned <- factor(Test_Final_1$Resigned, levels = c(0,1),
                    labels=c("Retained","Resigned")) 
Test_Final_1$Marital.Status <- factor(Test_Final_1$Marital.Status, levels = c('M','F'),
                     labels=c("Male","Female")) 


# grouped by Length of Service (indicated by color)
qplot(Overall.Experience, data=Dataframe111, geom="density", fill= Overall.Experience, alpha=I(.5), 
      main="Density curve Overall Service", xlab=" Overall Service", 
      ylab="Density")

table(Test_Final_1$Overall.Experience)
qplot(Gender, data=Dataframe111, geom="density", fill= Gender, alpha=I(.5), 
      main="Desity Curve Gender vs Resignee", xlab="Resigned", 
      ylab="Density")

qplot(Length.of.Service, data=Test_Final_1, geom="density", fill= Resigned, alpha=I(.5), 
      main="Desity Curve Gender vs Overall Experience", xlab="Resigned", 
      ylab="Density")

qplot(Final_Age, data=Dataframe111, geom="density", fill= Sal_bin1, alpha=I(.5), 
      main="Desity Curve Age vs Salary", xlab="Age", 
      ylab="Density")

qplot(Final_Age, data=Dataframe111, geom="density", fill= Gender, alpha=I(.5), 
      main="Desity Curve Age vs Gender", xlab="Age", 
      ylab="Density")

qplot(Final_Age, data=Dataframe111, geom="density", fill= Marital.Status, alpha=I(.5), 
      main="Desity Curve Age vs Marital Stat", xlab="Age", 
      ylab="Density")

qplot(Region_1, data=Test_Final_1, geom="density", fill= as.numeric(Length.of.Service), alpha=I(.5), 
      main="Desity Curve Region vs Length of Service", xlab="Region", 
      ylab="Density")

qplot(as.numeric(Length.of.Service), data=Test_Final_1, geom="density", fill= Region_1, alpha=I(.5), 
      main="Desity Curve Region vs Length of Service", xlab="Length.of.Service", 
      ylab="Density")

qplot(Gross_pay, data=Dataframe1, geom="density", fill= Resigned.y, alpha=I(.5), 
      main="Desity Curve Resigned vs Gross Pay", xlab="Gross_pay", 
      ylab="Density")

##Histogram of age distribution
qplot(Resigned.y, Final_Age, data=Dataframe111, geom=c("boxplot"), 
      fill=Gender, main="Resigned vs Final Age",
      xlab="Resigned.y", ylab="Age")

qplot(Gender, Final_Age, data=Dataframe111, geom=c("boxplot"), 
      fill=Gender, main="Gender vs Final Age",
      xlab="Gender", ylab="Age")

qplot(Marital.Status, Final_Age, data=Dataframe1, geom=c("boxplot"), 
      fill=Gender, main="Marstat vs Final Age",
      xlab="Marstat", ylab="Age")

qplot(Resigned.y, as.numeric(Overall.Experience), data=Dataframe1, geom=c("boxplot"), 
      fill=Gender, main="Marstat vs Final Age",
      xlab="Marstat", ylab="Age")

qplot(Final_Age, Gross_pay, data=Dataframe1, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=Gender, 
      main="Regression of age and Salary", 
      xlab="Age", ylab="Salary")

qplot(as.numeric(Length.of.Service), Gross_pay, data=Dataframe1, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=Region_1, 
      main="Regression of Experience and Salary", 
      xlab="Length of Service", ylab="Salary")

qplot(Gross_pay, Resigned.y, data=Dataframe111, geom=c("point", "smooth"), 
      method="glm", formula=y~x, color=Region_1, 
      main="Regression of Experience and Salary", 
      xlab="Length of Service", ylab="Salary")

time_series <- data.frame(Dataframe1,Region_1)
##table(Dataframe1$Region_1)

## Trend Analysis
Trend <- ggplot(time_series,aes(x= Year_aa , y= log(Gross_pay)))
Trend + geom_line(aes(color= Region_1))

Trend <- ggplot(time_series,aes(x= Year_aa, y= Gross_pay))
Trend + geom_line(aes(color= Resigned))

##Histograms.

ggplot(time_series,aes(x = Gross_pay)) + geom_histogram()
ggplot(time_series,aes(x = as.numeric(Sal_bin))) + geom_histogram()
ggplot(time_series,aes(x = as.numeric(Overall.Experience))) + geom_histogram()
ggplot(time_series,aes(x = as.numeric(Length.of.Service))) + geom_histogram()
ggplot(time_series,aes(x = Final_Age)) + geom_histogram()


##Scatterplot

ggplot(time_series, aes(y= Gross_pay, x= Overall.Experience)) +
  geom_point()

ggplot(time_series, aes(y= Gross_pay, x= as.numeric(Length.of.Service))) +
  geom_point()

ggplot(time_series, aes(y= Gross_pay, x=Final_Age )) +
  geom_point()

ggplot(Dataframe1, aes(y= Gross_pay, x=Resigned.y )) +
  geom_point()

##Salary with Binwidth

T1 <- ggplot(time_series, aes(x= Gross_pay)) 
T1 + geom_histogram()

T1 + geom_histogram(stat = "bin",binwidth = 1000000)

##Regression Line

time_series$pred <- predict(lm(Overall.Experience ~ Gross_pay, data = time_series))
line1 <- ggplot(time_series, aes(y= Gross_pay, x= Overall.Experience))
line1 + geom_point(aes(color = Region_1)) + geom_line(aes(y= pred))


##Text Plot
line1 + geom_text(aes(label=Region_1) , size =3 )

## Boxplot
boxplot(Final_Age ~ Overall.Experience, data = Test_Final_1, main = "Salary Vs Experience",
        xlab = "Experience", ylab= "Age")

boxplot(Gross_pay ~ Overall.Experience, data = time_series, main = "Salary Vs Experience",
        xlab = "Experience", ylab= "Age")

boxplot(Gross_pay ~ Length.of.Service, data = time_series, main = "Salary Vs Experience",
        xlab = "Experience", ylab= "salary")

boxplot(Length.of.Service ~ Overall.Experience, data = Test_Final_1, main = "Salary Vs Experience",
        xlab = "Experience", ylab= "Age")


p <- ggplot(Test_Final_1, aes(Region_1, Final_Age))
p + geom_boxplot(fill = "pink", colour = "#3366FF")

p1 <- ggplot(Test_Final_1, aes(Region_1, as.numeric(Sal_bin)))
p1 + geom_boxplot(fill = "yellow", colour = "#3366FF")

p2 <- ggplot(time_series, aes(Region_1, Gross_pay))
p2 + geom_boxplot(fill = "yellow", colour = "#3366FF")+ 
  scale_y_continuous(limits = quantile(time_series$Gross_pay, c(0.1, 0.9)))

## Violin chart

p3 <- ggplot(time_series, aes(Region_1, Gross_pay))
p3+ geom_violin(fill = "pink", colour = "#3366FF", scale = "area") +
  scale_y_continuous(limits = quantile(time_series$Gross_pay, c(0.1, 0.9)))

#### Model Validation


## test and train
##ID doesn't have any impact on outcome, removing that from dataset
Test_Final_1 <- Test_Final_1[-1]
Test_Final_1$Resigned.y <-  ifelse(Test_Final_1$Resigned.y == 'A',1,0)
table(Test_Final_1$Resigned.y)
Data <- sort(sample(nrow(Test_Final_1), nrow(Test_Final_1)* .7))
TRAIN <- Test_Final_1[Data,]
TEST <- Test_Final_1[-Data,]


## Model building
## Classification and Regreesion tree
library(rpart)
library(rpart.plot)
library(rattle)
## Cross-Validation
##Gender = as.numeric(as.factor(Test))
control = trainControl(method="repeatedcv", number=10, repeats=3)
model = train(Resigned.y ~., data=train_nn, method="rpart", preProcess="scale", trControl=control)
summary(model)
tree_fit <- rpart(Resigned.y ~ Gender+ Marital.Status + Highest.Educational.Qualification + Overall.Experience + Length.of.Service +
                   Final_Age + Sal_bin + Region_1
                  , data = train_nn, method = "class", na.action = na.pass,
                  control = rpart.control(minbucket = 4, cp = 0.008))
rpart.plot(tree_fit)
fancyRpartPlot(tree_fit)

prediction1 <- predict(tree_fit,test1_nn, type = "class")
confusionMatrix(test1_nn$Resigned.y,prediction1)

## Modified Decision Tree
tree_fit1 <- rpart(Resigned.y ~., data = train_nn, method = "class",
                   control = list(minsplit = 20, 
                                  minbucket = 7, cp = 0.008, maxcompete = 4, maxsurrogate = 5, 
                                  usesurrogate = 2, surrogatestyle = 0, maxdepth = 30, xval = 0))

rpart.plot(tree_fit1)
fancyRpartPlot(tree_fit1)
prediction_modified <- predict(tree_fit1,test1_nn, type = "class")
confusionMatrix(test1_nn$Resigned.y,prediction_modified)

## Regression Model.
Reg_fit <- glm(Resigned.y ~., family = binomial(link = "logit"), data = train_nn, control = list(maxit = 50))
summary(Reg_fit)

Regression_fit <- glm(Resigned.y ~ Gender + Highest.Educational.Qualification + Overall.Experience + Length.of.Service +
                       Department.Technology + Job.title + Sal_bin + Region_1 , family = binomial(link = "logit"),
                      data = train_nn)

summary(Regression_fit)

Regression_fit_1 <- glm(Resigned.y ~  Gender + Overall.Experience + Length.of.Service +
                          Job.title + Sal_bin + Region_1 , family = binomial(link = "logit"),
                        data = train_nn)
summary(Regression_fit_1)
anova(Regression_fit_1, test = "Chisq")

Reg_predict <- predict(Regression_fit_1, newdata = subset(test1_nn,select = c(1,4,6,7,11,12)), type = 'response')

Reg_predict <- ifelse(Reg_predict > 0.5,1,0)

## Regression Accuracy

library(ROCR)
library(Metrics)
library(caTools)
library(rattle)
confusionMatrix(test1_nn$Resigned.y,Reg_predict)
P2 <- predict(Regression_fit_1, newdata = subset(test1_nn,select = c(1,4,6,7,11,12)), type = 'response')
Prediction2 <- prediction(P2,test1_nn$Resigned.y)
perf <- performance(Prediction2, measure = "tpr", x.measure = "fpr")
plot(perf)
auc(test1_nn$Resigned.y,Reg_predict)

##Since cannot test model's performacne locally on test data using train data instead.

##split <- createDataPartition( y = train1$Resigned , p = 0.7 , list = FALSE)
##new_train <- train1[split]
##new_test <- train1[-split]
##new_test
##Regression_fit1 <- glm(Resigned ~ Gender + Marital.Status + Highest.Educational.Qualification + Length.of.Service +
##                         Final_Age + Sal_bin + Region_1, family = binomial(link = "logit"),
##                       data = new_train[-c(4,5,7,8)])

##Neural Network

##Scaling the data with Min-Max Normalization

Test_nn <- Test_Final_1
Test_nn$Gender <- as.numeric(as.factor(Test_nn$Gender))
Test_nn$Marital.Status <-as.numeric(as.factor(Test_nn$Marital.Status))
Test_nn$Highest.Educational.Qualification<- as.numeric(as.factor(Test_nn$Highest.Educational.Qualification))
Test_nn$Overall.Experience<-as.numeric(as.factor(Test_nn$Overall.Experience))
Test_nn$Department.Technology <- as.numeric(as.factor(Test_nn$Department.Technology))
Test_nn$Length.of.Service <- as.numeric(as.factor(Test_nn$Length.of.Service))
Test_nn$Job.title <- as.numeric(as.factor(Test_nn$Job.title))
Test_nn$Supporting.market <- as.numeric(as.factor(Test_nn$Supporting.market))
Test_nn$Sal_bin <- as.numeric(as.factor(Test_nn$Sal_bin))
Test_nn$Region_1 <- as.numeric(as.factor(Test_nn$Region_1))

##Scaling the Dataframe
index = sample()
max = apply(Test_nn, 2, max)
max
min = apply(Test_nn, 2, min)
scaled = as.data.frame(scale(Test_nn, center = min, scale = max - min))

Data1 <- sort(sample(nrow(Test_nn), nrow(Test_nn)* .7))
train_nn <- Test_nn[Data1,]
test1_nn <- Test_nn[-Data1,]

library(neuralnet)

set.seed(2)
NN = neuralnet(Resigned.y ~ Gender + Marital.Status + Highest.Educational.Qualification + Overall.Experience+ Department.Technology +
                Length.of.Service +Sal_bin + Region_1, train_nn, hidden = 2,
                linear.output = TRUE )
NN
plot(NN)

NN1 = neuralnet(Resigned.y ~,train_nn, hidden = 2,
               linear.output = TRUE )


NN1 <- nnet(Resigned.y ~., data = train_nn, size = 3, rang = 0.1, decay = 0.1, maxit = 20)

Predict2 <- predict(NN1,test1_nn, type = "raw")
Predict2 <- ifelse(Predict2 > 0.3,1,0)
confusionMatrix(Predict2, test1_nn$Resigned.y)

##Crossvalidation
install.packages("boot")
install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    train_nn = scaled[index,]
    test1_nn = scaled[-index,]
    datatest = data[-index,]
    
    NN_cross = neuralnet(Resigned.y ~ Gender + Marital.Status + Highest.Educational.Qualification + Overall.Experience+ Department.Technology +
                     Length.of.Service +Sal_bin + Region_1, train_nn, hidden = 2, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)


## Random Forest and XGBOOST model.

Random_forest_fit <- randomForest(Resigned.y ~., data = test1_nn, ntree= 200,
                                  importance = FALSE, na.action = na.omit)


print(Random_forest_fit)

Random_forest_fit1 <- randomForest(Resigned.y ~., data = test1_nn, ntree= 500,
                                  importance = TRUE, mtry= 11, na.action = na.omit)
print(Random_forest_fit1)

plot(Random_forest_fit1)
importance(Random_forest_fit1, type = 1)
varImpPlot(Random_forest_fit1)

Pred_rf <- predict(Random_forest_fit1, type = "response")
Pred_rf <- ifelse(Pred_rf > 0.5,1,0)
confusionMatrix(Pred_rf, test1_nn$Resigned.y)
str(test1_nn$Resigned.y)

## SVM
library(e1071)

## Crossvalidation
tuned = tune.svm(Resigned.y ~., data = train_nn, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=5))
summary(tuned)
obj <- tune(svm, Resigned.y~., data = train_nn, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))
summary(obj)

svm_fit <- svm(Resigned.y ~., data = train_nn, kernel= "radial", cost = 4, gamma = .05 )
summary(svm_fit)
pred_svm <- predict(svm_fit, data= test1_nn)
table(pred_svm1)
pred_svm2 <- predict(svm_fit, data= train_nn)
pred_svm2 <- ifelse(pred_svm2 > 0.5,1,0)
confusionMatrix(train_nn$Resigned.y,pred_svm2)
confusionMatrix(test1_nn$Resigned.y,pred_svm2)
points(train_nn$Resigned.y, pred_svm1, col= 'blue', pch = 4)

##pred_svm2_f <- ifelse(pred_svm2> 0.5,1,0)
##confusionMatrix(test1_nn$Resigned.y,pred_svm2_f)
##tune.control(random = FALSE, nrepeat = 2, repeat.aggregate = mean,
##             sampling = c("cross", "fix", "bootstrap"), sampling.aggregate = mean,
##             sampling.dispersion = sd,
##             cross = 10, fix = 2/3, nboot = 10, boot.size = 9/10, best.model = TRUE,
##             performances = TRUE, error.fun = NULL)

tune.svm(x, y = NULL, data = NULL, degree = NULL, gamma = NULL, coef0 = NULL,
         cost = NULL, nu = NULL, class.weights = NULL, epsilon = NULL, ...)
best.svm(x, tunecontrol = tune.control(), ...)





ggplot(Dataframe1, aes(x = Marital.Status , y = Gross_pay, color = )) + geom_boxplot()  

  









