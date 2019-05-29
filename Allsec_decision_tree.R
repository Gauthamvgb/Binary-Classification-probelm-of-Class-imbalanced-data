str(Dataframe111)

library(caret)
library(ctree)
library(rpart)
library(rpart.plot)
library(rattle)

Allsec_dctree <- Dataframe111

## Hot encoding
Allsec_dctree$Gender <- as.numeric(as.factor(Allsec_dctree$Gender))
Allsec_dctree$Marital.Status <-as.numeric(as.factor(Allsec_dctree$Marital.Status))
Allsec_dctree$Highest.Educational.Qualification<- as.numeric(as.factor(Allsec_dctree$Highest.Educational.Qualification))
Allsec_dctree$Overall.Experience<-as.numeric(Allsec_dctree$Overall.Experience)
Allsec_dctree$Department.Technology <- as.numeric(as.factor(Allsec_dctree$Department.Technology))
Allsec_dctree$Length.of.Service <- as.numeric(Allsec_dctree$Length.of.Service)
Allsec_dctree$Job.title <- as.numeric(as.factor(Allsec_dctree$Job.title))
Allsec_dctree$Supporting.market <- as.numeric(as.factor(Allsec_dctree$Supporting.market))
Allsec_dctree$Sal_bin1 <- as.numeric(Allsec_dctree$Sal_bin1)
Allsec_dctree$Region_2 <- as.numeric(Allsec_dctree$Region_2)


##Test & Train division

DataQ12 <- sort(sample(nrow(Allsec_dctree), nrow(Allsec_dctree)* .65))
Allsec_dctree_train <- Allsec_dctree[DataQ12,]
Allsec_dctree_test <- Allsec_dctree[-DataQ12,]


##str(Allsec_dctree_test)
##str(Allsec_dctree_train)

##table(is.na(Allsec_dctree_test))
##table(is.na(Allsec_dctree_train))

##Model Building

control = trainControl(method="repeatedcv", number=10, repeats=5)
Allsec_dctree_model = train(Resigned.y ~., data=Allsec_dctree_train, method="rpart", preProcess="scale", trControl=control)
summary(Allsec_dctree_model)

Allsec_tree1 <- rpart(Resigned.y ~ Gender+ Marital.Status + Highest.Educational.Qualification + Overall.Experience + Length.of.Service +
                       Final_Age + Sal_bin1 + Region_2 + Job.title
                     , data = Allsec_dctree_train, method = "class", na.action = na.pass,
                     control = rpart.control(minbucket = 7, cp = 0.003, minsplit = 20, maxsurrogate = 5,usesurrogate = 2,
                                             maxdepth = 30))

rpart.plot(Allsec_tree1)
fancyRpartPlot(Allsec_tree1)

Dectree_prediction1 <- predict(Allsec_tree1,Allsec_dctree_test, type = "class")
confusionMatrix(Allsec_dctree_test$Resigned.y,Dectree_prediction1)


Allsec_tree2 <- rpart(Resigned.y ~ .
                      , data = Allsec_dctree_train, method = "class", na.action = na.pass,
                      control = rpart.control(minbucket = 7, cp = 0,  minsplit = 20, maxsurrogate = 5,usesurrogate = 2,
                                              maxdepth = 30))

Dectree_prediction2 <- predict(Allsec_tree2,Allsec_dctree_test, type = "class")
confusionMatrix(Allsec_dctree_test$Resigned.y,Dectree_prediction2)


##GridSearch
library(mlr)
trainTask <- makeClassifTask(data = Allsec_dctree_train,target = "Resigned.y")
testTask <- makeClassifTask(data = Allsec_dctree_test,target = "Resigned.y")

makeatree <- makeLearner("classif.rpart", predict.type = "response")

getParamSet("classif.rpart")
set_cv <- makeResampleDesc("CV",iters = 3L)


##Hyperparameter search

gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

gscontrol <- makeTuneControlGrid()

stune <- tuneParams(learner = makeatree, resampling = set_cv, task = Allsec_dctree_train, par.set = gs, control = gscontrol, measures = acc)

