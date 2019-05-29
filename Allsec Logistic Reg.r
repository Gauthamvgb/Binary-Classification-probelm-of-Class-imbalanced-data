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

Reg_fit <- glm(Resigned.y ~., family = binomial(link = "logit"), data = Allsec_dctree_train, control = list(maxit = 50))
summary(Reg_fit)


Regression_fit <- glm(Resigned.y ~ Overall.Experience + Length.of.Service + Final_Age +
                        Department.Technology + Job.title + Sal_bin1 + Region_2 , family = binomial(link = "logit"),
                      data = Allsec_dctree_train)

summary(Regression_fit)
anova(Regression_fit, test = "Chisq")
Reg_pred <- predict(Regression_fit, newdata = subset(Allsec_dctree_test,select = c(4,5,6,7,9,11,12)), type = 'response')
Reg_pred = ifelse(Reg_pred > 0.5, 1, 0)
confusionMatrix(Reg_pred, Allsec_dctree_test$Resigned.y)



## SVM
library(e1071)

## Crossvalidation
tuned = tune.svm(Resigned.y ~., data = Allsec_dctree_train, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=5))
summary(tuned)
obj <- tune(svm, Resigned.y~Overall.Experience + Length.of.Service + Final_Age +
              Department.Technology + Job.title + Sal_bin1 + Region_2, data = Allsec_dctree_train, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))
summary(obj)

svm_fit <- svm(Resigned.y ~ Overall.Experience + Length.of.Service + Final_Age +
                 Department.Technology + Job.title + Sal_bin1 + Region_2, data = Allsec_dctree_train, kernel= "radial", cost = 4, gamma = .05 )
summary(svm_fit)
x1 = subset(Allsec_dctree_test,select = c(4,5,6,7,9,11,12))
pred_svm <- predict(svm_fit, x1, decision.values =  TRUE)
pred_svm = ifelse(pred_svm > 0.5,1,0)
confusionMatrix(pred_svm, Allsec_dctree_test$Resigned.y)


##with whole dataset

Fulldata = rbind(Allsec_dctree_test,Allsec_dctree_train)
x2 = subset(Fulldata,select = c(4,5,6,7,9,11,12))
pred_svm_full <- predict(svm_fit, x2, decision.values =  TRUE)
pred_svm = ifelse(pred_svm_full> 0.5,1,0)
confusionMatrix(pred_svm, Fulldata$Resigned.y)




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
