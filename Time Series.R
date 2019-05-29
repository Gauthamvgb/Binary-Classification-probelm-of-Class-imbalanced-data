#########################################################################################
attrition_2017 <- read.csv(file.choose())
attrition_2017 <- attrition_2017[c(-(6:12))]
data = attrition_2017
data= na.omit(data)
##data$Month<- as.Date(data$Month,
##              format = "%m-%y")

##Data preprocessing##
myts_allsec_chennai = ifelse(data$Location == 'chennai',data$Attrition, NA )
myts_allsec_chennai = na.omit(myts_allsec_chennai)
myts_allsec_Delhi = ifelse(data$Location == 'Delhi',data$Attrition, NA)
myts_allsec_Delhi = na.omit (myts_allsec_Delhi)
data$Location[data$Location == 'bangalore'] <- 'Bangalore'
myts_allsec_bangalore = ifelse(data$Location == 'Bangalore',data$Attrition, NA)
myts_allsec_bangalore = na.omit(myts_allsec_bangalore)

##timeseries for chennai, Delhi and Bangalore##
Chennai_ts <- ts(myts_allsec_chennai, start=c(2017,1), end=c(2017, 12),
                 frequency=12)
plot(Chennai_ts)
adf.test(Chennai_ts)
##fit1 <- stl(Chennai_ts, ##t.window= 50, s.window= "per", robust = TRUE##
#           "periodic")


Delhi_ts <- ts(myts_allsec_Delhi, start=c(2017,1), end=c(2017, 12),
               frequency=12)
plot(Delhi_ts)
adf.test(Delhi_ts)
##fit <- stl(Delhi_ts, t.window= 50, s.window= "per", robust = TRUE)

Bangalore_ts <- ts(myts_allsec_bangalore, start=c(2017,1), end=c(2017, 12),
                   frequency=12, deltat = 4)
plot(Bangalore_ts)
adf.test(Bangalore_ts)
##fit <- stl(Chennai_ts, t.window= 50, s.window= "per", robust = TRUE)

## Plotting as a whole Allsec Data##
Allsec_ts <- ts(data$Attrition,
                frequency=12, deltat = 2)
plot(Allsec_ts)

fit <- stl(Allsec_ts,"periodic")
plot(fit)
fit
## Prediction ##

monthplot(Allsec_ts)
monthplot(Chennai_ts)
monthplot(Bangalore_ts)
monthplot(Delhi_ts)

library(forecast)
seasonplot(Allsec_ts)
seasonplot(Chennai_ts)
seasonplot(Bangalore_ts)
seasonplot(Delhi_ts)


fit1 <- HoltWinters(Chennai_ts, beta=FALSE, gamma=FALSE)
fit2 <- HoltWinters(Bangalore_ts, beta=FALSE, gamma=FALSE)
fit3 <- HoltWinters(Delhi_ts, beta=FALSE, gamma=FALSE)
##accuracy(fit2)

forecast(fit1, 3)
plot(forecast(fit1, 3))

forecast(fit2, 3)
plot(forecast(fit2, 3))

forecast(fit3, 3)
plot(forecast(fit3, 3))


################################################################################################
Resigned_number <- c(132, 138, 200, 198, 214, 278, 170, 151, 161, 177, 198, 150)
whole_ts <- ts(Resigned_number, frequency = 12, start=c(2017,1), end=c(2017, 12))
plot(whole_ts)
seasonplot(whole_ts)
fit4 <- HoltWinters(whole_ts, beta=FALSE, gamma=FALSE)
forecast(fit4, 3)
plot(forecast(fit4,3))



#########################

adf.test(whole_ts)
nsdiffs(whole_ts)


ts.stl <- stl(Allsec_ts, "periodic")
ts.sa <- seasadj(ts.stl)
plot(ts.sa)