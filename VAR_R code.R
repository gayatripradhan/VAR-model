setwd("C:/Users/pradhanG/Downloads")
getwd()

data=read.csv("C:/Users/pradhanG/Downloads/data.csv")

Find_Abs_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot=FALSE, lag.max = length(a)-5)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  absres = data.frame(abscor,lag)
  absres_max = res[which.max(absres$abscor),]
  return(absres_max)
}

for (i in 0:length(data)){
  for (j in 1:length(data)){
    x = colnames(data)[i]
    y = colnames(data)[i]
    correlation = Find_Abs_Max_CCF(x,y)$cor
    lag = Find_Abs_Max_CCF(x,y)$lag
  }
}

#VAR model

setwd("D:/Internship/VAR")
getwd()
library(vars)
library(PerformanceAnalytics)
#top advisors
adv <- read.csv("D:/Internship/VAR/advisors.csv")
adv_ts <- ts(adv)

indexes = sample(1:nrow(adv), size=0.2*nrow(adv))

# Split data
test = adv[indexes,]
dim(test)  
train = adv[-indexes,]
dim(train)

#for each columns
Wescott.Financial.Advisory.Group = ts(train$Wescott.Financial.Advisory.Group, frequency=1, start=c(1), end=c(429))
Burt.Wealth.Advisors = ts(train$Burt.Wealth.Advisors, frequency=1, start=c(1), end=c(429))
Adams.Hall.Wealth.Advisors..LLC = ts(train$Adams.Hall.Wealth.Advisors..LLC, frequency=1, start=c(1), end=c(429))
SMF.Financial.Advisors..LLC = ts(train$SMF.Financial.Advisors..LLC, frequency=1, start=c(1), end=c(429))
Foster.Group..Inc. = ts(train$Foster.Group..Inc., frequency=1, start=c(1), end=c(429))
Lodestar.Private.Asset.Management.LLC = ts(train$Lodestar.Private.Asset.Management.LLC, frequency=1, start=c(1), end=c(429))
CPS.Investment.Advisors = ts(train$CPS.Investment.Advisors, frequency=1, start=c(1), end=c(429))
Jones.Barclay.Boston...Co. = ts(train$Jones.Barclay.Boston...Co., frequency=1, start=c(1), end=c(429))
Warren.Averett.Asset.Management..LLC = ts(train$Warren.Averett.Asset.Management..LLC, frequency=1, start=c(1), end=c(429))

i = cbind(Wescott.Financial.Advisory.Group,Burt.Wealth.Advisors,Adams.Hall.Wealth.Advisors..LLC,SMF.Financial.Advisors..LLC,
          Foster.Group..Inc.,Lodestar.Private.Asset.Management.LLC,
          CPS.Investment.Advisors,Jones.Barclay.Boston...Co.,
          Warren.Averett.Asset.Management..LLC)

plot(i)
lag = VARselect(i,lag.max=10)
lag
var = VAR(i, p=2)
var
predict = predict(var,n.ahead=10,ci=0.95)
predict
plot(predict)
details = summary(var)
summary(predict)

#Accuracy
library(forecast)
fit1 <- rwf(i[1:429,1],h=100)
fit2 <- meanf(i[1:429,1],h=100)
accuracy(fit1)
accuracy(fit2)
accuracy(fit1,adv[430:536,1])
accuracy(fit2,adv[430:536,1])
plot(fit1)
lines(adv[1:536,1])
accuracy(adv)


