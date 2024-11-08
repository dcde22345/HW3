install.packages("languageserver")

install.packages("jsonlite")

##Covariance & Correlation
install.packages("MVA")
install.packages("HSAUR2")

#
#library(fMultivar)
library(MVA)
library(HSAUR2)
data(USairpollution)
attach(USairpollution) # 使用US air pollution 資料集
pairs(USairpollution,pch=2,cex=1,col='blue') # 產生一個散布圖矩陣，呈現每兩個變數之間的關係


# Variance-Covariance matrix
# 變異數為對角線上數據
# 變異數代表每一個觀測值和平均值的偏差值的平方值
# X和Y的共變異數代表X變異時，Y變異的程度，可以表示兩個變數之間的方向性關係，但數值本身並不直觀
# 共變異數正數：代表X越大，Y越大
# 共變異數負數：代表X越大，Y越小
round(cov(USairpollution),3)

#Correlation matrix
# 代表兩變數之間的線性關係，可以直接帶入
round(cor(USairpollution),3)

# 停止使用USairpollution
detach(USairpollution)

                          
#Regression
# data可以check自己已載入的資料集有哪些
data()
attach(cars)
plot(cars)

#
# 做出一個linear model，dist為應變數，speed為自變數
L1=lm(dist~speed)
# 顯示L1的摘要
summary(L1)
plot(L1)

#
res=residuals(L1)
mean(res)
#
yhat=fitted(L1)
plot(yhat,res)

#
L2=lm(dist~0+speed+I(speed^2))
plot(cars)
abline(L1)
curve(1.239*x+0.090*x^2,add=T,col='blue',lwd=2,lty=3)


#Use the .csv file
kid_iq <- read.csv("kid_iq.csv")
attach(kid_iq)
plot(kid_iq)

#
install.packages('car')
install.packages("AER")
library(AER)
fit.1 = lm(kid.score ~ mom.hs + mom.iq, data = kid_iq)

# 檢驗兩者是否有線性關係
# mom.hs = 0, mom.iq
linearHypothesis(fit.1,c("mom.hs=0","mom.iq=0"))

#
lm(kid.score~mom.work)
lm(kid.score~as.factor(mom.work))
#
fit.2=lm(kid.score~mom.hs+mom.iq+mom.hs:mom.iq)
coef(fit.2)
confint(fit.2)
#
plot(mom.iq,kid.score,pch=20)
curve(cbind(1,1,x,1*x)%*%coef(fit.2),add=T,col="red")
curve(cbind(1,0,x,0*x)%*%coef(fit.2),add=T,col="green")
#
fit.all=lm(kid.score~mom.hs+mom.iq+mom.hs:mom.iq+mom.age+
             as.factor(mom.work),data=kid_iq)
summary(fit.all)



detach(kid.iq)

##training and validation set: Toyota example
car.df = read.csv("ToyotaCorolla.csv")
str(car.df)
# use first 1000 rows of data
car.df = car.df[1:1000, ]
# select variables for regression
selected.var = c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
set.seed(5566)  # set seed for reproducing the partition
train.index = sample(c(1:1000), 600, replace=FALSE)  
train.df = car.df[train.index, selected.var]
valid.df = car.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm = lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
#evaluate prediction accuracy
install.packages('forecast')
library(forecast)
# use predict() to make predictions on a new set. 
car.lm.pred = predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20] 
data.frame("Predicted" = car.lm.pred[1:20], 
           "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)



#significance of inverted U
library(car)
mom.iqsquares=mom.iq^2
fit.3=lm(kid.score~mom.hs+mom.iq+mom.iqsquares)
deltaMethod(fit.3,"mom.iq+2*mom.iqsquares*3",rhs=0)

#relative weights
#https://towardsdatascience.com/relative-importance-analysis-a-better-way-to-communicate-multiple-regression-results-d70a6fbbaf9c
install.packages('rwa')
library('rwa')
rwa(kid_iq,outcome="kid.score",predictors = c("mom.hs","mom.iq"))

