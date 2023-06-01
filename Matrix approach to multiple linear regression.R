
# set up work directory
setwd('~/MSU/Msc Mathematics/Applied regression analysis/Homework/Hw 5')
# read data
kroger <- read.table(file = 'lab4data.txt', header = F,
                     col.names = c('labor','cases','costs','holiday'))
#QUESTION 1
#a)
# print first few rows
head(kroger)
#find the structure
str(kroger)

#b)
#find b
### design matrix and hat matrix
X <- as.matrix(cbind(1, kroger[,-c(1,3)])) # design matrix
H <- X%*%solve(t(X)%*%X)%*%t(X) # hat matrix
Y <- as.matrix(kroger[,1,drop=F]) # vector of Y
Yhat <- H%*%Y # fitted value, or use mod$fitted.values
b <- solve(t(X)%*%X)%*%t(X)%*%Y # least square estimator

#d) b1 T-test
mod1 <- lm(labor~cases+holiday, data=kroger)
summary(mod1)

#e)Make a 95% confidence interval for beta2
confint(mod1)

#f)print out the anova table
anova(mod1)
anova(lm(labor~cbind(cases,holiday),data = kroger))

#g)fitted residuals model
qqnorm(scale(mod1$residuals))
qqline(scale(mod1$residuals))

#i)Make a 95% confidence interval for the mean expected total labor hours for 
#a non-holiday week when 245,000 cases are shipped
predict(mod1,newdata=data.frame(cases=245000,holiday=0),
       interval="confidence")

#j) 98% prediction with no holiday and 280,000 cases shipped.
predict(mod1,newdata=data.frame(cases=280000,holiday=0),
       interval="prediction",level=0.98)

#QUESTION TWO
#read data
commp<-read.table(file='commp.txt',header = F, 
                  col.names = c('rent_rate','age','xptax','vac_rate','sqft'))
#print first few rows
head(commp)

#a) Obtain the scatter plot matrix and the correlation matrix
library(ggplot2)
pairs(commp,col='maroon')
cor(commp)

#b) fit the regression model
### design matrix and hat matrix
X <- as.matrix(cbind(1, commp[-1])) # design matrix
H <- X%*%solve(t(X)%*%X)%*%t(X) # hat matrix
Y <- as.matrix(commp[,1,drop=F]) # vector of Y
Yhat <- H%*%Y # fitted value, or use mod$fitted.values
b <- solve(t(X)%*%X)%*%t(X)%*%Y # least square estimator

mod2 <- lm(rent_rate~age+xptax+vac_rate+sqft, data=commp)
summary(mod2)

#c)Residual boxplots
boxplot(mod2$residuals)

#d)	Plot the residuals against \hat{Y} and each predictor. Also, a normal Q-Q plot
par(mfrow=c(1,1))
plot(mod2$residuals~commp$rent_rate)
plot(mod2$residuals~commp$age)
plot(mod2$residuals~commp$xptax)
plot(mod2$residuals~commp$vac_rate)
plot(mod2$residuals~commp$sqft)
qqnorm(scale(mod2$residuals))
qqline(scale(mod2$residuals))

#e)Print out the ANOVA table
anova(lm(rent_rate~cbind(age,xptax,vac_rate,sqft),data = commp))

#f)What predictors that are helpful in predicting the rental rates
summary(mod2)

#g)Estimate Betas jointly by the Bonferroni procedure using a 98% family CI
MSE <- sigma(mod2)^2
covar_hat_b <- MSE*solve(t(X)%*%X) #covariance matrix foer the b vector
var_hat_b <- diag(covar_hat_b) #variance for each b_i
alpha <- 1-0.98 #family confidence coefficient
g <- 4 #estimating two Beta's simultaneously
B <- qt(1-alpha/(2*g),df.residual(mod2)) #BF coefficient
b[2]+c(-1,1)*B*sqrt(var_hat_b[2]) #BF interval for Beta_1
b[3]+c(-1,1)*B*sqrt(var_hat_b[3]) #BF interval for Beta_2
b[4]+c(-1,1)*B*sqrt(var_hat_b[4]) #BF interval for Beta_3
b[5]+c(-1,1)*B*sqrt(var_hat_b[5]) #BF interval for Beta_4

#h)estimates of the mean rental rates for four typical properties at 99% family CI
X_new <- cbind(c(1,5.5,8.25,0,250000),c(1,6,8.5,0.23,270000),
               c(1,14,11.5,0.11,300000),c(1,12,10.25,0,310000))
Yh_hat_vector <- t(X_new)%*%b #predicted responses vector
var_Y_hat <- diag(MSE*(t(X_new)%*%solve(t(X)%*%X)%*%X_new))
alpha <- 1-0.99

#working-Hotelling adjusted
p <- 3
W <- sqrt(p*qf(1-alpha,p,dim(X)[1]-p))

#Bonferroni adjusted
g <- 4 #number of mean responses estimated
B <- qt(1-alpha/(2*g),df.residual(mod2))
lapply(1:4, function(i){Yh_hat_vector[i]+c(-1,1)*B*sqrt(var_Y_hat[i])})

#i) prediction at 95% CI for each of the 3 properties
predict(mod2,newdata=data.frame(age=c(4,6.0,12.0),
                                xptax=c(10,11.5,12), 
                                vac_rate=c(0.10,0,.32),
                                sqft=c(80000,120000,340000)),
        interval="prediction",level=0.95)

#j)	Simultaneous prediction interval with family CI of 95%
X_newp <- cbind(c(1,4,10,0.10,80000),c(1,6,11.5,0,120000),
               c(1,12,12.5,0.32,340000))
Yh_hat_vector <- t(X_newp)%*%b #predicted responses vector
var_Y_hat <- diag(MSE*(1+t(X_newp)%*%solve(t(X)%*%X)%*%X_newp))
alpha <- 1-0.95

#working-Hotelling adjusted
p <- 2
W <- sqrt(p*qf(1-alpha,p,dim(X)[1]-p))

#Bonferroni adjusted
g <- 3 #number of mean responses estimated
B <- qt(1-alpha/(2*g),df.residual(mod2))
lapply(1:3, function(i){Yh_hat_vector[i]+c(-1,1)*B*sqrt(var_Y_hat[i])})

#Bonus
#Conduct the Brown-Forsythe test for constancy of the error variance, using alpha=0.05.
medcommp<-median(mod2$fitted.values)
ft_values<-mod2$fitted.values
g1<-ft_values[which(mod2$fitted.values<medcommp)]

g2<-ft_values[which(mod2$fitted.values>=medcommp)]

mod2_res<-mod2$residuals
g1.res <- mod2_res[which(mod2$fitted.values<medcommp)]
g2.res <- mod2_res[which(mod2$fitted.values>=medcommp)]

abd1 <- abs(g1.res - median(g1.res))
abd2 <- abs(g2.res - median(g2.res))

#test statistic based on the two-sample t-test
t.test(abd1,abd2)
