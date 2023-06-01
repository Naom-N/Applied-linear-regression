
# set up work directory
setwd('~/MSU/Msc Mathematics/Applied regression analysis/Notes/Labs/Four')
# read data
kroger <- read.table(file = 'lab4data.txt', header = F,
                     col.names = c('labor','cases','costs','holiday'))
# print first few rows
head(kroger)

#Question 3
#SSR Decomposition, dropping x3, and both x3 and x1 tests(parts a - c)
mod<-lm(labor~costs+cases+holiday, data=kroger)
anova(mod)

#part d
#compare SSR values
mod12<-lm(labor~cases+costs, data=kroger)
mod21<-lm(labor~costs+cases, data=kroger)
anova(mod12)
anova(mod21)

#part e
#Calculating coefficients of determination
mod1<-lm(labor~cases, data=kroger)
summary(mod1)
anova(mod1)

mod2<-lm(labor~costs, data=kroger)
summary(mod2)
anova(mod2)

cor(kroger[,-1])^2

anova(mod21)
(142092-11395)/3150741

anova(mod12)
(142092-136366)/3025770

mod132<-lm(labor~cases+holiday+costs, data=kroger)
summary(mod132)

#part f
#correlation transformation

# design matrix and hat matrix
Tkroger<-scale(kroger, center = TRUE, scale = TRUE)
X <- as.matrix(Tkroger[,-1]) # design matrix
H <- X%*%solve(t(X)%*%X)%*%t(X) # hat matrix
Y <- as.matrix(Tkroger[,1,drop=F]) # vector of Y
Yhat <- H%*%Y # fitted value, or use mod$fitted.values
b <- solve(t(X)%*%X)%*%t(X)%*%Y # least square estimators

#part g
Tmod<-lm(labor~cases+costs+holiday, data = as.data.frame(Tkroger))
summary(Tmod)

cor(Tkroger)^2

#part h
#standard errors
Cases_sd<- sd(kroger$cases)
Costs_sd<- sd(kroger$costs)
holiday_sd<- sd(kroger$holiday)
labor_sd<- sd(kroger$labor)

#beta values
b1<-(labor_sd/Cases_sd)*0.1747189
b2<-(labor_sd/Costs_sd)*-0.0463913
b3<-(labor_sd/holiday_sd)*0.8078617
