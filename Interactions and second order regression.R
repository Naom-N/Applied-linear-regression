
#setting a working directory
setwd('~/your/working/directory')

#reading data
copier1 <- read.table(file = 'Q1.20.txt', header = F,
                     col.names = c('mins','no'))
copier2<-read.table(file = 'Q8.15.txt', header = F,
                    col.names = c('type'))
copier<-cbind(copier1,copier2)

plot(copier$no,copier$mins)
boxplot(mins~type,data = copier)

#Estimated regression model
mod1<-lm(mins~.,data=copier)
summary(mod1)

#Residuals plot and QQpot
plot(mod1$fitted.values,mod1$residuals)
qqnorm(mod1$residuals)
qqline(mod1$residuals)

#Evidence that either predictor should be kept
summary(mod1)

#plot Y vs X1 distinguish points by type of copier
plot(mins~no, pch=type+1, data=copier)

#Interaction effect
mod2<-lm(mins~no+type+no*type, data=copier)

#plots with the interaction terms
plot(mod2$fitted.values~no, pch=type+1, data=copier)

#hypothesis testing to see if we can drop the interaction term
anova(mod2,mod1)

#Question 2
#setting working directory
setwd('~/MSU/Msc Mathematics/Applied regression analysis/Homework/Hw 7')

#reading the data
mmass<-read.table(file = 'musclemass.txt', header = F,
                  col.names = c('mass','age'))

mass<-mmass[,-2]
age<-mmass[,-1]
mean(age)
n_age<-age-mean(age)
sq_age<-n_age^2
n_mmass<-as.data.frame(cbind(mass,n_age,sq_age))

#a) Fit a second order model
mod3<-lm(mass~., data=n_mmass)
summary(mod3)

#b)Plot the data and add the fitted line
plot(mass~n_age, data=n_mmass)
lines(sort(n_age),fitted(mod3)[order(n_age)],col="blue",type='l')


#c)Test for a regression relation
summary(mod3)

#d)Estimate muscle mass for women aged 50 years
predict(mod3,newdata = data.frame(n_age=50-59.98333,sq_age=(50-59.98333)^2))

#e)Test whether the quadratic term can be dropped
mod4<-lm(mass~n_age, data=n_mmass)
anova(mod3,mod4)

#f)Fitted regression in the original variable X
beta0<-mod3$coefficients[1]-mod3$coefficients[2]*mean(age)+
  mod3$coefficients[3]*mean(age)^2
beta1<-mod3$coefficients[2]-2*mod3$coefficients[3]*mean(age)
beta2<-mod3$coefficients[3]

#g)Estimate muscle mass for women aged 50 with (f) above
Y50<-beta0+beta1*50+beta2*50^2
Y50

#h) Simple correlation between centered x and sq x and X and sq X
cor(n_mmass)
sqr_age<-age^2
msclmass<-cbind(mmass,sqr_age)
cor(msclmass)

#i)plot residual from (a) against fitted values and the centered variable & Q-Q plot
plot(mod3$residuals~mod3$fitted.values)
plot(mod3$residuals~n_age, data=n_mmass)
plot(mod3$fitted.values~sq_age, data=n_mmass)
qqnorm(mod3$residuals)
qqline(mod3$residuals)

#j)Fit a third order model and test whether or not to keep the three-way interaction
cbd_age<-n_age^3
order3<-cbind(n_mmass,cbd_age)
mod5<-lm(mass~.,data=order3)
anova(mod5,mod3)
