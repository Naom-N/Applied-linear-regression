#Homework 3
#set working directory
setwd('~/your/working/directory')

#read data
MuscleMass <- read.table(file = 'MuscleMass.txt',
                          col.names = c('musclemass','age'),
                          header = F)
CrimeRate <- read.table(file = 'CrimeRate.txt',
                          col.names = c('cr','pct'),
                          header = F)

#Question 1
massmod<-lm(musclemass~age,data = MuscleMass)
massmod$coefficients[1]
massmod$coefficients[2]

#c) beta 1 confidence interval
confint(massmod)

#d)Create an ANOVA table
anova(massmod)
#or
fvalue_m <- qf(p=1-0.05, 1,58)

#e) Construct a 98% confidence interval for the expected mean muscle mass for 
#women of age 55

age_h <- 55
musclemass_h <- predict(massmod, newdata = data.frame(age=age_h))
print(unname(musclemass_h))

# estimated variance of musclemass_h
mse_m<-sigma(massmod)^2
sdmass_h <- mse_m*(1/(length(MuscleMass$age)) + 
                   (age_h-mean(MuscleMass$age))^2/sum((MuscleMass$age-mean(MuscleMass$age))^2))
print(sdmass_h)

# tvalue
alpha <- 0.02
df <- length(MuscleMass$age)-2
tvalue_m <- qt(p=1-alpha/2, df)
print(tvalue_m)

# 98% CI
ci98_m <- musclemass_h + c(-1,1)*tvalue_m*sqrt(sdmass_h)
print(ci98_m)

# f) estimated variance of prediction
sdpred_m <- mse_m*(1+1/(length(MuscleMass$age)) + 
                 (age_h-mean(MuscleMass$age))^2/sum((MuscleMass$age-mean(MuscleMass$age))^2))
# 99% prediction interval
pred98_m <- musclemass_h + c(-1,1)*tvalue_m*sqrt(sdpred_m)
print(pred98_m)

#R squared value
summary(massmod)$r.squared

#Question 2
#a) i
crmod<-lm(cr~pct,data = CrimeRate)
crmod$coefficients[1]
crmod$coefficients[2]

#ii) mean crime rate
crmod$coefficients[1]+crmod$coefficients[2]*(80)

#iii)error(10)
Yhat10_cr<-crmod$coefficients[1]+crmod$coefficients[2]*CrimeRate$pct[10]
Truey10_cr<-CrimeRate$cr[10]
Truey10_cr-Yhat10_cr

#iv) variance
sigma(crmod)^2

#b) beta 1 confidence interval
#i)
confint(crmod, level=0.99)

#ii) Construct a 90% confidence interval for the expected mean crime rate for 
#counties with graduation rate at 82

pct_h <- 82
cr_h <- predict(crmod, newdata = data.frame(pct=pct_h))
print(unname(cr_h))

# estimated variance of cr_h
mse_cr<-sigma(crmod)^2
sdcr_h <- mse_cr*(1/(length(CrimeRate$pct)) + 
                   (pct_h-mean(CrimeRate$pct))^2/sum((CrimeRate$pct-mean(CrimeRate$pct))^2))
print(sdcr_h)

# tvalue
alpha <- 0.10
df <- length(CrimeRate$pct)-2
tvalue_cr <- qt(p=1-alpha/2, df)
print(tvalue_cr)

# 90% CI
ci90_cr <- cr_h + c(-1,1)*tvalue_cr*sqrt(sdcr_h)
print(ci90_cr)

# iii) estimated variance of prediction
sdpred_cr <- mse_cr*(1+1/(length(CrimeRate$pct)) + 
                 (pct_h-mean(CrimeRate$pct))^2/sum((CrimeRate$pct-mean(CrimeRate$pct))^2))
# 96% prediction interval
# 96% tvalue
alpha <- 0.04
df <- length(CrimeRate$pct)-2
tvalue_nw <- qt(p=1-alpha/2, df)
print(tvalue_nw)

pred96_cr <- cr_h + c(-1,1)*tvalue_nw*sqrt(sdpred_cr)
print(pred96_cr)

#c)conduct hypothesis tests an ANOVA table
anova(crmod)
#or
fvalue_cr <- qf(p=1-0.05, 1,length(CrimeRate$pct)-2)

#and
summary(crmod)
tvalue <- qt(p=0.95, df)

