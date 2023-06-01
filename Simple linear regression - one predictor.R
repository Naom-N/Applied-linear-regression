
#Simple Linear regression

#Question 2

Age<-c(1,1,3,3,5,5,7,7,9,9,11,11,15,15,19,19,21,21,25,25,30,30)
Wing<-c(4.4,4.4,4.6,4.7,5.7,5.4,6.1,6.2,6.6,6.9,7.4,7.6,8.4,
        8.4,10.4,9.9,10.4,10.5,11.7,11.8,13.8,13.9)

plot(Age,Wing)
#calculating summary statistics for each variable
summary(Age)
summary(Wing)


#center each variable by subtracting its respective mean
Cent_age<-Age-mean(Age)
Cent_wing<-Wing-mean(Wing)

#create a scatter diagram of the centered data and comment on the diagram
plot(Cent_age,Cent_wing,main = "Centered data plot")

#use the formula from 1 to estimate beta on the centered data
beta<-sum(Cent_age*Cent_wing)/sum(Cent_age^2)
beta

#add the fitted line to your plot
abline(a=0,b=beta, col="blue")

#Interpret the estimate of Beta in terms of the problem being considered.

#Obtain the estimate of sigma squared
#fitted value
Winghat<-(beta*Cent_age)+mean(Wing)
#error
e<-Wing-Winghat
#SSE
sse<-sum(e^2)
#degree of freedom
df<- length(Age)-1
# MSE
mse <- sse/df
print(mse)

#Question 3
setwd('~/MSU/Msc Mathematics/Applied regression analysis/Homework/Assignment 1') 

# read data
Senic <- read.table(file = 'senic.txt',
                          col.names = c('Id_No','los','age','IR','RCR','RCXR','beds',
                                        'MSA','Region','ADC','nurses','AFS'),
                          header = F)
#load dplyr package
library(dplyr)

#subseting to remain with only he data we need
senic<-data.frame(Senic)%>%select('los','IR',Region)

#getting regional data
Reg1<-senic%>%filter(Region==1)
Reg2<-senic%>%filter(Region==2)
Reg3<-senic%>%filter(Region==3)
Reg4<-senic%>%filter(Region==4)

#running linear regression models on all the four
mod1<-lm(los~IR,data=Reg1)
mod2<-lm(los~IR,data=Reg2)
mod3<-lm(los~IR,data=Reg3)
mod4<-lm(los~IR,data=Reg4)

#scatter plots with regression lines
plot(Reg1$IR,Reg1$los, Main="Region 1")
abline(mod1,col='blue')

plot(Reg2$IR,Reg2$los,Main="Region 2")
abline(mod2,col='blue')

plot(Reg3$IR,Reg3$los, Main="Region 3")
abline(mod3,col='blue')

plot(Reg4$IR,Reg4$los, Main="Region 4",xlim = c(0,7))
abline(mod4,col='blue')

#ggplot(senic,aes(IR,los,col=Region))+geom_point()+
  #geom_abline(slope = 1.348,intercept = 4.538)+
  #geom_abline(slope = 0.4832,intercept = 7.5605)+
  #geom_abline(slope = 0.5251,intercept = 7.1293)+
  #geom_abline(slope = 0.01728,intercept = 8.03805) #actually worked
#but need to know how to insert legend or name the lines individually
  
#ggplot(senic,aes(IR,los,col=Region))+geom_point()+
  #geom_smooth(method='lm',aes(Reg1$IR,Reg2$los))  

#calculating the mse for each region
mse <- sigma(mod1)^2
mse

mse <- sigma(mod2)^2
mse

mse <- sigma(mod3)^2
mse

mse <- sigma(mod4)^2
mse
