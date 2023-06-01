
#Wing chord length prediction ~ age

#Question 2
Age<-c(1,1,3,3,5,5,7,7,9,9,11,11,15,15,19,19,21,21,25,25,30,30)
Wing<-c(4.4,4.4,4.6,4.7,5.7,5.4,6.1,6.2,6.6,6.9,7.4,7.6,8.4,
        8.4,10.4,9.9,10.4,10.5,11.7,11.8,13.8,13.9)

#centered data
Cent_age<-Age-mean(Age)
Cent_wing<-Wing-mean(Wing)

#beta estimate on the centered data
b<-sum(Cent_age*Cent_wing)/sum(Cent_age^2)
b

#Obtain the estimate of sigma squared
#fitted value
Winghat<-(b*Cent_age)+mean(Wing)
#error
e<-Wing-Winghat
#SSE
sse<-sum(e^2)
#degree of freedom
df<- length(Age)-1
# MSE
mse <- sse/df
print(mse)

#Confidence Interval for beta
# estimated variance of b
varb_hat <- mse/sum(Cent_age^2)
print(varb_hat)

# percentile of the t distribution
alpha <- 0.02
df <- length(Age)-1
tvalue <- qt(p=1-alpha/2, df) 
print(tvalue)

# 98% CI
ci98 <- b + c(-1,1)*tvalue*sqrt(varb_hat)
print(ci98)

#part c and d
#test for H_0:Beta<=0.5
beta10 <- 0.5
tstar <- (b-beta10)/sqrt(varb_hat)
print(tstar)

pvalue <- 1-pt(tstar, df)
print(pvalue)


