#Applied Regression Analysis 
#Final Exam

#set working directory
setwd('~/your/working/directory')

#Question 1
#load data
salary <- read.table(file = 'salary.txt', header = T)

#a)What is the true model
#b)fit the linear model
salary$degree<-as.factor(salary$degree)
mod1<-lm(salary~.,data=salary)
summary(mod1)

#c)plot residuals vs experience and supervised
plot(mod1$residuals~experience, data=salary)
plot(mod1$residuals~supervise, data=salary)

#d)identify any potential outliers in the data set
#Extreme X
par(mfrow=c(1,3))
h_ii<-hatvalues(mod1)
plot(experience=experience, h_ii,data=salary)
abline(3*4/65,0,lty=2)
plot(supervise=supervise, h_ii,data=salary)
abline(3*4/65,0,lty=2)
plot(degree=degree, h_ii,data=salary)
abline(3*4/65,0,lty=2)
which(h_ii>(3*4)/65)

#Extreme Y
rstu1 <- rstandard(mod1)
plot(experience, rstu1,data=salary)
abline(3,0,lty=2)
plot(supervise, rstu1,data=salary)
abline(3,0,lty=2)
plot(degree, rstu1,data=salary)
abline(3,0,lty=2)
which(rstu1>3)

#e)any outlier influential on all fitted values
cks<-cooks.distance(mod1)
plot(experience, cks,data=salary)
abline(1,0,lty=2)
plot(supervise, cks,data=salary)
abline(1,0,lty=2)
plot(degree, cks,data=salary)
abline(1,0,lty=2)
which(cks>1)

#f)Remedial measure 
#g)Do any multicollinearity  problems exist?
library(car)
vif(mod1)

#Question 2
#load data
peru <- read.table(file = 'peru.txt', header = T)
fraction<-peru$Years/peru$Age
peru1<-cbind(peru,fraction)
peru2<-peru1[,-10]

#loading packages
library(MASS)
library(ISLR)
library(tidyverse)
library(broom)

#a)Using atleast 3 different criteria, determine the best subset model(s)
p <- ncol(peru2)-1 #no. of predictors
library(leaps)
bs <- regsubsets(Systol~., method="exhaustive", nvmax = p, nbest=1, data=peru2)
summary(bs)
bs_summary <- tidy(bs)

bs_summary_long <- bs_summary %>% 
  mutate(size = 1:p) %>%
  pivot_longer(cols = adj.r.squared:mallows_cp, names_to = "criteria", values_to = "value")
bs_summary_long %>%
  ggplot(aes(x = size, y = value)) +
  facet_wrap(~criteria, scales="free_y") +
  geom_line()

#b)What model is chosen by forward stepwise selection
fwd <- regsubsets(Systol~., method = "forward", nvmax = p, nbest=1, data = peru2)
summary(fwd)
tidy(fwd)
stwise<-format(round(as.data.frame(tidy(fwd)),3),nsmall=3)
which.max(stwise$adj.r.squared)

#c)Most appropraite model?
#d)Perform cross validation.
par(mfrow=c(1,1))
k <- 10
set.seed(123) # set seed for reproducible results
folds <- sample(1:k,nrow(peru2),replace=T) #allocates each observation to one of the folds
cv.err <- matrix(NA,k,p,dimnames=list(NULL,paste(1:p))) #matrix to store results

for(j in 1:k){
  ## fit the model to the training data
  train.mod <- regsubsets(Systol~.,data=peru2[folds!=j,],nvmax=p)
  for(i in 1:p){
    ## make predictions of the validation data
    form <- as.formula(train.mod$call[[2]])
    mat <- model.matrix(form,peru2[folds==j,]) #design matrix
    beta <- coef(train.mod, id=i)
    xvars <- names(beta)
    pred <- (mat[,xvars]%*%beta)[,1]
    ## compute the mean prediction errors
    cv.err[j,i] <- mean((peru2$Systol[folds==j]-pred)^2)
  }
}

mean.cv.err <- apply(cv.err,2,mean) #CV error for each model size
par(mfrow <- c(1,1))
plot(mean.cv.err,type='b') #select the 10-variable model

## Best model
best.mod <- regsubsets(Systol~.,data=peru2,nvmax=p)
summary(best.mod)
coef(best.mod,5)

#e)Fit a LASSO regression model using all the predictors

x <- model.matrix(Systol~., peru2)[,-1] #convert any categorical variable into dummies
n <- nrow(x)
y <- peru2$Systol

## split data
set.seed(1)
train <- sample(1:n, n/2)


### LASSO
## select lambda
lasso_cv_out <- cv.glmnet(x[train,], y[train], 
                          alpha = 1) # 0 for ridge, 1 for lasso
plot(lasso_cv_out)
lasso_cv_out$lambda.min

## test error
lasso_pred <- predict(lasso_cv_out, 
                      s = lasso_cv_out$lambda.min,
                      newx = x[-train,])
mean((lasso_pred-y[-train])^2)

## final model
lasso_out <- glmnet(x, y, alpha = 1) # 0 for ridge, 1 for lasso
coef(lasso_out, s = lasso_cv_out$lambda.min)
plot(lasso_out, label = TRUE, xvar = "lambda")

#Question 3
#load data
mnf <- read.table(file = 'manufacture.txt', header = T)

#a)The logistic regression model
#b)Fit the model in part (a)
mod2<-glm(Defect~Temp+Cooltime+Speed,data=mnf, family = 'binomial')

#c)The exponential of each variable 
exp(mod2$coefficients)


#d)95% CI for each parameter. 
confint(mod2)
or
exp(confint(mod2))
