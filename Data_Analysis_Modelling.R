# Check variable names
names(sales)

# Two new columns to store time difference data
library(anytime)
sales$datead <- sales$lpuryear
sales$datelp <- sales$lpuryear

# Store in Modified sales data
modsales <- sales[,c(-4)]
modsales <- na.omit(modsales)

# Compute the time difference for the two dates
modsales$datead <-  difftime(anydate(modsales$datead6), anydate("11/15/2012"),units="days")
modsales$datelp <-  difftime(anydate(modsales$datelp6), anydate("11/15/2012"),units="days")

# The overall train and test set
train <- modsales[which(modsales$train==1),]
test <- modsales[which(modsales$train==0),]

# Remove outliers
# some pre-processing
initialtrain <- train[which(train$targdol>0),]
names(initialtrain)
initialfit <- lm(targdol~slstyr + slslyr + sls2ago + sls3ago +slshist+  ordtyr + ordlyr + ord2ago
                 + ord3ago +falord + sprord+datead + datelp, data=initialtrain)
initialfit1 <- lm(log(targdol+1)~slstyr + slslyr + sls2ago + sls3ago +slshist+  ordtyr + ordlyr + ord2ago
                 + ord3ago +falord + sprord+datead + datelp, data=initialtrain)

plot(initialfit,which=1)# fitted values plot
plot(initialfit,which=2)# QQ plot
plot(initialfit1,which=1)# fitted values plot
plot(initialfit1,which=2)# QQ plot
plot(initialfit1,which=4) # cook's distance
plot(initialfit1,which=6) # ratio plot

# remove 6 outliers
train <- train[c(-41618,-72673),]
train <- train[c(-42104,-38551),]

# Train and test for logistic (0,1 response)
logistictrain <- train
count <- 0
for(j in 1:50054){
    if(train[j,1] > 0){
      logistictrain[j,1] = 1;
      count = count + 1;}
}

# Stepwise logistic regression model on logistictrain
library(MASS)
names(logistictrain)
logistictrain$targdol <- as.factor(logistictrain$targdol)
fit <- glm(targdol ~ slstyr + slslyr + sls2ago + sls3ago +slshist+  ordtyr + ordlyr + ord2ago
             + ord3ago +ordhist+falord + sprord+datead + datelp, family = binomial, data=logistictrain)

stepfit <- stepAIC(fit,scope=~slstyr + slslyr + sls2ago + sls3ago +slshist+  ordtyr + ordlyr + ord2ago
                   + ord3ago +ordhist+falord + sprord+datead + datelp, data=logistictrain)

# Logistic p-prime stored in trainpredict
trainpredict <- stepfit$fitted.values

# Calculate the oversampling adjustment
qprime <- count/50054
q <- qprime/3 # OVERSAMPLING FACTOR m = 3 (CHANGE LATER)

# oversampling adjustments
right <- qprime*(1-q)/(q*(1-qprime))
for (j in 1:50054){
  phat <- trainpredict[j]/(1-trainpredict[j])
  m <- phat/((1-phat)*right)
  trainpredict[j] = m/(m+1)
}

# Determine the threshold p*
# Draw a plot of  versus p*

pval <- seq(0,0.4,0.001)
ccr <- seq(0,0.4,0.001)
for(k in 1:401){
  tab <- table(logistictrain$targdol,trainpredict > pval[k])
  ccr[k] <- sum(diag(tab))/sum(tab)
}
plot(pval,ccr,xlab = "p*",ylab = "CCR",type = 'l')
abline(v = 0.2,col='red');

# Check the CCR table
tab <- table(logistictrain$targdol,trainpredict > 0.2)
tab

# ROC curve


# Store the purchasers data in lineartrain
logistictrain$predict <- trainpredict
lineartrain <- train[which(logistictrain$predict > 0.2),]
lineartrain$targdol <- log(lineartrain$targdol + 1)

names(lineartrain)
# Multiple linear regression model
linearfit <- lm(targdol~ slstyr + slslyr + sls2ago + sls3ago + slshist + ordtyr + ordlyr + ord2ago + ord3ago + ordhist + 
                  falord + sprord + datead + datelp,data=lineartrain)
summary(linearfit)

# Stepwise regression
steplinear <- step(linearfit)
summary(steplinear)

# Standardized linear regression model
sdtrain <- lineartrain[,c(-2,-3)]
sdtrain <- sdtrain[,c(-14)]
for(p in 1:15)
{
  sdtrain[,p] <- (sdtrain[,p] -mean(sdtrain[,p]) )/sd(sdtrain[,p])
}
sdfit <- lm(targdol~ sls2ago + ordtyr + ordlyr + ord2ago + ord3ago + ordhist + 
                           falord + sprord + datead + datelp,data=sdtrain)
summary(sdfit)

# Try lasso regression
library(glmnet)
y <- lineartrain$targdol
x <- model.matrix(y~slstyr + slslyr + sls2ago + sls3ago +slshist+  ordtyr + ordlyr + ord2ago
                  + ord3ago +falord + sprord+datead + datelp, lineartrain)
lassofit <- glmnet(x,y,alpha=1,lambda=seq(0,0.5,0.001))
set.seed(123456)
lassocv <- cv.glmnet(x,y,alpha=1,lambda=seq(0,0.5,0.001),nfold = 3)
lambdalasso <- lassocv$lambda.min
lambdalasso

plot(lassocv);

plot(lassofit,xvar="lambda",main="Coeffs of Lasso Regression", type = "l", xlab = expression("log_lambda",ylab = "Coeff"))
abline(h=0);abline(v = log(lambdalasso))
small.lambda.index <- which(lassocv$lambda == lambdalasso)
small.lambda.betas <- coef(lassocv$glmnet.fit)[,small.lambda.index]
small.lambda.betas


