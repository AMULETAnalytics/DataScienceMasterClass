# MODULE 9 - Supervised Machine Learning
#
# (c) Copyright 2015 - AMULET Analytics
# ---------------------------------------------------------------


# Perform EDA in preparation for linear regression

install.packages("UsingR")
library(UsingR)

data(galton)        # 928x2 just two variables: child height, parent height
summary(galton)     # Get familiar with data

# Do some EDA
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)
par(mfrow=c(1,1))

# ---------------------------------------------------------------

# Distribution of the child heights

hist(galton$child,col="blue",breaks=100)

# Add a mean line to the histogram. The mean shows where the distribution
# is centered.
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

# ---------------------------------------------------------------

# Scatterplot to see how data might be related. 
# An oval shaped cloud implied there might be a relationship
# NOTE: each point on the graph could actually be several stacked up pts.
plot(galton$parent,galton$child,pch=19,col="blue")

# Say we know average parent = 65 inches tall, then what is child height?
# Subset 89 rows where parent height is "close to" 65
near65 <- galton[abs(galton$parent - 65)<1, ]

# Plot the subset as red dots
points(near65$parent,near65$child,pch=19,col="red")

# Draw reference line in red: x coords: 64-66, y coords: subset child hts
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

# ---------------------------------------------------------------

plot(galton$parent,galton$child,pch=19,col="blue")

# Say - average parent = 71 inches tall, then what is child height?
near71 <- galton[abs(galton$parent - 71)<1, ]
points(near71$parent,near71$child,pch=19,col="red")
lines(seq(70,72,length=100),rep(mean(near71$child),100),col="red",lwd=4)

# ---------------------------------------------------------------

plot(galton$parent,galton$child,pch=19,col="blue")

# Fit a linear model using basic least squares
# Allows you to predict child given parent
lm1 <- lm(galton$child ~ galton$parent)

# Draw the regression line through the distribution
lines(galton$parent,lm1$fitted,col="red",lwd=3)

# ---------------------------------------------------------------

summary(lm1)
names(lm1)

lm1$coeff

# Can use the trained linear model to preduct new child heights
# For parent = 60
predict_child <- lm1$coeff[1] + 60 * lm1$coeff[2]
predict_child
(Intercept) 
62.71897 

# ---------------------------------------------------------------

par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)

# ---------------------------------------------------------------

# Residuals plot - shoudl all be centered around 0 line
# Residuals are the distances between the actual points and the 
# regression line. 
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)
par(mfrow=c(1,1))


# ---------------------------------------------------------------

# Multiple Linear regression

install.packages("car")
library(car)
data(Prestige)

summary(Prestige)
#  education          income          women           prestige    
#Min.   : 6.380   Min.   :  611   Min.   : 0.000   Min.   :14.80  
#1st Qu.: 8.445   1st Qu.: 4106   1st Qu.: 3.592   1st Qu.:35.23  
#Median :10.540   Median : 5930   Median :13.600   Median :43.60  
#Mean   :10.738   Mean   : 6798   Mean   :28.979   Mean   :46.83  
#3rd Qu.:12.648   3rd Qu.: 8187   3rd Qu.:52.203   3rd Qu.:59.27  
#Max.   :15.970   Max.   :25879   Max.   :97.510   Max.   :87.20  
#    census       type   
#Min.   :1113   bc  :44  
#1st Qu.:3120   prof:31  
#Median :5135   wc  :23  
#Mean   :5402   NA's: 4  
#3rd Qu.:8312            
#Max.   :9517     

head(Prestige)
#                    education income women prestige census type
#gov.administrators      13.11  12351 11.16     68.8   1113 prof
#general.managers        12.26  25879  4.02     69.1   1130 prof
#accountants             12.77   9271 15.70     63.4   1171 prof
#purchasing.officers     11.42   8865  9.11     56.8   1175 prof
#chemists                14.62   8403 11.68     73.5   2111 prof
#physicists              15.64  11030  5.13     77.6   2113 prof

# Remove observations with prof NA
Prestige_noNA <- na.omit(Prestige)

n <- nrow(Prestige_noNA)  # Number of observations = 102
ntrain <- round(n*0.6)    # 60% for training set
set.seed(333)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

trainPrestige <- Prestige_noNA[tindex,]  # Create training set
testPrestige <- Prestige_noNA[-tindex,]  # Create test set

# Exploratory
plot(trainPrestige$prestige, trainPrestige$education) #Trend
plot(trainPrestige$prestige, trainPrestige$income) #No trend
plot(trainPrestige$prestige, trainPrestige$women) #No trend


# ---------------------------------------------------------------

# Predict prestige
lm2 <- lm(prestige~., data=trainPrestige)
summary(lm2)
#Call:
#  lm(formula = prestige ~ ., data = trainPrestige)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-13.7864  -4.0290   0.8807   4.5369  16.9482 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.544e+01  9.901e+00  -1.560  0.12492    
#education    4.562e+00  8.320e-01   5.483 1.24e-06 ***
#income       9.607e-04  3.204e-04   2.999  0.00415 ** 
#women        7.252e-03  4.543e-02   0.160  0.87379    
#census       1.031e-03  7.390e-04   1.396  0.16876    
#typeprof     5.981e+00  5.773e+00   1.036  0.30495    
#typewc      -1.137e+00  3.962e+00  -0.287  0.77531    
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 7.145 on 52 degrees of freedom
#Multiple R-squared:  0.8406,  Adjusted R-squared:  0.8222 
#F-statistic: 45.71 on 6 and 52 DF,  p-value: < 2.2e-16

# The predicted vs. residual plot confirm a good distribution
plot(lm2$fitted, lm2$residuals)

# Plot by index (row of data set)
# Note: there seems to be NO trend based on row #
plot(lm2$residuals,pch=19)

# Index plot shows no trend
# Use the trained model to predict the output of test set
predict2 <- predict(lm2, newdata=testPrestige)

# 
cor(predict2, testPrestige$prestige)
#[1] 0.9151361

rs <- residuals(lm2)
qqnorm(rs)      # Quantile-quantile plot
qqline(rs)


# Plot predicted vs. actual in test set
# Use type to explore a post mortem of the analysis
plot(testPrestige$prestige,predict2, pch=c(testPrestige$type))
legend('topleft', legend=c("bc", "prof", "wc"), pch=c(1,2,3), bty='o')



