# Read and clean the data
cal.df <- read.table(file=file.choose())
names(cal.df)[names(cal.df) == "V1"] <- "weight"
names(cal.df)[names(cal.df) == "V2"] <- "calhour"
names(cal.df)[names(cal.df) == "V3"] <- "calories"
cal.df <- cal.df[-c(1), ]
cal.df['weight'] <- as.numeric(as.character(cal.df$weight))
cal.df['calhour'] <- as.numeric(as.character(cal.df$calhour))
cal.df['calories'] <- as.numeric(as.character(cal.df$calories))

summary(cal.df) # Descriptive Statistics

# na.omit(cal.df) function removes all the rows that have NA values in it.
cal.df.omitted <- na.omit(cal.df)
summary(cal.df.omitted)
# Visual analysis
# Boxplot
par(mfrow=c(2,2))
boxplot(cal.df.omitted$weight, main = "weight")
boxplot(cal.df.omitted$calhour, main = "calhour")
boxplot(cal.df.omitted$calories, main = "calories")
pairs(cal.df.omitted) # Here no direct correlation is seen between values
# However it seems calhour is in different categories not exactly continuous.
# So maybe it will be better to transform it into a categorical variable 
# instead of a continuous variable?
# Also it can be seen that there is a slightly positive linear correlation between
# weight and calories burned but there are many outliers which indicates
# there is an interaction with calhour.

# some interesting additional plots
# density plots
par(mfrow=c(2,2))
plot(density(cal.df.omitted$weight), main = "weight")
plot(density(cal.df.omitted$calhour), main = "calhour")
plot(density(cal.df.omitted$calories), main = "calories")
# It seems as if in the weight plot that there are 2 separate common densities.
# This might indicate a difference between males and females which could have
# been a valuable data to consider in muscular work research because of
# the physiological differences.

# index plot of sorted values
par(mfrow=c(2,2))
plot(sort(cal.df.omitted$weight), main = "weight", cex = 1.2)
plot(sort(cal.df.omitted$calhour), main = "calhour", cex = 1.2)
plot(sort(cal.df.omitted$calories), main = "calories", cex = 1.2)
# Here it is more obvious that calhour is categorical.

# Test for normality
apply(cal.df.omitted, 2, FUN = shapiro.test)
# Normality is rejected for calhour and weight, therefore we will use 
# Spearman correlation coefficient
cal.cor <- cor(cal.df.omitted, method = "spearman")
cal.cor # Very strong positive correlation between workout intensity and
# calories burned (0.9385585), slightly positive correlation between
# weight of individual and calories burned (0.25486644).

# Carry out complete case analysis
cal.lm1 <- lm(calories~weight+calhour, data = cal.df.omitted)
summary(cal.lm1) # Both are significant
par(mfrow = c(2, 2))
plot(cal.lm1)
# Let's check linearity
cal.lm1.fit <- fitted(cal.lm1)
cal.lm1.res <- rstandard(cal.lm1)
par(mfrow=c(2,2))
plot(cal.lm1.res~cal.df.omitted$weight)
plot(cal.lm1.res~cal.df.omitted$calhour) # This pattern is not random TODO:?
plot(cal.lm1.res~cal.df.omitted) # seems like there is a relationship


# 3d scatterplot for only weight and calhour
# install.packages("scatterplot3d")
library(scatterplot3d)
fit <- lm(calories~weight+calhour, data = cal.df.omitted)
png("Scatter3D.png")
s3d <- scatterplot3d(cal.df.omitted$calhour,cal.df.omitted$weight,cal.df.omitted$calories, main="3D Scatterplot")
s3d$plane3d(fit, lty.box = "solid")
dev.off()

cal.lm2 <- lm(calories~weight+calhour+I(weight*calhour), data = cal.df.omitted)
summary(cal.lm2) # Interaction is significant, Explanatory power is increased
# from Multiple R-squared 0.9436 to  0.9684
cal.lm2.fit <- fitted(cal.lm2)
cal.lm2.res <- rstandard(cal.lm2)
par(mfrow=c(2,2))
plot(cal.lm2.res~cal.df.omitted$weight)
plot(cal.lm2.res~cal.df.omitted$calhour)
plot(cal.lm2.res~I(cal.df.omitted$calhour*cal.df.omitted$weight)) 
plot(cal.lm2.res~cal.lm1.fit) # now feels more random

# Checking normality of standardized residuals
par(mfrow=c(1,1))
shapiro.test(cal.lm2.res) # Normality is not rejected
hist(cal.lm2.res, prob=TRUE) 

# Check for outliers
plot(cooks.distance(cal.lm2)) # There is one influential point at index 11.
# We might try removing it and fitting again. However it is a questionable
# Approach since we do not have many observations. TODO: ?

# Let's see what happens when we use calhour variable as a categorical variable
# instead of a continuous variable.
cal.categorical <- cal.df
cal.categorical$calhour <- as.factor(cal.categorical$calhour)
cal.lm3 <- lm(calories~weight+calhour, data = cal.categorical)
summary(cal.lm3)
# May need to create dummy variable in order to include interaction term if calhour is categorical
# If we have greater explanatory power, we might have to do ANOVA to see which categorical
# groups are significantly different from each other etc.
# TODO: ?

# Let's explore missing data
#install.packages("mice")
#install.packages("lattice")
#install.packages("VIM")
#install.packages("aod")
#install.packages("BaM")
library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)
# Explore missingness with VIM library
cal.missing.aggr<-aggr(cal.df,numbers=TRUE,prop=FALSE,ylab=c("Histogram of missing data","Pattern"))
cal.missing.aggr
# Percentage of missing data
aggr(cal.df,combined=TRUE,numbers=TRUE,prop=TRUE,cex.numbers=0.87,varheight=FALSE)

# Amount of missingness in each calhour group
barMiss(cal.df[,c("calhour","calories")]) # This doesn't seem random.
# All of the missing data is from calhours between 10-20. And in the
# 9 observations that have calhour 10-20, we observe 8 is missing.

# If we turn calhour into categorical variable it will be easier to see.
barMiss(cal.categorical[,c("calhour","calories")])
# Here we can further see all of the group 13 is missing and %75 of group 19 
# is missing.

# Amount of missingness for each weight group
barMiss(cal.df[,c("weight","calories")]) # There seems no correlation here.

# I think our case is MAR (Missing at Random).
# because missingness can be described using observed subject variables, in our case
# Using variable "calhour" we can describe missingness.//
# Generally, under MAR, simple techniques like complete and available case analysis 
# and overall mean imputation, give biased results. //

# Multiple Imputation
# Talk about page 345 - Observations and warnings
# Use package MICE- Chained equations algorithm (CEA). CEA has been found to work well in a variety of simulation studies
pattern<-md.pattern(cal.df)
pattern
pairs<-md.pairs(cal.df)
pairs
# Impute missing values (100 iterations are made)
imp <- mice(cal.df,m=100)
imp
# Diagnostic checking (Data seems normal, close to other data, sensible.)
# Imputed values for calories
imp$imp$calories[,1:5]
# Check the first of the completed data (Put the imputed values in corresponding missing places)
complete(imp,1)
## It is often useful to inspect the distributions of original and the imputed
## data. The complete() function extracts the original and the imputed data
## sets from the imp object as a long (row-stacked) matrix. The col vector
## separates the observed (blue) and imputed (red) data for calories
com <- complete(imp, "long", inc=T)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$calories))],101)
stripplot(calories~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4,xlab="Imputation number")
# Im not sure how to comment on the stripplot above. I guess it shows imputed values do not sway
# much from average so they are diagnostic-wise fine. //
# Predictive mean matching method is used for imputation.

fit <- with(data=imp, exp=lm(calories ~ calhour))
MI.matrix<-matrix(0,5,4)
for(k in 1:5) MI.matrix[k,]<-coefficients(fit$analyses[[k]])
MI.results=data.frame(Intercept=MI.matrix[,1],weight=MI.matrix[,2],calhour=MI.matrix[,3],Interaction=MI.matrix[,4])
MI.results[1:5,] # Results vary crazily though ! (Unlike the one in Titanic)
## Combining the results using Rubin’s rule
est <- pool(fit)
summary(est)  # Gives awful p-values
pool.r.squared(fit)

# summary(cal.lm2) # Significant here # Extremely different estimates

# IPW
cal.df$r<-as.numeric(!is.na(cal.df$calories))
# cal.df$calhour<-relevel(cal.df$calhour,ref="56") relevel to check if 13 is significant
## Fitting the logistic regression model to calculate the probabilities of being complete >
cal.ipw.glm<-glm(r ~ weight+calhour+I(calhour*weight), data=cal.df,family=binomial)
summary(cal.ipw.glm) # Again bad results?
cal.df$w<-1/fitted(cal.ipw.glm)
cal.df
cal.results.ipw<- glm(calories ~ weight+calhour+I(calhour*weight), data=cal.df, family=gaussian, weights=cal.df$w)
summary(cal.results.ipw)
confint(cal.results.ipw, level = 0.95)

# What to do in this absurd situation?