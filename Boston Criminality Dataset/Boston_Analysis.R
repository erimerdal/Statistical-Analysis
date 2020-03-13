# Exam 1 
# Import the dataset boston2.txt
boston.df <- read.delim(file=file.choose(),sep="\t")
# Show summary of the data according to internatfact
# Import describeBy from psych
# packages.install("psych")
library(psych)
described <- describeBy(boston.df,group=boston.df['internatfact'])
# 7th row is rm
low.df <- boston.df[which(boston.df["internatfact"]=="low"),]
medium.df <- boston.df[which(boston.df["internatfact"]=="medium"),]
high.df <- boston.df[which(boston.df["internatfact"]=="high"),]
very_high.df <- boston.df[which(boston.df["internatfact"]=="very_high"),]
# Check sizes
length(low.df$rm)
length(medium.df$rm)
length(high.df$rm)
length(very_high.df$rm)
# All > 25 so by using CLT we can apply t-test.
# T-test assumes normality. Check if normality holds with shapiro-test
shapiro.test(low.df$rm)
shapiro.test(medium.df$rm)
shapiro.test(high.df$rm)
shapiro.test(very_high.df$rm)
# Histograms
par(mfrow=c(2,2))
hist(low.df$rm)
hist(medium.df$rm)
hist(high.df$rm)
hist(very_high.df$rm)
# Normality is rejected for all categories (However we can still apply t-test because CLT holds)
# Instead of t-test, we have to apply 
# Construct significance interval for n with known sd with one sample T test
t.test(low.df$rm,conf.level = 0.95)
t.test(medium.df$rm,conf.level = 0.95)
t.test(high.df$rm, conf.level = 0.95)
t.test(very_high.df$rm, conf.level = 0.95)
# Try to regress a linear model (response = crime rate, regressor = distance)
boston.lm <- lm(crim~dis,data=boston.df)
summary(boston.lm)
boston.pred1 <- predict(boston.lm,list(dis=c(3.123)),interval = "confidence")
boston.pred1
# Run model diagnostics.. R'2 = 0.14 which means only 14 percent of variability is
# explained by this model which is really bad but still lets check.
boston.rs <- rstandard(boston.lm)
boston.fit <- fitted(boston.lm)
par(mfrow=c(1,2))
# 1- Check for linearity
plot(boston.rs~boston.fit) # Plot standard residuals against fitted values
plot(boston.rs~boston.df$dis) # Plot standard residuals against each regressor
# Doesnt look random at all. It doesnt also reflect any easy distribution really.
# 2- Check normality of standard residuals
shapiro.test(boston.rs)
par(mfrow=c(1,1))
hist(boston.rs) # Normality is also rejected. Transformation of response may be required.
# 3- Check cooks distance
plot(cooks.distance(boston.lm)) # No influential points
# Trying to predict crimcat means its logistic regression because regressor is
# a categorical variable.
boston.glm1 <- glm(crimcat ~ dis+chas+rm+rad,data = boston.df)
summary(boston.glm1)
# Wait it doenst give the same results with models in question at all.
# Lets try to answer verbally.
# We can check if the difference between Residual Deviance is statistically significant.
# Chi-square goodness of fit can be used here.
deviance_difference = 273.8-105.36
df_difference = 502-501
# In the way of more extreme should be checked therefore lower.tail = FALSE
pchisq(deviance_difference,df_difference,lower.tail = FALSE) 
# It is significantly lower than 0.05,where H0 = (Bx = 0 for $rad) is rejected.
# Meaning $rad's addition to the model makes model significantly better.

# For the 4'th question we need to use 1-way Anova because we will be comparing if
# Means of crime are significantly different or not, depending on 1 variable which is
# if charles river is passing or not.

# Lets first describeBy and check descriptive statistics
boston.df['chas.factor'] <- as.factor(boston.df$chas)
boston.by_region <- describeBy(boston.df,group=boston.df$chas)
boston.by_region$`0`[2,]
boston.by_region$`1`[2,] 

boston.glm2 <- lm(crim~chas.factor,data=boston.df)
# Check model diagnostics
# Is there constant-within group variance
# packages.install("car")
library(car)
# 1- Check homogeinity of variance between groups
leveneTest(boston.df$crim~boston.df$chas.factor) # Homogenity is not rejected
# 2- Check normality of cell residuals
shapiro.test(boston.glm2$residuals)
hist(boston.glm2$residuals) # Residuals are not normally distributed. 
# We will have to use kruskal wallis non parametric alternative.
kruskal.test(boston.df$crim~boston.df$chas.factor) # Not rejected. Therefore there are no significant difference in average crime
# depending on if Charles River is passing or not.
# 3- Check for influential observations
plot(cooks.distance(boston.glm2)) # No influential observations.

# If data was distributed normally, we could've done:
summary(boston.glm2) 
# F-statistic: 1.579 on 1 and 504 DF,  p-value: 0.2094
# h0 is not rejected. Therefore there are no significant difference in average crime
# depending on if Charles River is passing or not.