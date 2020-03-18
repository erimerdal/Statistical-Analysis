## Project: calorie burning

##Erim Erdal (r0772570) - Jaldert Francois (r0653709) - Pinar Onat (r0772819) - Alicia Hernandez Gimenez (r0734014)


## Exploring the data and descriptive statistics
library(pastecs)
cal.df <- read.table(file=file.choose())
names(cal.df)[names(cal.df) == "V1"] <- "weight"
names(cal.df)[names(cal.df) == "V2"] <- "calhour"
names(cal.df)[names(cal.df) == "V3"] <- "calories"

cal.df <- cal.df[-c(1), ]
cal.df['weight'] <- as.numeric(as.character(cal.df$weight))
cal.df['calhour'] <- as.numeric(as.character(cal.df$calhour))
cal.df['calories'] <- as.numeric(as.character(cal.df$calories))

head(cal.df)
stat.desc(cal.df, basic = F)

cal.df.omitted <- na.omit(cal.df)
plot(cal.df.omitted, main="Comparison of variables")
apply(cal.df.omitted, 2, FUN = shapiro.test)
(cor(cal.df.omitted, method = "spearman"))

library(grid)
library(gridExtra)
par(mfrow=c(1,3))
plot1<-plot(density(cal.df.omitted$weight), main = "weight")
plot2<-plot(density(cal.df.omitted$calhour), main = "calhour")
plot3<-plot(density(cal.df.omitted$calories), main = "calories")

library(grid)
library(ggplot2)
par(mfrow=c(1,2))
boxplot(calories~weight, data=cal.df, na.rm=T)
boxplot(calories~calhour, data=cal.df, na.rm=T)

### Graphical summary of missing data

library(VIM)
data.df.aggr <- aggr(cal.df,numbers=TRUE, prop=FALSE, ylab=c("missing measurements","missing measurements"))
vp6 <- viewport(width = 1, height = 0.5, x = 0.25, y= 0.5)
print(data.df.aggr, vp=vp6)

aggr(cal.df, combined=TRUE, numbers = TRUE, prop = TRUE, cex.numbers=1, varheight = FALSE)

par(mfrow=c(1,2))
barMiss(cal.df[,c("weight", "calories")], ylab = "# Calories observations")
barMiss(cal.df[,c("calhour", 'calories')], ylab = "# Calories observations")

##Complete case analysis
cal.lm1 <- lm(calories~weight+calhour+I(weight*calhour), data = cal.df.omitted)
summary(cal.lm1)))

cal.lm2 <- lm(calories~weight+calhour, data = cal.df.omitted)
summary(cal.lm2)
cal.lm3 <- lm(calories~calhour, data = cal.df.omitted)
summary(cal.lm3)
cal.lm4 <- lm(calories~weight, data = cal.df.omitted)
summary(cal.lm4)

library(lmtest)
waldtest(cal.lm1, cal.lm2)
waldtest(cal.lm1, cal.lm2)

par(mfrow = c(2, 2))
plot(cal.lm1)

cal.lm1.fit <- fitted(cal.lm1)
cal.lm1.res <- rstandard(cal.lm1)
par(mfrow=c(2,2))
plot(cal.lm1.res~cal.df.omitted$weight)
plot(cal.lm1.res~cal.df.omitted$calhour) # This pattern is not random TODO:?
plot(cal.lm1.res~cal.lm1.fit) # seems like there is a relationship

shapiro.test(cal.lm1.res)

hist(cal.lm1.res, prob=TRUE)

plot(cooks.distance(cal.lm1))

## Multiple Imputation

library(mice)
library(VIM)
imp <- mice(cal.df, m = 300, print=FALSE, method = "norm")
summary(imp)
imp$imp$calories[, 1:3]

com <- complete(imp, "long", inc=T)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$calories))],101)
stripplot(calories~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=0.5, xlab="Imputation number")

fit1 <- with(imp, lm(calories~calhour+weight+(calhour*weight)))
fit2 <- with(imp, lm(calories~calhour+weight))
fit3 <- with(imp, lm(calories~calhour))
fit4 <- with(imp, lm(calories~weight))
fit5 <- with(imp, lm(calories~1))
print("Model with interaction - model without interaction")
D1(fit1, fit2)
print("model without interaction - model without calhour")
D1(fit2, fit4)
print("model without interaction - model without weight")
D1(fit2, fit3)
print("model with calhour - intercept model")
D1(fit3, fit5)

fit1 <- with(imp, lm(calories~calhour+weight+(calhour*weight)), conf.int = TRUE)
combined <- pool(fit1)
summary(combined, conf.int = TRUE)
pool.r.squared(fit1)

fit3 <- with(imp, lm(calories~calhour+weight), conf.int = TRUE)
combined <- pool(fit3)
summary(combined, conf.int = TRUE)
pool.r.squared(fit3)

## Inverse Probability Weighting 

cal.df$r<-as.numeric(!is.na(cal.df$calories))
# cal.df$calhour<-relevel(cal.df$calhour,ref="56") relevel to check if 13 is significant
## Fitting the logistic regression model to calculate the probabilities of being complete >
cal.ipw.glm<-glm(r ~ calhour+weight+(calhour*weight), data=cal.df,family=binomial)
summary(cal.ipw.glm) 
cal.df$w<-1/fitted(cal.ipw.glm)

lm1.ipw<- glm(calories ~ weight+calhour+I(calhour*weight), data=cal.df, family=gaussian, weights=cal.df$w)
summary(lm1.ipw)
lm2.ipw<- glm(calories ~ weight+calhour, data=cal.df, family=gaussian, weights=cal.df$w)
lm3.ipw<- glm(calories ~ calhour, data=cal.df, family=gaussian, weights=cal.df$w)
lm4.ipw<- glm(calories ~ weight, data=cal.df, family=gaussian, weights=cal.df$w)

library(lmtest)
waldtest(lm1.ipw, lm2.ipw)


