# Diamonds..
# Import the dataset
diamonds.df <- read.delim(file=file.choose(),sep="\t")
# Get only Fair/Very Good/Ideal = 1/3/5
diamonds.st.df <- diamonds.df[which(diamonds.df$cut_now==1|diamonds.df$cut_now==3|diamonds.df$cut_now==5),]
# Create dummy variables for all these variables
low_quality <- rep(0,length(diamonds.st.df$cut_now))
medium_quality <- low_quality
high_quality <- low_quality
for(i in 1:length(diamonds.st.df$cut_now)){
  low_quality[i]<-ifelse(diamonds.st.df$cut_now[i]==1,1,0)
  medium_quality[i]<-ifelse(diamonds.st.df$cut_now[i]==3,1,0)
  high_quality[i]<-ifelse(diamonds.st.df$cut_now[i]==5,1,0)
}
diamonds.st.df['q0'] <- as.factor(low_quality)
diamonds.st.df['q1'] <- as.factor(medium_quality)
diamonds.st.df['q2'] <- as.factor(high_quality)
# We will be conducting 1-way anova to check if there is a significant
# Difference between the prices of diamonds according to their qualities.
diamonds.glm1 <- lm(price~q0+q1+q2,data=diamonds.st.df)
summary(diamonds.glm1)
diamonds.st.df['cut_now_f']<- as.factor(diamonds.st.df$cut_now)
diamonds.glm2 <- lm(price~cut_now_f,data=diamonds.st.df)
summary(diamonds.glm2) # This looks better than the one with dummies (glm1)
# Altough they are exactly the same model.

# Our hypothesis H0 was that mean(f1)=mean(f3)=mean(f5)
# F-statistic: 12.19 on 2 and 3316 DF,  p-value: 5.332e-06
# Means it is rejected. There is significant difference between
# Some types of diamonds, which we will investigate right now.

# Model diagnostics
# Check homogenous variances ( Levene Test )
leveneTest(price~cut_now_f,data=diamonds.st.df) # Homogeneity is rejected
# Lets check if still robust. (Max(var)<=5*Min(var))
var_1 <- var(diamonds.st.df$price[which(diamonds.st.df$cut_now_f==1)],)
var_3 <- var(diamonds.st.df$price[which(diamonds.st.df$cut_now_f==3)],)
var_5 <- var(diamonds.st.df$price[which(diamonds.st.df$cut_now_f==5)],)
ifelse(var_1*5<=var_5|var_1*5<=var_3,0,1) # Robustness also failed.
# Now we have to oneway.test because there is no homogenity of variances
oneway.test(price ~ cut_now_f, data=diamonds.st.df, var.equal = FALSE)
# Still equality is rejected.

# Check normality within cell residuals ( Shapiro )
shapiro.test(diamonds.glm2$residuals)
hist(diamonds.glm2$residuals) # Normality is also rejected.
# Can it not be considered Normal from CLT?
length(which(diamonds.st.df$cut_now_f=="1"))
length(which(diamonds.st.df$cut_now_f=="3"))
length(which(diamonds.st.df$cut_now_f=="5"))
# All of these are significantly bigger than 25. We could just
# bootstrap them and they would be normal then.

# Check influential points ( Cooks )
plot(cooks.distance(diamonds.glm2)) # No influential points
# Before doing multiple comparisons in R, use aov()
diamonds.aov1 <- aov(price~cut_now_f,data=diamonds.st.df)
summary(diamonds.aov1)
# Now do multiple comparisons with Tukey test
diffs <- TukeyHSD(diamonds.aov1,conf.level = 0.95)
diffs
plot(diffs) # Its obvious that 5-1 and 3-1 pairs have significant differences whereas 5-3 pair does not.


# Since we can not transform response variable we can try Kruskal-Wallis test
#kruskal.test(price~cut_now_f,data=ordered.diamonds.df) # Now it is not rejected.
#library(doBy) Test already orders them itself.
#ordered.diamonds.df<-orderBy(~price,data=diamonds.st.df)

# Question 2
# Lets first take IF only
# IF has spaces around so fix that using grepl:
diamonds.df.2 <- diamonds.df[which(grepl("IF",diamonds.df$clarity)),]
diamonds.lm1 <- lm(diamonds.df.2$price~diamonds.df.2$x+diamonds.df.2$heavy+I(diamonds.df.2$x*diamonds.df.2$heavy))
summary(diamonds.lm1)
diamonds.rs<- rstandard(diamonds.lm1)
diamonds.fit <- fitted(diamonds.lm1)
plot(diamonds.rs~diamonds.fit)

# 2b- it is not significantly different because the interaction term is not significant
# I(diamonds.df.2$x * diamonds.df.2$heavy)  -385.68    2077.60  -0.186    P = 0.853 >>> 0.05


# 2c- remove the interaction term
diamonds.lm2 <- lm(diamonds.df.2$price~diamonds.df.2$x+diamonds.df.2$heavy)
summary(diamonds.lm2) # H0 = at the same level of length, heavy is significant. 
# It is significant at p=0.1 level. Not significant at p=0.05 level though.


# 2d- Lets check what the fitted values look against residuals looks like in order to check that
diamonds.rs<- rstandard(diamonds.lm2)
diamonds.fit <- fitted(diamonds.lm2)
plot(diamonds.rs~diamonds.fit) # There is a pattern actually it is not random.
# Which means there should be another term. Lets try addibg an X^2 term
d.length <- diamonds.df.2$x
d.heavy <- diamonds.df.2$heavy
diamonds.lm3 <- lm(diamonds.df.2$price~d.length+I(d.length*d.length)+d.heavy+log(d.length))
summary(diamonds.lm3)
diamonds.rs<- rstandard(diamonds.lm3)
diamonds.fit <- fitted(diamonds.lm3)
plot(diamonds.rs~diamonds.fit) # THERE IS SOME TERM HERE I CANT FIND IT WTF. Anyway



# 3.1 We will do Anova here !! 2 Sample test is appropriate here.
# Best color
diamonds.best.df <- diamonds.df[which(grepl("D",diamonds.df$color)),]
# Is average width Y sig. bigger than avg. length X.
# H0 = mean(Y) - mean(X) > 0
# Check normality of group before t test
shapiro.test(diamonds.best.df$y)
shapiro.test(diamonds.best.df$x) # But again CLT comes to help
# Check homogeneity of variances
var.test(diamonds.best.df$y,diamonds.best.df$x) # Variances are homogenous
t.test(diamonds.best.df$y,diamonds.best.df$x,var.equal=TRUE,alternative="greater")
# p-value = 0.3717 so it is not significantly different
# Worst color
diamonds.worst.df <- diamonds.df[which(grepl("I",diamonds.df$color)),]
# Check homogeneity of variances
var.test(diamonds.worst.df$y,diamonds.worst.df$x) # Variances are homogenous
t.test(diamonds.worst.df$y,diamonds.worst.df$x,var.equal=TRUE,alternative="greater")
# p-value = 0.5334 so it is not significantly different
# Visualize p ? ???



# 3.2 Power
delta <- 0.005
sd <- 0.05
power.t.test(n=length(diamonds.worst.df$y),delta=delta,sd=sd, sig.level = 0.95, power=NULL,
             type="two.sample",alternative="two.sided") # power = 0.9353475 meaning
# There is 93.5% probability that null hypothesis will be rejected when true difference in
# means is 0.005

# Check for sample size
power.t.test(n=NULL,delta=delta,sd=sd, sig.level = 0.95, power=0.8,
             type="two.sample",alternative="two.sided") # n = 163.5628= 164.



# Question 4
# Diamonds with color D
diamonds.df.d <- diamonds.df[which(grepl("D",diamonds.df$color)),]
diamonds.df.d.len <- length(diamonds.df.d$color)
diamonds.df.d.sum <- length(which(diamonds.df.d$heavy==1))
# E
diamonds.df.e <- diamonds.df[which(grepl("E",diamonds.df$color)),]
diamonds.df.e.len <- length(diamonds.df.e$color)
diamonds.df.e.sum <- length(which(diamonds.df.e$heavy==1))
# Are they different?
prop_e <- diamonds.df.e.sum/diamonds.df.e.len
prop_d <- diamonds.df.d.sum/diamonds.df.d.len
prop_e
prop_d
x <- c(diamonds.df.e.sum,diamonds.df.d.sum)
y <- c(diamonds.df.e.len,diamonds.df.d.len)
prop.test(x,y) # They are significantly different with p-value = 0.001757

# Question 5 - 1)C , 2)iii




