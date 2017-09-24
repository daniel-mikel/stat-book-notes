# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 2 Problems
# Problem 5

# the data set for these exorcises is in:
library(alr3)

attach(wblake) # small mouth bass dataset


# 2.5

m <- lm(Length ~ Age)

summary(m)
anova(m)

par(mfrow=c(2, 1))
plot(Length ~ Age)
abline(m, col = 2)
plot(residuals(m) ~ Age, data = wblake)
abline(h = 0, col = 2)


# 2.5.1 
	# obtain 95% confidence intervals for the mean length at ages 2,4,6 
predict(m, 
	data.frame(Age = c(2, 4, 6)), 
	interval = "confidence", 
	level = 0.95)

# 2.5.2
	# obtain a 95% interval for mean length at age 9
	# explain why this interval is likely to be untrustworthy
predict(m, 
	data.frame(Age = c(9)), 
	interval = "confidence", 
	level = 0.95)

# 2.5.3
	# wblake2 has all the data for bass aged 1-8, plus a few older bass
	# show that a simple linear regression is inappropriate for this larger dataset

detach(wblake)
attach(wblake2) # small mouth bass dataset

m2 <- lm(Length ~ Age)

summary(m2)
anova(m2)

par(mfrow=c(2, 1))
plot(Length ~ Age)
abline(m2, col = 2)
plot(residuals(m2) ~ Age, data = wblake2)
abline(h = 0, col = 2)
