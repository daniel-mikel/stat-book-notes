# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 2 Problems
# Problem 4

# the data set for these exorcises is in:
library(alr3)

attach(heights)

# 2.4.1

model <- lm(Dheight ~ Mheight, data = heights)
summary(model)

plot(model)

par(mfrow=c(2, 1))
plot(Dheight ~ Mheight, data = heights)
abline(model, col = 2)
plot(residuals(model) ~ Mheight, data = heights)
abline(h = 0, col = 2)

# 2.4.2
	# obtain a 99% confidence interval for Beta_1

Mheight0 <- Mheight - mean(Mheight)
m0 <- lm(Dheight ~ Mheight0)
summary(m0)

# get the standard error of beta_1: 
n   <- length(m0$residuals)
RSS <- sum(m0$residuals ^ 2)
sigma_hat <- sqrt(RSS / (n - 2))
SXX <- sum((Mheight0 - mean(Mheight0)) ^ 2) 
se_beta_1 <- sigma_hat / sqrt(SXX) 

# get the confidence interval of beta_1:
#
alpha = 1 - 0.99
quant = qt(1 - alpha/2, n-2) 

m0$coefficients[2] - quant * se_beta_1
m0$coefficients[2] + quant * se_beta_1

# 2.4.3
# obtain a prediction and 99% population prediction interval for a daughter whose mother is 64 inches tall

predict(m0, 
	newdata=data.frame(Mheight0 = 64 - mean(Mheight)), 
	interval = "prediction", 
	level = 0.99)

