# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 2 Problems
# Problem 2

# the data set for these exorcises is in:
library(alr3)



# 2.2.1
	# as an alternative to Fores experiments: use the Clausius-Clapeyron formula of classic thermodynamics
	# plot u1 versus Lpres

data(forbes)
str(forbes)

Ktemp <- 255.37 + (5/9) * forbes$Temp

forbes$u1 <- 1/Ktemp

# compare farenheight and kelvin temp
par(mfrow=c(2, 1))
plot(forbes$Temp, forbes$Lpres)
plot(forbes$u1, forbes$Lpres)



# 2.2.2
	# compute the linear regression with the Clausius-Clapeyron formula
m_cc <- lm(forbes$Lpres ~ forbes$u1)
summary(m_cc)



# 2.2.3
	# compare the Clausius-Clapeyron formula with Forbes' experiments

m_f <- lm(forbes$Lpres ~ forbes$Temp)

# compare residuals of the two linear models
par(mfrow=c(2, 1))
plot(residuals(m_f) ~ forbes$Temp, forbes)
plot(residuals(m_cc) ~ forbes$u1, forbes)

# can also be compaired with:

p_f  <- predict(m_f)
p_cc <- predict(m_cc)

diff <- p_f - p_cc


par(mfrow=c(2, 1))
plot(p_f, p_cc, xlab = "Lpres from Forbes", ylab = "Lpres from Clausius-Clapeyron" )
abline(a = 0, b = 1)
plot(p_f, diff, xlab = "Lpres from Forbes", ylab = "E(Lpres|Forbes) - E(Lpres|Clausius-Clapeyron)")
abline(h = 0)



# 2.2.4

str(hooker)

Ktemp <- 255.37 + (5/9) * hooker$Temp
hooker$u1 <- 1/Ktemp

hooker$Lpres <- 100 * log10(hooker$Pressure) 

m_hooker <- lm(hooker$Lpres ~ hooker$u1)

# get the predictions under this model
y_hat <- predict(m_hooker)
y_hat

# view the data, the lm fit, and residual plot
par(mfrow=c(2, 1))
plot(hooker$Lpres ~ hooker$u1)
abline(m_hooker)
plot(residuals(m_hooker) ~ hooker$u1)
abline(h = 0)
# dev.off()


# 2.2.5

# calculate the predicted values for each value of y
y_hat <- predict(m_hooker)

# calculate the standard error of prediciton

SXX <- sum((hooker$u1 - mean(hooker$u1))^2)
SYY <- sum((hooker$Lpres - mean(hooker$Lpres))^2)
SXY <- sum((hooker$u1 - mean(hooker$u1)) * (hooker$Lpres - mean(hooker$Lpres))) 

RSS <- SYY - ((SXY^2)/SXX)

sigma_hat <- sqrt(RSS/(nrow(hooker)-2))

sepred <- sigma_hat * sqrt( 1 + 1/nrow(hooker) + (hooker$u1 - mean(hooker$u1) )^2 / SXX )

z <- (hooker$Lpres - y_hat)/sepred
mean(z)
sqrt(var(z))