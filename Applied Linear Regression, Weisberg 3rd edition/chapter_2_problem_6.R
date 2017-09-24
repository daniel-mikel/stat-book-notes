
# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 2 Problems
# Problem 6

# the data set for these exorcises is in:
library(alr3)


# 2.6.1 
	# using base 10 logs:
		#compute an ols 
		# get an analysis of varience table


logUN1 <- log(UN1, base = 10)
model <- lm(Fertility ~ PPgdp, data = logUN1)

summary(model)
anova(model)

# 2.6.2 
	# draw the summary line, add the fitted graph

par(mfrow=c(2, 1))
plot(Fertility ~ PPgdp, data = logUN1)
abline(model)
plot(residuals(model) ~ PPgdp, logUN1)
abline(a = 0, b = 0, lty = 2)

dev.off()

# 2.6.3
	# test the hypothesis that the slope is zero vs that it is negative
		# a one sided test
	# give the significance level fo the test

# t-test as:
	# t^2 = Beta_hat_1 / standard_error(Beta_hat_1)^2
model$coefficients[2] # beta_hat_1
coef(summary(model))[, "Std. Error"][2] # Beta_hat_1 standard error

t <- as.numeric(
	model$coefficients[2] / coef(summary(model))[, "Std. Error"][2]
	)
n <- nrow(logUN1)

pt(t, n-2)

# 2.6.4
	# give the value of the Coefficient of Determination
	# explain its meaning

summary(model) # R^2 is 0.4563, 45.6% of the variablility explained by the model

# 2.6.5
	# for a locality not in the data with PPgdp = 20000
	# obtain a prediction and a 95% prediction interval
	# convert these to a prediction of utility (not in log form!)

est <- predict(model, 
	data.frame(PPgdp = c(log(20000, base = 10))), 
	interval = "confidence", 
	level = 0.95)

# get these out of log and into nominal values

est_y <- c(10^(est[2]), 10^(est[1]), 10^(est[3]))
est_x <- c(20000, 20000, 20000)

plot(Fertility ~ PPgdp, data = UN1)
points(x = est_x, y = est_y, col = 2)


est_y <- c(est[2], est[1], est[3])
est_x <- c(log(20000, base = 10), log(20000, base = 10), log(20000, base = 10))

plot(Fertility ~ PPgdp, data = logUN1)
points(x = est_x, y = est_y, col = 2)
abline(model)


# 2.6.7:
# find the location of maximum fertility (Niger):

max_fert <- max(UN1$Fertility)
inds <- subset(UN1, UN1$Fertility == max_fert)
UN1$Locality[inds]

# find the location of minimum fertility (Hong.Kong):
min_fert <- min(UN1$Fertility)
inds <- UN1$Fertility == min_fert
row.names(inds)

# find the location of the two smallest residuals ... 
# indices: 6, 181

sr <- sort(model$residuals)
sr[1:2]

UN1[6,]   # Armenia, 
UN1[181,] # Ukraine

# find the location of the two largest residuals ...  
# indices: 55, 129 

sr <- sort(model$residuals, decreasing=TRUE)
sr[1:2]

UN1[55,]  # Equatorial.Guinea,
UN1[129,] # Oman
