# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 2 Problems
# Problem 1

# the data set for these exorcises is in:
library(alr3)




# Problem 1

# gives height in cm and weight in kg for a sample of n= 10 18 year old girls
str(htwt)

# 2.1.1 
# make a scatter plot with Wt on the verticle axis
	# does a simple linear regression make sense in this case?
with(htwt, 
	plot(Ht, Wt)
	)

model <- with(htwt, lm(Wt ~ Ht))

with(htwt, 
	plot(Ht, Wt, 
		xlab = "height in cm", 
		ylab = "weight in kg")
	)
abline(model)


# 2.1.2
x_bar <- with(htwt, 
	mean(Ht)
	)

x_bar # 165.52

y_bar <- with(htwt, 
	mean(Wt)
	)

y_bar # 59.47

c_htwt <- htwt
c_htwt$near_SXX <- (c_htwt$Ht - x_bar)^2

c_htwt$near_SYY <- (c_htwt$Wt - y_bar)^2

c_htwt$near_SXY <- (c_htwt$Ht - x_bar)*(c_htwt$Wt - y_bar)

SXX <- sum(c_htwt$near_SXX) # 472.076
SYY <- sum(c_htwt$near_SYY) # 731.961
SXY <- sum(c_htwt$near_SXY) # 274.786

beta_hat_1 <- SXY/SXX
beta_hat_1 # 0.58208

beta_hat_0 <- y_bar - beta_hat_1*x_bar
beta_hat_0 # -36.87588

summary(model) # Intercept of -36.8759 and Slope of 0.5821, just like the hand calculations

with(htwt, 
	plot(Ht, Wt, 
		xlab = "height in cm", 
		ylab = "weight in kg")
	)
abline(beta_hat_0, beta_hat_1)


# 2.1.3
	# calculate the estimate of sigma^2

RSS <- SYY - SXY^2/SXX
RSS 

sigma_hat2 <- RSS/(nrow(c_htwt)-2)
sigma_hat <- sqrt(sigma_hat2)


se_beta_1 <- sqrt(sigma_hat2 * (1 / SXX))
se_beta_0 <- sqrt(sigma_hat2 * ((1/nrow(c_htwt) + ((x_bar)^2)/SXX)))

se_beta_1
se_beta_0

cov_beta_0_beta_1 <- -1*sigma_hat2*(x_bar/SXX)

# t-tests for beta_0 = 0 and beta_1 = 0

t_0 = beta_hat_0 / se_beta_0
t_1 = beta_hat_1 / se_beta_1

# compute the p-values for these estimates ... compare to the t(n-2) = t(8) distribution
2 * pt(-abs(t_0),nrow(c_htwt)-2) # 0.58305
2 * pt(-abs(t_1),nrow(c_htwt)-2) # 0.17310

summary(model)


# 2.1.4
	# calculate the F statistic
	# show numerically that F = t^2
		# where t was computed from 2.1.3 for testing beta_1 = 0

F <- ((SYY-RSS)/1)/sigma_hat2
F

# compare the significant digits, since they are 'slightly' different, but essentially identical
signif(F) == signif(t_1^2)