# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 3 Problems
# Problem 1

# the data set for these exorcises is in:
library(alr3) 

# data
	# UN2 already comes with a log transformation
attach(UN2)
str(UN2)

OLS <- lm(logPPgdp ~ logFertility)

Multiple <- lm(logPPgdp ~ logFertility + Purban)

summary(OLS)
summary(Multiple)