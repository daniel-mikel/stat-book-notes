# solutions to Weisberg "Applied Linear REgression" 3rd Edition
# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html

# Chapter 3 Problems
# Problem 1

# the data set for these exorcises is in:
library(alr3) 

attach(BGSgirls)
str(BGSgirls)

# 3.1.1 for girls only
	# draw a scatterplot of all age two variables
		# all age 9 variables and Soma

pairs(Soma~WT2+HT2+WT9+HT9+LG9+ST9)

# 3.1.2 obtain the frou pltos equivalent to Figure 3.1 in the text
	# do: E(Soma|WT9), E(Soma|LG9), LG9 ~ WT9, and E(Soma|LG9, WT9)

par(mfrow=c(2, 2))
plot(WT9 ~ Soma, xlab="WT9", ylab="Soma")
abline(lm(WT9 ~ Soma))

plot(LG9 ~ Soma, xlab="LG9", ylab="Soma")
abline(lm(LG9 ~ Soma))

plot(LG9 ~ WT9, xlab="WT9", ylab="LG9")
abline(lm(LG9 ~ WT9))

m0 <- lm(Soma ~ WT9)
ma <- lm(LG9 ~ WT9)
plot(ma$residuals, m0$residuals, xlab="e_hat LG9 on WT9", ylab="e_hat Soma on WT9")
abline(lm(m0$residuals ~ ma$residuals))

# 3.1.3 fit the multiple linear regression model with the mean function
	# Soma ~ HT2, WT2, HT9, WT9, ST9
	# below, 'm' is our target multiple regression
	# 'm0' is the 'intercept', no covarites

m <- lm(Soma ~ HT2 + WT2 + HT9 + WT9 + ST9, data = BGSgirls)
m0 = update( m, ~. - HT2 - WT2 - HT9 - WT9 - ST9 ) # get a regression model on just a constant ... 

anova(m0, m) # gives us an ANOVA table
summary(m)	# note the similarity to above

# 3.1.4/3.1.5
	# obtain an ANOVA table for both the 3.1.3 and 3.1.3 withe the covariates in the reverse sequence
	# we'll see that reversing the sequence of the covarietes DOES NOT affect the values in the regressions

mLR <- lm(Soma ~ HT2 + WT2 + HT9 + WT9 + ST9, data = BGSgirls)
mRL <- lm(Soma ~ HT2 + WT2 + HT9 + WT9 + ST9, data = BGSgirls)

anova(mLR, mRL)

summary(mLR)
summary(mRL)

