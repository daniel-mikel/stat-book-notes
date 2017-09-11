# solutions also provided:
	# http://www.waxworksmath.com/Authors/N_Z/Weisberg/weisberg.html



library("alr4")

# [1] Smallmouth Bass Data

# compute the means and variences for each of the eight subpopulations in the smallmouth bass data
(meanLength <- with(wblake, tapply(Length, Age, mean)))
(varLength <- with(wblake, tapply(Length, Age, var)))

# plot the standard deviations vs. age
	# if the variance function is constant, then the plot should be a null plot
sdLength <- with(wblake, tapply(Length, Age, sd))

par(mfrow=c(2, 1))
plot(Length ~ Age, data = wblake)
abline(with(data = wblake, lm(Length ~ Age)))
plot(1:8, sdLength)




# [2] Mitchell Data

# plotting the data
with(data = Mitchell, plot(Month, Temp))

# put a line through it
	# the data makes more sense this way...
with( data = Mitchell, lines(Month, Temp, type = "l"))





# [3] United Nations
	# data on GNP and Birth Rate in the year 2000
		# for 193 UN member countries
str(UN1)

par(mfrow=c(2, 1))
model1 <- with(data = UN1, lm(Fertility ~ PPgdp))
with(data = UN1, plot(Fertility ~ PPgdp))
abline(model1)
plot(residuals(model1) ~ PPgdp, UN1)
abline(a = 0, b = 0, lty=2)

# a linear model performs poorly over the raw data
	# as evidence by just looking at the scatterplot
	# even more obvious when looking at the residuals

# we apply a log base 2 to the data
	# much more appropriate

logUN1 <- log(UN1, base = 2)

par(mfrow=c(2, 1))
model2 <- with(data = logUN1, lm(Fertility ~ PPgdp))
with(data = logUN1, plot(Fertility ~ PPgdp))
abline(model2)
plot(residuals(model2) ~ PPgdp, logUN1)
abline(a = 0, b = 0, lty = 2)

summary(model1)
summary(model2)




# [4] Old Faithful
	# one month of data on the intervals between old faithful eruptions
	# Duration is given in seconds
	# Interval is given in minutes

str(oldfaith)

par(mfrow=c(2, 1))
mof <- with(data = oldfaith, lm(Interval ~ Duration))
with(data = oldfaith, plot(Interval ~ Duration, 
	xlab = "Duration (in Seconds)", ylab = "Interval (in Minutes)"))
abline(mof)
plot(residuals(mof) ~ Duration, oldfaith)
abline(a = 0, b = 0, lty = 2)



# [5] Water run-off in the Sierras
	# 43 years of percipitation measurements taken at six sites


str(water)
with(data = water, pairs(BSAAM~APMAM+APSAB+APSLAKE+OPBPC+OPRC+OPSLAKE))