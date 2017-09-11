

library("alr4")

# the following will plot the same graph
plot(dheight ~ mheight, data=Heights)

plot(Heights$mheight, Heights$dheight)

with(Heights, plot(mheight, dheight))


# this plots the rounded data
	# originally the data was rounded to the nearest inch
	# the Heights data comes pre-jittered to avoid overplotting
plot(round(dheight) ~ round(mheight), Heights)


# this subsets our data 
	# the result is a vector of values that is either TRUE or FALSE
		# TRUE if the value is in the specified subset
sel <- with(Heights,
	(57.5 < mheight) & (mheight <= 58.5) |
	(62.5 < mheight) & (mheight <= 63.5) |
	(67.5 < mheight) & (mheight <= 68.5))

plot(dheight ~ mheight, data = Heights, subset = sel)


# plots the inverse of our previous subset
plot(dheight ~ mheight, data=Heights, subset= !sel)


# the following allows the graphical window to hold an array of graphs
	# this has one row with two columns
oldpar <- par(mfrow=c(2, 1))

m0 <- lm(pres ~ bp, data=Forbes)
plot(pres ~ bp, data=Forbes, xlab="Boiling Point (deg. F)",
	ylab="Pressure (in Hg)")
abline(m0)
plot(residuals(m0) ~ bp, Forbes, xlab="Boiling Point (deg. F)",
	ylab="Residuals")
abline(a=0, b=0, lty=2)
par(oldpar)


# log(pressure) ~ boiling point
oldpar <- par(mfrow=c(2, 1))

m0 <- lm(log(pres) ~ bp, data=Forbes)
plot(pres ~ bp, data=Forbes, xlab="Boiling Point (deg. F)",
	ylab="Pressure (in Hg)")
abline(m0)
plot(residuals(m0) ~ bp, Forbes, xlab="Boiling Point (deg. F)",
	ylab="Residuals")
abline(a=0, b=0, lty=2)
par(oldpar)


# data on smallmouth bass
	# first command takes the average length at each age of bass
(meanLength <- with(wblake, tapply(Length, Age, mean)))
plot(Length ~ Age, data = wblake)
abline(lm(Length ~ Age, data = wblake))
lines(1:8, meanLength, lty = 2)


#
plot(Gain ~ A, turkey, xlab="Amount (percent of diet)",
	ylab="Weight gain (g)", pch=S)
legend("bottomright", inset=0.02, legend=c("1 Control", "2 New source A",
	"3 New source B"), cex=0.75, lty=1:3, pch=1:3, lwd=c(1, 1.5, 2))



# back to the Heights dataset
	# two models:
		# one a ols with daughter regressed on mother height
		# the second that mothers and daughters have the same height on average
model <- lm(dheight ~ mheight, data = Heights)
plot(dheight ~ mheight, data=Heights)
abline(model)
abline(a=0, b=1, lty=2)


# summary graphs (Anscombe 1973)
oldpar <- par(mfrow=c(2, 2))
xs <- names(anscombe)[1:4]
ys <- names(anscombe)[5:8]
for (i in 1:4){
	plot(anscombe[, xs[i]], anscombe[, ys[i]], xlab=xs[i], ylab=ys[i],
		xlim=c(4,20), ylim=c(2, 16))
	abline(lm( anscombe[, ys[i]] ~ anscombe[, xs[i]]))
}


