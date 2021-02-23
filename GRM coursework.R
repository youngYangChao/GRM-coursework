# GRM coursework
# read the file
petroleum = read.csv("petroleum.csv")
# extract the explanatory variables
x1 = petroleum$gravity
x2 = petroleum$pressure
x3 = petroleum$dist_point
x4 = petroleum$fraction
# extract the response variable
y = petroleum$yield

# summary of data
summary(x1)
summary(x2)
summary(x3)
summary(x4)
summary(y)

# correlation plot
par(mfrow = c(2, 3))
plot(x1, x2)
plot(x1, x3)
plot(x1, x4)
plot(x2, x3)
plot(x2, x4)
plot(x3, x4)

# draw scater plots
par(mfrow = c(2, 2))
plot(x1, y, xlab = "gravity(x1)", ylab = "yield", cex.main = 1.2,
     main = "Scatter plot for gravity and yield")
plot(x2, y, xlab = "pressure(x2)", ylab = "yield", cex.main = 1.2,
     main = "Scatter plot for pressure and yield")
plot(x3, y, xlab = "dist_point(x3)", ylab = "yield", cex.main = 1.2,
     main = "Scatter plot for dist_point and yield")
plot(x4, y, xlab = "fraction(x4)", ylab = "yield", cex.main = 1.2,
     main = "Scatter plot for fraction and yield")

# linear regression
fit = lm(y ~ x1 + x2 + x3 + x4)
summary(fit)

# draw Q-Q plots and other residuals plots
par(mfrow = c(2, 2))
plot(fit)

# draw residual plots
par(mfrow = c(1, 1))
residuals = residuals(fit)
plot(residuals, main = "Residuals vs Index")
abline(h = 0, lwd = 2, lty = 3, col = "red")

# remove "pressure"
fit_1 = lm(y ~ x1 + x3 + x4)
summary(fit_1)

# convert the yield to log yield
fit_2 = lm(y_log ~ x1 + x2 + x3 + x4)
summary(fit_2)
plot(fit_2)

par(mfrow = c(1, 2))
hist(y, col = "steelblue")
hist(log(y), col = "steelblue")

# draw residual plots
par(mfrow = c(1, 1))
residuals = residuals(fit)
plot(residuals, main = "Residuals vs Index")
abline(h = 0, lwd = 2, lty = 3, col = "red")

# create a new variable D and remove the intercept
new_variable = x2/x3
fit_3 = lm(log(y) ~ x1 + new_variable + x4 - 1)
summary(fit_3)
par(mfrow = c(2, 2))
plot(fit_3)
# draw residual plots
par(mfrow = c(1, 1))
residuals = residuals(fit_3)
plot(residuals, main = "Residuals vs Index")
abline(h = 0, lwd = 2, lty = 3, col = "red")

# prediction
x1_new = mean(x1);x1_new
x2_new = mean(x2);x2_new
x3_new = mean(x3);x3_new
x4_new = mean(x4);x4_new
D_new = x2_new/x3_new
predict(fit, data.frame(x1 = 39.25, x2 = 4.18125, x3 = 241.5, x4 = 332.0938), 
        interval = "confidence")
predict(fit_3, data.frame(x1 = 39.25, new_variable = 0.01731366, x4 = 332.0938), 
        interval = "confidence")
