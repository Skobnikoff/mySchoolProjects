
# title: "Analysis of a financial asset"
# author: "Arthur Skobnikov"
# date: "Friday, March 30, 2015"
# description: "The final project for the "Systems and Signals" course. It has purly learning goals
#               and can not be used in a real analysis of a financial asset"


##### Given a dataset of prices of financial asset (time series)

X <- read.table("data.txt", header = TRUE, dec = ",")
X <- as.vector(X[, 1])
N <- length(X)
n <- 1:N
plot.ts(X, type='l', xlab = 'Time', ylab = 'Price' )


### PART 1 : create a model

# We suppose that process is linear and has a look: X = m + Y
  
#### Hypothesis 1 : model *m* is polynomial of order 1:  m = a + b*n
  
m1 <- lm(X ~ n)     # lm() is a built-in function for generating a model
summary <- summary(m1)
a <- summary$coefficients[1, 1]
b <- summary$coefficients[2, 1]
cat("model: ", "m =", a, "+", b, "* n")

plot.ts(X, type='l', col='gray', xlab = 'Time', ylab = 'Price' )
lines(n, predict(m1), lwd = 2, col = 'red')


# **Problem** : curve of linear model does not fit the data.
# Let's check the coeficient of determination R².
# (reminder: R² = Explained variation / Total variation)

r.squared <- summary$r.squared
cat("R²", r.squared)


# R² is almost zero. Variance of time `n` does not explain variance of price `X`. 
# **Conclusion** : Model does not fit the given data. 


#### Hypothesis 2 : model is polynomial of second order  m = a + b_1*n + b_2*n^2
# Repeating the same actions we did before gives us the following results :
  
m2 <- lm(X ~ poly(n, 2, raw=TRUE))
summary <- summary(m2)
a <- summary$coefficients[1, 1]
b1 <- summary$coefficients[2, 1]
b2 <- summary$coefficients[3, 1]
cat("model: ", "m =", a, "+", b1, "* n", "+", b2, "* n?")

plot.ts(X, type='l', col='gray', xlab = 'Time', ylab = 'Price' )
lines(n, predict(m2), lwd = 2, col = 'red')

r.squared <- summary$r.squared
cat("R² =", r.squared)


# This time we obtained the same problem as before. Model does not fit the given data. 
# Let's try again with another model.


#### Hypothesis 3 : model is polynomial of third order  m = a + b_1*n + b_2*n^2 + b_3*n^3

# Repeating the same actions we did before gives us the following results :

m3 <- lm(X ~ poly(n, 3, raw=TRUE))
summary <- summary(m3)
a <- summary$coefficients[1, 1]
b1 <- summary$coefficients[2, 1]
b2 <- summary$coefficients[3, 1]
b3 <- summary$coefficients[4, 1]
cat("model: ", "m =", a, "+", b1, "* n", "+", b2, "* n^2", "+", b3, "* n^3")

plot.ts(X, type='l', col='gray', xlab = 'Time', ylab = 'Price' )
lines(n, predict(m3), lwd = 2, col = 'red')


# This time curve of model fits much better the data than in previous cases.
# Let's compute R², adjusted R².

r.squared <- summary$r.squared
adj.r.squared <- summary$adj.r.squared
cat("R² =", r.squared)
cat("Adjusted R² =", adj.r.squared)


# Finally R² and adjusted R² show that there exists some dependancy between time `n` and price `X`.
# More precisely, about 4.6% of the valiation of the price `X` is explained by the change of the time. 

##### P-values of parameters :
p.a <- summary$coefficients[1, 4]
cat("p.a =", p.a)
p.b1 <- summary$coefficients[2, 4]
cat("p.b1 =", p.b1)
p.b2 <- summary$coefficients[3, 4]
cat("p.b2 =", p.b2)
p.b3 <- summary$coefficients[4, 4]
cat("p.b3 =", p.b3)



# P-values of parameters are almost zero and respect 5% confidence level.
# **Conclusion** : let's stop on this model and analyze it.

# Let's take a look on error between actual data and values predected by model :

Y <- X - predict(m3)
mean_error <- mean(Y)
cat("Mean of Y =", mean_error)
plot.ts(Y, main = "Errors Y", col='dark gray', xlab = 'Time', ylab = 'Delta of price' )


# Though mean of `Y` almost equals to zero, curve of `Y` has a similar shape of 
# initial data and does not really look like white noise.
# Nevertheless, let's compute and plot autocovariance of `Y` : 

autocov.Y <- acf(Y, type = "covariance", main="Autocovariance of Y", col = "blue")


# Plot of autocovariance of `Y` is supposed but does not look like Dirac signal at all, 
# so we can conclude that given model is not the best solution for the given data. 

# Non-linear models are not good neither. It means that time `n` itself does not really 
# explain the behavior of asset price `X` . There are other factors that may explain evolution of 
# price but they are not of our interest now.


### PART 2 : autoregressive model y_0 = (a_1*y_1 + a_2*y_2 + ... + a_p*y_p) + whiteNoise_0

# Goal is to create a filter which transforms white noise to signal `Y` (errors between real 
# and predicted values of price `X`).
# `ar()` function fits an autoregressive model to the data, by default selecting the complexity
# by Akaike Information Criterion. We apply Yule-Walker to find coefficients of the model.

temp <- ar.yw(Y)  # method = "yule-walker"
parameters <- temp$ar
print (parameters)


# So we have 2 parameters and AR(p=2). Model has a following look : 
# y_0 = 0.91628673*y_1 + 0.06145462*y_2 + + whiteNoise_0


AR2 <- function(y1, y2, parameters, whiteNoise){
y0 <- parameters[1]*y1 + parameters[2]*y2 + whiteNoise 
y0
}

whiteNoise <- rnorm((N-1), sd = sqrt(temp$var.pred))
y_1 <- Y[1:(N-1)]
y_2 <- Y[2:N]

Y_hat <- AR2(y_1, y_2, parameters, whiteNoise)

plot.ts(Y_hat, col='blue', main="White Noise vs Y(hat), AR(2)")
lines(whiteNoise, col='dark gray')


### PART 3 : Analysis of resuduals

# We've got a new signal `Y_hat`. Now we can compare it with `Y` and analyze a difference between them.

dif <- Y[1:(N-1)] - Y_hat
plot.ts(dif, col='dark gray', main="Difference between Y and Y(hat)")

cat("Mean of differences =", mean(dif))

# Autocorrelation of differences
autocov.dif <- acf(dif, type = "correlation",main="Autocorrelation of differences, AR(2)", col = "blue")


# Plot of autocorrelation of differences shows a perfect situation, we can recognize Dirac signal. 
# At point 0 autocorrelation of `dif` between y0 and y0 equals to 1, which is logic. 
# With lags bigger then 0 autocorrelation of `dif` is around zero. 
# So differencies between `Y` and `Y_hat` are independend.

##========== Summary ==========##

# **In part 1** of this work I was trying 3 different models to fit given data. 
# Least squares method was applied using built-in R function `lm()`. 
# Finally polynomial one of order 3 was chosen as the model with the best characteristics. 
# However there was a prooblem - errors `Y` between real and predicted values of `X` were not random. 
# I made such conclusion looking at the plot of `Y` and plot of autocovariance of `Y`. 
# Last one was supposed to show graph similar to Dirac, but it was not a case. 
# Nevertheless I stoped to analyze the last model.  
# 
# **In part 2** I found autoregressive model of errors `Y` using built-in R function `ar()`. 
# p=2 was computed as the most informative parameter of AR. 
# To test AR model I generated white noise of the same variance that `Y` has. 
# Then using formula y_0 = 0.91628673*y_1 + 0.06145462*y_2 + + whiteNoise_0  `Y_hat` was computed.
# 
# **In part 3** I simply compared `Y` and `Y_hat` and figured out that differences between 
# them are completly random and that my AR(2) model is good.


