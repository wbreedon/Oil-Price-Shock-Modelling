#generally it is best to run each "paragraph" of code sequentially

data9 <- read.csv("C:/datasets/POILBREUSDM9.csv")
library(grf)
#Combine X variables into one matrix
X <- cbind(data9$Oil_price, data9$Price_level, data9$Interest_Rate)
Y <- c(data9$GDP)
W <- c(data9$Maj_event) #must be vectors

cau_forest <- causal_forest(X, Y, W)
#need to split by time and not randomly in a time series context
X_test <- X[201:334]
X_train <- X[1:200]
X_test <- matrix(X_test)
c.pred <- predict(cau_forest, X)

average_treatment_effect(cau_forest, target.sample = "all") # -6 std error 25
average_treatment_effect(cau_forest, target.sample="treated") #4 std error 30, not significant

#Taken and adapted from package documentation
X_test <- X #Since according to Bremain random forests do not overfit - we can actually afford to do without a test set

cau_forest <- causal_forest(X, Y, W, num.trees = 4000) #increase number of trees to get accurate intervals
cau_hat <- predict(cau_forest, X_test, estimate.variance = TRUE)
sigma_hat <- sqrt(cau_hat$variance.estimates) #find least squares between predicted X and actual X
plot(X_test[, 1], cau_hat$predictions, ylim = range(cau_hat$predictions + 1.96 * sigma_hat, cau_hat$predictions - 1.96 * sigma_hat, 0, 2), xlab = "x", ylab = "cau_forest", type = "l")
lines(X_test[, 1], cau_hat$predictions + 1.96 * sigma_hat, col = 1, lty = 2) #1.96 = 5% significance level
lines(X_test[, 1], cau_hat$predictions - 1.96 * sigma_hat, col = 1, lty = 2)
lines(X_test[, 1], pmax(0, X_test[, 1]), col = 2, lty = 1)


variable_importance(cau_forest, decay.exponent = 2, max.depth = 5) 
test_calibration(cau_forest) #documentation refers to a "vcov type" parameter that is actually unused
cau_forest2 <- causal_forest(X, Y, W, tune.parameters = "all")
average_treatment_effect(cau_forest2, target.sample = "all")