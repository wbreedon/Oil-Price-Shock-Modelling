install.packages("vars")
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
#Install librarys that are not used

#Partially based off tutorial: https://towardsdatascience.com/a-deep-dive-on-vector-autoregression-in-r-58767ebb3f06

#run each "paragraph" sequentially
data6 <- read.csv("C:/datasets/POILBREUSDM6.csv")


intr <- ts(data6$Interest_Rate, start = c(1992,1,1), frequency = 12) #12 because of 12 months
pri_level <- ts(data6$Price_level, start = c(1992,1,1), frequency = 12)
diff_gdp <- ts(data6$Diff_GDP, start = c(1992,1,1), frequency = 12)
diff_oil <- ts(data6$Diff_oil, start = c(1992,1,1), frequency = 12)


autoplot(pri_level) 


#comb <- cbind(intr, pri_level, diff_gdp, diff_oil)
#colnames(comb) <- cbind("intr", "pri_level", "diff_gdp", "diff_oil")

#mode_one <- VAR(comb, p = 3, type = "const", season = NULL, exog = NULL) #mode_one because autofill incorrectly completes "model" to "model.choice()"
#summary(mode_one)

#Commented code failed because of multicolinearity producing a full rank matrix which is not invertible

#comb_two <- cbind(intr, diff_gdp, diff_oil)
#colnames(comb_two) <- cbind("intr", "diff_gdp", "diff_oil")

#mode_two <- VAR(comb_two, p = 3, type = "const", season = NULL, exog = NULL) #mode_one because autofill incorrectly completes "model" to "model.choice()"
#summary(mode_two)

#run nonparametric Philips-Peron tests

pp.test(diff_gdp) #lags = 5, p < 0.01
pp.test(intr) #lags = 5 p=0.69
pp.test(diff_oil) #lags =5 p = 0.01
comb_three <- cbind(diff_oil, intr, diff_gdp)
colnames(comb_three) <- cbind("diff_oil", "intr", "diff_gdp")

mode_three <- VAR(comb_three, p=5, type = "const", season = NULL, exog = NULL)
summary(mode_three)

#Get Granger casuality

granger_diff_oil <- causality(mode_three, cause = "diff_oil") #this causation is the main cause of interest
granger_intr <- causality(mode_three, cause = "intr")
granger_diff_gdp <- causality(mode_three, cause = "diff_gdp")

#Get impulse response

impulse_reponse <- irf(mode_three, impulse="diff_oil", reponse = "diff_gdp", n.ahead=24, ortho=FALSE, runs=1200) #amount of runs does not make much difference
plot(impulse_reponse) 