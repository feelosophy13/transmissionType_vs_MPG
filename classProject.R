# You work for Motor Trend, a magazine about the automobile industry. 
# Looking at a data set of a collection of cars, they are interested in exploring the 
# relationship between a set of variables and miles per gallon (MPG) (outcome). 
# They are particularly interested in the following two questions:
# 1. "Is an automatic or manual transmission better for MPG"
# 2. "Quantify the MPG difference between automatic and manual transmissions"



#### initial setup
rm(list = ls())
getwd()
setwd('/Users/hawooksong/Desktop/regression_models')
dir()



#### quick summary of data 
?mtcars
head(mtcars)
dim(mtcars)
summary(mtcars)



#### 
# am 0: automatic transmission
# am 1: manual transmission
class(mtcars$am)
mtcarsOrig <- mtcars
mtcars$am <- gsub(0, 'auto', mtcars$am)
mtcars$am <- gsub(1, 'manual', mtcars$am)
table(mtcars$am)



#### mpg comparison between auto- and manual-transmission cars
tapply(mtcars$mpg, mtcars$am, mean)  # cars with manual transmission have higher mpg (in this dataset)

library(ggplot2)
ggplot(mtcars) + 
  geom_boxplot(aes(x = am, y = mpg, fill = am)) + 
  guides(fill = guide_legend(title = 'transmission\ntype')) + 
  ggtitle('Boxplot of MPG by Transmission Type')



#### is the difference in MPGs between the two samples stat. sig.?
## check for homogeneity of variance between the two samples through Levene's test
library(car)
suppressWarnings(leveneTest(mtcars$mpg ~ mtcars$am))  
# homogeneity of variance not assumed; cannot use independent t-test; resort to Mann-Whitney U test

## Mann-Whitney U test (non-parametric)
auto <- subset(mtcars, am == 'auto')
manual <- subset(mtcars, am == 'manual')
wilcox.test(auto$mpg, manual$mpg, paired = FALSE, exact = FALSE)  
# yes, the difference in MPG between the two samples is stat. sig.



#### correlation between variables
corMatrix <- round(cor(mtcars), 2)
corMatrix

library(corrplot)
corrplot(corMatrix, method = 'ellipse')



#### exploratory plotting
origPlotSettings <- par()  # save original plot settings
par(origPlotSettings)  # how to restore original plot settings

pairs(mtcars, col = mtcars$am + 1, 
      main = 'Pair-wise Scatter Plot')

plot(mtcars$disp, mtcars$mpg,
     pch = 21, bg = mtcars$am + 1)

plot(mtcars$hp, mtcars$mpg,
     pch = 21, bg = mtcars$am + 1)

plot(mtcars$wt, mtcars$mpg,
     pch = 21, bg = mtcars$am + 1)



#### model building
## include all three highly correlated variables 
model0 <- lm(mpg ~ disp + hp + wt, data = mtcars)
summary(model0)

## remove disp (since most it is redundant after including hp and wt)
model1 <- lm(mpg ~ wt + hp, data = mtcars)
summary(model1)

## include ony wt
model2 <- lm(mpg ~ wt, data = mtcars)  # better than model3
summary(model2)

## include only hp
model3 <- lm(mpg ~ hp, data = mtcars)
summary(model3)

## include wt with am
model4 <- lm(mpg ~ wt * am, data = mtcars)  # better than model5
summary(model4)

## include hp with am
model5 <- lm(mpg ~ hp * am, data = mtcars)
summary(model5)

## include wt * am and hp * am
model6 <- lm(mpg ~ wt * am + hp * am, data = mtcars)
summary(model6)

## include wt * am and hp (remove interaction between hp and am since it's not stat. sig.)
model7 <- lm(mpg ~ wt * am + hp, data = mtcars)
summary(model7)



#### centering the weight variable
head(mtcars)
wtMean <- mean(mtcars$wt)
mtcars$wt_centered <- mtcars$wt - wtMean



#### modeling with the centered weight variable
## subset auto- and manual-transmission vehicles into two separate datasets
auto <- subset(mtcars, am == 'auto')
manual <- subset(mtcars, am == 'manual')

## build separate models for auto- and manual-transmission datasets
modelAuto <- lm(mpg ~ wt_centered, auto)
modelManual <- lm(mpg ~ wt_centered, manual)
model <- lm(mpg ~ wt_centered, mtcars)

summary(modelAuto)
summary(modelManual)
summary(model)

## plot points and regression lines
plot(mtcars$wt_centered, mtcars$mpg,
     pch = 21, bg = mtcars$am + 1)
abline(modelAuto, lwd = 2, col = 'black')
abline(modelManual, lwd = 2, col = 'red')
# abline(model, lwd = 2, col = 'blue')

## plot points and regression lines in ggplot2
library(ggplot2)
ggplot(mtcars, aes(x = wt_centered, y = mpg, color = factor(am))) + 
  geom_point() + 
  geom_smooth(method = 'lm', fill = NA) +
  guides(col = guide_legend(title = 'transmission\ntype'))

## build second-order model (one that contains interaction)
model4 <- lm(mpg ~ wt_centered * am, data = mtcars)
summary(model4)  # 81.5% of variance in data explained by the model

## model interpretation
# expected mpg of an automatic-transmission vehicle with an average weight: 19.236 (stat. sig.)
# expected change in mpg per 1-unit (1000 lb) change in weight: -3.786 (stat. sig.)
# expected change in mpg from automatic-transmission cars to manual-transmission cars (not stat. sig.)
# expected change in slope in regression models from automatic-transmission cars to manual-transmission cars (stat. sig.)

## residuals, sum of squared errors (SSE), and mean squared error (MSE)
residuals <- residuals(model4)
SSE <- sum(residuals^2); SSE
MSE <- SSE / length(residuals); MSE

## plot residuals to ensure heteroscedasticity
plot(mtcars$wt_centered, residuals)
abline(h = 0, lwd = 2, col = 'red')  

