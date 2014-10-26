Analysis of Vehicle Fuel Efficiency by Transmission Type (Auto vs. Manual)
========================================================

<h2>Objective</h2>
The goal of this regression analysis is to explore vehicles' fuel efficiency--measured by miles-per-gallon (MPG)--by transmission type (automatic or manual). This analysis will attempt to answer whether automatic or manual transmission cars have better MPGs. The analysis will also attempt to quantify the MPG difference between automatic and manual transmission vehicles.

<h2>About the Data</h2>
The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models). The dataset contains 32 observations on 11 variables.
- mpg: Miles/(US) gallon
- cyl: Number of cylinders
- disp: Displacement (cu.in.)
- hp: Gross horsepower
- drat: Rear axle ratio
- wt: Weight (lb/1000)
- qsec: 1/4 mile time
- vs: V/S
- am: Transmission (0 = automatic, 1 = manual)
- gear: Number of forward gears
- carb: Number of carburetors

In order to make the analysis more human-readable, the coded values in 'am' variable, 0s and 1s, were substituted by 'auto' and 'manual' and saved into 'tmType' variable using the script below.


```r
## change coded values (0 or 1) in the transmission type variable
mtcarsOrig <- mtcars
mtcars$tmType <- gsub(0, 'auto', mtcars$am)
mtcars$tmType <- gsub(1, 'manual', mtcars$tmType)
```

<h2>Average MPG by Transmission Type</h2>
In order to see if there existed a notable difference in MPG between automatic and manual transmission vehicles, an average MPG was calculated for each transmission type. 

<b>MPG by transmission type</b>

```
##     auto   manual 
## 17.14737 24.39231
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The result from a Mann-Whitney U test suggested that the difference between the two samples in MPGs was statistically significant. (No indepdent t-test was performed as the two samples failed to meet the assumption of homogeneity of variance.)


```r
## check for homogeneity of variance between the two samples through Levene's test
library(car)
leveneTest(mtcars$mpg ~ mtcars$tmType)  # p-value < 0.05; homogeneity of variance not assumed; cannot use independent t-test; resort to Mann-Whitney U test

## Mann-Whitney U test (non-parametric)
auto <- subset(mtcars, tmType == 'auto')
manual <- subset(mtcars, tmType == 'manual')
wilcox.test(auto$mpg, manual$mpg, paired = FALSE, exact = FALSE)  # p-value < 0.05; the difference in MPG between the two samples is stat. sig.
```

<h2>Search for Potential Confounder</h2>
Because the difference in MPGs between automatic and manual transmission vehicles could be attributed to other variables, additional analyses were performed in search of potential confounders. First, a pair-wise correlation matrix was plotted in order to identify variables that were highly correlated with the MPG variable.

<b>correlation matrix</b>

```
##        mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
## mpg   1.00 -0.85 -0.85 -0.78  0.68 -0.87  0.42  0.66  0.60  0.48 -0.55
## cyl  -0.85  1.00  0.90  0.83 -0.70  0.78 -0.59 -0.81 -0.52 -0.49  0.53
## disp -0.85  0.90  1.00  0.79 -0.71  0.89 -0.43 -0.71 -0.59 -0.56  0.39
## hp   -0.78  0.83  0.79  1.00 -0.45  0.66 -0.71 -0.72 -0.24 -0.13  0.75
## drat  0.68 -0.70 -0.71 -0.45  1.00 -0.71  0.09  0.44  0.71  0.70 -0.09
## wt   -0.87  0.78  0.89  0.66 -0.71  1.00 -0.17 -0.55 -0.69 -0.58  0.43
## qsec  0.42 -0.59 -0.43 -0.71  0.09 -0.17  1.00  0.74 -0.23 -0.21 -0.66
## vs    0.66 -0.81 -0.71 -0.72  0.44 -0.55  0.74  1.00  0.17  0.21 -0.57
## am    0.60 -0.52 -0.59 -0.24  0.71 -0.69 -0.23  0.17  1.00  0.79  0.06
## gear  0.48 -0.49 -0.56 -0.13  0.70 -0.58 -0.21  0.21  0.79  1.00  0.27
## carb -0.55  0.53  0.39  0.75 -0.09  0.43 -0.66 -0.57  0.06  0.27  1.00
```

It was clear that 'cyl', 'disp', 'hp', 'wt' variables were highly correlated with 'mpg' variable. Among these four variables, 'cyl' variable was excluded from the model selection process as its values were more categorical than continuous. 

<b>mean for 'disp' by transmission type</b>

```
##     auto   manual 
## 290.3789 143.5308
```

<b>mean for 'hp' by transmission type</b>

```
##     auto   manual 
## 160.2632 126.8462
```

<b>mean for 'wt' by transmission type</b>

```
##     auto   manual 
## 3.768895 2.411000
```

It soon became apparent that the difference in MPGs between automatic and manual transmission vehicles could be attributed to other vehicle features. Specifically, automatic transmission cars were, on average, heavier--in addition to having more horsepower and displacement (in this dataset)--which could explain why they had lower MPGs than those of manual transmission cars. 

<h2>Model Selection</h2>

```r
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
```

In order to avoid collinearity in the model, a quick correlation analysis was performed 
between 'wt' and 'hp' variables. 

```r
cor.test(mtcars$wt, mtcars$hp)  # correlation coefficient: 0.66
```

The correlation coefficient between 'wt' and 'hp' variables was around 0.66. It was decided that both variables included in the model for the following reasons:
- The correlation coefficient was less than 0.7
- The inclusion of both variables increase the adjusted R-squared value.
- All regression coefficients in the last model were statistically significant.

<h2>Heteroscedasticity: Plotting the Residuals</h2>
![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Through the residual plots confirmed, heteroscedasticity assumption was confirmed.

<h2>Model Comparisons</h2>

```r
anova(model4, model7)  # performance difference between model4 and model7 stat. sig.
anova(model5, model7)  # performance difference between model5 and model7 stat. sig.
anova(model6, model7)  # performance difference between model6 and model7 NOT stat. sig.
```
It was demonstrated through analysis of variance (ANOVA) that model7 out-performed most other models except model6. ANOVA did not detect any significant performance difference between model6 and model7. Nonetheless, model7 was selected because all of its regression coefficients, including those of interaction terms, were statistically significant (as opposed to those of model6).

<h2>Final Regression Model: Centered 'wt' and 'hp' Variables and Interpretation</h2>
In order to increase the interpretability, the 'wt' and 'hp' variables were centered before the final regression model was constructed, based on model7.

```r
## mean of wt and hp
wtMean <- mean(mtcars$wt)
hpMean <- mean(mtcars$hp)

## centering predictor variables
mtcars$wt_centered <- mtcars$wt - wtMean
mtcars$hp_centered <- mtcars$hp - hpMean

## model building
model8 <- lm(mpg ~ wt_centered * am + hp_centered, data = mtcars)
summary(model8)
```

<b>Final regression model equation:</b><br />
$mpg = 18.9 - 2.52 * wt_c + 0.044 * am - 0.027 * hp_c - 3.58 * wt_c * am$

<b>Regression coefficient interpretation:</b>
- Expected MPG for an average-weight automatic transmission vehicle: 18.9 (notable in magnitude and statistically significant)
- Expected change in MPG per 1-unit (1000 lb) change in wt: -2.5 (notable in magnitude and statistically significant)
- Expected change in MPG going from automatic transmission vehicles to manual transmission vehicles: 0.044 (small in magnitude and not statistically significant; could be ignored)
- Expected change in MPG per 1-unit change in hp: -0.027 (small in magnitude but statistically significant)
- Expected change in slope relating wt and MPG going from automatic transmission vehicles to manual transmission vehicles: -3.58 (notable in magnitude and statistically significant)

<h2>Conclusion</h2>
At an initial glance, unaware of the confounders, one may have thought that vehicles with manual transmissions have higher MPG than those with automatic transmissions. After all, the mean MPG for automatic transmission vehicles was 17.1 MPG while that for manual transmission vehicles was 24.4 MPG. 

However, a further analysis showed that the dataset contained a few confounders. For example, automatic transmission vehicles tended to be heavier than manual transmission vehicles. Automatic transmission vehicles also tended to have higher horsepower and displacement than those with manual transmission. All of these other variables could have contributed to the difference in MPGs between automatic and manual transmission vehicles. 

A regression model was created to address this problem. The two continuous variables in the model, 'wt' and 'hp', were centered before being added as predictors. 

$mpg = 18.9 - 2.52 * wt_c + 0.044 * am - 0.027 * hp_c - 3.58 * wt_c * am$

The regression coefficients suggest the following: 
- Expected MPG for an average-weight automatic transmission vehicle is 18.9.
- Expected MPG for an average-weight manual transmission vehicles is 18.9.
- Expected change in MPG per 1-unit (1000 lb) change in wt is -2.5.
- Expected change in MPG per 1-unit change in hp is -0.027. 
- Expected change in slope relating wt and MPG going from automatic transmission vehicles to manual transmission vehicles is -3.58.

The last term, $-3.58 * wt_c * am$, is the most interesting term of all. It suggests that for a manual transmission vehicle (am = 1), its fuel efficiency depends on its weight. If the weight of a manual transmission car is greater than the average weight of cars sampled in the dataset, then the $wt_c$ term would be positive and hence $-3.58 * wt_c * am$ term would contribute to the decrease in MPG. On the other hand, if the weight of the car is less than the average weight of cars, then the $wt_c$ term would be negative and the $-3.58 * wt_c * am$ term would contribute to the increase in MPG. 

Therefore, one can conclude that manual transmission vehicles are not strictly more fuel efficient than automatic transmission vehicles. Manual transmission vehicles have higher MPGs in the dataset because they on average are lighter and have less horsepower. However, provided that every other feature is is equal, a manual transmission car would have higher MPG if its weight were less than the average weight (of cars in the dataset) compared to its automatic transmission counterpart. Otherwise, if its weight were greater than the average weight of cars, it would have lower MPG than its counterpart vehicle with automatic transmission.
