### R Script for Linear Regression Exercise

## Read in data
my_data <- read.csv("https://dl.dropboxusercontent.com/s/i0qtus70l0n57wd/OECDdata.csv?dl=0")
head(my_data)

## Summarise dependent variable
summary(my_data$HealthSpending)

## PLOT DEPENDENT VARIABLE
library(ggplot2)
g <- ggplot(my_data, aes(x=HealthSpending))

## Set the number of bins using the Freedman-Diaconis rule
x <- my_data$HealthSpending
bw <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))

## Plot graph
(graph <- g + geom_histogram(bins=bw)
  + labs(title="Distribution of Health Spending Variable"))


### FIT A BIVARIATE OLS
l_model <- lm(HealthSpending ~ GDPPC, data=my_data)

summary(l_model)

### EXAMINE THE MODEL RESIDUALS

## Residual plot
plot(residuals(l_model), main="Linear Model Residuals")
abline(0,0, col="red")

## QQNorm plot
l_model <- lm(HealthSpending ~ GDPPC, data=my_data)
l_resids <- rstandard(l_model)

qqnorm(l_resids, main="Linear Model QQPlot")
qqline(l_resids)


### MODEL SCATTER PLOT

## Without country labels
g <- ggplot(my_data, aes(x=GDPPC, y=HealthSpending))
(graph <- g + geom_point() 
  + geom_smooth(method="lm")
  + labs(x = "GDP Per Capita (1000s USD)", 
         y = "Health Spending (% of GDP)", title ="Linear Model Scatter Plot"))

## With country labels
g <- ggplot(my_data, aes(x=GDPPC, y=HealthSpending))
(graph <- g + ggplot2::geom_point() 
  + geom_smooth(method="lm") 
  + geom_label(label=my_data$Country, label.size = 0.1, hjust = "inward")
  + labs(x = "GDP Per Capita (1000s USD)", 
         y = "Health Spending (% of GDP)", title ="Linear Model Scatter Plot with Country Labels"))


### FIT MULTIPLE LINEAR REGRESSION MODEL
ml_model <- lm(HealthSpending ~ GDPPC + LifeExpectancy + Unemployment, data = my_data)

summary(ml_model)

### CHECK ML RESIDUALS

## Residuals plot
plot(residuals(mlm_model), main="ML Model Residuals")
abline(0,0, col="red")

## QQNorm plot
qqnorm(ml_resids, main="ML Model QQPlot")
qqline(ml_resids)

### PLOT ML MODELS ESTIMATES
plot_summs(ml_model, ci_level=0.9, color.class = "blue")