#######Lab 9 - Correlation and Linear Regression 
rm(list = ls())
getwd()

library("ggfortify")

install.packages("broom")
library("broom")

library("tidyverse")
tidyverse_update()

fowler <- read_csv("datasets/fowler.csv")

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

###Diagnosing departures using residual plots 

model07 <- lm(YIELD ~ FERTILIZER, data = fowler)
autoplot(model07, smooth.colour = NA)

ggplot(data = fowler)+
  geom_point(aes(x = FERTILIZER, y = resid(model01)))

summary(model07)

###Normal Q-Q looks normal, Residuals v Y and Residuals v X look good 

# The p-value of interest is found in the row "YIELD", the intercept
# and slope are found under the column header "Estimate".

# Grass plots with fertilizer have significantly higher yeilds 
# (Linear regression: Fertilizer = 51.93 + 0.811(Yield);
# df = 1, 8, F=94.04, P<0.0001), and fertilizer explained more than 90%
# of the variabiity in yeilds (R2 = 0.9216).

# For a linear regression, we usually want to add a regression line to 
# our plot and often we also want to give an idea about how confident we
# are in our estimate of that regression line.  To do this, we generate
# what are known as confidence bands.  The narrower the band, the more
# confident we are in our estimate of the line.

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")

##With a confidence band of 0.99

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")


