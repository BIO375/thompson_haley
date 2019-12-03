#####Problem 11 - just read in the data to see it 

rm(list = ls())
getwd()

library("ggfortify")

install.packages("broom")
library("broom")

library("tidyverse")
tidyverse_update()

glucose <- read_csv("datasets/exams/glucose.csv")

######Problem 12

driver <- read_csv("datasets/exams/DriverVision.csv")

##Checking for bivariate normality - basic scatter plot 

ggplot(data = driver) +
  geom_point(mapping = aes(x = Age, y = Distance ))

##Normal distribution of x and y seperately 

ggplot(data = driver)+
  geom_histogram(aes(Age), binwidth = 2)

ggplot(data = driver)+
  geom_histogram(aes(Distance), binwidth = 10)

ggplot(data = driver)+
  geom_boxplot(aes("",Age))

ggplot(data = driver)+
  geom_qq(aes(sample = Age))

####diagnosing departures using residual plots 

model01 <- lm(Distance ~ Age, data = driver)
autoplot(model01, smooth.colour = NA)

ggplot(data = driver)+
  geom_point(aes(x = Age, y = resid(model01)))

summary(model01)

#### Code runs perfectly 5/5 ####