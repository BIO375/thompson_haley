rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

######Binomial test 

model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

#####Chi square test 
# x = observed counts of each level of your categorical variable
# p = expected probabilities for each level of your categorical variable, must be number between 0 and 1

flower <- read_csv("datasets/flowers.csv") 

model02 <-chisq.test(x = flower$pheno_n, p = flower$expected_p)
model02

#####Contingency Chi square - creating the 2 x 2 

tab01 <- matrix(c(30, 17, 41, 49), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Outcome" = c("Belgium", "Holland"),
                        "Treatment" = c("Female", "Male"))
as.matrix(tab01)
model05 <- chisq.test(tab01, correct = FALSE)
model05
