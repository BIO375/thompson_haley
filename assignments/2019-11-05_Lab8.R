# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
install.packages("hms")

##########Question 15-22 - example of random effects ANOVA 

sticks <-read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", col_types = cols(
  specimen = col_factor() ))

ggplot(sticks) +
  geom_histogram(aes(headwidth), binwidth = 0.01)
 
ggplot(sticks)+
  geom_qq(aes(sample = headwidth, color = ""))

ggplot(sticks) +
  geom_boxplot(aes(x = "", y = headwidth))

summ_sticks <- sticks %>%
  summarise(mean_sticks = mean(headwidth),
            median_sticks = median(headwidth),
            sd_sticks = sd(headwidth),
            n_sticks = n())

sticks_02 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = sticks)

sticks_02_varcomp <- VarCorr(sticks_02)
sticks_02_varcomp

varAmong  <- as.numeric( sticks_02_varcomp[1,1] )
#Variance among = 0.002459167

varWithin <- as.numeric( sticks_02_varcomp[2,1] )
#Variance within = 0.00166

repeatability <- varAmong / (varAmong + varWithin)
repeatability
#0.5970059 repeatability 

##Part d: The repeatability of example 5.6 was 74% and in 15-22 it was 60% meaning that this "repeatability" is lower. 
#By "repeatability" I mean that 60% of varitability is due to specimen identity or between specimen/group variance. 
# so, 40% is within group variability/measurement error in 15-22 and is more effected to measurement error then example 5.6

#######Problem 15-23 
#Planned multiple comparison test is showed below!!! 

pinecone <-read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

ggplot(pinecone) +
  geom_histogram(aes(conemass), binwidth = 0.1)

ggplot(pinecone)+
  geom_qq(aes(sample = conemass, color = habitat))

ggplot(pinecone) +
  geom_boxplot(aes(x = habitat, y = conemass))

pinecone01 <- lm(conemass~habitat, data = pinecone)

summ_pinecone <- pinecone %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            median_conemass = median(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())

ratio <-(max(summ_pinecone$sd_conemass))/(min(summ_pinecone$sd_conemass))
#equal to 1.3 

###1-way fixed anova - ok ignore this haha it is answering c 

autoplot(pinecone01)
anova(pinecone01)

###Planed comparisons anova

planned <- glht(pinecone01, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
                    
confint(planned)
summary(planned)

#######Question 15-26

bugs <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))

ggplot(bugs) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.1)

ggplot(bugs)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

ggplot(bugs) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))

bugs01 <- lm(logSporozoiteNumbers~treatmentGroup, data = bugs)

summ_bugs <- bugs %>%
  group_by(treatmentGroup) %>% 
  summarise(mean = mean(logSporozoiteNumbers),
            median = median(logSporozoiteNumbers),
            sd = sd(logSporozoiteNumbers),
            n = n())

ratio <-(max(summ_bugs$sd))/(min(summ_bugs$sd))

autoplot(bugs01)
anova(bugs01)

tukey <- glht(bugs01, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)
###Significant diff between scorpine and control and scorpine and WT at p<0.001 

##########Question 15-30 or 15-31 
#Looking to see if the mean rate differs between among groups 


crabs <-read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))

crabs <- crabs[1:84,]

ggplot(crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)

ggplot(crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

ggplot(crabs) +
  geom_boxplot(aes(x = crabType, y = bodyTemperature))

crabs01 <- lm(bodyTemperature~crabType, data = crabs)

summ_crabs <- crabs %>%
  group_by(crabType) %>% 
  summarise(mean = mean(bodyTemperature),
            median = median(bodyTemperature),
            sd = sd(bodyTemperature),
            n = n())

ratio <-(max(summ_crabs$sd))/(min(summ_crabs$sd))

autoplot(bugs01)
anova(bugs01)

#There is a significant difference among groups 

tukey <- glht(crabs01, linfct = mcp(crabType = "Tukey"))
summary(tukey)

### There is a significant difference between the intact male, male minor removed, and male major removed and the famale crabs at p <0.001
## There is a significant difference between the male major removed and male minor removed at p<0.05 


# 26/26 pts ####