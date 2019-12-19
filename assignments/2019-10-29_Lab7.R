rm(list = ls())

getwd()

install.packages("ggfortify")
library("ggfortify")

install.packages("multcomp")
library("multcomp")

install.packages("nlme")
library("nlme")

library("tidyverse")
tidyverse_update()

ggplot(aldrin) +
  geom_histogram(aes(Aldrin_value), binwidth = 0.5)+
  facet_wrap(~Depth_Aldrin)

ggplot(aldrin) +
  geom_boxplot(aes(x = Depth_Aldrin, y = Aldrin_value))

ggplot(aldrin)+
  geom_qq(aes(sample = Aldrin_value, color = Depth_Aldrin))


ggplot(HCB) +
  geom_histogram(aes(HCB_value), binwidth = 0.5)+
  facet_wrap(~Depth_HCB)

ggplot(HCB) +
  geom_boxplot(aes(x = Depth_HCB, y = HCB_value))

ggplot(HCB)+
  geom_qq(aes(sample = HCB_value, color = Depth_HCB))

summ_aldrin <- aldrin %>%
  group_by(Depth_Aldrin) %>% 
  summarise(mean = mean(Aldrin_value),
            sd = sd(Aldrin_value),
            n = n())

ratio <-(max(summ_aldrin$sd))/(min(summ_aldrin$sd))
View(ratio)

summ_HCB <- HCB %>%
  group_by(Depth_HCB) %>% 
  summarise(mean = mean(HCB_value),
            sd = sd(HCB_value),
            n = n())

ratio2 <-(max(summ_HCB$sd))/(min(summ_HCB$sd))

#######1-way ANOVA


aldrin01 <- lm(Aldrin_value~Depth_Aldrin, data = aldrin)

autoplot(aldrin01)

anova(aldrin01)

# <dataset_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))

aldrin <- mutate(aldrin,log_aldrin = log10(Aldrin_value))

aldrin02 <- lm(log_aldrin~Depth_Aldrin, data = aldrin)

autoplot(aldrin02)

anova(aldrin02)

HCB01 <- lm(HCB_value~Depth_HCB, data = HCB)

autoplot(HCB01)

anova(HCB01)               

######Tukey 

aldrin <-read_csv("datasets/aldrin.csv", col_types = cols(
  Depth_Aldrin = col_factor() ))

tukey <- glht(aldrin02, linfct = mcp(Depth_Aldrin = "Tukey"))
summary(tukey)

# Code breaks line 16 because data not read in
# 9/10 pts ####
