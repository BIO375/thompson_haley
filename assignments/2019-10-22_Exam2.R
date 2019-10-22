rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

install.packages("purrr")

install.packages("DescTools")
Yes
library("DescTools")

#####Problem 9

summ_birds <- birds %>%
  group_by(type) %>% 
  summarise(mean_eggs = mean(yellow),
            sd_eggs = sd(yellow),
            n_eggs = n())

ratio <-(max(summ_birds$sd_eggs))/(min(summ_birds$sd_eggs))

ggplot(birds) +
  geom_histogram(aes(yellow), binwidth = 0.03)+
  facet_wrap(~type)

ggplot(birds) +
  geom_boxplot(aes(x = type, y = yellow))

wilcox.test(yellow ~ type, data = birds, alternative = "two.sided", conf.level = 0.95)

#######Question 10 

baker <- mutate(baker, diff = After - Before)

ggplot(baker) +
  geom_histogram(aes(diff), binwidth = 0.1)

ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(baker)+
  geom_qq(aes(sample = diff))

SignTest(baker$diff, alternative = "greater", mu = 0, conf.level = 0.95)

######Question 11

summ_alga <- alga %>%
  group_by(treatment) %>% 
  summarise(mean_alga = mean(growthrate),
            sd_alga = sd(growthrate),
            n_alga = n())

ratio <-(max(summ_alga$sd_alga))/(min(summ_alga$sd_alga))

ggplot(alga) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(alga)+
  geom_qq(aes(sample = growthrate, color = treatment))

t.test(growthrate ~ treatment, data = alga, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

