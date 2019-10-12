rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

install.packages("DescTools")
library("DescTools")

#Chapter 13, Question 20 #####
#a) The two methods that would be appropriate: Welch's t test (because not equal variance) with current data or transform data with natural log and do normal 2 sample t test 
#The following is a two sample t test with transformation of data 

#Normal summary stats (not mutated) - we saw that there was unequal variance
summ_salmon <- Salmon_data %>%
  group_by(species) %>% 
  summarise(mean = mean(skinColor),
            median = median(skinColor),
            sd = sd(skinColor),
            var = var(skinColor),
            n = n())

#Mutate data for natural log - transformation when the sd and mean are both larger
mutate_salmon <- Salmon_data %>%
  mutate(Ln_salmon = log(skinColor) )

summ_salmon2 <- mutate_salmon %>%
  group_by(species) %>% 
  summarise(mean = mean(Ln_salmon),
            median = median(Ln_salmon),
            sd = sd(Ln_salmon),
            var = var(Ln_salmon),
            n = n())

#Ratio of standard variance to loosely test homoscacity 
ratio <-(max(summ_salmon2$sd))/(min(summ_salmon2$sd))

#Because it was less then 3, we can say that homogeniety of variance was not violated

#Two sided test for a 2-sample t test with transformed data 
# formula (response ~ predictor), identify the data,
t.test(Ln_salmon ~ species, data = mutate_salmon, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#There is a significant difference between the mean of the kokanee and mean of the sockeye (two sample test, t =12.133, df = 33, p-value < 0.0001)

#Chapter 13, Question 25 ########

#Paired t test: given the difference 
#According to histogram provided, distriubtion is left skewed
#compute with a sign test 

SignTest(trees$biomassChange, alternative = "two.sided", mu = 0, conf.level = 0.95)

#There is not a significant difference between the biomass change after clear cutting (paired t test, S = 21, number of differences = 36, p value > 0.05)

#Chapter 13, Question 26 ########
#One sample, one sided
#Alternate hypothesis: Perference > 0 
# null hypothesis: Preference < (equal to) 0 

summ_beaks <- beaks %>%
  summarise(mean = mean(preference),
            median = median(preference),
            sd = sd(preference),
            var = var(preference),
            n = n())

ggplot(beaks) +
  geom_histogram(count(preference), binwidth = 0.5)

ggplot(beaks) +
  geom_boxplot(aes(x = "", y = preference))

ggplot(beaks)+
  geom_qq(aes(sample = preference))

#Because of a low sample size, we are going to say that there is a not normal distribution. The box plot shows unequal median and mean and unequal whiskers

SignTest(beaks$preference, 
         alternative = "greater", mu = 0, conf.level = 0.95)

#Therefore we can say that there was a significant preference for the beefed up birds (One sample sign t test, S=10, p value <0.001 )

#Review Problem 2: # 16 ########

#Alternate hypothesis: Mean of spd - mean of wild type > 150 seconds
#Null hypothesis: mean of spd - mean of wild type < (or equal) 150 seconds 
#I chose 150 seconds because this is equal to half of the total time. Therefore if a fish was agressive over half the time, it would be called agressive 

t.test(fishmean$diff, 
       alternative = "greater", mu = 150, conf.level = 0.95)

summ_fish <- fish %>%
  group_by(species) %>%
  summarise(mean = mean(sec),
            median = median(sec),
            sd = sd(sec),
            var = var(sec),
            n = n())

ratio <-(max(summ_fish$sd))/(min(summ_fish$sd))

ggplot(fish) +
  geom_histogram(aes(sec), binwidth = 10)+
  facet_wrap(~species)

ggplot(fish) +
  geom_boxplot(aes(x = species, y = sec))

ggplot(fish)+
  geom_qq(aes(sample = sec, color = species))

##Going ahead with the normal two sample t test because the ratio is below 3. The mean and medians are pretty close in the box plots and same with the q q plot 

t.test(sec ~ species, data = fish, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#b) We have found that it is significant that the difference between the two means is not equal to 0 (2-sample t test, t19 = 3.3802, p < 0.05)



