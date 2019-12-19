rm(list = ls())
getwd()
library("ggfortify")
install.packages("broom")
library("broom")
library("tidyverse")
tidyverse_update()
install.packages("cili")

library("ggmosaic")
library("epitools")

library("nlme")
library("multcomp")

#####Scenario 1 

insulation <- read_csv("datasets/final/insulation.csv")

model01 <- lm(heat_loss ~ leanness, data = insulation)
autoplot(model01, smooth.colour = NA)

ggplot(data = insulation)+
  geom_point(aes(x = leanness, y = resid(model01)))

ggplot(data = insulation) +
  geom_point(mapping = aes(x = leanness, y = heat_loss ))

summary(model01)

#####Scenario 2 

caffeine <- read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))

summ_caffeine <- caffeine %>%
  group_by(group) %>% 
  summarise(mean = mean(half_life),
            sd = sd(half_life),
            median = median(half_life),
            n = n())

ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 5)+
  facet_wrap(~group)

ggplot(caffeine) +
  geom_boxplot(aes(x = group, y = half_life))

ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

model04 <- lm(half_life~group, data = caffeine)

anova(model04)

summary(model04)

planned <- glht(model04, linfct = 
                  mcp(group = c("high_prog - norm_prog = 0",
                                   "norm_prog - male = 0")))
confint(planned)
summary(planned)


######Scenario 3 

davis <- read_csv("datasets/final/davis.csv") 

model03 <-chisq.test(x = davis$observed, p = davis$expected_p)
model03


#### 10/10 code runs without breaking ####