rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

install.packages("DescTools")
library("DescTools")

#QUESTION 1 - one sample, two sided

earth_data <- read_csv("datasets/demos/earth.csv")
y<-earth_data$Obliquity

#Option A 

null_mean <- 23.4722

summ_earth <- earth_data %>%
  summarise (mean = mean(Obliquity),
             median = median(Obliquity),
             IQR = IQR(Obliquity),
             sd = sd(Obliquity),
             var = var(Obliquity))
View(summ_earth)

sample_mean <- 23.49878
sample_sd <- 	0.019613
sample_n <- 5
df <- 4

t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
two_tailed <- 2*(1-pt(abs(t_sample), df))

View(t_sample)
View(two_tailed)

#Option B 

t.test(earth_data$Obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

#QUESTION 2 

heart_data <- read_csv("datasets/demos/HeartAttack_short.csv", col_names = TRUE,
col_types = cols(
  group = col_character() )
)

summ_heart <- heart_data %>%
  group_by(group) %>% 
  summarise(mean = mean(cholest),
            sd = sd(cholest),
            n = n())

ratio <-(max(summ_heart$sd))/(min(summ_heart$sd))
View(ratio)

ggplot(heart_data) +
  geom_histogram(aes(cholest), binwidth = 10)+
  facet_wrap(~group)

ggplot(heart_data) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(heart_data)+
  geom_qq(aes(sample = cholest, color = group))

t.test(cholest ~ group, data = heart_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#QUESTION 3 

fulmar_data <- read_csv("datasets/quinn/chpt3/furness.csv")

wilcox.test(METRATE ~ SEX, data = fulmar_data, alternative = "two.sided", conf.level = 0.95)

#QUESTION 4 

spider_data <- read_csv("datasets/quinn/chpt3/elgar.csv")

t.test(spider_data$HORIZLIG, spider_data$HORIZDIM, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
