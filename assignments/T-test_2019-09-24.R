rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
install.packages("tidyr")
tidyverse_update()
library("tidyverse")
tidyverse_update()
countries_data<-read_csv("datasets/demos/countries.csv", col_names = TRUE)

ggplot(countries_data) +
  geom_histogram(aes(difference), binwidth = 0.5)

ggplot(countries_data)+
  geom_boxplot(aes(x = "", y = difference), notch = FALSE, varwidth = TRUE) 

summ_countries <- countries_data %>% 
summarise(mean_resp = mean(difference),
          median_resp = median(difference),
          IQR_resp = IQR(difference),
          sd_resp = sd(difference),
          var_resp = var(difference))

View(summ_countries)

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

data01 <- data01 %>% slice(-105)

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 2)+
  facet_wrap(~Survival)

ggplot(data01)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = FALSE, varwidth = TRUE)

summ_data01 <- data01 %>%
  group_by(Survival) %>% 
  summarise (mean = mean(squamosalHornLength),
             median = median(squamosalHornLength),
             IQR = IQR(squamosalHornLength),
             sd = sd(squamosalHornLength),
             var = var(squamosalHornLength))
View(summ_data01)

#### 10/10 code runs without breaking ####