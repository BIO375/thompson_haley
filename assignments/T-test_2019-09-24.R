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

summ_countries <- countries_data
group_by(Country) 
summarise(mean_resp = mean(difference),
          median_resp = median(difference),
          IQR_resp = IQR(difference),
          sd_resp = sd(difference),
          var_resp = var(difference))

View(summ_countries)

