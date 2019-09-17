rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

cricket_data<-read.csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

cricket_data<-mutate(cricket_data, log(timeToMating+1))

ggplot(cricket_data) +
  geom_histogram(aes(timeToMating), binwidth = 5)+
  facet_wrap(~feedingStatus)

ggplot(cricket_data) +
  geom_histogram(aes(log(timeToMating+1)), binwidth = 0.5)+
  facet_wrap(~feedingStatus)

