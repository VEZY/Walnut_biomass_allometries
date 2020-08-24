# 
library(ggplot2)
library(magrittr)
library(data.table)
library(tidyverse)

df_mtg = fread("2-results/data.csv", data.table = FALSE)

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= diameter, y= number_leaves))+
  geom_boxplot(aes(group = branch))
