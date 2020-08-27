# 
library(ggplot2)
library(magrittr)
library(data.table)
library(tidyverse)
library(tidymodels)
library(patchwork)

source("1-code/0-function.R")

df_mtg = fread("2-results/data.csv", data.table = FALSE)

df_mtg$cross_section[df_mtg$cross_section > 200 & !is.na(df_mtg$cross_section)] = 
  df_mtg$cross_section[df_mtg$cross_section > 200 & !is.na(df_mtg$cross_section)] / 100

# NUMBER of LEAVES
# toutes les branches
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3)+xlim(0,100)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= cross_section, y= number_leaves/6.3))+
  geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(color = branch), size=3)+xlim(0,100)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= cross_section, y= (number_leaves+1)/6.3))+
  geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(color = branch), size=3)+xlim(0,100)

# arbre par arbre
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree1")))%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree2")))%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3)+xlim(0,25)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree3")))%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree4")))%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3)+xlim(0,15)

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree4m")))%>%
  ggplot(aes(x= cross_section, y= number_leaves))+
  geom_point(aes(color = branch), size=3) + xlim(0,13) + ylim(0,13)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree4m")))%>%
  ggplot(aes(x= cross_section, y= number_leaves+1))+
  geom_point(aes(color = branch), size=3) + xlim(0,13) + ylim(0,13)

# DIAMETRE vs PATHLENGTH
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree))+
  geom_point(aes(color = branch), size=3)+xlim(0,30)
# arbre par arbre
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree1")))%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree))+
  geom_point(aes(color = branch), size=3)+xlim(0,25)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree2")))%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree))+
  geom_point(aes(color = branch), size=3)+xlim(0,30)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree3")))%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree))+
  geom_point(aes(color = branch), size=3)+xlim(0,30)
df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & (str_detect(branch,"tree4")))%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree))+
  geom_point(aes(color = branch), size=3)+xlim(0,15)


diam_terminal_segments = 
  df_mtg%>%
  filter(topological_order == 1)


ggplot(diam_terminal_segments, aes(x= branch, y = diameter))+
  geom_violin(aes(fill = branch))+
  geom_point()

ggplot(diam_terminal_segments, aes(x= branch, y = diameter))+
  geom_boxplot(aes(fill = branch))+
  geom_point()

# model -------------------------------------------------------------------

df_mtg = fread("2-results/data.csv", data.table = FALSE)

df_mtg$cross_section[df_mtg$cross_section > 200 & !is.na(df_mtg$cross_section)] = 
  df_mtg$cross_section[df_mtg$cross_section > 200 & !is.na(df_mtg$cross_section)] / 100


min_diam = 20 # Minimal diameter where the LiDAR can measure the diameters right 

model = ~ lm(cross_section ~ 0 + number_leaves, data = .x)

model_fit_all = fit_model(data = df_mtg, model = model, min_diam = min_diam)

# model_fit_all%>%
#   ggplot(aes(x= meas_cross_section, y = pred_cross_section, color = branch))+
#   geom_point()+
#   geom_abline(slope = 1, intercept = 0)

model_fit_all%>%
  ggplot(aes(x= meas_cross_section, y = pred_cross_section, color = branch))+
  facet_wrap(type ~ ., scales = "free")+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

model_fit_all%>%
  filter(type == "training")%>%
  ggplot(aes(x= meas_cross_section, y = pred_cross_section, color = branch))+
  facet_wrap(branch ~ ., scales = "free")+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

p = 
  df_mtg%>%
  ggplot(aes(x= cross_section, y = number_leaves, color = branch))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)


