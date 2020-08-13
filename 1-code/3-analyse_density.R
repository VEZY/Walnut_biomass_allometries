# Aim: Analyze the variability of wood density along branches from mtg data.
# Author: M. Millan, J. Dauzat and R. Vezy
# Date of creation: 12/08/2020

library(data.table)
library(tidyverse)

# Read the data.frame -----------------------------------------------------

df_mtg = fread("2-results/density/density_data.csv", data.table = FALSE)

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= topological_order, y= density))+
  geom_boxplot(aes(group = topological_order))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= segment_index_on_axis, y= density))+
  geom_point(aes(color = branch))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= segment_index_on_axis, y= density))+
  facet_wrap(.~branch)+
  geom_point(aes(color = branch))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= diameter, y= density))+
  facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = segment_index_on_axis))


df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= volume, y= density))+
  facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = segment_index_on_axis))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= volume_subtree, y= density))+
  facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = segment_index_on_axis))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= volume_ph, y= density))+
  facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = segment_index_on_axis))

df_mtg%>%
  dplyr::filter(density < 1.0)%>%
  ggplot(aes(x= density, y= density_ph))+
  geom_abline(slope = 1,intercept = 0)+
  # facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = volume))+
  scale_color_viridis_c()


df_mtg%>%
  dplyr::filter(density < 1.0)%>%
  ggplot(aes(x= diameter, y= density_ph/density))+
  geom_abline(slope = 1,intercept = 0)+
  # facet_wrap(.~branch, scales = "free")+
  geom_point(aes(color = volume))+
  scale_color_viridis_c()

# autoplot(tree1l)
# plotly_mtg(tree1l)
