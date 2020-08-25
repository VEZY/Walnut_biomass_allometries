# 
library(ggplot2)
library(magrittr)
library(data.table)
library(tidyverse)

df_mtg = fread("2-results/data.csv", data.table = FALSE)

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


# ....


# df_mtg%>%
#   dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296 & number_leaves==0 )%>%
#   ggplot(aes(x= length, y= cross_section))+
#   geom_point(aes(color = branch), size=3) + ylim(0,3)





df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= cross_section, y= pathlength_subtree*number_leaves))+
  geom_point(aes(color = branch), size=3)+xlim(0,30)
