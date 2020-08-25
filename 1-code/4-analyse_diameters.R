# 
library(ggplot2)
library(magrittr)
library(data.table)
library(tidyverse)
library(tidymodels)

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



# model -------------------------------------------------------------------

min_diam = 20 # Minimal diameter where the LiDAR can measure the diameters right 

df_mtg_no_na = 
  df_mtg%>%
  filter(!is.na(cross_section))%>%
  mutate(split = ifelse(.data$diameter > min_diam, "train","test"))

model_fit = 
  df_mtg_no_na %>%
  nest(data = c(-branch)) %>% 
  mutate(
    train_data = map(data, function(x) x[x$split=="train",]),
    test_data = map(data, function(x) x[x$split=="test",]),
    fit = map(train_data, possibly(~ lm(cross_section ~ 0 + number_leaves, data = .x), otherwise = NA)),
    meas_cross_section_train = map(train_data, function(x) x$cross_section),
    meas_cross_section_test = map(test_data, function(x) x$cross_section),
    pred_cross_section_train = map(fit, possibly(~ predict(.x), otherwise = NA)),
    pred_cross_section_test = map2(fit,test_data, possibly(~ predict.lm(object = .x, newdata = .y), otherwise = NA)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

model_fit_training = 
  model_fit%>%
  select(branch, meas_cross_section_train, pred_cross_section_train)%>%
  unnest(c(meas_cross_section_train,pred_cross_section_train))%>%
  rename(meas_cross_section = meas_cross_section_train,
         pred_cross_section = pred_cross_section_train)%>%
  mutate(type = "training")

model_fit_testing = 
  model_fit%>%
  select(branch, meas_cross_section_test, pred_cross_section_test)%>%
  unnest(c(meas_cross_section_test,pred_cross_section_test))%>%
  rename(meas_cross_section = meas_cross_section_test,
         pred_cross_section = pred_cross_section_test)%>%
  mutate(type = "testing")


model_fit_all = bind_rows(model_fit_training,model_fit_testing)

model_fit_all%>%
  ggplot(aes(x= meas_cross_section, y = pred_cross_section, color = branch))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

model_fit_all%>%
  ggplot(aes(x= meas_cross_section, y = pred_cross_section, color = branch))+
  facet_wrap(type ~ ., scales = "free")+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

