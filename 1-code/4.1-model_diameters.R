# model 2 -----------------------------------------------------------------

# using a simple model that we correct with a proportionality factor that
# is computed using the measurements that we know are well made (e.g. diameters > 2cm)  

library(caret)
library(data.table)
library(tidyverse)

df_mtg = fread("2-results/data.csv", data.table = FALSE)

df_mtg$tree = stringr::str_sub(gsub("tree","",df_mtg$branch), start = 1, end = 1)
df_mtg$branch = stringr::str_sub(gsub("tree","",df_mtg$branch), start = 2)
# df_mtg$cross_section[df_mtg$cross_section > 80 & !is.na(df_mtg$cross_section)] = NA
# df_mtg$diameter[df_mtg$diameter > 80 & !is.na(df_mtg$diameter)] = NA


# Adding one to the number of leaves for the terminal leaves (they bear themselves)
df_mtg$number_leaves[df_mtg$number_leaves==0] = 1

# Complete model (use all possible variables available from LiDAR):
complete_model = lm(cross_section ~ number_leaves + pathlength_subtree + segment_index_on_axis 
                    + axis_length + length, data = df_mtg)
caret::varImp(complete_model)
# Only pathlength_subtree and segment_index_on_axis seems to have a high weight

model = complete_model
# model = lm(cross_section ~ pathlength_subtree + segment_index_on_axis, data = df_mtg)
caret::varImp(model)

min_diam = 20 # Minimal diameter where the LiDAR can measure the diameters right 

vars_in_model = names(model$coefficients)
vars_in_model = vars_in_model[!grepl("(Intercept)",vars_in_model)]

reshape2::melt(df_mtg%>%select(branch,tree,diameter,tidyselect::all_of(vars_in_model)), 
               id.vars = c("tree","branch","diameter"))%>%
  ggplot(aes(x = diameter, y = value, color = paste(tree,branch)))+
  # facet_wrap(variable + branch ~ ., scales = "free_y")+
  facet_grid(rows = vars(variable), cols = vars(tree,branch), scales = "free_y")+
  geom_point()

reshape2::melt(df_mtg%>%select(branch,tree,diameter,tidyselect::all_of(vars_in_model)), 
               id.vars = c("tree","branch","diameter"))%>%
  ggplot(aes(x = diameter, y = value, color = paste(tree,branch), shape = tree))+
  # facet_wrap(variable + branch ~ ., scales = "free_y")+
  facet_grid(rows = vars(variable), scales = "free_y")+
  geom_point()

df_mtg_no_na = 
  df_mtg%>%
  select(tidyselect::all_of(c("branch","tree","cross_section","diameter",vars_in_model)))%>%
  filter_all(all_vars(!is.na(.)))%>%
  mutate(pred_cross_section = predict(model, newdata = .))

# Our simple model, without any correction:
simple_model_p = 
  df_mtg_no_na%>%
  # filter(diameter < min_diam)%>%
  # filter(diameter >= min_diam)%>%
  # filter(branch == "tree2h")%>%
  ggplot(aes(x= cross_section, y = pred_cross_section, color = paste(branch,tree)))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

# simple_model_p


# Fitting a correction factor (alpha) for each branch on "measured" points
# i.e. the ones that have a diameter above min_diam:
cor_model = ~lm(cross_section ~ 0 + pred_cross_section, data = .x)

df_mtg_no_na_cor = 
  df_mtg_no_na%>%
  mutate(split = ifelse(.data$diameter >= min_diam, "train","test"))%>%
  # filter(diameter >= min_diam)%>%
  nest(data = c(-branch,-tree))%>%
  mutate(
    train_data = map(data, function(x) x[x$split=="train",]),
    # test_data = map(data, function(x) x[x$split=="test",]),
    fit = map(train_data, possibly(cor_model,otherwise = NA)),
    # cross_section_train = map(train_data, function(x) x$cross_section),
    # cross_section_test = map(test_data, function(x) x$cross_section),
    # pred_cross_section_train = map(fit, possibly(~ predict(.x), otherwise = NA)),
    # pred_cross_section_test = map2(fit,test_data, possibly(~ predict.lm(object = .x, newdata = .y), otherwise = NA)),
    diameter = map(data, function(x) x$diameter),
    cross_section = map(data, function(x) x$cross_section),
    pred_cross_section = map(data, function(x) x$pred_cross_section),
    # pred_cross_section_cor = map(fit, possibly(~ predict(.x), otherwise = NA)),
    pred_cross_section_cor = map2(fit,data, possibly(~ predict.lm(object = .x, newdata = .y), otherwise = NA)),
    intercept = map(fit, possibly(~coef(.x)[1],otherwise = NA)),
    slope = map(fit, possibly(~coef(.x)[2],otherwise = NA)),
    # tidied = map(fit, tidy)
  )%>% 
  unnest(c(intercept,slope,diameter,cross_section,pred_cross_section,pred_cross_section_cor))%>% 
  select(-data,-fit)%>%
  mutate(diameter_pred = sqrt(pred_cross_section_cor/pi)*10*2,
         volume = cross_section * .data$length)

 # df_mtg_no_na= 
#   df_mtg_no_na%>%
#   dplyr::full_join(cor_factors, by = "branch")%>%
#   mutate(pred_cross_section_cor = intercept + slope * pred_cross_section)%>%
#   select(-intercept,-slope)


# Our simple model, now corrected:
simple_model_p_cor =
  df_mtg_no_na_cor%>%
  filter(diameter < min_diam)%>%
  # filter(branch == "tree2h")%>%
  ggplot(aes(x= cross_section, y = pred_cross_section_cor, color = branch))+
  facet_wrap(branch ~ ., scales = "free")+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

simple_model_p + simple_model_p_cor

# ggsave("2-results/cross_section_prediction_number_leaves_and_pathlength_subtree_cor_slope_inter.png", plot = simple_model_p_cor,
#        width = 18, height = 9)
# ggsave("2-results/cross_section_prediction_number_leaves_cor_slope_inter.png", plot = simple_model_p_cor,
#        width = 18, height = 9)
# ggsave("2-results/cross_section_prediction_segment_index_on_axis_and_pathlength_subtree_cor_slope_inter.png", plot = simple_model_p_cor,
#        width = 18, height = 9)
# ggsave("2-results/cross_section_prediction_segment_index_on_axis_and_pathlength_subtree_cor_slope.png", plot = simple_model_p_cor,
#        width = 18, height = 9)
# ggsave("2-results/cross_section_prediction_complete_model_cor_slope.png", plot = simple_model_p_cor,
#        width = 18, height = 9)

# Statistics:
df_mtg_no_na_cor%>%
  filter(diameter < min_diam)%>%
  group_by(branch)%>%
  summarise(nrmse = CroPlotR::nRMSE(sim = pred_cross_section, obs = cross_section),
            nrmse_cor = CroPlotR::nRMSE(sim = pred_cross_section_cor, obs = cross_section),
            EF = CroPlotR::EF(sim = pred_cross_section, obs = cross_section),
            EF_cor = CroPlotR::EF(sim = pred_cross_section_cor, obs = cross_section),
            Bias = CroPlotR::Bias(sim = pred_cross_section, obs = cross_section),
            Bias_cor = CroPlotR::Bias(sim = pred_cross_section_cor, obs = cross_section)
  )

# Plot to see what is the effect of the correction on each branch,
# and what are the points used to train the correction:
df_mtg_no_na_cor%>%
  mutate(Point = ifelse(diameter >= min_diam, "Cor training", "Cor left out"))%>%
  # filter(branch == "tree2h")%>%
  ggplot(aes(x= cross_section, color = Point))+
  facet_wrap(branch ~ ., scales = "free")+
  geom_point(aes(y = pred_cross_section))+
  geom_point(aes(y = pred_cross_section_cor, color = "corrected"))+
  geom_abline(slope = 1, intercept = 0)


# TODO: Calculer les volumes predis, et comparer aux volumes issus de mesures 

