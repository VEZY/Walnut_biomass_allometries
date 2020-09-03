# model 2 -----------------------------------------------------------------

# using a simple model that we correct with a proportionality factor that
# is computed using the measurements that we know are well made (e.g. diameters > 2cm)  

library(caret)
library(data.table)
library(tidyverse)
source("1-code/0-function.R")


df_mtg = fread("2-results/data.csv", data.table = FALSE)
df_mtg$tree = stringr::str_sub(gsub("tree","",df_mtg$branch), start = 1, end = 1)
df_mtg$branch = stringr::str_sub(gsub("tree","",df_mtg$branch), start = 2)

# Adding one to the number of leaves for the terminal leaves (they bear themselves)
df_mtg$number_leaves[df_mtg$number_leaves==0] = 1


formula = cross_section ~ number_leaves + pathlength_subtree + 
  segment_index_on_axis + axis_length + segment_subtree


vars_in_model = all.vars(formula)[-1]

reshape2::melt(df_mtg%>%select(branch,tree,diameter,tidyselect::all_of(vars_in_model)), 
               id.vars = c("tree","branch","diameter"))%>%
  ggplot(aes(x = diameter, y = value, color = paste(tree,branch)))+
  # facet_wrap(variable + branch ~ ., scales = "free_y")+
  facet_grid(rows = vars(variable), cols = vars(tree,branch), scales = "free_y")+
  geom_point()

model = fit_model(data = df_mtg, formula = formula, min_diam = 20)

# TODO: Calculer les volumes predis, et comparer aux volumes issus de mesures 

