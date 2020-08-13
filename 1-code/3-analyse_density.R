# Aim: Analyze the variability of wood density along branches from mtg data.
# Author: M. Millan, J. Dauzat and R. Vezy
# Date of creation: 12/08/2020


# Read the data.frame -----------------------------------------------------

df_mtg = fread("2-results/density/density_data.csv", data.table = FALSE)

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= topological_order, y= density))+
  geom_point(aes(group = topological_order))

df_mtg%>%
  dplyr::filter(ID != 1500 & ID != 1515 & ID != 4296)%>%
  ggplot(aes(x= diameter, y= density))+
  geom_point(aes(color = topological_order))

mtg_df%>%
  dplyr::filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= diameter, y= density_ph))+
  geom_point(aes(color = topological_order))

mtg_df$ID[mtg_df$density>1 & !is.na(mtg_df$density)]

mtg_df$dry_weight_p1[mtg_df$density>1 & !is.na(mtg_df$density)]
mtg_df$volume_bh[mtg_df$density>1 & !is.na(mtg_df$density)]

plot(mtg_df$dry_weight_p1 / mtg_df$volume_bh)

autoplot(tree1l)
plotly_mtg(tree1l)


get_children_values(attribute = "length",node = tree1l$MTG$node_2, symbol = "S",recursive = TRUE)
# aggregate_children_values(attribute = "length",node = tree1l$MTG$node_2, symbol = "S",recursive = TRUE)

get_follow_values(attribute = "length", node = FindNode(MTG$MTG,"node_8"), symbol = "S")
