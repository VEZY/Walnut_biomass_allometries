# Aim: Analyse the variability of wood density along branches from mtg data.
# Author: M. Millan and R. Vezy
# Date of creation: 10/08/2020


# Imports -----------------------------------------------------------------

# devtools::install_github("VEZY/XploRer")
library(XploRer)
library(ggplot2)

# mtg = read_mtg(file = "0-data/1-mtg/tree1h.mtg")

mtg_files = list.files("0-data/1-mtg",pattern = "mtg$",
                       full.names = TRUE)

# Initialize an empty list of same length than mtg_files:
mtgs = vector(mode = "list", length = length(mtg_files))

# Read each mtg and put the result in the list by index:
for (i in seq_along(mtg_files)) {
  mtgs[[i]] = read_mtg(file = mtg_files[i])
}

# The for loop does:
# mtgs[[1]] = read_mtg(file = mtg_files[1])
# mtgs[[2]] = read_mtg(file = mtg_files[2])
# mtgs[[3]] = read_mtg(file = mtg_files[3])
# mtgs[[...]] = read_mtg(file = mtg_files[...])
# mtgs[[12]] = read_mtg(file = mtg_files[12])

# Name each element in the list as the name of the mtg file:
names(mtgs) = basename(mtg_files)%>%gsub(".mtg","",.)

# Plot
autoplot(mtgs$tree1l)
plotly_mtg(mtgs$tree1l)


# Adding new variables ----------------------------------------------------

# We will use as factors: diameter, length, topological order, the axis length, 
# and the segment diameter / axis length

tree1h = mtgs$tree1h  
tree1h$classes
tree1h$features
tree1h$MTG

# Compute the wood density:
mutate_mtg(tree1h, density = node$dry_weight_p1/node$volume_bh, .symbol = "S")

# Topological order:
topological_order(tree1h,ascend = FALSE)
# We use topological order from tip to root to allow comparisons between branches of 
# different ages (the last emitted segment will always be of order 1).

# mutate_mtg(tree1h,axis_length = get_children_values(attribute = "length",symbol = "S"), .symbol = "A")


# Compute the total length of the axis:
mutate_mtg(tree1h, axis_length = sum(get_nodes_symbol_values(attribute = "length", symbol = "S")), .symbol = "A")
# Associate the axis length to each segment:
mutate_mtg(tree1h, axis_length = get_parent_value(attribute = "axis_length", symbol = "A"), .symbol = "S")

# segment diameter / axis length:
mutate_mtg(tree1h, d_seg_len_ax_ratio = node$diameter / node$axis_length, .symbol = "S")


mtg_df = data.tree::ToDataFrameTree(tree1h$MTG,"ID","density","diameter","length","axis_length",
                                    "topological_order","d_seg_len_ax_ratio","dry_weight_p1","volume_bh")


mtg_df%>%
  filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= d_seg_len_ax_ratio, y= density))+
  geom_point(aes(color = topological_order))

mtg_df%>%
  filter(ID != 1500 & ID != 1515)%>%
  ggplot(aes(x= topological_order, y= density))+
  geom_boxplot(aes(group = topological_order))


mtg_df$ID[mtg_df$density>1 & !is.na(mtg_df$density)]

mtg_df$dry_weight_p1[mtg_df$density>1 & !is.na(mtg_df$density)]
mtg_df$volume_bh[mtg_df$density>1 & !is.na(mtg_df$density)]

plot(mtg_df$dry_weight_p1 / mtg_df$volume_bh)

autoplot(tree1l)
plotly_mtg(tree1l)


get_children_values(attribute = "length",node = tree1l$MTG$node_2, symbol = "S",recursive = TRUE)
# aggregate_children_values(attribute = "length",node = tree1l$MTG$node_2, symbol = "S",recursive = TRUE)

get_nodes_symbol_values(attribute = "length", node = FindNode(MTG$MTG,"node_8"), symbol = "S")
