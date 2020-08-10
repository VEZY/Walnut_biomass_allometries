# objective:analyze MTG data 
# author: Mathilde Millan
# date of creation: 5.08.2020


# import library+data -----------------------------------------------------

# devtools::install_github("VEZY/XploRer")

library(XploRer)
library(ggplot2)
library(data.tree)

MTG= read_mtg(file = "0-data/1-mtg/Arbre1B.mtg")


# Analyzing ---------------------------------------------------------------

# Plot the MTG:
autoplot(MTG)
plotly_mtg(MTG)
# Add attributes:
plotly_mtg(MTG,diameter,length,ID)

# Get all attributes available in the mtg (even after computation)
MTG$MTG$attributesAll

MTG$MTG$node_2
MTG$MTG$node_2$node_3$node_3+--
  # To transform into a data.frame:
  mtg_df = ToDataFrameTable(MTG$MTG, "lenght", "diamater","ID")
head(mtg_df, 30)
