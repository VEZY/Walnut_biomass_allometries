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

mtgs$tree1h$features
mtgs$tree1h$MTG

autoplot(mtgs$tree4m)
