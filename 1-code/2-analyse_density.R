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

for (i in mtg_files) {
  mtg = read_mtg(file = i)
}


autoplot(mtg)

undebug(read_mtg)
