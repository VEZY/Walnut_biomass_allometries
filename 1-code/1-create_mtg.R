# Aim: create the mtg files from the raw data in xlsx
# Authors: M. Millan and R. Vezy
# Date: 10/08/2020


# Import all functions needed ---------------------------------------------

source("1-code/0-function.R")
library(tidyverse)
# Depencies: data.table, xlsx

# Read mtg from an xlsx file, write it in an mtg file
all_xlsx_to_mtg("0-data/0-raw","0-data/1-mtg/")
