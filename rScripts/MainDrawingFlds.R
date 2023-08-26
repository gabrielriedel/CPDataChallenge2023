library(tidyverse)
library(sportyR)
library(ggplot2)
source("BuntDrawFunctions.R")
source("BuntPlotClean.R")

# Testing draw functions
draw_all_bunts(bunt_situation_table)
field_play_draw(bunt_situation_table, 7)
draw_play_after(bunt_situation_table, 7)
success_or_fail(bunt_situation_table, 7)
draw_success_fail(bunt_situation_table)
draw_success_fail_score(bunt_scores_final)
