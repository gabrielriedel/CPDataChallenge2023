library(tidyverse)
library(sportyR)
library(ggplot2)


# Determines the ids of the runners on base on a given play (including batter)
runners_onbase <- function(dt, row_num){
  
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  
  runner_ids <- c(batter, first, second)
  
  return(runner_ids)
}


runner_check_first2 <- function(dt, row_num){
  
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  first2 <- dt[row_num, ]$first_baserunner_2
  
  if(batter == first2){
    return("white")
  }
  else if(first == first2){
    return("black")
  }
  else if(second == first2){
    return("blue")
  }
  else{
    return("pink")
  }
  
}

runner_check_second2 <- function(dt, row_num){
  
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  second2 <- dt[row_num, ]$second_baserunner_2
  
  if(batter == second2){
    return("white")
  }
  else if(first == second2){
    return("black")
  }
  else if(second == second2){
    return("blue")
  }
  else{
    return("pink")
  }
  
}

runner_check_thrid2 <- function(dt, row_num){
  
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  third2 <- dt[row_num, ]$third_baserunner_2
  
  if(batter == third2){
    return("white")
  }
  else if(first == third2){
    return("black")
  }
  else if(second == third2){
    return("blue")
  }
  else{
    return("pink")
  }
  
  
}

# Draw field function to draw an individual bunt

field_play_draw <- function(dt, row_num) {
  
  field <- geom_baseball('MLB', display_range = "infield")
  
  play <- dt[row_num, ]
  
  if (play$second_baserunner == 0) {
    
    bunt_field <- field + geom_point(data = dt[row_num, ], aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 1) + geom_point(aes(x = 63, y = 63), color = "black", size = 3) + geom_point(aes(x = 0, y = 0), color = "white", size = 3)
  }
  
  else {
    
    bunt_field <- field + geom_point(data = dt[row_num, ], aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 1) + geom_point(aes(x = 63, y = 63), color = "black", size = 3) + geom_point(aes(x = 0, y = 127), color = "blue", size = 3) + geom_point(aes(x = 0, y = 0), color = "white", size = 3)
  }
  
  
  
  bunt_field
  
}



# Draw field function that shows where runners are on the next play

draw_play_after <- function(dt, row_num) {
  
  field <- geom_baseball('MLB', display_range = "infield")
  
  play <- dt[row_num, ]
  
  color_first <- runner_check_first2(dt, row_num)
  color_second <- runner_check_second2(dt, row_num)
  color_third <- runner_check_thrid2(dt, row_num)
  
  if(play$first_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = 63, y = 63), color = color_first, size = 3)
  }
  
  if(play$second_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = 0, y = 127), color = color_second, size = 3)
  }
  
  if(play$third_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = -63, y = 63), color = color_third, size = 3)
  }
  
  field
  
  
  
  
}

a