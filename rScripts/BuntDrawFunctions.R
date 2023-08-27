install.packages("sportyR")
library(tidyverse)
library(sportyR)
library(ggplot2)


# Create visualization of infield with all plotted points for each bunt in table
draw_all_bunts <- function(dt){
  
  # Create basic baseball field as ggplot using sportypy package; specified infield view
  data_fld <- geom_baseball('MLB', display_range = "infield")
  
  # Add the (x,y) coordinates of each bunt play as geom_points to field graphic
  new_fld <- data_fld + geom_point(data = dt, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 2)
  
  # Draw field
  new_fld
}

# Determines vector of the ids of the runners on base on a given play (including batter)
runners_onbase <- function(dt, row_num){
  
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  
  runner_ids <- c(batter, first, second)
  
  return(runner_ids)
}


# Runner check functions designed for "bunt_situation_table" which has data for locations of runners on base during bunt play and the following play
# Important color code for runners: 
  # White = batter during bunt play
  # Black = runner on first during bunt_play
  # Blue = runner on second during bunt_play
  # No runners will be on third due to nature of sac bunt conditions
# Runner check functions used in draw_play_after to follow runners from bunt play to next play and keep their color code
# Returns color code of which player is on each base

runner_check_first2 <- function(dt, row_num){
  # Store IDs of runners and batters
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  first2 <- dt[row_num, ]$first_baserunner_2
  
  # Compare ID of runner on first during play after bunt to IDs of other runners
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
  # Store IDs of runners and batters
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  second2 <- dt[row_num, ]$second_baserunner_2
  
  # Compare ID of runner on second during play after bunt to IDs of other runners
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
  # Store IDs of runners and batters
  batter <- dt[row_num, ]$batter
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  third2 <- dt[row_num, ]$third_baserunner_2
  
  # Compare ID of runner on third during play after bunt to IDs of other runners
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
  
  # Draw baseball field from sportypy
  field <- geom_baseball('MLB', display_range = "infield")
  
  # Store specific bunt play row of data
  play <- dt[row_num, ]
  
  # If there is no runner on second, draw points for bunt (where acquired by defense), batter as white dot, first base runner as black dot
  if (play$second_baserunner == 0) {
    
    bunt_field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 2) + geom_point(aes(x = 63, y = 63), color = "black", size = 3) + geom_point(aes(x = 0, y = 0), color = "white", size = 3)
  }
  
  # Else there will be a runner on first and second. So add the same points and a blue point for second base runner
  else {
    
    bunt_field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 2) + geom_point(aes(x = 63, y = 63), color = "black", size = 3) + geom_point(aes(x = 0, y = 127), color = "blue", size = 3) + geom_point(aes(x = 0, y = 0), color = "white", size = 3)
  }
  
  # Draws bunt field
  bunt_field
  
}

# Draw field function that shows where runners are on the next play
draw_play_after <- function(dt, row_num) {
  
  # Draw baseball field from sportypy
  field <- geom_baseball('MLB', display_range = "infield")
  
  # Store specific bunt play row of data
  play <- dt[row_num, ]
  
  # Stores color of runners for each base of the play after bunt
  color_first <- runner_check_first2(dt, row_num)
  color_second <- runner_check_second2(dt, row_num)
  color_third <- runner_check_thrid2(dt, row_num)
  
  # If there is a runner on first, draw point on first with correct color code
  if(play$first_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = 63, y = 63), color = color_first, size = 3)
  }
  
  # If there is a runner on second, draw point on second with correct color code
  if(play$second_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = 0, y = 127), color = color_second, size = 3)
  }
  
  # If there is a runner on third, draw point on third with correct color code
  if(play$third_baserunner_2 != 0) {
    
    field <- field + geom_point(data = dt[row_num, ], aes(x = -63, y = 63), color = color_third, size = 3)
  }
  
  # Draw field
  field
}

# Function determines whether specific bunt was runner movement success or failure
# Success is defined by the lead runner moving up either one or two bases after the bunt play ends
movement_s_or_f <- function(dt, row_num){
  
  # Store IDs of runners on different bases for bunt play and following play
  first <- dt[row_num, ]$first_baserunner
  second <- dt[row_num, ]$second_baserunner
  second2 <- dt[row_num, ]$second_baserunner_2
  third2 <- dt[row_num, ]$third_baserunner_2
  
  # If runner on second during bunt play
  if (second != 0){
    
    # If runner on second moves to third after bunt, then sac bunt is successful
    if (third2 == second){
    
      return("success")
    }
    
    # Else the sac bunt is a failure
    else{
      return("fail")
    }
  }
  
  # Else there must be a just a runner on first (by nature of data table conditions)
  else{
    
    # If runner on first moves to second after bunt, then sac bunt is successful
    if(second2 == first){
      return("success")
    }
    
    # If runner on first moves to third after bunt, then sac bunt is successful
    else if(third2 == first){
      return("success")
    }
    
    # Else the sac bunt is a failure
    else{
      return("fail")
    }
    
  }
  
}

# Function determines whether specific bunt resulted in scoring success or failure
# Success is defined by the offense scoring in the inning after the sac bunt is laid
score_s_or_f <- function(dt, row_num){
  
  play <- dt[row_num, ]
  
  if(play$score != 0){
    return("success")
  }
  
  else{
    return("fail")
  }
}

# Function draws all bunts and colors them red or blue for runner movement success or fail respectively 
draw_movement_success_fail <- function(dt){
  
  f <- 0
  s <- 0
  # Draw baseball field from sportypy
  field <- geom_baseball('MLB', display_range = "infield")
  
  # Loops through each row of bunt plays
  for (row_num in 1:nrow(dt)){
    
    play <- dt[row_num, ]
    
    # Checks if bunt is success or fail and stores in var
    suc_or_fail <- success_or_fail(dt, row_num)
    
    # If success, plot the position of bunt when ball acquired using red dot
    if(suc_or_fail == "success"){
      
      field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 2)
      s <- s + 1
    }
    
    # Else fail, so plot the position of bunt when ball acquired using blue dot
    else{
      field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "blue", size = 2)
      f <- f + 1
    }
    
  }
  
  
  print(paste("Successes:", s))
  print(paste("Fails:", f))
  
  # Draw field
  field
}

# Function draws all bunts and colors them red or blue for scoring success or fail respectively 
draw_success_fail_score <- function(dt){
  
  f <- 0
  s <- 0
  # Draw baseball field from sportypy
  field <- geom_baseball('MLB', display_range = "infield")
  
  # Loops through each row of bunt plays
  for (row_num in 1:nrow(dt)){
    
    play <- dt[row_num, ]
    
    # Checks if bunt is success or fail and stores in var
    suc_or_fail <- score_s_or_f(dt, row_num)
    
    # If success, plot the position of bunt when ball acquired using red dot
    if(suc_or_fail == "success"){
      
      field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "red", size = 2)
      s <- s + 1
    }
    
    # Else fail, so plot the position of bunt when ball acquired using blue dot
    else{
      field <- field + geom_point(data = play, aes(x = as.numeric(ball_position_x), y = as.numeric(ball_position_y)), color = "blue", size = 2)
      f <- f + 1
    }
    
  }
  
  
  
  print(paste("Successes:", s))
  print(paste("Fails:", f))
  
  # Draw field
  field
}

# Function creates histogram to compare success and failures from runner movement model
draw_movement_histogram <- function(){
  # Create a data frame with success and fail categories and their respective values
  data <- data.frame(
    category = rep(c("Success", "Failure"), each = 13),
    value = c(rep(19, 13), rep(3, 13))
  )
  
  # Create the side-by-side histograms
  ggplot(data, aes(x = category, y = value, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Runner Movement Model Sucess vs. Failure", x = "Success or Failure", y = "Count") +
    scale_fill_manual(values = c("Success" = "red", "Failure" = "blue"))
}

# Function creates histogram to compare success and failures from scoring model
draw_score_histogram <- function(){
  # Create a data frame with success and fail categories and their respective values
  data <- data.frame(
    category = rep(c("Success", "Failure"), each = 13),
    value = c(rep(13, 13), rep(9, 13))
  )
  
  # Create the side-by-side histograms
  ggplot(data, aes(x = category, y = value, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Scoring Model Sucess vs. Failure", x = "Success or Failure", y = "Count") +
    scale_fill_manual(values = c("Success" = "red", "Failure" = "blue"))
}



