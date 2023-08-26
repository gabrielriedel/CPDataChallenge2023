install.packages("sportyR")
install.packages("DBI")
install.packages("RSQLite")
library(tidyverse)
library(sportyR)
library(ggplot2)
library(DBI)
library(RSQLite)
library(dplyr)
source("BuntDrawFunctions.R")

# Connect SQLite Data Base
con <- dbConnect(RSQLite::SQLite(), "/Users/gaberiedel/Projects/DataChallenge/fldGame/Database/data_challenge.db")


# SQL queries used to clean bunt data and create data tables:


# SQL Query to get initial batch of plays that are bunts in one data table
  # Conditions for bunt situation:
  # Ball acquired (event_code == 2) immediately following a "ball bounce" (event_code == 16)
  # Y coordinate position of ball when acquired: 10 units < y < 50 units
  # Ball position is inside the base paths (satisfies the inequality y > abs(x))
  # There is a runner at least on first base (could be another on second base because still considered a sac bunt situation)
  # No runner on third base (situation then becomes squeeze not technically sac)
data_clean_query <- "SELECT DISTINCT 
    g1.game_str, g1.id, g1.play_id, g1.at_bat, g1.play_per_game, 
    g1.timestamp, g1.player_position, g1.event_code, 
    b.ball_position_x, b.ball_position_y, b.ball_position_z,
    gi.first_baserunner, gi.second_baserunner
FROM game_events g1
JOIN game_events g2 
    ON g1.game_str = g2.game_str
    AND g1.id = CAST(CAST(g2.id AS INTEGER) + 1 AS TEXT)
JOIN ball_pos b
    ON g1.game_str = b.game_str
    AND g1.timestamp = b.timestamp
JOIN game_info gi
    ON g1.game_str = gi.game_str
    AND g1.play_per_game = gi.play_per_game
WHERE g1.event_code = 2
AND g2.event_code = 16
AND b.ball_position_y > 10 AND b.ball_position_y < 50
AND b.ball_position_y > ABS(b.ball_position_x)
AND gi.first_baserunner <> 0
AND gi.third_baserunner = 0
ORDER BY g1.game_str, g1.id;

"


# SQL query to get batch of bunts with more specific conditions; builds upon data table produced by previous query (table entitled "bunt_table")
  # Further conditions include:
  # Makes sure that the bunt is not the last play of the inning
  # Makes sure that the batter id switches after play is over (eliminating error in data)
  # Adds columns to view the runners on base of the play immediately following bunt play
bunt_situation_query <- "SELECT
    bt.game_str,
    bt.play_per_game,
    bt.timestamp,
    bt.player_position,
    bt.event_code,
    bt.ball_position_x,
    bt.ball_position_y,
    bt.ball_position_z,
    gi1.inning,
    gi1.top_bottom_inning,
    gi1.batter,
    gi1.first_baserunner,
    gi1.second_baserunner,
    gi1.third_baserunner,
    gi2.inning AS inning_2,
    gi2.top_bottom_inning AS top_bottom_inning2,
    gi2.batter AS batter_2,
    gi2.first_baserunner AS first_baserunner_2,
    gi2.second_baserunner AS second_baserunner_2,
    gi2.third_baserunner AS third_baserunner_2
FROM bunt_table bt
JOIN game_info gi1
    ON bt.game_str = gi1.game_str
    AND bt.play_per_game = gi1.play_per_game
JOIN game_info gi2
    ON bt.game_str = gi2.game_str
    AND bt.play_per_game + 1 = gi2.play_per_game
WHERE gi1.top_bottom_inning = gi2.top_bottom_inning
    AND gi1.batter <> gi2.batter;
"


# SQL query to pull in all of the game_info data tables into one large data table for easy access
game_info_query <- "SELECT
    gi.game_str,
    gi.play_per_game,
    gi.top_bottom_inning,
    gi.batter,
    gi.first_baserunner,
    gi.second_baserunner,
    gi.third_baserunner
FROM game_info gi
ORDER BY gi.game_str
LIMIT 26315;
"


# Final SQL query with the same conditions as "bunt_situation_query" but with a more narrowed down list of columns to view in the data table
final_bunt_sit_query <- "SELECT
    gi1.game_str,
    gi1.play_per_game,
    gi1.top_bottom_inning,
    gi1.batter,
    gi1.first_baserunner,
    gi1.second_baserunner,
    gi1.third_baserunner
FROM bunt_table bt
JOIN game_info gi1
    ON bt.game_str = gi1.game_str
    AND bt.play_per_game = gi1.play_per_game
JOIN game_info gi2
    ON bt.game_str = gi2.game_str
    AND bt.play_per_game + 1 = gi2.play_per_game
WHERE gi1.top_bottom_inning = gi2.top_bottom_inning
    AND gi1.batter <> gi2.batter;
"


# Create new data tables of bunts cleaned by queries
bunt_table <- dbGetQuery(con, data_clean_query)
bunt_situation_table <- dbGetQuery(con, bunt_situation_query)
game_info <- dbGetQuery(con, game_info_query)
final_bunt_sit <- dbGetQuery(con, final_bunt_sit_query)
bunt_scores = read_csv("fullbuntscores.csv")
bunt_scores_final <- inner_join(bunt_situation_table, bunt_scores, by = c("game_str" = "games"))
bunt_scores_final <- bunt_scores_final[1:22, ]


# game_info data table required fixes with inconsistencies with the incrementing of play_per_game
game_info[15581, "play_per_game"] <- 40
game_info[18724, "play_per_game"] <- 149
game_info[18725, "play_per_game"] <- 150
game_info[18726, "play_per_game"] <- 151
game_info[18727, "play_per_game"] <- 152
game_info[18728, "play_per_game"] <- 153


# Disconnect SQLite database
dbDisconnect(con)

# View Data Tables
View(bunt_table)
View(bunt_situation_table)
View(game_info)
View(final_bunt_sit)
View(bunt_scores)



# Function extracts game_info rows for bunt plays and subsequent plays within the same inning, creating an ordered data table
after_bunt_seq <- function(dt, row_num){
  
  column_names <- c("game_str", "play_per_game", "top_bottom_inning", "batter", "first_baserunner", "second_baserunner", "third_baserunner")
  
  specific_game_info <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  
  colnames(specific_game_info) <- column_names 
  
  for (row_num in 1:nrow(dt)){
    
  
    play_num <- as.numeric(dt[row_num, ]$play_per_game)
    
    top_or_bot <- dt[row_num, ]$top_bottom_inning
    
    play_game_str <- dt[row_num, ]$game_str
  
    current <- top_or_bot
    
    
    
    while(top_or_bot == current){
      
      copied_row <- game_info[game_info$game_str == play_game_str & game_info$play_per_game == play_num, ]
      
      specific_game_info <- rbind(specific_game_info, copied_row)
      
      
      play_num <- play_num + 1
      
      current <- game_info[game_info$game_str == play_game_str & game_info$play_per_game == play_num, ]$top_bottom_inning
      
      
    }
    
  
  }
  
  return(specific_game_info)
  
}

# Testing function --> turned after_bunt data table into CSV for easier sharing and use
after_bunt <- after_bunt_seq(bunt_situation_table, 1)
View(after_bunt)



