library(tidyverse)
library(sportyR)
library(ggplot2)
install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)

# Connect SQLite Data Base
con <- dbConnect(RSQLite::SQLite(), "/Users/gaberiedel/Projects/DataChallenge/fldGame/Database/data_challenge.db")


# SQL query used to clean bunt data
data_clean_query <- "
SELECT DISTINCT 
    g1.game_str, g1.id, g1.play_id, g1.at_bat, g1.play_per_game, 
    g1.timestamp, g1.player_position, g1.event_code, 
    b.ball_position_x, b.ball_position_y, b.ball_position_z
FROM game_events g1
JOIN game_events g2 
    ON g1.game_str = g2.game_str
    AND g1.id = CAST(CAST(g2.id AS INTEGER) + 1 AS TEXT)
JOIN ball_pos b
    ON g1.game_str = b.game_str
    AND g1.timestamp = b.timestamp
WHERE g1.event_code = 2
AND g2.event_code = 16
AND b.ball_position_y > 10 AND b.ball_position_y < 50
AND b.ball_position_y > ABS(b.ball_position_x)
ORDER BY g1.game_str, g1.id;
"

bunt_table <- dbGetQuery(con, data_clean_query)

dbDisconnect(con)

View(bunt_table)

data_fld <- geom_baseball('MLB', display_range = "infield")
new_fld <- data_fld + geom_point(data = bunt_table, aes(x = ball_position_x, y = ball_position_y), color = "red", size = 1)
new_fld



