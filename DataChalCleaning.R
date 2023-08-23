---
  title: "Data Wrangling SMT"
format:
  html:
  theme: default
---
  
  ```{r Setup}
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
AND b.ball_position_y > 5 AND b.ball_position_y < 50
ORDER BY g1.game_str, g1.id;
"

bunt_table <- dbGetQuery(con, data_clean_query)

dbDisconnect(con)

View(bunt_table)
```



Importing the Data
```{r}
setwd("/Users/gaberiedel/Downloads/smt_data_challenge_2023/game_info")
gameinfo1 = read_csv("game_info-1903_32_TeamNB_TeamA1.csv")
setwd("/Users/gaberiedel/Downloads/smt_data_challenge_2023/ball_pos")
location1 = read_csv("ball_pos-1903_32_TeamNB_TeamA1.csv")
location2 = read_csv("ball_pos-1900_02_TeamKJ_TeamB.csv")
setwd("/Users/gaberiedel/Downloads/smt_data_challenge_2023/game_events")
gameevents1 = read_csv("game_events-1903_32_TeamNB_TeamA1.csv")
gameevents2 = read_csv("game_events-1900_02_TeamKJ_TeamB.csv")
setwd("/Users/gaberiedel/Downloads/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1902_TeamA1")
playerpos1 = read_csv("player_pos-1902_17_TeamMB_TeamA1.csv")
```

#```{r}

# Bunt Data Cleaning:


# Find instances immediately after ground ball has finished bouncing (event_code == 16) and when ball is picked up (event_code == 2)

groundball = gameevents1|>
  filter(lag(event_code) == 16 & event_code == 2)
View(groundball)

# Time stamps of when ground balls are received by fielders
groundtimestamps <- groundball$timestamp

# Filter ball_pos to the time stamps when ball is received
gbpos <- location1 %>%
  filter(timestamp %in% groundtimestamps)
View(gbpos)

# Filter ball_pos to be less than 50 ft (filtering for bunts)
bunts <- gbpos %>% 
  filter(ball_position_y<=50,ball_position_y>=5)
View(bunts)

# Graphical Testing of bunts
x_values <- bunts$ball_position_x
y_values <- bunts$ball_position_y


data_fld <- geom_baseball('MLB', display_range = "infield")
new_fld <- data_fld + geom_point(data = bunt_table, aes(x = ball_position_x, y = ball_position_y), color = "red", size = 1)
new_fld







#```



