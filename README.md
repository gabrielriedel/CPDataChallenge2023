# Bunt Defense SMT Data Challenge 2023
## Project Description
The purpose of this data challenge project is to determine how infield defenses should approach sacrifice bunt situations. We specifically look at whether the defense should make a concerted effort to get the lead runner out or take the "free out" from the sac bunt. We look at sacrifice bunts in relevant situations (where base runners are on first base or first and second base) and measure two main categories. First we measure the success rate of the sac bunt where success is determined by whether the lead runner moves up on the basepaths or not. We also measure the success of the overall offense after the sacrifice bunt as determined by whether or not the offense scored afterwards in that inning. We observe the results and success rates from the data in order to determine if defenses should be trying to get the the lead runner out.
## Installation
Most code in this project was written in R, so you will need an R compatible IDE such as RStudio. We used several packages in RStudio to perform various operations. Here is a list of the packages and their insstall commands:
```
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("sportyR")
install.packages("DBI")
install.packages("RSQLite")
```
Also, in order to run the SQL queries in the R scripts you need to have downlaoded our SQLite database. You can find it in the DatabaseLink file in our main git hub repo. For simplest access, clone this git hub repo to your device.
