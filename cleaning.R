#Sets directory to my folder to load data set
setwd("/Users/Tejas/Desktop/Kings_Project/")

#Loads data set into data frame
shot_data <- read.csv("complete_data.csv")
players <- read.csv("players.csv")
teams <- read.csv("teams.csv")

#MERGES data for PASSER in 31, 32
m1 <- merge(shot_data,players, by.x = "passer", by.y = "player_id", all.x = TRUE)
m1 <- m1[-c(34,35,36,37)]

#HAVE TO LABEL 31, 32 as passer name
names(m1)[31] <- 'passer_first'
names(m1)[32] <- 'passer_last'
names(m1)[33] <- 'passer_position'

#MERGES data for SHOOTER in 34,35
m1 <- merge(m1,players, by.x = "shooter", by.y = "player_id", all.x = TRUE)
m1 <- m1[-c(37,38,39,40)]

names(m1)[34] <- 'shooter_first'
names(m1)[35] <- 'shooter_last'
names(m1)[36] <- 'shooter_position'

#MERGES data for DEFENDER in 37,38
m1 <- merge(m1,players, by.x = "defender", by.y = "player_id", all.x = TRUE)
m1 <- m1[-c(40,41,42,43)]

names(m1)[37] <- 'defender_first'
names(m1)[38] <- 'defender_last'
names(m1)[39] <- 'defender_position'

#MERGES data for offensive TEAM in 40
m1 <- merge(m1, teams, by.x = "team", by.y = "global_id", all.x = TRUE)
m1 <- m1[-c(40,41,42,44)]
names(m1)[40] <- 'offensive_team'

#MERGES data for defensive OPPONENT in 41
m1 <- merge(m1, teams, by.x = "opponent", by.y = "global_id", all.x = TRUE)
m1 <- m1[-c(41,42,43,45)]
names(m1)[41] <- 'defensive_team'

#remove 'X' column / record id duplicate
m1 <- m1[-6]


#REORDER columns
cleaned <- m1[,c(6,7,8,2,39,1,40,9,10,11,5,30,31,32,12,13,14,15,16,17,4,33,34,35,18,19,20,21,22,23,24,25,29,26,27,3,36,37,38,28)]

#EXPORT
write.csv(cleaned, file = "cleaned_data.csv")