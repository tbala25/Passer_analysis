##RUN cleaning.R before this
#Sets directory to my folder to load data set
#setwd("/Users/Tejas/Desktop/Kings_Project/")
#Loads fullcourt template
#Originally from Rajiv Shah: GitHub @rajshah4
source("_function_fullcourt.R")

library(dplyr)
library(party)


#Loads data set into data frame
cleaned <- read.csv("cleaned_data.csv")
cleaned <- cleaned[,-c(1)]
#remove where no passer is recorded
cleaned <- cleaned[!is.na(cleaned$passer),]


players <- read.csv("players.csv")

##############################
##############################
#Basic function for statistical mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



###################################################################
#Enter player name as a string or player_id
#Creates DF of all records of player as a PASSER
##################################################################
getPlayer <- function(x, df) {
  #handle if playerID is input
  if(is.numeric(x)) {
    y <- x
    #passer df
    passer <- df[which(df$passer == y),]
    #shooter df
    #shooter <<- cleaned[which(cleaned$shooter == y),]
  }
  
  #handle if player name is input
  if(is.character(x)) {
    y <- strsplit(x, "\\s")
    y <- unlist(y)
    
    # find ID of passer
    passer <- df[which(df$passer_first == y[1] & df$passer_last == y[2]),]
    #passer df
    #passer <<- df[which(df$passer == passer_id),]
  }
  return(passer)
}


##########################################################################
##########################################################################
areasAlt <- function(x,y) {
  val = ''
  #transition
  if(x >= 55){
    val <- 'transition'
  }
  
  #midcourt
  else if(x >=35 && x < 55){
    val <- 'midcourt'
  }
  
  #top_key, high_post
  else if(x >= 12 && x < 35){
    
    if(y >= 12 && y < 38) {
      if(x < 22) {
        val = "high_post"
      }
      else {
        val = "top_key"
      }
    }
    #right_wing, left_wing
    else if(y < 12) {
        val = "left_wing"
      }
    else if(y >= 38) {
        val = "right_wing"
    }
  }
  
  #low_post, left_corner, right_corner
  else if(x < 12){
    if(y < 12) {
      val = "left_corner"
    }
    else if(y >= 38) {
      val = "right_corner"
    }
    else {
      val = "low_post"
    }
  }
  return(val)
}

###########################################################################
#Transforms all shows to be on Left basket
##########################################################################

subsetR <- cleaned[which(cleaned$offense_basket == 'R'),]
subsetL <- cleaned[which(cleaned$offense_basket == 'L'),]
subsetR$pass_x <- 94 - subsetR$pass_x
subsetR$pass_y<-50 - subsetR$pass_y
subsetR$shot_x<-94 - subsetR$shot_x
subsetR$shot_y<-50 - subsetR$shot_y
subsetR$poss_x<-94 - subsetR$poss_x
subsetR$poss_y<-50 - subsetR$poss_y

transformed <- data.frame(rbind(subsetL, subsetR))

#####################################################################
#Gets areas for all rows
#####################################################################

areas <- transformed %>%
  rowwise() %>%
  mutate(poss_area = areasAlt(poss_x, poss_y),pass_area = areasAlt(pass_x, pass_y))

areas$pass_area <- as.factor(areas$pass_area)
areas$poss_area <- as.factor(areas$poss_area)

###################################################################
#Get all shooters fg% in each area they shot in
####################################################################
allshootfgs <- areas %>%
  group_by(poss_area, shooter) %>% 
  summarise(mean = mean(made,na.rm=TRUE))

##################################################################
#Gets different player's dfs as passers
#################################################################
Lebron <- getPlayer("LeBron James", areas)
SteveNash <- getPlayer("Steve Nash", areas)
KD <- getPlayer("Kevin Durant", areas)
Steph <- getPlayer("Stephen Curry", areas)

###############################################################
#Group player's passes by area passed to and calculate fg% of all shooters combined in that area
###############################################################

Lebron_grouped <- Lebron %>% 
  group_by(pass_area) %>% 
  summarise(mean = mean(made, na.rm = TRUE), freq = n())

Steph_grouped <- Steph %>% 
  group_by(pass_area) %>% 
  summarise(mean = mean(made, na.rm = TRUE), freq = n())



##################################################################
#Gets df of all passes and that shooter's fg% when recieving the ball in that area
#For histogram
#################################################################
Lebron_shooters_fgp <- Lebron %>%
  rowwise() %>%
  mutate(shooter_fgp = )


################################################################
#Predicting where Lebron will pass
#And FG% of shooter of that pass
################################################################

var_imp_select <- ctree(made~ ., Lebron)
plot(var_imp_select)

