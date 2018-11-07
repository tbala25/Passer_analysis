##RUN cleaning.R before this
#Sets directory to my folder to load data set
#setwd("/Users/Tejas/Desktop/Kings_Project/")
#Loads fullcourt template
#Originally from Rajiv Shah: GitHub @rajshah4
source("_function_fullcourt.R")

library(dplyr)

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
#
###########################################################################
getFG_pass_from <- function(df) {
  #get FG pct of all shots when player passed from area on court
  passfrom_top <- mean(df[which(df$pass_area == 'top_key'),31])
  passfrom_lw <- mean(df[which(df$pass_area == 'left_wing'),31])
  passfrom_rw <- mean(df[which(df$pass_area == 'right_wing'),31])
  passfrom_lc <- mean(df[which(df$pass_area == 'left_corner'),31])
  passfrom_rc <- mean(df[which(df$pass_area == 'right_corner'),31])
  passfrom_lp <- mean(df[which(df$pass_area == 'low_post'),31])
  passfrom_hp <- mean(df[which(df$pass_area == 'high_post'),31])
  
  countfrom_top <- nrow(df[which(df$pass_area == 'top_key'),])
  countfrom_lw <- nrow(df[which(df$pass_area == 'left_wing'),])
  countfrom_rw <- nrow(df[which(df$pass_area == 'right_wing'),])
  countfrom_lc <- nrow(df[which(df$pass_area == 'left_corner'),])
  countfrom_rc <- nrow(df[which(df$pass_area == 'right_corner'),])
  countfrom_lp <- nrow(df[which(df$pass_area == 'low_post'),])
  countfrom_hp <- nrow(df[which(df$pass_area == 'high_post'),])
  
  topofkey  <- c(passfrom_top, countfrom_top)
  leftwing  <- c(passfrom_lw, countfrom_lw)
  rightwing  <- c(passfrom_rw, countfrom_rw)
  leftcorner  <- c(passfrom_lc, countfrom_lc)
  rightcorner  <- c(passfrom_rc, countfrom_rc)
  lowpost  <- c(passfrom_lp, countfrom_lp)
  highpost  <- c(passfrom_hp, countfrom_hp)
  
  fgp_df <<- data.frame(rbind(topofkey, leftwing, rightwing, leftcorner, rightcorner, lowpost, highpost))
  
  
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
allshootfgs <- areas %>%
  group_by(poss_area, shooter) %>% 
  summarise(mean = mean(made,na.rm=TRUE))

##################################################################
Lebron <- getPlayer("LeBron James", areas)
SteveNash <- getPlayer("Steve Nash", areas)
KD <- getPlayer("Kevin Durant", areas)
Steph <- getPlayer("Stephen Curry", areas)

Lebron_grouped <- Lebron %>% 
  group_by(pass_area) %>% 
  summarise(mean = mean(made, na.rm = TRUE), freq = n())

Steph_grouped <- Steph %>% 
  group_by(pass_area) %>% 
  summarise(mean = mean(made, na.rm = TRUE), freq = n())
