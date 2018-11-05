##RUN cleaning.R before this
#Sets directory to my folder to load data set
#setwd("/Users/Tejas/Desktop/Kings_Project/")
#Loads fullcourt template
#Originally from Rajiv Shah: GitHub @rajshah4
source("_function_fullcourt.R")

#Loads data set into data frame
cleaned <- read.csv("cleaned_data.csv")
cleaned <- cleaned[,-c(1)]

#players <- read.csv("players.csv")

##############################
##############################
#Basic function for statistical mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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

cleaned <- data.frame(rbind(subsetL, subsetR))

###################################################################
#Enter player name as a string or player_id
#Creates DF of all records of player as a PASSER
##################################################################
getPlayer <- function(x) {
  #handle if playerID is input
  if(is.numeric(x)) {
    y <- x
    #passer df
    passer <- cleaned[which(cleaned$passer == y),]
    #shooter df
    #shooter <<- cleaned[which(cleaned$shooter == y),]
  }
  
  #handle if player name is input
  if(is.character(x)) {
    y <- strsplit(x, "\\s")
    y <- unlist(y)
    
    # find ID of passer
    passer_id <- cleaned[which(cleaned$passer_first == y[1] & cleaned$passer_last == y[2]),11]
    #passer df
    passer <<- cleaned[which(cleaned$passer == passer_id),]
  }
  return(passer)
}



##################################################################
#Groups passes into origin area and where shooter possesses
#################################################################
createAreas <- function() {
  #TRANSFORMS COORDINATES TO AREA ON COURT
  areas <- passer
  areas <- areas[-c(3,4)]
  areas$pass_area <- 'null'
  areas$poss_area <- 'null'
  
  for(i in 1:nrow(areas)) {
    ### PASS AREAS ###
    #TOP OF KEY, HIGH POST, LOW POST
    if(areas[i,14] <=38 && areas[i,14]>=12) {
      if(areas[i,13] <= 35 && areas[i,13] > 22) {
        areas[i,39] <<- 'top_key'
      }
      else if(areas[i,13] <= 22 && areas[i,13] > 12) {
        areas[i,39] <<- 'high_post'
      }
      else if(areas[i,13] <= 12) {
        areas[i,39] <<- 'low_post'
      }
    }
    
    #WINGS & CORNERS
    #LEFT
    else if(areas[i,14] >38){
      if(areas[i,13] <= 35 && areas[i,13] > 12){
        areas[i,39] <<- 'left_wing'
      }
      else if(areas[i,13] <= 12) {
        areas[i,39] <<- 'left_corner'
      }
    }
    #RIGHT
    else if(areas[i,14] < 15){
      if(areas[i,13] <= 35 && areas[i,13] > 12){
        areas[i,39] <<- 'right_wing'
      }
      else if(areas[i,13] <= 12) {
        areas[i,39] <<- 'right_corner'
      }
    }
    
    else if(areas[i,13] >=35 && areas[i,13] <=55){
      areas[i,39] <<- 'midcourt'
    }
    
    else if(areas[i,13] >55){
      areas[i,39] <<- 'transition'
    }
    
    
    #### POSSESSION AREAS #####
    
    #TOP OF KEY, HIGH POST, LOW POST
    if(areas[i,14] <=38 && areas[i,14]>=12) {
      if(areas[i,13] <= 35 && areas[i,13] > 22) {
        areas[i,40] <<- 'top_key'
      }
      else if(areas[i,13] <= 22 && areas[i,13] > 12) {
        areas[i,40] <<- 'high_post'
      }
      else if(areas[i,13] <= 12) {
        areas[i,40] <<- 'low_post'
      }
    }
    
    #WINGS & CORNERS
    #LEFT
    else if(areas[i,14] >38){
      if(areas[i,13] <= 35 && areas[i,13] > 12){
        areas[i,40] <<- 'left_wing'
      }
      else if(areas[i,13] <= 12) {
        areas[i,40] <<- 'left_corner'
      }
    }
    #RIGHT
    else if(areas[i,14] < 15){
      if(areas[i,13] <= 35 && areas[i,13] > 12){
        areas[i,40] <<- 'right_wing'
      }
      else if(areas[i,13] <= 12) {
        areas[i,40] <<- 'right_corner'
      }
    }
    
    else if(areas[i,13] >=35 && areas[i,13] <=55){
      areas[i,40] <<- 'midcourt'
    }
    
    else if(areas[i,13] >55){
      areas[i,40] <<- 'transition'
    }
    
    
  }

  areas$pass_area <- as.factor(areas$pass_area)
  areas$poss_area <- as.factor(areas$poss_area)
  
  return(areas)
}

