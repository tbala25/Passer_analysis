##RUN cleaning.R before this
#Sets directory to my folder to load data set
#setwd("/Users/Tejas/Desktop/Kings_Project/")
#Loads fullcourt template
#Originally from Rajiv Shah: GitHub @rajshah4
source("_function_fullcourt.R")

#Loads data set into data frame
cleaned <- read.csv("cleaned_data.csv")
cleaned <- cleaned[,-c(1)]

players <- read.csv("players.csv")



transformRightBaskets <- function(df) {
  #if(df$offense_basket == 'R') {
    df$new_passx <- 94 - df$pass_x
    df$new_passy<-50 - df$pass_y
    df$new_shotx<-94 - df$new_shotx
    df$new_shoty<-50 - df$new_shoty
    df$new_possx<-94 - df$poss_x
    df$new_possy<-50 - df$poss_y
  #}
 #  #else {
 #    df$new_passx<-df$pass_x
 #    df$new_passy<-df$pass_y
 #    df$new_shotx<-df$new_shotx
 #    df$new_shoty<-df$new_shoty
 #    df$new_possx<-df$poss_x
 #    df$new_possy<-df$poss_y
 # # }
  return(df)
}

subsetR <- subset[which(subset$offense_basket == 'R'),]
subsetR$pass_x <- 94 - subsetR$pass_x
subsetR$pass_y<-50 - subsetR$pass_y
subsetR$shot_x<-94 - subsetR$shot_x
subsetR$shot_y<-50 - subsetR$shot_y
subsetR$poss_x<-94 - subsetR$poss_x
subsetR$poss_y<-50 - subsetR$poss_y

#cleaned_transformed <- transformRightBaskets(subsetR)



#Enter player name as a string or player_id
#Creates DF of all records of player as a PASSER
getPlayer <- function(x) {
  #handle if playerID is input
  if(is.numeric(x)) {
    y <- x
    #passer df
    passer <<- cleaned[which(cleaned$passer == y),]
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
    
    # find ID of shooter
    #shooter_id <- cleaned[which(cleaned$shooter_first == y[1] & cleaned$shooter_last == y[2]),11]
    #shooter df
   # shooter <<- cleaned[which(cleaned$shooter == passer_id),]
  }
  
 # return(passer, shooter)
}


makemiss <- function() {
  #assume passer/shooter df exist
  
  passer_make <<- passer[which(passer$made == 1),]
  #shooter_make <<- shooter[which(shooter$made == 1),]
  
  passer_miss <<- passer[which(passer$made == 0),]
  #shooter_miss <<- shooter[which(shooter$made == 0),]
}

nddAnalysis <- function() {
  #creates HIST for ndd for both makes and miss
  pass_make_ndd_hist <<- hist(passer_make$ndd)
  pass_miss_ndd_hist <<- hist(passer_miss$ndd)
  
  hist(passer$ndd)
  
  #computes median
  pass_make_ndd_median <<- median(passer_make$ndd)
  
}

dribblesAnalysis <- function() {
  hist(passer_make$dribbles)
  hist(passer_miss$dribbles)
  
  hist(passer$dribbles)
}

passDistAnalysis <- function() {
  hist(passer_make$pass_distance)
  hist(passer_miss$pass_distance)
}

#Basic function for statistical mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Groups passes into origin area and where shooter possesses
createAreas <- function() {
  #TRANSFORMS COORDINATES TO AREA ON COURT
  areas <<- passer
  areas <<- areas[-c(3,4)]
  areas$pass_area <- 'null'
  areas$poss_area <- 'null'
  
  ### PASS AREAS ###
  
  for(i in 1:nrow(areas)) {
    #TOP OF KEY, HIGH POST, LOW POST
    if(areas[i,14] <=35 && areas[i,14]>=15) {
      if(areas[i,13] <= 40 && areas[i,13] > 25) {
        areas[i,39] <<- 'top_key'
      }
      else if(areas[i,13] <= 69 && areas[i,13] > 54) {
        areas[i,39] <<- 'top_key'
      }
      
      else if(areas[i,13] <= 25 && areas[i,13] > 13) {
        areas[i,39] <<- 'high_post'
      }
      else if(areas[i,13] <= 81 && areas[i,13] > 69) {
        areas[i,39] <<- 'high_post'
      }
      else if(areas[i,13] <= 13) {
        areas[i,39] <<- 'low_post'
      }
      else if(areas[i,13] > 81) {
        areas[i,39] <<- 'low_post'
      }
    }
    
    #WINGS & CORNERS
    else if(areas[i,14] >35){
      if(areas[i,13] <= 40 && areas[i,13] > 23){
        areas[i,39] <<- 'right_wing'
      }
      else if(areas[i,13] <= 71 && areas[i,13] > 54) {
        areas[i,39] <<- 'left_wing'
      }
      else if(areas[i,13] <= 23) {
        areas[i,39] <<- 'right_corner'
      }
      else if(areas[i,13] > 71) {
        areas[i,39] <<- 'left_corner'
      }
    }
    
    else if(areas[i,14] < 15){
      if(areas[i,13] <= 40 && areas[i,13] > 23){
        areas[i,39] <<- 'left_wing'
      }
      else if(areas[i,13] <= 71 && areas[i,13] > 54) {
        areas[i,39] <<- 'right_wing'
      }
      else if(areas[i,13] <= 23) {
        areas[i,39] <<- 'left_corner'
      }
      else if(areas[i,13] > 71) {
        areas[i,39] <<- 'right_corner'
      }
    }
    
  }
  areas$pass_area <- as.factor(areas$pass_area)
  
  #### POSSESSION AREAS #####
  
  for(i in 1:nrow(areas)) {
    #TOP OF KEY, HIGH POST, LOW POST
    if(areas[i,24] <=35 && areas[i,24]>=15) {
      if(areas[i,23] <= 40 && areas[i,23] > 25) {
        areas[i,40] <<- 'top_key'
      }
      else if(areas[i,23] <= 69 && areas[i,23] > 54) {
        areas[i,40] <<- 'top_key'
      }
      
      else if(areas[i,23] <= 25 && areas[i,23] > 13) {
        areas[i,40] <<- 'high_post'
      }
      else if(areas[i,23] <= 81 && areas[i,23] > 69) {
        areas[i,40] <<- 'high_post'
      }
      else if(areas[i,23] <= 13) {
        areas[i,40] <<- 'low_post'
      }
      else if(areas[i,23] > 81) {
        areas[i,40] <<- 'low_post'
      }
    }
    
    #WINGS & CORNERS
    else if(areas[i,24] >35){
      if(areas[i,23] <= 40 && areas[i,23] > 23){
        areas[i,40] <<- 'right_wing'
      }
      else if(areas[i,23] <= 71 && areas[i,23] > 54) {
        areas[i,40] <<- 'left_wing'
      }
      else if(areas[i,23] <= 23) {
        areas[i,40] <<- 'right_corner'
      }
      else if(areas[i,23] > 71) {
        areas[i,40] <<- 'left_corner'
      }
    }
    
    else if(areas[i,24] < 15){
      if(areas[i,23] <= 40 && areas[i,23] > 23){
        areas[i,40] <<- 'left_wing'
      }
      else if(areas[i,23] <= 71 && areas[i,23] > 54) {
        areas[i,40] <<- 'right_wing'
      }
      else if(areas[i,23] <= 23) {
        areas[i,40] <<- 'left_corner'
      }
      else if(areas[i,23] > 71) {
        areas[i,40] <<- 'right_corner'
      }
    }
    
  }
  areas$pass_area <- as.factor(areas$pass_area)
  areas$poss_area <- as.factor(areas$poss_area)
}


#For each pass that our selected player makes,
#calculate the FG of the shooter in that area
getShooterFGinArea <- function() {
  #get the shooters FG % in the area that he caught in
  shooter_ids <- 'null'
  pass_from <- 'null'
  pass_to <- 'null'
  shooter_fg_in_area <- 'null'
  #shot_count <- 'null'
  
  for(i in 1:nrow(areas)) {
    #get pass area
    pass_area <- areas[i,39]
    #create pass area list
    pass_from <- append(pass_from, pass_area)
    
    #get possession area
    poss_area <- areas[i,40]
    #create list
    pass_to <- append(pass_to, poss_area) #this does not exist yet
    
    #get shooter
    shooter <- areas[i,19]
    #create list
    shooter_ids <- append(shooter_ids, shooter)
    
    #create df of this shooter in this possession area
    shooter_area_df <<- areas[which(areas$shooter == shooter & areas$V40 == poss_area),]
    #get the fg pct
    shooter_fg_in_area <- append(shooter_fg_in_area, mean(shooter_area_df$made))
  }
  #create df of all the lists
  all_shooter_all_areas <<- data.frame(pass_from, pass_to, shooter_ids, shooter_fg_in_area)
  #add passer id which is same for all
  all_shooter_all_areas$passer <<- areas[i,9]
  
  all_shooter_all_areas <<- merge(all_shooter_all_areas,players,by.x = "shooter_ids", by.y = "player_id", all.x = TRUE)
  all_shooter_all_areas <<- all_shooter_all_areas[,-c(9,10,11,12)]
}


getFG_pass_from <- function() {
  #get FG pct of all shots when player passed from area on court
  passfrom_top <- mean(areas[which(areas$V39 == 'top_key'),31])
  passfrom_lw <- mean(areas[which(areas$V39 == 'left_wing'),31])
  passfrom_rw <- mean(areas[which(areas$V39 == 'right_wing'),31])
  passfrom_lc <- mean(areas[which(areas$V39 == 'left_corner'),31])
  passfrom_rc <- mean(areas[which(areas$V39 == 'right_corner'),31])
  passfrom_lp <- mean(areas[which(areas$V39 == 'low_post'),31])
  passfrom_hp <- mean(areas[which(areas$V39 == 'high_post'),31])
  
  countfrom_top <- nrow(areas[which(areas$V39 == 'top_key'),])
  countfrom_lw <- nrow(areas[which(areas$V39 == 'left_wing'),])
  countfrom_rw <- nrow(areas[which(areas$V39 == 'right_wing'),])
  countfrom_lc <- nrow(areas[which(areas$V39 == 'left_corner'),])
  countfrom_rc <- nrow(areas[which(areas$V39 == 'right_corner'),])
  countfrom_lp <- nrow(areas[which(areas$V39 == 'low_post'),])
  countfrom_hp <- nrow(areas[which(areas$V39 == 'high_post'),])
  
  topofkey  <- c(passfrom_top, countfrom_top)
  leftwing  <- c(passfrom_lw, countfrom_lw)
  rightwing  <- c(passfrom_rw, countfrom_rw)
  leftcorner  <- c(passfrom_lc, countfrom_lc)
  rightcorner  <- c(passfrom_rc, countfrom_rc)
  lowpost  <- c(passfrom_lp, countfrom_lp)
  highpost  <- c(passfrom_hp, countfrom_hp)
  
  fgp_df <<- data.frame(rbind(topofkey, leftwing, rightwing, leftcorner, rightcorner, lowpost, highpost))
  
  
}


#Viz marking all passes from player
#Circle is made shot, X is missed shot
#Green is <3 dribbles from shooter before shot, Orange is >3 (Color coding shot creation)
vizPass <- fullcourt() + geom_point(data=passer,aes(x=pass_x,y=pass_y), 
                             color = ifelse(passer$dribbles<3, 'green', 'orange'), shape =ifelse(passer$made==1, 4,1)) 
    


