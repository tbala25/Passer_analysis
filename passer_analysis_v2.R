##RUN cleaning.R before this
#Sets directory to my folder to load data set
#setwd("/Users/Tejas/Desktop/Kings_Project/")
#Loads fullcourt template
#Originally from Rajiv Shah: GitHub @rajshah4
source("_function_fullcourt.R")

library(dplyr)
library(party)
library(cutpointr)
library(randomForest)


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

transformed[is.na(transformed$pass_shot_clock), ]$pass_shot_clock <- transformed[is.na(transformed$pass_shot_clock),]$pass_game_clock

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
# allshootfgs <- areas %>%
#   group_by(poss_area, shooter) %>% 
#   summarise(mean = mean(made,na.rm=TRUE))

##################################################################
#Gets different player's dfs as passers
#################################################################
#Lebron <- getPlayer("LeBron James", areas)
#SteveNash <- getPlayer("Steve Nash", areas)
#KD <- getPlayer("Kevin Durant", areas)
#Steph <- getPlayer("Stephen Curry", areas)
Ben <- getPlayer("Ben McLemore", areas)


###############################################################
#Group player's passes by area passed to and calculate fg% of all shooters combined in that area
###############################################################

# Lebron_grouped <- Lebron %>% 
#   group_by(pass_area) %>% 
#   summarise(mean = mean(made, na.rm = TRUE), freq = n())
# 
# Steph_grouped <- Steph %>% 
#   group_by(pass_area) %>% 
#   summarise(mean = mean(made, na.rm = TRUE), freq = n())



Ben_fgp <- Ben %>%
  group_by(pass_area) %>%
  summarise(mean = mean(made, na.rm = TRUE), freq = n())

Ben_ndd <- Ben %>%
  group_by(pass_area) %>%
  summarise(mean = mean(ndd, na.rm = TRUE), freq = n())

Ben_dribbles <- Ben %>%
  group_by(pass_area) %>%
  summarise(mean = mean(dribbles, na.rm = TRUE), freq = n())



##################################################################
#Gets df of all passes and that shooter's fg% when recieving the ball in that area
#For histogram
#################################################################
# Lebron_shooters_fgp <- Lebron %>%
#   rowwise() %>%
#   mutate(shooter_fgp = )


################################################################
#Predicting where Lebron will pass
#And FG% of shooter of that pass
################################################################

# var_imp_select <- ctree(made~ ., Lebron)
# plot(var_imp_select)

selected_var <- transformed %>%
  na.omit() %>%
  select("quarter", "pass_x", "pass_y", "pass_distance", "pass_shot_clock", "pass_game_clock", "poss_x", "poss_y", "dribbles", "distance_travelled", "made",  "ndd")


selected_small <- transformed %>%
  na.omit() %>%
  select("pass_distance", "pass_shot_clock", "pass_game_clock", "dribbles", "distance_travelled", "made",  "ndd")


var_imp_select <- ctree(made~ ., selected_var)
plot(var_imp_select)

#Elbow Method for finding the optimal number of clusters
#set.seed(4)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- selected_small
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=30,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


clusters <- kmeans(data,3, nstart = 15)
plot(clusters)

transformed2 <- transformed %>% na.omit()

transformed2$three_clust <- clusters$cluster
write.csv(transformed2, "pass_clustered_v1.csv")

#################################################################################
#################################################################################
data <- areas
smp_size <- floor(.8 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind,]
test <- data[-train_ind,]
######################################################################################
mylogit <- glm(made ~ ndd + pass_distance + pass_shot_clock + dribbles + distance_travelled + poss_area + pass_area + shooter_position + defender_position + passer_position , data = train, family = "binomial" )
summary(mylogit)
confint(mylogit)
confint.default(mylogit)
exp(coef(mylogit))

exp(cbind(OR = coef(mylogit), confint(mylogit)))

test$predict <- predict(mylogit, newdata = test, type = "response")
test$p1 <- ifelse(test$predict > .40, 1, 0)

tp <- nrow(test[test$made == 1 & test$p1 == 1,])
tn <- nrow(test[test$made == 0 & test$p1 == 0,])
fp <- nrow(test[test$made == 0 & test$p1 == 1,])
fn <- nrow(test[test$made == 1 & test$p1 == 0,])

precision = tp/(tp+fp)
recall = tp/(tp+fn)

fp_mean <- mean(test[test$made == 0 & test$p1 == 1,]$predict, na.rm = TRUE)
tp_mean <- mean(test[test$made == 1 & test$p1 == 1,]$predict, na.rm = TRUE)
fn_mean <- mean(test[test$made == 1 & test$p1 == 0,]$predict, na.rm = TRUE)
tn_mean <- mean(test[test$made == 0 & test$p1 == 0,]$predict, na.rm = TRUE)

nrow(test[test$made == 0,])

fp_df <- test[test$made == 0 & test$p1 == 1,]
hist(fp_df$ndd, na.rm = TRUE)

fn_df <- test[test$made == 1 & test$p1 == 0,]
hist(fn_df$ndd, na.rm = TRUE)



#############################################
classifier <- randomForest(formula = made ~ ndd + pass_distance + pass_shot_clock + dribbles + distance_travelled + poss_area + pass_area + shooter_position + defender_position + passer_position , data = train, na.action = na.exclude)

