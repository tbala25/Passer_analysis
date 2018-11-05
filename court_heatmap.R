lowpost <- c(0, 12, 12, 38, .68, 100)
highpost <- c(12, 22, 12, 38, .50, 120)
leftcorner <- c(0, 12, 38, 50, .38, 60)
rightcorner <- c(0, 12, 0, 12, .38, 60)
leftwing <- c(12, 35, 38, 50, .75, 150)
rightwing <- c(12, 35, 0, 12, .75, 150)
topofkey <- c(22, 35, 12, 38, .50, 120)
midcourt <- c(35, 55, 0, 50, .40, 20)
transition <- c(55,94, 0, 50, .30, 10)

heatmap_df <- data.frame(rbind(lowpost, highpost, leftcorner, rightcorner, leftwing, rightwing, topofkey, midcourt, transition))
heatmap_df <- `colnames<-`(heatmap_df, c('xmin', 'xmax', 'ymin', 'ymax', 'pct', 'freq'))
heatmap_df$freq <- heatmap_df$freq/sum(heatmap_df$freq)

heatmap_df <- heatmap_df[,-c(5,6)]
heatmap_df <- merge(heatmap_df, fgp_df, by = "row.names")
heatmap_df <- heatmap_df[,-1]
heatmap_df <- `colnames<-`(heatmap_df, c('xmin', 'xmax', 'ymin', 'ymax', 'pct', 'freq'))
heatmap_df$freq <- heatmap_df$freq/sum(heatmap_df$freq)





#fullcourt() + geom_rect(xmin = 0, xmax = 12, ymin = 12, ymax = 38, fill = "red", alpha = .5) + geom_rect(xmin = 12, xmax = 22, ymin = 12, ymax = 38, fill = "blue", alpha = .5) + geom_rect(xmin = 0, xmax = 12, ymin = 38, ymax = 50, fill = "blue", alpha = .5) + geom_rect(xmin = 0, xmax = 12, ymin = 0, ymax = 12, fill = "blue", alpha = .5) + geom_rect(xmin = 10, xmax = 35, ymin = 38, ymax = 50, fill = "green", alpha = .5)+ geom_rect(xmin = 10, xmax = 35, ymin = 0, ymax = 12, fill = "green", alpha = .5)  + geom_rect(xmin = 22, xmax = 35, ymin = 12, ymax = 38, fill = "red", alpha = .5)  + geom_rect(xmin = 35, xmax = 55, ymin = 0, ymax = 50, fill = "blue", alpha = .5)



fullcourt() + geom_rect(xmin = heatmap_df[1,1], xmax = heatmap_df[1,2], ymin = heatmap_df[1,3], ymax = heatmap_df[1,4], aes(fill = heatmap_df[1,5], alpha = heatmap_df[1,6])) +  geom_rect(xmin = heatmap_df[2,1], xmax = heatmap_df[2,2], ymin = heatmap_df[2,3], ymax = heatmap_df[2,4], aes(fill = heatmap_df[2,5], alpha = heatmap_df[2,6])) +  geom_rect(xmin = heatmap_df[3,1], xmax = heatmap_df[3,2], ymin = heatmap_df[3,3], ymax = heatmap_df[3,4], aes(fill = heatmap_df[3,5], alpha = heatmap_df[3,6])) + geom_rect(xmin = heatmap_df[4,1], xmax = heatmap_df[4,2], ymin = heatmap_df[4,3], ymax = heatmap_df[4,4], aes(fill = heatmap_df[4,5], alpha = heatmap_df[4,6])) + geom_rect(xmin = heatmap_df[5,1], xmax = heatmap_df[5,2], ymin = heatmap_df[5,3], ymax = heatmap_df[5,4], aes(fill = heatmap_df[5,5], alpha = heatmap_df[5,6])) + geom_rect(xmin = heatmap_df[6,1], xmax = heatmap_df[6,2], ymin = heatmap_df[6,3], ymax = heatmap_df[6,4], aes(fill = heatmap_df[6,5], alpha = heatmap_df[6,6])) + geom_rect(xmin = heatmap_df[7,1], xmax = heatmap_df[7,2], ymin = heatmap_df[7,3], ymax = heatmap_df[7,4], aes(fill = heatmap_df[7,5], alpha = heatmap_df[7,6])) + geom_rect(xmin = heatmap_df[8,1], xmax = heatmap_df[8,2], ymin = heatmap_df[8,3], ymax = heatmap_df[8,4], aes(fill = heatmap_df[8,5], alpha = heatmap_df[8,6])) + geom_rect(xmin = heatmap_df[9,1], xmax = heatmap_df[9,2], ymin = heatmap_df[9,3], ymax = heatmap_df[9,4], aes(fill = heatmap_df[9,5], alpha = heatmap_df[9,6])) + scale_fill_gradient2(low = 'red', high = 'green', mid = 'yellow' , midpoint = .5) + scale_alpha_continuous(range = c(0,.7)) 
