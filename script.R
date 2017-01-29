library(dplyr)
library(ggplot2)
library(neuralnet)
data <- read.csv("data.csv", stringsAsFactor = FALSE)

train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]

# Functions
indexOf <- function(feature, data){
    return (grep(feature, colnames(data)))
}

without <- function(colFeatures, data){
    indexes <- c()
    for (feat in colFeatures){
        indexes <- cbind(indexes, indexOf(feat, data))
    }
    return (indexes)
}

scaleData <- function(data){
    maxs <- apply(data, 2, max)
    mins <- apply(data, 2, min)
    scaled.data <- as.data.frame(scale(data, center = mins, scale=maxs-mins))
    return (cbind(scaled.data, data$shot_made_flag))
}

convertData <- function(data){
    data$shot_type <- as.numeric(substring(data$shot_type, 0, 1))
    data$season <- as.numeric(substring(data$season, 0, 4))
    #data$shot_made_flag <- as.factor(data$shot_made_flag)
    #data$shot_made_flag <- factor(data$shot_made_flag, levels=c("1","0"))
    data$shot_zone_range <- as.numeric(factor(data$shot_zone_range, labels=c("1", "2", "3", "4", "5")))
    data$shot_zone_area <- as.numeric(factor(data$shot_zone_area, labels=c("1", "2", "3", "4", "5", "6")))
    data$shot_zone_basic <- as.numeric(factor(data$shot_zone_basic, labels=c("1", "2", "3", "4", "5", "6", "7")))
    #data$year <- as.numeric(substring(data$game_date, 0, 4))
    #data$month <- as.numeric(substring(data$game_date, 5, 6))
    #data$day <- as.numeric(substring(data$game_date, 9, 10))
    return (data)
}


# Modifying data
withoutCols <- c("opponent", "action_type", "combined_shot_type", "team_name", "matchup", "game_event_id", "game_id", "game_date", "shot_id")

train_without<-train[,-without(withoutCols, train)]
train_without$shot_made_flag <- as.numeric(train_without$shot_made_flag)-1
test <- test[, -without(withoutCols, test)]

train_without <- convertData(train_without)
test <- convertData(test)

train_without <- scaleData(train_without)
test <- scaleData(test)

f <- paste(c("shot_distance", "playoffs", "period"), collapse = " + ")
f <- paste('shot_made_flag ~', f)
f <- as.formula(f)

nn <- neuralnet(f,train_without, hidden=c(10, 10, 10), linear.output=FALSE)
test <- test[,!(names(test) %in% "shot_made_flag")] # remove shot_made_flag as that is the predictive
predicted.nn.values <- compute(nn, test)

plot(nn)







# Plot data
#pplot <- function(feat) {
#    feat <- substitute(feat)
#    ggplot(data = train, aes_q(x = feat)) +
#        geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
#    scale_fill_brewer(palette = "Set1", direction = -1) +
#     ggtitle(paste("accuracy by", feat))
#}
#
#courtplot <- function(feat) {
#        feat <- substitute(feat)
#    train %>% 
#    ggplot(aes(x = lon, y = lat)) +
#        geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
#        ylim(c(33.7, 34.0883)) +
#        scale_color_brewer(palette = "Set1") +
#        theme_void() +
#        ggtitle(paste(feat))
#}
#
#courtplot(combined_shot_type) # Plot shots on the court
#
## plots shots without jump shot
#ggplot() + geom_point(data = filter(train, combined_shot_type == "Jump Shot"), aes(x = lon, y = lat), color = "grey", alpha = 0.3, size = 2) + geom_point(data = filter(train, combined_shot_type != "Jump Shot"), aes(x = lon, y = lat, color = combined_shot_type), alpha = 0.7, size = 3) + ylim(c(33.7, 34.0883)) + scale_color_brewer(palette = "Set1") + theme_void() + ggtitle("Shot Types")
