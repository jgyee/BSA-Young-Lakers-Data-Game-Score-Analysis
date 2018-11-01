#GmSc
#Game Score; the formula is PTS + 0.4 * FG - 0.7 * FGA - 0.4*(FTA - FT) + 0.7 * ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV. 

#Game Score was created by John Hollinger to give a rough measure of a player's productivity for a single game. The scale is similar to that of points scored, (40 is an outstanding performance, 10 is an average performance, etc.). 

#Rewards pts as 1 point, rewards field goals by 0.4, penalizes attempts by 0.7, penalizes by free throws missed * 0.4,
#rewards rebounds, 0.7 * for offensive, 0.3* for defensive, steal is 1 point, assist is * 0.7, block is * 0.7,
#personal foul is * -0.4, and minus 1 point per turnover

#Set up
setwd("~/Desktop/Bruin_Sports/Data_Journalism/Article_1")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(norm)
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library(corrplot) 
library(visreg) 
library(rgl)
library(knitr)
library(scatterplot3d)
library(leaps)
library(car)
library(caret)
library(bestglm)
library(GGally)
library(DT)
library(xtable)

#Reading in the data sets

Ingram <- read_csv("Ingram_Game_Log.csv")
  
Ball <- read_csv("Lonzo_Game_Log.csv")

Kuzma <- read_csv("Kuzma_Game_Log.csv")

Hart <- read_csv("Hart_Game_Log.csv")

#Viewing the data sets

glimpse(Hart)
View(Hart)
str(Hart)
summary(Hart)


#Cleaning the data sets: removing games DNP, irrelevant variables
#Since they cannot control Minutes Played (MP), we exclude.
#For percentages where there were zero attempts, I had to coerce NA values
#to be imputed as the mean


#Rewards pts as 1 point, rewards field goals by 0.4, penalizes attempts by 0.7, penalizes by free throws missed * 0.4,
#rewards rebounds, 0.7 * for offensive, 0.3* for defensive, steal is 1 point, assist is * 0.7, block is * 0.7,
#personal foul is * -0.4, and minus 1 point per turnover


Hart_clean <- (Hart[!is.na(Hart$GmSc), - c(1:10, 30)])
Hart_clean$`FG%`[is.na(Hart_clean$`FG%`)] <- mean(Hart_clean$`FG%`, na.rm = TRUE)
Hart_clean$`FT%`[is.na(Hart_clean$`FT%`)] <- mean(Hart_clean$`FT%`, na.rm = TRUE)
Hart_clean$`3P%`[is.na(Hart_clean$`3P%`)] <- mean(Hart_clean$`3P%`, na.rm = TRUE)
Hart_clean$GmSc <- Hart_clean$GmSc

Ingram_clean <- Ingram[!is.na(Ingram$GmSc),- c(1:10, 30) ]
Ingram_clean$`FG%`[is.na(Ingram_clean$`FG%`)] <- mean(Ingram_clean$`FG%`, na.rm = TRUE)
Ingram_clean$`FT%`[is.na(Ingram_clean$`FT%`)] <- mean(Ingram_clean$`FT%`, na.rm = TRUE)
Ingram_clean$`3P%`[is.na(Ingram_clean$`3P%`)] <- mean(Ingram_clean$`3P%`, na.rm = TRUE)

Kuzma_clean <- Kuzma[!is.na(Kuzma$GmSc),- c(1:10, 30) ]
Kuzma_clean$`FG%`[is.na(Kuzma_clean$`FG%`)] <- mean(Kuzma_clean$`FG%`, na.rm = TRUE)
Kuzma_clean$`FT%`[is.na(Kuzma_clean$`FT%`)] <- mean(Kuzma_clean$`FT%`, na.rm = TRUE)
Kuzma_clean$`3P%`[is.na(Kuzma_clean$`3P%`)] <- mean(Kuzma_clean$`3P%`, na.rm = TRUE)

Ball_clean <- Ball[!is.na(Ball$GmSc),- c(1:10, 30)]
Ball_clean$`FG%`[is.na(Ball_clean$`FG%`)] <- mean(Ball_clean$`FG%`, na.rm = TRUE)
Ball_clean$`FT%`[is.na(Ball_clean$`FT%`)] <- mean(Ball_clean$`FT%`, na.rm = TRUE)
Ball_clean$`3P%`[is.na(Ball_clean$`3P%`)] <- mean(Ball_clean$`3P%`, na.rm = TRUE)

#View histogram of Gamescores

#Hart
Hart_plot <- ggplot(data = Hart_clean, aes(x = GmSc, color = "purple", fill = "yellow")) +
  geom_histogram(color = "purple", fill = "yellow", bins = 10)+
  ggtitle("Histogram of Hart's Game Scores")

#Ingram
Ingram_plot <- ggplot(data = Ingram_clean, aes(x = GmSc, color = "purple", fill = "yellow")) +
  geom_histogram(color = "purple", fill = "yellow", bins = 10)+
  ggtitle("Histogram of Ingram's Game Scores")

#Kuzma
Kuzma_plot <- ggplot(data = Kuzma_clean, aes(x = GmSc, color = "purple", fill = "yellow")) +
  geom_histogram(color = "purple", fill = "yellow", bins = 10)+
  ggtitle("Histogram of Kuzma's Game Scores")

#Ball
Ball_plot <- ggplot(data = Ball_clean, aes(x = GmSc, color = "purple", fill = "yellow")) +
  geom_histogram(color = "purple", fill = "yellow", bins = 10)+
  ggtitle("Histogram of Ball's Game Scores")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Aggregate Plot
multiplot(Hart_plot, Ingram_plot, Kuzma_plot, Ball_plot, cols = 2)

#For sake of pure comparison, let's see view the average GameScores
#of all the young Lakers

Kuzma_average <- mean(Kuzma$GmSc, na.rm = TRUE)
Ingram_average <- mean(Ingram$GmSc, na.rm = TRUE)
Hart_average <- mean(Hart$GmSc, na.rm = TRUE)
Ball_average <- mean(Ball$GmSc, na.rm = TRUE)

#Tidy the data
#Use the gather function to collapse into key-value pairs

Averages_df<- data.frame("Kuzma" = Kuzma$GmSc, "Ingram" = Ingram$GmSc, "Hart" = Hart$GmSc, "Ball" = Ball$GmSc)

Averages_df <- gather(Averages_df, key = c("Kuzma", "Ingram", "Hart", "Ball"), value = GmSc)
colnames(Averages_df)<- c("Player", "GameScore")
ggplot(data = Averages_df, aes(x = Player, y = GameScore,  color = "purple", fill = "yellow")) +
  geom_bar(stat = "summary", fun.y = "mean", color = 'black', fill = "yellow")



Averages <- matrix(c(Kuzma_average, Ingram_average, Hart_average, Ball_average), nrow = 4, ncol = 1)
Averages <- cbind(Averages, c("Kuzma", "Ingram", "Hart", "Ball"))
colnames(Averages) <- c("GameScore", "Names")
Averages <- as.data.frame(Averages)
ggplot(data = Averages, aes(x = Names, y = GameScore,  color = "purple", fill = "yellow")) +
geom_bar(stat = "identity", , color = "purple",fill = "yellow")


#Normalizing the data before Model Fitting, leaving Gamescore unscaled
#So that we can interpret in terms of pure Gamescore


Hart_normal <- as_data_frame(scale(Hart_clean[-length(Hart_clean)]))
Hart_normal$GmSc <- Hart_clean$GmSc

Ingram_normal <- as_data_frame(scale(Ingram_clean[, -length(Ingram_clean)]))
Ingram_normal$GmSc <- Ingram_clean$GmSc  

Kuzma_normal <- as_data_frame(scale(Kuzma_clean[, -length(Kuzma_clean)]))
Kuzma_normal$GmSc <- Kuzma_clean$GmSc  

Ball_normal <- as_data_frame(scale(Ball_clean[, -length(Ball_clean)]))
Ball_normal$GmSc <- Ball_clean$GmSc  

#Methodolgy of normalization:

#Should I normalize the data according to standard deviations
#of the stats of the individual player, according to league averages,
#or other method?

#Discussion: If indiviual basis: The model would include the 
#predictive power of the # of standard dev. above the mean for the
#player in predicting that player's GameScore
#YES, we want to normalize by individual since this will tell
# us what the individual should work on more for their own
# personal game, not compared to everyone else, or likewise,
# tell the individual what aspects of their game are negatively
# affecting their Gamescore and what they must improve upon



#Fitting Models: Predicting GmSc

#Separating test versus training data: Holdout method: training set, test set, and validation set:
#50/25/25 is standard procedure
#Using the predict function after training a model
#predict(model, test_data)



#OR use Cross-Validation Instead: k-fold: where k = 10

#trainControl parameters so you can use the whole data set
#and use cross-val. instead of splitting data
#Need to get rid of NA values in percentage statistics in order to train the data
#FG%, 3P%, and FT% are the variables. Should we get rid of these variables all together?
#THE PURPOSE OF K-FOLD CV is model checking, not model building, you want to train the desired
#model on all of the data afterwards

#Using bestglm package to find optimal model from subsets
#For bestglm, the outcome variable must be named y

#Hart Model Selection
regsubsets_Hart=regsubsets(x = Hart_normal$GmSc~ .,method=c("exhaustive"),nbest=1,data=Hart_normal, nvmax = 4, force.out = 18)
regsubsets_Hart
summary_Hart <- summary(regsubsets_Hart)
summary_Hart

plot(regsubsets_Hart, scale = "adjr2", max.size = 3, main = "Adjusted R^2")

which.max(summary_Hart$adjr2)

Hart_model<- lm(GmSc ~ FG + FGA + FT + AST + STL, data = Hart_normal)
summary(Hart_model)

#Ingram Model Selection
regsubsets_Ingram=regsubsets(x = Ingram_normal$GmSc~ .,method=c("exhaustive"),nbest=1,data=Ingram_normal, nvmax = 4, force.out = 18)
regsubsets_Ingram
summary_Ingram <- summary(regsubsets_Ingram)
summary_Ingram

Ingram_model<- lm(GmSc ~ FG + FGA + FT + AST + TOV, data = Ingram_normal)
summary(Ingram_model)


#Kuzma Model Selection
regsubsets_Kuzma=regsubsets(x = Ingram_normal$GmSc~ .,method=c("exhaustive"),nbest=1,data=Kuzma_normal, nvmax = 4, force.out = 18)
regsubsets_Kuzma
summary_Kuzma <- summary(regsubsets_Ingram)
summary_Kuzma

Kuzma_model<- lm(GmSc ~ FG + FGA + FT + AST + TOV, data = Kuzma_normal)
summary(Kuzma_model)

#Ball Model Selection

regsubsets_Ball=regsubsets(x = Ball_normal$GmSc~ .,method=c("exhaustive"),nbest=1,data=Ball_normal, nvmax = 4, force.out = 18)
regsubsets_Ball
summary_Ball <- summary(regsubsets_Ball)
summary_Ball

Ball_model<- lm(GmSc ~ FG + FGA + AST + STL + TOV, data = Ball_normal)
summary(Ball_model)


#Creating a table of coefficients
Hart_model$coefficients[7] = 0
Hart_coefficients <- Hart_model$coefficients

Kuzma_model$coefficients[7] = Kuzma_model$coefficients[6]
Kuzma_model$coefficients[6] = 0
Kuzma_coefficients <- Kuzma_model$coefficients

Ingram_model$coefficients[7] = Ingram_model$coefficients[6]
Ingram_model$coefficients[6] = 0
Ingram_coefficients <- Ingram_model$coefficients

Ball_model$coefficients[7] = Ball_model$coefficients[6]
Ball_model$coefficients[6] = Ball_model$coefficients[5]
Ball_model$coefficients[5] = Ball_model$coefficients[4]
Ball_model$coefficients[4] = 0
Ball_coefficients <- Ball_model$coefficients



#Need to add in zeroes for players who didn't have variable in their final model
Coefficients <- matrix(c(Hart_coefficients, Kuzma_coefficients, Ingram_coefficients, Ball_coefficients), ncol = 4)
colnames(Coefficients)<- c("Hart", "Kuzma", "Ingram", "Ball")
rownames(Coefficients)<- c("Intercept", "FG", "FGA", "FT", "AST", "STL", "TOV")


kable(Coefficients)


#Looking across the various models, we can notice some differences. 
#These results are somewhat suprising, as they give us a different perspective than looking at a player's overall
#season statistics, such as Josh Hart's 47% FG% on the season telling us that he is an efficient scorer. This 
#compared to Lonzo Ball's shockingly-low 36% FG% from last season. Lonzo also shot at a much higher clip than
#Hart... However, we must keep in mind that these statistics are normalized, so they are adjusted for 
#in respect to that player's average performance.THIS IS THE KEY, as to why the results are the way they are.

#For Hart, his field goal atempts most drastically
#affects his Gamescore negatively, meaning he should focus on better shot attempts.

#For Kuzma, his turnovers most adversely affect his Gamescore negatively, meaning he should focus on being more
#careful with the ball. In addtion, his field goal attempts do not dramatically decrease his Gamescore compared
#to his fellow teammates. 

#For Ingram, his field goals most positively improve his performance, so he should focus on staying aggressive
#and being a primary scoring option.

#For Ball, his assists and steals most positively affect his Gamescore relative to the other three players.
#His made field goals contribute the least to his Gamescore relative to the other three players.
#For him to succeed, he must maintain aggressive on defensive possession as well as improve on his scoring.






#What is the power of Gamescore, does it predict wins? Individual
#accopmlishments? Let's see... Maybe if we sum up the Gamescores
#of all the players we can see what the outcome will be.
#Which team had the overall higher cumulative game score?
#MAIN QUESTION: What is the predictive power of Gamescore 
#variable in determining win/loss. Only need the logical comparison
#variable of which team had higher overall Gamescore, and the 
#Winner of the game


#Further observations: Observing the importance of streaks


#Looking at the Advanced Stats box score rather than regular for predictors




#Who's success is most important for wins?

