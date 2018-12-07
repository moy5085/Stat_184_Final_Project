library(ggplot2)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(data.table)

setwd("~/STAT_184/Final Project")

#This reads in the Game plays data and stores it as an object called 'play'
play<-fread("game_plays.csv")


#This makes a subset of the data from 'play' to only include when the event column is "Shot" and stores it as an object called 'shots'
shots<-play[event=="Shot"]
#This gets the count for each shot type and store the object as 'Type_Shot'
Type_Shot<-dcast(shots,event+secondaryType~.,length,value.var=c("secondaryType"))
#This renames the columns in 'Type_Shot'
setnames(Type_Shot,c('secondaryType','.'),c('Shot_Type','Count'))
#This makes a bar graph with the count of each type of shot taken
ggplot(Type_Shot,aes(Shot_Type,Count,fill = Shot_Type))+geom_bar(stat='identity')+ggtitle("Count of Each Shot Type Taken in Past 6 Years")+xlab("Shot Type")+ylab("Count")
#This saves the plot as a pdf named 'Count_of_Each_Shot_Type'
ggsave("Count_of_Each_Shot_Type.pdf", width = 8, height = 5) 

#This gets the total number of shots and stores table as 'total'
total<-dcast(shots,event~.,length,value.var=c("secondaryType"))
#This sets the names in 'total'
setnames(total,'.','Total')
#This merges the two tables, 'Type_Shot' and 'Total', and stores data as "Type_Shot"
Type_Shot<-merge(Type_Shot,total,all.x=T)
#This creates new column in 'Type_Shot' and calcuate the percentage each type of shot was taken
Type_Shot$Freq<-Type_Shot$Count/Type_Shot$Total*100
#This makes a bar graph with the percentage of each type of shot taken
ggplot(Type_Shot,aes(Shot_Type,Freq,fill=Shot_Type))+geom_bar(stat='identity')+ggtitle("Percentage of Types of Shots Taken in Past 6 Years")+xlab("Shot Type")+ylab("Percentage")
#This saves the plot as a pdf and saves it as 'Percentage_of_Each_Shot_Type'
ggsave("Percentage_of_Each_Shot_Type.pdf", width = 8, height = 5) 

#This makes a subset of the data from 'play' to only include when the event column is "Penalty"
penalty<-play[event=="Penalty"]
#This reads in the Team Info data and stores it as an object called 'team'
team<-fread("team_info.csv")
#This makes a function that takes the input of a team name and output the teams id number 
team_id <- function(myval) {
  if (myval %in% team$teamName){
    specific_team<-team[teamName==myval]
    return(specific_team$team_id)
  }
  else {
    return('N/A')
  }
}
#This makes a variable named 'id' using the team_id function, in this case it is the team id number for the flyers
id<-team_id('Flyers')
#This makes a subset which includes only penalties against the flyers in the past 6 years
penalty<-penalty[team_id_against==id]
#This gets the count for each penalty type and store the table as 'Type_Penalty'
Type_Penalty<-dcast(penalty,event+secondaryType~.,length,value.var=c("secondaryType"))
#This renames the columns in 'Type_Penalty'
setnames(Type_Penalty,c('secondaryType','.'),c('Penalty_Type','Count_Against'))
#This puts the penalties in order from most frequently occurred to least
Type_Penalty<-Type_Penalty[order(-Count_Against)]
#This keeps the 5 most frequently called penalties against the flyers
Type_Penalty<-Type_Penalty[1:5,]
#This makes a bar graph with the count of the 5 most frequent penalties called against the Flyers in past 6 years
ggplot(Type_Penalty,aes(Penalty_Type,Count_Against,fill=Penalty_Type))+geom_bar(stat='identity')+coord_flip()+ggtitle("Count of 5 Most Frequent Penalties Called Against Flyers in Past 6 Years")+xlab("Count")+ylab("Penalty Type")
#This saves the plot as a pdf and saves it as 'Count_of_Frequent_Penalties'
ggsave("Count_of_Frequent_Penalties.pdf", width = 8, height = 5) 

#This reads in the Game Shifts data and stores it as an object called 'shifts'
shifts<-fread('game_shifts.csv')
#This reads in the Player Information data and stores it as an object called 'players'
players<-fread('player_info.csv')
#This makes a subset of players to include their plaer_id, name, and position and stores it as 'id'
id<-players[,c('player_id','firstName','lastName','primaryPosition')]
#This merges the two data tables 'shifts' and 'id'
shifts<-merge(shifts,id,all.x=T)
#This formats the shifts start and end as hour, minutes, and seconds
shifts$shift_start<-hms::as.hms(shifts$shift_start)
shifts$shift_end<-hms::as.hms(shifts$shift_end)
#This calculates the duration of each shifts
shifts$duration<-shifts$shift_end-shifts$shift_start
#This puts the shifts in order from longest to shortest duration
shifts<-shifts[order(-duration)]
#This removes all goalies from list
#Since they typically play for the entire period, we are not interested in their shifts
shifts<-shifts[primaryPosition!='G']
#This write out .csv which is a new tidy data set with information on the 100 longest shifts in the past 6 years by players who are not goalies
fwrite(shifts[1:100,],"Longest_Shifts.csv")
