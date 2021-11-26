#COLLEY RANKINGS NCAA BASKETBALL TEAMS

rm(list=ls())
#loading necessary packages

library(tidyverse)
library(igraph)
library(formattable)

#Importing Data
meta <- "/Users/DidiTrifonova/Documents/GitHub/MatrixProject/Code/"
data <- read.csv((paste0(meta,"RegularSeasonCompactResults.csv")))
data16 <- data %>% filter(Season == 2016)

############################### 3X3 EXAMPLE ##############################
##########################################################################
##########################################################################

# SMALLER EXAMPLE WITH A 3X3 MATRIX (3 pac-12 teams)
# COLORADO 1160
# WASHINGTON 1449 
# OREGON 1332

#Vector with team identifiers 
teams <- c(1160,1449,1332)
teamNames <- c("Colorado","Washington","Oregon")

#creating an nxn matrix with zeroes 
m1 <- matrix(0L,nrow = length(teams), ncol = length(teams))

#filtering data to only include relevant games 
teamdata <- data16 %>% filter(Wteam %in% teams & Lteam %in% teams)

# filling in adjaceny matrix 
for(i in 1:nrow(teamdata)) #i is 1 to 6 
{
  for(j in 1:length(teams)) #j is 1 to 3 
  {
    if(teamdata$Wteam[i] == teams[j])
    {
      m1[j,which(teamdata$Lteam[i] == teams)[[1]]] = m1[j,which(teamdata$Lteam[i] == teams)[[1]]] +1; 
    }
  }
}

#creating C matrix 
c1 <- matrix(0L,nrow = length(teams), ncol = length(teams))

#creating a vector to hold the number of games played 
gamesPlayed <- numeric(length(teams))

for(i in 1:nrow(teamdata))
{
  for(j in 1:length(teams))
  {
    if(teamdata$Wteam[i] == teams[j] || teamdata$Lteam[i] == teams[j])
    {
      gamesPlayed[j]= gamesPlayed[j] +1 
    }
  }
}

#entries except for i=j 
#entry is how many times team i has played team j 
for(i in 1:nrow(c1))
{
  for(j in 1:ncol(c1))
  {
    c1[i,j] = -(m1[i,j] + m1[j,i])
  }
}

# diagonals of C are games played + 2 
diagonalsC = gamesPlayed +2

for(i in 1:nrow(c1))
{
  c1[i,i] = diagonalsC[i]
}

#vector for number of wins
numWin <- rowSums(m1)

#vector for number of losses
numLoss <- colSums(m1)

# creating the b vector
# 1 + (wins - losses)/2
bVec <- numeric(length(teams))
for(i in 1:length(bVec))
{
  bVec[i] = 1 + (numWin[i] - numLoss[i])/2
}

#Solving the system Cr=b
r = solve(c1,bVec)
 
rankings <- cbind(teamNames,r)
rankings <- rankings %>% as_tibble()
rankings <- rankings %>% rename("Team" = "teamNames", "Score" = "r")
rankings <- rankings[order(rankings$Score,decreasing=TRUE),]

################################# PAC 12 #################################
##########################################################################
##########################################################################

#Vector with team identifiers 
teams1 <- c(1160,1449,1332,1333,1417,1428,1112,1143,1425,1390,1113,1450)
teamNames1 <- c("Colorado","Washington","Oregon","Oregon State","UCLA",
                "Utah","Arizona","California","USC","Stanford","Arizona State",
                "Washington State")

#creating an nxn matrix with zeroes 
m2 <- matrix(0L,nrow = length(teams1), ncol = length(teams1))

#filtering data to only include relevant games 
teamdata1 <- data16 %>% filter(Wteam %in% teams1 & Lteam %in% teams1)

# filling in adjaceny matrix 
for(i in 1:nrow(teamdata1)) 
{
  for(j in 1:length(teams1)) 
  {
    if(teamdata1$Wteam[i] == teams1[j])
    {
      m2[j,which(teamdata1$Lteam[i] == teams1)[[1]]] = m2[j,which(teamdata1$Lteam[i] == teams1)[[1]]] +1; 
    }
  }
}

#creating C matrix 
c2 <- matrix(0L,nrow = length(teams1), ncol = length(teams1))

#creating a vector to hold the number of games played 
gamesPlayed1 <- numeric(length(teams1))

for(i in 1:nrow(teamdata1))
{
  for(j in 1:length(teams1))
  {
    if(teamdata1$Wteam[i] == teams1[j] || teamdata1$Lteam[i] == teams1[j])
    {
      gamesPlayed1[j]= gamesPlayed1[j] +1 
    }
  }
}

#entries except for i=j 
#entry is how many times team i has played team j 
for(i in 1:nrow(c2))
{
  for(j in 1:ncol(c2))
  {
    c2[i,j] = -(m2[i,j] + m2[j,i])
  }
}

# diagonals of C are games played + 2 
diagonalsC1 = gamesPlayed1 +2

for(i in 1:nrow(c2))
{
  c2[i,i] = diagonalsC1[i]
}

#vector for number of wins
numWin1 <- rowSums(m2)

#vector for number of losses
numLoss1 <- colSums(m2)

# creating the b vector
# 1 + (wins - losses)/2
bVec1 <- numeric(length(teams1))
for(i in 1:length(bVec1))
{
  bVec1[i] = 1 + (numWin1[i] - numLoss1[i])/2
}

#Solving the system Cr=b
r1 = solve(c2,bVec1)

rankings1 <- cbind(teamNames1,r1)
rankings1 <- rankings1 %>% as_tibble()
rankings1 <- rankings1 %>% rename("Team" = "teamNames1", "Colley Score" = "r1")
rankings1 <- rankings1[order(rankings1$'Colley Score',decreasing=TRUE),]

#figure 5
formattable(rankings1)

#figure 6
winLossTable <- cbind(teamNames1,numWin1,numLoss1) %>% as_tibble()
winLossTable <- winLossTable %>% rename("Team" = "teamNames1", "Wins" = "numWin1", "Losses" = "numLoss1")
winLossTable$Wins <- winLossTable$Wins %>% as.numeric()
winLossTable$Losses <- winLossTable$Losses %>% as.numeric()
winLossTable <- winLossTable %>% mutate(Percentage = Wins/(Wins+Losses))
winLossTable$Percentage <- winLossTable$Percentage %>% as.numeric()
winLossTable <- winLossTable[order(winLossTable$Percentage,decreasing=TRUE),]
formattable(winLossTable)



