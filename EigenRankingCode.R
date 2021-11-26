#EIGEN-RANKING NCAA BASKETBALL TEAMS

rm(list=ls())
#loading necessary packages

library(tidyverse)
library(igraph)
library(formattable)

#Importing Data
meta <- "/Users/DidiTrifonova/Documents/GitHub/MatrixProject/dataSets/"
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

#eigenvalues and eigenvectors
e1 <- eigen(m1)
e1$values
e1$vectors

#creating dataset with rankings 
rankings <- cbind(teamNames,abs(e1$vectors[,1])) %>% as_tibble()
rankings <- rankings %>% rename("Score" = "V2","Team" = "teamNames")
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

#eigenvalues and eigenvectors
e2 <- eigen(m2)
e2$values
e2$vectors 

#creating dataset with rankings 
rankings1 <- cbind(teamNames1,abs(e2$vectors[,1])) %>% as_tibble()
rankings1 <- rankings1 %>% rename("Eigen Score" = "V2","Team" = "teamNames1" )
rankings1 <- rankings1[order(rankings1$'Eigen Score',decreasing=TRUE),]

################################# FIGURES ################################
##########################################################################
##########################################################################

#visualizing adjacency graph 
graph <- graph_from_adjacency_matrix(
  m2,
  mode = c("directed"),
  weighted = TRUE,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
#figure 1 
plot(graph,edge.arrow.size = .4,edge.curved = .4,vertex.color = "slate gray")

#figure 2 
teamTable <- c(1:12)
teamTable <- teamTable %>% as_tibble()
teamTable$Team <- teamNames1
teamTable <- teamTable %>% rename("Team Number" = "value")
formattable(teamTable)

#figure 3 
m2 <- m2 %>% as_tibble
names(m2) = teamNames1
adjacency <- cbind(teamNames1,m2)
adjacency <- adjacency %>% rename("team" ="teamNames1")
formattable(adjacency)

#figure 4
formattable(rankings1)
