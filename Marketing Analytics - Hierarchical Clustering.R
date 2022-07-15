#Marketing Analytics Project - Clustering Top NBA Players for FS Basketball

library(caret)
library(stringr)

#Reading in dataset
nba  = read.csv("all_seasons.csv")
nrow(nba)

#Modifying season variable to be useable for analysis
nba$season = str_sub(nba$season, end=-4)

#Extracting all observations from 2018
nba2018 = nba[nba$season == "2018",]
nrow(nba2018)

#Replacing numeric row labels with player names fot future visualization
row.names(nba2018) = (nba2018$player_name)

#Extracting all observations with sufficient game experience (>=5 games played)
nbaexp = subset(nba2018, gp >= 5)
nrow(nbaexp)

#Eliminate undrafted players 
nbadrafted = nbaexp[nbaexp$draft_year != "Undrafted",]

#Eliminate outliers after visualization and looking at the data set
nbadrafted2 = nbadrafted[nbadrafted$player_name !="Giannis Antetokounmpo" & 
                           nbadrafted$player_name !="Joel Embiid" &
                           nbadrafted$player_name !="Davon Reed" &
                           nbadrafted$player_name !="Raul Neto",] 

#Extracting top 50 players based on net_rating
nbatop = head(nbadrafted2[order(nbadrafted$net_rating,decreasing = TRUE), ], 50)
nrow(nbatop)

#Extracting relevant columns with variables usable for clustering

#Physique, style, position and experience
nbatop = nbatop[c("player_height","player_weight","oreb_pct","dreb_pct","ast_pct","pts",
                  "gp","net_rating")]

#Renaming prepared dataset to original dataset
nba = nbatop
ncol(nba)

#Checking for high correlation at threshold 0.85 using caret
correlation.mat = cor(nba[,c(1:ncol(nba))]) 
correlation.mat
highcorrelation = findCorrelation(correlation.mat, 0.85)
highcorrelation

#Normalizing data
for (i in 1:ncol(nba)){
  nba[,i] = round(scale(nba[,i], center = T, scale = T),2)
}

#Calculating Euclidean distances
NBADistances = (dist(nba[1:ncol(nba)], method = "euclidean", diag=T))

#Complete Linkage Clustering
NBAClusters = hclust(NBADistances, method = "complete")

#Building agglomeration table
Agglomeration = data.frame(ClustersRemaining = 
                             seq(from = nrow(nba)-1, to=1, by=-1),
                           CentroidDistances = NBAClusters$height,
                           Item1 = NBAClusters$merge[,1],
                           Item2 = NBAClusters$merge[,2])
Agglomeration

#Plotting clusters
plot(NBAClusters, hang = -1, cex = 0.6)
rect.hclust(NBAClusters, k=5, border = 2:6)

#Adding cluster variables to dataset with unnormalized variables
nbatop$Cluster_Eucl_Complete = cutree(NBAClusters, k = 5)

#Comparison of clusters for giving names
FinalClusters = round(aggregate(. ~ Cluster_Eucl_Complete, data = nbatop, FUN = mean),2)
FinalClusters
