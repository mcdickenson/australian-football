# set up workspace
setwd('~/github/australian-football')
library(evtree)
library(mclust)
library(nnet)
library(party)
library(RColorBrewer)
library(randomForest)

# load data
players = read.csv('data/players.csv', as.is=TRUE)
players$league.factor = as.factor(players$league)
nrow(players)
odds = seq(1, nrow(players), by=2)
evens = seq(2, nrow(players), by=2)
features = c("age", "height", "weight")
target = "league.factor"
trnData = players[odds,  features]
tstData = players[evens, features]
trnClass = players[odds,  target]
tstClass = players[evens, target]
formula = league.factor ~ age + height + weight
data = cbind(trnData, trnClass)
names(data)[4] = target

# run models

################################
# hierarchical clustering
models = c("EII", "VEV", "VII", "EEE", "VVV")
mclust = Mclust(trnData, G=4, modelNames=models)
summary(mclust) # EEE

playersMclust = MclustDA(trnData, trnClass,
  modelType="MclustDA",
  modelNames="EEE")
save(playersMclust, file="rcode/playersMclust.rda")
load("rcode/playersMclust.rda")

trnPred = predict(playersMclust, trnData, type="class")$classification
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(playersMclust, tstData, type="class")$classification
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

################################
# party - conditional inference tree
playersCIT = ctree(formula, data=data)
save(playersCIT, file="rcode/playersCIT.rda")
load("rcode/playersCIT.rda")

trnPred = predict(playersCIT)
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(playersCIT, newdata=tstData)
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

################################
# evtree
playersEV = evtree(formula, data=data)
save(playersEV, file="rcode/playersEV.rda")
load("rcode/playersEV.rda")

trnPred = predict(playersEV, trnData)
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(playersEV, tstData)
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

################################
# random forest 
playersRF = randomForest(formula, data=data)
save(playersRF, file="rcode/playersRF.rda")
load("rcode/playersRF.rda")

trnPred = predict(playersRF, trnData)
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(playersRF, tstData)
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

################################
# neural net
ideal = class.ind(trnClass)
playersANN = nnet(trnData, ideal,
  size=10,
  MaxNWts=1500,
  softmax=TRUE,
  # censored=TRUE,
  skip=TRUE,
  maxit=100)
save(playersANN, file="rcode/playersANN.rda")
load("rcode/playersANN.rda")

trnPred = predict(playersANN, trnData, type="class")
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(playersANN, tstData, type="class")
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

playersANNmatrix = table(tstClass, tstPred)


