# set up workspace
setwd('~/github/australian-football')

# load data
leagues = c('afl', 'aleague', 'nrl', 'super')
for(league in leagues){
  filename = paste('data/players-', league, '.csv', sep='')
  cmd = paste(league, " = read.csv('", filename, "', as.is=TRUE)", sep="")
  eval(parse(text=cmd))
  # print(cmd)
}

# combine data
afl$league      = "afl"
aleague$league  = "aleague"
nrl$league      = "nrl"
super$league    = "super"

want = c("weight", "dob", "height", "player", "link", "team", "position", "league")

afl = afl[, want]
aleague = aleague[, want]
nrl = nrl[, want]
super = super[, want]

players = rbind(afl, aleague, nrl, super)

# compute age
players$dob.date = as.Date(players$dob, "%d-%m-%Y")
start = as.Date("2014-01-01", "%Y-%m-%d")
players$age = start - players$dob.date
players$age2 = as.numeric(floor(players$age / 365))
players$age = players$age2
want = c("height", "weight", "age",  "player", "link", "team", "position", "league")
players = players[, want]

players$height = as.numeric(players$height)
players$weight = as.numeric(players$weight)
players$age 
dim(players)
players = players[complete.cases(players), want]
dim(players)

write.csv(players, file="data/players.csv", row.names=FALSE)


# descriptive statistics
summary(players$height)
summary(players$weight)
summary(players$age)