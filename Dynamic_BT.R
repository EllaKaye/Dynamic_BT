library(ggplot2)

results <- read.csv("footballresults.csv", header=TRUE)
leagues <- read.csv("footballleagues.csv", header=TRUE)
teams <- read.csv("footballteams.csv", header=TRUE)

dim(results) # 38782    
colnames(results)  # "No.LEAGUE" "YEAR"      "No.WEEK"   "No.TEAM1"  "No.TEAM2"  "GOALTEAM1" "GOALTEAM2" "CODEWID"  

table(results[,1])

# premier league
premier <- subset(results, No.LEAGUE == 9)
dim(premier)
table(premier$YEAR)
head(premier)
UK.teams <- sort(union(unique(premier$No.TEAM1), unique(premier$No.TEAM2)))

# remove draws
table(premier$CODEWID)
premier.win <- subset(premier, CODEWID != "D")
dim(premier.win)

# number of times i beats j at time t
# start with t = 2000

premier.2000 <- subset(premier, YEAR == 2000)
nrow(premier.2000)

premier.win.2000 <- subset(premier.win, YEAR == 2000)
dim(premier.win.2000)
UK.teams.2000 <- sort(union(unique(premier.win.2000$No.TEAM1), unique(premier.win.2000$No.TEAM2)))
K <- length(UK.teams.2000)
beats <- matrix(0, K, K)
beats
rownames(beats) <- UK.teams.2000
colnames(beats) <- UK.teams.2000
beats

# beats[as.character(premier.win.2000[1,]$No.TEAM1), "130"] <- NA

for (i in 1:nrow(premier.win.2000)) {
  if (premier.win.2000[i,]$CODEWID == "W1") {
    beats[as.character(premier.win.2000[i,]$No.TEAM1), as.character(premier.win.2000[i,]$No.TEAM2)] <- beats[as.character(premier.win.2000[i,]$No.TEAM1), as.character(premier.win.2000[i,]$No.TEAM2)] + 1
  }
  else {
    beats[as.character(premier.win.2000[i,]$No.TEAM2), as.character(premier.win.2000[i,]$No.TEAM1)] <- beats[as.character(premier.win.2000[i,]$No.TEAM2), as.character(premier.win.2000[i,]$No.TEAM1)] + 1
  }
}

sum(beats)

premier.times.played.2000 <- beats + t(beats)
sum(premier.times.played.2000)/2

# matrix for number of times i beats j at time t, as function
beats.t <- function(data, league.no, year) {
  data <- subset(data, YEAR == year)
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  beats <- matrix(0, K, K)
  rownames(beats) <- teams
  colnames(beats) <- teams
  for (i in 1:nrow(data)) {
  
    if (data[i,]$CODEWID == "W1") {
        beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] <- beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] + 1
      }
    else {
        beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] <- beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] + 1
      }
    }
  beats
}

# matrix for number of times i plays j at time t, as function
played.t <- function(beats) {
  beats + t(beats)
}

