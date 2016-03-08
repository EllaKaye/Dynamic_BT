## Data for top UK 7 teams, win lose and draw, one matrix for each timepoint

# 3     131              CHELSEA
# 8     136              EVERTON
#11     139            LIVERPOOL
#10     138          ASTON VILLA
#14     142              ARSENAL
#15     143            TOTTENHAM
#19     147    MANCHESTER UNITED

results <- read.csv("footballresults.csv", header=TRUE)
premier <- subset(results, No.LEAGUE == 9)
top.teams <- c(131, 136, 138, 139, 142, 143, 147)
premier.top <- subset(premier, No.TEAM1 %in% top.teams & No.TEAM2 %in% top.teams)
K.pt <- length(top.teams)
years.pt <- sort(unique(premier.top$YEAR))
TT.pt <- length(years.pt)
match_ups.pt <- t(combn(top.teams,2))
cnames <- c("idx", "Team.i", "Team.j", "win", "draw", "lose")
Kc2 <- choose(K.pt,2)

# set up list of data frames, one for each year
beats.pt <- replicate(TT.pt, as.data.frame(matrix(c(1:Kc2, match_ups.pt[,1], match_ups.pt[,2], rep(0, 3*Kc2)), Kc2, 6, dimnames=list(1:Kc2, cnames))), simplify = FALSE)
names(beats.pt) <- years.pt

# go through premier top and put each result in the right place
for (l in 1:nrow(premier.top)) {
  
  # find right list item for the year
  year.ind <- which(years.pt == premier.top[l,]$YEAR)
  
  # find row number (id) to update
  pair <- sort(c(premier.top[l, "No.TEAM1"], premier.top[l, "No.TEAM2"]))
  id <- subset(beats.pt[[year.ind]], Team.i == pair[1] & Team.j == pair[2])$idx  
  
  # increment the right entry
  res <- premier.top[l, "CODEWID"]
  
  if (res == "D") {
    beats.pt[[year.ind]][id, "draw"] <- beats.pt[[year.ind]][id, "draw"] + 1
  }
  
  if (res == "W1" & pair[1] == premier.top[l, "No.TEAM1"]) {
    beats.pt[[year.ind]][id, "win"] <- beats.pt[[year.ind]][id, "win"] + 1
  }
  
  if (res == "W1" & pair[1] != premier.top[l, "No.TEAM1"]) {
    beats.pt[[year.ind]][id, "lose"] <- beats.pt[[year.ind]][id, "lose"] + 1
  }
  
  if (res == "W2" & pair[1] == premier.top[l, "No.TEAM1"]) {
    beats.pt[[year.ind]][id, "lose"] <- beats.pt[[year.ind]][id, "lose"] + 1
  }
  
  if (res == "W2" & pair[1] != premier.top[l, "No.TEAM1"]) {
    beats.pt[[year.ind]][id, "win"] <- beats.pt[[year.ind]][id, "win"] + 1
  }
}

# cull final year (not enough data)
beats.pt[[11]] <- NULL


# take only win/draw/lose columns
beats.pt <- lapply(beats.pt, function(df) df[,c("win", "draw", "lose")])

## beats.pt is now a list of 10 data-frames, one for each year 2000-2009,
## where wach data frame has 7 choose 2 rows, for each pairing of the 7 top teams in the premier league
## and columns for the number of time team i wins/draws/loses against team j
## the matrix match_ups.pt gives the corresponding team i and j for each row (i in col1, j in col2)