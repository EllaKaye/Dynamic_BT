## N.B. not all these functions will be useful! They give different data types, and some are exploratory simpler cases.

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
played.t <- function(beats.mat) {
  beats.mat + t(beats.mat)
}

# array for number of times i beats j, for all years, as function
beats.all.t.array <- function(data, league.no) {
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)
  beats <- array(0, c(K, K, TT))
  dimnames(beats) <- list(teams, teams, years)
  for (i in 1:nrow(data)) {
    
    if (data[i,]$CODEWID == "W1") {
      beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2), as.character(data[i,]$YEAR)] <- beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2), as.character(data[i,]$YEAR)] + 1
    }
    else {
      beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1), as.character(data[i,]$YEAR)] <- beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1), as.character(data[i,]$YEAR)] + 1
    }
  }
  beats
}

# number of times i beats j, for all years, list of matrices
beats.all.t.list <- function(data, league.no) {
  
  # subset data by league and wins (no drawns)
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  
  # get team and year names 
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)
  
  # set up named list of matrices with named rows/columns
  beats <- replicate(TT, matrix(0, K, K, dimnames=list(teams, teams)), simplify = FALSE)
  names(beats) <- years
  
  # loop through rows and increment appropriate element of appropriate matrix with the winner
  for (i in 1:nrow(data)) {
    
    if (data[i,]$CODEWID == "W1") {
      beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] <- beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] + 1
    }
    
    else {
      beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] <- beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] + 1
    }
  }
  beats  
}

# helper function to return upper-triangular matrix by row
upper.tri.row <- function(mat) {
  t(mat)[lower.tri(t(mat))]
} 

# matrix to number of times i beats j (row for each pair), with cols as year
beats.all.t.mat <- function(data, league.no) {
  
  # subset data by league and wins (no drawns)
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  
  # get team and year names 
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)
  
  # set up named list of matrices with named rows/columns
  beats <- replicate(TT, matrix(0, K, K, dimnames=list(teams, teams)), simplify = FALSE)
  names(beats) <- years
  
  # loop through rows and increment appropriate element of appropriate matrix with the winner
  for (i in 1:nrow(data)) {
    
    if (data[i,]$CODEWID == "W1") {
      beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] <- beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2)] + 1
    }
    
    else {
      beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] <- beats[[which(years == data[i,]$YEAR)]][as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1)] + 1
    }
  }
  
  # generate matrix (dim (K choose 2), TT) of number of times i beats j (rows) by year (columns)
  sapply(beats, upper.tri.row)
}

# function to turn the output of beats.all.t.list to matrix to number of times i beats j, with cols as year
beats.all.t.list.to.mat <- function(beats.list) {
  sapply(beats.list, upper.tri.row)
}

# matrix to number of times i plays j, with cols as year, takes result of beats.all.t.list as input
played.all.t.list.to.mat <- function(beats.list) {
  out <- lapply(beats.list, played.t)
  out <- sapply(out, upper.tri.row)
  out
}

# array for number of times i plays j, for all years, as function
played.all.t <- function(beats.array) {
  out <- array(NA, dim(beats.array))
  
  for (i in 1:dim(A)[3]) {
    out[,,i] <- beats.array[,,i] + t(beats.array[,,i])
  }
  out
}

# array for number of times i beats j, for all years, as function
beats.all.t <- function(data, league.no) {
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)
  beats <- array(0, c(K, K, TT))
  dimnames(beats) <- list(teams, teams, years)
  
  # create array of data
  for (i in 1:nrow(data)) {
    
    if (data[i,]$CODEWID == "W1") {
      beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2), as.character(data[i,]$YEAR)] <- beats[as.character(data[i,]$No.TEAM1), as.character(data[i,]$No.TEAM2), as.character(data[i,]$YEAR)] + 1
    }
    else {
      beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1), as.character(data[i,]$YEAR)] <- beats[as.character(data[i,]$No.TEAM2), as.character(data[i,]$No.TEAM1), as.character(data[i,]$YEAR)] + 1
    }
  }
  beats
  
  # change into list, one for each year
  
}

# matrix with 1 for team i, -1 for team j, from total of K teams - dim (K choose 2), K
pairs.mat <- function(K) {
  # all possible pairs
  entries <- t(combn(K, 2))
  
  # initialise matrix
  mat <- matrix(0, choose(K,2), K)
  
  # fill in matrix with 1 for team i, -1 for team j
  for (l in 1:nrow(entries)) {
    mat[l,entries[l,1]] <- 1
    mat[l,entries[l,2]] <- -1
  }
  mat
}

# matrix with 1 for team i, -1 for team j, from total of K teams, and row of zeros where those teams don't play
# returns a list with one of these matrices for each year
pairs.mat.zero <- function(played.mat, K) {
  # number of years in the data
  TT <- ncol(played.mat)
  
  pairs <- pairs.mat(K)
  
  # initialise list
  out <- list()
  
  # create logical matrix (1 if played, 0 if not - regardless of number of times played)
  played.mat[as.logical(played.mat)] <- 1

  # for each timepoint, append to the list the pairs matrix with rows of zeros for any pairs that have not played
  for (i in 1:TT) {
    out[[length(out)+1]] <- pairs * played.mat[,i]
  }
  out
}

# function to extract info on years of league data, and teams played
years.teams <- function(data, league.no) {
  # subset data by league and wins (no drawns)
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  
  # get team and year names 
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)
  
  return(list(teams=teams, num.teams=K, years=years, num.years=TT))
}

# played, beats, pairs and X for home and away
home.away <- function(data, league.no) {
  # subset data for correct league and remove draws
  data <- subset(data, No.LEAGUE == league.no)
  data <- subset(data, CODEWID != "D")
  teams <- sort(union(unique(data$No.TEAM1), unique(data$No.TEAM2)))
  K <- length(teams)
  years <- sort(unique(data$YEAR))
  TT <- length(years)

  # create matrix of all possible pairs, playing both home and away
  entries <- t(combn(teams, 2))
  rev_entries <- entries[,c(2,1)]
  all_entries <- rbind(entries, rev_entries)
  colnames(all_entries) <- c("Home.Team", "Away.Team")
  
  # create zero-matrix of pairs by years
  years_mat <- matrix(0, nrow=2*choose(K,2), ncol = TT)
  colnames(years_mat) <- years
  
  # cbind the two matrices, and add index column, convert to data frame and create a copy for 'played' matrix
  beats.mat <- cbind(all_entries, years_mat)
  beats.mat <- cbind(beats.mat, 1:(2*choose(K,2)))
  colnames(beats.mat)[ncol(beats.mat)] <- "idx"
  beats.df <- as.data.frame(beats.mat)
  played.df <- beats.df
  
  # loop through data and assign to the appropriate place
  for (i in 1:nrow(data)) {
    id <- subset(beats.df, Home.Team == data[i, "No.TEAM1"] & Away.Team == data[i, "No.TEAM2"])$idx
    yr <- as.character(data[i, "YEAR"])
    played.df[id, yr] <- played.df[id, yr] + 1
    
    if (data[i, "CODEWID"] == "W1") {
      beats.df[id, yr]  <- beats.df[id, yr] + 1
    }
  }
  
  # separate out counts from team names
  beats <- beats.df[,-c(1,2,ncol(beats.df))]
  played <- played.df[,-c(1,2,ncol(played.df))]
  match.teams <- beats.df[,1:2]
  
  # create X matrix which index all pairs of teams (both home and away) and fill with 1 for team i, -1 for team j
  pairs <- matrix(0, 2*choose(K,2), K)

  entries_ind <- t(combn(K, 2))
  rev_entries_ind <- entries_ind[,c(2,1)]
  all_entries_ind <- rbind(entries_ind, rev_entries_ind)
  
  for (l in 1:nrow(all_entries)) {
    pairs[l,all_entries_ind[l,1]] <- 1
    pairs[l,all_entries_ind[l,2]] <- -1
  }
  
  # matrix with 1 for team i, -1 for team j, from total of K teams, and row of zeros where those teams don't play
  # returns a list with one of these matrices for each year
  # i.e. pairs matrices, with zero is that pair didn't play
  # initialise list
  X <- list()
  
  # create logical matrix (1 if played, 0 if not - regardless of number of times played)
  played.mat <- as.matrix(played)
  played.mat[as.logical(played.mat)] <- 1
  
  # for each timepoint, append to the list the pairs matrix with rows of zeros for any pairs that have not played
  # add a column of 1s at the end (if that pair played), to pick up the home advantage coefficient
  for (i in 1:TT) {
    temp_mat <- pairs * played.mat[,i]
    temp_vec <- as.numeric(as.logical(played.mat[,i]))
    X[[length(X)+1]] <- cbind(temp_mat, temp_vec)
  }

  # return
  return(list(beats = beats, played = played, match.teams = match.teams, X=X))
}

