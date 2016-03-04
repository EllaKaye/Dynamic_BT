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

# matrix to number of times i beats j, with cols as year
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
played.all.t.mat <- function(beats.list) {
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
pairs.mat.zero <- function(played.mat, num.teams) {
  # number of years in the data
  TT <- ncol(played.mat)
  
  pairs <- pairs.mat(num.teams)
  
  # initialise list
  out <- list()
  
  # for each timepoint, append to the list the pairs matrix with rows of zeros for any pairs that have not played
  for (i in 1:TT) {
    out[[length(out)+1]] <- pairs * played.mat[,i]
  }
  out
}

