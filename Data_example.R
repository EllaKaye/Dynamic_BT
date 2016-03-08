# provides use-case for main functions in Data_functions.R

# load the functions
source("Data_functions.R")

# load the data
results <- read.csv("footballresults.csv", header=TRUE)
# leagues <- read.csv("footballleagues.csv", header=TRUE)
# teams <- read.csv("footballteams.csv", header=TRUE)

# Example - UK Premier League
# Step 1: stepping stone to required matrices
## generate list a matrices, one for each year, where each matrix gives number of times i beats j in that years
UK.beats.list <- beats.all.t.list(results, 9)

# Step 2: info about teams and years generated from same arguments
UK.years.teams <- years.teams(results, 9)

# Step 3: generate matrix of number of times i beats j for each year
## rows give every possible pairing of teams (i < j) and columns give the years
## takes output of Step 1 as input
UK.beats.mat <- beats.all.t.list.to.mat(UK.beats.list)

# Step 4: generate matrix of number of times i plays j for each year
## rows give every possible pairing of teams (i < j) and columns give the years
## takes output of Step 1 as input
UK.played.mat <- played.all.t.list.to.mat(UK.beats.list)

# Step 5: create list (one element for each year), of matrices indictating which two teams are represented
# in each row of the beats and played matrices.
# dimension (K choose 2) by K, where K is the number of teams
# each row has a 1 in the column for team i, -1 in the column for team j, 0 otherwise
# if two teams did not play each other that year, the row for that pair is just zeros.
# takes output of Step 4 and Step 2 as input
UK.pairs.mats <- pairs.mat.zero(UK.played.mat, K = UK.years.teams$num.teams)

# assign names to the list (if desired)
names(UK.pairs.mats) <- UK.years.teams$years

# UK teams
UK.teams <- UK.years.teams$teams
UK.teams.df <- subset(teams, No.Team %in% UK.teams)
rownames(UK.teams.df) <- NULL
UK.teams.df
write(UK.teams.df, file = "Premier teams")

## Using the function for home/away
UK.ha <- home.away(results, 9)

# matrix with one row for each pair (i,j) (nrow = 2 * (K choose 2)) - with i playing at home - and one column for each year
# entries counts number of times i beats j in year t
UK.ha.beats <- UK.ha$beats

# matrix with one row for each pair (i,j) (nrow = 2 * (K choose 2)) - with i playing at home - and one column for each year
# entries counts number of times i played j in year t
UK.ha.played <- UK.ha$played

# matrix of dim 2 * (K choose 2) by 2, giving the team numbers of the home and away teams for each row of the beats and played matrices
UK.ha.match.teams <- UK.ha$match.teams

# X is a list (one element for each year), of matrices indictating which two teams are represented 
# in each row of the beats and played matrices.
# each matrix is of dimension 2 * (K choose 2) by (K + 1), where K is the number of teams
# the first K columns represent the K teams
# each row has a 1 in the column for team i, -1 in the column for team j, 0 otherwise (for columns 1:K)
# the (K+1)th column is a column of 1s (if pair played - 0 otherwise), to pick of the coefficient for the home advantage
# if two teams did not play each other that year, the row for that pair is just zeros
UK.ha.X <- UK.ha$X
