
Sample_thin_burn = Samples[101:500]
Sample_thin = list()
Sample_thin<- Sample_thin_burn[seq(1, length(Sample_thin_burn), 5)]
GGGG = matrix(0, 80,37)
for (i in 1: 80) GGGG[i,] = Sample_thin[[i]][,11] 
colMeans(GGGG)
beta_var_single = 1:400
beta_var_time = matrix(0, 37, 11)


Sample_thin = list()
means_over_time = matrix(0,37, 11)
for (j in 1 : 11){
  GGGG = matrix(0, 400,37)
  for (i in 101: 500) GGGG[i-100,] = Samples[[i]][, j] 
  means_over_time[,j] = colMeans(GGGG)
}

for (l in 1:11){
  for (j in 1:37){
    for (i in 101:500) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:400
}