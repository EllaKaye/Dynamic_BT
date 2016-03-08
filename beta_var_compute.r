
Sample_thin_burn = Samples[101:500]
Sample_thin = list()
Sample_thin<- Sample_thin_burn[seq(1, length(Sample_thin_burn), 5)]
GGGG = matrix(0, 80,37)
for (i in 1: 80) GGGG[i,] = Sample_thin[[i]][,11] 
colMeans(GGGG)
beta_var_single = 1:400
beta_var_time = matrix(0, 37, 11)


Sample_thin = list()
means_over_time = matrix(0,7, 10)
premier_sample0.7sigma1 = premier_sample0.7sigma1[seq(1, 10000, 5)]
for (j in 1 : 10){
  GGGG = matrix(0, 1900,7)
  for (i in 101: 2000) GGGG[i-100,] = premier_sample0.7sigma1[[i]][, j] 
  means_over_time[,j] = colMeans(GGGG)
}
# 130 131 132 136 138 139 141 142 143 146 147 148 196 195 reorder 


for (l in 1:11){
  for (j in 1:37){
    for (i in 101:500) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:400
}