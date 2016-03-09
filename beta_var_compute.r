#ACF analysis:
#Burn In Analsysi:
acfvector = 1:10000
for (i in 1:10000){
  acfvector[i] = as.vector(premier_sample0.9sigma1[[i]][1,1])
}
acf(acfvector)
plot(acfvector[1:200])
#0.9 0.7 0.5 sigma = 0.6 premier
#0.7 0.7 on bigger team set
--------------- #0.9 pho sigma 1 c(131, 136, 138, 139, 142, 143, 147)
premier_sample0.9sigma0.7 = PGSmootherPost(5, 10000, 0, 0, UK.beats.mat.pt, UK.played.mat.pt, 0.9, 0.7, UK.pairs.mats.pt)
means_over_time0.9sigma0.7 = matrix(0,7, 10)
premier_sample0.9sigma0.7_thin = premier_sample0.9sigma0.7[seq(1, 10000, 2)]
for (j in 1 : 10){
  GGGG = matrix(0, 4900,7)
  for (i in 101: 5000) GGGG[i-100,] = premier_sample0.9sigma0.7_thin[[i]][, j] 
  means_over_time0.9sigma0.7[,j] = colMeans(GGGG)
}
Samples = premier_sample0.9sigma0.7_thin
beta_var_single = 1:4900
beta_var_time0.9sigma0.7 = matrix(0, 7, 10)
for (l in 1:10){
  for (j in 1:7){
    for (i in 101:5000) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time0.9sigma0.7[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:4900
}

--------------- #0.7 pho sigma 1 c(131, 136, 138, 139, 142, 143, 147)
premier_sample0.7sigma0.7 = PGSmootherPost(5, 10000, 0, 0, UK.beats.mat.pt, UK.played.mat.pt, 0.7, 0.7, UK.pairs.mats.pt)
means_over_time0.7sigma0.7 = matrix(0,7, 10)
premier_sample0.7sigma0.7_thin = premier_sample0.7sigma0.7[seq(1, 10000, 2)]
for (j in 1 : 10){
  GGGG = matrix(0, 4900,7)
  for (i in 101: 5000) GGGG[i-100,] = premier_sample0.7sigma0.7_thin[[i]][, j] 
  means_over_time0.7sigma0.7[,j] = colMeans(GGGG)
}
Samples = premier_sample0.7sigma0.7_thin
beta_var_single = 1:4900
beta_var_time0.7sigma0.7 = matrix(0, 7, 10)
for (l in 1:10){
  for (j in 1:7){
    for (i in 101:5000) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time0.7sigma0.7[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:4900
}
--------------- #0.5 pho sigma 1 c(131, 136, 138, 139, 142, 143, 147)
  premier_sample0.5sigma0.7 = PGSmootherPost(5, 10000, 0, 0, UK.beats.mat.pt, UK.played.mat.pt, 0.5, 0.7, UK.pairs.mats.pt)
means_over_time0.5sigma0.7 = matrix(0,7, 10)
premier_sample0.5sigma0.7_thin = premier_sample0.5sigma0.7[seq(1, 10000, 2)]
for (j in 1 : 10){
  GGGG = matrix(0, 4900,7)
  for (i in 101: 5000) GGGG[i-100,] = premier_sample0.5sigma0.7_thin[[i]][, j] 
  means_over_time0.5sigma0.7[,j] = colMeans(GGGG)
}
Samples = premier_sample0.5sigma0.7_thin
beta_var_single = 1:4900
beta_var_time0.5sigma0.7 = matrix(0, 7, 10)
for (l in 1:10){
  for (j in 1:7){
    for (i in 101:5000) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time0.5sigma0.7[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:4900
}

# 130 131 132 136 138 139 141 142 143 146 147 148 196 195 reorder
#More teams
top.teams <- c(130, 131, 132, 136, 138, 139, 141, 142, 143, 146, 147, 148, 195, 196)
premier_sample0.7sigma0.7_additional = PGSmootherPost(5, 10000, 0, 0, UK.beats.mat.pt, UK.played.mat.pt, 0.7, 0.7, UK.pairs.mats.pt)
means_over_time0.7sigma0.7_additional = matrix(0,14, 10)
premier_sample0.7sigma0.7_thin_additional = premier_sample0.7sigma0.7[seq(1, 10000, 2)]
for (j in 1 : 10){
  GGGG = matrix(0, 4900,7)
  for (i in 101: 5000) GGGG[i-100,] = premier_sample0.7sigma0.7_thin_additional[[i]][, j] 
  means_over_time0.7sigma0.7_additional[,j] = colMeans(GGGG)
}
Samples = premier_sample0.7sigma0.7_thin_additional
beta_var_single = 1:4900
beta_var_time0.7sigma0.7_additional = matrix(0, 7, 10)
for (l in 1:10){
  for (j in 1:14){
    for (i in 101:5000) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time0.7sigma0.7_additional[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:4900
}
