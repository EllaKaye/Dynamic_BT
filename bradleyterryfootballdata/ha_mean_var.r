
premier.top.ha <- home.away(results, 9, top.teams)
premier.top.ha.beats <- premier.top.ha$beats
premier.ha.played <- premier.top.ha$played
premier.ha.X <- premier.top.ha$X
premier.ha.match.teams <- premier.top.ha$match.teams

premier_sample0.7sigma0.7_ha = PGSmootherPostHA(5, 10000, 0, 0, premier.top.ha.beats, premier.ha.played, 0.7, 0.7, premier.ha.X, 0.1)
means_over_time0.7sigma0.7_ha = matrix(0,8, 10)
premier_sample0.7sigma0.7_thin_ha = premier_sample0.7sigma0.7_ha[seq(1, 10000, 2)]
for (j in 1 : 10){
  GGGG = matrix(0, 4900,8)
  for (i in 101: 5000) GGGG[i-100,] = premier_sample0.7sigma0.7_ha[[i]][, j] 
  means_over_time0.7sigma0.7_ha[,j] = colMeans(GGGG)
}
Samples = premier_sample0.7sigma0.7_thin_ha
beta_var_single = 1:4900
beta_var_time0.7sigma0.7_ha= matrix(0, 8, 10)
for (l in 1:10){
  for (j in 1:8){
    for (i in 101:5000) {
      beta_var_single[i-100] = Samples[[i]][j,l]
    }
    beta_var_time0.7sigma0.7_ha[j,l] = var(beta_var_single)
  }
  beta_var_single = 1:4900
}
