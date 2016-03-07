
beta_var_single = 1:400
beta_var_time = matrix(0, 37, 11)

for (l in 1:11){
 for (j in 1:37){
  for (i in 101:500) {
  beta_var_single[i-100] = Samples[[i]][j,l]
  }
 beta_var_time[j,l] = var(beta_var_single)
 }
beta_var_single = 1:400
}