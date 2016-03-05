library(BayesLogit)
library(MASS)

Recursion = function (w, x_matrix, h_matrix_non_diag, t_t, r_t, team_pair_length, time, team_length){
  y_plus = matrix(0, team_pair_length, time)
  beta_plus = matrix(0, team_length, time)
  beta_time[,1] = rnorm(team_pair_length)
  yplus[,1] = x_matrix[[1]] %*% beta_time[,1] + mvrnorm(1,0,diag(h_matrix_non_diag[,1]))
  for (t in 2 : time){
    #r_t scalar here
    beta_time[,t] = t_t %*% as.matrix(beta_time) + r_t * rnorm(team_length)
    yplus[,t] = x_matrix[[t]] %*% beta_time[,t] + mvrnorm(1,0,diag(h_matrix_non_diag[,t]))
  }
  Recursion = list(y = yplus, beta = beta_plus)
}

Backward_recursion = function(y_vec, index_matrix_list, team_pair_length, time, team_length, F_t, K_t, L_t, P_t, r_t, t_t, h_t_non_diag){
  A_matrix = matrix(0, team_length, time)
  V_matrix = matrix(0, team_pair_length, time)
  A_matrix[,1] = rep(0, team_length)
  V_matrix[,1] = y_vec[, 1] - index_matrix_list[[1]] %*% A_matrix[, 1]
  for (t in 2 : time){
    A_matrix[, t] = t_t %*% A_matrix[, t-1] + K_t[ , , t] %*% V_matrix[, t-1]
    V_matrix[, t] = y_vec[, t] - index_matrix_list[[t]] %*% A_matrix[, t]
  }
  
}

PGSmootherPost = function(samples_size, iterations, thinning, burn_in, win_vector_matrix, total_vector_matrix, pho, sigma, index_matrix_list){
  beta_samples = lapply(beta_init_list <-vector(mode = 'list', iterations), function(x) x <- list())
  time = dim(win_vector_matrix)[2]
  team_pair_length = dim(win_vector_matrix)[1]
  team_length = dim(index_matrix_list[[1]])[2]
  beta_init_list <- lapply(beta_init_list <-vector(mode = 'list', time), function(x) x <- 1 : team_length)
  beta_init_list[[1]] <- rnorm(team_length)
  for (i in 2 : time){
    beta_init_list[[i]] = pho * beta_init_list[[i-1]] + sigma* sqrt(1-pho^2)*rnorm(team_length)
  }
  beta_samples[[1]] = beta_init_list
  poly_gamma_var = matrix(0, team_pair_length, time)
  for (s in 1 : iterations){
    
    team_play_matrix = matrix(0, team_pair_length, time)
    for (j in 1 : time){
      team_play_matrix[, j] = index_matrix_list[[j]]%*%beta_samples[[s]]
    }
    # Can change to apply
    for (t in 1 : time){
      for (i in 1 : team_pair_length){
        n_ij_total = total_vector_matrix[i, t]
        if (n_ij_total = 0){
          poly_gamma_var[i, t] = 1
        }
        else{  poly_gamma_var[i, t] = rpg(1, n_ij_total, team_play_matrix[i, t]) }
      }
    }
    z_matrix = (win_vector_matrix - 0.5 * total_vector_matrix) / poly_gamma_var 
    h_t_non_diag = 1/ poly_gamma_var
    t_t = pho * diag(team_length)
    r_t = sigma * sqrt(1-pho^2)
    
    #Draw from p(noise for all n)
    w_plus = rep(0,team_pair_length * time + team_length * time)
    index = 1
    for (t in 1 : time){
      new_index = index + team_pair_length
      w_plus[index: (new_index - 1)] = sapply(h_t_non_diag[,t], function(x) mvrnorm(1, 0, diag(x)))
      wplus[new_index : new_index + team_length -1 ] = rnorm(team_length)
      index = index + team_pair_length + team_length
    }
    
    y_plus = Recursion(w_plus, index_matrix_list, h_t_non_diag, t_t, r_t, team_pair_length, time, team_length)
    P_matrix = array(0,dim = c(team_length, team_length, time))
    F_matrix = array(0, dim = c(team_pair_length, team_pair_length, time))
    K_matrix = array(0, dim = c(team_length, team_pair_length, time))
    L_matrix = array(0, dim = c(team_length, team_length, time))
    P_matrix[ , , 1] = diag(c(team_length))
    
    for (t in 1 : time-1){
      F_matrix[ , , t] = index_matrix_list[[t]] %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) + diag(h_t_non_diag[, t])
      K_matrix[ , , t] = t_t %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) %*% ginv(F_matrix[ , , t])
      L_matrix[ , , t] = t_t - K_matrix[ , , t] %*% index_matrix_list[[t]]
      P_matrix[ , , t+1] = t_t%*%P_matrix[, , t] %*% t(L_matrix[ , , t]) + r_t %*% diag(team_length) %*% t(r_t)
    }
    F_matrix[ , , time] = index_matrix_list[[time]] %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) + diag(h_t_non_diag[, time])
    K_matrix[ , , time] = t_t %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) %*% ginv(F_matrix[ , , time])
    L_matrix[ , , time] = t_t - K_matrix[ , , time] %*% index_matrix_list[[time]]
    
    
  }
}