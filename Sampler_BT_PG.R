library(BayesLogit)
library(MASS)

Recursion = function (w, x_matrix, h_matrix_non_diag, t_t, r_t, team_pair_length, time, team_length){
  y_plus = matrix(0, team_pair_length, time)
  beta_plus = matrix(0, team_length, time)
  beta_plus[,1] = rnorm(team_length)
  y_plus[,1] = x_matrix[[1]] %*% beta_plus[,1] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,1]))
  for (t in 2 : time){
    #r_t scalar here
    beta_plus[, t] = t_t %*% (beta_plus[, t-1]) + r_t * as.vector(rnorm(team_length))
    y_plus[,t] = x_matrix[[t]] %*% beta_plus[,t] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,t]))
  }
  Recursion = list(y = y_plus, beta = beta_plus)
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
  R_matrix = matrix(0, team_length, time + 1)
  R_matrix[ , time +1] = rep(0, team_length)
  for (t in time : 1){
    R_matrix[,t] = t(index_matrix_list[[t]]) %*% ginv(F_t[ , , t]) %*% as.matrix(V_matrix[, t]) + t(L_t[ , ,t]) %*% R_matrix[, t+1]
    #Error In paper???? Need transpose?
  }
  Backward_recursion = R_matrix
}

PGSmootherPost = function(samples_size, iterations, thinning, burn_in, win_vector_matrix, total_vector_matrix, pho, sigma, index_matrix_list){
  start.time = Sys.time()
  print("Start time = ")
  print(start.time)
  beta_samples = lapply(beta_init_list <-vector(mode = 'list', iterations), function(x) x <- list())
  time = dim(win_vector_matrix)[2]
  team_pair_length = dim(win_vector_matrix)[1]
  team_length = dim(index_matrix_list[[1]])[2]
  beta_init_list <- matrix(0, team_length, time)
  beta_init_list[,1] <- rnorm(team_length)
  for (i in 2 : time){
    beta_init_list[, i] = pho * beta_init_list[,i-1] + sigma* sqrt(1-pho^2)*rnorm(team_length)
  }
  beta_samples[[1]] = beta_init_list
  poly_gamma_var = matrix(0, team_pair_length, time)
  pb <- txtProgressBar(min = 0, max = iterations - 1, style = 3)
  for (s in 2 : iterations){
    
    team_play_matrix = matrix(0, team_pair_length, time)
    for (j in 1 : time){
      team_play_matrix[, j] = index_matrix_list[[j]] %*% beta_samples[[s-1]][ ,j]
    }
    # Can change to apply
    for (t in 1 : time){
      for (i in 1 : team_pair_length){
        n_ij_total = total_vector_matrix[i, t]
        if (n_ij_total == 0){
          poly_gamma_var[i, t] = 1
        }
        else{ poly_gamma_var[i, t] = rpg(1, n_ij_total, team_play_matrix[i, t]) }
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
      w_plus[index: (new_index - 1)] = mvrnorm(1, rep(0,length(h_t_non_diag[,t])), diag(h_t_non_diag[,t]))
      w_plus[new_index : (new_index + team_length -1) ] = rnorm(team_length)
      index = index + team_length + team_pair_length
    }
    
    Value = Recursion(w_plus, index_matrix_list, h_t_non_diag, t_t, r_t, team_pair_length, time, team_length)
    y_plus = Value$y
    beta_plus = Value$beta
    P_matrix = array(0,dim = c(team_length, team_length, time))
    F_matrix = array(0, dim = c(team_pair_length, team_pair_length, time))
    K_matrix = array(0, dim = c(team_length, team_pair_length, time))
    L_matrix = array(0, dim = c(team_length, team_length, time))
    P_matrix[ , , 1] = diag(c(team_length))
    for (t in 1 : (time - 1)){
      F_matrix[ , , t] = index_matrix_list[[t]] %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) + diag(h_t_non_diag[, t])
      K_matrix[ , , t] = t_t %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) %*% ginv(F_matrix[ , , t])
      L_matrix[ , , t] = t_t - K_matrix[ , , t] %*% index_matrix_list[[t]]
      P_matrix[ , , t+1] = t_t%*%P_matrix[, , t] %*% t(L_matrix[ , , t]) + r_t * diag(team_length) * r_t
    }
    F_matrix[ , , time] = index_matrix_list[[time]] %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) + diag(h_t_non_diag[, time])
    K_matrix[ , , time] = t_t %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) %*% ginv(F_matrix[ , , time])
    L_matrix[ , , time] = t_t - K_matrix[ , , time] %*% index_matrix_list[[time]]
    
    R_matrix = Backward_recursion((z_matrix - y_plus), index_matrix_list, team_pair_length, time, team_length, F_matrix, K_matrix, L_matrix, P_matrix, r_t, t_t, h_t_non_diag)
    beta_samples_t = matrix(0, team_length, time)
    beta_samples_t[,1] = R_matrix[,1]
    for (t in 2 : time){
      beta_samples_t[,t] = t_t %*% beta_samples_t[,t-1] + r_t *(r_t)*R_matrix[,t-1]
    }
    beta_samples[[s]] = beta_samples_t + beta_plus
    setTxtProgressBar(pb, s)
  }
  close(pb)
  end.time = Sys.time()
  print("End time = ")
  print(end.time)
  PGSmootherPost = beta_samples 
}