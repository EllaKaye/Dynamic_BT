library(BayesLogit)
library(MASS)

Recursion.tie = function (w, x_matrix, h_matrix_non_diag, t_t, r_t, team_pair_length_double, time, team_length, gamma){
  y_plus = matrix(0, team_pair_length_double, time)
  beta_plus = matrix(0, team_length, time)
  beta_plus[,1] = rnorm(team_length)
  # We do not have parameter for the last part, since we are not time varying it
  # Problem I think this should be gamma but then it will give you 2*gamma output, need to think about incorporation differently!!!
  beta_plus[team_length,1] = 0
  y_plus[,1] = x_matrix[[1]] %*% beta_plus[,1] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,1]))
  for (t in 2 : time){
    # r_t scalar here
    # The last parameter should not vary, hence dummy_vec
    dummy_vec = as.vector(rnorm(team_length))
    dummy_vec[team_length] = 0
    beta_plus[, t] = t_t %*% (beta_plus[,t-1]) + r_t * dummy_vec
    y_plus[,t] = x_matrix[[t]] %*% beta_plus[,t] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,t]))
  }
  Recursion = list(y = y_plus, beta = beta_plus)
}

Backward_recursion.tie = function(y_vec, index_matrix_list, team_pair_length_double, time, team_length, F_t, K_t, L_t, P_t, r_t, t_t, h_t_non_diag){
  A_matrix = matrix(0, team_length, time)
  V_matrix = matrix(0, team_pair_length_double, time)
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
    #Error In paper as said earlier, tranpose
  }
  Backward_recursion = R_matrix
}

PGSmootherPost_tie = function(iterations, win_vector_matrix, tie_matrix, total_matrix, pho, sigma, index_matrix_list, gamma){
  # Function for Rao-Kupper Ties Bradley Terry Dynamic Model 
  # Win_vector_matrix: matrix: number of wins (comparison by time) 
  # tie_matrix: matrix: number of ties (comparison by time) 
  # total_matrix: matrix: number of totals (comparison by time)
  # Gamma parameter is fixed over time 
  # Index_matrix list is now list index by time points and matrix is now double the team comparison, so the rows should be of form ( ... 1 ... -1 ... 1) (the last one for gamma) and there should
  # be a row that is the same with 1 and -1 switched, if people dont play just set the row to 0. 
  start.time = Sys.time()
  print("Start time = ")
  print(start.time)
  beta_samples = lapply(beta_init_list <-vector(mode = 'list', iterations), function(x) x <- list())
  time = dim(win_vector_matrix)[2]
  team_pair_length = dim(win_vector_matrix)[1]
  team_pair_length_double = dim(index_matrix_list[[1]])[1]
  team_length = dim(index_matrix_list[[1]])[2]
  # Actually this is one more than K number of team from gamma parameter
  beta_init_list <- matrix(0, team_length, time)
  beta_init_list[,1] <- rnorm(team_length)
  for (i in 2 : time){
    beta_init_list[, i] = pho * beta_init_list[,i-1] + sigma* sqrt(1-pho^2)*rnorm(team_length)
  }
  beta_init_list[team_length,1:time] = gamma
  beta_samples[[1]] = beta_init_list
  # We now have two set of Polya Gammas, introduce by product of binomial likelihood
  poly_gamma_var_1 = matrix(0, team_pair_length, time)
  poly_gamma_var_2 = matrix(0, team_pair_length, time)
  pb <- txtProgressBar(min = 0, max = iterations - 1, style = 3)
  for (s in 2 : iterations){
    team_play_matrix = matrix(0, team_pair_length_double, time)
    for (j in 1 : time){
      team_play_matrix[, j] = index_matrix_list[[j]] %*% beta_samples[[s-1]][ ,j]
    }
    # Can change to apply
    for (t in 1 : time){
      for (i in 1 : team_pair_length){
        s_ij_total = win_vector_matrix[i, t]
        t_ij_total = tie_matrix[i,t]
        s_ji_total_rev = total_matrix[i,t] - s_ij_total - t_ij_total 
        if (s_ij_total + t_ij_total == 0){
          # I have set to 1 here, but it should not matter since the X_t row should be 0 and should not affect posterior, but should check
          poly_gamma_var_1[i, t] = 1
        }
        else{ 
          # Polya_gamma should not have 2 here! Check algebra below this and change accordingly
          poly_gamma_var_1[i, t] = rpg(1, s_ij_total + t_ij_total, team_play_matrix[i, t]/2)
        }
        if (s_ji_total_rev + t_ij_total == 0){
          # I have set to 1 here, but it should not matter since the X_t row should be 0 and should not affect posterior, but should check
          poly_gamma_var_2[i,t] = 1
        }
        else{
          # Polya_gamma should not have 2 here! Check algebra below this and change accordingly
          poly_gamma_var_2[i, t] = rpg(1, s_ji_total_rev + t_ij_total, team_play_matrix[(team_pair_length + i), t]/2)
        }
      }
    }  
    z_matrix_1 = win_vector_matrix + tie_matrix / poly_gamma_var_1
    z_matrix_2 = (total_matrix - win_vector_matrix + tie_matrix) / poly_gamma_var_2
    # Check algebra here and we combine into a big matrix over time
    z_matrix = rbind(z_matrix_1, z_matrix_2)
    h_t_non_diag_1 = 1/ (4*poly_gamma_var_1)
    h_t_non_diag_2 = 1/ (4*poly_gamma_var_2)
    h_t_non_diag = rbind(h_t_non_diag_1, h_t_non_diag_2)
    # We combine the matrix 
    t_t = pho * diag(team_length)
    t_t[team_length, team_length] = 1
    # Represents gamma parameter being fixed
    r_t = sigma * sqrt(1-pho^2)
    # Will adjust this to 0 for gamme parameter later
    
    #Draw from p(noise for all n)
    w_plus = rep(0,team_pair_length_double * time + team_length * time)
    index = 1
    # Same as before actually
    for (t in 1 : time){
      new_index = index + team_pair_length_double
      w_plus[index: (new_index - 1)] = mvrnorm(1, rep(0,length(h_t_non_diag[,t])), diag(h_t_non_diag[,t]))
      w_plus[new_index : (new_index + team_length -1) ] = rnorm(team_length)
      index = index + team_length + team_pair_length_double
    }
    Value = Recursion.tie(w_plus, index_matrix_list, h_t_non_diag, t_t, r_t, team_pair_length_double, time, team_length, gamma)
    y_plus = Value$y
    beta_plus = Value$beta
    P_matrix = array(0,dim = c(team_length, team_length, time))
    F_matrix = array(0, dim = c(team_pair_length_double, team_pair_length_double, time))
    K_matrix = array(0, dim = c(team_length, team_pair_length_double, time))
    L_matrix = array(0, dim = c(team_length, team_length, time))
    P_matrix[ , , 1] = diag(c(team_length))
    P_matrix[team_length, team_length, 1] = 0
    for (t in 1 : (time - 1)){
      F_matrix[ , , t] = index_matrix_list[[t]] %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) + diag(h_t_non_diag[, t])
      K_matrix[ , , t] = t_t %*% P_matrix[ , , t] %*% t(index_matrix_list[[t]]) %*% ginv(F_matrix[ , , t])
      L_matrix[ , , t] = t_t - K_matrix[ , , t] %*% index_matrix_list[[t]]
      dummy_2 = diag(team_length)
      # Gamma parameter not varying with time
      dummy_2[team_length, team_length] = 0
      P_matrix[ , , t+1] = t_t%*%P_matrix[, , t] %*% t(L_matrix[ , , t]) + r_t * dummy_2 * r_t 
    }
    F_matrix[ , , time] = index_matrix_list[[time]] %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) + diag(h_t_non_diag[, time])
    K_matrix[ , , time] = t_t %*% P_matrix[ , , time] %*% t(index_matrix_list[[time]]) %*% ginv(F_matrix[ , , time])
    L_matrix[ , , time] = t_t - K_matrix[ , , time] %*% index_matrix_list[[time]]
    
    R_matrix = Backward_recursion.tie((z_matrix - y_plus), index_matrix_list, team_pair_length_double, time, team_length, F_matrix, K_matrix, L_matrix, P_matrix, r_t, t_t, h_t_non_diag)
    beta_samples_t = matrix(0, team_length, time)
    start_value = rep(0,team_length)
    start_value[team_length] = gamma
    # Since the beta value gamma should not vary and only come in at polya gamma part, but check carefully
    dummy_3 = R_matrix[,1]
    dummy_3[team_length] = 0
    beta_samples_t[,1] = start_value + dummy_3
    # Should not be time varying care here, but check to make sure:
    for (t in 2 : time){
      dummy = r_t*(r_t)*R_matrix[,t-1]
      dummy[team_length] = 0
      beta_samples_t[,t] = t_t %*% beta_samples_t[,t-1] + dummy
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