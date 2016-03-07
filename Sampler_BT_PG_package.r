library(BayesLogit)
library(MASS)
library(dlm)

  Recursion = function (w, x_matrix, h_matrix_non_diag, t_t, r_t, team_pair_length, time, team_length){
    y_plus = matrix(0, team_pair_length, time)
    beta_plus = matrix(0, team_length, time)
    beta_plus[,1] = rnorm(team_length)
    y_plus[,1] = x_matrix[[1]] %*% beta_plus[,1] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,1]))
    for (t in 2 : time){
      #r_t scalar here
      beta_plus[, t] = t_t %*% (beta_plus[,t-1]) + r_t * as.vector(rnorm(team_length))
      y_plus[,t] = x_matrix[[t]] %*% beta_plus[,t] + mvrnorm(1,rep(0,length(h_matrix_non_diag[,1])),diag(h_matrix_non_diag[,t]))
    }
    Recursion = list(y = y_plus, beta = beta_plus)
  }

PGSmootherPost_package = function(samples_size, iterations, thinning, burn_in, win_vector_matrix, total_vector_matrix, pho, sigma, index_matrix_list){
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
    y_star = (z_matrix - y_plus)
    comp_list = list()
    comp_list$m0 = rep(0,team_length)
    comp_list$C0 = diag(team_length)
    comp_list$GG = pho* diag(team_length)
    comp_list$W = sigma*sqrt(1-pho^2)*diag(team_length)
    comp_list$V = diag(rep(1,team_pair_length))
    comp_list$JV = diag(1:team_pair_length)
    comp_list$FF = matrix(0,team_pair_length, team_length)
    comp_list$JFF = t(matrix(((team_pair_length+1) : (team_pair_length+team_length*team_pair_length)), team_length, team_pair_length))
    X = matrix(0, time, team_pair_length+team_length*team_pair_length)
    for (t in 1 : time){
      X[t, 1:team_pair_length] =  h_t_non_diag[, t]
      X[t, ((team_pair_length+1): (team_pair_length+team_length*team_pair_length))] = as.vector(t(index_matrix_list[[t]]))
    }
    comp_list$X = X
    dlm_model = dlm(comp_list)
    FilteredDLM = dlmFilter(y_star, dlm_model)
    beta_samples_t = dlmSmooth.dlmFiltered(FilteredDLM)$s[(2: (time+1)),]
    beta_samples[[s]] = t(beta_samples_t) + beta_plus
    setTxtProgressBar(pb, s)
  }
  close(pb)
  end.time = Sys.time()
  print("End time = ")
  print(end.time)
  PGSmootherPost = beta_samples 
}