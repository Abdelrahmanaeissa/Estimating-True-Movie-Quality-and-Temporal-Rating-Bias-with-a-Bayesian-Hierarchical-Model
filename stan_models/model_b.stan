
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N; 
  int<lower=1> N_users; 
  int<lower=1> N_movies;
  int<lower=1> N_genres;
  int<lower=1> K;
  int<lower=1, upper=N_users> user_id[N];
  int<lower=1, upper=N_movies> movie_id[N];
  int<lower=1, upper=N_genres> genre_id[N];
  matrix[N, K] B; 
  vector[N] rating;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta_0;
  vector[N_movies] z_alpha; 
  vector[N_users] z_delta; 
  vector[K] gamma;
  real<lower=0> sigma_alpha;
  matrix<lower=-1.5, upper=1.5>[N_users, N_genres] theta; 
  real<lower=0> sigma_delta; 
  real<lower=0> sigma_y;
}
transformed parameters {
  // Reconstruct true parameters smoothly
  vector[N_movies] alpha = z_alpha * sigma_alpha;
  vector[N_users] delta = z_delta * sigma_delta;
}


model {
  beta_0 ~ normal(3.5, 1.0); 
  gamma ~ normal(0, 1.0);
  sigma_alpha ~ exponential(1.0); 
  sigma_delta ~ exponential(1.0); 
  sigma_y ~ exponential(1.0);
  
  z_alpha ~ normal(0, 1); 
  z_delta ~ normal(0, 1);
  
  vector[N] theta_obs;
  for (n in 1:N)
  theta_obs[n] = theta[user_id[n], genre_id[n]];
  
  vector[N] mu;
  mu = beta_0 + alpha[movie_id] + delta[user_id] + B * gamma + theta_obs;
  rating ~ normal(mu, sigma_y);
}






