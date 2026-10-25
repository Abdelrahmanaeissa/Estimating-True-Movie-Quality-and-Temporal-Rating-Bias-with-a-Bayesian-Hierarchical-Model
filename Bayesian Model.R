library(tidyverse)
library(readxl)
library(lubridate)

#load the files
movies <- read_csv("movies.xlsx")
ratings <- read_csv("ratings.csv")
links <- read_csv("links.csv")
tags <- read_csv("tags.csv")

#Merge movies and ratings datasets

movies_ratings_draft <- ratings %>%
  left_join(movies, by = "movieId")

#clean movie ratings table

movies_ratings <- movies_ratings_draft %>%
  mutate(
    rating_year = lubridate::year(as_datetime(timestamp)),
    movie_year_numeric = as.numeric(Year) 
  ) %>%
  filter(!is.na(movie_year_numeric)) %>%
  mutate(
    movie_age = rating_year - movie_year_numeric
  ) %>%
  filter(movie_age >= 0)

## Descriptive Analysis for presenation
#1) Nostalgia Plot 
library(ggplot2)
library(dplyr)

# 1. Create Age Buckets
movies_ratings_simple <- movies_ratings %>%
  mutate(age_group = cut(movie_age, 
                         breaks = seq(0, 100, by = 10), 
                         labels = c("0-10", "11-20", "21-30", "31-40", "41-50", 
                                    "51-60", "61-70", "71-80", "81-90", "91-100"),
                         include.lowest = TRUE)) %>%
  group_by(age_group) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    se = sd(rating, na.rm = TRUE) / sqrt(n()) 
  ) %>%
  filter(!is.na(age_group))

# 2. Plot the Simple Trend
ggplot(movies_ratings_simple, aes(x = age_group, y = avg_rating, group = 1)) +
  geom_line(color = "#2563eb", size = 1) +
  geom_point(color = "#2563eb", size = 3) +
  geom_errorbar(aes(ymin = avg_rating - se, ymax = avg_rating + se), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average Rating by Movie Age",
    subtitle = "Evidence of Nostalgia Bias: Older movies carry higher average ratings",
    x = "Age of Movie (Decades)",
    y = "Average Rating"
  ) +
  coord_cartesian(ylim = c(3.0, 4.2))

# User Harshness
user_analysis <- movies_ratings %>%
  group_by(userId) %>%
  summarise(
    avg_user_rating = mean(rating),
    n_ratings = n()
  )

# 2. Plot the Distribution of User Averages
ggplot(user_analysis, aes(x = avg_user_rating)) +
  geom_histogram(fill = "#2563eb", color = "white", bins = 25) +
  geom_vline(aes(xintercept = mean(avg_user_rating)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "The Distribution of User 'Harshness'",
    subtitle = "Evidence that users have different internal baselines",
    x = "Average Rating Given by a User",
    y = "Number of Users"
  ) +
  annotate("text", x = 2.5, y = 50, label = "The 'Grumps'", color = "red") +
  annotate("text", x = 4.5, y = 50, label = "The 'Optimists'", color = "red")

# 3) Law of Small Numbers
movie_reliability <- movies_ratings %>%
  group_by(title) %>%
  summarise(
    n_ratings = n(),
    avg_rating = mean(rating)
  )

# 2. Create the Funnel Plot
ggplot(movie_reliability, aes(x = n_ratings, y = avg_rating)) +
  geom_point(alpha = 0.2, color = "#2563eb") +
  geom_hline(yintercept = mean(movies_ratings$rating), 
             color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Uncertainty",
    subtitle = "Low-volume movies show 'fake' extreme ratings (1.0 or 5.0)",
    x = "Number of Ratings",
    y = "Average Rating"
  ) +
  scale_x_log10() + # Log scale makes the funnel shape obvious
  annotate("text", x = 2, y = 4.8, label = "", color = "darkred")

# 1. Fit a basic linear model matching your Model A assumption
linear_model <- lm(rating ~ movie_age, data = movies_ratings)

# 2. Add residuals back to the dataset
movies_ratings_residuals <- movies_ratings %>%
  mutate(residual = residuals(linear_model)) %>%
  group_by(movie_age) %>%
  summarise(avg_residual = mean(residual, na.rm = TRUE))

# 3. Plot the residuals
ggplot(movies_ratings_residuals, aes(x = movie_age, y = avg_residual)) +
  geom_point(color = "#2563eb", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  # Add a trend line to the errors
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Residual Diagnostic Plot for Movie Age",
    subtitle = "A curved red line proves the linear assumption systematically fails",
    x = "Movie Age at Time of Review",
    y = "Average Residual Error"
  )


movies_ratings_master <- ratings %>%
  left_join(movies, by = "movieId") %>%
  mutate(
    rating_year = lubridate::year(as_datetime(timestamp)),
    movie_year_numeric = as.numeric(year) 
  ) %>%
  filter(!is.na(movie_year_numeric)) %>%
  mutate(
    movie_age = rating_year - movie_year_numeric
  ) %>%
  filter(movie_age >= 0) %>%
  # Construct contiguous integer indices required by Stan for categorical indexing
  mutate(
    user_idx  = as.integer(factor(userId)),
    movie_idx = as.integer(factor(movieId)),
    genre_idx = as.integer(factor(genres))
  )


N_obs    <- nrow(movies_ratings_master)
N_users  <- max(movies_ratings_master$user_idx)
N_movies <- max(movies_ratings_master$movie_idx)
N_genres <- max(movies_ratings_master$genre_idx)
K_basis  <- ncol(B_matrix_shared)

# Create Spline Matrix
library(splines)
knots_choice <- c(15, 50, 60)
B_matrix <- bs(movies_ratings_master$movie_age, knots = knots_choice, degree = 3, intercept = FALSE)
K_basis <- ncol(B_matrix)



# Stan sampler compilation
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# MODEL A: BASELINE HIERARCHICAL SPLINE FILTER
stan_list_model_a <- list(
  N = N_obs,
  N_users = N_users,
  N_movies = N_movies,
  K = K_basis,
  user_id = movies_ratings_master$user_idx,
  movie_id = movies_ratings_master$movie_idx,
  B = B_matrix,
  rating = movies_ratings_master$rating
)

# 3. Execution Block
fit_a <- stan(
  file = "stan_models/model_a.stan", 
  data = stan_list_model_a,
  iter = 1000,
  warmup = 500,
  chains = 2, 
  thin = 1,
  seed = 1
)
print(fit_a, pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y"))
library(bayesplot)
library(ggplot2)
mcmc_trace(as.array(fit_a), pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y")) +
  theme_minimal() +
  ggtitle("MCMC Trace Plot: Model A Parametrs")

library(tidyverse)

# 1. Calculate raw empirical averages and sample sizes
raw_data_features <- movies_ratings_master %>%
  group_by(movie_idx) %>%
  summarise(
    n_ratings = n(),
    raw_avg = mean(rating),
    .groups = "drop"
  )

# 2. Extract beta_0 and alpha posterior means from Model A
beta_0_mean_a <- summary(fit_a, pars = "beta_0")$summary[, "mean"]

bayes_estimates_a <- summary(fit_a, pars = "alpha")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, post_mean_alpha = mean) %>%
  left_join(raw_data_features, by = "movie_idx") %>%
  mutate(bayes_score = beta_0_mean_a + post_mean_alpha)

# 3. Reshape data into a long format for side-by-side plotting
plot_data <- bayes_estimates_a %>%
  select(movie_idx, n_ratings, `Raw Average` = raw_avg, `Bayesian Score` = bayes_score) %>%
  pivot_longer(
    cols = c(`Raw Average`, `Bayesian Score`),
    names_to = "Estimation_Type",
    values_to = "Rating"
  )

# 4. Generate the Bayesian Shrinkage Funnel Plot
ggplot(plot_data, aes(x = n_ratings, y = Rating, color = Estimation_Type)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = beta_0_mean_a, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(
    breaks = c(1, 2, 3, beta_0_mean_a, 4, 5), 
    labels = c("1", "2", "3", as.character(round(beta_0_mean_a, 2)), "4", "5")
  ) +
  scale_color_manual(values = c("Raw Average" = "#E41A1C", "Bayesian Score" = "#377EB8")) +
  labs(
    title = "Bayesian Shrinkage Effect",
    subtitle = "How Hierarchical Modeling Pulls Low-Volume Noise Toward the Global Mean",
    x = "Number of Ratings",
    y = "Movie Rating Score",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )



# Posterior dENSITY 
color_scheme_set("blue")

# Generate the posterior density intervals plot
mcmc_areas(
  as.array(fit_a), 
  pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y"),
  prob = 0.50,  # Inner thick band shows the 50% interval
  prob_outer = 0.95 # Outer thin tails show the 95% Credible Interval
) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Posterior Error Distributions with Credible Intervals",
    x = "Parameter Value Estimate",
    y = "Parameter Name"
  ) +
  theme(
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

library(dplyr)
library(tidyr)

# 1. Pull the raw empirical averages from your data
raw_rankings <- movies_ratings_master %>%
  group_by(movie_idx, title) %>%
  summarise(
    n_ratings = n(),
    raw_avg = mean(rating),
    .groups = "drop"
  ) %>%
  mutate(raw_rank = min_rank(desc(raw_avg)))

# 2. Extract the converged posterior means from your final model run
beta_0_mean <- summary(fit_a, pars = "beta_0")$summary[,"mean"]

bayes_rankings <- summary(fit_a, pars = "alpha")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, post_mean_alpha = mean) %>%
  left_join(raw_rankings, by = "movie_idx") %>%
  # Reconstruct the absolute rating scale: beta_0 + alpha_i
  mutate(bayes_est = beta_0_mean + post_mean_alpha) %>%
  mutate(bayes_rank = min_rank(desc(bayes_est)))

# 3. Print the Final Converged Top 10 Report Table
report_table <- bayes_rankings %>%
  arrange(bayes_rank) %>%
  head(10) %>%
  select(
    `Bayesian Rank` = bayes_rank,
    Title = title,
    `Bayesian Score` = bayes_est,
    `Raw Rank` = raw_rank,
    `Raw Score` = raw_avg,
    `Sample Size (N)` = n_ratings
  ) %>%
  mutate(
    `Rank Shift` = `Raw Rank` - `Bayesian Rank`
  )

print(report_table)

spline_means <- summary(fit_a, pars = "gamma")$summary

spline_df <- data.frame(
  Time = 1:nrow(spline_means),
  Effect = spline_means[, "mean"]  # Explicitly pulling the posterior means
)

# 3. Plot the timeline trend
ggplot(spline_df, aes(x = Time, y = Effect)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) + # Added dots for the individual knots
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Latent Rating Trends Over Time",
    subtitle = "Non-linear spline component (Gamma)",
    x = "Timeline (Spline Knots/Bins)",
    y = "Rating Deflection Effect"
  ) +
  theme(plot.title = element_text(face = "bold"))


saveRDS(fit_a, file = "converged_model_a_fit.rds")

# MODEL B
movies_ratings_genre <- ratings %>%
  left_join(movies, by = "movieId") %>%
  mutate(
    rating_year = lubridate::year(as_datetime(timestamp)),
    movie_year_numeric = as.numeric(Year)
  ) %>%
  filter(!is.na(movie_year_numeric)) %>%
  mutate(
    movie_age = rating_year - movie_year_numeric
  ) %>%
  filter(movie_age >= 0) %>%
  mutate(
    genre_group = case_when(
      grepl("Action|Adventure|War", genres) ~ "Action",
      grepl("Romance|Musical", genres) ~ "Romance",
      grepl("Sci-Fi|Fantasy|Animation", genres) ~ "SciFi_Fantasy",
      grepl("Comedy|Children", genres) ~ "Comedy",
      grepl("Drama", genres) ~ "Drama",
      grepl("Horror|Thriller|Mystery|Crime", genres) ~ "Horror_Thriller",
      grepl("Documentary", genres) ~ "Documentary",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    user_idx  = as.integer(factor(userId)),
    movie_idx = as.integer(factor(movieId)),
    genre_idx = as.integer(factor(genre_group))
  )


N_obs_b    <- nrow(movies_ratings_genre)
N_users_b  <- max(movies_ratings_genre$user_idx)
N_movies_b <- max(movies_ratings_genre$movie_idx)
N_genres_b <- max(movies_ratings_genre$genre_idx)

#stan
stan_list_model_b <- list(
  N = N_obs_b,
  N_users = N_users_b,
  N_movies = N_movies_b,
  N_genres = N_genres_b,
  K = K_basis,
  user_id = movies_ratings_genre$user_idx,
  movie_id = movies_ratings_genre$movie_idx,
  genre_id = movies_ratings_genre$genre_idx,
  B = B_matrix,
  rating = movies_ratings_genre$rating
)
fit_b <- stan(
  file = "stan_models/model_b.stan", 
  data = stan_list_model_b,
  iter = 1000,
  warmup = 500,
  chains = 2, 
  thin = 1,
  seed = 1
)
fit_b <- readRDS("converged_model_b_fit.rds")
print(fit_b, pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y"))

mcmc_trace(as.array(fit_b), pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y")) +
  theme_minimal() +
  ggtitle("MCMC Trace Plot: Model B Parametrs")

#B posterior error
mcmc_areas(
  as.array(fit_b), 
  pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y"),
  prob = 0.50,  # Inner thick band shows the 50% interval
  prob_outer = 0.95 # Outer thin tails show the 95% Credible Interval
) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Posterior Error Distributions with Credible Intervals",
    x = "Parameter Value Estimate",
    y = "Parameter Name"
  ) +
  theme(
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )
library(tidyverse)

library(rstan)


raw_rankings_b <- movies_ratings_genre %>%
  group_by(movie_idx, title) %>%
  summarise(
    n_ratings = n(),
    raw_avg = mean(rating),
    .groups = "drop"
  ) %>%
  mutate(raw_rank = min_rank(desc(raw_avg)))

beta_0_mean_b <- summary(fit_b, pars = "beta_0")$summary[, "mean"]

bayes_rankings_b <- summary(fit_b, pars = "alpha")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, post_mean_alpha = mean) %>%
  left_join(raw_rankings_b, by = "movie_idx") %>%
  mutate(bayes_est = beta_0_mean_b + post_mean_alpha) %>%
  mutate(bayes_rank = min_rank(desc(bayes_est)))

report_table_b <- bayes_rankings_b %>%
  arrange(bayes_rank) %>%
  head(10) %>%
  select(
    `Bayesian Rank` = bayes_rank,
    Title = title,
    `Bayesian Score` = bayes_est,
    `Raw Rank` = raw_rank,
    `Raw Score` = raw_avg,
    `Sample Size (N)` = n_ratings
  ) %>%
  mutate(
    `Rank Shift` = `Raw Rank` - `Bayesian Rank`
  )

print(report_table_b)

saveRDS(fit_b, file = "converged_model_b_fit.rds")


get_personalized_recommendations <- function(target_user_idx, fit_b, df_master, B_matrix, top_n = 10) {
  
  # 1. Extract posterior means from Model B
  beta_0_m   <- summary(fit_b, pars = "beta_0")$summary[, "mean"]
  alpha_m    <- summary(fit_b, pars = "alpha")$summary[, "mean"]
  delta_m    <- summary(fit_b, pars = "delta")$summary[, "mean"]
  gamma_m    <- summary(fit_b, pars = "gamma")$summary[, "mean"]
  theta_m    <- summary(fit_b, pars = "theta")$summary[, "mean"] 
  
  # Extract dimensions and reconstruct theta matrix
  N_users  <- max(df_master$user_idx)
  N_genres <- max(df_master$genre_idx)
  theta_matrix <- matrix(theta_m, nrow = N_users, ncol = N_genres)
  
  # 2. Identify movies the user has ALREADY rated (to exclude them)
  rated_movies <- df_master %>%
    filter(user_idx == target_user_idx) %>%
    pull(movie_idx)
  
  # 3. Create a clean mapping dataframe of movie_idx to their text Title, Age, Genre ID, and Genre Group
  movie_title_lookup <- df_master %>%
    select(movie_idx, movie_title = title, movie_age, genre_idx, Genre_Name = genre_group) %>% 
    distinct(movie_idx, .keep_all = TRUE)
  movie_profile <- movie_title_lookup %>%
    filter(!movie_idx %in% rated_movies)
  movie_spline_mapping <- df_master %>%
    select(movie_idx) %>%
    mutate(row_num = row_number()) %>%
    group_by(movie_idx) %>%
    slice(1) %>%
    ungroup() %>%
    select(movie_idx, row_num) 
  recommendations <- movie_profile %>%
    left_join(movie_spline_mapping, by = "movie_idx") %>%
    rowwise() %>%
    mutate(
      spline_effect = sum(B_matrix[row_num, ] * gamma_m),
  
      predicted_rating = beta_0_m + 
        alpha_m[movie_idx] + 
        delta_m[target_user_idx] + 
        spline_effect + 
        theta_matrix[target_user_idx, genre_idx]
    ) %>%
    ungroup() %>%
    arrange(desc(predicted_rating)) %>%
    head(top_n) %>%
    select(Movie_Name = movie_title, Genre = Genre_Name, Age = movie_age, Expected_Rating = predicted_rating) %>%
    mutate(Rank = 1:n()) %>%
    select(Rank, Movie_Name, Genre, Age, Expected_Rating)
  
  return(recommendations)
}

# Run the updated function for your target user 41
user_39_recommendations <- get_personalized_recommendations(
  target_user_idx = 39, 
  fit_b = fit_b, 
  df_master = movies_ratings_genre, 
  B_matrix = B_matrix
)

print(user_39_recommendations)


# MODEL C
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
stan_list_model_c <- list(
  N = N_obs_b,
  N_users = N_users_b,
  N_movies = N_movies_b,
  N_genres = N_genres_b,
  K = K_basis,
  user_id = movies_ratings_genre$user_idx,
  movie_id = movies_ratings_genre$movie_idx,
  genre_id = movies_ratings_genre$genre_idx,
  B = B_matrix,
  rating = movies_ratings_genre$rating
)
fit_c <- stan(
  file = "stan_models/model_c.stan", 
  data = stan_list_model_c,
  iter = 1000,
  warmup = 500,
  chains = 2, 
  thin = 1,
  seed = 1
)

fit_c <- readRDS("converged_model_c_fit.rds")
print(fit_c, pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y"))

library(bayesplot)
library(ggplot2)
mcmc_trace(as.array(fit_c), pars = c("beta_0", "sigma_alpha", "sigma_delta", "sigma_y")) +
  theme_minimal() +
  ggtitle("MCMC Trace Plot: Model B Parametrs")

eta_summary <- summary(fit_c, pars = "eta")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, mean_eta = mean)

# Map back to movie names and calculate true standard deviation: sigma = exp(eta)
movie_risk_profiles <- movies_ratings_genre %>%
  select(movie_idx, Movie_Name = title, Genre = genre_group) %>%
  distinct(movie_idx, .keep_all = TRUE) %>%
  left_join(eta_summary, by = "movie_idx") %>%
  mutate(Audience_Sigma = exp(mean_eta))

# Pull the Top 5 most polarizing (Highest Sigma)
battlegrounds <- movie_risk_profiles %>%
  arrange(desc(Audience_Sigma)) %>%
  head(5) %>%
  mutate(Type = "Divisive Battleground (High Variance)")

# Pull the Top 5 lowest variance
safe_bets <- movie_risk_profiles %>%
  arrange(Audience_Sigma) %>%
  head(5) %>%
  mutate(Type = "Safe Consensus (Low Variance)")


model_c_polarization_table <- bind_rows(battlegrounds, safe_bets) %>%
  select(Type, Movie_Name, Genre, Audience_Sigma)

print(model_c_polarization_table)


library(dplyr)

library(dplyr)

plot_data <- data.frame(Rating = seq(1, 5, length.out = 1000)) %>%
  mutate(
    # Compute normal density curves
    Safe_Density  = dnorm(Rating, mean = 4.0, sd = 0.45),
    Risky_Density = dnorm(Rating, mean = 4.0, sd = 1.25)
  )

risk_zone <- plot_data %>% filter(Rating <= 3.0)


ggplot(plot_data, aes(x = Rating)) +
  geom_line(aes(y = Safe_Density, color = "Safe Consensus Movie (Low Variance)"), linewidth = 1.2) +
  geom_line(aes(y = Risky_Density, color = "High-Risk Cult Movie (High Variance)"), linewidth = 1.2) +
  geom_area(data = risk_zone, aes(y = Risky_Density, fill = "Risk Zone: P(Rating < 3.0) for Cult Movie"), alpha = 0.2) +
  scale_color_manual(name = "Model C Profiles", values = c("Safe Consensus Movie (Low Variance)" = "#1f77b4", "High-Risk Cult Movie (High Variance)" = "#d62728")) +
  scale_fill_manual(name = "Bayesian Evaluation", values = c("Risk Zone: P(Rating < 3.0) for Cult Movie" = "#d62728")) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Conceptual Framework: Model C Posterior Predictive Distributions (PPD)",
    subtitle = "Comparing recommendations with identical structural means but divergent polarization risks",
    x = "Simulated Future Rating (Stars)",
    y = "Probability Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_rect(fill = "white", color = NA)
  )

beta_0_mean_c <- summary(fit_c, pars = "beta_0")$summary[, "mean"]

# movie quality and polarization posterior
alpha_summary_c <- summary(fit_c, pars = "alpha")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, post_mean_alpha = mean)

eta_summary_c <- summary(fit_c, pars = "eta")$summary %>%
  as_tibble() %>%
  mutate(movie_idx = 1:n()) %>%
  select(movie_idx, post_mean_eta = mean)


movie_metadata <- movies_ratings_genre %>%
  select(movie_idx, Movie_Name = title, Genre = genre_group) %>%
  distinct(movie_idx, .keep_all = TRUE)

top_10_model_c <- movie_metadata %>%
  left_join(alpha_summary_c, by = "movie_idx") %>%
  left_join(eta_summary_c, by = "movie_idx") %>%
  mutate(
    # Model C Score: 
    Bayesian_Score = beta_0_mean_c + post_mean_alpha,
    Audience_Risk  = exp(post_mean_eta)
  ) %>%
  # Sorting
  arrange(desc(Bayesian_Score)) %>%
  head(10) %>%
  mutate(Rank = 1:n()) %>%
  select(Rank, Movie_Name, Genre, Bayesian_Score, Audience_Risk)

# Print the final golden table
print(top_10_model_c)


library(rstan)
library(tidyverse)

# Model c recommendation system
get_model_c_ppd_recommendations <- function(target_user_idx, fit_c, movies_ratings_genre, B_matrix, top_n = 10) {
  

  chains <- rstan::extract(fit_c)
  

  rated_movies <- movies_ratings_genre %>% 
    filter(user_idx == target_user_idx) %>% 
    pull(movie_idx)
  

  movie_profile <- movies_ratings_genre %>% 
    select(movie_idx, movie_title = title, movie_age, genre_idx, genre_name = genre_group) %>% 
    distinct(movie_idx, .keep_all = TRUE) %>% 
    filter(!movie_idx %in% rated_movies)
  

  movie_spline_mapping <- movies_ratings_genre %>% 
    select(movie_idx) %>% 
    mutate(row_num = row_number()) %>% 
    group_by(movie_idx) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(movie_idx, row_num)
  

  recommendation_results <- movie_profile %>% 
    left_join(movie_spline_mapping, by = "movie_idx") %>% 
    rowwise() %>% 
    mutate(
      spline_effect_chain = list(as.vector(chains$gamma %*% B_matrix[row_num, ])),
      mu_chain = list(chains$beta_0 + 
                      chains$alpha[, movie_idx] + 
                      chains$delta[, target_user_idx] + 
                      unlist(spline_effect_chain)),
      sigma_chain = list(exp(chains$eta[, movie_idx])),
      ppd_simulations = list(rnorm(length(unlist(mu_chain)), 
                                   mean = unlist(mu_chain), 
                                   sd = unlist(sigma_chain))),
      Expected_Rating = mean(unlist(ppd_simulations)), 
      Polarization_Risk = mean(unlist(sigma_chain)), 
      Prob_Under_3_Stars = mean(unlist(ppd_simulations) < 3.0) 
    ) %>% 
    ungroup() %>% 
    group_by(genre_idx) %>% 
    arrange(desc(Expected_Rating)) %>% 
    slice_head(n = 4) %>% 
    ungroup() %>% 
    arrange(desc(Expected_Rating)) %>% 
    head(top_n) %>% 
    mutate(Rank = 1:n()) %>% 
    select(Rank, Movie_Name = movie_title, Genre = genre_name, Age = movie_age, Expected_Rating, Polarization_Risk, Prob_Under_3_Stars)
  
  return(recommendation_results)
}

# Run it for your target user 42
user_42_model_c_rec <- get_model_c_ppd_recommendations(
  target_user_idx = 42, 
  fit_c = fit_c, 
  movies_ratings_genre = movies_ratings_genre, 
  B_matrix = B_matrix
)

print(user_42_model_c_rec)
saveRDS(fit_c, file = "converged_model_c_fit.rds")

