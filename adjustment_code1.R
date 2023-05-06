
# adapted from Alok Pattani's python code (https://colab.research.google.com/drive/13L4b36cTrnC55ahD6dVf4-r9pzkYV-j5#scrollTo=NL7BQDlveAd9)

library(tidyverse)
library(glmnet)

# Read in Seasons Table from CSV
seasons_csv_file_id <- '1f_ooylIcJ9AYSLe0WposhGoeTqjo0Ihm'
seasons_csv_file_name <- 'seasons.csv'

seasons <- read_csv(sprintf('https://drive.google.com/uc?id=%s&export=download', 
                            seasons_csv_file_id))


# Read in Metrics Table from CSV
metrics_csv_file_id <- '1azVm-0kFZu66lYnjFB9YOKKuveZYTN-Z'
metrics_csv_file_name <- 'metrics.csv'

metrics <- read_csv(sprintf('https://drive.google.com/uc?id=%s&export=download', 
                            metrics_csv_file_id))


# Read in Team Games Table from CSV
team_games_csv_file_id <- '15RPqLc9Xfqq4-5LezteOHtF0E7FS8xB8'
team_games_csv_file_name <- 'team_games.csv'

team_games <- read_csv(sprintf('https://drive.google.com/uc?id=%s&export=download', 
                               team_games_csv_file_id))


# Read in Team Seasons Table from CSV
team_seasons_csv_file_id <- '1G-Pt9-TVvkJ0mSpAGta6fBMHxwRsjpUK'
team_seasons_csv_file_name <- 'team_seasons.csv'

team_seasons <- read_csv(sprintf('https://drive.google.com/uc?id=%s&export=download', 
                                 team_seasons_csv_file_id))


# Read in Team Colors Table from CSV
team_colors_csv_file_id <- '1__VF83X2wqztvIXWIye3Hfs0mSnbxC49'
team_colors_csv_file_name <- 'team_colors.csv'

# Reading the data and renaming columns
team_colors <- read_csv(sprintf('https://drive.google.com/uc?id=%s&export=download', 
                                team_colors_csv_file_id)) %>%
  rename(tm_code = team_code, tm = school)


# Melt Team Season Stats, Get List of Seasons/Stats to Adjust
team_seasons_stats_melt <- team_seasons %>%
  select(season, tm_code, tm, all_of(metrics$stat_name)) %>%
  pivot_longer(cols = all_of(metrics$stat_name),
               names_to = "stat_name",
               values_to = "raw_stat") 

season_stats <- team_seasons_stats_melt %>%
  distinct(season, stat_name) %>%
  # select(-raw_stat) %>%
  arrange(season, stat_name)

#@title Use Ridge Regression to Get Adjusted Team Stats

# Create combo fields for season_tm & season_opp to be used in regression
team_games <- team_games %>%
  mutate(season_tm = paste0(season, "_", tm_code),
         season_opp = paste0(season, "_", opp_code))

# Set up empty data frame to store regression results
reg_results_collection <- tibble(season = integer(),
                                 stat_name = character(),
                                 coef_name = character(),
                                 ridge_reg_coef = double(),
                                 ridge_reg_value = double())

# Iterate through each season and stat
for (i in 1:nrow(season_stats)) {
  row <- season_stats[i, ]
  
  # Subset games to this season
  this_season_game_stat <- team_games %>%
    filter(season == row$season) %>%
    select(season_tm, tm_hca, season_opp, !!sym(row$stat_name)) %>%
    mutate(index = row_number()) %>%
    select(index, everything())
  
  # Convert categorical variables to dummy variables
  this_season_game_dummy_vars <- this_season_game_stat %>%
    select(season_tm, tm_hca, season_opp) %>%
    mutate_all(as.factor) %>%
    model.matrix(~., data = .)
  
  # Prepare response variable
  this_season_game_response <- this_season_game_stat[[row$stat_name]]
  
  # Fit ridge regression to given statistic using season game dummy variables
  reg_fit <- cv.glmnet(x = this_season_game_dummy_vars,
                       y = this_season_game_response,
                       alpha = 0,
                       lambda.min.ratio = 0.0001,
                       standardize = FALSE,
                       intercept = TRUE)
  
  # Extract regression coefficients and put into data frame with coef names
  coef_names <- rownames(coef(reg_fit, s = "lambda.min"))
  this_reg_results <- tibble(season = row$season,
                             stat_name = row$stat_name,
                             coef_name = coef_names,
                             ridge_reg_coef = as.matrix(coef(reg_fit, s = "lambda.min"))[,"s1"])
  
  # Add intercept back in to reg coef to get 'adjusted' value
  this_reg_results <- this_reg_results %>%
    mutate(ridge_reg_value = ridge_reg_coef + coef(reg_fit)[1])
  
  reg_results_collection <- bind_rows(reg_results_collection,
                                      this_reg_results)
}

# Only keep ratings from 'season_tm' perspective for this
tm_seasons_adjresults <- reg_results_collection %>%
  filter(str_sub(coef_name, 1, 9) == "season_tm") %>%
  rename(adj_stat = ridge_reg_value) %>%
  mutate(season = as.integer(season),
         tm_code = as.integer(str_sub(coef_name, 15)))


# compare adjusted to raw

adj_eff_18 <- tm_seasons_adjresults %>% 
  filter(season == 2018, 
         stat_name  == "tm_net_eff")

raw_eff_18 <- team_seasons_stats_melt %>% 
  filter(season == 2018,
         stat_name  == "tm_net_eff",
         tm_code != -1)

combined_eff_18 <- left_join(adj_eff_18, raw_eff_18, by = c("season", "tm_code", "stat_name"))

ggplot(combined_eff_18, aes(raw_stat, adj_stat)) +
  geom_point() +
  geom_abline(slope = 1)















