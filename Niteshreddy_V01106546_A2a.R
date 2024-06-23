# Load necessary libraries
library(readr)
library(dplyr)
library(fuzzywuzzyR)
library(caret)
library(broom)

setwd('/Users/niteshreddy/Downloads/untitled folder')
getwd()

install.packages("readxl")

# Load data
df_ipl <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

install.packages("caret")
# Group data by season, innings, striker, and bowler
grouped_data <- df_ipl %>%
  group_by(Season, Innings_No, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), wicket_confirmation = sum(wicket_confirmation))

# Calculate total runs and wickets for each year
total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored))

total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation))

# Match names using fuzzy matching
match_names <- function(name, names_list) {
  match <- fuzzy_match(name, names_list, threshold = 80)
  return(match)
}

# Create a new column in df_salary with matched names from df_runs
df_salary$Matched_Player <- sapply(df_salary$Player, function(x) match_names(x, total_runs_each_year$Striker))

# Merge the DataFrames on the matched names
df_merged <- merge(df_salary, total_runs_each_year, by.x = "Matched_Player", by.y = "Striker")

# Subset data for last three years
df_merged <- df_merged %>% filter(Season %in% c("2021", "2022", "2023"))

# Split data into training and test sets
set.seed(42)
train_index <- createDataPartition(df_merged$Rs, p = 0.8, list = FALSE)
X_train <- df_merged[train_index, "runs_scored"]
y_train <- df_merged[train_index, "Rs"]
X_test <- df_merged[-train_index, "runs_scored"]
y_test <- df_merged[-train_index, "Rs"]

# Fit linear regression model
model <- lm(Rs ~ runs_scored, data = df_merged[train_index, ])
summary(model)

# Repeat the process for wickets
df_salary$Matched_Player <- sapply(df_salary$Player, function(x) match_names(x, total_wicket_each_year$Bowler))
df_merged <- merge(df_salary, total_wicket_each_year, by.x = "Matched_Player", by.y = "Bowler")
df_merged <- df_merged %>% filter(Season %in% c("2022"))

set.seed(42)
train_index <- createDataPartition(df_merged$Rs, p = 0.8, list = FALSE)
X_train <- df_merged[train_index, "wicket_confirmation"]
y_train <- df_merged[train_index, "Rs"]
X_test <- df_merged[-train_index, "wicket_confirmation"]
y_test <- df_merged[-train_index, "Rs"]

model <- lm(Rs ~ wicket_confirmation, data = df_merged[train_index, ])
summary(model)