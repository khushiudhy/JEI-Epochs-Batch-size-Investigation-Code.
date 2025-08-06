library(tidyverse)
library(modeldata)
library(ggplot2)
library(dplyr)
library(viridis)

# GRAPHS 

# EPOCHS

# changing training and test accuracies to values in %
Epochs_train <- mutate(Epochs_data, percent_train_accuracy = accuracy * 100)
Epochs_test <- mutate(Epochs_train, percent_test_accuracy = test_accuracy * 100)

# Plotting for epochs 3-7.
Epochs <- filter(Epochs_test, epochs <= 7) 


# TRAINING GRAPH 

# Create dummy data frame for benchmark line for Epochs plotting
benchmark_df_epochs <- data.frame(
  x = c(min(Epochs$epochs), max(Epochs$epochs)),
  y = c(50, 50),
  label = "Benchmark for Random Guessing"
)

ggplot(Epochs, aes(x = epochs, y = percent_train_accuracy)) +
  geom_point(aes(color = "Trials")) +
  geom_smooth(aes(color = "Regression line"), method = "lm", se = FALSE) +
  geom_line(data = benchmark_df, aes(x = x, y = y, color = label), linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    # Order legend by the order you specify here
    breaks = c("Trials", "Regression line", "Benchmark for Random Guessing"),
    values = c("Trials" = "black",
               "Regression line" = "blue",
               "Benchmark for Random Guessing" = "tan1")
  ) +
  labs(x = "Epochs",
       y = "Training accuracy (%) with 100 samples") +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "right")


#TESTING GRAPH

ggplot(Epochs, aes(x = epochs, y = percent_test_accuracy)) +
  geom_point(aes(color = "Trials")) +
  geom_smooth(aes(color = "Regression line"), method = "lm", se = FALSE) +
  geom_line(data = benchmark_df, aes(x = x, y = y, color = label), linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    # Order legend by the order you specify here
    breaks = c("Trials", "Regression line", "Benchmark for Random Guessing"),
    values = c("Trials" = "black",
               "Regression line" = "blue",
               "Benchmark for Random Guessing" = "tan1")
  ) +
  labs(x = "Epochs",
       y = "Testing accuracy (%) with 20 samples") +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "right")


# BATCH SIZE

# changing training and test accuracies to values in %
Batch_train <- mutate(BatchSize_data, percent_train_accuracy = accuracy * 100)
BatchSize <- mutate(Batch_train, percent_test_accuracy = test_accuracy * 100)

# Create dummy data frame for benchmark line for BatchSize plotting
benchmark_df_batchsize <- data.frame(
  x = c(min(BatchSize$batch_size), max(BatchSize$batch_size)),
  y = c(50, 50),
  label = "Benchmark for Random Guessing"
)

# TRAINING GRAPH

ggplot(BatchSize, aes(x = batch_size, y = percent_train_accuracy)) +
  geom_point(aes(color = "Trials")) +
  geom_smooth(aes(color = "Regression line"), method = "lm", se = FALSE) +
  geom_line(data = benchmark_df_batchsize, aes(x = x, y = y, color = label), linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    # Order legend by the order you specify here
    breaks = c("Trials", "Regression line", "Benchmark for Random Guessing"),
    values = c("Trials" = "black",
               "Regression line" = "blue",
               "Benchmark for Random Guessing" = "tan1")
  ) +
  labs(x = "Batch Size",
       y = "Training accuracy (%) with 100 samples") +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "right")


# TESTING GRAPH

ggplot(BatchSize, aes(x = batch_size, y = percent_test_accuracy)) +
  geom_point(aes(color = "Trials")) +
  geom_smooth(aes(color = "Regression line"), method = "lm", se = FALSE) +
  geom_line(data = benchmark_df_batchsize, aes(x = x, y = y, color = label), linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    # Order legend by the order you specify here
    breaks = c("Trials", "Regression line", "Benchmark for Random Guessing"),
    values = c("Trials" = "black",
               "Regression line" = "blue",
               "Benchmark for Random Guessing" = "tan1")
  ) +
  labs(x = "Batch Size",
       y = "Testing accuracy (%) with 20 samples") +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "right")
# Bubble Plot
freq_Batchsize <- BatchSize %>%
  group_by(batch_size, test_accuracy) %>%
  summarise(freq = n())

ggplot(freq_Batchsize, aes(x = batch_size, y = (test_accuracy*100), size = freq)) +
  geom_point(alpha = 0.8, shape = 21, color = "black", fill = "tan1") +
  scale_size_continuous(name = "Frequency of trials", range = c(1, 16), guide = "none") +
  #scale_fill_viridis_c(name = "Frequency", option = "viridis") +
  labs(x = "Batch Size", y = "Testing Accuracy (%) for 20 test samples", size = "Frequency") +
  guides(size = guide_legend(override.aes = list(shape = 21)))+
  scale_x_continuous(breaks = c(5, 10, 25, 50, 75, 100))+
  theme(axis.title.y = element_text(size = 8), axis.title.x = element_text(size =8))

# STATISTICAL ANALYSIS 

# epochs/training accuracy and epochs/testing accuracy
epochsTrain.regression <- lm(percent_train_accuracy ~ epochs, data=Epochs)
summary(epochsTrain.regression)

epochsTest.regression <- lm(percent_test_accuracy ~ epochs, data=Epochs)
summary(epochsTest.regression)

# batch_size/training accuracy and batch_size/testing accuracy
batchTrain.regression <- lm(percent_train_accuracy ~ batch_size, data=BatchSize)
summary(batchTrain.regression)

batchTest.regression <- lm(percent_test_accuracy ~ batch_size, data=BatchSize)
summary(batchTest.regression)


# EPOCHS mean accuracy, standard deviation and margin of error (Epochs_big refers to epochs >= 10)
Epochs_big <- filter(Epochs_data, epochs >= 10)
data <- Epochs_big

#TRAINING
# Group data by epochs and calculate mean, standard deviation, margin of error (w/ 95% confidence interval), and confidence interval
epoch_train_results <- data %>%
  group_by(epochs) %>%
  summarise(
    epoch_mean_train_accuracy = mean(accuracy),
    epoch_sd_train_accuracy = sd(accuracy),
    n = n(),
    t_value = qt(0.975, df = n-1),
    margin_of_error = t_value * (epoch_sd_train_accuracy / sqrt(n)),
    lower_bound = epoch_mean_train_accuracy - margin_of_error,
    upper_bound = epoch_mean_train_accuracy + margin_of_error
  )

# Display the results
print(epoch_train_results)

#TESTING
# Group data by epochs and calculate mean, standard deviation, margin of error (w/ 95% confidence interval), and confidence interval
epoch_test_results <- data %>%
  group_by(epochs) %>%
  summarise(
    epoch_mean_test_accuracy = mean(test_accuracy),
    epoch_sd_test_accuracy = sd(test_accuracy),
    n = n(),
    t_value = qt(0.975, df = n-1),
    margin_of_error = t_value * (epoch_sd_test_accuracy / sqrt(n)),
    lower_bound = epoch_mean_test_accuracy - margin_of_error,
    upper_bound = epoch_mean_test_accuracy + margin_of_error
  )

# Display the results
print(epoch_test_results)


# BATCH_SIZE testing mean accuracy, standard deviation and margin of error
# Group data by batch_size and calculate mean, standard deviation, margin of error (w/ 95% confidence interval)
batch_test_results <- BatchSize %>%
  group_by(batch_size) %>%
  summarise(
    batch_mean_test_accuracy = mean(test_accuracy),
    batch_sd_test_accuracy = sd(test_accuracy),
    n = n(),
    t_value = qt(0.975, df = n-1),
    margin_of_error = t_value * (batch_sd_test_accuracy / sqrt(n)),
    lower_bound = batch_mean_test_accuracy - margin_of_error,
    upper_bound = batch_mean_test_accuracy + margin_of_error
  )

# Display the results
print(batch_test_results)


# EVALUATING THE FREQUENCY OF GETTING 50% TEST ACCURACY FOR BATCH_SIZE 75 AND 100
# Filter and count for batch size 75
count_batch_75 <- BatchSize %>%
  filter(batch_size == 75 & test_accuracy == 0.5) %>%
  tally()

# Filter and count for batch size 100
count_batch_100 <- BatchSize %>%
  filter(batch_size == 100 & test_accuracy == 0.5) %>%
  tally()

# Print the results
cat("Number of times 0.5 accuracy was received for batch size 75: ", count_batch_75$n, "\n")
cat("Number of times 0.5 accuracy was received for batch size 100: ", count_batch_100$n, "\n")

