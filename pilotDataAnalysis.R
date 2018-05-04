# =============================================
# Pilot Study: Analysis of the collected Data
# =============================================

# =============================================
# Load Packages
pacman::p_load(
  # data processing
  plyr,
  tidyverse,  # collection of the tidyverse packages
  stringr,    #   - for string functions
  forcats,    #   - utility functions for working with factor levels
  lubridate,  #   - utility for parsing and performing arithematic on dates 
  tools,      #   - for package development, administration and documentation
  data.table, #   - for data manipulation 
  tibble     #   - to create data frames
)

# =============================================
## Data Import
# Create a function that imports all .csv files from a directory
file_list <- list.files(path = file.path("PilotData"), pattern = "*.csv", recursive = TRUE, full.names = TRUE)

data_raw <- do.call("rbind", lapply(file_list, function(filename)
  data.frame(read.delim(filename, sep = ";"))))

# Keep only the columns of interest and give them more readable names
data <- data_raw %>% 
  select(
    Participant = Part,
    Condition = Cond,
    Block,
    TrialNumber = Trial,
    TargetNumber = Click,
    TargetHighlighted = Click_t,
    TargetClicked = Tar_t,
    PosX,
    PosY,
    Distance = Dist,
    TargetIsHit = Succ
  )


# Renaming of cell values in a more meaningful way
data$Condition[data$Condition == '0'] <- "Regular Edge"
data$Condition[data$Condition == '1'] <- "Virtual Edge"
data$Block[data$Block == '0'] <- "Demo"
data$TargetIsHit[data$TargetIsHit == '0'] <- FALSE
data$TargetIsHit[data$TargetIsHit == '1'] <- TRUE
data$TargetHighlighted <- as.POSIXct(data$TargetHighlighted, format = "%d-%m-%Y %H:%M:%S:%OS")
data$TargetClicked <- as.POSIXct(data$TargetClicked, format = "%d-%m-%Y %H:%M:%S:%OS")

# Option to display the ms as well. -> Doesn't work properly yet...
options(digits.secs = 3)

# Remove all Demo Trial & the 10 sequence (because it is inconsistent) from the data set
data <- data %>%
  filter(Block != "Demo" & Block != "10")

# =============================================
# CALCULATE VALUES PER SEQUENCE
# Calculate Error_Rate for each sequence
temp_get_TargetsHit <- data %>%
  group_by(Condition, Participant, TrialNumber, Block)  %>%
  summarise(ErrorRate = 1 - mean(TargetIsHit))


# cauculate Movement Time for each sequence
# Get first Timestamp (TargetHighlighted)of  a sequence and calculate the difference ==> Movement Time
temp_get_start_time <- data %>%
  select(
    Participant,
    Condition,
    Block,
    TrialNumber,
    TargetNumber,
    StartTime = TargetHighlighted) %>%
  filter(TargetNumber == 1) %>%
  select(-TargetNumber)

# Get last Timestamp (TargetClicked) of a sequence
temp_get_end_time <- data %>%
  select(
    Participant,
    Condition,
    Block,
    TrialNumber,
    TargetNumber,
    EndTime = TargetClicked) %>%
  # Since some values for target 5 are missing, we will only look at target 1-4 for the pilot study.
  # TODO: Change TargetNumber back to 5 in the real analysis
  filter(TargetNumber == 5) %>%
  select(-TargetNumber)

# Create table with both first and last timestamp of a sequence and calculate the difference
data_per_sequence <- data %>%
  select(
    Participant,
    Condition,
    Block,
    TrialNumber
    ) %>%
  full_join(temp_get_start_time) %>%
  full_join(temp_get_end_time) %>%
  mutate(MovementTime = as.numeric(EndTime - StartTime, unit ="secs")) %>%
  unique() %>%
  full_join(temp_get_TargetsHit)


# Add all calculated Movement_Times together to get the total time for one pointing task trial
# data_per_sequence <- data_per_sequence %>%
  #filter(MovementTime > 0 ) #& MovementTime < 10

# =============================================
# CALCULATE VALUES FOR WHOLE TRIAL
movement_time_per_trial <- aggregate(MovementTime ~ Condition + Participant + TrialNumber, data = data_per_sequence, sum)
movement_time_per_trial_virtual_edge <- movement_time_per_trial %>%
  filter(Condition == "Virtual Edge")

movement_time_per_trial_regular_edge <- movement_time_per_trial %>%
  filter(Condition == "Regular Edge")

error_rate_per_trial <- aggregate(ErrorRate ~ Condition + Participant + TrialNumber, data = data_per_sequence, mean)

error_rate_per_trial_regular_edge <- error_rate_per_trial %>%
  filter(Condition == "Regular Edge")


error_rate_per_trial_virtual_edge <- error_rate_per_trial %>%
  filter(Condition == "Virtual Edge")

# =============================================
# DISPLAY RESULTS
# Display movement time per trial for the regular edge condition
ggplot (data = movement_time_per_trial_regular_edge, mapping = aes(x = TrialNumber, y = MovementTime, col = Participant, group = Participant)) +
  geom_point() +
  geom_line() +
  labs(x = 'Trial Number - Regular Edge', y = 'Movement Time per Trial in ms')

# Display movement time per trial for the virtual edge condition
ggplot (data = movement_time_per_trial_virtual_edge, mapping = aes(x = TrialNumber, y = MovementTime, col = Participant, group = Participant)) +
  geom_point() +
  geom_line() +
  labs(x = 'Trial Number - Virtual Edge', y = 'Movement Time per Trial in ms')

# Display error rate per trial for the regular edge condition
ggplot(data = error_rate_per_trial_regular_edge, mapping = aes(x = TrialNumber, y = ErrorRate, col = Participant, group = Participant)) +
  geom_point() +
  geom_line() +
  labs(x = 'Trial Number - Regular Edge', y = 'Error Rate in %')

ggplot(data = error_rate_per_trial_virtual_edge, mapping = aes(x = TrialNumber, y = ErrorRate, col = Participant, group = Participant)) +
  geom_point() +
  geom_line() +
  labs(x = 'Trial Number - Virtual Edge', y = 'Error Rate in %')





