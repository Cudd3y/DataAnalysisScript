### Error management
## Clear workspace
rm(list=ls())

# Load Packages
pacman::p_load(
  # data processing
  plyr,
  tidyverse  # collection of the tidyverse packages
)

# because of a coding problem, participants 11 - 17 were coded as 1 - 7. To correct this problem, we import the data for each participant separately. 
data1 <- data.frame(read.delim("ExperimentalData/pointing_1.csv", sep = ";"))
data2 <- data.frame(read.delim("ExperimentalData/pointing_2.csv", sep = ";"))
data3 <- data.frame(read.delim("ExperimentalData/pointing_3.csv", sep = ";"))
data4 <- data.frame(read.delim("ExperimentalData/pointing_4.csv", sep = ";"))
data5 <- data.frame(read.delim("ExperimentalData/pointing_5.csv", sep = ";"))
data6 <- data.frame(read.delim("ExperimentalData/pointing_6.csv", sep = ";"))
data7 <- data.frame(read.delim("ExperimentalData/pointing_7.csv", sep = ";"))
data8 <- data.frame(read.delim("ExperimentalData/pointing_8.csv", sep = ";"))
data9 <- data.frame(read.delim("ExperimentalData/pointing_9.csv", sep = ";"))
data10 <- data.frame(read.delim("ExperimentalData/pointing_10.csv", sep = ";"))
data11 <- data.frame(read.delim("ExperimentalData/pointing_11.csv", sep = ";"))
data11$Part <- rep(11,nrow(data11))
data12 <- data.frame(read.delim("ExperimentalData/pointing_12.csv", sep = ";"))
data12$Part <- rep(12,nrow(data12))
data13 <- data.frame(read.delim("ExperimentalData/pointing_13.csv", sep = ";"))
data13$Part <- rep(13,nrow(data13))
data14 <- data.frame(read.delim("ExperimentalData/pointing_14.csv", sep = ";"))
data14$Part <- rep(14,nrow(data14))
data15 <- data.frame(read.delim("ExperimentalData/pointing_15.csv", sep = ";"))
data15$Part <- rep(15,nrow(data15))
data16 <- data.frame(read.delim("ExperimentalData/pointing_16.csv", sep = ";"))
data16$Part <- rep(16,nrow(data16))
data17 <- data.frame(read.delim("ExperimentalData/pointing_17.csv", sep = ";"))
data17$Part <- rep(17,nrow(data17))
data_raw <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16, data17)

rm(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16, data17)

# Keep only the columns of interest and give them more readable names
data <- data_raw %>% 
  select(
    Participant = Part,
    Condition = Cond,
    TrialNumber = Rep,
    Block,
    TargetNumber = Click,
    TargetIsHit = Succ,
    TargetHighlighted = Tar_t,
    TargetClicked = Click_t,
    PosX,
    PosY,
    Distance = Dist
  )

# Remove all Demo Trial from the data set
data <- data %>%
  filter(TrialNumber != "demo")

data$Participant <- as.factor(data$Participant)
data$Block <- as.factor(data$Block)

# Renaming of cell values in a more meaningful way
data$Condition[data$Condition == '0'] <- "Regular Edge"
data$Condition[data$Condition == '1'] <- "Virtual Edge"
data$Condition <- as.factor(data$Condition)

options(digits.secs = 3) # Set accuracy to one millisecond
# Convert the timestamps into a time format
data$TargetHighlighted <- as.POSIXct(data$TargetHighlighted, format = "%H:%M:%OS")
data$TargetClicked <- as.POSIXct(data$TargetClicked, format = "%H:%M:%OS")

dat_error <- data[data$Block_start > data$Block_end, ]

# If the target click lays before the target highlight, we look which of the target click value or the target highlight value is odd and remove this one. By odd, we mean that the difference with the next value in the same column is negative (for both the lagged and lead value).
data$TargetClicked[data$TargetClicked < data$TargetHighlighted & (data$TargetClicked - lag(data$TargetClicked) < 0 | lead(data$TargetClicked) - data$TargetClicked < 0)] <- NA

data$TargetHighlighted[data$TargetClicked < data$TargetHighlighted & (lead(data$TargetHighlighted) - data$TargetHighlighted < 0 | data$TargetHighlighted - lag(data$TargetHighlighted) < 0)] <- NA

# Add Columns with the start time and the end time of a block
data <- 
  data %>%
  mutate(
    Block_start = case_when(
      TargetNumber == 1 ~ TargetHighlighted, 
      TargetNumber == 2 ~ lag(TargetHighlighted, 1), 
      TargetNumber == 3 ~ lag(TargetHighlighted, 2), 
      TargetNumber == 4 ~ lag(TargetHighlighted, 3),
      TargetNumber == 5 ~ lag(TargetHighlighted, 4))) %>%
  mutate(
    Block_end = case_when(
      TargetNumber == 1 ~ lead(TargetClicked, 4), 
      TargetNumber == 2 ~ lead(TargetClicked, 3), 
      TargetNumber == 3 ~ lead(TargetClicked, 2), 
      TargetNumber == 4 ~ case_when(
        lead(TargetNumber) == 5 ~ lead(TargetClicked, 1), 
        lead(TargetNumber) != 5 ~ TargetClicked), 
      TargetNumber == 5 ~ TargetClicked)) %>%
  select (Participant, Condition, TrialNumber, Block, TargetNumber, Block_start, Block_end, TargetIsHit, TargetHighlighted, TargetClicked, PosX, PosY, Distance)

# If the block end lays after the block start, we set block end and target click time to NA (should not be the case after the previous steps)
data$Block_end[data$Block_start > data$Block_end] <- NA
data$TargetClicked[data$Block_start > data$Block_end] <- NA

# If the time of the target highlight or the target click lays outside of the block time window, the value is turned to NA
data$TargetHighlighted[data$TargetHighlighted < data$Block_start | data$TargetHighlighted > data$Block_end] <- NA
data$TargetClicked[data$TargetClicked < data$Block_start | data$TargetClicked > data$Block_end] <- NA
data$TargetClicked[data$TargetClicked - data$TargetHighlighted > 20] <- NA
