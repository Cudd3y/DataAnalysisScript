# =============================================
# Pilot Study: Analysis of the collected Data
# =============================================

# =============================================
# Load Packages
pacman::p_load(
  # data processing
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
data$TargetIsHit[data$TargetIsHit == '0'] <- "FALSE"
data$TargetIsHit[data$TargetIsHit == '1'] <- "TRUE"








