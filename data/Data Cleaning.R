library(tidyverse)

# List to store datasets and removed column names
all_datasets <- list()

common_columns <- NULL

# initiallize our cols
# file_name <- paste0("data/analytic_data2010.csv")
# column_names <- read.csv(file_name, nrows = 1, header = FALSE)

# Loop through years from 2014 to 2023
for (year in 2014:2023) {
  # Construct the file name based on the year
  file_name <- paste0("data/analytic_data", year, ".csv")
  
  # Import the data for the current year
  data <- read.csv(file_name, skip = 2, header = FALSE,  stringsAsFactors = FALSE)
  headers <- read.csv(file_name, skip=1, nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  colnames(data) <- headers
  
  # common columns
  if (is.null(common_columns)) {
    common_columns <- colnames(data)
  } else {
    # For subsequent datasets, identify the common columns
    common_columns <- intersect(common_columns, colnames(data))
  }

  # add "year" column to dataset
  data$year <- year
  
  # add the years dataset to the list
  all_datasets[[as.character(year)]] <- data
  
  
  # column names
  column_names[[year]] <- colnames(data)
  
  
  print(year)
}

filtered_datasets <- list()

for (year in 2014:2023) {
  # Retrieve the dataset for the year
  yearly_data <- all_datasets[[as.character(year)]]
  
  if (!is.null(yearly_data)) {
    # Select only the columns that are common across all years
    yearly_data <- yearly_data[, common_columns, drop = FALSE]
    
    # Store the filtered dataset
    filtered_datasets[[as.character(year)]] <- yearly_data
  }
}

# Combine all yearly datasets into one
combined_dataset <- do.call(rbind, filtered_datasets)

summary(combined_dataset)

View(combined_dataset)

data <- combined_dataset
ncol(data) # 98 columns
nrow(data) # 41516 rows

# now we want to see the missing values

missing_counts <- colSums(is.na(data))
print(sort(missing_counts/nrow(data)))


# pulling out the national and state rows
states <- read_csv("data/states.csv")
states[nrow(states) + 1,] = list("United States","US")

county_data = data[!(data$county %in% states$State),]

# removes columns with no values
data <- county_data[colSums(!is.na(county_data)) > 0]

ncol(data)

# replace missing values with column mean
# compute each column's mean using mean() function 

# Get the unique years in the dataset
years <- unique(data$year)

# Get the unique states in the dataset
states <- unique(data$state)

for (i in 8:ncol(data)) {
  
  # Check if there are any NA values in the column
  if (any(is.na(data[, i]))) {
    
    # Compute the mean of the entire column (excluding NAs)
    overall_mean <- mean(data[, i], na.rm = TRUE)
    
    
    for (year in years) {
      for (state in states) {
        
        # Compute the mean for the current state and year (excluding NAs)
        state_year_mean <- mean(data[data$year == year & data$state == state, i], na.rm = TRUE)
        
        # If state_year_mean is NA, then use the mean of the entire year
        if (is.na(state_year_mean)) {
          state_year_mean <- mean(data[data$year == year, i], na.rm = TRUE)
          
          # If mean for the year is also NA, use the overall_mean
          if (is.na(state_year_mean)) {
            state_year_mean <- overall_mean
          }
        }
        
        # Replace NA values for the current state and year with the computed mean
        data[data$year == year & data$state == state & is.na(data[, i]), i] <- state_year_mean
        
      }
    }
  }
}

ncol(data)
nrow(data)

colnames(data)
View(data)

rows_with_na <- data[apply(is.na(data), 1, any),]
rows_with_na

# county ranked is missing for 7 rows so we fill in with 0
data$county_ranked[is.na(data$county_ranked)] <- 0

data <- data[!is.na(data$fipscode), ]

View(data)

# now all our data is complete, with no missing values

write.csv(data, "final_dataset_14_23.csv", row.names = FALSE)
