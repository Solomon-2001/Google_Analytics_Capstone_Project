# -----------------------------
# Load Required Libraries
# -----------------------------
library(tidyverse)
library(lubridate)

# -----------------------------
# Import All 12 CSV Files
# -----------------------------
# Set the folder containing your 12 months of Divvy data
folder <- "/Users/solomonebrahimoff/Documents/raw_csv"

# List all CSV files in the folder
files <- list.files(folder, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSVs into one dataframe
trips <- files %>%
  map_df(read_csv)

# -----------------------------
# Data Cleaning & Preparation
# -----------------------------
trips <- trips %>%
  # Convert timestamps into proper datetime formats
  mutate(
    started_at   = ymd_hms(started_at),
    ended_at     = ymd_hms(ended_at),

    # Create ride length in minutes
    ride_length  = as.numeric(difftime(ended_at, started_at, units = "mins")),

    # Extract day of week and hour of day
    day_of_week  = wday(started_at, label = TRUE, abbr = FALSE),
    start_hour   = hour(started_at),

    # Standardise rider type
    member_casual = tolower(member_casual)
  ) %>%
  
  # Remove invalid rows
  filter(
    ride_length > 0,         # remove negative or zero durations
    !is.na(started_at),      # remove NA timestamps
    !is.na(ended_at)
  )

# -----------------------------
# Summary 1: By User Type
# -----------------------------
summary_by_user <- trips %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride = mean(ride_length),
    rides    = n()
  )

# Export summary to CSV
write_csv(summary_by_user, "summary_by_user.csv")

# -----------------------------
# Summary 2: By Weekday
# -----------------------------
summary_by_weekday <- trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    avg_ride = mean(ride_length),
    rides    = n(),
    .groups = "drop"
  )

write_csv(summary_by_weekday, "summary_by_weekday.csv")

# -----------------------------
# Summary 3: By Hour of Day
# -----------------------------
summary_by_hour <- trips %>%
  group_by(member_casual, start_hour) %>%
  summarise(
    rides = n(),
    .groups = "drop"
  )

write_csv(summary_by_hour, "summary_by_hour.csv")

# -----------------------------
# End of Script
# -----------------------------