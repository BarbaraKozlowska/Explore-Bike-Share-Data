
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# Calculate the average trip duration for each user type in each city
avg_duration_ny <- ny %>%
  group_by(User.Type) %>%
  summarise(Avg_Trip_Duration = mean(Trip.Duration))

avg_duration_wash <- wash %>%
  group_by(User.Type) %>%
  summarise(Avg_Trip_Duration = mean(Trip.Duration))

avg_duration_chi <- chi %>%
  group_by(User.Type) %>%
  summarise(Avg_Trip_Duration = mean(Trip.Duration))

# Print the results
cat("Average Trip Duration for User Types in New York:\n")
print(avg_duration_ny)

cat("\nAverage Trip Duration for User Types in Washington:\n")
print(avg_duration_wash)

cat("\nAverage Trip Duration for User Types in Chicago:\n")
print(avg_duration_chi)


# Load required libraries
library(dplyr)
library(ggplot2)

# Create data frames for average trip duration
avg_duration_ny <- data.frame(User.Type = c("Customer", "Subscriber"),
                               Avg_Trip_Duration = c(2193, 755))

avg_duration_wash <- data.frame(User.Type = c("Customer", "Subscriber"),
                                 Avg_Trip_Duration = c(2634, 733))

avg_duration_chi <- data.frame(User.Type = c("Customer", "Subscriber"),
                                Avg_Trip_Duration = c(1930, 685))

# Create bar plots
ny_plot <- ggplot(avg_duration_ny, aes(x = User.Type, y = Avg_Trip_Duration, fill = User.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Trip Duration for User Types in New York",
       x = "User Type",
       y = "Average Trip Duration (seconds)") +
  theme_minimal()

wash_plot <- ggplot(avg_duration_wash, aes(x = User.Type, y = Avg_Trip_Duration, fill = User.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Trip Duration for User Types in Washington",
       x = "User Type",
       y = "Average Trip Duration (seconds)") +
  theme_minimal()

chi_plot <- ggplot(avg_duration_chi, aes(x = User.Type, y = Avg_Trip_Duration, fill = User.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Trip Duration for User Types in Chicago",
       x = "User Type",
       y = "Average Trip Duration (seconds)") +
  theme_minimal()

# Display the plots
ny_plot
wash_plot
chi_plot


# Load required library
library(dplyr)

# Find the most common start station for each city
common_start_ny <- ny %>%
  group_by(Start.Station) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(1)

common_start_wash <- wash %>%
  group_by(Start.Station) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(1)

common_start_chi <- chi %>%
  group_by(Start.Station) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(1)

# Print the results
cat("Most Common Start Station in New York:", common_start_ny$Start.Station, "\n")
cat("Most Common Start Station in Washington:", common_start_wash$Start.Station, "\n")
cat("Most Common Start Station in Chicago:", common_start_chi$Start.Station, "\n")


# Load required libraries
library(dplyr)
library(ggplot2)

# Create data frames for most common start stations
common_start_ny <- data.frame(Start.Station = c("Station 467"), Count = c(467))
common_start_wash <- data.frame(Start.Station = c("Station 210"), Count = c(210))
common_start_chi <- data.frame(Start.Station = c("Station 422"), Count = c(422))



# Calculate the distribution of user types for each city
user_dist_ny <- table(ny$User.Type) / nrow(ny) * 100
user_dist_wash <- table(wash$User.Type) / nrow(wash) * 100
user_dist_chi <- table(chi$User.Type) / nrow(chi) * 100

# Print the results
cat("User Type Distribution in New York:\n")
print(user_dist_ny)
cat("\nUser Type Distribution in Washington:\n")
print(user_dist_wash)
cat("\nUser Type Distribution in Chicago:\n")
print(user_dist_chi)


# Load required libraries
library(ggplot2)

# Create data frames for user type distribution
user_dist_ny <- data.frame(User.Type = c("Customer", "Subscriber"),
                            Percentage = c(0.2172722, 89.6348366))

user_dist_wash <- data.frame(User.Type = c("Customer", "Subscriber"),
                              Percentage = c(0.001122952, 73.665652267))

user_dist_chi <- data.frame(User.Type = c("Customer", "Subscriber"),
                             Percentage = c(0.01158749, 79.75666280))

# Create pie charts
ny_plot <- ggplot(user_dist_ny, aes(x = "", y = Percentage, fill = User.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "User Type Distribution in New York") +
  theme_void()

wash_plot <- ggplot(user_dist_wash, aes(x = "", y = Percentage, fill = User.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "User Type Distribution in Washington") +
  theme_void()

chi_plot <- ggplot(user_dist_chi, aes(x = "", y = Percentage, fill = User.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "User Type Distribution in Chicago") +
  theme_void()

# Display the pie charts
ny_plot
wash_plot
chi_plot

