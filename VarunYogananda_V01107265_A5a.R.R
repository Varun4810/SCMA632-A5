# Load necessary libraries
library(dplyr)

data <- read.csv("C:\\Users\\varuny\\Desktop\\Assignments\\SCMA 632\\NSSO68.csv")

# Filter data for Assam
assam_data <- filter(data, state_1 == 'ASSM')

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)

# Subsetting the data
ASSM <- assam_data %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
ASSM$Meals_At_Home <- impute_with_mean(ASSM$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  ASSM<- remove_outliers(ASSM, col)
}

# Summarize consumption
ASSM$total_consumption <- rowSums(ASSM[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- ASSM %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Kokrajhar Assam", "2" = "Dhubri Assam", "3" = "Goalpara Assam", "4" = "Bongaigaon Assam", "5" = "Barpeta Assam", "6" = "Kamrup Assam", "7" = "Nalbari Assam", "8" = "Darrang Assam", "9" = "Marigaon Assam", "10" = "Nagaon Assam", "11" = "Sonitpur Assam", "12" = "Lakhimpur Assam", "13" = "Dhemaji Assam", "14" = "Tinsukia Assam", "15" = "Dibrugarh Assam", "16" = "Sibsagar Assam", "17" = "Jorhat Assam", "18" = "Golaghat Assam", "19" = "Karbi Anglong Assam", "20" = "North Cachar Hills Assam", "21" = "Cachar Assam", "22" = "Karimganj Assam", "23" = "Hailakandi Assam", "24" = "Chirag Assam", "25" = "Baksa Assam", "26" = "Guwahati Assam", "27" = "Udalgur Assam")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

ASSM$District <- as.character(ASSM$District)
ASSM$Sector <- as.character(ASSM$Sector)
ASSM$District <- ifelse(ASSM$District %in% names(district_mapping), district_mapping[ASSM$District], ASSM$District)
ASSM$Sector <- ifelse(ASSM$Sector %in% names(sector_mapping), sector_mapping[ASSM$Sector], ASSM$Sector)

View(ASSM)

hist(ASSM$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Assam State")

ASSM_consumption <- aggregate(total_consumption ~ District, data = ASSM, sum) 
View(ASSM_consumption)

barplot(ASSM_consumption$total_consumption, 
        names.arg = ASSM_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) 

# b) Plot on the Assam state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C:\\Users\\varuny\\Desktop\\ASSAM_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 

# Merge ASSM_consumption with data_map
data_map_data <- data_map %>%
  left_join(ASSM_consumption, by = c("District" = "District"))

library(sf)

# Reproject data_map to UTM zone 45N
data_map_utm <- st_transform(data_map, crs = 32645)

# Merge ASSM_consumption with data_map_utm
data_map_data_utm <- data_map_utm %>%
  left_join(ASSM_consumption, by = c("District" = "District"))

# Plot the data
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
