# March 19, 2024- arshia 
require(dplyr)
require(stringr)
require(ggplot2)
require(lubridate)

# read data
#=================================================================================================================================
# SPES Data
transect_SPES <- read.csv("data/SPES/Transect_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad1m_SPES <- read.csv("data/SPES/1m_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m_SPES <- read.csv("data/SPES/0.25m_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet_SPES <- read.csv("data/SPES/Limpet_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))

# ENVR 400 2024 Data
transect_ENVR_2024 <- read.csv("data/ENVR_2024/transect_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m_ENVR_2024 <- read.csv("data/ENVR_2024/0.25m_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet_ENVR_2024 <- read.csv("data/ENVR_2024/Limpet_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))

# ENVR 400 2023 Data - their data is crazy idk if its worth it lmao
#ENVR_2023 <- read.csv("data/******.csv", check.names = FALSE, na.strings=c("N/A", ""))

# Monthly Algae Survey Data
#Algae_Data <- read.csv("data/******.csv", check.names = FALSE, na.strings=c("N/A", ""))

# Abiotic Data
weather <- read.csv("data/Abiotic/UBC_Rooftop_obs_2019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
tide <- read.csv("data/Abiotic/Tide_Jan012019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
#=================================================================================================================================

# clean data
#=================================================================================================================================
# Functions
#=================================================================================================================================
# function to add a year column - for ease of yearly analysis
add_year_column <- function(data, date_column_name) {
  data[[date_column_name]] <- format(as.Date(data[[date_column_name]]), "%d/%m/%Y") # format original date column to be in day/month/year
  data$Year <- format(as.Date(data[[date_column_name]], "%d/%m/%Y"), "%Y") # from formatted date column, pulls out year
  return(data)
}

# change 0 and 1 values to true and false/presence and absence
change_to_logical <- function(df, start_col_index, end_col_index) {
  df <- df %>%
    mutate(across(start_col_index:end_col_index, as.logical)) # change the columns inbetween the start and end to logical or true/false
  return(df)
}

# get the season and month
get_season <- function(date){
  date <- as.Date(date) # make sure the input has date format
  mon <- months.Date(date, abbreviate = TRUE) # get month abbreviation
  
  # define winter (Dec, Jan, Feb), spring (Mar, Apr, May),
  # summer (Jun, Jul, Aug), fall (Sep, Oct, Nov)
  ifelse(mon %in% c("Dec", "Jan", "Feb"), "Winter",
         ifelse(mon %in% c("Mar", "Apr", "May"), "Spring",
                ifelse(mon %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
}
#=================================================================================================================================

# SPES Data
#=================================================================================================================================
# add year column
transect_SPES <- add_year_column(transect_SPES, "Date")
quad1m_SPES <- add_year_column(quad1m_SPES, "Date")
quad0.25m_SPES <- add_year_column(quad0.25m_SPES, "Date")
limpet_SPES <- add_year_column(limpet_SPES, "Date")

# change 0 and 1 values to true and false/presence and absence
quad0.25m_SPES <- change_to_logical(quad0.25m_SPES, 14, 44)
transect_SPES <- change_to_logical(transect_SPES, 12, 14)

# get the season and month
transect_SPES$season <- get_season(transect_SPES$Date)
transect_SPES$month <- month(as.Date(transect_SPES$Date))

quad1m_SPES$season <- get_season(quad1m_SPES$Date)
quad1m_SPES$month <- month(as.Date(quad1m_SPES$Date))

quad0.25m_SPES$season <- get_season(quad0.25m_SPES$Date)
quad0.25m_SPES$month <- month(as.Date(quad0.25m_SPES$Date))

limpet_SPES$season <- get_season(limpet_SPES$Date)
limpet_SPES$month <- month(as.Date(limpet_SPES$Date))

# select specific limpet data
select_limpet_SPES <- data.frame(site_TA = limpet_SPES$Site_TA,
                                 year = limpet_SPES$Year,
                                 length = limpet_SPES[,"Mean_Length_mm"],
                                 width = limpet_SPES[,"Mean_Width_mm"],
                                 month = limpet_SPES$month)
select_limpet_SPES$site_TA <- as.character(select_limpet_SPES$site_TA)
#=================================================================================================================================

# ENVR 400 2024 Data
#=================================================================================================================================
# add year column - figure out how to change the column name to remove ymd now - aesthetic worry later
transect_ENVR_2024 <- add_year_column(transect_ENVR_2024, "Date_ymd")
quad0.25m_ENVR_2024 <- add_year_column(quad0.25m_ENVR_2024, "Date_ymd")
limpet_ENVR_2024 <- add_year_column(limpet_ENVR_2024, "Date_ymd")

# change presence/absence columns to logical - note if any other columns are added have to change range of columns
transect_ENVR_2024 <- change_to_logical(transect_ENVR_2024, 14, 16)

quad0.25m_ENVR_2024 <- change_to_logical(quad0.25m_ENVR_2024, 15, 26)
quad0.25m_ENVR_2024 <- change_to_logical(quad0.25m_ENVR_2024, 29, 36)
quad0.25m_ENVR_2024 <- change_to_logical(quad0.25m_ENVR_2024, 39, 44)
quad0.25m_ENVR_2024 <- change_to_logical(quad0.25m_ENVR_2024, 47, 54)
quad0.25m_ENVR_2024 <- change_to_logical(quad0.25m_ENVR_2024, 57, 62)

# get season and month
transect_ENVR_2024$season <- get_season(transect_ENVR_2024$Date_ymd)
transect_ENVR_2024$month <- month(as.Date(transect_ENVR_2024$Date_ymd))

quad0.25m_ENVR_2024$season <- get_season(quad0.25m_ENVR_2024$Date)
quad0.25m_ENVR_2024$month <- month(as.Date(quad0.25m_ENVR_2024$Date))

limpet_ENVR_2024$season <- get_season(limpet_ENVR_2024$Date)
limpet_ENVR_2024$month <- month(as.Date(limpet_ENVR_2024$Date))

#=================================================================================================================================

# Abiotic Data
#=================================================================================================================================
# Separate out tide data date and time
tide <- tide %>%
  mutate(date = as.Date(Obs_date),
         time = format(strptime(Obs_date, format = "%Y-%m-%d %H:%M"), "%H:%M"))
tide <- add_year_column(tide, "date")

# Separate out the hour from the tide time
tide$time <- as.POSIXct(tide$time, format = "%H:%M")
tide$hour <- format(tide$time, "%H")
tide$hour <- as.numeric(tide$hour)

# seasonal and monthly tide
tide$season <- get_season(tide$date)
tide$month <- month(as.Date(tide$date))

# Format weather data date
weather$date <- ymd(weather$Dates)
weather <- add_year_column(weather, "date")

weather$season <- get_season(weather$date)
weather$month <- month(as.Date(weather$date))
#=================================================================================================================================

# Historical SPES Data plotting
#=================================================================================================================================
# Functions 
#=================================================================================================================================
# plot variable per TA, averaged per site visit
plot_var_per_TA <- function(varname, plot_varname, data){
  
  # create data frame with relevant columns
  df_var <- data.frame(site_TA = data$Site_TA,
                       year = data$Year,
                       var = data[,varname])
  df_var$site_TA <- as.character(df_var$site_TA) 
  
  # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
  df_var <- aggregate(var ~ year + site_TA, data=df_var, FUN=mean)
  
  ggplot(data = df_var, aes(x = year, y = var, group = site_TA, fill = site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab(plot_varname) + xlab("Time (years)") + labs(fill = "Site TA")
  
}

# Identity of Sea Star - excludes all NA values, need to figure out how to have different ones depending on what is avaliable for each site
logical_plot <- function(data, species_column) {
  ggplot(data, aes_string(x = "Year", y = "Site_TA", fill = species_column), na.rm = TRUE) +
    geom_tile(colour ='black') +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +  # Adjust colors as needed
    labs(x = "Year", y = "Sampling Site", fill = paste0(species_column, " Presence")) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
    ) +
    scale_y_continuous(breaks = unique(data$Site_TA))  # Set breaks for y-axis
}

#=================================================================================================================================

# Sea Star (transect) data
#=================================================================================================================================
# Density of sea stars per TA (2019-2023)
SS_density_TA <- plot_var_per_TA("Density_of_Sea_Stars_Count", "Mean count of sea stars", transect_SPES)
SS_density_TA

# Presence absence of sea stars 
plot_ochre <- logical_plot(transect_SPES, "Ochre")
plot_ochre
plot_leather <- logical_plot(transect_SPES, "Leather")
plot_leather
plot_molted <- logical_plot(transect_SPES, "Molted")
plot_molted 
#=================================================================================================================================

# Limpet Data
#=================================================================================================================================
# aggregate by year and site (mean per visit)
# Length
limp_agg_length_SPES <- aggregate(length ~ year + site_TA, data=select_limpet_SPES, FUN=mean)
ggplot(data = limp_agg_length_SPES, aes(x = year, y = length, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet length (mm)") + xlab("Time (years)") + labs(fill = "Site TA")

# Width
limp_agg_width_SPES <- aggregate(width ~ year + site_TA, data=select_limpet_SPES, FUN=mean)
ggplot(data = limp_agg_width_SPES, aes(x = year, y = width, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet width (mm)") + xlab("Time (years)") + labs(fill = "Site TA")

# aggregate by month and site (mean per visit) -  Not liking the seasonality - incorporate once we get our data sorted
df_limp_agg <- aggregate(width ~ month + site_TA, data=select_limpet_SPES, FUN=mean)
ggplot(data = df_limp_agg, aes(x = month, y = width, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  ylab("Mean limpet length (mm)") + xlab("Time (months)") + labs(fill = "Site TA")
#=================================================================================================================================

# 1m quadrat data
#=================================================================================================================================
# Littorine Snails
plot_var_per_TA("Littorine_snails", "Mean count of littorine snails per field visit", quad1m_SPES)

# Nucella Snails - not great, not 100% needed
plot_var_per_TA("Nucella_snails", "Mean count of nucella snails per field visit", quad1m_SPES)

# Limpet Count
plot_var_per_TA("Limpets", "Mean count of limpets per field visit", quad1m_SPES)
#=================================================================================================================================


# 0.25m quadrat data
#=================================================================================================================================
# plot variable per TA, averaged per site visit

# Define a function to create a multi-colored bar graph
#plot_cover_per_0.25_quadrant <- function(quad0.25m) {

#data_clean <- quad0.25m %>%
#filter(!is.na('Total_Cover_%') & !is.na('Algae_%_cover') & !is.na('Sessile_Invertebrates_%_cover'))



#data_summary <- data_clean %>%
#group_by(Site_TA) %>%
#summarize(Total_Cover = mean('Total_Cover_%'),
# Algae_percent = mean('Algae_%_cover'),
# Invertebrates_percent = mean('Sessile_Invertebrates_%_Cover'))
#print(data_summary)

# Create the bar graph
#ggplot(data_summary, aes(x = Site_TA)) +
# geom_bar(aes(y = Algae_percent, fill = "Algae"), stat = "identity", position = "stack") +
#geom_bar(aes(y = Invertebrates_percent, fill = "Invertebrates"), stat = "identity", position = "stack") +
# scale_fill_manual(values = c("Algae" = "green", "Invertebrates" = "blue")) +
# labs(x = "Site", y = "Percentage of Total Cover", fill = "Cover Type") +
#theme_minimal()
#}

# Assuming your data frame is named 'data' with columns: Site_TA, Total_Cover_%, Algae_%_cover, Sessile_Invertebrates_%_cover
plot_cover_per_0.25_quadrant(quad0.25m)


plot_cover_per_0.25_quadrant <- function(varname, plot_varname, data){
  
  # create data frame with relevant columns
  df_var <- data.frame(site_TA = data$Site_TA,
                       year = data$Year,
                       var = data[,varname])
  df_var$site_TA <- as.character(df_var$site_TA) 
  
  # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
  df_var <- aggregate(var ~ year + site_TA, data=df_var, FUN=mean)
  
  ggplot(data = df_var, aes(x = year, y = var, group = site_TA, fill = site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab(plot_varname) + xlab("Time (years)") + labs(fill = "Site TA")
  
}


#------
install.packages("reshape2")
plot_cover_components <- function(quad0.25m) {
  # Calculate percentage of total cover comprised of algae and invertebrates - need to clean but its at 100
  quad0.25m$Algae_percent <- quad0.25m$'Algae_%_cover' * 1
  quad0.25m$Invertebrates_percent <- quad0.25m$'Sessile_Invertebrates_%_Cover' * 1
  
  # Aggregate data by year and site_TA
  data_agg <- aggregate(cbind(Algae_percent, Invertebrates_percent ) ~ Year + Site_TA, data = quad0.25m, FUN = mean, na.action = na.omit)
  
  # Reshape data for plotting
  data_plot <- reshape2::melt(data_agg, id.vars = c("Year", "Site_TA"))
  
  # Create the multi-colored bar graph
  ggplot(data = data_plot, aes(x = Year, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Year", y = "Percentage of Total Cover", fill = "Cover Component") +
    facet_wrap(~ Site_TA) +
    theme_minimal()
}

# Call the function to create the multi-colored bar graph
plot_cover_components(quad0.25m)
#=================================================================================================================================

# ENVR 400 2024 Data Analysis
#=================================================================================================================================

mean_counts_SS_2024 <- aggregate(Density_of_Sea_Stars_count~ Site_TA, data = transect_ENVR_2024, FUN = mean)

# Now, create the bar plot using ggplot2 - need to figure out 
ggplot(mean_counts_SS_2024, aes(x = Site_TA, y = Density_of_Sea_Stars_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sites", y = "Mean Count of Sea Stars") +
  ggtitle("Mean Count of Sea Stars by Site")

#=================================================================================================================================
# Functions 
#=================================================================================================================================
# plot variable per TA, averaged per site visit
plot_var_per_TA_400 <- function(varname, plot_varname, data){
  
  # create data frame with relevant columns
  df_var <- data.frame(site_TA = data$Site_TA,
                       var = data[,varname])
  df_var$site_TA <- as.character(df_var$site_TA) 
  
  # aggregate to the mean per Site_TA
  df_var <- aggregate(var ~ site_TA, data=df_var, FUN=mean)
  
  ggplot(data = df_var, aes(x = site_TA, y = var, fill = site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab(plot_varname) + xlab("Sampling Site") + labs(fill = "Site TA") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
}


# Identity of Sea Star --> Looks wrong
logical_plot_400 <- function(data, species_column) {
  ggplot(data, aes_string(x = "Site_TA", y = species_column, fill = species_column), na.rm = TRUE) +
    geom_tile(colour ='black') +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +  # Adjust colors as needed
    labs(x = "Sampling Site", y = paste0(species_column, " Presence"), fill = paste0(species_column, " Presence")) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Adjust x-axis text alignment
    ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10) 
  ) +
  scale_y_continuous(breaks = unique(data$Site_TA)) # Wrap x-axis labels for better readability
}


#=================================================================================================================================

# Sea Star (transect) data
#=================================================================================================================================
# Density of sea stars per TA (2019-2023)
SS_density_TA_400 <- plot_var_per_TA_400("Density_of_Sea_Stars_count", "Count of sea stars", transect_ENVR_2024)
SS_density_TA_400

oyster_density_TA_400 <- plot_var_per_TA_400("Density_of_Oysters_count", "Count of Oysters", transect_ENVR_2024)
oyster_density_TA_400


# Presence absence of sea stars 

plot_ochre_400 <- logical_plot(transect_ENVR_2024, "Ochre_EO")
plot_ochre_400

plot_leather_400 <- logical_plot(transect_ENVR_2024, "Leather_EL")
plot_leather_400

plot_molted_400 <- logical_plot(transect_ENVR_2024, "Mottled_EM")
plot_molted_400

# Limpet Data
#=================================================================================================================================
# aggregate by year and site (mean per visit)
# Length - how to add site TA to x axis 
limp_agg_length_400 <- aggregate(Mean_Length_mm ~ Site_TA, data = limpet_ENVR_2024, FUN = mean)
ggplot(data = limp_agg_length_400, aes(x = Site_TA, y = Mean_Length_mm, fill = Site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet length (mm)") + xlab("Sampling Site") + labs(fill = "Site TA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust x-axis text alignment
  ) 

 
# Width
limp_agg_width_400 <- aggregate(Mean_Width_mm ~ Site_TA, data = limpet_ENVR_2024, FUN = mean)
ggplot(data = limp_agg_width_400, aes(x = Site_TA, y = Mean_Width_mm, fill = Site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet width (mm)") + xlab("Sampling Site") + labs(fill = "Site TA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust x-axis text alignment
  ) 

# aggregate by month and site (mean per visit) -  Not liking the seasonality - incorporate once we get our data sorted
#MONTHS being weird 
df_limp_agg_400 <- aggregate(Mean_Width_mm ~ month + Site_TA, data= limpet_ENVR_2024, FUN=mean)
ggplot(data = df_limp_agg_400, aes(x = month, y = Mean_Width_mm, group = Site_TA, fill = Site_TA)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  ylab("Mean limpet length (mm)") + xlab("Time (months)") + labs(fill = "Site TA")
#=================================================================================================================================

# don't know what to do 
#=================================================================================================================================
# Littorine Snails
#plot_var_per_TA("Littorine_snails", "Mean count of littorine snails per field visit", quad1m_SPES)

# Nucella Snails - not great, not 100% needed
#plot_var_per_TA("Nucella_snails", "Mean count of nucella snails per field visit", quad1m_SPES)

# Limpet Count
#plot_var_per_TA("Limpets", "Mean count of limpets per field visit", quad1m_SPES)
#=================================================================================================================================

#percent
install.packages("reshape2")
library(ggplot2)
library(reshape2)



plot_cover_components <- function(quad0.25m_ENVR_2024) {
  # Aggregate data by Site_TA
  data_agg <- aggregate(cbind(Algae_percent = Percent_Cover_Algae, Invertebrates_percent = Percent_Cover_Invertebrates) ~ Site_TA, data = quad0.25m_ENVR_2024, FUN = mean, na.action = na.omit)
  
  # Reshape data for plotting
  data_plot <- reshape2::melt(data_agg, id.vars = "Site_TA")
  
  # Create the multi-colored bar graph
  ggplot(data = data_plot, aes(x = Site_TA, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Site_TA", y = "Percentage of Total Cover", fill = "Cover Component") +
    theme_minimal()
}


# Call the function to create the multi-colored bar graph
plot_cover_components(quad0.25m_ENVR_2024)

#=================================================================================================================================

# Abiotic Analysis
#=================================================================================================================================
# Find average time of minimum tide height per month 
# MAKE FUNCTION???
# Minimum tide height
# Find the minimum tide height for each month for each year
monthly_min_tide <- tide %>%
  group_by(month, Year) %>%
  summarise(Min_Tide_Height = min(SLEV_metres))
# average the minimum height for each month 
average_monthly_min_tide <- monthly_min_tide %>%
  group_by(month) %>%
  summarise(Avg_Min_Tide_Height = mean(Min_Tide_Height))

# Time of minimum tide height 
monthly_min_tides_time <- tide %>%
  group_by(month, Year) %>%
  summarise(min_tide_height = min(SLEV_metres),
            hour_of_min_tide = hour[which.min(SLEV_metres)])
average_monthly_min_tide_time <- monthly_min_tides_time %>%
  group_by(month) %>%
  summarise(Avg_Min_Tide_Height = mean(min_tide_height),
            mean_hour_of_min_tide = mean(hour_of_min_tide))

# Min temperature 
monthly_min_temperature <- weather %>%
  group_by(month, Year) %>%
  summarise(Min_Temp = min(AirTemp_degC))

average_monthly_min_temperature <- monthly_min_temperature %>%
  group_by(month) %>%
  summarise(Avg_Min_Temp = mean(Min_Temp))

# Max temperature
monthly_max_temperature <- weather %>%
  group_by(month, Year) %>%
  summarise(Max_Temp = max(AirTemp_degC))

average_monthly_max_temperature <- monthly_max_temperature %>%
  group_by(month) %>%
  summarise(Avg_Max_Temp = mean(Max_Temp))

# merge the abotic dfs by month 
monthly_temperature_data <- merge(average_monthly_max_temperature, average_monthly_min_temperature, by = "month")
monthly_abiotic_data <- merge(monthly_temperature_data, average_monthly_min_tide_time, by = "month")


# Plotting - minimum tide height - need to figure out temperature scale
monthly_abiotic_data$month <- factor(monthly_abiotic_data$month, levels = 1:12,
                                     labels = c("January", "February", "March", "April", "May", "June",
                                                "July", "August", "September", "October", "November", "December"))

ggplot(monthly_abiotic_data, aes(x = month)) +
  geom_bar(aes(y = Avg_Min_Tide_Height), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = Avg_Max_Temp * (3/max(Avg_Max_Temp)), group = 1), color = "red") +
  geom_line(aes(y = Avg_Min_Temp * (3/max(Avg_Max_Temp)), group = 1), color = "blue") +
  scale_y_continuous(name = "Average height of low tide (m)", 
                     limits = c(0, 1.5),
                     sec.axis = sec_axis(~ . * (. + 4) / (max(monthly_abiotic_data$Avg_Max_Temp) + 4) * 35 - 4, name = "Temperature (°C)")) +
  labs(x = "Month", y = "Average height of low tide (m)", 
       title = "Average height of low tide and average maximum and minimum Temperature (2019-2023)",
       caption = "Data Source: Your Source") +
  theme_minimal()

# plotting - time of minimum tide height - need to figure out temperature scale 
ggplot(monthly_abiotic_data, aes(x = month)) +
  geom_bar(aes(y = mean_hour_of_min_tide), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = Avg_Max_Temp * (3/max(Avg_Max_Temp)), group = 1), color = "red") +
  geom_line(aes(y = Avg_Min_Temp * (3/max(Avg_Max_Temp)), group = 1), color = "blue") +
  scale_y_continuous(name = "Average time of low tide (hour)", 
                     limits = c(0, 23),
                     sec.axis = sec_axis(~ . * (. + 4) / (max(monthly_abiotic_data$Avg_Max_Temp) + 4) * 35 - 4, name = "Temperature (°C)")) +
  labs(x = "Month", y = "Average height of low tide (m)", 
       title = "Average height of low tide and average maximum and minimum Temperature (2019-2023)",
       caption = "Data Source: Your Source") +
  theme_minimal()
