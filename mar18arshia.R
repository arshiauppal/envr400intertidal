# March 18, 2024 - arshia 
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

# ENVR 400 2023 Data
ENVR_2023 <- read.csv("data/******.csv", check.names = FALSE, na.strings=c("N/A", ""))

# Monthly Algae Survey Data
Algae_Data <- read.csv("data/******.csv", check.names = FALSE, na.strings=c("N/A", ""))

# Abiotic Data
weather <- read.csv("data/Abiotic/UBC_Rooftop_obs_2019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
tide <- read.csv("data/Abiotic/Tide_Jan012019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))

# clean data
#=================================================================================================================================
# SPES Data
#=================================================================================================================================
# function to add a year column - for ease of yearly analysis
add_year_column <- function(data, date_column_name) {
  data[[date_column_name]] <- format(as.Date(data[[date_column_name]]), "%d/%m/%Y") # format original date column to be in day/month/year
  data$Year <- format(as.Date(data[[date_column_name]], "%d/%m/%Y"), "%Y") # from formatted date column, pulls out year
  return(data)
}

transect_SPES <- add_year_column(transect_SPES, "Date")
quad1m_SPES <- add_year_column(quad1m_SPES, "Date")
quad0.25m_SPES <- add_year_column(quad0.25m_SPES, "Date")
limpet_SPES <- add_year_column(limpet_SPES, "Date")

# change 0 and 1 values to true and false/presence and absence
change_to_logical <- function(df, start_col_index, end_col_index) {
  df <- df %>%
    mutate(across(start_col_index:end_col_index, as.logical)) # change the columns inbetween the start and end to logical or true/false
  return(df)
}

quad0.25m_SPES <- change_to_logical(quad0.25m_SPES, 14, 44)
transect_SPES <- change_to_logical(transect_SPES, 12, 14)

# get the season and month - CODE NOT WORKING ANYMORE
get_season <- function(date){
  
  date <- as.Date(date) # make sure the input has date format
  mon <- months.Date(date, abbreviate = TRUE) # get month abbreviation
  
  # define winter (Dec, Jan, Feb), spring (Mar, Apr, May),
  # summer (Jun, Jul, Aug), fall (Sep, Oct, Nov)
  ifelse(mon %in% c("Dec", "Jan", "Feb"), "Winter",
         ifelse(mon %in% c("Mar", "Apr", "May"), "Spring",
                ifelse(mon %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
}

transect_SPES$season <- get_season(transect_SPES$Date)
transect_SPES$month <- month(as.Date(transect_SPES$Date))

quad1m_SPES$season <- get_season(quad1m_SPES$Date)
quad1m_SPES$month <- month(as.Date(quad1m_SPES$Date))

quad0.25m_SPES$season <- get_season(quad0.25m_SPES$Date)
quad0.25m_SPES$month <- month(as.Date(quad0.25m_SPES$Date))

limpet_SPES$season <- get_season(limpet_SPES$Date)
limpet_SPES$month <- month(as.Date(limpet_SPES$Date))
#=================================================================================================================================

# ENVR 400 2024 Data
#=================================================================================================================================
# add year column - figure out how to change the column name to remove ymd now - aesthetic worry later
transect_ENVR_2024 <- add_year_column(transect_ENVR_2024, "Date_ymd")
quad0.25m_ENVR_2024 <- add_year_column(quad0.25m_ENVR_2024, "Date_ymd")
limpet_ENVR_2024 <- add_year_column(limpet_ENVR_2024, "Date_ymd")

# change columns to logical
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

# seasonal and monthly tide
tide$season <- get_season(tide$date)
tide$month <- month(as.Date(tide$date))

# Format weather data date
weather$date <- ymd(weather$Dates)
weather <- add_year_column(weather, "date")

weather$season <- get_season(weather$date)
weather$month <- month(as.Date(weather$date))
#=================================================================================================================================

# plot sea star
#=================================================================================================================================

#description - then describe the inputs

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

plot_var_per_TA("Density_of_Sea_Stars_Count", "Mean count of sea stars", transect)

# Identity of Sea Star - excludes all NA values, need to figure out how to have different ones depending on what is avaliable for each site
#not working 
logical_plot <- function(data, species_column1) {
  ggplot(data, aes_string(x = "Site_TA", y = species_column1, fill = species_column1), na.rm = TRUE) +
    geom_tile(colour ='black') +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), na.value= "grey") +  # Adjust colors as needed
    labs(x = "Sampling Site", y = paste0(species_column1, " Presence"), fill = paste0(species_column1, " Presence")) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
    )
}

# Create separate plots for each species using the function
plot_ochre_400 <- logical_plot(transect_ENVR_2024, "Ochre_EO")
plot_leather_400 <- logical_plot(transect_ENVR_2024, "Leather_EL")
plot_molted <- logical_plot(transect_ENVR_2024, "Molted_EM")

# Display the plots
plot_ochre_400
plot_leather_400
plot_molted_400 



# plot limpet data
#=================================================================================================================================
# create data frame with relevant columns
df_limp <- data.frame(site_TA = limpet$Site_TA,
                      year = limpet$Year,
                      length = limpet[,"Mean_Length_mm"],
                      width = limpet[,"Mean_Width_mm"],
                      month = limpet$month)
df_limp$site_TA <- as.character(df_limp$site_TA)

# aggregate by year and site (mean per visit)
# Length
df_limp_agg_length <- aggregate(length ~ year + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg_length, aes(x = year, y = length, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet length (mm)") + xlab("Time (years)") + labs(fill = "Site TA")

# Width
df_limp_agg_width <- aggregate(width ~ year + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg_width, aes(x = year, y = width, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet width (mm)") + xlab("Time (years)") + labs(fill = "Site TA")

# aggregate by month and site (mean per visit) - *NOT WORKING* * Not liking the seasonality - incorporate once we get our data sorted
df_limp_agg <- aggregate(width ~ month + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg, aes(x = month, y = width, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  ylab("Mean limpet length (mm)") + xlab("Time (months)") + labs(fill = "Site TA")

# plot 1m quadrat
#=================================================================================================================================

# Littorine Snails
plot_var_per_TA("Littorine_snails", "Mean count of littorine snails per field visit", quad1m)

# Nucella Snails - not great but ot 100% necessary
plot_var_per_TA("Nucella_snails", "Mean count of nucella snails per field visit", quad1m)

# Limpet Count
plot_var_per_TA("Limpets", "Mean count of limpets per field visit", quad1m)


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


# ENVR 400 2024 Data Analysis
#=================================================================================================================================
mean_counts_SS_2024 <- aggregate(Density_of_Sea_Stars_count~ Site_TA, data = transect_ENVR_2024, FUN = mean)

# Now, create the bar plot using ggplot2 - need to figure out 
ggplot(mean_counts_SS_2024, aes(x = Site_TA, y = Density_of_Sea_Stars_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sites", y = "Mean Count of Sea Stars") +
  ggtitle("Mean Count of Sea Stars by Site")

# Identity of Sea Star - excludes all NA values, need to figure out how to have different one

library(ggplot2)
library(tidyr)

# Assuming your data frame is named 'sea_stars_data'

# 1. Reshape the data frame
sea_stars_reshaped <- transect_ENVR_2024 %>%
  pivot_longer(cols = c(Ochre_EO, Leather_EL, Mottled_EM), names_to = "Species", values_to = "Presence")

# 2. Create the geom_tile plot
ggplot(data = sea_stars_reshaped, aes(x = Site_TA, y = Species, fill = Presence)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "lightgrey")) +
  labs(x = "Site_TA", y = "Species", fill = "Presence") +
  theme_minimal()


#=================================================================================================================================

# Abiotic Analysis
#=================================================================================================================================
min_tide_time <- aggregate(date ~ month, data = tide, FUN = function(x) {
  x[which.min(tide$SLEV_metres)] # Extract the date of the minimum tide height
})
# Extract hour from the minimum tide time
min_tide_time$time_column <- as.character(min_tide_time$time_column)
min_tide_time$hour <- as.integer(sapply(strsplit(as.character(min_tide_time$time), ":"), `[`, 1))

min_tide_time <- aggregate(hour ~ month, data = tide, FUN = function(x) {
  x[which.min(tide$SLEV_metres)] # Extract the hour of the minimum tide height
})
temperature_monthly_mean <- aggregate(AirTemp_degC ~ month, data = weather, FUN = mean)

# Merge the two datasets
monthly_abiotic_data <- merge(min_tide_time, temperature_monthly_mean, by = "month")

# Plotting
ggplot(monthly_data, aes(x = month)) +
  geom_bar(aes(y = hour), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = temperature), color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Temperature (°C)")) +
  labs(x = "Month", y = "Hour of Minimum Tide Height", 
       title = "Average Time of Minimum Tide Height and Temperature (2019-2023)",
       caption = "Data Source: Your Source") +
  theme_minimal()

