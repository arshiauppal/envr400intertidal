# March 7, 2024

require(dplyr)
require(stringr)
require(ggplot2)
require(lubridate)

# read data
#=================================================================================================================================
transect <- read.csv("data/Transect.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad1m <- read.csv("data/1m.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m <- read.csv("data/0.25m.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet <- read.csv("data/Limpet.csv", check.names = FALSE, na.strings=c("N/A", ""))

# clean data
#=================================================================================================================================
# add a year column
# need to make generalized - think we should make transect just dataframe but then it stops working - talk to Christina
add_year_column <- function(transect, date_column_name) {
  transect[[date_column_name]] <- format(as.Date(transect[["Date"]]), "%d/%m/%Y")
  transect$Year <- format(as.Date(transect[[date_column_name]], "%d/%m/%Y"), "%Y")
  return(transect)
}

transect <- add_year_column(transect, "Date")
quad1m <- add_year_column(quad1m, "Date")
quad0.25m <- add_year_column(quad0.25m, "Date")
limpet <- add_year_column(limpet, "Date")

# change 0 and 1 values to true false/presence absence
change_to_logical <- function(df, start_col_index, end_col_index) {
  df <- df %>%
    mutate(across(start_col_index:end_col_index, as.logical))
  return(df)
}

quad0.25m <- change_to_logical(quad0.25m, 14, 44)

transect <- change_to_logical(transect, 12, 14)



# Define function to create bar graph of mean limpet length and width by year not working
create_bar_graph <- function() {
  # Calculate mean length and width by year
  mean_length <- tapply(limpet$Mean_Length_mm, limpet$Year, mean)
  mean_width <- tapply(limpet$Mean_Width_mm, limpet$Year, mean)
  
  # Create a new data frame
  limpet_mean <- data.frame(
    Year = as.integer(names(limpet$mean_length)),
    Mean_Length_mm = as.numeric(limpet$mean_length),
    Mean_Width_mm = as.numeric(limpet$mean_width)
  )
  # Check for missing or infinite values
  if (any(is.infinite(limpet_mean$Mean_Length_mm)) || any(is.infinite(limpet_mean$Mean_Width_mm))) {
    stop("Some mean values are infinite.")
  }
  
  print(limpet_mean)
  # Plot the bar graph
  par(mfrow = c(1, 2))  # Set the plotting layout to have 1 row and 2 columns
  barplot(limpet_mean$Mean_Length_mm, names.arg = limpet_mean$Year,
          col = "blue", main = "Mean Limpet Length by Year",
          xlab = "Year", ylab = "Mean Length")
  barplot(limpet_mean$Mean_Width_mm, names.arg = limpet_mean$Year,
          col = "red", main = "Mean Limpet Width by Year",
          xlab = "Year", ylab = "Mean Width")

}

# Call the function
create_bar_graph()

plot_var_per_TA <- function(varname, plot_varname, data=quad1m){
  
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

plot_var_per_TA("Littorine_snails", "Mean count of littorine snails per field visit")

#--------------------

plot_var_per_TA <- function(varname, plot_varname, data=quad1m){
  
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

plot_var_per_TA("Nucella_snails", "Mean count of nucella snails per field visit")

#---------------
plot_var_per_TA <- function(varname, plot_varname, data=limpet){
  
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

plot_var_per_TA("Mean_Length_mm", "Mean length of limpets per field visit")



# Christina Plot
#=================================================================================================================================

# plot variable per TA, averaged per site visit - Transect
plot_var_per_TA <- function(varname, plot_varname, data=transect){
  
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

plot_var_per_TA("Density_of_Sea_Stars_Count", "Mean count of sea stars per field visit")

# DO FOR THE IDENTITY OF THE STAR

# get the season
get_season <- function(date){
  
  date <- as.Date(date) # make sure the input has date format
  mon <- months.Date(date, abbreviate = TRUE) # get month abbreviation
  
  # define winter (Dec, Jan, Feb), spring (Mar, Apr, May),
  # summer (Jun, Jul, Aug), fall (Sep, Oct, Nov)
  ifelse(mon %in% c("Dec", "Jan", "Feb"), "Winter",
         ifelse(mon %in% c("Mar", "Apr", "May"), "Spring",
                ifelse(mon %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
  
}
limpet$season <- get_season(limpet$Date)

limpet$month <- month(as.Date(limpet$Date))


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

# aggregate by month and site (mean per visit) - * Not liking the seasonality - incorporate once we get our data sorted
df_limp_agg <- aggregate(var ~ month + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg, aes(x = month, y = var, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  ylab("Mean limpet length (mm)") + xlab("Time (months)") + labs(fill = "Site TA")

