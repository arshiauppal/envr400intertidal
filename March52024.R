# March 5, 2024

# Load packages 
#=================================================================================================================================
require(dbplyr)
require(stringr)
require(ggplot2)
require(lubridate)

# Read data
#=================================================================================================================================
transect <- read.csv("data/Transect.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad1m <- read.csv("data/1m.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m <- read.csv("data/0.25m.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet <- read.csv("data/Limpet.csv", check.names = FALSE, na.strings=c("N/A", ""))

# Clean data
#=================================================================================================================================
# Creating a year column
  separateYear <- function(data, date_column_name = "Date") {
    data$Year <- format(as.Date(data[[date_column_name]]), "%Y")
    return(data) }
  
  transect <- separateYear(transect, date_column_name = "Date")
  quad1m <- separateYear(quad1m, date_column_name = "Date")
  quad0.25m <- separateYear(quad0.25m, date_column_name = "Date")
  limpet <- separateYear(limpet, date_column_name = "Date")
  
# Change 0 and 1 values to true false/ presence absence - make a function ****
  #Code not working - need to make a function
  convertColumnsToLogical <- function(data, start_col, end_col) {
    data <- data %>%
      mutate(across(start_col:end_col, as.logical))
    return(data)}
  
  quad0.25m <- convertColumnsToLogical(data = quad0.25m, start_col = 'SUM_WORM', end_col = 'SUM_HERMIT_CRAB')
  
  # The original code that we want to turn into a function
  quad0.25m <- quad0.25m |> 
    mutate(across('SUM_WORM': 'SUM_HERMIT_CRAB', as.logical))

  transect <- transect |> 
    mutate(across('Ochre': 'Molted', as.logical))

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






