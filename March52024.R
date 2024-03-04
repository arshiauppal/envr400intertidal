# March 5, 2024

require(dplyr)
require(stringr)
require(ggplot2)

# read data
#=================================================================================================================================
transect <- read.csv("data/Transect.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad1m <- read.csv("data/1m.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m <- read.csv("data/0.25m.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet <- read.csv("data/Limpet.csv", check.names = FALSE, na.strings=c("N/A", ""))

# clean data
#=================================================================================================================================
# Creating a year column

#Changing all dates to NA - unsure how to fix
format_date <- function(data, columnName) {
  data$Date <- format(as.Date(data[[columnName]], "%d/%m/%Y", na.rm = TRUE), "%d/%m/%Y")
  data$Year <- format(as.Date(data$Date, "%d/%m/%Y"), "%Y") 
  return(data)}

transect <- format_date(transect, "Date")

transect$Date <- format(as.Date(transect$Date), "%d/%m/%Y")
transect$Year <-  format(as.Date(transect$Date, "%d/%m/%Y"), "%Y")

quad1m$Date <- format(as.Date(quad1m$Date), "%d/%m/%Y")
quad1m$Year <- quad1m$Date
quad1m$Year <-  format(as.Date(quad1m$Date, "%d/%m/%Y"), "%Y")

quad0.25m$Date <- format(as.Date(quad0.25m$Date), "%d/%m/%Y")
quad0.25m$Year <- quad0.25m$Date
quad0.25m$Year <-  format(as.Date(quad0.25m$Date, "%d/%m/%Y"), "%Y")

limpet$Date <- format(as.Date(limpet$Date), "%d/%m/%Y")
limpet$Year <- limpet$Date
limpet$Year <-  format(as.Date(limpet$Date, "%d/%m/%Y"), "%Y")

# change 0 and 1 values to true false/ presence absence - make a function ****
quad0.25m <- quad0.25m |> 
  mutate(across('SUM WORM': 'SUM HERMIT CRAB', as.logical))

transect <- transect |> 
  mutate(across('Ochre': 'Molted', as.logical))

# Christina Plot
#=================================================================================================================================

# plot variable per TA, averaged per site visit
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
                      var = limpet[,"Mean_Length_mm"],
                      month = limpet$month)
df_limp$site_TA <- as.character(df_limp$site_TA)

# aggregate by year and site (mean per visit)
df_limp_agg <- aggregate(var ~ year + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg, aes(x = year, y = var, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Mean limpet length (mm)") + xlab("Time (years)") + labs(fill = "Site TA")


# aggregate by month and site (mean per visit)
df_limp_agg <- aggregate(var ~ month + site_TA, data=df_limp, FUN=mean)
ggplot(data = df_limp_agg, aes(x = month, y = var, group = site_TA, fill = site_TA)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  ylab("Mean limpet length (mm)") + xlab("Time (months)") + labs(fill = "Site TA")


# Old Plots
  # Sea star transect plots 
  Transect_Sea_Star <- ggplot(transect, aes(x = Year, y = Density of Sea Stars (Count), fill = Year)) +
  geom_bar(stat = "identity")
Transect_Sea_Star

# Limpet Size distribution per site
Limpet_Length_Site <- ggplot(limpet, aes(x = "Site TA", y = "Mean Length (mm)", fill = "Site TA")) + 
  geom_bar(stat = 'identity')
Limpet_Length_Site



