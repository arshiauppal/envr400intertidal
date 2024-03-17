#Aanalysis of 2024 data: March 17
require(dplyr)
require(stringr)
require(ggplot2)
require(lubridate)
install.packages("tidyverse")
library(tidyverse)

# read data #don't use quad0.25m2024 use quad0.252024
#=================================================================================================================================
winter_transect <- read.csv("data/transect2024.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad0.25m_winter <- read.csv("data/quad0.252024.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet_winter <- read.csv("data/Limpet2024.csv", check.names = FALSE, na.strings=c("N/A", ""))

# clean data
#=================================================================================================================================
# add a year column
# need to make generalized - think we should make transect just dataframe but then it stops working - talk to Christina

add_year_column <- function(winter_transect, date_column_name) {
  winter_transect[[date_column_name]] <- as.Date(winter_transect[[date_column_name]], format = "%m/%d/%Y")
  winter_transect$Year <- format(winter_transect[[date_column_name]], "%Y")
  return(winter_transect)
}

winter_transect <- add_year_column(winter_transect, "Date")

#i Can't add the year column for 0.25 has to be seperate bc has a diff dat format 
add_year_column1 <- function(quad0.25m_winter, date_column_name) {
  quad0.25m_winter[[date_column_name]] <- as.Date(quad0.25m_winter[[date_column_name]], format = "%Y-%m-%d")
  quad0.25m_winter$Year <- format(quad0.25m_winter[[date_column_name]], "%Y")
  return(quad0.25m_winter)
}


winter_transect <- add_year_column(winter_transect, "Date")
quad0.25m_winter <- add_year_column1(quad0.25m_winter, "Date")
limpet_winter <- add_year_column(limpet_winter, "Date")

# change 0 and 1 values to true false/p

change_to_logical <- function(df, start_col_index, end_col_index, exclude_cols_list = list()) {
  cols_to_convert <- setdiff(start_col_index:end_col_index, unlist(exclude_cols_list))
  df <- mutate(df, across(cols_to_convert, as.logical))
  return(df)
}

#struggling bc the spes one had all the presence and absence from columns 12- 44. Here theres other stuff inbetween 
# the presence absence bc inbetween there is like species ID and count so you have to seperate each group manually
# idk if there is a better way 
# Example usage:
quad0.25m_winter <- change_to_logical(quad0.25m_winter, 15, 26, exclude_cols_list = list(c(27:29)))
quad0.25m_winter <- change_to_logical(quad0.25m_winter, 15, 26, exclude_cols_list = list(c(27:29), c(40:42)))


quad0.25m_winter <- change_to_logical(quad0.25m_winter, 15, 26)

transect <- change_to_logical(transect, 12, 14)

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
winter_transect$season <- get_season(winter_transect$Date)
winter_transect$month <- month(as.Date(winter_transect$Date))


quad0.25m_winter$season <- get_season(quad0.25m_winter$Date)
quad0.25m_winter$month <- month(as.Date(quad0.25m_winter$Date))

limpet_winter$season <- get_season(limpet_winter$Date)
limpet_winter$month <- month(as.Date(limpet_winter$Date))


# plot sea star
#=================================================================================================================================

# idk what is wrong w this

# plot variable per TA , averaged per year 
plot_var_per_TA <- function(varname, plot_varname, data){
  
  # create data frame with relevant columns
  df_var <- data.frame(site_TA = data$Site_TA,
                       year = data$Year,
                       var = data[,varname])
  df_var$Year <- as.character(df_var$year) 
  
  # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
  df_var <- aggregate(var ~ site_TA + year, data=df_var, FUN=mean)
  
  ggplot(data = df_var, aes(x = site_TA, y = var, group = year, fill = year)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab(plot_varname) + xlab("Site TA") + labs(fill = "Years")
  
}

plot_var_per_TA("Density_of_Sea_Stars_(count)", "Mean count of sea stars", winter_transect)


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

plot_var_per_TA("Density_of_Sea_Stars_(count)", "Mean count of sea stars", winter_transect)

