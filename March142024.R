# March 14, 2024
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
weather <- read.csv("data/UBC_Rooftop_obs_2019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
tide <- read.csv("data/Tide_Jan012019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))


# clean data
#=================================================================================================================================
# SPES Data
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
    
    # change 0 and 1 values to true false/presence absence - getting warning messages - any fix?
    
    change_to_logical <- function(df, start_col_index, end_col_index) {
      df <- df %>%
        mutate(across(start_col_index:end_col_index, as.logical))
      return(df)
    }
    
    quad0.25m <- change_to_logical(quad0.25m, 14, 44)
    
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
    transect$season <- get_season(transect$Date)
    transect$month <- month(as.Date(transect$Date))
    
    quad1m$season <- get_season(quad1m$Date)
    quad1m$month <- month(as.Date(quad1m$Date))
    
    quad0.25m$season <- get_season(quad0.25m$Date)
    quad0.25m$month <- month(as.Date(quad0.25m$Date))
    
    limpet$season <- get_season(limpet$Date)
    limpet$month <- month(as.Date(limpet$Date))

# Abiotic Data
    # Separate out tide data
    tide$date <- as.Date(tide$Obs_date)
    tide$time <- format(strptime(tide$Obs_date, format = "%Y-%m-%d %H:%M"), "%H:%M")
    # cant get function to work
    tide <- add_year_column(tide, "date")
    # in the meantime have this
      tide$year <- format(tide$date, "%Y")
      tide$month <- format(tide$date, "%m")
      tide$day <- format(tide$date, "%d")
    # seasonal tide
    tide$season <- get_season(tide$date)
    
    # Format weather data date
    weather$date <- ymd(weather$Dates)
    weather$year <- format(weather$date, "%Y")
    weather$month <- format(weather$date, "%m")
    weather$day <- format(weather$date, "%d")
    weather$season <- get_season(weather$date)
    
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


# Create separate plots for each species using the function
plot_ochre <- logical_plot(transect, "Ochre")
plot_leather <- logical_plot(transect, "Leather")
plot_molted <- logical_plot(transect, "Molted")

# Display the plots
plot_ochre
plot_leather
plot_molted


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

# plot 0.25m quadrat
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

plot_var_per_TA_Transect("Total_Cover_%", "Total % Cover of Algae and Invertebrates Combined", quad0.25m)

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
