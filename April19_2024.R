# April 3, 2024
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(lubridate)
require(reshape2)
require(viridis)
require(RColorBrewer)

# read data
#=================================================================================================================================
  # SPES Data
    transect_SPES <- read.csv("data/SPES/Transect_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
    quad1m_SPES <- read.csv("data/SPES/1m_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
    quad0.25m_SPES <- read.csv("data/SPES/0.25m_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))
    limpet_SPES <- read.csv("data/SPES/Limpet_SPES.csv", check.names = FALSE, na.strings=c("N/A", ""))

  # ENVR 400 Data
    transect_ENVR <- read.csv("data/ENVR_2024/transect_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))
    quad0.25m_ENVR <- read.csv("data/ENVR_2024/0.25m_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))
    limpet_ENVR <- read.csv("data/ENVR_2024/Limpet_ENVR_2024.csv", check.names = FALSE, na.strings=c("N/A", ""))

  # Abiotic Data
    weather <- read.csv("data/Abiotic/UBC_Rooftop_obs_2019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
    tide <- read.csv("data/Abiotic/Tide_Jan012019-2024.csv", check.names = FALSE, na.strings=c("N/A",""))
#=================================================================================================================================
    
# clean data
#=================================================================================================================================
# Functions
  #=================================================================================================================================
  # function to add a year column - for ease of yearly analysis (extracts the year out of the date column)
    add_year_column <- function(data, date_column_name) {
      data[[date_column_name]] <- format(as.Date(data[[date_column_name]]), "%d/%m/%Y") # format original date column to be in day/month/year
      data$Year <- format(as.Date(data[[date_column_name]], "%d/%m/%Y"), "%Y") # from formatted date column, pulls out year
      return(data)
    }
    
  # change 0 and 1 values to true and false to indicate presence and absence of species
    convert_to_logical <- function(df, start_col_index, end_col_index) {
      df <- df %>%
        mutate(across(start_col_index:end_col_index, as.logical)) #  used to indicate the start and end of columns that need to be changed to true/false
      return(df)
    }
    
  # change site TA columns to character values with "TA-" infront of the number --> adding the TA helps with understanding so it is not just the site numbers 
    change_TA_column <- function(data_frame) {
      modified_df <- data_frame %>%
        mutate(modified_TA = paste0("TA-", as.character(Site_TA)))
      return(modified_df)
    }

  # get the season and month - extract the month column from the date to use for seasonal analysis 
    get_season <- function(date){
      date <- as.Date(date) # make sure the input has date format
      mon <- months.Date(date, abbreviate = TRUE) # get month abbreviation
    
      # use function to define which months fall under winter (Dec, Jan, Feb), spring (Mar, Apr, May),
      # summer (Jun, Jul, Aug), and fall (Sep, Oct, Nov)
      ifelse(mon %in% c("Dec", "Jan", "Feb"), "Winter",
            ifelse(mon %in% c("Mar", "Apr", "May"), "Spring",
                    ifelse(mon %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
    }
  
  # divide the transect into low, medium and high for intertidal height analysis 
    intertidal_height <- function(data_frame, column_name) {
      # Define breaks and labels
      breaks <- c(0, 10, 20, 30)
      labels <- c("low", "medium", "high")
      data_frame$intertidal_height <- cut(data_frame[[column_name]], breaks = breaks, labels = labels, include.lowest = TRUE)
      return(data_frame)
    }
    
  # proportional percent cover - function used to fix SPES's relative to total percent cover 
    adjusted_percent_cover <- function(dataframe, algae_cover_col, invertebrates_cover_col, total_cover_col) {
      dataframe$Adjusted_Algae_Cover <- dataframe[[algae_cover_col]] / (dataframe[[algae_cover_col]] + dataframe[[invertebrates_cover_col]]) * dataframe[[total_cover_col]]
      dataframe$Adjusted_Invert_Cover <- dataframe[[invertebrates_cover_col]] / (dataframe[[algae_cover_col]] + dataframe[[invertebrates_cover_col]]) * dataframe[[total_cover_col]]
      return(dataframe)
    }
    
  #=================================================================================================================================
  
# Cleaning Functions Applied to SPES Data
  #=================================================================================================================================
  # add year column
    transect_SPES <- add_year_column(transect_SPES, "Date")
    quad1m_SPES <- add_year_column(quad1m_SPES, "Date")
    quad0.25m_SPES <- add_year_column(quad0.25m_SPES, "Date")
    limpet_SPES <- add_year_column(limpet_SPES, "Date")

  # create a DOY column that takes the existing date column and rearranges it to date, month, year
    transect_SPES$DOY <- yday(dmy(transect_SPES$Date))
    quad1m_SPES$DOY <- yday(dmy(quad1m_SPES$Date))
    quad0.25m_SPES$DOY <- yday(dmy(quad0.25m_SPES$Date))
    limpet_SPES$DOY <- yday(dmy(limpet_SPES$Date))
    
  # change 0 and 1 values to true and false/presence and absence
    quad0.25m_SPES <- convert_to_logical(quad0.25m_SPES, 14, 44)
    transect_SPES <- convert_to_logical(transect_SPES, 12, 14)

  # change site_TA column to a character and to correct format --> easiest to work with site_ta as a character
    transect_SPES <- change_TA_column(transect_SPES)
    quad1m_SPES <- change_TA_column(quad1m_SPES)
    quad0.25m_SPES <- change_TA_column(quad0.25m_SPES)
    limpet_SPES <- change_TA_column(limpet_SPES)
    
  # get the season and month --> creates new month and season columns 
    transect_SPES$season <- get_season(transect_SPES$Date)
    transect_SPES$month <- month(as.Date(transect_SPES$Date))

    quad1m_SPES$season <- get_season(quad1m_SPES$Date)
    quad1m_SPES$month <- month(as.Date(quad1m_SPES$Date))

    quad0.25m_SPES$season <- get_season(quad0.25m_SPES$Date)
    quad0.25m_SPES$month <- month(as.Date(quad0.25m_SPES$Date))

    limpet_SPES$season <- get_season(limpet_SPES$Date)
    limpet_SPES$month <- month(as.Date(limpet_SPES$Date))
  
  # get intertidal height through transect point column
    quad0.25m_SPES <- intertidal_height(quad0.25m_SPES, "Transect_Point_m")
    quad1m_SPES <- intertidal_height(quad1m_SPES, "Transect_Point_m")
    
  # proportional percent cover to adjust relative to total % cover for SPES data 
    quad0.25m_SPES <- adjusted_percent_cover(quad0.25m_SPES, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
    
  #=================================================================================================================================

# Cleaning Functions Applied to ENVR 400 2024 Data
  #=================================================================================================================================
  # add year column
    transect_ENVR <- add_year_column(transect_ENVR, "Date") 
    quad0.25m_ENVR <- add_year_column(quad0.25m_ENVR, "Date")
    limpet_ENVR <- add_year_column(limpet_ENVR, "Date") 
  
  # create a DOY column - in order to run statistics on the seasonality the DOY did not start at 1 again on Janurary 1st (created linear models)
    transect_ENVR$DOY <- yday(dmy(transect_ENVR$Date))
      transect_ENVR$DOY <- ifelse(transect_ENVR$DOY >= 0 & transect_ENVR$DOY <= 50,
                                 transect_ENVR$DOY + 365,
                                 transect_ENVR$DOY)
    quad0.25m_ENVR$DOY <- yday(dmy(quad0.25m_ENVR$Date))
      quad0.25m_ENVR$DOY <- ifelse(quad0.25m_ENVR$DOY >= 0 & quad0.25m_ENVR$DOY <= 50,
                                 quad0.25m_ENVR$DOY + 365,
                                 quad0.25m_ENVR$DOY)
    limpet_ENVR$DOY <- yday(dmy(limpet_ENVR$Date))
      limpet_ENVR$DOY <- ifelse(limpet_ENVR$DOY >= 0 & limpet_ENVR$DOY <= 50,
                                 limpet_ENVR$DOY + 365,
                                 limpet_ENVR$DOY)
  
  # change presence/absence columns to logical - note if any other columns are added have to change range of columns
    transect_ENVR <- convert_to_logical(transect_ENVR, 14, 16)
    
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 15, 26)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 29, 36)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 39, 44)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 47, 54)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 57, 62)
    
  # change site_TA column to character and to correct format
    transect_ENVR <- change_TA_column(transect_ENVR)
    quad0.25m_ENVR <- change_TA_column(quad0.25m_ENVR)
    limpet_ENVR <- change_TA_column(limpet_ENVR)
    
  # get season and month
    transect_ENVR$season <- get_season(transect_ENVR$Date)
    transect_ENVR$month <- month(as.Date(transect_ENVR$Date))
  
    quad0.25m_ENVR$season <- get_season(quad0.25m_ENVR$Date)
    quad0.25m_ENVR$month <- month(as.Date(quad0.25m_ENVR$Date))
  
    limpet_ENVR$season <- get_season(limpet_ENVR$Date)
    limpet_ENVR$month <- month(as.Date(limpet_ENVR$Date))
    
  # get intertidal height
    quad0.25m_ENVR <- intertidal_height(quad0.25m_ENVR, "Transect_Point_M")
    
  # proportional percent cover
    quad0.25m_ENVR <- adjusted_percent_cover(quad0.25m_ENVR, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
    
  #=================================================================================================================================

# Cleaning Functions Applied to Abiotic Data
  #=================================================================================================================================
  # Separate out date and time from tide data
    tide <- tide %>%
      mutate(date = as.Date(Obs_date),
          time = format(strptime(Obs_date, format = "%Y-%m-%d %H:%M"), "%H:%M"))
    tide <- add_year_column(tide, "date")
  
  # Separate out the hour from the tide time
    tide$time <- as.POSIXct(tide$time, format = "%H:%M")
    tide$hour <- format(tide$time, "%H")
    tide$hour <- as.numeric(tide$hour)
  
  #get seasonal and monthly information for tide
    tide$season <- get_season(tide$date)
    tide$month <- month(as.Date(tide$date))

  # Format weather data date
    weather$date <- ymd(weather$Dates)
    weather <- add_year_column(weather, "date")

    weather$season <- get_season(weather$date)
    weather$month <- month(as.Date(weather$date))
  #=================================================================================================================================

# Overall functions- General 
  #================================================================================================================================
  # plot percent cover along the transect line - need to fix aesthetics
    cover_intertidal_height <- function(data, color_scale = c("low" = "lightblue", "high" = "darkblue"), plot_title = "Intertidal Height and Mean Percent Cover") {
      selected_data <- data.frame(
        intertidal_height = data$intertidal_height,
        Total = data$Total_Cover,
        Algae = data$`Adjusted_Algae_Cover`,
        Invertebrates = data$`Adjusted_Invert_Cover`
      )
      
      quad_agg_total <- aggregate(Total ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
      quad_agg_algae <- aggregate(Algae ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
      quad_agg_intertebrates <- aggregate(Invertebrates ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
      
      quad_agg <- quad_agg_total %>%
        left_join(quad_agg_algae, by = "intertidal_height") %>%
        left_join(quad_agg_intertebrates, by = "intertidal_height")
      
      tidy_quad_data <- pivot_longer(quad_agg, cols = c(Total, Algae, Invertebrates), names_to = "Percent_Cover_Type", values_to = "Percent_Cover")
      
      ggplot(tidy_quad_data, aes(x = Percent_Cover_Type, y = intertidal_height, fill = Percent_Cover)) +
        geom_tile(color = "white", alpha = 0.85) +
        geom_text(aes(label = Percent_Cover), vjust = 1.3) +
        scale_fill_gradientn(colours = color_scale, name = "Percent Cover") +
        labs(x = "Percent Cover", y = "Intertidal Height", title = plot_title) +  # Custom plot title
        theme_minimal() +
        theme(plot.title = element_text(size = rel(1.3))) +
        scale_y_discrete(limits = c("low", "medium", "high"))
    }
  
  # create a dataframe for the total percent cover plot
    select_total_cover <- function(dataframe, include_year = TRUE, include_season = TRUE) {
      if (include_year & include_season) {
        cover <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          season = dataframe$season,
          total_cover = dataframe$Total_Cover,
          algae_cover = dataframe$Adjusted_Algae_Cover,
          invert_cover = dataframe$Adjusted_Invert_Cover
        )
      } else if (include_year & !include_season) {
        cover <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          total_cover = dataframe$Total_Cover,
          algae_cover = dataframe$Adjusted_Algae_Cover,
          invert_cover = dataframe$Adjusted_Invert_Cover
        )
      } else if (!include_year & include_season) {
        cover <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          season = dataframe$season,
          total_cover = dataframe$Total_Cover,
          algae_cover = dataframe$Adjusted_Algae_Cover,
          invert_cover = dataframe$Adjusted_Invert_Cover
        )
      } else {
        cover <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          total_cover = dataframe$Total_Cover,
          algae_cover = dataframe$Adjusted_Algae_Cover,
          invert_cover = dataframe$Adjusted_Invert_Cover
        )
      }
      
      cover <- cover[complete.cases(cover$total_cover) & complete.cases(cover$algae_cover) & complete.cases(cover$invert_cover), ]
      return(cover)
    }
  
  # create a dataframe for the alage percent cover and species count plot
    select_algae <- function(dataframe, include_year = TRUE, include_season = TRUE) {
      if (include_year & include_season) {
        algae_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          season = dataframe$Season,
          algae_percent_cover = dataframe$Algae_Cover,
          algae_count = dataframe$Algae_Count
        )
      } else if (include_year & !include_season) {
        algae_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          algae_percent_cover = dataframe$Algae_Cover,
          algae_count = dataframe$Algae_Count
        )
      } else if (!include_year & include_season) {
        algae_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          season = dataframe$Season,
          algae_percent_cover = dataframe$Algae_Cover,
          algae_count = dataframe$Algae_Count
        )
      } else {
        algae_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          algae_percent_cover = dataframe$Algae_Cover,
          algae_count = dataframe$Algae_Count
        )
      }
      
      algae_data <- algae_data[complete.cases(algae_data$algae_percent_cover) & is.numeric(algae_data$algae_percent_cover), ]
      return(algae_data)
    }
    
  # create a dataframe for the invertebrate percent cover and species count plot
    select_invertebrate <- function(dataframe, include_year = TRUE, include_season = TRUE) {
      if (include_year & include_season) {
        invertebrates_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          season = dataframe$Season,
          invert_percent_cover = dataframe$Invertebrates_Cover,
          sessile_count = dataframe$Sessile_Invertebrates_Count,
          mobile_count = dataframe$Mobile_Invertebrates_Count
        )
      } else if (include_year & !include_season) {
        invertebrates_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          year = dataframe$Year,
          invert_percent_cover = dataframe$Invertebrates_Cover,
          sessile_count = dataframe$Sessile_Invertebrates_Count,
          mobile_count = dataframe$Mobile_Invertebrates_Count
        )
      } else if (!include_year & include_season) {
        invertebrates_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          season = dataframe$Season,
          invert_percent_cover = dataframe$Invertebrates_Cover,
          sessile_count = dataframe$Sessile_Invertebrates_Count,
          mobile_count = dataframe$Mobile_Invertebrates_Count
        )
      } else {
        invertebrates_data <- data.frame(
          site_TA = dataframe$Site_TA,
          modified_TA = dataframe$modified_TA,
          invert_percent_cover = dataframe$Invertebrates_Cover,
          sessile_count = dataframe$Sessile_Invertebrates_Count,
          mobile_count = dataframe$Mobile_Invertebrates_Count
        )
      }
      
      invertebrates_data <- invertebrates_data[complete.cases(invertebrates_data$invert_percent_cover) & is.numeric(invertebrates_data$invert_percent_cover), ]
      return(invertebrates_data)
    }
    
  # aggregate total percent cover data
    aggregate_cover_data <- function(data, cover_cols, season_col = "season", site_TA_col = "site_TA", modified_TA_col = "modified_TA") {
      # Aggregate mean cover data
      cover_agg <- aggregate(. ~ season + site_TA + modified_TA, data = data[, c(cover_cols, season_col, site_TA_col, modified_TA_col)], FUN = mean, na.action = na.omit)
      
      # Aggregate standard deviation of cover data
      cover_agg_sd <- aggregate(data[[cover_cols[3]]] ~ season + site_TA + modified_TA, data = data[, c(cover_cols[3], season_col, site_TA_col, modified_TA_col)], FUN = function(x) sd(x, na.rm = TRUE), na.action = na.omit)
      
      # Rename the standard deviation column
      colnames(cover_agg_sd) <- c(season_col, site_TA_col, modified_TA_col, "cover_sd")
      
      # Merge aggregated data
      cover_merge <- merge(cover_agg, cover_agg_sd, by = c(season_col, site_TA_col, modified_TA_col))
      
      return(cover_merge)
    }
    
  # aggregate algae and invertebrate percent cover and count data
    aggregate_count_data <- function(data, percent_cover_col, count_col, additional_count_col = NULL) {
      # Check if "year" column exists in the data
      if ("year" %in% colnames(data)) {
        # If "year" column exists, aggregate with it
        total_cover_mean <- aggregate(data[[percent_cover_col]] ~ site_TA + modified_TA + year, data = data, FUN = function(x) mean(x))
        names(total_cover_mean) <- c("site_TA", "modified_TA", "year", "percent_cover") # Assign column names
        
        count_rounded_mean <- aggregate(data[[count_col]] ~ site_TA + modified_TA + year, data = data, FUN = function(x) round(mean(x)))
        names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original data column
        
        if (!is.null(additional_count_col)) {
          additional_count_rounded_mean <- aggregate(data[[additional_count_col]] ~ site_TA + modified_TA + year, data = data, FUN = function(x) round(mean(x)))
          names(additional_count_rounded_mean)[length(names(additional_count_rounded_mean))] <- additional_count_col # Assign column name as the original data column
          
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
          total <- merge(total, additional_count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
        } else {
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
        }
        
        # Calculate standard deviation of percent cover
        percent_sd <- aggregate(data[[percent_cover_col]] ~ site_TA + modified_TA + year, data = data, FUN = function(x) sd(x), na.action = na.omit)
        names(percent_sd) <- c("site_TA", "modified_TA", "year", paste(percent_cover_col, "sd", sep = "_")) # Assign column names
        
        return(list(total = total, percent_sd = percent_sd))
        
      } else {
        # If "year" column doesn't exist, aggregate without it
        total_cover_mean <- aggregate(data[[percent_cover_col]] ~ site_TA + modified_TA, data = data, FUN = function(x) mean(x))
        names(total_cover_mean) <- c("site_TA", "modified_TA", "percent_cover") # Assign column names
        
        count_rounded_mean <- aggregate(data[[count_col]] ~ site_TA + modified_TA, data = data, FUN = function(x) round(mean(x)))
        names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original data column
        
        if (!is.null(additional_count_col)) {
          additional_count_rounded_mean <- aggregate(data[[additional_count_col]] ~ site_TA + modified_TA, data = data, FUN = function(x) round(mean(x)))
          names(additional_count_rounded_mean)[length(names(additional_count_rounded_mean))] <- additional_count_col # Assign column name as the original data column
          
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA"))
          total <- merge(total, additional_count_rounded_mean, by = c("site_TA", "modified_TA"))
        } else {
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA"))
        }
        
        # Calculate standard deviation of percent cover
        percent_sd <- aggregate(data[[percent_cover_col]] ~ site_TA + modified_TA, data = data, FUN = function(x) sd(x), na.action = na.omit)
        names(percent_sd) <- c("site_TA", "modified_TA", paste(percent_cover_col, "sd", sep = "_")) # Assign column names
        
        return(list(total = total, percent_sd = percent_sd))
      }
    }
    
  # adjustment for secondary y-axis for percent cover and count plots 
    adj <- 25
    
# SPES Data Visualization 
#=================================================================================================================================
# Functions 
  #=================================================================================================================================
  # General Function for plotting a variable for every TA for every year.
    plot_count_per_TA_SPES <- function(data, varname, plot_title, plot_yaxis){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = data$modified_TA,
                           year = data$Year,
                           var = data[,varname])

      # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
      df_var_mean <- aggregate(var ~ year + site_TA, data=df_var, FUN=mean)
      names(df_var_mean)[3] <- "mean_count"
      df_var_sd <- aggregate(var ~ year + site_TA, data = df_var, FUN = function(x) sd(x))
      names(df_var_sd)[3] <- "sd_count"
      
      df_var_all <- merge(df_var_mean, df_var_sd, by = c("year", "site_TA"))
      
      ggplot(data = df_var_all, aes(x = site_TA, y = mean_count, group = year, fill = year)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        geom_errorbar(aes(ymin = pmax(mean_count - sd_count, 0), ymax = mean_count + sd_count),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(plot_yaxis) + xlab("Site") + labs(fill = "Year") +
        scale_fill_viridis(discrete = TRUE) +
        labs(title = plot_title) +
        theme_minimal() +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title   
    }
    
  # presence absence plot of sea star species
    presence_absence_SPES <- function(data, species_column) {
      df <- data.frame(Year = data$Year,
          Site_TA = data$modified_TA,
          Species = data[,species_column])
      df <- df[complete.cases(df),]

      df <- aggregate(Species ~ Year + Site_TA, data=df, FUN=sum)
      
      df$Species <- as.logical(df$Species)

      df_grid <- merge(expand.grid(Site_TA=1:6, Year = 2019:2023), df, all.x = TRUE)
      
      ggplot(df_grid, aes(x = as.character(Year), y = as.character(Site_TA), fill = Species)) +
        geom_tile(colour ='black') +
        scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                          labels = c("TRUE" = "Present", "FALSE" = "Absent"),
                          na.value = "gray",
                          drop=FALSE) +  # Adjust colors as needed
        labs(x = "Year", y = "Site", fill = paste0(species_column, " Presence/Absence"), 
             title = paste(species_column, "Sea Star Presence Absence in the Spring/Summer from 2019-2023")) +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
          ) +
        scale_y_discrete(breaks = unique(data$Site_TA)) # Set breaks for y-axis
    }
    
  # Function for limpet plots
    limpet_plots_SPES <- function(data, agg_variable, plot_title, plot_yaxis) {
      agg_mean <- aggregate(get(agg_variable) ~ Year + modified_TA, data = data, FUN = mean)
      names(agg_mean)[3] <- paste("mean_", agg_variable, sep = "")
      
      agg_sd <- aggregate(get(agg_variable) ~ Year + modified_TA, data = data, FUN = function(x) sd(x))
      names(agg_sd)[3] <- paste("sd_", agg_variable, sep = "")
      
      agg_all <- left_join(agg_mean, agg_sd, by = c("Year", "modified_TA"))
      
      ggplot(data = agg_all, aes(x = Year, y = get(paste("mean_", agg_variable, sep = "")), group = modified_TA, fill = Year)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        geom_errorbar(aes(ymin = get(paste("mean_", agg_variable, sep = "")) - get(paste("sd_", agg_variable, sep = "")), 
                          ymax = get(paste("mean_", agg_variable, sep = "")) + get(paste("sd_", agg_variable, sep = ""))),
                      position = position_dodge(width = 0.9), width = 0.25) +
        facet_wrap(~modified_TA) +
        ylab(plot_yaxis) +
        xlab("Site") +
        labs(fill = "Year") +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal() +
        labs(title = plot_title) +
        scale_y_continuous(limits = c(0, 25)) +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title   
    }
  
  # plot for specified variable count  along intertidal height
    plot_count_intertidal_height <- function(data, count_variable, scale_fill = c("lightblue", "darkblue"), plot_title) {
      # Aggregate the data for the specified count variable
      agg_data <- aggregate(data[[count_variable]], by = list(data$intertidal_height), FUN = function(x) round(mean(x, na.rm = TRUE)))
      # Rename columns
      names(agg_data) <- c("intertidal_height", "Count")
      # Reshape the data
      tidy_data <- tidyr::pivot_longer(agg_data, cols = c(Count), names_to = "Count_Type", values_to = "Count")
      # Generate the plot
      ggplot(tidy_data, aes(x = Count_Type, y = intertidal_height, fill = Count)) +
        geom_tile(color = "white", alpha = 0.85) +
        geom_text(aes(label = Count), vjust = 1) +
        scale_fill_gradient(low = scale_fill[1], high = scale_fill[2]) +
        labs(x = "Count", y = "Tide Height", title = plot_title, fill = paste0(count_variable, " Count")) +
        theme_minimal() +
        scale_y_discrete(limits = c("low", "medium", "high")) +
        theme(plot.title=element_text(size=15), #change font size of plot title
                     axis.text=element_text(size=11), #change font size of axis text
                     axis.title=element_text(size=14), #change font size of axis titles
                     legend.text=element_text(size=11), #change font size of legend text
                     legend.title=element_text(size=14)) #change font size of legend title   
    }

  #=================================================================================================================================
  
# Transect Data - aesthetics good, stats done
  #=================================================================================================================================
  # Density of sea stars per TA (2019-2023) - SPES
    SS_density_TA <- plot_count_per_TA_SPES(transect_SPES, "Density_of_Sea_Stars_Count", 
                                            "Mean Count of Sea Stars in the Spring/Summer from 2019-2023 ", 
                                            "Mean Count of Sea Stars")
      SS_density_TA
      
      SS_var_sd <- aggregate(Density_of_Sea_Stars_Count ~ Year + Site_TA, data = transect_SPES, FUN = function(x) sd(x))
      names(df_var_sd)[3] <- "sd_count"
      
      # Stats
        # Year
          SS_Year_anova <- aov(Density_of_Sea_Stars_Count ~ Year, data = transect_SPES)
            summary(SS_Year_anova)
            # p-value = 0.406
        # Sites
            SS_Sites_anova <- aov(Density_of_Sea_Stars_Count ~ Site_TA, data = transect_SPES)
              summary(SS_Sites_anova)
              # p-value = 0.0619
      
  # Presence absence of sea stars visualization
    plot_ochre <- presence_absence_SPES(transect_SPES, "Ochre")
      plot_ochre
    plot_leather <- presence_absence_SPES(transect_SPES, "Leather")
      plot_leather
    plot_mottled <- presence_absence_SPES(transect_SPES, "Mottled")
      plot_mottled 
  #=================================================================================================================================
      
# Limpet Data for SPES - aesthetics good, stats done
  #=================================================================================================================================
  # plot limpet length
    limpet_length_SPES <- limpet_plots_SPES(limpet_SPES, "Mean_Length_mm",
                                            "Mean Length and Width of Limpets in the Spring/Summer from 2019-2023",
                                            "Mean Length (mm)")
      limpet_length_SPES
      
      # Stats
        # Year
          Length_Year_anova <- aov(Mean_Length_mm ~ Year, data = limpet_SPES)
            summary(Length_Year_anova)
            # p-value = 3.83e-05
        # Sites
          Length_Site_anova <- aov(Mean_Length_mm ~ Site_TA, data = limpet_SPES)
            summary(Length_Site_anova)
            # p-value = 0.00267
      
  # plot limpet width
      limpet_width_SPES <- limpet_plots_SPES(limpet_SPES, "Mean_Width_mm",
                                              "Mean Width of Limpets in the Spring/Summer from 2019-2023",
                                              "Mean Width (mm)")
      limpet_width_SPES
      
      # Anova Statistics
        # Year
          Width_Year_anova <- aov(Mean_Width_mm ~ Year, data = limpet_SPES)
            summary(Width_Year_anova)
            # p-value = 8.86e-05
        # Sites
          Width_Site_anova <- aov(Mean_Width_mm ~ Site_TA, data = limpet_SPES)
            summary(Width_Site_anova)
            # p-value = 0.0017
  #=================================================================================================================================
      
# SPES 1m quadrat data - aesthetics good, stats done
  #=================================================================================================================================
  # Littorine Snails Visualization
    Lit_density_TA <- plot_count_per_TA_SPES(quad1m_SPES, "Littorine_snails", 
                                              "Mean Count of Littorine Snails in the Spring/Summer from 2019-2023", 
                                              "Mean Count of Littorine Snails")
    Lit_density_TA
    
    #ANOVA Statistics
      # Year
        Lit_Year_anova <- aov(Littorine_snails ~ Year, data = quad1m_SPES)
          summary(Lit_Year_anova)
          # p-value = 0.0059
      # Sites
        Lit_Site_anova <- aov(Littorine_snails ~ Site_TA, data = quad1m_SPES)
          summary(Lit_Site_anova)
          # p-value = 0.0015

  # Limpet Count Visualization
    Limpet_density_TA <- plot_count_per_TA_SPES(quad1m_SPES, "Limpets", 
                                             "Mean Count of Limpets in the Spring/Summer from 2019-2023", 
                                             "Mean Count of Limpets")
    Limpet_density_TA
    
    # ANOVA Stats
      # Year
        Limp_Year_anova <- aov(Limpets ~ Year, data = quad1m_SPES)
          summary(Limp_Year_anova)
            # p-value = 6.7e-05
      # Sites
        Limp_Site_anova <- aov(Limpets ~ Site_TA, data = quad1m_SPES)
          summary(Limp_Site_anova)
          # p-value = 0.00178

  # Changes in intertidal height composition Visualiization 
      lit_snail_height <- plot_count_intertidal_height(quad1m_SPES, 'Littorine_snails', scale_fill = c("lightgreen", "darkgreen"), 
                                                       "Intertidal Height and Count of Littorine Snails in the Spring/Summer")
        lit_snail_height
        
        # ANOVA Stats
          # Intertidal height
            Lit_Height_anova <- aov(Littorine_snails ~ intertidal_height, data = quad1m_SPES)
              summary(Lit_Height_anova)
              # p-value = 0.0146
        
      limpet_height <- plot_count_intertidal_height(quad1m_SPES, 'Limpets', scale_fill = c("purple", "purple4"),
                                                    "Intertidal Height and Count of Organisms in the Spring/Summer")
        limpet_height  
        
        # ANOVA Stats
          # Intertidal height
            Limp_Height_anova <- aov(Limpets ~ intertidal_height, data = quad1m_SPES)
              summary(Limp_Height_anova)
              # p-value = 0.874
  #=================================================================================================================================
    
# SPES 0.25m quadrat data - stats done, should make functions (messy and overwhelming)
  #=================================================================================================================================
  # Mean total and relative percent cover of algae and invertebrates for each site and year
   # Select data
    percent_cover_SPES <- select_total_cover(quad0.25m_SPES, include_year = TRUE, include_season = FALSE)
   
    # aggregate data 
    all_cover <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ year + site_TA, data= percent_cover_SPES, FUN=mean, na.action = na.omit)
    cover_sd <- aggregate(invert_cover ~ year + site_TA, data = percent_cover_SPES, FUN = function(x) sd(x))
      names(cover_sd)[3] <- "sd_cover"
    
      percent_cover_all <- merge(all_cover, cover_sd, by = c("year", "site_TA"))
      
    # plot
      ggplot(percent_cover_all, aes(x = as.factor(year), y = total_cover)) +
        geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - sd_cover/2, ymax = invert_cover + sd_cover/2,
                          group = site_TA),  # Group by site_TA
                      position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
        facet_wrap(~site_TA) +
        labs(x = "Year", y = "Percent Cover", title = "Mean Percent Cover of Algae and Invertebrates in the Spring/Summer from 2019-2023", fill = "Organismal Class") +
        scale_fill_viridis(discrete = TRUE, option = "D", alpha = 0.8) +  # Using viridis color palette for fill with lighter shades
        theme_minimal() +
        scale_y_continuous(limits = c(0, 100))
      
    # ANOVA Stats
      # Year
        TCover_Year_anova <- aov(Total_Cover ~ Year, data = quad0.25m_SPES)
          summary(TCover_Year_anova)
          # p-value = 0.000577
      # Sites
        TCover_Site_anova <- aov(Total_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(TCover_Site_anova)
          # p-value = 0.395
  
  # Visualization of percent cover and count of algae
    algae_SPES <- select_algae(quad0.25m_SPES, include_year = TRUE, include_season = FALSE)
    
    algae_SPES_agg <- aggregate_count_data(algae_SPES, "algae_percent_cover", "algae_count")  
    algae_SPES_total <- merge(algae_SPES_agg$total, algae_SPES_agg$percent_sd, by = c("site_TA", "modified_TA", "year"))
  
    quad_0.25m_SPES_algae_plot <- ggplot(data = algae_SPES_total, aes(x = year, y = percent_cover)) +
      geom_bar(stat = "identity", aes(fill = as.factor(year)), position = "stack", alpha = 0.8) +  # Adjust transparency with alpha
      geom_line(data = algae_SPES_total, aes(x = year, y = algae_count*adj, group = 1, color = "Algae")) +  # Add line for algae count
      geom_errorbar(aes(ymin = pmax(0, percent_cover - algae_percent_cover_sd), 
                        ymax = pmin(100, percent_cover + algae_percent_cover_sd)), 
                    width = 0.25, position = position_dodge(width = 0.9),
                    color = "black", linewidth = 0.5) +  # Adjust error bar aesthetics
      labs(x = "Year", y = "Percent Cover", 
           title = "Mean Percent Cover and Count of Algae in the Spring/Summer 2019-2023", 
           fill = "Year", color = "Algae Species Count") +
      scale_y_continuous(sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +
      facet_wrap(~ site_TA) +
      scale_fill_viridis(discrete = TRUE) +      
      scale_color_manual(name = "Species Count",
                         values = c("Algae" = "red"),
                         labels = c("Algae")) +  # Define the color and label for the legend
      theme_minimal() +
      theme(plot.title=element_text(size=15), #change font size of plot title
            axis.text=element_text(size=11), #change font size of axis text
            axis.title=element_text(size=14), #change font size of axis titles
            legend.text=element_text(size=11), #change font size of legend text
            legend.title=element_text(size=14)) #change font size of legend title 
    
    print(quad_0.25m_SPES_algae_plot)
    
    # ANOVA Stats
      # Year
        AlgaeCover_Year_anova <- aov(Algae_Cover ~ Year, data = quad0.25m_SPES)
          summary(AlgaeCover_Year_anova)
          # p-value = 0.525
      # Sites
        AlgaeCover_Site_anova <- aov(Algae_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(AlgaeCover_Site_anova)
          # p-value = 5.24e-06
    
  # percent cover of invertebrates and count of sessile and mobile - NEED TO FIX LEGEND CODE
    invert_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                        modified_TA = quad0.25m_SPES$modified_TA,
                                        year = quad0.25m_SPES$Year,
                                        invert_percent_cover = quad0.25m_SPES$Invertebrates_Cover,
                                        sessile_count = quad0.25m_SPES$Sessile_Invertebrates_Count,
                                        mobile_count = quad0.25m_SPES$Mobile_Invertebrates_Count)
    invert_quad0.25m_SPES <- invert_quad0.25m_SPES[complete.cases(invert_quad0.25m_SPES$invert_percent_cover) & is.numeric(invert_quad0.25m_SPES$invert_percent_cover), ]
    
    invert_SPES <- aggregate_count_data(invert_quad0.25m_SPES, "invert_percent_cover", "sessile_count", "mobile_count")  
    invert_SPES_total <- merge(invert_SPES$total, invert_SPES$percent_sd, by = c("site_TA", "modified_TA", "year"))
    
    quad_0.25_SPES_invert_plot <- ggplot(data = invert_SPES_total, aes(x = year, y = percent_cover)) +
      geom_bar(stat = "identity", aes(fill = as.factor(year)), position = "stack", alpha = 0.8) +  
      geom_line(aes(y = sessile_count * adj, color = "Sessile"), group = 1) +
      geom_line(aes(y = mobile_count * adj, color = "Mobile"), group = 1) +
      geom_errorbar(aes(ymin = pmax(0, percent_cover - invert_percent_cover_sd), 
                        ymax = pmin(100, percent_cover + invert_percent_cover_sd)), 
                    width = 0.25, position = position_dodge(width = 0.9),
                    color = "black", size = 0.5) +
      labs(x = "Year", y = "Percent Cover", 
           title = "Mean Percent Cover and Count of Invertebrates in the Spring/Summer 2019-2023", 
           fill = "Year", color = "Species") +
      scale_y_continuous(sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +
      facet_wrap(~ modified_TA) +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_manual(name = "Species Count", values = c("Sessile" = "red", "Mobile" = "blue")) +
      theme_minimal() +
      theme(plot.title=element_text(size=15), #change font size of plot title
            axis.text=element_text(size=11), #change font size of axis text
            axis.title=element_text(size=14), #change font size of axis titles
            legend.text=element_text(size=11), #change font size of legend text
            legend.title=element_text(size=14),
            strip.text = element_text(size = 11))
    
    quad_0.25_SPES_invert_plot
    
    # Stats
      # Year
        InvertCover_Year_anova <- aov(Invertebrates_Cover ~ Year, data = quad0.25m_SPES)
          summary(InvertCover_Year_anova)
          # p-value = 0.371
      # Sites
        InvertCover_Site_anova <- aov(Invertebrates_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(InvertCover_Site_anova)
          # p-value = 4.93e-05

  # cover of algae vs invertebrates across the transect - summer
    cover_intertidal_height(quad0.25m_SPES, color_scale = c("low" = "yellow", "high" = "red"), 
                            plot_title = "Intertidal Height and Mean Percent Cover in the Spring/Summer from 2019-2023")
    # Stats
      # Total Cover
        TCover_Height_anova <- aov(Total_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(TCover_Height_anova)
          # p-value = 0.00348
      # Algae Cover
        AlgaeCover_Height_anova <- aov(Algae_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(AlgaeCover_Height_anova)
          # p-value = 0.966
      # Invertebrates Cover
        InvertCover_Height_anova <- aov(Invertebrates_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(InvertCover_Height_anova)
          # p-value = 0.738
    
  #=================================================================================================================================

# ENVR 400 2024 Data Analysis
#=================================================================================================================================
# Functions
  #=================================================================================================================================
  site_colors_ENVR <- c("skyblue2", "orchid2", "coral")
    
  # count per TA
  plot_count_per_TA_ENVR <- function(data, varname, plot_varname, plot_title){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = data$modified_TA,
                           var = data[,varname])
      
      # aggregate to the mean per Site_TA
      df_var_mean <- aggregate(var ~ site_TA, data=df_var, FUN=mean)
      names(df_var_mean)[2] <- "mean_count"
      df_var_sd <- aggregate(var ~ site_TA, data = df_var, FUN = function(x) sd(x))
      names(df_var_sd)[2] <- "sd_count"
      
      df_var_all <- merge(df_var_mean, df_var_sd, by = "site_TA")
      
      ggplot(data = df_var_all, aes(x = site_TA, y = mean_count, fill = site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax(mean_count - sd_count, 0), ymax = mean_count + sd_count),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(plot_varname) + xlab("Site") + labs(title = plot_title, fill = "Site") +
        scale_fill_manual(values = site_colors_ENVR) +
        theme_minimal() +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title  
  }
    
  # identity of sea stars
  presence_absence_ENVR <- function(data, species_column, plot_varname) {
      df <- data.frame(Site_TA = data$Site_TA,
                       Species = data[[species_column]])
      
      df_1 <- subset(df, Site_TA == 1)
      
      df <- aggregate(Species ~ Site_TA, data=df, FUN=sum)
      df_1 <- aggregate(Species ~ Site_TA, data=df_1, FUN=sum, na.action = na.pass)
      
      df_merge <- rbind(df, df_1)
      
      df_merge$Species <- as.logical(df_merge$Species)
      
      ggplot(df_merge, aes(x = as.character(Site_TA), y = "", fill = Species)) +
        geom_tile(colour ='black') +
        scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red",
                          labels = c("TRUE" = "Present", "FALSE" = "Absent")), 
                          na.value = "gray", drop=FALSE) +
        labs(x = "Sampling Site", y = plot_varname,
             title = "Sea Star Presence Absence in Winter 2023/2024", fill = paste0(species_column, " Presence/Absence")) +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
              panel.grid.minor = element_blank(),  # Remove minor gridlines
              axis.text.y = element_text(angle = 0, hjust = 0.5)) +
        scale_y_discrete(breaks = unique(df_merge$Site_TA))  # Set breaks for y-axis
    }
  
  # limpet plots
  limpet_plots_ENVR <- function(data, agg_variable, plot_varname, plot_title) {
    agg_mean <- aggregate(get(agg_variable) ~ modified_TA, data = data, FUN = mean)
    names(agg_mean)[2] <- paste("mean_", agg_variable, sep = "")
    
    agg_sd <- aggregate(get(agg_variable) ~ modified_TA, data = data, FUN = function(x) sd(x))
    names(agg_sd)[2] <- paste("sd_", agg_variable, sep = "")
    
    agg_all <- left_join(agg_mean, agg_sd, by = "modified_TA")
    
    ggplot(data = agg_all, aes(x = modified_TA, y = get(paste("mean_", agg_variable, sep = "")), fill = modified_TA)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = get(paste("mean_", agg_variable, sep = "")) - get(paste("sd_", agg_variable, sep = "")), ymax = get(paste("mean_", agg_variable, sep = "")) + get(paste("sd_", agg_variable, sep = ""))),
                    position = position_dodge(width = 0.9), width = 0.25) +
      ylab(plot_varname) +
      xlab("Site") +
      labs(fill = "Site", title = plot_title) +
      scale_fill_manual(values = site_colors_ENVR) +
      scale_y_continuous(limits = c(0, 33)) +
      theme_minimal() +
      theme(plot.title=element_text(size=15), #change font size of plot title
            axis.text=element_text(size=11), #change font size of axis text
            axis.title=element_text(size=14), #change font size of axis titles
            legend.text=element_text(size=11), #change font size of legend text
            legend.title=element_text(size=14)) #change font size of legend title 
  }
  
  #=================================================================================================================================

# Transect - cant change TRUE/FALSE to Present/Absent idk why but not working, stats done
  #=================================================================================================================================
  # Density of sea stars per TA 
    SS_density_TA_ENVR <- plot_count_per_TA_ENVR(transect_ENVR, "Density_of_Sea_Stars_count", "Mean Count of Sea Stars",
                                                 "Mean Count of Sea Stars during Winter 2023/2024")
      SS_density_TA_ENVR
      
    # Stats
      SS_ENVR_anova <- aov(Density_of_Sea_Stars_count ~ Site_TA, data = transect_ENVR)
        summary(SS_ENVR_anova)
      # p-value = 0.000106
  
  # Density of Oysters per TA
    oyster_density_TA_ENVR <- plot_count_per_TA_ENVR(transect_ENVR, "Density_of_Oysters_count", "Mean Count of Oysters",
                                                     "Mean Count of Oysters during Winter 2023/2024")
      oyster_density_TA_ENVR
      
    # Stats
      Oyster_ENVR_anova <- aov(Density_of_Oysters_count ~ Site_TA, data = transect_ENVR)
        summary(Oyster_ENVR_anova)
      # p-value = 0.000307
  
  # Presence absence of sea stars 
    plot_ochre_ENVR <- presence_absence_ENVR(transect_ENVR, "Ochre_EO", "Ochre")
      plot_ochre_ENVR
  
    plot_leather_ENVR <- presence_absence_ENVR(transect_ENVR, "Leather_EL", "Leather")
      plot_leather_ENVR
  
    plot_mottled_ENVR <- presence_absence_ENVR(transect_ENVR, "Mottled_EM", "Mottled")
      plot_mottled_ENVR
  #=================================================================================================================================

# Limpet data - aesthetics good, stats done
  #=================================================================================================================================
  # limpet length
    limpet_length_ENVR <- limpet_plots_ENVR(limpet_ENVR, "Mean_Length_mm", 
                                            "Mean Length (mm)", 
                                            "Mean Length and Width of Limpets in Winter 2023/2024")
      limpet_length_ENVR
    
    # Stats
      Length_ENVR_anova <- aov(Mean_Length_mm ~ Site_TA, data = limpet_ENVR)
        summary(Length_ENVR_anova)
        # p-value = 0.000404
        
  # limpet width
    limpet_width_ENVR <- limpet_plots_ENVR(limpet_ENVR, "Mean_Width_mm", 
                                           "Mean Width (mm)", 
                                           "Mean Length and Width of Limpets in Winter 2023/2024")
      limpet_width_ENVR
      
    # Stats
      Width_ENVR_anova <- aov(Mean_Width_mm ~ Site_TA, data = limpet_ENVR)
        summary(Width_ENVR_anova)
        # p-value = 0.000953
  #=================================================================================================================================

# 0.25m Quadrat - aesthetics on algae and invert bad, stats done
  #=================================================================================================================================
  # Mean total and relative percent cover of algae and invertebrates for each site and year - need to fix aesthetics
    # Select data
    percent_cover_ENVR <- select_total_cover(quad0.25m_ENVR, include_year = FALSE, include_season = FALSE)
    
  # Aggregate data by year and site_TA
  all_cover <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ modified_TA, data= percent_cover_ENVR, FUN=mean)
  cover_sd <- aggregate(invert_cover ~ modified_TA, data = percent_cover_ENVR, FUN = function(x) sd(x))
    names(cover_sd)[names(cover_sd) == "invert_cover"] <- "sd"

  percent_cover_all <- merge(all_cover, cover_sd, by = "modified_TA")
  
  ggplot(percent_cover_all, aes(x = modified_TA, y = total_cover)) +
    geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
    geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
    geom_errorbar(aes(ymin = invert_cover - sd/2, ymax = invert_cover + sd/2), 
                  position = position_dodge(width = 0.9), width = 0.5) +
    labs(x = "Site", y = "Percent Cover", title = "Mean Percent Cover of Algae and Invertebrates in Winter 2023/2024", fill = "Organismal Class") +
    scale_fill_viridis(discrete = TRUE, option = "D", alpha = 0.8) +  # Using viridis color palette for fill with lighter shades
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) +
    theme(plot.title=element_text(size=15), #change font size of plot title
          axis.text=element_text(size=11), #change font size of axis text
          axis.title=element_text(size=14), #change font size of axis titles
          legend.text=element_text(size=11), #change font size of legend text
          legend.title=element_text(size=14)) #change font size of legend title 
  
  # Stats
    TCover_ENVR_anova <- aov(Total_Cover ~ Site_TA, data = quad0.25m_ENVR)
      summary(TCover_ENVR_anova)
      # p-value = 0.00474
      
  # bar graph of relative percent cover of algae and count of algae species
    algae_ENVR <- select_algae(quad0.25m_ENVR, include_year = FALSE, include_season = FALSE)
      
    algae_ENVR_agg <- aggregate_count_data(algae_ENVR, "algae_percent_cover", "algae_count")  
    algae_ENVR_total <- merge(algae_ENVR_agg$total, algae_ENVR_agg$percent_sd, by = c("site_TA", "modified_TA"))

    quad_0.25m_ENVR_algae_plot <- ggplot(algae_ENVR_total, aes(x = modified_TA)) +
      geom_bar(aes(y = percent_cover, fill = modified_TA), stat = "identity", width = 0.5) +
      geom_line(aes(y = algae_count*adj, color = "Algae"), group = 1) +
      geom_errorbar(aes(ymin = pmax(percent_cover - algae_percent_cover_sd, 0), 
                      ymax = pmin(percent_cover + algae_percent_cover_sd, 100)), 
                  width = 0.2, position = position_dodge(width = 0.5), color = "black") +
      labs(x = "Site", y = "Percent Cover", 
         title = "Mean Percent Cover and Count of Algae in Winter 2023/2024", 
         fill = "Site", color = "Species Count") +
      scale_y_continuous(limits = c(0, 100), name = "Percent Cover",
                       sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +  # Adjusted primary y-axis scale
      scale_fill_manual(values = site_colors_ENVR) +
      theme_minimal() +
      theme(plot.title=element_text(size=15), #change font size of plot title
          axis.text=element_text(size=11), #change font size of axis text
          axis.title=element_text(size=14), #change font size of axis titles
          legend.text=element_text(size=11), #change font size of legend text
          legend.title=element_text(size=14)) + #change font size of legend title 
      guides(fill = guide_legend(override.aes = list(color = NULL)),
           color = guide_legend(override.aes = list(fill = NULL))) +
      guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))
    print(quad_0.25m_ENVR_algae_plot)
  
  # Stats
    AlgaeCover_ENVR_anova <- aov(Algae_Cover ~ Site_TA, data = quad0.25m_ENVR)
      summary(AlgaeCover_ENVR_anova)
      # p-value = 3.81e-05
  
  # bar graph of relative percent cover of invertebrates and count of mobile and sessile
  invert_quad0.25m_ENVR <- data.frame(site_TA = quad0.25m_ENVR$Site_TA,
                                      modified_TA = quad0.25m_ENVR$modified_TA,
                                     invert_percent_cover = quad0.25m_ENVR$Invertebrates_Cover,
                                     sessile_count = quad0.25m_ENVR$Sessile_Invertebrates_Count_Above + quad0.25m_ENVR$Sessile_Invertebrates_Count_Below,
                                     mobile_count = quad0.25m_ENVR$Mobile_Invertebrates_Count_Above + quad0.25m_ENVR$Mobile_Invertebrates_Count_Below)
  invert_quad0.25m_ENVR <- invert_quad0.25m_ENVR[complete.cases(invert_quad0.25m_ENVR$invert_percent_cover) & is.numeric(invert_quad0.25m_ENVR$invert_percent_cover), ]
  
  invert_ENVR <- aggregate_count_data(invert_quad0.25m_ENVR, "invert_percent_cover", "sessile_count", "mobile_count")  
  invert_ENVR_total <- merge(invert_ENVR$total, invert_ENVR$percent_sd, by = c("site_TA", "modified_TA"))

  quad_0.25_ENVR_invert_plot <- ggplot(data = invert_ENVR_total, aes(x = modified_TA)) +
    geom_bar(aes(y = percent_cover, fill = modified_TA), stat = "identity", width = 0.5) +
    geom_line(aes(y = sessile_count * adj, color = "Sessile"), group = 1) +
    geom_line(aes(y = mobile_count * adj, color = "Mobile"), group = 1) +
    geom_errorbar(aes(ymin = pmax(0, percent_cover - invert_percent_cover_sd), 
                      ymax = pmin(100, percent_cover + invert_percent_cover_sd)), 
                  width = 0.25, position = position_dodge(width = 0.9),
                  color = "black", size = 0.5) +
    labs(x = "Site", y = "Percent Cover", 
         title = "Mean Percent Cover and Count of Invertebrates in Winter 2023/2024", 
         fill = "Site", color = "Species Count") +
    scale_y_continuous(sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +
    scale_fill_manual(values = site_colors_ENVR) +
    scale_color_manual(name = "Species Count", values = c("Sessile" = "red", "Mobile" = "blue")) +
    theme_minimal() +
    theme(plot.title=element_text(size=15), #change font size of plot title
          axis.text=element_text(size=11), #change font size of axis text
          axis.title=element_text(size=14), #change font size of axis titles
          legend.text=element_text(size=11), #change font size of legend text
          legend.title=element_text(size=14))
  
  quad_0.25_ENVR_invert_plot
  
  # Stats
    InvertCover_ENVR_anova <- aov(Invertebrates_Cover ~ Site_TA, data = quad0.25m_ENVR)
      summary(InvertCover_ENVR_anova)
      # p-value = 1.95e-08
  
  # cover of algae vs invertebrates across the transect - winter
  cover_intertidal_height(quad0.25m_ENVR, color_scale = c("low" = "lightblue", "high" = "darkblue"), 
                          plot_title = "Intertidal Height and Mean Percent Cover in Winter 2023/2024")
    # Stats
      # Total Cover
        TCover_Height_ENVR_anova <- aov(Total_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(TCover_Height_ENVR_anova)
          # p-value = 0.000146
      # Algae Cover
        AlgaeCover_Height_ENVR_anova <- aov(Algae_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(AlgaeCover_Height_ENVR_anova)
          # p-value = 0.00353
      # Invertebrates Cover
        InvertCover_Height_ENVR_anova <- aov(Invertebrates_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(InvertCover_Height_ENVR_anova)
          # p-value = 0.718
  #=================================================================================================================================

# Combined SPES and ENVR 400  
#=================================================================================================================================
  # select TA-1, 4 and 6 from SPES data
  subset_TAs <- function(dataframe, column_name, values) {
    subset(dataframe, Site_TA %in% c(1, 4, 6) & Year == 2023)
  }
  
    transect_SPES_TA <- subset_TAs(transect_SPES)
    quad1m_SPES_TA <- subset_TAs(quad1m_SPES)
    quad0.25m_SPES_TA <- subset_TAs(quad0.25m_SPES)
    limpet_SPES_TA <- subset_TAs(limpet_SPES)
  
  # aggregate count data
    aggregate_count_data_seasonal <- function(data, percent_cover_col, count_col, additional_count_col = NULL) {
      # Calculate mean percent cover and rounded mean count
      total_cover_mean <- aggregate(data[[percent_cover_col]] ~ season + site_TA + modified_TA, data = data, FUN = function(x) mean(x))
      names(total_cover_mean) <- c("season", "site_TA", "modified_TA", "percent_cover") # Assign column names
      
      count_rounded_mean <- aggregate(data[[count_col]] ~ season + site_TA + modified_TA, data = data, FUN = function(x) round(mean(x)))
      names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original data column
      
      if (!is.null(additional_count_col)) {
        additional_count_rounded_mean <- aggregate(data[[additional_count_col]] ~ season + site_TA + modified_TA, data = data, FUN = function(x) round(mean(x)))
        names(additional_count_rounded_mean)[length(names(additional_count_rounded_mean))] <- additional_count_col # Assign column name as the original data column
        
        total <- merge(total_cover_mean, count_rounded_mean, by = c("season", "site_TA", "modified_TA"))
        total <- merge(total, additional_count_rounded_mean, by = c("season", "site_TA", "modified_TA"))
      } else {
        total <- merge(total_cover_mean, count_rounded_mean, by = c("season", "site_TA", "modified_TA"))
      }
      
      # Calculate standard deviation of percent cover
      percent_sd <- aggregate(data[[percent_cover_col]] ~ season + site_TA + modified_TA, data = data, FUN = function(x) sd(x), na.action = na.omit)
      names(percent_sd) <- c("season", "site_TA", "modified_TA", paste(percent_cover_col, "sd", sep = "_")) # Assign column names
      
      return(list(total = total, percent_sd = percent_sd))
    }
  
  # seasonal stats
    calculate_seasonality <- function(data1, data2, var_name) {
      # Create data frames for each dataset
        Stats1 <- data.frame(DOY = data1$DOY, 
                           data1[[var_name]])
          names(Stats1)[2] <- var_name
      
        Stats2 <- data.frame(DOY = data2$DOY, 
                           data2[[var_name]])
          names(Stats2)[2] <- var_name
      
      # Combine data frames from both datasets
        Stats_all <- rbind(Stats1, Stats2)
      
      # Fit linear model
        Seasonality <- lm(as.formula(paste(var_name, "~ DOY")), data = Stats_all)
      
      # Return summary of the linear model
        return(summary(Seasonality))
    }

# Transect data - fix aesthetics, stats done
  #=================================================================================================================================
    names(transect_ENVR)[names(transect_ENVR) == "Density_of_Sea_Stars_count"] <- "Density_of_Sea_Stars_Count"
    
    ss_agg_SPES <- aggregate(cbind(Density_of_Sea_Stars_Count) ~ season + Site_TA + modified_TA, data = transect_SPES_TA, FUN = mean, na.rm = TRUE)
    
    # Calculate standard deviation of sea stars count by season and Site_TA for SPES data
    ss_agg_sd_SPES <- aggregate(cbind(Density_of_Sea_Stars_Count) ~ season + Site_TA + modified_TA, 
                                data = transect_SPES_TA, 
                                FUN = function(x) sd(x, na.rm = TRUE))
      names(ss_agg_sd_SPES)[names(ss_agg_sd_SPES) == "Density_of_Sea_Stars_Count"] <- "sd_ss"

    ss_agg_ENVR <- aggregate(cbind(Density_of_Sea_Stars_Count) ~ season + Site_TA + modified_TA, data = transect_ENVR, FUN = mean, na.rm = TRUE)
    
    # Calculate standard deviation of sea stars count by season and Site_TA for ENVR data
    ss_agg_sd_ENVR <- aggregate(cbind(Density_of_Sea_Stars_Count) ~ season + Site_TA + modified_TA, 
                                data = transect_ENVR, 
                                FUN = function(x) sd(x, na.rm = TRUE))
    
    # Rename standard deviation column for consistency
    names(ss_agg_sd_ENVR)[names(ss_agg_sd_ENVR) == "Density_of_Sea_Stars_Count"] <- "sd_ss"
    
    # Combine aggregated data and standard deviation data for both datasets
    ss_agg_combined <- rbind(ss_agg_SPES, ss_agg_ENVR)
    ss_agg_sd_combined <- rbind(ss_agg_sd_SPES, ss_agg_sd_ENVR)
    
    ss_merge <- merge(ss_agg_combined, ss_agg_sd_combined, by = c("season", "Site_TA", "modified_TA"))
    
    plot_ss <- function(data, y_variable, title, x_label, y_label) {
      ggplot(data, aes(x = factor(modified_TA), y = !!sym(y_variable), fill = season)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax(!!sym(y_variable) - sd_ss, 0), 
                          ymax = !!sym(y_variable) + sd_ss),
                      position = position_dodge(width = 0.9), 
                      width = 0.25) +
        labs(x = x_label, y = y_label, title = title) +
        scale_fill_discrete(name = "Season") +
        theme_minimal() +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title
    }
    
    plot_ss(data = ss_merge, y_variable = "Density_of_Sea_Stars_Count", 
            title = "Density of Sea Stars from May 2023 - February 2024", 
            x_label = "Site", y_label = "Density of Sea Stars (Count)")
    
    # stats
      calculate_seasonality(transect_SPES_TA, transect_ENVR, 
                          "Density_of_Sea_Stars_Count")
        # Adjusted R-squared = 0.04474
        # p-value = 0.1135
      
  #=================================================================================================================================
    
# Limpet data - fix aesthetics, stats done
  #=================================================================================================================================
    # Aggregate data by Site_TA
    limp_agg_SPES <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA + modified_TA, data = limpet_SPES_TA, FUN = mean, na.action = na.omit)
    limp_agg_sd_SPES <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA + modified_TA, 
                             data = limpet_SPES_TA, 
                             FUN = function(x) sd(x, na.rm = TRUE),
                             na.action = na.omit)
      names(limp_agg_sd_SPES)[names(limp_agg_sd_SPES) == "Mean_Length_mm"] <- "sd_length"
      names(limp_agg_sd_SPES)[names(limp_agg_sd_SPES) == "Mean_Width_mm"] <- "sd_width"

    limp_agg_ENVR <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA + modified_TA, data = limpet_ENVR, FUN = mean, na.action = na.omit)
    limp_agg_sd_ENVR <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA + modified_TA, data = limpet_ENVR, FUN = function(x) sd(x, na.rm = TRUE), na.action = na.omit)
      names(limp_agg_sd_ENVR)[names(limp_agg_sd_ENVR) == "Mean_Length_mm"] <- "sd_length"
      names(limp_agg_sd_ENVR)[names(limp_agg_sd_ENVR) == "Mean_Width_mm"] <- "sd_width"
    
    limp_agg_combined <- rbind(limp_agg_SPES, limp_agg_ENVR)
    limp_agg_sd_combined <- rbind(limp_agg_sd_SPES, limp_agg_sd_ENVR)
      
    limpet_merge <- merge(limp_agg_combined, limp_agg_sd_combined, by = c("season", "Site_TA", "modified_TA"))

    # plot length
   plot_limpet_length_season <- ggplot(limpet_merge, aes(x = factor(modified_TA), y = Mean_Length_mm, fill = season)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax(Mean_Length_mm - sd_length, 0), 
                          ymax = Mean_Length_mm + sd_length),
                      position = position_dodge(width = 0.9), 
                      width = 0.25) +
        labs(x = "Site", y = "Mean Length (mm)", title = "Mean Length of Limpets from May 2023 - February 2024") +
        scale_fill_discrete(name = "Season") +
        theme_minimal() + 
        ylim(0, 30) +
        theme(plot.title=element_text(size=15), #change font size of plot title
           axis.text=element_text(size=11), #change font size of axis text
           axis.title=element_text(size=14), #change font size of axis titles
           legend.text=element_text(size=11), #change font size of legend text
           legend.title=element_text(size=14)) #change font size of legend title
   
    plot_limpet_length_season
      
    # plot width
    plot_limpet_width_season <- ggplot(limpet_merge, aes(x = factor(modified_TA), y = Mean_Width_mm, fill = season)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = pmax(Mean_Width_mm - sd_width, 0), 
                        ymax = Mean_Width_mm + sd_length),
                    position = position_dodge(width = 0.9), 
                    width = 0.25) +
      labs(x = "Site", y = "Mean Width (mm)", title = "Mean Width of Limpets from May 2023 - February 2024") +
      scale_fill_discrete(name = "Season") +
      theme_minimal() +
      ylim(0, 30) +
      theme(plot.title=element_text(size=15), #change font size of plot title
            axis.text=element_text(size=11), #change font size of axis text
            axis.title=element_text(size=14), #change font size of axis titles
            legend.text=element_text(size=11), #change font size of legend text
            legend.title=element_text(size=14)) #change font size of legend title
    
    plot_limpet_width_season  
    
    
    # length
      plot_ss(limpet_merge, "Mean_Length_mm",
              title = "Mean Length of Limpets from May 2023 - February 2024", 
              x_label = "Site TA", y_label = "Mean Length of Limpets (mm)")
      # width
      plot_ss(limpet_merge, "Mean_Width_mm")
    
    # stats
      # Length
        calculate_seasonality(limpet_SPES_TA, limpet_ENVR, 
                              "Mean_Length_mm")
          # Adjusted R-squared = 0.7297
          # p-value = 1.963e-13
        
      # Width
        calculate_seasonality(limpet_SPES_TA, limpet_ENVR, 
                                "Mean_Width_mm")
            # Adjusted R-squared = 0.7352
            # p-value = 1.287e-13
  #=================================================================================================================================

# 0.25m data - need to fix percent cover graphs and make individual graphs, stats done
  #=================================================================================================================================
  #Percent cover 
    # Select Data
      cover_SPES_seasonal <- select_total_cover(quad0.25m_SPES, include_year = FALSE, include_season = TRUE)
      cover_ENVR_seasonal <- select_total_cover(quad0.25m_ENVR, include_year = FALSE, include_season = TRUE)
      
    # Aggregate SPES and ENVR data and combine into one dataframe
      cover_agg_SPES_seasonal <- aggregate_cover_data(cover_SPES_seasonal, cover_cols = c("total_cover", "algae_cover", "invert_cover"))
      cover_agg_ENVR_seasonal <- aggregate_cover_data(cover_ENVR_seasonal, cover_cols = c("total_cover", "algae_cover", "invert_cover"))
      cover_agg_total_seasonal <- rbind(cover_agg_SPES_seasonal, cover_agg_ENVR_seasonal) 
      
    # Plot total cover according to the season
      total_cover_seasonal <- ggplot(cover_agg_total_seasonal, aes(x = factor(modified_TA), y = total_cover)) +
        geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - cover_sd/2, ymax = invert_cover + cover_sd/2,
                          group = site_TA),  # Group by site_TA
                      position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
        facet_grid(~season, scales = "free_x") +  # Facet by season with different bar graphs for each site
        labs(x = "Site", y = "Percent Cover", title = "Mean Total Percent Cover of Algae and Invertebrates from May 2023 - February 2024", fill = "Organismal Class") +
        scale_fill_viridis(discrete = TRUE, option = "D", alpha = 0.8) +  # Using viridis color palette for fill with lighter shades
        theme_minimal() +
        scale_y_continuous(limits = c(0, 100)) +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14),
              strip.text = element_text(size = 11))
      total_cover_seasonal

      # stats
        # Total cover
          calculate_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, 
                                "Total_Cover")
              # Adjusted R-squared = -0.004734
              # p-value = 0.6075
          
          # Relative Algae
            calculate_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, 
                                    "Adjusted_Algae_Cover")
                # Adjusted R-squared = -0.006537
                # p-value = 0.8097
              
          # Relative Invertebrates
            calculate_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, 
                                  "Adjusted_Invert_Cover")
                  # Adjusted R-squared = -0.006866
                  # p-value = 0.916
              
  # Seasonal Algae Cover and Count
    # Select data
      algae_SPES_seasonal <- select_algae(quad0.25m_SPES_TA, include_year = FALSE, include_season = TRUE)
      algae_ENVR_seasonal <- select_algae(quad0.25m_ENVR, include_year = FALSE, include_season = TRUE)
    
    # Aggregate and combine data  
      algae_SPES_seasonal_total <- aggregate_count_data_seasonal(algae_SPES_seasonal, "algae_percent_cover", "algae_count")  
      algae_ENVR_seasonal_total <- aggregate_count_data_seasonal(algae_ENVR_seasonal, "algae_percent_cover", "algae_count")  
        
      algae_agg_combined <- rbind(algae_SPES_seasonal_total$total, algae_ENVR_seasonal_total$total)
      algae_agg_sd_combined <- rbind(algae_SPES_seasonal_total$percent_sd, algae_ENVR_seasonal_total$percent_sd)
        
      algae_merge_season <- merge(algae_agg_combined, algae_agg_sd_combined, by = c("season", "site_TA", "modified_TA"))
    
    # Plot Data    
      algae_cover_seasonal <- ggplot(algae_merge_season, aes(x = factor(modified_TA), y = algae_percent_cover)) +
            geom_bar(aes(y = algae_percent_cover, fill = season), stat = "identity", width = 0.5) +
            geom_point(aes(y = algae_count*adj, color = "Algae")) +
            geom_errorbar(aes(ymin = pmax(0, algae_percent_cover - algae_percent_sd),
                              ymax = pmin(100, algae_percent_cover + algae_percent_sd)),
                          width = 0.25, position = position_dodge(width = 0.9),
                          color = "black", linewidth = 0.5) +  # Adjust error bar aesthetics
            facet_grid(~season, scales = "free_x") +  # Facet by season with different bar graphs for each site
            labs(x = "Site", y = "Percent Cover", title = "Mean Percent Cover and Count of Algae from May 2023 - February 2024", fill = "Site TA") +
            scale_fill_discrete(name = "Season") +
            scale_color_manual(name = "Species Count",
                               values = c("Algae" = "red"),
                               labels = c("Algae")) +  # Define the color and label for the legend
            theme_minimal() +
            scale_y_continuous(limits = c(0, 100), name = "Percent Cover",
                               sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +
            guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
            theme(plot.title=element_text(size=15), #change font size of plot title
                  axis.text=element_text(size=11), #change font size of axis text
                  axis.title=element_text(size=14), #change font size of axis titles
                  legend.text=element_text(size=11), #change font size of legend text
                  legend.title=element_text(size=14),
                  strip.text = element_text(size = 11))

        algae_cover_seasonal
          
  # Seasonal Invertebrate Cover and Count
    # Select data
      invert_SPES_seasonal <- select_invertebrate(quad0.25m_SPES_TA, include_year = FALSE, include_season = TRUE)
      invert_ENVR_seasonal <- select_invertebrate(quad0.25m_ENVR, include_year = FALSE, include_season = TRUE)
      
      # Aggregate the data according to site and season   
          invert_SPES_seasonal_total <- aggregate_count_data_seasonal(invert_SPES_seasonal, "invert_percent_cover", "sessile_count", "mobile_count")  
          invert_ENVR_seasonal_total <- aggregate_count_data_seasonal(invert_ENVR_seasonal, "invert_percent_cover", "sessile_count", "mobile_count")  
      
      # Combine the standard deviation aggregation with the percent cover and count columns    
          invert_agg_combined <- rbind(invert_SPES_seasonal_total$total, invert_ENVR_seasonal_total$total)
          invert_agg_sd_combined <- rbind(invert_SPES_seasonal_total$percent_sd, invert_ENVR_seasonal_total$percent_sd)
      
      # Combine all the aggregated SPES data with the aggregated ENVR data    
          invert_merge_season <- merge(invert_agg_combined, invert_agg_sd_combined, by = c("season", "site_TA", "modified_TA"))
      
      # plot the invertebrate cover and count from spring to winter    
          invert_cover_seasonal <- ggplot(invert_merge_season, aes(x = factor(modified_TA), y = percent_cover)) +
            geom_bar(aes(y = percent_cover, fill = season), stat = "identity", width = 0.5) +
            geom_point(aes(y = sessile_count*adj, color = "Sessile")) +
            geom_point(aes(y = mobile_count*adj, color = "Mobile")) +
            geom_errorbar(aes(ymin = pmax(0, percent_cover - invert_percent_cover_sd),
                              ymax = pmin(100, percent_cover + invert_percent_cover_sd)),
                          width = 0.25, position = position_dodge(width = 0.9),
                          color = "black", linewidth = 0.5) +
            facet_grid(~season, scales = "free_x", labeller = as_labeller(function(x) stringr::str_remove_all(x, "season: "))) +  # Adjusted labeller function
            labs(x = "Site", y = "Percent Cover", title = "Mean Percent Cover and Count of Invertebrates from May 2023 - February 2024", fill = "Season") +
            scale_color_manual(name = "Species Count", values = c("Sessile" = "red", "Mobile" = "blue")) +
            theme_minimal() +
            theme(plot.title=element_text(size=15),
                  axis.text=element_text(size=11),
                  axis.title=element_text(size=14),
                  legend.text=element_text(size=11),
                  legend.title=element_text(size=14),
                  strip.text = element_text(size = 11)) +  
            scale_y_continuous(limits = c(0, 100), name = "Percent Cover",
                               sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) +
            guides(fill = guide_legend(order = 2), color = guide_legend(order = 1))
          
          invert_cover_seasonal
  #=================================================================================================================================
  
# Abiotic Analysis
#=================================================================================================================================
# Find average time of lowest low tide  per month 
  # Time of lowest low tide height 
    monthly_low_tide_time <- tide %>%
      group_by(month, Year) %>%
      summarise(hour_of_min_tide = hour[which.min(SLEV_metres)])
    average_low_tide_time <- monthly_low_tide_time %>%
      group_by(month) %>%
      summarise(low_tide_time = mean(hour_of_min_tide))
  
  # Min temperature 
    summarize_monthly_temperature <- function(weather, summary_type = "max") {
      if(summary_type == "max") {
        summarized_data <- weather %>%
          group_by(month, Year) %>%
          summarise(Max_Temp = max(AirTemp_degC)) %>%
          group_by(month) %>%
          summarise(Avg_Max_Temp = mean(Max_Temp))
      } else if(summary_type == "min") {
        summarized_data <- weather %>%
          group_by(month, Year) %>%
          summarise(Min_Temp = min(AirTemp_degC)) %>%
          group_by(month) %>%
          summarise(Avg_Min_Temp = mean(Min_Temp))
      } else {
        stop("Invalid summary type. Please choose 'max' or 'min'.")
      }
      
      return(summarized_data)
    }
    
    monthly_min_temperature <- summarize_monthly_temperature(weather, summary_type = "min")
    monthly_max_temperature <- summarize_monthly_temperature(weather, summary_type = "max")
  
  # merge the abotic dfs by month 
    monthly_temperature_data <- merge(monthly_max_temperature, monthly_min_temperature, by = "month")
    monthly_abiotic_data <- merge(monthly_temperature_data, average_low_tide_time, by = "month")

  # plotting - time of minimum tide height - need to figure out temperature scale 
    monthly_abiotic_data$month <- factor(monthly_abiotic_data$month, levels = 1:12,
                                         labels = c("January", "February", "March", "April", "May", "June",
                                                    "July", "August", "September", "October", "November", "December"))
    file_path <- "~/Desktop/abiotic.csv"
    write.csv(monthly_abiotic_data, file = file_path, row.names = FALSE)
    
  # Create the plot
    ggplot(monthly_abiotic_data, aes(x = month)) +
      geom_bar(aes(y = low_tide_time), stat = "identity") +
      geom_line(aes(y = Avg_Max_Temp, group = 1, color = "Maximum"), show.legend = TRUE) +
      geom_line(aes(y = Avg_Min_Temp, group = 1, color = "Minimum"), show.legend = TRUE) +
      scale_y_continuous(name = "Temperature (°C)", limits = c(-5, 33), sec.axis = sec_axis(~. - 5, name = "Low Tide Time (hrs)")) +
      coord_cartesian(ylim = c(-5, 33)) +  
      labs(x = "Month",
           y = "Low Tide Time (hrs)",
           title = "Average Time of the Lowest Low Tide and Average Maximum and Minimum Temperatures",
           color = "Temperature") +
      theme_minimal()
    
#=================================================================================================================================
