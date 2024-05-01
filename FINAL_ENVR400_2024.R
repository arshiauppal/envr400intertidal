# ENVR 400 2024 Data Analysis

# Required packages
#=================================================================================================================================
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(ggplot2)
  require(lubridate)
  require(reshape2)
  require(viridis)
  require(RColorBrewer)
#=================================================================================================================================

# Read data
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
    
# Clean data
#===================================================================================================================================
# Functions
  #=================================================================================================================================
  # Extracts the year from the date column to create a new column
      # df: the dataframe that the year needs to be extracted from
      # date_column_name: the date column name from the provided data
    add_year_column <- function(df, date_column_name) {
      df[[date_column_name]] <- format(as.Date(df[[date_column_name]]), "%d/%m/%Y") # format original date column to be in day/month/year
      df$Year <- format(as.Date(df[[date_column_name]], "%d/%m/%Y"), "%Y") # from formatted date column, pulls out year
      return(df)
    }
    
  # Converts columns containing 0 and 1 (presence/absence) data to true and false (logical data) to indicate presence and absence of species.
    # df: the dataframe that contains the 0 and 1 data
    # start_col_index: the first column which needs to be converted 
    # end_col_index: the first column which needs to be converted 
    convert_to_logical <- function(df, start_col_index, end_col_index) {
      df <- df %>%
        mutate(across(start_col_index:end_col_index, as.logical)) #  used to indicate the start and end of columns that need to be changed to true/false
      return(df)
    }
    
  # Change site TA columns to character values. Adds an additional column which has TA- in front of the number to be used within visualizations
    # df: the dataframe that has the columns to be converted
    modify_TA_column <- function(df) {
      modified_df <- df %>%
        mutate(modified_TA = paste0("TA-", as.character(Site_TA)))
      return(modified_df)
    }

  # Get the season and month of collection 
    # Extracts the month column from the date and determines the season based on the month to use for seasonal analysis
    # date: the date column from the respective dataframe (in the format df$date)
    get_season <- function(date){
      date <- as.Date(date) # make sure the input has date format
      mon <- months.Date(date, abbreviate = TRUE) # get month abbreviation
    
      # use function to define which months fall under winter (Dec, Jan, Feb), spring (Mar, Apr, May),
      # summer (Jun, Jul, Aug), and fall (Sep, Oct, Nov)
      ifelse(mon %in% c("Dec", "Jan", "Feb"), "Winter",
            ifelse(mon %in% c("Mar", "Apr", "May"), "Spring",
                    ifelse(mon %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
    }
  
  # Divide the transect length into low, medium and high categorizations for intertidal height analysis 
    # df: the dataframe that has the intertudal height column
    # column_name: the column name to divide the transect height from 
    intertidal_height <- function(df, column_name) {
      # Define breaks and labels
      breaks <- c(0, 10, 20, 30)
      labels <- c("low", "medium", "high")
      df$intertidal_height <- cut(df[[column_name]], breaks = breaks, labels = labels, include.lowest = TRUE)
      return(df)
    }
    
  # Calculation of proportional percent cover - calculates the proportion of algae and invertebrate percent cover to the observed total percent cover
    # df: the dataframe which contains the percent cover data
    # algae_cover_col: algae percent cover column name
    # inverterbrates_cover_col: inverterbrates percent cover column name
    # total_cover_col: total percent cover column name
    proportional_percent_cover <- function(df, algae_cover_col, invertebrates_cover_col, total_cover_col) {
      df$Adjusted_Algae_Cover <- df[[algae_cover_col]] / (df[[algae_cover_col]] + df[[invertebrates_cover_col]]) * df[[total_cover_col]]
      df$Adjusted_Invert_Cover <- df[[invertebrates_cover_col]] / (df[[algae_cover_col]] + df[[invertebrates_cover_col]]) * df[[total_cover_col]]
      return(df)
    }
    
  #=================================================================================================================================
  
# SPES data cleaning
  #=================================================================================================================================
  # Add year column
    transect_SPES <- add_year_column(transect_SPES, "Date")
    quad1m_SPES <- add_year_column(quad1m_SPES, "Date")
    quad0.25m_SPES <- add_year_column(quad0.25m_SPES, "Date")
    limpet_SPES <- add_year_column(limpet_SPES, "Date")
    
  # Change 0 and 1 values to true and false (presence and absence) - note if any other columns are added have to change range of columns
    quad0.25m_SPES <- convert_to_logical(quad0.25m_SPES, 14, 44)
    transect_SPES <- convert_to_logical(transect_SPES, 12, 14)

  # Change site_TA column to a character and add "TA-#" column
    transect_SPES <- modify_TA_column(transect_SPES)
    quad1m_SPES <- modify_TA_column(quad1m_SPES)
    quad0.25m_SPES <- modify_TA_column(quad0.25m_SPES)
    limpet_SPES <- modify_TA_column(limpet_SPES)
    
  # Add season and month column - ensure month column is in date format
    transect_SPES$season <- get_season(transect_SPES$Date)
    transect_SPES$month <- month(as.Date(transect_SPES$Date))

    quad1m_SPES$season <- get_season(quad1m_SPES$Date)
    quad1m_SPES$month <- month(as.Date(quad1m_SPES$Date))

    quad0.25m_SPES$season <- get_season(quad0.25m_SPES$Date)
    quad0.25m_SPES$month <- month(as.Date(quad0.25m_SPES$Date))

    limpet_SPES$season <- get_season(limpet_SPES$Date)
    limpet_SPES$month <- month(as.Date(limpet_SPES$Date))
  
  # Determine intertidal height of the transect line
    quad0.25m_SPES <- intertidal_height(quad0.25m_SPES, "Transect_Point_m")
    quad1m_SPES <- intertidal_height(quad1m_SPES, "Transect_Point_m")
    
  # Calculate proportional percent cover of algae and invertebrates
    quad0.25m_SPES <- proportional_percent_cover(quad0.25m_SPES, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
    
  # Create a DOY column that takes the existing date column and rearranges it to date, month, year
    transect_SPES$DOY <- yday(dmy(transect_SPES$Date))
    quad1m_SPES$DOY <- yday(dmy(quad1m_SPES$Date))
    quad0.25m_SPES$DOY <- yday(dmy(quad0.25m_SPES$Date))
    limpet_SPES$DOY <- yday(dmy(limpet_SPES$Date))
    
  #=================================================================================================================================

# ENVR 400 2024 data cleaning
  #=================================================================================================================================
  # Add year column
    transect_ENVR <- add_year_column(transect_ENVR, "Date") 
    quad0.25m_ENVR <- add_year_column(quad0.25m_ENVR, "Date")
    limpet_ENVR <- add_year_column(limpet_ENVR, "Date") 
  
  # Change 0 and 1 values to true and false (presence and absence) -  note if any other columns are added have to change range of columns
    transect_ENVR <- convert_to_logical(transect_ENVR, 14, 16)
    
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 15, 26)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 29, 36)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 39, 44)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 47, 54)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 57, 62)
  
  # Change site_TA column to a character and add "TA-#" column
    transect_ENVR <- modify_TA_column(transect_ENVR)
    quad0.25m_ENVR <- modify_TA_column(quad0.25m_ENVR)
    limpet_ENVR <- modify_TA_column(limpet_ENVR)
    
  # Add season and month column - ensure month column is in date format
    transect_ENVR$season <- get_season(transect_ENVR$Date)
    transect_ENVR$month <- month(as.Date(transect_ENVR$Date))
  
    quad0.25m_ENVR$season <- get_season(quad0.25m_ENVR$Date)
    quad0.25m_ENVR$month <- month(as.Date(quad0.25m_ENVR$Date))
  
    limpet_ENVR$season <- get_season(limpet_ENVR$Date)
    limpet_ENVR$month <- month(as.Date(limpet_ENVR$Date))
    
  # Determine intertidal height of the transect line
    quad0.25m_ENVR <- intertidal_height(quad0.25m_ENVR, "Transect_Point_M")
    
  # Calculate proportional percent cover of algae and invertebrates
    quad0.25m_ENVR <- proportional_percent_cover(quad0.25m_ENVR, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
  
  # Create a DOY column that takes the existing date column and rearranges it to date, month, year - note since data went into the new year DOY was additive above 365
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
      
  #=================================================================================================================================

# Abiotic data cleaning 
  #=================================================================================================================================
  # Separate date and time from tide data
    tide <- tide %>%
      mutate(date = as.Date(Obs_date),
          time = format(strptime(Obs_date, format = "%Y-%m-%d %H:%M"), "%H:%M"))
    tide <- add_year_column(tide, "date")
  
  # Separate the hour from the tide time
    tide$time <- as.POSIXct(tide$time, format = "%H:%M")
    tide$hour <- format(tide$time, "%H")
    tide$hour <- as.numeric(tide$hour)
  
  # Add season and month column - ensure month column is in date format
    tide$season <- get_season(tide$date)
    tide$month <- month(as.Date(tide$date))

  # Create date column in the format day/month/year and create a year, month and season column
    weather$date <- ymd(weather$Dates)
    weather <- add_year_column(weather, "date")

    weather$season <- get_season(weather$date)
    weather$month <- month(as.Date(weather$date))
  #=================================================================================================================================

# General visualization functions
  #================================================================================================================================
  # Plot the total, algae and invertebrate percent cover across the intertidal zone height
    # df: the dataframe that contains the percent cover data
    # color_scale: the colour scale for the plot
    # plot_title: the title of the plot
    cover_intertidal_height <- function(df, color_scale = c("low" = "lightblue", "high" = "darkblue"), plot_title = "Intertidal Height and Mean Percent Cover") {
      selected_data <- data.frame(
        intertidal_height = df$intertidal_height,
        Total = df$Total_Cover,
        Algae = df$`Adjusted_Algae_Cover`,
        Invertebrates = df$`Adjusted_Invert_Cover`
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
        scale_y_discrete(limits = c("low", "medium", "high")) +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title 
        
    }
  
  # Create a dataframe for the total percent cover plot
    # df: the dataframe that contains the total percent cover data
    # include_year: selection if the dataframe should include the year or not - TRUE for SPES data for yearly analysis
    # include_season: selection if the dataframe should include the season or not - TRUE for SPES and ENVR data for seasonal analysis
    select_total_cover <- function(df, include_year = TRUE, include_season = TRUE) {
      # Subset the data based on the selected options
      cover <- data.frame(
        site_TA = df$Site_TA,
        modified_TA = df$modified_TA,
        year = if(include_year) df$Year else NULL,
        season = if(include_season) df$season else NULL,
        total_cover = df$Total_Cover,
        algae_cover = df$Adjusted_Algae_Cover,
        invert_cover = df$Adjusted_Invert_Cover
      )
      
      # Remove rows with missing or non-numeric values in any of the cover columns
      cover <- cover[complete.cases(cover$total_cover) & complete.cases(cover$algae_cover) & complete.cases(cover$invert_cover), ]
      
      return(cover)
    }
  
  # Create a dataframe for the alage percent cover and species count plot
    # df: the dataframe that contains the total percent cover data
    # include_year: selection if the dataframe should include the year or not - TRUE for SPES data for yearly analysis
    # include_season: selection if the dataframe should include the season or not - TRUE for SPES and ENVR data for seasonal analysis
    select_algae <- function(df, include_year = TRUE, include_season = TRUE) {
      # Subset the data based on the selected options
      algae_data <- data.frame(
        site_TA = df$Site_TA,
        modified_TA = df$modified_TA,
        year = if(include_year) df$Year else NULL,
        season = if(include_season) df$Season else NULL,
        algae_percent_cover = df$Algae_Cover,
        algae_count = df$Algae_Count
      )
      
      # Remove rows with missing or non-numeric values in algae_percent_cover
      algae_data <- algae_data[complete.cases(algae_data$algae_percent_cover) & is.numeric(algae_data$algae_percent_cover), ]
      
      return(algae_data)
    }
    
  # Create a dataframe for the invertebrate percent cover and species count plot
    # df: the dataframe that contains the total percent cover data
    # include_year: selection if the dataframe should include the year or not - TRUE for SPES data for yearly analysis
    # include_season: selection if the dataframe should include the season or not - TRUE for SPES and ENVR data for seasonal analysis
    select_invertebrate <- function(df, include_year = TRUE, include_season = TRUE) {
      # Subset the data based on the selected options
      invertebrates_data <- data.frame(
        site_TA = df$Site_TA,
        modified_TA = df$modified_TA,
        year = if(include_year) df$Year else NULL,
        season = if(include_season) df$Season else NULL,
        invert_percent_cover = df$Invertebrates_Cover,
        sessile_count = df$Sessile_Invertebrates_Count,
        mobile_count = df$Mobile_Invertebrates_Count
      )
      
      # Remove rows with missing or non-numeric values in invert_percent_cover
      invertebrates_data <- invertebrates_data[complete.cases(invertebrates_data$invert_percent_cover) & is.numeric(invertebrates_data$invert_percent_cover), ]
      
      return(invertebrates_data)
    }
    
  # Aggregate algae and invertebrate percent cover and count data for their respective plots
    # df: The dataframe which contains the data to be aggregated
    # percent_cover_col: The column which contains the algae or invertebrates percent cover data
    # count_col: The column which contains the algae species count data or sessile species count data
    # additional_count_col: Only used if aggregating invertebrate data - lets you aggreate the additional mobile species count data
    aggregate_count_data <- function(df, percent_cover_col, count_col, additional_count_col = NULL) {
      # Check if "year" column exists in the df
      if ("year" %in% colnames(df)) {
        # If "year" column exists, aggregate with it
        total_cover_mean <- aggregate(df[[percent_cover_col]] ~ site_TA + modified_TA + year, data = df, FUN = function(x) mean(x))
        names(total_cover_mean) <- c("site_TA", "modified_TA", "year", "percent_cover") # Assign column names
        
        count_rounded_mean <- aggregate(df[[count_col]] ~ site_TA + modified_TA + year, data = df, FUN = function(x) round(mean(x)))
        names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original df column
        
        if (!is.null(additional_count_col)) {
          additional_count_rounded_mean <- aggregate(df[[additional_count_col]] ~ site_TA + modified_TA + year, data = df, FUN = function(x) round(mean(x)))
          names(additional_count_rounded_mean)[length(names(additional_count_rounded_mean))] <- additional_count_col # Assign column name as the original df column
          
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
          total <- merge(total, additional_count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
        } else {
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA", "year"))
        }
        
        # Calculate standard deviation of percent cover
        percent_sd <- aggregate(df[[percent_cover_col]] ~ site_TA + modified_TA + year, data = df, FUN = function(x) sd(x), na.action = na.omit)
        names(percent_sd) <- c("site_TA", "modified_TA", "year", paste(percent_cover_col, "sd", sep = "_")) # Assign column names
        
        return(list(total = total, percent_sd = percent_sd))
        
      } else {
        # If "year" column doesn't exist, aggregate without it
        total_cover_mean <- aggregate(df[[percent_cover_col]] ~ site_TA + modified_TA, data = df, FUN = function(x) mean(x))
        names(total_cover_mean) <- c("site_TA", "modified_TA", "percent_cover") # Assign column names
        
        count_rounded_mean <- aggregate(df[[count_col]] ~ site_TA + modified_TA, data = df, FUN = function(x) round(mean(x)))
        names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original df column
        
        if (!is.null(additional_count_col)) {
          additional_count_rounded_mean <- aggregate(df[[additional_count_col]] ~ site_TA + modified_TA, data = df, FUN = function(x) round(mean(x)))
          names(additional_count_rounded_mean)[length(names(additional_count_rounded_mean))] <- additional_count_col # Assign column name as the original df column
          
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA"))
          total <- merge(total, additional_count_rounded_mean, by = c("site_TA", "modified_TA"))
        } else {
          total <- merge(total_cover_mean, count_rounded_mean, by = c("site_TA", "modified_TA"))
        }
        
        # Calculate standard deviation of percent cover
        percent_sd <- aggregate(df[[percent_cover_col]] ~ site_TA + modified_TA, data = df, FUN = function(x) sd(x), na.action = na.omit)
        names(percent_sd) <- c("site_TA", "modified_TA", paste(percent_cover_col, "sd", sep = "_")) # Assign column names
        
        return(list(total = total, percent_sd = percent_sd))
      }
    }
    
  # Adjustment for secondary y-axis for percent cover and count plots 
    adj <- 25
  #================================================================================================================================
    
# SPES Data Analysis
#=================================================================================================================================
# SPES visualization functions
  #=================================================================================================================================
  # Generate a plot of the mean count/size measurements of an organism at the three sampled sites
    # df: The dataframe that contains the variable being plotted
    # varname: The variable to be plotted
    # plot_title: The title of the plot
    # plot_yaxis: The title of the y-axis
    # y_axis_limits: Specify the limits of the y axis in the format c(lower limit, upper limit)
    plot_SPES <- function(df, varname, plot_title, plot_yaxis, y_axis_limits){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = df$modified_TA,
                           year = df$Year,
                           var = df[,varname])
      
      # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
      df_var_mean <- aggregate(var ~ year + site_TA, data=df_var, FUN = function(x) round(mean(x)))
        names(df_var_mean)[3] <- "mean"
      df_var_sd <- aggregate(var ~ year + site_TA, data = df_var, FUN = function(x) sd(x))
        names(df_var_sd)[3] <- "sd"
      
      df_var_all <- merge(df_var_mean, df_var_sd, by = c("year", "site_TA"))
      
      ggplot(data = df_var_all, aes(x = site_TA, y = mean, group = year, fill = year)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        geom_errorbar(aes(ymin = pmax(mean - sd, 0), ymax = mean + sd),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(plot_yaxis) + xlab("Site") + labs(fill = "Year") +
        scale_fill_viridis(discrete = TRUE) +
        labs(title = plot_title) +
        coord_cartesian(ylim = y_axis_limits) +  # specify y-axis limits
        theme_minimal() +
        theme(plot.title=element_text(size=15), #change font size of plot title
              axis.text=element_text(size=11), #change font size of axis text
              axis.title=element_text(size=14), #change font size of axis titles
              legend.text=element_text(size=11), #change font size of legend text
              legend.title=element_text(size=14)) #change font size of legend title   
    }

  # Generate a presence/absence plot of sea star species across all sites and time
    # df: The dataframe which contains the variable being plotted
    # species_column: The column which contains the species presence/absence data
    presence_absence_SPES <- function(df, species_column) {
      df <- data.frame(Year = df$Year,
          Site_TA = df$Site_TA,
          Species = df[,species_column])
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
        theme(panel.grid.major = element_line(color = "black", linewidth = 0.5),  # Adjusted to use linewidth instead of size
              panel.grid.minor = element_blank(),  # Remove minor gridlines
              axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
        ) +
        scale_y_discrete(breaks = unique(df_grid$Site_TA)) # Set breaks for y-axis
    }
  
  # Generate a plot of species count along intertidal height
    # df: The dataframe which contains the variable being plotted
    # count_variable: The organismal count variable to be plotted
    # scale_fill: The colour scheme for the plot
    # plot_title: The title of the plot
    plot_count_intertidal_height <- function(df, count_variable, scale_fill = c("lightblue", "darkblue"), plot_title) {
      # Aggregate the data for the specified count variable
      agg_data <- aggregate(df[[count_variable]], by = list(df$intertidal_height), FUN = function(x) round(mean(x, na.rm = TRUE)))
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
  
# Transect data visualizations and statistics
  #=================================================================================================================================
  # Density of sea stars per TA from summer 2019-2023
    SS_density_SPES <- plot_SPES(transect_SPES, "Density_of_Sea_Stars_Count", 
                                            "Mean Count of Sea Stars in the Spring/Summer from 2019-2023 ", 
                                            "Mean Count of Sea Stars", c(0, 60))
      SS_density_SPES
      
      # Statistical analysis - ANOVA test
        # Differences across years
          SS_Year_anova <- aov(Density_of_Sea_Stars_Count ~ Year, data = transect_SPES)
            summary(SS_Year_anova)
            # p-value = 0.406
        # Differences across sites
            SS_Sites_anova <- aov(Density_of_Sea_Stars_Count ~ Site_TA, data = transect_SPES)
              summary(SS_Sites_anova)
              # p-value = 0.0619
      
  # Presence/absence of sea star species per TA from summer 2019-2023
    plot_ochre_SPES <- presence_absence_SPES(transect_SPES, "Ochre")
      plot_ochre_SPES
    plot_leather_SPES <- presence_absence_SPES(transect_SPES, "Leather")
      plot_leather_SPES
    plot_mottled_SPES <- presence_absence_SPES(transect_SPES, "Mottled")
      plot_mottled_SPES 
  #=================================================================================================================================
      
# Limpet size distribution visualizations and statistics
  #=================================================================================================================================
  # Limpet length per-TA from summer 2019-2023
    limpet_length_SPES <- plot_SPES(limpet_SPES, "Mean_Length_mm",
                                            "Mean Length of Limpets in the Spring/Summer from 2019-2023",
                                            "Mean Length (mm)", c(0,25))
      limpet_length_SPES
      
      # Statistical analysis - ANOVA test
        # Differences across years
          Length_Year_anova <- aov(Mean_Length_mm ~ Year, data = limpet_SPES)
            summary(Length_Year_anova)
            # p-value = 3.83e-05
        # Differences across sites
          Length_Site_anova <- aov(Mean_Length_mm ~ Site_TA, data = limpet_SPES)
            summary(Length_Site_anova)
            # p-value = 0.00267
      
  # Limpet width per-TA from summer 2019-2023
      limpet_width_SPES <- plot_SPES(limpet_SPES, "Mean_Width_mm",
                                              "Mean Width of Limpets in the Spring/Summer from 2019-2023",
                                              "Mean Width (mm)", c(0,25))
      limpet_width_SPES
      
      # Statistical analysis - ANOVA test
        # Differences across years
          Width_Year_anova <- aov(Mean_Width_mm ~ Year, data = limpet_SPES)
            summary(Width_Year_anova)
            # p-value = 8.86e-05
        # Differences across sites
          Width_Site_anova <- aov(Mean_Width_mm ~ Site_TA, data = limpet_SPES)
            summary(Width_Site_anova)
            # p-value = 0.0017
  #=================================================================================================================================
      
# 1m quadrat data visualization and analysis
  #=================================================================================================================================
  # Density of littorine snails per TA from summer 2019-2023
    Lit_density_TA <- plot_SPES(quad1m_SPES, "Littorine_snails", 
                                              "Mean Count of Littorine Snails in the Spring/Summer from 2019-2023", 
                                              "Mean Count of Littorine Snails", c(0, 550))
    Lit_density_TA
    
    # Statistical analysis - ANOVA test
      # Differences across years
        Lit_Year_anova <- aov(Littorine_snails ~ Year, data = quad1m_SPES)
          summary(Lit_Year_anova)
          # p-value = 0.0059
      # Differences across sites
        Lit_Site_anova <- aov(Littorine_snails ~ Site_TA, data = quad1m_SPES)
          summary(Lit_Site_anova)
          # p-value = 0.0015

  # Density of limpets per TA from summer 2019-2023
    Limpet_density_TA <- plot_SPES(quad1m_SPES, "Limpets", 
                                             "Mean Count of Limpets in the Spring/Summer from 2019-2023", 
                                             "Mean Count of Limpets", c(0,220))
    Limpet_density_TA
    
    # Statistical analysis - ANOVA test
      # Differences across years
        Limp_Year_anova <- aov(Limpets ~ Year, data = quad1m_SPES)
          summary(Limp_Year_anova)
            # p-value = 6.7e-05
      # Differences across sites
        Limp_Site_anova <- aov(Limpets ~ Site_TA, data = quad1m_SPES)
          summary(Limp_Site_anova)
          # p-value = 0.00178

  # Density of littorine snails across the height of the intertidal zone
      lit_snail_height <- plot_count_intertidal_height(quad1m_SPES, 'Littorine_snails', scale_fill = c("lightgreen", "darkgreen"), 
                                                       "Intertidal Height and Count of Littorine Snails in the Spring/Summer")
        lit_snail_height
        
        # Statistical analysis - ANOVA test
          # Differences across the intertidal zone height
            Lit_Height_anova <- aov(Littorine_snails ~ intertidal_height, data = quad1m_SPES)
              summary(Lit_Height_anova)
              # p-value = 0.0146
  
  # Density of limpets across the height of the intertidal zone 
      limpet_height <- plot_count_intertidal_height(quad1m_SPES, 'Limpets', scale_fill = c("purple", "purple4"),
                                                    "Intertidal Height and Count of Organisms in the Spring/Summer")
        limpet_height  
        
        # Statistical analysis - ANOVA test
          # Differences across the intertidal zone height
            Limp_Height_anova <- aov(Limpets ~ intertidal_height, data = quad1m_SPES)
              summary(Limp_Height_anova)
              # p-value = 0.874
  #=================================================================================================================================
    
# 0.25m quadrat data visualization and analysis
  #=================================================================================================================================
  # Total and proportional percent cover of algae and invertebrates per TA from summer 2019-2023
   # Select data
      percent_cover_SPES <- select_total_cover(quad0.25m_SPES, include_year = TRUE, include_season = FALSE)
              
   # Aggregate and merge data 
      all_cover_SPES <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ year + site_TA, data= percent_cover_SPES, FUN=mean, na.action = na.omit)
      cover_sd_SPES <- aggregate(invert_cover ~ year + site_TA, data = percent_cover_SPES, FUN = function(x) sd(x))
        names(cover_sd_SPES)[3] <- "sd_cover"
      percent_cover_SPES <- merge(all_cover_SPES, cover_sd_ENVR, by = c("year", "site_TA"))
      
    # Plot the data
      total_cover_SPES <- ggplot(percent_cover_SPES, aes(x = as.factor(year), y = total_cover)) +
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
      total_cover_SPES
      
    # Statistical analysis - ANOVA test
      # Differences across years
        TCover_Year_anova <- aov(Total_Cover ~ Year, data = quad0.25m_SPES)
          summary(TCover_Year_anova)
          # p-value = 0.000577
      # Differences across sites
        TCover_Site_anova <- aov(Total_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(TCover_Site_anova)
          # p-value = 0.395
          
  # Percent cover of algae and count of species per TA from summer 2019-2023
    # Select data
      algae_SPES <- select_algae(quad0.25m_SPES, include_year = TRUE, include_season = FALSE)
    
    # Aggregate and merge data
      algae_SPES_agg <- aggregate_count_data(algae_SPES, "algae_percent_cover", "algae_count")  
      algae_SPES_total <- merge(algae_SPES_agg$total, algae_SPES_agg$percent_sd, by = c("site_TA", "modified_TA", "year"))
      
    # Plot the data
      algae_cover_count_SPES <- ggplot(data = algae_SPES_total, aes(x = year, y = percent_cover)) +
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
      algae_cover_count_SPES
    
    # Statistical analysis - ANOVA test
      # Percent cover differences across years
        AlgaeCover_Year_anova <- aov(Algae_Cover ~ Year, data = quad0.25m_SPES)
          summary(AlgaeCover_Year_anova)
          # p-value = 0.525
      # Percent differences across sites
        AlgaeCover_Site_anova <- aov(Algae_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(AlgaeCover_Site_anova)
          # p-value = 5.24e-06
    
  # Percent cover of invertebrates and count of sessile and mobile species per TA from summer 2019-2023
    # Select data
      invert_SPES <- select_invertebrate(quad0.25m_SPES, include_year = TRUE, include_season = FALSE)
    
    # Aggregate and merge data  
      invert_SPES <- aggregate_count_data(invert_SPES, "invert_percent_cover", "sessile_count", "mobile_count")  
      invert_SPES_total <- merge(invert_SPES$total, invert_SPES$percent_sd, by = c("site_TA", "modified_TA", "year"))
    
    # Plot data  
      invert_cover_count_SPES <- ggplot(data = invert_SPES_total, aes(x = year, y = percent_cover)) +
        geom_bar(stat = "identity", aes(fill = as.factor(year)), position = "stack", alpha = 0.8) +  
        geom_line(aes(y = sessile_count * adj, color = "Sessile"), group = 1) +
        geom_line(aes(y = mobile_count * adj, color = "Mobile"), group = 1) +
        geom_errorbar(aes(ymin = pmax(0, percent_cover - invert_percent_cover_sd), 
                          ymax = pmin(100, percent_cover + invert_percent_cover_sd)), 
                      width = 0.25, position = position_dodge(width = 0.9),
                      color = "black", linewidth = 0.5) +  # Change 'size' to 'linewidth'
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
    invert_cover_count_SPES
    
    # Statistical analysis - ANOVA test
      # Percent cover differences across years
        InvertCover_Year_anova <- aov(Invertebrates_Cover ~ Year, data = quad0.25m_SPES)
          summary(InvertCover_Year_anova)
          # p-value = 0.371
      # Percent cover differences across sites
        InvertCover_Site_anova <- aov(Invertebrates_Cover ~ Site_TA, data = quad0.25m_SPES)
          summary(InvertCover_Site_anova)
          # p-value = 4.93e-05

  # Total, algae and invertebrate percent cover across the height of the intertidal zone
    cover_intertidal_height(quad0.25m_SPES, color_scale = c("low" = "yellow", "high" = "red"), 
                            plot_title = "Intertidal Height and Mean Percent Cover in the Spring/Summer from 2019-2023")
    
    # Statistical analysis - ANOVA test
      # Differences in total percent cover across the height of the intertidal zone
        TCover_Height_anova <- aov(Total_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(TCover_Height_anova)
          # p-value = 0.00348
      # Differences in algae percent cover over across the height of the intertidal zone
        AlgaeCover_Height_anova <- aov(Algae_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(AlgaeCover_Height_anova)
          # p-value = 0.966
      # Differences in invertebrate percent cover across the height of the intertidal zone
        InvertCover_Height_anova <- aov(Invertebrates_Cover ~ intertidal_height, data = quad0.25m_SPES)
          summary(InvertCover_Height_anova)
          # p-value = 0.738
    
  #=================================================================================================================================

# ENVR 400 2024 Data Analysis
#=================================================================================================================================
# ENVR 400 2024 visualization functions
  #=================================================================================================================================
  # Colours of the three sampled sites (TA-1, TA-4, and TA-6)
    # skyblue2: TA-1
    # orchard2: TA-4
    # coral: TA-6
    site_colors_ENVR <- c("skyblue2", "orchid2", "coral")
    
  # Generate a plot of the mean count/size measurements of an organism at the three sampled sites
    # df: The dataframe that contains the variable being plotted
    # varname: The variable to be plotted
    # plot_title: The title of the plot
    # plot_yaxis: The title of the y-axis
    # y_axis_limits: Specify the limits of the y axis in the format c(lower limit, upper limit)
    plot_ENVR <- function(df, varname, plot_yaxis, plot_title, y_axis_limits){
            # create data frame with relevant columns
            df_var <- data.frame(site_TA = df$modified_TA,
                                 var = df[,varname])
            
            # aggregate to the mean per Site_TA
            df_var_mean <- aggregate(var ~ site_TA, data=df_var, FUN = function(x) round(mean(x)))
            names(df_var_mean)[2] <- "mean"
            df_var_sd <- aggregate(var ~ site_TA, data = df_var, FUN = function(x) sd(x))
            names(df_var_sd)[2] <- "sd"
            
            df_var_all <- merge(df_var_mean, df_var_sd, by = "site_TA")
            
            ggplot(data = df_var_all, aes(x = site_TA, y = mean, fill = site_TA)) +
              geom_bar(stat = "identity", position = "dodge") +
              geom_errorbar(aes(ymin = pmax(mean - sd, 0), ymax = mean + sd),
                            position = position_dodge(width = 0.9), width = 0.25) +
              ylab(plot_yaxis) + xlab("Site") + labs(title = plot_title, fill = "Site") +
              scale_fill_manual(values = site_colors_ENVR) +
              coord_cartesian(ylim = y_axis_limits) +  # specify y-axis limits
              theme_minimal() +
              theme(plot.title=element_text(size=15), #change font size of plot title
                    axis.text=element_text(size=11), #change font size of axis text
                    axis.title=element_text(size=14), #change font size of axis titles
                    legend.text=element_text(size=11), #change font size of legend text
                    legend.title=element_text(size=14)) #change font size of legend title  
    }
    
  # Generate a presence/absence plot of sea star species across the three sampled sites
    # df: The dataframe which contains the variable being plotted
    # species_column: The column which contains the species presence/absence data
    # plot_yaxis: The y axis title
    presence_absence_ENVR <- function(df, species_column, plot_yaxis) {
      df <- data.frame(Site_TA = df$Site_TA,
                       Species = df[[species_column]])
      
      df_1 <- subset(df, Site_TA == 1)
      
      df <- aggregate(Species ~ Site_TA, data=df, FUN=sum)
      df_1 <- aggregate(Species ~ Site_TA, data=df_1, FUN=sum, na.action = na.pass)
      
      df_merge <- rbind(df, df_1)
      
      df_merge$Species <- as.logical(df_merge$Species)
      
      ggplot(df_merge, aes(x = as.character(Site_TA), y = "", fill = Species)) +
        geom_tile(colour ='black') +
        scale_fill_manual(values = c("green", "red", "gray"),
                          labels = c("Present", "Absent", "Not Counted"),
                          na.value = "gray",
                          drop = FALSE) +
        labs(x = "Sampling Site", y = plot_yaxis,
             title = "Sea Star Presence Absence in Winter 2023/2024",
             fill = paste0(species_column, " Presence/Absence")) +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
              panel.grid.minor = element_blank(),  # Remove minor gridlines
              axis.text.y = element_text(angle = 0, hjust = 0.5)) +
        scale_y_discrete(breaks = unique(df_merge$Site_TA))  # Set breaks for y-axis
      
    }
  
  #=================================================================================================================================

# Transect
  #=================================================================================================================================
  # Density of sea stars per TA 
    SS_density_ENVR <- plot_ENVR(transect_ENVR, "Density_of_Sea_Stars_Count", "Mean Count of Sea Stars",
                                                 "Mean Count of Sea Stars during Winter 2023/2024", c(0,35))
      SS_density_ENVR
      
    # Statistical analysis - ANOVA test
      # Differences across sites
      SS_ENVR_anova <- aov(Density_of_Sea_Stars_count ~ Site_TA, data = transect_ENVR)
        summary(SS_ENVR_anova)
        # p-value = 0.000106
  
  # Density of Oysters per TA
    oyster_density_ENVR <- plot_ENVR(transect_ENVR, "Density_of_Oysters_count", "Mean Count of Oysters",
                                                     "Mean Count of Oysters during Winter 2023/2024", c(0,225))
      oyster_density_ENVR
      
    # Statistical analysis - ANOVA test
      # Differences across sites
      Oyster_ENVR_anova <- aov(Density_of_Oysters_count ~ Site_TA, data = transect_ENVR)
        summary(Oyster_ENVR_anova)
        # p-value = 0.000307
  
  # Presence/absence of sea star species per sampled TA
    plot_ochre_ENVR <- presence_absence_ENVR(transect_ENVR, "Ochre_EO", "Ochre")
      plot_ochre_ENVR
    plot_leather_ENVR <- presence_absence_ENVR(transect_ENVR, "Leather_EL", "Leather")
      plot_leather_ENVR
    plot_mottled_ENVR <- presence_absence_ENVR(transect_ENVR, "Mottled_EM", "Mottled")
      plot_mottled_ENVR
  #=================================================================================================================================

# Limpet data
  #=================================================================================================================================
  # Limpet length per sampled TA
    limpet_length_ENVR <- plot_ENVR(limpet_ENVR, "Mean_Length_mm", "Mean Length (mm)", 
                                            "Mean Length of Limpets in Winter 2023/2024", c(0,35))
      limpet_length_ENVR
    
    # Statistical analysis - ANOVA test
      # Differences across sites
      Length_ENVR_anova <- aov(Mean_Length_mm ~ Site_TA, data = limpet_ENVR)
        summary(Length_ENVR_anova)
        # p-value = 0.000404
        
  # Limpet width per sampled TA
    limpet_width_ENVR <- plot_ENVR(limpet_ENVR, "Mean_Width_mm", "Mean Width (mm)", 
                                           "Mean Width of Limpets in Winter 2023/2024", c(0,35))
      limpet_width_ENVR
      
    # Statistical analysis - ANOVA test
      # Differences across sites
      Width_ENVR_anova <- aov(Mean_Width_mm ~ Site_TA, data = limpet_ENVR)
        summary(Width_ENVR_anova)
        # p-value = 0.000953
  #=================================================================================================================================

# 0.25m Quadrat
  #=================================================================================================================================
  # Total and proportional percent cover of algae and invertebrates per sampled TA
    # Select data
      percent_cover_ENVR <- select_total_cover(quad0.25m_ENVR, include_year = FALSE, include_season = FALSE)

    # Aggregate and merge data
      all_cover_ENVR <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ modified_TA, data= percent_cover_ENVR, FUN=mean)
      cover_sd_ENVR <- aggregate(invert_cover ~ modified_TA, data = percent_cover_ENVR, FUN = function(x) sd(x))
        names(cover_sd_ENVR)[names(cover_sd_ENVR) == "invert_cover"] <- "sd"

      percent_cover_ENVR <- merge(all_cover_ENVR, cover_sd_ENVR, by = "modified_TA")
  
    # Plot data
      total_cover_ENVR <- ggplot(percent_cover_ENVR, aes(x = modified_TA, y = total_cover)) +
        geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - sd/2, ymax = invert_cover + sd/2), 
                  position = position_dodge(width = 0.9), width = 0.5) +
        labs(x = "Site", y = "Percent Cover", title = "Mean Percent Cover of Algae and Invertebrates in Winter 2023/2024", fill = "Organismal Class") +
        scale_fill_viridis(discrete = TRUE, option = "D", alpha = 0.8) +
        theme_minimal() +
        scale_y_continuous(limits = c(0, 100)) +
        theme(plot.title=element_text(size=15), #change font size of plot title
            axis.text=element_text(size=11), #change font size of axis text
            axis.title=element_text(size=14), #change font size of axis titles
            legend.text=element_text(size=11), #change font size of legend text
            legend.title=element_text(size=14)) #change font size of legend title 
      total_cover_ENVR
    
    # Statistical analysis - ANOVA test
      # Differences in total cover across sampled sites
      TCover_ENVR_anova <- aov(Total_Cover ~ Site_TA, data = quad0.25m_ENVR)
        summary(TCover_ENVR_anova)
        # p-value = 0.00474
      
  # Percent cover of algae and count of species per sampled TA in the winter
    # Select data
      algae_ENVR <- select_algae(quad0.25m_ENVR, include_year = FALSE, include_season = FALSE)
    
    # Aggregate and merge data
      algae_ENVR_agg <- aggregate_count_data(algae_ENVR, "algae_percent_cover", "algae_count")  
      algae_ENVR_total <- merge(algae_ENVR_agg$total, algae_ENVR_agg$percent_sd, by = c("site_TA", "modified_TA"))

    # Plot data  
    algae_cover_count_ENVR <- ggplot(algae_ENVR_total, aes(x = modified_TA)) +
      geom_bar(aes(y = percent_cover, fill = modified_TA), stat = "identity", width = 0.5) +
      geom_line(aes(y = algae_count*adj, color = "Algae"), group = 1) +
      geom_errorbar(aes(ymin = pmax(percent_cover - algae_percent_cover_sd, 0), 
                      ymax = pmin(percent_cover + algae_percent_cover_sd, 100)), 
                  width = 0.2, position = position_dodge(width = 0.5), color = "black") +
      labs(x = "Site", y = "Percent Cover", 
         title = "Mean Percent Cover and Count of Algae in Winter 2023/2024", 
         fill = "Site", color = "Species Count") +
      scale_y_continuous(limits = c(0, 100), name = "Percent Cover",
                       sec.axis = sec_axis(~.x/adj, name = "Species Count", breaks = seq(0, 4, 1))) + 
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
    algae_cover_count_ENVR
  
  # Statistical analysis - ANOVA test
    # Differences in algae percent cover across sampled sites
    AlgaeCover_ENVR_anova <- aov(Algae_Cover ~ Site_TA, data = quad0.25m_ENVR)
      summary(AlgaeCover_ENVR_anova)
      # p-value = 3.81e-05
  
  # Percent cover of invertebrates and count of sessile and mobile species per TA from summer 2019-2023
    # Select data
      invert_ENVR <- select_invertebrate(quad0.25m_ENVR, include_year = FALSE, include_season = FALSE)
      
    # Aggregate and merge data  
      invert_ENVR <- aggregate_count_data(invert_quad0.25m_ENVR, "invert_percent_cover", "sessile_count", "mobile_count")  
      invert_ENVR_total <- merge(invert_ENVR$total, invert_ENVR$percent_sd, by = c("site_TA", "modified_TA"))
      
    # Plot data
      invert_cover_count_ENVR <- ggplot(data = invert_ENVR_total, aes(x = modified_TA)) +
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
      invert_cover_count_ENVR
  
  # Statistical analysis - ANOVA test
    # Differences in invert percent cover across sampled sites
    InvertCover_ENVR_anova <- aov(Invertebrates_Cover ~ Site_TA, data = quad0.25m_ENVR)
      summary(InvertCover_ENVR_anova)
      # p-value = 1.95e-08
  
  # Total, algae and invertebrate percent cover across the height of the intertidal zone
    cover_intertidal_height(quad0.25m_ENVR, color_scale = c("low" = "lightblue", "high" = "darkblue"), 
                          plot_title = "Intertidal Height and Mean Percent Cover in Winter 2023/2024")
    
    # Statistical analysis - ANOVA test
      # Differences in total percent cover across the height of the intertidal zone
        TCover_Height_ENVR_anova <- aov(Total_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(TCover_Height_ENVR_anova)
          # p-value = 0.000146
      # Differences in algae percent cover across the height of the intertidal zone
        AlgaeCover_Height_ENVR_anova <- aov(Algae_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(AlgaeCover_Height_ENVR_anova)
          # p-value = 0.00353
      # Differences in invertebrate percent cover across the height of the intertidal zone
        InvertCover_Height_ENVR_anova <- aov(Invertebrates_Cover ~ intertidal_height, data = quad0.25m_ENVR)
          summary(InvertCover_Height_ENVR_anova)
          # p-value = 0.718
  #=================================================================================================================================

# Combined SPES and ENVR 400 Seasonal Analysis  
#=================================================================================================================================
# Combined analysis functions
  #=================================================================================================================================
  # Select TA-1, 4 and 6 from SPES data - the TAs which were also sampled in ENVR data
    # df: dataframe which contains the SPES data to be filtered by sampled TAs
    subset_TAs <- function(df) {
    subset(df, Site_TA %in% c(1, 4, 6) & Year == 2023)
    }
      transect_SPES_TA <- subset_TAs(transect_SPES)
      quad1m_SPES_TA <- subset_TAs(quad1m_SPES)
      quad0.25m_SPES_TA <- subset_TAs(quad0.25m_SPES)
      limpet_SPES_TA <- subset_TAs(limpet_SPES)
  
  # Aggregate seasonal organismal count data
    # df: The dataframe which contains the variable which is being aggregated
    # varname: The name of the variable to aggregate by
    aggregate_seasonal <- function(df, varname) {
      aggregate_mean <- aggregate(df[[varname]], by = df[c("season", "Site_TA", "modified_TA")], 
                                    FUN = mean, na.rm = TRUE)
        colnames(aggregate_mean) <- c("season", "Site_TA", "modified_TA", "mean")
        
      aggregate_sd <- aggregate(df[[varname]], by = df[c("season", "Site_TA", "modified_TA")], 
                                  FUN = function(x) sd(x, na.rm = TRUE))
        colnames(aggregate_sd) <- c("season", "Site_TA", "modified_TA", "sd")
        
        return(list(mean = aggregate_mean, sd = aggregate_sd))
    }
    
  # Plot seasonal data
    # df: The dataframe which contains the data to be plotted
    # y_var: the variable to be plotted
    # ylab_text: The y axis title
    # title_text: The plot title
    plot_seasonal <- function(df, y_var, ylab_text, title_text) {
      ggplot(df, aes(x = factor(modified_TA), y = {{y_var}}, fill = season)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax({{y_var}} - sd, 0), 
                          ymax = {{y_var}} + sd),
                      position = position_dodge(width = 0.9), 
                      width = 0.25) +
        labs(x = "Site", y = ylab_text, title = title_text) +
        scale_fill_discrete(name = "Season") +
        theme_minimal() + 
        ylim(0, 30) +
        theme(plot.title=element_text(size=15), # change font size of plot title
              axis.text=element_text(size=11), # change font size of axis text
              axis.title=element_text(size=14), # change font size of axis titles
              legend.text=element_text(size=11), # change font size of legend text
              legend.title=element_text(size=14)) # change font size of legend title
    }

  # Aggregate seasonal algae and invertebrate percent cover and count data for their respective plots
    # df: The dataframe which contains the data to be aggregated
    # percent_cover_col: The column which contains the algae or invertebrates percent cover data
    # count_col: The column which contains the algae species count data or sessile species count data
    # additional_count_col: Only used if aggregating invertebrate data - lets you aggreate the additional mobile species count data
    aggregate_cover_count_seasonal <- function(df, percent_cover_col, count_col, additional_count_col = NULL) {
      # Calculate mean percent cover and rounded mean count
      total_cover_mean <- aggregate(df[[percent_cover_col]] ~ season + site_TA + modified_TA, data = df, FUN = function(x) mean(x))
      names(total_cover_mean) <- c("season", "site_TA", "modified_TA", "percent_cover") # Assign column names
      
      count_rounded_mean <- aggregate(df[[count_col]] ~ season + site_TA + modified_TA, data = df, FUN = function(x) round(mean(x)))
      names(count_rounded_mean)[length(names(count_rounded_mean))] <- count_col # Assign column name as the original data column
      
      if (!is.null(additional_count_col)) {
        additional_count_rounded_mean <- aggregate(df[[additional_count_col]] ~ season + site_TA + modified_TA, data = data, FUN = function(x) round(mean(x)))
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
  
  # Create a linear model for seasonal statistical analysis
    # df1: The first dataframe used in the linear model - generally SPES data
    # df2: The second dataframe used in the linear model - generally ENVR data
    # var_name: The predictor variable - day of year in this case
    lm_seasonality <- function(df1, df2, var_name) {
      # Create data frames for each dataset
        Stats1 <- data.frame(DOY = df1$DOY, 
                           df1[[var_name]])
          names(Stats1)[2] <- var_name
      
        Stats2 <- data.frame(DOY = df2$DOY, 
                           df2[[var_name]])
          names(Stats2)[2] <- var_name
      
      # Combine data frames from both datasets
        Stats_all <- rbind(Stats1, Stats2)
      
      # Fit linear model
        Seasonality <- lm(as.formula(paste(var_name, "~ DOY")), data = Stats_all)
      
      # Return summary of the linear model
        return(summary(Seasonality))
    }
  #=================================================================================================================================
    
# Transect data
  #=================================================================================================================================
   # Density of sea stars at each measured site (TA-1,4, and 6) from spring/summer 2023-winter 2024
    # Determine mean count and standard deviation of SPES and ENVR data
      ss_agg_SPES <- aggregate_seasonal(transect_SPES_TA, "Density_of_Sea_Stars_Count")
      ss_agg_ENVR <- aggregate_seasonal(transect_ENVR, "Density_of_Sea_Stars_Count")
    
    # Combine the count and standard deviation into a single dataframe
      ss_agg_combined <- rbind(ss_agg_SPES[["mean"]], ss_agg_ENVR[["mean"]])
      ss_agg_sd_combined <- rbind(ss_agg_SPES[["sd"]], ss_agg_ENVR[["sd"]])
    
        ss_merge <- merge(ss_agg_combined, ss_agg_sd_combined, by = c("season", "Site_TA", "modified_TA"))
    
    # Plot data
      ss_count_seasonal <- plot_seasonal(ss_merge, mean, "Density of Sea Stars (Count)", "Density of Sea Stars from May 2023 - February 2024")
        ss_count_seasonal
    
    # Statistical analysis - linear regression model
      lm_seasonality(transect_SPES_TA, transect_ENVR, "Density_of_Sea_Stars_Count")
        # Adjusted R-squared = 0.04474
        # p-value = 0.1135
      
  #=================================================================================================================================
    
# Limpet data
  #=================================================================================================================================
    # Seasonal limpet length from spring/summer 2023 - winter 2024
      # Aggregate length data by season
      length_agg_SPES <- aggregate_seasonal(limpet_SPES_TA, "Mean_Length_mm")
      length_agg_ENVR <- aggregate_seasonal(limpet_ENVR, "Mean_Length_mm")
      
      # Combine the mean length and standard deviation of the length - merge the dataset
      length_agg_combined <- rbind(length_agg_SPES[["mean"]], length_agg_ENVR[["mean"]])
      length_agg_sd_combined <- rbind(length_agg_SPES[["sd"]], length_agg_ENVR[["sd"]]) 
      
        length_merge <- merge(length_agg_combined, length_agg_sd_combined, by = c("season", "Site_TA", "modified_TA"))
  
      # Plot data
        limpet_length_plot_seasonal <- plot_seasonal(length_merge, mean, "Mean Length (mm)", "Mean Length of Limpets from May 2023 - February 2024")
          limpet_length_plot_seasonal
    
    # Seasonal limpet width from spring/summer 2023 - winter 2024
      # Aggregate length data by season
      width_agg_SPES <- aggregate_seasonal(limpet_SPES_TA, "Mean_Width_mm")
      width_agg_ENVR <- aggregate_seasonal(limpet_ENVR, "Mean_Width_mm")
      
      # Combine the mean length and standard deviation of the length - merge the dataset
      width_agg_combined <- rbind(width_agg_SPES[["mean"]], width_agg_ENVR[["mean"]])
      width_agg_sd_combined <- rbind(width_agg_SPES[["sd"]], width_agg_ENVR[["sd"]]) 
      
        width_merge <- merge(width_agg_combined, width_agg_sd_combined, by = c("season", "Site_TA", "modified_TA"))
  
      # Plot data
        limpet_width_plot_seasonal <- plot_seasonal(width_merge, mean, "Mean Width (mm)", "Mean Width of Limpets from May 2023 - February 2024")
          limpet_width_plot_seasonal
    
    # Statistical analysis - linear regression model
      # Limpet length
        lm_seasonality(limpet_SPES_TA, limpet_ENVR, "Mean_Length_mm")
          # Adjusted R-squared = 0.7297
          # p-value = 1.963e-13
        
      # Limpet width
        lm_seasonality(limpet_SPES_TA, limpet_ENVR, "Mean_Width_mm")
            # Adjusted R-squared = 0.7352
            # p-value = 1.287e-13
  #=================================================================================================================================

# 0.25m data
  #=================================================================================================================================
  # Total and proportional percent cover of algae and invertebrates per sampled TA from spring/summer 2023-winter 2024
    # Select Data
      cover_SPES_seasonal <- select_total_cover(quad0.25m_SPES_TA, include_year = FALSE, include_season = TRUE)
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

      # Statistical analysis - linear regression model
        # Total percent cover
          lm_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, "Total_Cover")
              # Adjusted R-squared = -0.004734
              # p-value = 0.6075
              
  # Percent cover of algae and count of species per sampled TA from spring/summer 2023 - winter 2024
    # Select data
      algae_SPES_seasonal <- select_algae(quad0.25m_SPES_TA, include_year = FALSE, include_season = TRUE)
      algae_ENVR_seasonal <- select_algae(quad0.25m_ENVR, include_year = FALSE, include_season = TRUE)
    
    # Aggregate and combine data  
      algae_SPES_seasonal_total <- aggregate_cover_count_seasonal(algae_SPES_seasonal, "algae_percent_cover", "algae_count")  
      algae_ENVR_seasonal_total <- aggregate_cover_count_seasonal(algae_ENVR_seasonal, "algae_percent_cover", "algae_count")  
        
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
        
      # Statistical analysis - linear regression model
        # Algae percent cover
        lm_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, "Algae_Cover")
          # Adjusted R-squared = -0.002941
          # p-value = 0.4625
          
  # Seasonal Invertebrate Cover and Count
    # Select data
      invert_SPES_seasonal <- select_invertebrate(quad0.25m_SPES_TA, include_year = FALSE, include_season = TRUE)
      invert_ENVR_seasonal <- select_invertebrate(quad0.25m_ENVR, include_year = FALSE, include_season = TRUE)
      
      # Aggregate the data according to site and season   
          invert_SPES_seasonal_total <- aggregate_cover_count_seasonal(invert_SPES_seasonal, "invert_percent_cover", "sessile_count", "mobile_count")  
          invert_ENVR_seasonal_total <- aggregate_cover_count_seasonal(invert_ENVR_seasonal, "invert_percent_cover", "sessile_count", "mobile_count")  
      
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
          
      # Statistical analysis - linear regression model
        # Invertebrate percent cover
          lm_seasonality(quad0.25m_SPES_TA, quad0.25m_ENVR, "Invertebrates_Cover")
          # Adjusted R-squared = -0.003061
          # p-value = 0.4702
  #=================================================================================================================================
  
# Abiotic Statistical Analysis
#=================================================================================================================================
# Monthly average time of lowest low tide and maximum and minimum temperatures from January 2019-2024
  # Find the average time of lowest low tide
    monthly_low_tide_time <- tide %>%
      group_by(month, Year) %>%
      summarise(hour_of_min_tide = hour[which.min(SLEV_metres)])
    average_low_tide_time <- monthly_low_tide_time %>%
      group_by(month) %>%
      summarise(low_tide_time = mean(hour_of_min_tide))
  
  # Find monthly maximum and minimum temperatures
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
  
  # Merge the tide data with the temperature data
    monthly_temperature_data <- merge(monthly_max_temperature, monthly_min_temperature, by = "month")
    monthly_abiotic_data <- merge(monthly_temperature_data, average_low_tide_time, by = "month")

  # Generate the plot
    # Generate monthly labels
      monthly_abiotic_data$month <- factor(monthly_abiotic_data$month, levels = 1:12,
                                         labels = c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))
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
