# March 20, 2024
require(dplyr)
require(stringr)
require(ggplot2)
require(lubridate)
require(reshape2)

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
    convert_to_logical <- function(df, start_col_index, end_col_index) {
      df <- df %>%
        mutate(across(start_col_index:end_col_index, as.logical)) # change the columns inbetween the start and end to logical or true/false
      return(df)
    }
    
  # change columns to character
    convert_to_character <- function(data, column_name) {
      data[[column_name]] <- as.character(data[[column_name]])
      return(data)
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
  
  # divide the transect into low, medium and high
    intertidal_height <- function(data_frame, column_name) {
      # Define breaks and labels
      breaks <- c(0, 10, 20, 30)
      labels <- c("low", "medium", "high")
      data_frame$intertidal_height <- cut(data_frame[[column_name]], breaks = breaks, labels = labels, include.lowest = TRUE)
      return(data_frame)
    }
    
  # proportional percent cover
    adjusted_percent_cover <- function(dataframe, algae_cover_col, invertebrates_cover_col, total_cover_col) {
      dataframe$Adjusted_Algae_Cover <- dataframe[[algae_cover_col]] / (dataframe[[algae_cover_col]] + dataframe[[invertebrates_cover_col]]) * dataframe[[total_cover_col]]
      dataframe$Adjusted_Invert_Cover <- dataframe[[invertebrates_cover_col]] / (dataframe[[algae_cover_col]] + dataframe[[invertebrates_cover_col]]) * dataframe[[total_cover_col]]
      return(dataframe)
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
    quad0.25m_SPES <- convert_to_logical(quad0.25m_SPES, 14, 44)
    transect_SPES <- convert_to_logical(transect_SPES, 12, 14)

  # change site_TA to character
    transect_SPES <- convert_to_character(transect_SPES, "Site_TA")
    quad1m_SPES <- convert_to_character(quad1m_SPES, "Site_TA")
    quad0.25m_SPES <- convert_to_character(quad0.25m_SPES, "Site_TA")
    limpet_SPES <- convert_to_character(limpet_SPES, "Site_TA")
    
  # get the season and month
    transect_SPES$season <- get_season(transect_SPES$Date)
    transect_SPES$month <- month(as.Date(transect_SPES$Date))

    quad1m_SPES$season <- get_season(quad1m_SPES$Date)
    quad1m_SPES$month <- month(as.Date(quad1m_SPES$Date))

    quad0.25m_SPES$season <- get_season(quad0.25m_SPES$Date)
    quad0.25m_SPES$month <- month(as.Date(quad0.25m_SPES$Date))

    limpet_SPES$season <- get_season(limpet_SPES$Date)
    limpet_SPES$month <- month(as.Date(limpet_SPES$Date))
  
  # get intertidal height
    quad0.25m_SPES <- intertidal_height(quad0.25m_SPES, "Transect_Point_m")
    quad1m_SPES <- intertidal_height(quad1m_SPES, "Transect_Point_m")
    
  # proportional percent cover
    quad0.25m_SPES <- adjusted_percent_cover(quad0.25m_SPES, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
    
  #=================================================================================================================================

# ENVR 400 2024 Data
  #=================================================================================================================================
  # add year column - figure out how to change the column name to remove ymd now - aesthetic worry later
    transect_ENVR_2024 <- add_year_column(transect_ENVR_2024, "Date_ymd")
    quad0.25m_ENVR_2024 <- add_year_column(quad0.25m_ENVR_2024, "Date")
    limpet_ENVR_2024 <- add_year_column(limpet_ENVR_2024, "Date_ymd")
  
  # change presence/absence columns to logical - note if any other columns are added have to change range of columns
    transect_ENVR_2024 <- convert_to_logical(transect_ENVR_2024, 14, 16)
    
    quad0.25m_ENVR_2024 <- convert_to_logical(quad0.25m_ENVR_2024, 15, 26)
    quad0.25m_ENVR_2024 <- convert_to_logical(quad0.25m_ENVR_2024, 29, 36)
    quad0.25m_ENVR_2024 <- convert_to_logical(quad0.25m_ENVR_2024, 39, 44)
    quad0.25m_ENVR_2024 <- convert_to_logical(quad0.25m_ENVR_2024, 47, 54)
    quad0.25m_ENVR_2024 <- convert_to_logical(quad0.25m_ENVR_2024, 57, 62)
    
  # change site_TA to character
    transect_ENVR_2024 <- convert_to_character(transect_ENVR_2024, "Site_TA")
    quad0.25m_ENVR_2024 <- convert_to_character(quad0.25m_ENVR_2024, "Site_TA")
    limpet_ENVR_2024 <- convert_to_character(limpet_ENVR_2024, "Site_TA")
    
  # get season and month
    transect_ENVR_2024$season <- get_season(transect_ENVR_2024$Date_ymd)
    transect_ENVR_2024$month <- month(as.Date(transect_ENVR_2024$Date_ymd))
  
    quad0.25m_ENVR_2024$season <- get_season(quad0.25m_ENVR_2024$Date)
    quad0.25m_ENVR_2024$month <- month(as.Date(quad0.25m_ENVR_2024$Date))
  
    limpet_ENVR_2024$season <- get_season(limpet_ENVR_2024$Date)
    limpet_ENVR_2024$month <- month(as.Date(limpet_ENVR_2024$Date))
    
  # get intertidal height
    quad0.25m_ENVR_2024 <- intertidal_height(quad0.25m_ENVR_2024, "Transect_Point_M")
    
  # proportional percent cover
    quad0.25m_ENVR_2024 <- adjusted_percent_cover(quad0.25m_ENVR_2024, "Algae_Cover", "Invertebrates_Cover", "Total_Cover")
    
  #=================================================================================================================================

# Algae Data - need to think about algae and how to deal with
  #=================================================================================================================================
    #algae_data$season <- get_season(algae_data$Date)
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

# Overall functions
    # plotting count of species - can do for both SPES data (aggregated by year and TA) and ENVR data (aggregated by TA)
    plot_count_per_TA <- function(varname, plot_varname, data, aggregate_by_year_site = TRUE){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = data$Site_TA,
                           year = data$Year,
                           var = data[,varname])
      df_var$site_TA <- as.character(df_var$site_TA) 
      
      # Aggregate by year and site if requested
      if (aggregate_by_year_site) {
        df_var_mean <- aggregate(var ~ year + site_TA, data=df_var, FUN=mean)
        names(df_var_mean)[3] <- "mean_count"
        df_var_sd <- aggregate(var ~ year + site_TA, data = df_var, FUN = function(x) sd(x) / sqrt(length(x)))
        names(df_var_sd)[3] <- "sd_count"
        
        df_var_all <- merge(df_var_mean, df_var_sd, by = c("year", "site_TA"))
      } else {  # Otherwise, aggregate only by site
        df_var_mean <- aggregate(var ~ site_TA, data = df_var, FUN = mean)
        names(df_var_mean)[2] <- "mean_count"
        df_var_sd <- aggregate(var ~ site_TA, data = df_var, FUN = function(x) sd(x) / sqrt(length(x)))
        names(df_var_sd)[2] <- "sd_count"
        
        df_var_all <- merge(df_var_mean, df_var_sd, by = "site_TA")
      }
      
      # Plot
      ggplot(data = df_var_all, aes(x = ifelse(aggregate_by_year_site, year, site_TA), y = mean_count, group = site_TA, fill = site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean_count - sd_count, ymax = mean_count + sd_count), 
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(plot_varname) + 
        xlab(ifelse(aggregate_by_year_site, "Time (years)", "Site TA")) + 
        labs(fill = "Site TA")
    }
    
# Historical SPES Data plotting
#=================================================================================================================================
# Functions 
  #=================================================================================================================================
  # identity of Sea Star - excludes all NA values, need to figure out how to have different ones depending on what is avaliable for each site
    presence_absence_SPES <- function(data, species_column) {
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
      scale_y_discrete(breaks = unique(data$Site_TA))  # Set breaks for y-axis
  }
    
  # plot percent cover bar graphs
    percent_cover_SPES <- function(quad0.25m_SPES) {
      # Calculate percentage of total cover comprised of algae and invertebrates - need to clean but its at 100
      quad0.25m_SPES$Algae_percent <- quad0.25m_SPES$'Algae_%_cover' * 1
      quad0.25m_SPES$Invertebrates_percent <- quad0.25m_SPES$'Invertebrates_%_Cover' * 1
      
      # Aggregate data by year and site_TA
      data_agg <- aggregate(cbind(Algae_percent, Invertebrates_percent ) ~ Year + Site_TA, data = quad0.25m_SPES, FUN = mean, na.action = na.omit)
      
      # Reshape data for plotting
      data_plot <- reshape2::melt(data_agg, id.vars = c("Year", "Site_TA"))
      
      # Create the multi-colored bar graph
      ggplot(data = data_plot, aes(x = Year, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "Year", y = "Percentage of Total Cover", fill = "Cover Component") +
        facet_wrap(~ Site_TA) +
        theme_minimal()
    }
  
  #=================================================================================================================================
  
# Sea Star (transect) data
  #=================================================================================================================================
  # Density of sea stars per TA (2019-2023)
    SS_density_TA <- plot_count_per_TA("Density_of_Sea_Stars_Count", "Mean count of sea stars", transect_SPES, aggregate_by_year_site = TRUE)
    SS_density_TA
    
  # Presence absence of sea stars 
    plot_ochre <- presence_absence_SPES(transect_SPES, "Ochre")
      plot_ochre
    plot_leather <- presence_absence_SPES(transect_SPES, "Leather")
      plot_leather
    plot_mottled <- presence_absence_SPES(transect_SPES, "Molted")
      plot_mottled 
  #=================================================================================================================================
      
# Limpet Data
  #=================================================================================================================================
    # select specific limpet data
    select_limpet_SPES <- data.frame(site_TA = limpet_SPES$Site_TA,
                                      year = limpet_SPES$Year,
                                      length = limpet_SPES[,"Mean_Length_mm"],
                                      width = limpet_SPES[,"Mean_Width_mm"],
                                      month = limpet_SPES$month)
    select_limpet_SPES$site_TA <- as.character(select_limpet_SPES$site_TA)
    
  # aggregate by year and site (mean per visit)
    # Length
      limp_agg_length_SPES <- aggregate(length ~ year + site_TA, data=select_limpet_SPES, FUN=mean)
      names(limp_agg_length_SPES)[3] <- "mean_length"
      
      limp_agg_length_SPES_sd <- aggregate(length ~ year + site_TA, data = select_limpet_SPES, FUN = function(x) sd(x) / sqrt(length(x)))
      names(limp_agg_length_SPES_sd)[3] <- "sd_length"
      
      limp_agg_legth_SPES_all <- left_join(limp_agg_length_SPES, limp_agg_length_SPES_sd, by = c("year", "site_TA"))
      
      ggplot(data = limp_agg_legth_SPES_all, aes(x = year, y = mean_length, group = site_TA, fill = site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean_length - sd_length, ymax = mean_length + sd_length),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab("Mean limpet length (mm)") +
        xlab("Site TA") +
        labs(fill = "Site TA") +
        theme_minimal()
      
    # Width
      limp_agg_width_SPES <- aggregate(width ~ year + site_TA, data=select_limpet_SPES, FUN=mean)
      names(limp_agg_width_SPES)[3] <- "mean_width"
      
      limp_agg_width_SPES_sd <- aggregate(width ~ year + site_TA, data = select_limpet_SPES, FUN = function(x) sd(x) / sqrt(length(x)))
      names(limp_agg_width_SPES_sd)[3] <- "sd_width"
      
      limp_agg_width_SPES_all <- left_join(limp_agg_width_SPES, limp_agg_width_SPES_sd, by = c("year", "site_TA"))
      
      ggplot(data = limp_agg_width_SPES_all, aes(x = year, y = mean_width, group = site_TA, fill = site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean_width - sd_width, ymax = mean_width + sd_width),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab("Mean limpet width (mm)") +
        xlab("Site TA") +
        labs(fill = "Site TA") +
        theme_minimal()
  
  #=================================================================================================================================
      
# 1m quadrat data
  #=================================================================================================================================
  # Littorine Snails
    plot_count_per_TA("Littorine_snails", "Mean count of littorine snails per field visit", quad1m_SPES, aggregate_by_year_site = TRUE)

  # Limpet Count
    plot_count_per_TA("Limpets", "Mean count of limpets per field visit", quad1m_SPES, aggregate_by_year_site = TRUE)
    
  # Changes in intertidal height composition - ask Christina about making seperate scales or if its just worth making multiple of the same plot and combining together in powerpoint
    # selecting and aggregating the data
      select_quad1m_SPES <- data.frame(site_TA = quad1m_SPES$Site_TA,
                                     year = quad1m_SPES$Year,
                                     season = quad1m_SPES$season,
                                     month = quad1m_SPES$month,
                                     intertidal_height = quad1m_SPES$intertidal_height,
                                     littorine_snails = quad1m_SPES$Littorine_snails,
                                     limpets = quad1m_SPES$Limpets)

      quad1m_SPES_agg_snail <- aggregate(littorine_snails ~ intertidal_height, data=select_quad1m_SPES, FUN = function(x) round(mean(x)))
      quad1m_SPES_agg_limpet <- aggregate(limpets ~ intertidal_height, data=select_quad1m_SPES, FUN = function(x) round(mean(x)))
 
      quad1m_SPES_agg <- left_join(quad1m_SPES_agg_snail, quad1m_SPES_agg_limpet, by = "intertidal_height")
      
      tidy_quad1m_data <- tidyr::pivot_longer(quad1m_SPES_agg, cols = c(littorine_snails, limpets), names_to = "Count_Type", values_to = "Count")
      
    ggplot(tidy_quad1m_data, aes(x = Count_Type, y = intertidal_height, fill = Count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Count), vjust = 1) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Count", y = "Tide Height", title = "Intertidal Height vs Count of Snails and limpets") +
      theme_minimal() +
      scale_y_discrete(limits = c("low", "medium", "high"))
  
  #=================================================================================================================================
    
# 0.25m quadrat data
  #=================================================================================================================================
  # total and relative percent cover
    percent_cover_SPES(quad0.25m_SPES)   
  
  # percent cover and count of algae
    algae_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                       year = quad0.25m_SPES$Year,
                                       algae_percent_cover = quad0.25m_SPES$'Algae_%_cover',
                                       algae_count = quad0.25m_SPES$Algae_Count_)
    quad_0.25m_SPES_agg_algae_percent <- aggregate(algae_percent_cover ~ site_TA + year, data=algae_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    quad_0.25m_SPES_agg_algae_count <- aggregate(algae_count ~ site_TA + year, data=algae_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    
    quad_0.25m_SPES_algae_merge <- left_join(quad_0.25m_SPES_agg_algae_percent, quad_0.25m_SPES_agg_algae_count, by = c("site_TA", "year"))
    
    # cannot get the line to show up
    quad_0.25_SPES_algae_plot <- ggplot(data = quad_0.25m_SPES_algae_merge, aes(x = year, y = algae_percent_cover)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_line(data = quad_0.25m_SPES_algae_merge, aes(x = year, y = algae_count), color = "blue") +  # Add line for algae count
      labs(x = "Year", y = "Percentage of Total Cover", fill = "Cover Component") +
      scale_y_continuous(sec.axis = sec_axis(~., name = "Algae Count", breaks = seq(0, 4, 1))) +  # Secondary y-axis for algae count
      facet_wrap(~ site_TA) +
      theme_minimal()
    print(quad_0.25_SPES_algae_plot)
    
    # kinda crazy but kinda like it?
    wild_quad_0.25_SPES_algae_plot <- ggplot(quad_0.25m_SPES_algae_merge, aes(x = year)) +
      geom_bar(aes(y = algae_percent_cover), stat = "identity", fill = "blue") +
      geom_line(aes(y = algae_count * 4), size = 1, color = "red") +  # Scale algae count to match y-axis range
      scale_y_continuous(name = "Algae Percent Cover", breaks = seq(0, 100, by = 20), expand = c(0, 0),
                         sec.axis = sec_axis(~./4, name = "Count of Algae", breaks = seq(0, 4, by = 1))) +
      facet_wrap(~ site_TA, ncol = 1) +  # Separate plots for each year
      labs(title = "Algae Cover and Count by Year", color = "Algae Count") +
      theme_minimal()
    print(wild_quad_0.25_SPES_algae_plot)
  
  # percent cover of invertebrates and count of sessile and mobile - scale for count still messed   
    invert_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                        year = quad0.25m_SPES$Year,
                                        invert_percent_cover = quad0.25m_SPES$`Invertebrates_%_Cover`,
                                        sessile_count = quad0.25m_SPES$Sessile_Invertebrates_Count,
                                        mobile_count = quad0.25m_SPES$Mobile_Invertebrates_Count)
    
    quad_0.25m_SPES_agg_invert_percent <- aggregate(invert_percent_cover ~ site_TA + year, data=invert_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    quad_0.25m_SPES_agg_sessile_count <- aggregate(sessile_count ~ site_TA + year, data=invert_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    quad_0.25m_SPES_agg_mobile_count <- aggregate(mobile_count ~ site_TA + year, data=invert_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    
    quad_0.25m_SPES_invert_merge <- left_join(quad_0.25m_SPES_agg_invert_percent, quad_0.25m_SPES_agg_sessile_count, by = c("site_TA", "year"))
    quad_0.25m_SPES_invert_merge <- left_join(quad_0.25m_SPES_invert_merge, quad_0.25m_SPES_agg_mobile_count, by = c("site_TA", "year"))
    
    quad_0.25_SPES_invert_plot <- ggplot(data = quad_0.25m_SPES_invert_merge, aes(x = year, y = invert_percent_cover)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_line(aes(y = sessile_count), color = "red", linetype = "solid", group = 1) +
      geom_line(aes(y = mobile_count), color = "green", linetype = "dashed", group = 1) +
      labs(x = "Year", y = "Percentage of Total Cover", fill = "Cover Component") +
      scale_y_continuous(sec.axis = sec_axis(~., name = "Count", breaks = seq(0, 4, 1))) +  # Secondary y-axis for algae count
      facet_wrap(~ site_TA) +
      theme_minimal()
    print(quad_0.25_SPES_invert_plot)
    
  # cover of algae vs invertebrates across the transect - summer
    select_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                    year = quad0.25m_SPES$Year,
                                    season = quad0.25m_SPES$season,
                                    month = quad0.25m_SPES$month,
                                    intertidal_height = quad0.25m_SPES$intertidal_height,
                                    total_percent_cover = quad0.25m_SPES$'Total_Cover_%',
                                    algae_percent_cover = quad0.25m_SPES$`Algae_%_cover`,
                                    invertebrates_percent_cover = quad0.25m_SPES$`Invertebrates_%_Cover`)

    quad_0.25m_SPES_agg_total <- aggregate(total_percent_cover ~ intertidal_height, data=select_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    quad_0.25m_SPES_agg_algae_percent <- aggregate(algae_percent_cover ~ intertidal_height, data=select_quad0.25m_SPES, FUN = function(x) round(mean(x)))
    quad_0.25m_SPES_agg_intertebrates <- aggregate(invertebrates_percent_cover ~ intertidal_height, data=select_quad0.25m_SPES, FUN = function(x) round(mean(x)))

    quad_0.25m_SPES_agg <- quad_0.25m_SPES_agg_total %>%
      left_join(quad_0.25m_SPES_agg_algae_percent, by = "intertidal_height") %>%
      left_join(quad_0.25m_SPES_agg_intertebrates, by = "intertidal_height")

    tidy_quad0.25_SPES_data <- tidyr::pivot_longer(quad_0.25m_SPES_agg, cols = c(total_percent_cover, algae_percent_cover, invertebrates_percent_cover), names_to = "Percent_Cover_Type", values_to = "Percent_Cover")

    ggplot(tidy_quad0.25_SPES_data, aes(x = Percent_Cover_Type, y = intertidal_height, fill = Percent_Cover)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Percent_Cover), vjust = 1) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Percent Cover", y = "Intertidal Height", title = "Intertidal Height vs Total and Relative Percent Covers of Alage and Invertebrates") +
      theme_minimal() +
      scale_y_discrete(limits = c("low", "medium", "high"))
  #=================================================================================================================================

# ENVR 400 2024 Data Analysis
#=================================================================================================================================
# Functions
  #=================================================================================================================================
  # identity of sea stars
  presence_absence_400 <- function(data, species_column) {
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
      ) 
      #scale_y_continuous(breaks = unique(data$Site_TA)) # Wrap x-axis labels for better readability
  }
  
  # percent cover function
  percent_cover_400 <- function(quad0.25m_ENVR_2024) {
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
  #=================================================================================================================================

# Transect
  #=================================================================================================================================
  # Density of sea stars per TA 
  SS_density_TA_400 <- plot_count_per_TA("Density_of_Sea_Stars_count", "Count of sea stars", transect_ENVR_2024, aggregate_by_year_site = FALSE)
  SS_density_TA_400
  
  # Density of Oysters per TA
  oyster_density_TA_400 <- plot_count_per_TA("Density_of_Oysters_count", "Count of Oysters", transect_ENVR_2024, aggregate_by_year_site = FALSE)
  oyster_density_TA_400
  
  # Presence absence of sea stars 
  plot_ochre_400 <- presence_absence_400(transect_ENVR_2024, "Ochre_EO")
  plot_ochre_400
  
  plot_leather_400 <- presence_absence_400(transect_ENVR_2024, "Leather_EL")
  plot_leather_400
  
  plot_molted_400 <- presence_absence_400(transect_ENVR_2024, "Mottled_EM")
  plot_molted_400
  #=================================================================================================================================

# Limpet data
  #=================================================================================================================================
  select_limpet_ENVR <- data.frame(site_TA = limpet_ENVR_2024$Site_TA,
                                   year = limpet_ENVR_2024$Year,
                                   length = limpet_ENVR_2024[,"Mean_Length_mm"],
                                   width = limpet_ENVR_2024[,"Mean_Width_mm"],
                                   month = limpet_ENVR_2024$month)
  select_limpet_ENVR$site_TA <- as.character(select_limpet_ENVR$site_TA)
  
  # Mean Length - how to add site TA to x axis 
  limp_agg_length_ENVR <- aggregate(length ~ site_TA, data=select_limpet_ENVR, FUN=mean)
  names(limp_agg_length_ENVR)[2] <- "mean_length"
  
  limp_agg_length_ENVR_sd <- aggregate(length ~ site_TA, data = select_limpet_ENVR, FUN = function(x) sd(x) / sqrt(length(x)))
  names(limp_agg_length_ENVR_sd)[2] <- "sd_length"
  
  # not done, not working either
  limp_agg_legth_ENVR_all <- left_join(limp_agg_length_ENVR, limp_agg_length_ENVR_sd, by=site_TA)
  
  ggplot(data = limp_agg_legth_SPES_all, aes(x = year, y = mean_length, group = site_TA, fill = site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_length - sd_length, ymax = mean_length + sd_length),
                  position = position_dodge(width = 0.9), width = 0.25) +
    ylab("Mean limpet length (mm)") +
    xlab("Site TA") +
    labs(fill = "Site TA") +
    theme_minimal()
  
  limp_agg_length_400 <- aggregate(Mean_Length_mm ~ Site_TA, data = limpet_ENVR_2024, FUN = mean)
  ggplot(data = limp_agg_length_400, aes(x = Site_TA, y = Mean_Length_mm, fill = Site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Mean limpet length (mm)") + xlab("Sampling Site") + labs(fill = "Site TA") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust x-axis text alignment
    ) 
  
  # Mean Width
  limp_agg_width_400 <- aggregate(Mean_Width_mm ~ Site_TA, data = limpet_ENVR_2024, FUN = mean)
  ggplot(data = limp_agg_width_400, aes(x = Site_TA, y = Mean_Width_mm, fill = Site_TA)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Mean limpet width (mm)") + xlab("Sampling Site") + labs(fill = "Site TA") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust x-axis text alignment
    ) 

  #=================================================================================================================================

# 0.25m Quadrat
  #=================================================================================================================================
  # Bar graph of total and relative percent cover
  percent_cover_400(quad0.25m_ENVR_2024)
  
  # bar graph of relative percent cover of algae and count of algae species - dont love it tbh LMAO
  algae_quad0.25m_ENVR <- data.frame(site_TA = quad0.25m_ENVR_2024$Site_TA,
                                     algae_percent_cover = quad0.25m_ENVR_2024$Percent_Cover_Algae,
                                     algae_count = quad0.25m_ENVR_2024$Algae_Count_Above)
  quad_0.25m_ENVR_agg_algae_percent <- aggregate(algae_percent_cover ~ site_TA, data=algae_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  quad_0.25m_ENVR_agg_algae_count <- aggregate(algae_count ~ site_TA, data=algae_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  
  quad_0.25m_ENVR_algae_merge <- merge(quad_0.25m_ENVR_agg_algae_percent, quad_0.25m_ENVR_agg_algae_count, by = "site_TA")
  
  quad_0.25m_ENVR_algae_plot <- ggplot(quad_0.25m_ENVR_algae_merge, aes(x = site_TA)) +
    geom_bar(aes(y = algae_percent_cover), stat = "identity", fill = "blue", width = 0.5) +
    geom_line(aes(y = algae_count*max(quad_0.25m_ENVR_algae_merge$algae_percent_cover)/max(quad_0.25m_ENVR_algae_merge$algae_count)), color = "red", group = 1) +
    scale_y_continuous(name = "Percent Cover",
                       sec.axis = sec_axis(~./max(quad_0.25m_ENVR_algae_merge$algae_percent_cover)*max(quad_0.25m_ENVR_algae_merge$algae_count), name = "Count of Algae", labels = scales::comma)) +
    labs(x = "Sites") +
    theme_minimal()
  print(quad_0.25m_ENVR_algae_plot)
  
  # bar graph of relative percent cover of invertebrates and count of mobile and sessile
  # *** NEED TO FIGURE OUT SCALE*****
  invert_quad0.25m_ENVR <- data.frame(site_TA = quad0.25m_ENVR_2024$Site_TA,
                                     invert_percent_cover = quad0.25m_ENVR_2024$Percent_Cover_Invertebrates,
                                     sessile_count = quad0.25m_ENVR_2024$Sessile_Invertebrates_Count_Above + quad0.25m_ENVR_2024$Sessile_Invertebrates_Count_Below,
                                     mobile_count = quad0.25m_ENVR_2024$Mobile_Invertebrates_Count_Above + quad0.25m_ENVR_2024$Mobile_Invertebrates_Count_Below)
  
  quad_0.25m_ENVR_agg_invert_percent <- aggregate(invert_percent_cover ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  quad_0.25m_ENVR_agg_sessile_count <- aggregate(sessile_count ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  quad_0.25m_ENVR_agg_mobile_count <- aggregate(mobile_count ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
 
  quad_0.25m_ENVR_invert <- merge(quad_0.25m_ENVR_agg_invert_percent, quad_0.25m_ENVR_agg_sessile_count, by = "site_TA")
  quad_0.25m_ENVR_invert <- merge(quad_0.25m_ENVR_invert, quad_0.25m_ENVR_agg_mobile_count, by = "site_TA")
  
  invert_quad0.25m_ENVR_plot <- ggplot(quad_0.25m_ENVR_invert, aes(x = site_TA)) +
    geom_bar(aes(y = invert_percent_cover), stat = "identity", fill = "blue", width = 0.5) +
    geom_line(aes(y = sessile_count), color = "red", linetype = "solid", group = 1) +
    geom_line(aes(y = mobile_count), color = "green", linetype = "dashed", group = 1) +
    scale_y_continuous(name = "Invert Percent Cover", limits = c(0, 60), sec.axis = sec_axis(~ . / 4, name = "Count", breaks = seq(0, 4, by = 1))) +
    labs(x = "Sites") +
    theme_minimal()
  
  print(invert_quad0.25m_ENVR_plot)
  
  # cover of algae vs invertebrates across the transect - winter
  select_quad0.25m_ENVR <- data.frame(site_TA = quad0.25m_ENVR_2024$Site_TA,
                                 year = quad0.25m_ENVR_2024$Year,
                                 season = quad0.25m_ENVR_2024$season,
                                 month = quad0.25m_ENVR_2024$month,
                                 intertidal_height = quad0.25m_ENVR_2024$intertidal_height,
                                 total_percent_cover = quad0.25m_ENVR_2024$'Total_Cover_%',
                                 algae_percent_cover = quad0.25m_ENVR_2024$Percent_Cover_Algae,
                                 invertebrates_percent_cover = quad0.25m_ENVR_2024$Percent_Cover_Invertebrates)

  quad_0.25m_ENVR_agg_total <- aggregate(total_percent_cover ~ intertidal_height, data=select_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  quad_0.25m_ENVR_agg_algae_percent <- aggregate(algae_percent_cover ~ intertidal_height, data=select_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  quad_0.25m_ENVR_agg_intertebrates <- aggregate(invertebrates_percent_cover ~ intertidal_height, data=select_quad0.25m_ENVR, FUN = function(x) round(mean(x)))

  quad_0.25m_ENVR_agg <- quad_0.25m_ENVR_agg_total %>%
    left_join(quad_0.25m_ENVR_agg_algae_percent, by = "intertidal_height") %>%
    left_join(quad_0.25m_ENVR_agg_intertebrates, by = "intertidal_height")

  tidy_quad0.25_ENVR_data <- tidyr::pivot_longer(quad_0.25m_ENVR_agg, cols = c(total_percent_cover, algae_percent_cover, invertebrates_percent_cover), names_to = "Percent_Cover_Type", values_to = "Percent_Cover")

  ggplot(tidy_quad0.25_ENVR_data, aes(x = Percent_Cover_Type, y = intertidal_height, fill = Percent_Cover)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Percent_Cover), vjust = 1) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(x = "Percent Cover", y = "Intertidal Height", title = "Intertidal Height vs Total and Relative Percent Covers of Alage and Invertebrates") +
    theme_minimal() +
    scale_y_discrete(limits = c("low", "medium", "high"))
  

  #=================================================================================================================================

# Combined SPES and ENVR 400  
#=================================================================================================================================
  # select TA-1, 4 and 6 from SPES data
  subset_TAs <- function(dataframe, column_name, values) {
    subset(dataframe, Site_TA %in% c(1, 4, 6))
  }
    transect_SPES_TA <- subset_TAs(transect_SPES)
    quad1m_SPES_TA <- subset_TAs(quad1m_SPES)
    quad0.25m_SPES_TA <- subset_TAs(quad0.25m_SPES)
    limpet_SPES_TA <- subset_TAs(limpet_SPES)
  
    
    # Call the function to create the multi-colored bar graph
    plot_cover_components(quad0.25m_ENVR_2024)
    
    #Analysis of 400 & SPES
    
    filtered_data <- subset(limpet_SPES, Site_TA %in% c(1, 4, 6))
    
    # Aggregate data by Site_TA
    aggregated_data_SPES <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ Year + season + Site_TA, data = filtered_data, FUN = mean, na.action = na.omit)
    
    aggregated_data_400 <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ Year + season + Site_TA, data = limpet_ENVR_2024, FUN = mean, na.action = na.omit)
    
    limpet_merge <- rbind(aggregated_data_SPES, aggregated_data_400)
    
    
    limp_agg_length_combined <- aggregate(Mean_Length_mm ~ Year + Site_TA, data=limpet_merge, FUN=mean)
    ggplot(data = limp_agg_length_combined, aes(x = Year, y = Mean_Length_mm, group = Site_TA, fill = Site_TA)) +
      geom_bar(stat = "identity", position = "dodge") +
      ylab("Mean limpet length (mm)") + xlab("Time (years)") + labs(fill = "Site TA")
    
    # Width
    limp_agg_width_combined <- aggregate(Mean_Width_mm ~ Year + Site_TA, data=limpet_merge, FUN=mean)
    ggplot(data = limp_agg_width_combined, aes(x = Year, y = Mean_Width_mm, group = Site_TA, fill = Site_TA)) +
      geom_bar(stat = "identity", position = "dodge") +
      ylab("Mean limpet Width (mm)") + xlab("Time (years)") + labs(fill = "Site TA")
    
    df_limp_agg_400 <- aggregate(Mean_Width_mm ~ season + Site_TA, data= limpet_merge, FUN=mean)
    ggplot(data = df_limp_agg_400, aes(x = season, y = Mean_Width_mm, group = Site_TA, fill = Site_TA)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      ylab("Mean limpet width (mm)") + xlab("season") + labs(fill = "Site TA")
    
    df_limp_agg_400 <- aggregate(Mean_Length_mm ~ season + Site_TA, data= limpet_merge, FUN=mean)
    ggplot(data = df_limp_agg_400, aes(x = season, y = Mean_Length_mm, group = Site_TA, fill = Site_TA)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      ylab("Mean limpet Length (mm)") + xlab("season") + labs(fill = "Site TA")
  
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
#=================================================================================================================================

