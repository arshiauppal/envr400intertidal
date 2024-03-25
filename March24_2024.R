# March 24, 2024 - jess
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
  # add year column
    transect_ENVR <- add_year_column(transect_ENVR, "Date") 
    quad0.25m_ENVR <- add_year_column(quad0.25m_ENVR, "Date")
    limpet_ENVR <- add_year_column(limpet_ENVR, "Date") 
  
  # change presence/absence columns to logical - note if any other columns are added have to change range of columns
    transect_ENVR <- convert_to_logical(transect_ENVR, 14, 16)
    
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 15, 26)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 29, 36)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 39, 44)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 47, 54)
    quad0.25m_ENVR <- convert_to_logical(quad0.25m_ENVR, 57, 62)
    
  # change site_TA to character
    transect_ENVR <- convert_to_character(transect_ENVR, "Site_TA")
    quad0.25m_ENVR <- convert_to_character(quad0.25m_ENVR, "Site_TA")
    limpet_ENVR <- convert_to_character(limpet_ENVR, "Site_TA")
    
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
  #================================================================================================================================
  # plot percent cover along the transect line - need to fix aesthetics
      cover_intertidal_height <- function(data) {
        selected_data <- data.frame(
          intertidal_height = data$intertidal_height,
          total_percent_cover = data$Total_Cover,
          algae_percent_cover = data$`Algae_Cover`,
          invertebrates_percent_cover = data$`Invertebrates_Cover`
        )
        
        quad_agg_total <- aggregate(total_percent_cover ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
        quad_agg_algae <- aggregate(algae_percent_cover ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
        quad_agg_intertebrates <- aggregate(invertebrates_percent_cover ~ intertidal_height, data = selected_data, FUN = function(x) round(mean(x)))
        
        quad_agg <- quad_agg_total %>%
          left_join(quad_agg_algae, by = "intertidal_height") %>%
          left_join(quad_agg_intertebrates, by = "intertidal_height")
        
        tidy_quad_data <- pivot_longer(quad_agg, cols = c(total_percent_cover, algae_percent_cover, invertebrates_percent_cover), names_to = "Percent_Cover_Type", values_to = "Percent_Cover")
        
        ggplot(tidy_quad_data, aes(x = Percent_Cover_Type, y = intertidal_height, fill = Percent_Cover)) +
          geom_tile(color = "white") +
          geom_text(aes(label = Percent_Cover), vjust = 1) +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          labs(x = "Percent Cover", y = "Intertidal Height", title = "Intertidal Height vs Total and Relative Percent Covers of Algae and Invertebrates") +
          theme_minimal() +
          scale_y_discrete(limits = c("low", "medium", "high"))
      }
      
# SPES Data
#=================================================================================================================================
# Functions 
  #=================================================================================================================================
  # plotting a variable for every TA for every year.
    plot_count_per_TA_SPES <- function(varname, plot_varname, data){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = data$Site_TA,
                           year = data$Year,
                           var = data[,varname])
      df_var$site_TA <- as.character(df_var$site_TA)
      
      # aggregate to the mean per visit (or check what makes sense for you: e.g. average monthly count, etc.)
      df_var_mean <- aggregate(var ~ year + site_TA, data=df_var, FUN=mean)
      names(df_var_mean)[3] <- "mean_count"
      df_var_sd <- aggregate(var ~ year + site_TA, data = df_var, FUN = function(x) sd(x))
      names(df_var_sd)[3] <- "sd_count"
      
      df_var_all <- merge(df_var_mean, df_var_sd, by = c("year", "site_TA"))
      
      ggplot(data = df_var_all, aes(x = year, y = mean_count, group = site_TA, fill = site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax(mean_count - sd_count, 0), ymax = mean_count + sd_count),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(plot_varname) + xlab("Time (years)") + labs(fill = "Site TA")
      
    }
    
  # presence absence of sea star species
    presence_absence_SPES <- function(data, species_column) {
      df <- data.frame(Year = data$Year,
          Site_TA = data$Site_TA,
          Species = data[,species_column])
      df <- df[complete.cases(df),]

      df <- aggregate(Species ~ Year + Site_TA, data=df, FUN=sum)
      
      df$Species <- as.logical(df$Species)

      df_grid <- merge(expand.grid(Site_TA=1:6, Year = 2019:2023), df, all.x = TRUE)
      
      ggplot(df_grid, aes(x = as.character(Year), y = as.character(Site_TA), fill = Species)) +
        geom_tile(colour ='black') +
        scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), na.value = "gray",
                          drop=FALSE) +  # Adjust colors as needed
        labs(x = "Year", y = "Sampling Site", fill = paste0(species_column, " Presence")) +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
          ) +
        scale_y_discrete(breaks = unique(data$Site_TA))  # Set breaks for y-axis
    }
    
  # limpet plots
    limpet_plots_SPES <- function(data, agg_variable) {
      agg_mean <- aggregate(get(agg_variable) ~ Year + Site_TA, data = data, FUN = mean)
      names(agg_mean)[3] <- paste("mean_", agg_variable, sep = "")
      
      agg_sd <- aggregate(get(agg_variable) ~ Year + Site_TA, data = data, FUN = function(x) sd(x))
      names(agg_sd)[3] <- paste("sd_", agg_variable, sep = "")
      
      agg_all <- left_join(agg_mean, agg_sd, by = c("Year", "Site_TA"))
      
      ggplot(data = agg_all, aes(x = Year, y = get(paste("mean_", agg_variable, sep = "")), group = Site_TA, fill = Site_TA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = get(paste("mean_", agg_variable, sep = "")) - get(paste("sd_", agg_variable, sep = "")), ymax = get(paste("mean_", agg_variable, sep = "")) + get(paste("sd_", agg_variable, sep = ""))),
                      position = position_dodge(width = 0.9), width = 0.25) +
        ylab(paste("Mean limpet", agg_variable, " (mm)")) +
        xlab("Site TA") +
        labs(fill = "Site TA") +
        theme_minimal()
    }
  
  # plot count along intertidal height
    plot_count_intertidal_height <- function(data, count_variable, scale_fill = c("lightblue", "darkblue")) {
      # Aggregate the data for the specified count variable
      agg_data <- aggregate(data[[count_variable]], by = list(data$intertidal_height), FUN = function(x) round(mean(x, na.rm = TRUE)))
      # Rename columns
      names(agg_data) <- c("intertidal_height", "Count")
      # Reshape the data
      tidy_data <- tidyr::pivot_longer(agg_data, cols = c(Count), names_to = "Count_Type", values_to = "Count")
      # Generate the plot
      ggplot(tidy_data, aes(x = Count_Type, y = intertidal_height, fill = Count)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Count), vjust = 1) +
        scale_fill_gradient(low = scale_fill[1], high = scale_fill[2]) +
        labs(x = "Count", y = "Tide Height", title = paste("Intertidal Height vs Count of", count_variable)) +
        theme_minimal() +
        scale_y_discrete(limits = c("low", "medium", "high"))
    }
    
  #=================================================================================================================================
  
# Transect Data
  #=================================================================================================================================
  # Density of sea stars per TA (2019-2023)
    # error bars look insane
    SS_density_TA <- plot_count_per_TA_SPES("Density_of_Sea_Stars_Count", "Mean count of sea stars", transect_SPES)
      SS_density_TA
    
  # Presence absence of sea stars 
    plot_ochre <- presence_absence_SPES(transect_SPES, "Ochre")
      plot_ochre
    plot_leather <- presence_absence_SPES(transect_SPES, "Leather")
      plot_leather
    plot_mottled <- presence_absence_SPES(transect_SPES, "Molted")
      plot_mottled 
  #=================================================================================================================================
      
# Limpet Data - fix aesthetics
  #=================================================================================================================================
  # plot limpet length
    limpet_length_SPES <- limpet_plots_SPES(limpet_SPES, "Mean_Length_mm")
      limpet_length_SPES
      
  # plot limpet width
    limpet_width_SPES <- limpet_plots_SPES(limpet_SPES, "Mean_Width_mm")
      limpet_width_SPES
    
  #=================================================================================================================================
      
# 1m quadrat data - fix aesthetics
  #=================================================================================================================================
  # Littorine Snails
    plot_count_per_TA_SPES("Littorine_snails", "Mean count of littorine snails per field visit", quad1m_SPES)

  # Limpet Count
    plot_count_per_TA_SPES("Limpets", "Mean count of limpets per field visit", quad1m_SPES)
    
  # Changes in intertidal height composition
      lit_snail_height <- plot_count_intertidal_height(quad1m_SPES, 'Littorine_snails', scale_fill = c("lightblue", "darkblue"))
        lit_snail_height
        
      limpet_height <- plot_count_intertidal_height(quad1m_SPES, 'Limpets', scale_fill = c("yellow", "red"))
        limpet_height  
        
  #=================================================================================================================================
    
# 0.25m quadrat data - working on
  #=================================================================================================================================
  # Mean total and relative percent cover of algae and invertebrates for each site and year - need to fix aesthetics
    percent_cover_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                     year = quad0.25m_SPES$Year,
                                     total_cover = quad0.25m_SPES$Total_Cover,
                                     algae_cover = quad0.25m_SPES$Adjusted_Algae_Cover,
                                     invert_cover = quad0.25m_SPES$Adjusted_Invert_Cover)
    percent_cover_SPES <- percent_cover_SPES[complete.cases(percent_cover_SPES$total_cover) &
                                               complete.cases(percent_cover_SPES$algae_cover) &
                                               complete.cases(percent_cover_SPES$invert_cover), ]
   
    # aggregate data 
    all_cover <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ year + site_TA, data= percent_cover_SPES, FUN=mean, na.action = na.omit)
    cover_sd <- aggregate(invert_cover ~ year + site_TA, data = percent_cover_SPES, FUN = function(x) sd(x))
      names(cover_sd)[3] <- "sd_cover"
    
      percent_cover_all <- merge(all_cover, cover_sd, by = c("year", "site_TA"))
      percent_cover_all$year <- factor(percent_cover_all$year)
    
    ggplot(percent_cover_all, aes(x = as.factor(year), y = total_cover)) +
      geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
      geom_bar(aes(y = invert_relative, fill = "Invertebrates"), position = "stack", stat = "identity") +
      geom_errorbar(aes(ymin = invert_relative - sd_cover/2, ymax = invert_relative + sd_cover/2,
                        group = site_TA),  # Group by site_TA
                    position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
      facet_wrap(~site_TA) +
      labs(x = "Year", y = "Total Cover", title = "Total Cover segmented by Algae and Invertebrates") +
      scale_fill_manual(values = c("Algae" = "green", "Invertebrates" = "blue")) +
      theme_minimal() 
  
  # percent cover and count of algae - cant get second axis to look right
    algae_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                       year = quad0.25m_SPES$Year,
                                       algae_percent_cover = quad0.25m_SPES$Algae_Cover,
                                       algae_count = quad0.25m_SPES$Algae_Count)
    
    algae_cc <- aggregate(cbind(algae_percent_cover, algae_count) ~ year + site_TA, data= algae_quad0.25m_SPES, FUN=mean, na.action = na.omit)
     algae_percent_sd <- aggregate(algae_percent_cover ~ year + site_TA, data = algae_quad0.25m_SPES, FUN = function(x) sd(x))
      names(algae_percent_sd)[3] <- "sd_cover"
    
    algae_cover_count <- merge(algae_cc, algae_percent_sd, by = c("year", "site_TA"))

    p <- ggplot(algae_cover_count, aes(x = factor(year))) +
      geom_bar(aes(y = algae_percent, fill = factor(year)), stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = pmax(0, algae_percent - sd_cover), ymax = pmin(100, algae_percent + sd_cover)), width = 0.25, position = position_dodge(width = 0.9)) +
      facet_wrap(~ site_TA, scales = "free") +  # Facet by site_TA
      scale_fill_manual(values = c("red", "blue", "green", "orange", "purple")) +  # Providing enough colors for different years
      labs(x = "Year", y = "Algae Percent Cover", fill = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    
    # Adding secondary y-axis for algae count
    p <- p + geom_line(data = algae_cover_count, aes(y = algae_count), color = "black", size = 1, alpha = 0.5) +
      scale_y_continuous(name = "Algae Percent Cover", sec.axis = sec_axis(~./0.01, name = "Algae Count", breaks = seq(0, 4, by = 1)))
    
    # Display the plot
    print(p)
    
  
  # percent cover of invertebrates and count of sessile and mobile - scale works!
    invert_quad0.25m_SPES <- data.frame(site_TA = quad0.25m_SPES$Site_TA,
                                        year = quad0.25m_SPES$Year,
                                        invert_percent_cover = quad0.25m_SPES$Invertebrates_Cover,
                                        sessile_count = quad0.25m_SPES$Sessile_Invertebrates_Count,
                                        mobile_count = quad0.25m_SPES$Mobile_Invertebrates_Count)
    
    invert_cc <- aggregate(cbind(invert_percent_cover, sessile_count, mobile_count) ~ year + site_TA, data= invert_quad0.25m_SPES, FUN=mean, na.action = na.omit)
    invert_percent_sd <- aggregate(invert_percent_cover ~ year + site_TA, data = invert_quad0.25m_SPES, FUN = function(x) sd(x))
      names(invert_percent_sd)[3] <- "sd_cover"
    
    invert_cover_count <- merge(invert_cc, invert_percent_sd, by = c("year", "site_TA"))  
    
    adj <- 25
    
    quad_0.25_SPES_invert_plot <- ggplot(data = invert_cover_count, aes(x = year, y = invert_percent)) +
      geom_bar(stat = "identity", aes(fill = as.factor(year)), position = "stack") +
      geom_line(aes(y = sessile_count*adj), color = "red", linetype = "solid", group = 1) +
      geom_line(aes(y = mobile_count*adj), color = "green", linetype = "dashed", group = 1) +
      labs(x = "Year", y = "Percentage of Total Cover", fill = "Year", color = "Cover Component") +
      scale_y_continuous(sec.axis = sec_axis(~.x/adj, name = "Count", breaks = seq(0, 4, 1))) +  # Secondary y-axis for algae count
      facet_wrap(~ site_TA) +
      scale_fill_manual(values = c("blue", "green", "red", "orange", "purple", "brown")) +  # Manually setting colors for years
      theme_minimal()
    
    print(quad_0.25_SPES_invert_plot)
    
    
  # cover of algae vs invertebrates across the transect - summer
    cover_intertidal_height(quad0.25m_SPES)
    
  #=================================================================================================================================

# ENVR 400 2024 Data Analysis
#=================================================================================================================================
# Functions
  #=================================================================================================================================
  # count per TA
  plot_count_per_TA_400 <- function(data, varname, plot_varname){
      
      # create data frame with relevant columns
      df_var <- data.frame(site_TA = data$Site_TA,
                           var = data[,varname])
      df_var$site_TA <- as.character(df_var$site_TA) 
      
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
        ylab(plot_varname) + xlab("Sampling Site") + labs(fill = "Site TA")
      
  }
    
  # identity of sea stars - had to change christinas 
  presence_absence_ENVR <- function(data, species_column) {
      df <- data.frame(Site_TA = data$Site_TA,
                       Species = data[[species_column]])
      
      df_1 <- subset(df, Site_TA == 1)
      
      df <- aggregate(Species ~ Site_TA, data=df, FUN=sum)
      df_1 <- aggregate(Species ~ Site_TA, data=df_1, FUN=sum, na.action = na.pass)
      
      df_merge <- rbind(df, df_1)
      
      df_merge$Species <- as.logical(df_merge$Species)
      
      ggplot(df_merge, aes(x = as.character(Site_TA), y = "", fill = Species)) +
        geom_tile(colour ='black') +
        scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), na.value = "gray",
                          drop=FALSE) +  # Adjust colors as needed
        labs(x = "Sampling Site", y = paste0(species_column, " Presence")) +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "black", size = 0.5),  # Customize major gridlines
              panel.grid.minor = element_blank(),  # Remove minor gridlines
              axis.text.y = element_text(angle = 0, hjust = 0.5)  # Adjust y-axis text alignment
        ) +
        scale_y_discrete(breaks = unique(df_merge$Site_TA))  # Set breaks for y-axis
    }
  
  # limpet plots
  limpet_plots_ENVR <- function(data, agg_variable) {
    agg_mean <- aggregate(get(agg_variable) ~ Site_TA, data = data, FUN = mean)
    names(agg_mean)[2] <- paste("mean_", agg_variable, sep = "")
    
    agg_sd <- aggregate(get(agg_variable) ~ Site_TA, data = data, FUN = function(x) sd(x))
    names(agg_sd)[2] <- paste("sd_", agg_variable, sep = "")
    
    agg_all <- left_join(agg_mean, agg_sd, by = "Site_TA")
    
    ggplot(data = agg_all, aes(x = Site_TA, y = get(paste("mean_", agg_variable, sep = "")), fill = Site_TA)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = get(paste("mean_", agg_variable, sep = "")) - get(paste("sd_", agg_variable, sep = "")), ymax = get(paste("mean_", agg_variable, sep = "")) + get(paste("sd_", agg_variable, sep = ""))),
                    position = position_dodge(width = 0.9), width = 0.25) +
      ylab(paste("Mean limpet", agg_variable, " (mm)")) +
      xlab("Site TA") +
      labs(fill = "Site TA") +
      theme_minimal()
  }
  
  #=================================================================================================================================

# Transect - need to fix aesthetics
  #=================================================================================================================================
  # Density of sea stars per TA 
    SS_density_TA_400 <- plot_count_per_TA_400(transect_ENVR, "Density_of_Sea_Stars_count", "Count of sea stars")
      SS_density_TA_400
  
  # Density of Oysters per TA
    oyster_density_TA_400 <- plot_count_per_TA_400("Density_of_Oysters_count", "Count of Oysters", transect_ENVR)
      oyster_density_TA_400
  
  # Presence absence of sea stars 
    plot_ochre_400 <- presence_absence_ENVR(transect_ENVR, "Ochre_EO")
      plot_ochre_400
  
    plot_leather_400 <- presence_absence_ENVR(transect_ENVR, "Leather_EL")
      plot_leather_400
  
    plot_molted_400 <- presence_absence_ENVR(transect_ENVR, "Mottled_EM")
      plot_molted_400
  #=================================================================================================================================

# Limpet data - fix aesthetics
  #=================================================================================================================================
  # limpet length
    limpet_length_ENVR <- limpet_plots_ENVR(limpet_ENVR, "Mean_Length_mm")
      limpet_length_ENVR
      
  # limpet width
    limpet_width_ENVR <- limpet_plots_ENVR(limpet_ENVR, "Mean_Width_mm")
      limpet_width_ENVR
      
  #=================================================================================================================================

# 0.25m Quadrat - lots of working
  #=================================================================================================================================
  # Mean total and relative percent cover of algae and invertebrates for each site and year - need to fix aesthetics
    percent_cover_ENVR <- data.frame(site_TA = quad0.25m_ENVR$Site_TA,
                                     year = quad0.25m_ENVR$Year,
                                     season = quad0.25m_ENVR$season,
                                     total_cover = quad0.25m_ENVR$Total_Cover,
                                     algae_cover = quad0.25m_ENVR$Adjusted_Algae_Cover,
                                     invert_cover = quad0.25m_ENVR$Adjusted_Invert_Cover)
      percent_cover_ENVR <- percent_cover_ENVR[complete.cases(percent_cover_ENVR$total_cover) &
                                               complete.cases(percent_cover_ENVR$algae_cover) &
                                               complete.cases(percent_cover_ENVR$invert_cover), ]
    
  # Aggregate data by year and site_TA
  total_cover <- aggregate(total_cover ~ site_TA, data= percent_cover_ENVR, FUN=mean)
  algae_relative <- aggregate(algae_cover ~ site_TA, data= percent_cover_ENVR, FUN=mean)
  invert_relative <- aggregate(invert_cover ~ site_TA, data= percent_cover_ENVR, FUN=mean)
  cover_sd <- aggregate(invert_cover ~ site_TA, data = percent_cover_ENVR, FUN = function(x) sd(x))
    names(cover_sd)[names(cover_sd) == "invert_cover"] <- "sd"
  
  percent_cover_all <- merge(algae_relative, invert_relative, by = "site_TA")
  percent_cover_all <- merge(percent_cover_all, total_cover, by = "site_TA")
  percent_cover_all <- merge(percent_cover_all, cover_sd, by = "site_TA")
  
  ggplot(percent_cover_all, aes(x = site_TA, y = total_cover)) +
    geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
    geom_bar(aes(y = invert_relative, fill = "Invertebrates"), position = "stack", stat = "identity") +
    geom_errorbar(aes(ymin = invert_relative - sd_cover/2, ymax = invert_relative + sd_cover/2,
                      group = site_TA),  # Group by site_TA
                  position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
    labs(x = "Site_TA", y = "Total Cover", title = "Total Cover segmented by Algae and Invertebrates") +
    scale_fill_manual(values = c("Algae" = "green", "Invertebrates" = "blue")) +
    theme_minimal()
  
  # bar graph of relative percent cover of algae and count of algae species *ADJUST AESTHETICS
  algae_ENVR <- data.frame(site_TA = quad0.25m_ENVR$Site_TA,
                                     algae_percent_cover = quad0.25m_ENVR$Algae_Cover,
                                     algae_count = quad0.25m_ENVR$Algae_Count)
  algae_percent_ENVR <- aggregate(algae_percent_cover ~ site_TA, data=algae_ENVR, FUN = mean)
  algae_percent_sd_ENVR <- aggregate(algae_percent_cover ~ site_TA, data=algae_ENVR, FUN = function(x) sd(x))
    names(algae_percent_sd_ENVR)[2] <- "algae_percent_sd"
  algae_count_ENVR <- aggregate(algae_count ~ site_TA, data=algae_ENVR, FUN = function(x) round(mean(x)))
  
  algae_merge_ENVR <- merge(algae_percent_ENVR, algae_percent_sd_ENVR, by = "site_TA")
  algae_merge_ENVR <- merge(algae_merge_ENVR, algae_count_ENVR, by = "site_TA")
  
  quad_0.25m_ENVR_algae_plot <- ggplot(algae_merge_ENVR, aes(x = site_TA)) +
    geom_bar(aes(y = algae_percent_cover, fill = site_TA), stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = pmin(algae_percent_cover - algae_percent_sd, 100), 
                      ymax = pmin(algae_percent_cover + algae_percent_sd, 100)), 
                  width = 0.2, position = position_dodge(width = 0.5), color = "black") +
    geom_line(aes(y = algae_count*max(algae_merge_ENVR$algae_percent_cover)/max(algae_merge_ENVR$algae_count)), color = "red", group = 1) +
    scale_y_continuous(name = "Percent Cover",
                       sec.axis = sec_axis(~./max(algae_merge_ENVR$algae_percent_cover)*max(algae_merge_ENVR$algae_count), name = "Count of Algae", labels = scales::comma)) +
    labs(x = "Sites") +
    scale_fill_manual(values = rainbow(length(unique(algae_merge_ENVR$site_TA)))) +  # Change bar colors according to site
    theme_minimal()
  print(quad_0.25m_ENVR_algae_plot)
  
  # bar graph of relative percent cover of invertebrates and count of mobile and sessile - added error bars but the scale of the count is way off, need to fix aesthetics too
  invert_quad0.25m_ENVR <- data.frame(site_TA = quad0.25m_ENVR$Site_TA,
                                     invert_percent_cover = quad0.25m_ENVR$Invertebrates_Cover,
                                     sessile_count = quad0.25m_ENVR$Sessile_Invertebrates_Count_Above + quad0.25m_ENVR$Sessile_Invertebrates_Count_Below,
                                     mobile_count = quad0.25m_ENVR$Mobile_Invertebrates_Count_Above + quad0.25m_ENVR$Mobile_Invertebrates_Count_Below)
  
  invert_percent_ENVR <- aggregate(invert_percent_cover ~ site_TA, data=invert_quad0.25m_ENVR, FUN = mean)
  invert_percent_sd_ENVR <- aggregate(invert_percent_cover ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) sd(x))
    names(invert_percent_sd_ENVR)[2] <- "invert_percent_sd"
  sessile_count_ENVR <- aggregate(sessile_count ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
  mobile_count_ENVR <- aggregate(mobile_count ~ site_TA, data=invert_quad0.25m_ENVR, FUN = function(x) round(mean(x)))
 
  invert_merge_ENVR <- merge(invert_percent_ENVR, invert_percent_sd_ENVR, by = "site_TA")
  invert_merge_ENVR <- merge(invert_merge_ENVR, sessile_count_ENVR, by = "site_TA")
  invert_merge_ENVR <- merge(invert_merge_ENVR, mobile_count_ENVR, by = "site_TA")
  
  invert_quad0.25m_ENVR_plot <- ggplot(invert_merge_ENVR, aes(x = site_TA)) +
    geom_bar(aes(y = invert_percent_cover, fill = site_TA), stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = pmax(invert_percent_cover - invert_percent_sd, 0), 
                      ymax = pmin(invert_percent_cover + invert_percent_sd, 100)), 
                  width = 0.2, position = position_dodge(width = 0.5), color = "black") +
    geom_line(aes(y = sessile_count), color = "red", linetype = "solid", group = 1) +
    geom_line(aes(y = mobile_count), color = "green", linetype = "dashed", group = 1) +
    scale_y_continuous(name = "Percent Cover",
                       sec.axis = sec_axis(~./max(invert_merge_ENVR$invert_percent_cover)*max(invert_merge_ENVR$sessile_count), name = "Count of Sessile and Mobile", labels = scales::comma)) +
    labs(x = "Sites") +
    scale_fill_manual(values = rainbow(length(unique(invert_merge_ENVR$site_TA)))) +  # Change bar colors according to site
    theme_minimal()
  print(invert_quad0.25m_ENVR_plot)
  
  # cover of algae vs invertebrates across the transect - winter
  cover_intertidal_height(quad0.25m_ENVR)
  
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

# Transect data - NEED TO PLOT SEA STAR
  #=================================================================================================================================
  #=================================================================================================================================
    
# Limpet data
  #=================================================================================================================================
    # Aggregate data by Site_TA
    limp_agg_SPES <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA, data = limpet_SPES_TA, FUN = mean, na.action = na.omit)
    limp_agg_sd_SPES <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA, 
                             data = limpet_SPES_TA, 
                             FUN = function(x) sd(x, na.rm = TRUE),
                             na.action = na.omit)
      names(limp_agg_sd_SPES)[names(limp_agg_sd_SPES) == "Mean_Length_mm"] <- "sd_length"
      names(limp_agg_sd_SPES)[names(limp_agg_sd_SPES) == "Mean_Width_mm"] <- "sd_width"

    limp_agg_ENVR <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA, data = limpet_ENVR, FUN = mean, na.action = na.omit)
    limp_agg_sd_ENVR <- aggregate(cbind(Mean_Length_mm, Mean_Width_mm) ~ season + Site_TA, data = limpet_ENVR, FUN = function(x) sd(x, na.rm = TRUE), na.action = na.omit)
      names(limp_agg_sd_ENVR)[names(limp_agg_sd_ENVR) == "Mean_Length_mm"] <- "sd_length"
      names(limp_agg_sd_ENVR)[names(limp_agg_sd_ENVR) == "Mean_Width_mm"] <- "sd_width"
    
    limp_agg_combined <- rbind(limp_agg_SPES, limp_agg_ENVR)
    limp_agg_sd_combined <- rbind(limp_agg_sd_SPES, limp_agg_sd_ENVR)
      
    limpet_merge <- merge(limp_agg_combined, limp_agg_sd_combined, by = c("season", "Site_TA"))

    # plotting limpet function - need to fix aesthetics
    plot_limpet <- function(data, y_variable) {
      # Generate the plot
      ggplot(data, aes(x = factor(Site_TA), y = !!sym(y_variable), fill = season)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = pmax(!!sym(y_variable) - sd_length, 0), 
                          ymax = !!sym(y_variable) + sd_length),
                      position = position_dodge(width = 0.9), 
                      width = 0.25) +
        labs(x = "Site_TA", y = y_variable, title = paste("Plot of", y_variable, "by Site and Season")) +
        scale_fill_discrete(name = "Season") +
        theme_minimal()
    }
      # length
      plot_limpet(limpet_merge, "Mean_Length_mm")
      # width
      plot_limpet(limpet_merge, "Mean_Width_mm")
  #=================================================================================================================================

# 0.25m data - need to fix percent cover graphs and make individual graphs
  #=================================================================================================================================
  #Percent cover 
    # SPES
      cover_SPES <- data.frame(site_TA = quad0.25m_SPES_TA$Site_TA,
                                       year = quad0.25m_SPES_TA$Year,
                                       season = quad0.25m_SPES_TA$season,
                                       total_cover = quad0.25m_SPES_TA$Total_Cover,
                                       algae_cover = quad0.25m_SPES_TA$Adjusted_Algae_Cover,
                                       invert_cover = quad0.25m_SPES_TA$Adjusted_Invert_Cover)
      cover_SPES <- cover_SPES[complete.cases(cover_SPES$total_cover) &
                                                 complete.cases(cover_SPES$algae_cover) &
                                                 complete.cases(cover_SPES$invert_cover), ]
      
      cover_agg_SPES <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ season + site_TA, data = cover_SPES, FUN = mean, na.action = na.omit)
      cover_agg_sd_SPES <- aggregate(invert_cover ~ season + site_TA, 
                                    data = cover_SPES, 
                                    FUN = function(x) sd(x, na.rm = TRUE),
                                    na.action = na.omit)
        names(cover_agg_sd_SPES)[names(cover_agg_sd_SPES) == "invert_cover"] <- "sd"

    # ENVR
      cover_ENVR <- data.frame(site_TA = quad0.25m_ENVR$Site_TA,
                               year = quad0.25m_ENVR$Year,
                               season = quad0.25m_ENVR$season,
                               total_cover = quad0.25m_ENVR$Total_Cover,
                               algae_cover = quad0.25m_ENVR$Adjusted_Algae_Cover,
                               invert_cover = quad0.25m_ENVR$Adjusted_Invert_Cover)
      cover_ENVR <- cover_ENVR[complete.cases(cover_ENVR$total_cover) &
                                 complete.cases(cover_ENVR$algae_cover) &
                                 complete.cases(cover_ENVR$invert_cover), ]
      
      cover_agg_ENVR <- aggregate(cbind(total_cover, algae_cover, invert_cover) ~ season + site_TA, data = cover_ENVR, FUN = mean, na.action = na.omit)
      cover_agg_sd_ENVR <- aggregate(invert_cover ~ season + site_TA, 
                                     data = cover_ENVR, 
                                     FUN = function(x) sd(x, na.rm = TRUE),
                                     na.action = na.omit)
        names(cover_agg_sd_ENVR)[names(cover_agg_sd_ENVR) == "invert_cover"] <- "sd"

    # Combined
      cover_agg_combined <- rbind(cover_agg_SPES, cover_agg_ENVR)
      cover_agg_sd_combined <- rbind(cover_agg_sd_SPES, cover_agg_sd_ENVR)
      
      cover_merge <- merge(cover_agg_combined, cover_agg_sd_combined, by = c("season", "site_TA"))
      
    # plot - arshia can you look i give up 
      ggplot(cover_merge, aes(x = factor(site_TA), y = total_cover)) +
        geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - sd/2, ymax = invert_cover + sd/2,
                          group = site_TA),  # Group by site_TA
                      position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
        facet_grid(~season, scales = "free_x") +  # Facet by season with different bar graphs for each site
        labs(x = "Site_TA", y = "Total Cover", title = "Total Cover segmented by Algae and Invertebrates") +
        scale_fill_manual(values = c("Algae" = "green", "Invertebrates" = "blue")) +
        theme_minimal()
      
      ggplot(cover_merge, aes(x = factor(site_TA), y = total_cover)) +
        geom_bar(aes(fill = "Algae"), position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover, fill = "Invertebrates"), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - sd/2, ymax = invert_cover + sd/2,
                          group = site_TA),  # Group by site_TA
                      position = position_dodge(width = 0.9), width = 0.5) +  # Use position_dodge()
        facet_grid(season ~ ., scales = "free_x", space = "free_x") +  # Facet by season with different bar graphs for each site
        labs(x = "Site_TA", y = "Total Cover", title = "Total Cover segmented by Algae and Invertebrates") +
        scale_fill_manual(values = c("Algae" = "green", "Invertebrates" = "blue")) +
        theme_minimal()
      
      ggplot(cover_merge, aes(x = factor(site_TA), y = total_cover, fill = season)) +
        geom_bar(position = "stack", stat = "identity") +
        geom_bar(aes(y = invert_cover), position = "stack", stat = "identity") +
        geom_errorbar(aes(ymin = invert_cover - sd/2, ymax = invert_cover + sd/2,
                          group = interaction(site_TA, season)),
                      position = position_dodge(width = 0.9), width = 0.5) +
        labs(x = "Site_TA", y = "Total Cover", title = "Total Cover segmented by Algae and Invertebrates") +
        scale_fill_discrete(name = "Season") +
        theme_minimal()
  #=================================================================================================================================
  
# Abiotic Analysis - scale looks better but need to fix aesthetics
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
    monthly_temperature_data <- merge(average_monthly_max_temperature, average_monthly_min_temperature, by = "month")
    monthly_abiotic_data <- merge(monthly_temperature_data, average_low_tide_time, by = "month")

  # plotting - time of minimum tide height - need to figure out temperature scale 
    monthly_abiotic_data$month <- factor(monthly_abiotic_data$month, levels = 1:12,
                                         labels = c("January", "February", "March", "April", "May", "June",
                                                    "July", "August", "September", "October", "November", "December"))
  
    ggplot(monthly_abiotic_data, aes(x = month)) +
      geom_bar(aes(y = low_tide_time), stat = "identity", fill = "blue", alpha = 0.5) +
      geom_line(aes(y = Avg_Max_Temp, group = 1), color = "red") +
      geom_line(aes(y = Avg_Min_Temp, group = 1), color = "green") +
      scale_y_continuous(sec.axis = sec_axis(~.+5, name = "Temperature (°C)")) +
      labs(x = "Month",
           y = "Low Tide Time (hrs)",
           title = "Monthly Low Tide Time and Temperature",
           color = "Temperature") +
      theme_minimal()
    
#=================================================================================================================================

    
