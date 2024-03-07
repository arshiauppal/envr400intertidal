# March 7, 2024

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

add_year_column <- function(transect, date_column_name) {
  transect[[date_column_name]] <- format(as.Date(transect[["Date"]]), "%d/%m/%Y")
  transect$Year <- format(as.Date(transect[[date_column_name]], "%d/%m/%Y"), "%Y")
  return(transect)
}

transect <- add_year_column(transect, "Date")
transect

quad1m <- add_year_column(quad1m, "Date")
quad1m

quad0.25m <- add_year_column(quad0.25m, "Date")
quad0.25m

limpet <- add_year_column(limpet, "Date")
limpet


# change 0 and 1 values to true false/ presence absence - make a function ****
change_0_1_to_logical <- function(df, start_col_index, end_col_index) {
  df <- df %>%
    mutate(across(start_col_index:end_col_index, as.logical))
  return(df)
}

quad0.25m <- change_0_1_to_logical(quad0.25m, 14, 44)
quad0.25m

quad1m <- change_0_1_to_logical(quad1m, 12, 36)
quad1m

transect <- change_0_1_to_logical(transect, 12, 14)
transect
