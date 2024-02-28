# Feb 27, 2024

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
# idk if we need
quad1m <- quad100[,colSums(is.na(quad100))<nrow(quad100)] # delete all columns where all values are NA
quad0.25m <- quad25[,colSums(is.na(quad25))<nrow(quad25)] 

# Creating a year column
transect$Date <- format(as.Date(transect$Date), "%d/%m/%Y")
transect$Year <- transect$Date
transect$Year <-  format(as.Date(transect$Date, "%d/%m/%Y"), "%Y")

# change 0 and 1 values to true false/ presence absence 
quad0.25m <- quad0.25m |> 
    mutate(across('SUM WORM': 'SUM HERMIT CRAB', as.logical))

transect <- transect |> 
  mutate(across('Ochre': 'Molted', as.logical))

# change limpet column name?

# merge data
#=================================================================================================================================
# not doing rn
quad100_tran <- left_join(quad100, transect, by = c("Site TA", "Site & transect", "Date"))
quad25_tran <- left_join(quad25, transect, by = c("Site TA", "Site & transect", "Date"))

# early plot 
#=================================================================================================================================

Transect_Sea_Star <- ggplot(transect, aes(x = factor(transect$Year), y = transect$`Density of Sea Stars (Count)`, fill = factor(Year))) +
  geom_bar(stat = "identity")

