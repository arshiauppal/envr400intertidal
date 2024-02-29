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
# delete all columns where all values are NA
transect <- transect[,colSums(is.na(transect))<nrow(transect)] 
limpet <- limpet[,colSums(is.na(limpet))<nrow(limpet)] 
quad0.25m <- quad0.25m[,colSums(is.na(quad0.25m))<nrow(quad0.25m)] 
quad1m <- quad1m[,colSums(is.na(quad1m))<nrow(quad1m)] 

# Creating a year column
transect$Date <- format(as.Date(transect$Date), "%d/%m/%Y")
transect$Year <- transect$Date
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

# Sea star transect plots 
Transect_Sea_Star <- ggplot(transect, aes(x = factor(Year), y =`Density of Sea Stars (Count)`, fill = factor(Year))) +
  geom_bar(stat = "identity")
Transect_Sea_Star



