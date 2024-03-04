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



# merge data
#=================================================================================================================================
# not doing rn
quad100_tran <- left_join(quad100, transect, by = c("Site TA", "Site & transect", "Date"))
quad25_tran <- left_join(quad25, transect, by = c("Site TA", "Site & transect", "Date"))

# early plot 
#=================================================================================================================================

ggplot(transect, aes(x = 'Year', y = ')', fill = 'Site TA')) +
  geom_bar(stat = "identity")

limpet$`Site TA` <- as.character(limpet$`Site TA`)
limpet$`Mean Length (mm)` <- as.numeric(limpet$`Mean Length (mm)`)

ggplot(limpet, aes(x = "Site TA", y = "Mean Length (mm)")) + 
  geom_bar(stat = 'identity')

l1 <- limpet[which(limpet$`Site TA` == 1),]



function(ta_name,)

# Sea star transect plots 
Transect_Sea_Star <- ggplot(transect, aes(x = Year, y = Density of Sea Stars (Count), fill = Year)) +
  geom_bar(stat = "identity")
Transect_Sea_Star

# Limpet Size distribution per site
Limpet_Length_Site <- ggplot(limpet, aes(x = "Site TA", y = "Mean Length (mm)", fill = "Site TA")) + 
    geom_bar(stat = 'identity')
Limpet_Length_Site

TEST

