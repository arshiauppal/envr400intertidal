setwd("~/Documents/envr400/envr400intertidal")
require(dplyr)

tr <- read.csv("data/Transect_Data.csv", check.names = FALSE)
quad <- read.csv("data/1m_Quadrat.csv", check.names = FALSE)

tr$Date <- format(as.Date(tr$Date), "%d/%m/%Y")


df <- left_join(quad, tr, by = c("Site & transect", "Date"))
