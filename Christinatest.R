require(dplyr)
require(stringr)

# read data
#=================================================================================================================================
`transect <- read.csv("data/Transect_Data.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad100 <- read.csv("data/1m_Quadrat.csv", check.names = FALSE, na.strings=c("N/A", ""))``
quad25 <- read.csv("data/0.25m_Quadrat.csv", check.names = FALSE, na.strings=c("N/A", ""))

# clean data
#=================================================================================================================================
# transect$Date <- format(as.Date(transect$Date), "%d/%m/%Y") # transform date such that quadrat and transect data match
quad100 <- quad100[,colSums(is.na(quad100))<nrow(quad100)] # delete all columns where all values are NA
quad25 <- quad25[,colSums(is.na(quad25))<nrow(quad25)] 

# merge data
#=================================================================================================================================
quad100_tran <- left_join(quad100, transect, by = c("Site TA", "Site & transect", "Date"))
quad25_tran <- left_join(quad25, transect, by = c("Site TA", "Site & transect", "Date"))

