library(dplyr)

transect <- read.csv("data/Transect_Data.csv", check.names = FALSE, na.strings=c("N/A", ""))
quad100 <- read.csv("data/1m_Quadrat.csv", check.names = FALSE, na.strings=c("N/A", ""))
limpet <- read.csv("data/Limpet_Data.csv", check.names = FALSE, na.strings=c("N/A", ""))

transect %>% group_by(Date, `Site TA`)