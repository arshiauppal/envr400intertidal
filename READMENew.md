# ENVR 400 2024 Intertidal Data Analysis 

## Summary
An R-script for analysis of intertidal data collected by SPES and ENVR 400 2024 project.
Provides base functions which can be used to create visualizations of data and run statistical analysis of yearly, site and seasonal trends

## How to Run File 
The main R script is located at 
`envr400intertidal/finalcopy.R` and can be run within R-studio. 

## Required Libraries 
- dplr
- tidyr
- stringr
- ggplot2
- libridate
- reshape2
- viridis
- RColorBrewer

## Summary of Data that is Used
The data can be found at `envr400intertidal/data`
There are 3 categories of data that are used in this analysis:
  - SPES data located at `envr400intertidal/data/SPES`
  - ENVR 400 Data located at `envr400intertidal/data/ENVR_2024`
  - Abiotic data located at `envr400intertidal/data/Abiotic`

### SPES Data 
This data includes summer data from 2019-2023 of the intertidal biodiversity within Stanley Park that was provided by Stanley Park Ecological Society (SPES). 
The collected data had 4 different resolutions: transect, 1m and 0.25m quadrat, and limpet size measurements. Each resolution provides differential data with transect and 1m quadrat data providing counts of different intertidal organisms, 0.25m quadrat data consisting of percent cover and species identifcation data, and the limpet size data includes length and width measurements of up to 10 limpet individuals across the transect line.

The SPES data consists of 4 different csv files:
  - Transect data is located at `envr400intertidal/data/SPES/Transect_SPES.csv`
  - 1m quadrat data is located at `envr400intertidal/data/SPES/1m_SPES.csv`
  - O.25m quadrat data is located at `envr400intertidal/data/SPES/0.25m_SPES.csv`
  - Limpet size data is located at `envr400intertidal/data/SPES/Limpet_SPES.csv`

### ENVR 400 Data
This data includes winter intertidal biodiversity at Stanley Park from December 2023-February 2024.  
The collected data had 3 different resolutions, complementary to SPES' data: transect, 0.25m quadrat, and limpet size measurements. Each resolution contains similar data collected as with SPES' data. 

The ENVR 400 data consists of 3 different csv files:
  - Transect data is located at`envr400intertidal/data/ENVR_400/Transect_ENVR_2024.csv`
  - 0.25 m quadrat data is located at `envr400intertidal/data/ENVR_400/0.25m_ENVR_2024.csv`
  - Limpet size data is located at `envr400intertidal/data/ENVR_400/Limpet_ENVR_2024.csv`

### Abiotic data
The data consists of weather data, and tidal height data. 
The weather data was provided by Dr. Roland Stull from UBC and was collected from data from the rooftop of UBC's ESB building from January 1 2019 till January 1 2024. Data includes includes measurements of daily air temperatures, relative humidity, hourly precipitation, wind speed, wind direction and pressure.
The tidal height data was obtained Vancouver station 07735 [Government of Canada’s tide prediction database](https://www.tides.gc.ca/en/stations/7735). This data includes the hourly tide heights, in meters, from January 1st, 2019 till January 1st 2024.

The Abiotic data consists of 2 csv files:
  - Climatic data is located at `envr400intertidal/data/Abiotic/UBC_Rooftop_obs_2019-2024.csv`
  - Tidal height data is located at `envr400intertidal/data/Abiotic/Tide_Jan012019-2024.csv`

## Description of Analysis
Pre-processing of Data: 
In our data analysis workflow, the preliminary SPES data required data cleaning through Excel and R-studio to create comparable data visualizations and analyses between our collected winter data. On Excel, comments denoting individual species distinction were transformed into individual columns denoting presence-absence (0/1 counts), or individual count depending on the type of comment. Once all three categories of data were loaded onto R, each file within the SPES, 400, and abiotic data were read into a dataframe.

Cleaning of Data: 
To clean the data for our analysis, functions in R-studio were created to extract the year from the collection date along with a designation of the season according to the month (Winter = December, January, February, Spring = March, April, May, Summer = June, July, August). 

A function to transform the 0/1 values indicating species presence were converted to TRUE and FALSE using another created function. 

Additionally, a function was created to categorize the length of the transects to a low height (0-10m, closest to the water), medium height (11-20), and high height (21-30m, closest to the seawall). Finally, as the percent cover of algae and invertebrates was collected on a scale of 0-100%, a function was created to determine the percent cover of the organismal class proportional to the total percent cover. This was calculated as the (relative percent cover of the organismal class/100) *total percent cover. This was done to create appropriate visualizations of the total percent cover and the relative contribution of algae and invertebrates to that overall cover.

Visualizations:
For most of the variables present within the data visualizations were created. These included bar graphs of count and percent cover, tiling graphs of species presence/absence overtime and across sites, and additional tiling graphs of changes in species counts and percent covers across the intertidal height.

Statistical Analysis:
After each visualization basic statistical tests were conducted. An analysis of variance test (ANOVA) for assessing differences across years and between sampled sites. A linear regression model was developed for assessing seasonal trends in data from summer 2023-winter 2024.



