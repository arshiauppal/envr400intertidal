# ENVR 400 2024 Intertidal Data Analysis 

## Summary
An R-script for analysis of intertidal data collected by SPES and ENVR 400 2024 project.

Provides base functions which can be used to create visualizations of data and run statistical analysis of yearly, site and seasonal trends

## How to Run File 
The main R script is located at 
`envr400intertidal/********.R` and can be run within R-studio. 

## Required Libraries 
- dplr
- tidyr
- stringr
- ggplot2
- libridate
- reshape2
- viridis
- RColorBrewer

## Summary of Data
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
Initial data required pre-processing in excel to put information contained in the comments within the data to useable data. This included including species presence and absence along with denoting species counts where available. The raw data is not accessible but the pre-processed data is contained within the included .csv files. 

Cleaning of Data:
In order to perform statistical analysis of data, multiple functions are contained within the R-script which change the type of data in the column (ie. logical) and the creation of new columns (ie. season, month). The functions with descriptions are included within the R-script

Visualizations:
For most of the variables present within the data visualizations were created. These included bar graphs of count and percent cover, tiling graphs of species presence/absence overtime and across sites, and additional tiling graphs of changes in species counts and percent covers across the intertidal height.

Statistical Analysis:
After each visualization basic statistical tests were conducted. An analysis of variance test (ANOVA) for assessing differences between years and between sampled sites. A linear regression model was developed for assessing seasonal trends in data from summer 2023-winter 2024.



