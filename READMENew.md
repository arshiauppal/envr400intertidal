# ENVR 400 Interdial Data Analysis 

## Summary


## How to Run File 
The main R script is located at 
`envr400intertidal/finalcopy.R` and can be run within Rstudio to create the visualization. 

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
The SPES data consists of 4 different csv files:
- `envr400intertidal/data/SPES/Transect_SPES.csv`
- `envr400intertidal/data/SPES/1m_SPES.csv`
- `envr400intertidal/data/SPES/0.25m_SPES.csv`
- `envr400intertidal/data/SPES/Limpet_SPES.csv`

These include summer data from 2019-2023 that was provided by Stanley Park Ecological Society. The data was preprocessed on excel to create logical categories from the comments column to indicate species presence or absence. This helped in the ease of access of data to conduct further analysis and create visdualizations. The data includes Transect data, 1m Quadrat, 0.25 Quadrat, and Limpet data respectively. 

### ENVR 400 Data
The ENVR 400 data consists of 3 different csv files:
- `envr400intertidal/data/ENVR_400/Transect_ENVR_2024.csv`
- `envr400intertidal/data/ENVR_400/0.25m_ENVR_2024.csv`
- `envr400intertidal/data/ENVR_400/Limpet_ENVR_2024.csv`

This data was obtained from sampling at Stanley Park over this project's time period in 2024. 
The data was formatted a bit different, with some ID's to aggregate over, as well as the logical columns to indicate presense or absence. The data includes Transect data, 0.25m qudrat, and Limpet data respectively. 

### Abiotic data
The Abiotic data consists of 2 csv files:
- `envr400intertidal/data/Abiotic/UBC_Rooftop_obs_2019-2024.csv`
- `envr400intertidal/data/Abiotic/Tide_Jan012019-2024.csv`

The data consists of weather data, and Tide data repestively. The weather data was provided by Dr. Roland Stull from UBC, which includes weather data from the rooftop of UBC's ESB building to provide some Abiotic variables for our statistical analysis.

The abiotic data also included the Vancouver predictions of the lowest low tide using the [Government of Canada’s tide prediction database](https://www.tides.gc.ca/en/stations/7735). This Data provided the hourly tide heights, in meters, from January 1st, 2019 till January 1st 2024.

## Description of Analysis
Pre-processing of Data: In our data analysis workflow, the preliminary SPES data required data cleaning through Excel and R-studio to create comparable data visualizations and analyses between our collected winter data. On Excel, comments denoting individual species distinction were transformed into individual columns denoting presence-absence (0/1 counts), or individual count depending on the type of comment. Once all three categories of data were loaded onto R, each file within the SPES, 400, and abiotic data were read into a dataframe.

Cleaning of Data: To clean the data for our analysis, functions in R-studio were created to extract the year from the collection date along with a designation of the season according to the month (Winter = December, January, February, Spring = March, April, May, Summer = June, July, August). 

A function to transform the 0/1 values indicating species presence were converted to TRUE and FALSE using another created function. 

Additionally, a function was created to categorize the length of the transects to a low height (0-10m, closest to the water), medium height (11-20), and high height (21-30m, closest to the seawall). Finally, as the percent cover of algae and invertebrates was collected on a scale of 0-100%, a function was created to determine the percent cover of the organismal class proportional to the total percent cover. This was calculated as the (relative percent cover of the organismal class/100) *total percent cover. This was done to create appropriate visualizations of the total percent cover and the relative contribution of algae and invertebrates to that overall cover.





