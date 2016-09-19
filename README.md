# MISR
Code for processing and integrating MISR satellite data with air quality and meteorological data.
File list:
1. Data Processing
* EPA Data Processing.R   Code to process downloaded EPA AQS air monitoring data (PM2.5, PM10, STN PM2.5)
* MISR Data Processing.R  Code to extract local mode (4.4 km) aerosol data from MISR netcdf files
* NCDC Data Processing.R  Code to wget and process NOAA weather station data

2. Spatio-temporal Modeling and Mapping
* MISR AQS MET Match.R    Code to spatially and temporally link MISR AOD with AQS and meteorology data
* MISR Mapping.R          Code to create maps and visualizations of MISR data
* MISR Modeling.R         Code to conduct sptio-temporal modeling (See Franklin, Kalashnikova, Garay RSE 2016)
