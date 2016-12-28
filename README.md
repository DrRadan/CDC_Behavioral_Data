# CDC_Behavioral_Data
#####Python and R functions wrangling codified datasets that have been locally downloaded

The first part of the file refers to the specific data set (see bellow).

For each dataset there are two files: a _data_exploration file that contains simple script for exploring the downloaded data, and a _wrangling_function file that contains the function to automatically extract pre-selected features and modify the fields so that they make human sense (eg substitute code '7' with 'NA', according to manual). The function files can be easily modified to include more features and change the codes, by adding subroutines.

## Datasets

#### 1.BRFSS, Behavior Risk Factor Surveillance System
Annual survey data, collected from the CDC. Source data and documentation can be found [here](https://www.cdc.gov/brfss/annual_data/annual_data.htm).
The current version imports the following features: '_STATE', 'AGE', 'EDUCA', '_RFBMI5' 
