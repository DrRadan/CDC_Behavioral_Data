# CDC_Behavioral_Data
##### Python and R functions wrangling CDC public datasets that have been locally downloaded

The first part of the file name refers to the specific data set (see bellow).

For each dataset there are two files:  a _wrangling_function file that contains the function to automatically extract pre-selected features and modify the fields so that they make human sense (eg substitute code '7' with 'NA', according to manual). A _data_exploration file that contains simple script for exploring the downloaded data, containing examples of how to apply the wrangling functions. The function files can be easily modified to include more features and change the codes, by adding subroutines.

## Datasets

#### 1.BRFSS, Behavior Risk Factor Surveillance System
Annual survey data, collected from the CDC. Source data and documentation can be found [here](https://www.cdc.gov/brfss/annual_data/annual_data.htm).
The current version imports the following features: '_STATE', 'AGE', 'EDUCA', '_RFBMI5' 

#### 2. SLAITS, State and Local Area Integrated Telephone Survey
Periodic cross-sectional data. Available for 2003, 2007 and 2011. Here only the subset regarding children’s welfare indexes are included ([National Survey of Children’s Health] (https://www.cdc.gov/nchs/slaits/nsch.htm)). 

There are three functions defined in the R script SLAITS_wrangling_functions:

  a. beautify()

  Extracts features and converts codes into human-readable entries

  b. data2freq()

  This function used the weights column and created tables that are representative of the population using them

##Analysis
Analysis.R contains the scripts used for the comparison outcomes between recipients of two different food assistance programs, WIC and SNAP. I am interested in those because:
1. The recipients of the two benefits are assumed to be from essentially the same demographic pool. The responses are still weighted based on family income, sex and other demographic parameters, but I am more confident with regards to the existence of lurking variables by using these two groups.
2. Assuming all other demographics are matched, the main difference between the WIC and SNAP programs is that WIC is aimed as an incentive for healthier nutrition and food habits, while SNAP is a money supplement program. This  difference has interesting implications with regards to choice in adopting healthy nutritional habits.
