# CDC_Behavioral_Data
##### Python and R functions wrangling codified datasets that have been locally downloaded

The first part of the file refers to the specific data set (see bellow).

For each dataset there are two files: a _data_exploration file that contains simple script for exploring the downloaded data, and a _wrangling_function file that contains the function to automatically extract pre-selected features and modify the fields so that they make human sense (eg substitute code '7' with 'NA', according to manual). The function files can be easily modified to include more features and change the codes, by adding subroutines.

## Datasets

#### 1.BRFSS, Behavior Risk Factor Surveillance System
Annual survey data, collected from the CDC. Source data and documentation can be found [here](https://www.cdc.gov/brfss/annual_data/annual_data.htm).
The current version imports the following features: '_STATE', 'AGE', 'EDUCA', '_RFBMI5' 

#### 2. SLAITS, State and Local Area Integrated Telephone Survey
Periodic cross-sectional data. Available for 2003, 2007 and 2011. Here only the subset regarding children’s welfare indexes are included ([National Survey of Children’s Health] (https://www.cdc.gov/nchs/slaits/nsch.htm)). 

There are three functions defined in the R script SLAITS_wrangling_functions:
	a. beautify()
	Extracts features "STATE", "AGEYR_CHILD", "TOTKIDS4","AGEPOS4", "BMICLASS", "POVERTY_LEVELR”, "EDUCATION_LVL","LANGUAGE", "WEIGHT”, “SEX”, and converts codes into human-readable entries
	b. bmi()
	Calculated BMI based on the WEIGHT and HEIGHT (self-reported) values from the SLAITS datasets
	c. data2freq()
	This function used the weights column and created tables that are representative of the population using them
