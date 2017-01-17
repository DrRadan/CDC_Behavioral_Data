#Takes in raw data and processes the fiel values to be intuitively understandable (reference the NICH codebook at www.cdc.gov)
beautify <- function(dataset, column_list=c("STATE", "AGEYR_CHILD", "TOTKIDS4","AGEPOS4"))   #general dataset values to be included in output
                                             {
  require("dplyr")
  dataset <- tbl_df(dataset)
  
  #First make "STATES" human-readable
  states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS",
              "KY","LA","ME","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","MD","MA","MI","MN","MS",
              "MO","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  for (i in 1:length(states)){
    a <-which(dataset$STATE==i)
    dataset$STATE[a] <- states[i]
  }
  
  #Retrieve the fields described at variable "column list" + the weights
  diagnostic <- c("EDUCATIONR","EDUC_PARR", "EDUC_MOMR") #possible, alternative, column names of variables I want to summarize (currently the weights)
  test <- colnames(dataset) #the colnames of my current table
  
  if (length(grep(diagnostic[2],test))>0) { #variable present only at the 2011-2012 dataset
    var1<-c( "K10Q30", "K10Q31", "K10Q32") #dataset-specific values to be included in output, notice that this dataset does not have specific variables for height and weight, only BMICLASS
    var2 <- c("SEX","PLANGUAGE", "EDUC_PARR","K11Q60", "K11Q61", "K11Q62","BMICLASS", "POVERTY_LEVELR")    #dataset-specific values not to be included in output
    dots <- lapply(c(column_list,var1,var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(WEIGHT=sum(NSCHWT, na.rm=TRUE)) #compress based of selected features
    #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset %>% mutate(LANGUAGE=ifelse(is.na(PLANGUAGE)==T | PLANGUAGE==6 | PLANGUAGE == 7, NA, ifelse(PLANGUAGE == 1, "ENG", ifelse(PLANGUAGE== 2,"NENG", "what?")))) %>%
      mutate(EDUCATION_LVL = ifelse( is.na(EDUC_PARR)==T | EDUC_PARR==6 | EDUC_PARR==7, NA, ifelse(EDUC_PARR==1, "highschool-", ifelse(EDUC_PARR==2, "highschool", ifelse(EDUC_PARR==3, "highschool+", "what?"))))) %>%
      mutate( TANF=ifelse(is.na(K11Q60)==T | K11Q60==6 | K11Q60 == 7 | K11Q60 == "P", NA, ifelse(K11Q60 == 0 | K11Q60 == "L", 0, 1))) %>%
      mutate( SNAP=ifelse(is.na(K11Q61)==T | K11Q61==6 | K11Q61 == 7 | K11Q61 == "P", NA, ifelse(K11Q61 == 0 | K11Q61 == "L", 0, 1))) %>%
      mutate( WIC=ifelse(is.na(K11Q62)==T | K11Q62==6 | K11Q62 == 7 | K11Q62 == "P", NA, ifelse(K11Q62 == 0 | K11Q62 == "L", 0, 1))) %>%
      mutate(BMI_category = ifelse( is.na(BMICLASS)==T | BMICLASS=="M" | BMICLASS=="N", NA, ifelse(BMICLASS==1, "U", ifelse(BMICLASS==2, "N", ifelse(BMICLASS==3, "O", ifelse(BMICLASS==4, "MO", "what?"))))))  %>%
      mutate( child_sex=ifelse(is.na(SEX)==T | SEX==6 | SEX == 7 , NA, ifelse(SEX == 2, "F", ifelse(SEX==1, "M", "what?")))) %>% 
      mutate( Income_class=ifelse(is.na(POVERTY_LEVELR)==T | POVERTY_LEVELR=="M" , NA, ifelse(POVERTY_LEVELR == 1, "A", ifelse(
        POVERTY_LEVELR==2, "B", ifelse(POVERTY_LEVELR == 3, "C", ifelse(POVERTY_LEVELR == 4, "D", ifelse(POVERTY_LEVELR == 5, "E", ifelse(POVERTY_LEVELR == 6, "F", ifelse(
          POVERTY_LEVELR == 7, "G", ifelse(POVERTY_LEVELR == 8, "H", "what?"))))))))))
      
    
    return(dataset[,c(column_list,var1, "Income_class","child_sex","BMI_category","TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE", "WEIGHT")])
  }
  
  else  if (length(grep(diagnostic[3],test))>0) { #variable present in the 2011-2012 and 2007 datasets
    var1<-c(  "K10Q30", "K10Q31", "K10Q32",  "K2Q02R", "K2Q03R") #dataset-specific values to be included in output
    var2 <- c( "SEX","EDUC_MOMR", "EDUC_DADR","PLANGUAGE","K11Q60", "K11Q61", "K11Q62","BMICLASS", "POVERTY_LEVELR") #dataset-specific values not to be included in output
    dots <- lapply(c(column_list,var1, var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(WEIGHT=sum(NSCHWT, na.rm=TRUE)) #compress based of selected features
    #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset %>% mutate(LANGUAGE=ifelse(is.na(PLANGUAGE)==T | PLANGUAGE==6 | PLANGUAGE == 7, NA, ifelse(PLANGUAGE == 1, "ENG", ifelse(PLANGUAGE== 2,"NENG", "what?")))) %>%
      mutate(temp=ifelse(EDUC_MOMR >= EDUC_DADR, EDUC_MOMR, EDUC_DADR)) %>% 
      mutate(EDUCATION_LVL = ifelse( is.na(temp)==T | temp==6 | temp==7, NA, ifelse(temp==1, "highschool-", ifelse(temp==2, "highschool", ifelse(temp==3, "highschool+", "what?"))))) %>%
      mutate( TANF=ifelse(is.na(K11Q60)==T | K11Q60==6 | K11Q60 == 7 | K11Q60 == "P", NA, ifelse(K11Q60 == 0 | K11Q60 == "L", 0, 1))) %>%
      mutate( SNAP=ifelse(is.na(K11Q61)==T | K11Q61==6 | K11Q61 == 7 | K11Q61 == "P", NA, ifelse(K11Q61 == 0 | K11Q61 == "L", 0, 1))) %>%
      mutate( WIC=ifelse(is.na(K11Q62)==T | K11Q62==6 | K11Q62 == 7 | K11Q62 == "P", NA, ifelse(K11Q62 == 0 | K11Q62 == "L", 0, 1))) %>%
      mutate(BMI_category = ifelse( is.na(BMICLASS)==T | BMICLASS=="M" | BMICLASS=="N", NA, ifelse(BMICLASS==1, "U", ifelse(BMICLASS==2, "N", ifelse(BMICLASS==3, "O", ifelse(BMICLASS==4, "MO", "what?")))))) %>%
      mutate( child_sex=ifelse(is.na(SEX)==T | SEX==6 | SEX == 7 , NA, ifelse(SEX == 2, "F", ifelse(SEX==1, "M", "what?"))))%>% 
      mutate( Income_class=ifelse(is.na(POVERTY_LEVELR)==T | POVERTY_LEVELR=="M" , NA, ifelse(POVERTY_LEVELR == 1, "A", ifelse(
        POVERTY_LEVELR==2, "B", ifelse(POVERTY_LEVELR == 3, "C", ifelse(POVERTY_LEVELR == 4, "D", ifelse(POVERTY_LEVELR == 5, "E", ifelse(POVERTY_LEVELR == 6, "F", ifelse(
          POVERTY_LEVELR == 7, "G", ifelse(POVERTY_LEVELR == 8, "H", "what?"))))))))))
    
    
    return(dataset[,c(column_list,var1, "Income_class","child_sex", "BMI_category","TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE", "WEIGHT")])
  }
  
  else if (length(grep(diagnostic[1],test))>0) { #variable present in the 2007 and 2003 datasets
    var1<-c( "S10Q01","S10Q02", "S10Q03",  "S2Q02R", "S2Q03R") #dataset-specific values to be included in output
    var2 <- c("S1Q01" , "PLANGUAGE", "EDUCATIONR","C11Q11", "C11Q11A", "C11Q11B","BMICLASS", "POVERTY_LEVELR") #dataset-specific values not to be included in output
    dots <- lapply(c(column_list,var1, var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(WEIGHT=sum(WEIGHT_I, na.rm=TRUE))  #compress based of selected features
      #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset %>% mutate(LANGUAGE=ifelse(is.na(PLANGUAGE)==T | PLANGUAGE==6 | PLANGUAGE == 7, NA, ifelse(PLANGUAGE == 1, "ENG", ifelse(PLANGUAGE== 2,"NENG", "what?")))) %>%
      mutate(EDUCATION_LVL = ifelse( is.na(EDUCATIONR)==T | EDUCATIONR==6 | EDUCATIONR==7, NA, ifelse(EDUCATIONR==1, "highschool-", ifelse(EDUCATIONR==2, "highschool", ifelse(EDUCATIONR==3, "highschool+", "what?"))))) %>%
      mutate(TANF=ifelse(is.na(C11Q11)==T | C11Q11==6 | C11Q11 == 7 | C11Q11 == "P", NA, ifelse(C11Q11 == 0 | C11Q11 == "L", 0, 1))) %>%
      mutate(SNAP=ifelse(is.na(C11Q11A)==T | C11Q11A==6 | C11Q11A == 7 | C11Q11A == "P", NA, ifelse(C11Q11A == 0 | C11Q11A == "L", 0, 1))) %>%
      mutate(WIC=ifelse(is.na(C11Q11B)==T | C11Q11B==6 | C11Q11B == 7 | C11Q11B == "P", NA, ifelse(C11Q11B == 0 | C11Q11B == "L", 0, 1))) %>%
      mutate(BMI_category = ifelse( is.na(BMICLASS)==T | BMICLASS=="M" | BMICLASS=="N", NA, ifelse(BMICLASS==1, "U", ifelse(BMICLASS==2, "N", ifelse(BMICLASS==3, "O", ifelse(BMICLASS==4, "MO", "what?")))))) %>%
      mutate( child_sex=ifelse(is.na(S1Q01)==T | S1Q01==6 | S1Q01 == 7 , NA, ifelse(S1Q01 == 2, "F", ifelse(S1Q01==1, "M", "what?"))))%>% 
      mutate( Income_class=ifelse(is.na(POVERTY_LEVELR)==T | POVERTY_LEVELR=="M" , NA, ifelse(POVERTY_LEVELR == 1, "A", ifelse(
        POVERTY_LEVELR==2, "B", ifelse(POVERTY_LEVELR == 3, "C", ifelse(POVERTY_LEVELR == 4, "D", ifelse(POVERTY_LEVELR == 5, "E", ifelse(POVERTY_LEVELR == 6, "F", ifelse(
          POVERTY_LEVELR == 7, "G", ifelse(POVERTY_LEVELR == 8, "H", "what?"))))))))))
    
    return(dataset[,c(column_list,var1,"Income_class","child_sex","BMI_category", "TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE", "WEIGHT")])
  } 
  
  else { #in the case of unknown formats
    print("I cannot recognize the dataset you entered as a SLAITS survey table. Please input one of the 2011-2012, 2007, or 2003 data tables.")
  }
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Calculate and add exact BMI value to existing table, class tbl_df from the dplyr package, produced from function beautify()
bmi <- function(dataset){
  require("dplyr")
  #Calculate BMI
  vars <- colnames(dataset) #the colnames of my current table
  if (length(grep("S2Q03R",vars))>0) { #diagnostic of 2003
    dataset <- dataset %>% mutate(BMI=ifelse(is.na(BMI_category)==T, NA, (S2Q03R*0.45)/(S2Q02R*0.025)^2))
    vars <- colnames(dataset) # update the colnames list 
    drop = c("S2Q03R", "S2Q02R")
    dataset <- dataset[,!vars %in% drop]
    return(dataset)
  } 
  else  if (length(grep("K2Q03R",vars))>0) { #diagnostic of 2007
    dataset <- dataset %>% mutate(BMI=ifelse(is.na(BMI_category)==T, NA,(K2Q03R*0.45)/(K2Q02R*0.025)^2))
    vars <- colnames(dataset) # update the colnames list 
    drop = c("K2Q03R","K2Q02R")
    dataset <- dataset[,!vars %in% drop]
    return(dataset)
  } 
  else { #bmi calculation is not possible for 2011-2012
    print("There is no weight and height information in the dataset. If the dataset comes from the 2011-2012 survey it is a known omission. Please input tables from either the 2003 or 2007 surveys")
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data2freq <- function(dataset){
  require("dplyr")
  column_list=colnames(dataset)
  var <- c("WEIGHT_I","NSCHWT") #possible, alternative, column names of variables I want to summarize (currently the weights)
  dots <- lapply(column_list, as.name)
  if (length(grep(var[1],column_list))>0) { 
    dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(TOT=ceiling(sum(WEIGHT_I, na.rm=TRUE)/100))
    #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset[,c(column_list, "TOT")]
    
    library("splitstackshape")
    dataset <- expandRows(dataset, "TOT")
  } 
  else  if (length(grep(var[2],column_list))>0) {
    dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(TOT=ceiling(sum(NSCHWT, na.rm=TRUE)/100))
    dataset <- dataset[,c(column_list,"TOT")]
    
    library("splitstackshape")
    dataset <- expandRows(dataset, "TOT")
  } 
  else {
    print("There is no column WEIGHT_I or NSCHWT in the dataset to summarize. Please input the correct weight column name")
  }
}
