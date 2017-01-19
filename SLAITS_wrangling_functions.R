#Takes in raw data and processes the fiel values to be intuitively understandable (reference the NICH codebook at www.cdc.gov)
beautify <- function(dataset)  
                                             {
  
  require("dplyr")
  dataset <- tbl_df(dataset)
  
  #First make "STATES" human-readable
 # states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS",
 #             "KY","LA","ME","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","MD","MA","MI","MN","MS",
 #             "MO","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  
  states <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts', 'michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york', 'north carolina',
              'north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont', 'virginia','washington','west virginia','wisconsin','wyoming')
  for (i in 1:length(states)){
    a <-which(dataset$STATE==i)
    dataset$STATE[a] <- states[i]
  }
  
  column_list=c("STATE", "AGEYR_CHILD")  #universal dataset values to be included in output
  #Retrieve the fields described at variable "column list" + the weights
  diagnostic <- c("EDUCATIONR","EDUC_PARR", "EDUC_MOMR") #possible, alternative, column names of variables I want to summarize (currently the weights)
  test <- colnames(dataset) #the colnames of my current table
  
  if (length(grep(diagnostic[2],test))>0) { #variable present only at the 2011-2012 dataset
    var2 <- c("SEX","PLANGUAGE", "EDUC_PARR","K11Q60", "K11Q61", "K11Q62","BMICLASS", "POVERTY_LEVELR")    #dataset-specific values not to be included in output
    dots <- lapply(c(column_list,var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% 
      summarize(WEIGHT=sum(NSCHWT, na.rm=TRUE), COUNTS = length(NSCHWT)) #compress based of selected features. Useful for increasing speed, but should not count that the final result of this function is compressed
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
      
    
    return(dataset[,c(column_list, "Income_class","child_sex","BMI_category","TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE", "COUNTS","WEIGHT")])
  }
  
  else  if (length(grep(diagnostic[3],test))>0) { #variable present in the 2011-2012 and 2007 datasets
    var2 <- c( "SEX","EDUC_MOMR", "EDUC_DADR","PLANGUAGE","K11Q60", "K11Q61", "K11Q62","BMICLASS", "POVERTY_LEVELR", "K2Q03R", "K2Q02R") #dataset-specific values not to be included in output
    dots <- lapply(c(column_list, var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% 
      summarize(WEIGHT=sum(NSCHWT, na.rm=TRUE), COUNTS = length(NSCHWT)) #compress based of selected features. Useful for increasing speed, but should not count that the final result of this function is compressed
    #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset %>% mutate(LANGUAGE=ifelse(is.na(PLANGUAGE)==T | PLANGUAGE==6 | PLANGUAGE == 7, NA, ifelse(PLANGUAGE == 1, "ENG", ifelse(PLANGUAGE== 2,"NENG", "what?")))) %>%
      mutate(temp=ifelse(EDUC_MOMR >= EDUC_DADR, EDUC_MOMR, EDUC_DADR)) %>% 
      mutate(EDUCATION_LVL = ifelse( is.na(temp)==T | temp==6 | temp==7, NA, ifelse(temp==1, "highschool-", ifelse(temp==2, "highschool", ifelse(temp==3, "highschool+", "what?"))))) %>%
      mutate(BMI=ifelse(is.na(BMICLASS)==T, NA,(K2Q03R*0.45)/(K2Q02R*0.025)^2)) %>%
      mutate( TANF=ifelse(is.na(K11Q60)==T | K11Q60==6 | K11Q60 == 7 | K11Q60 == "P", NA, ifelse(K11Q60 == 0 | K11Q60 == "L", 0, 1))) %>%
      mutate( SNAP=ifelse(is.na(K11Q61)==T | K11Q61==6 | K11Q61 == 7 | K11Q61 == "P", NA, ifelse(K11Q61 == 0 | K11Q61 == "L", 0, 1))) %>%
      mutate( WIC=ifelse(is.na(K11Q62)==T | K11Q62==6 | K11Q62 == 7 | K11Q62 == "P", NA, ifelse(K11Q62 == 0 | K11Q62 == "L", 0, 1))) %>%
      mutate(BMI_category = ifelse( is.na(BMICLASS)==T | BMICLASS=="M" | BMICLASS=="N", NA, ifelse(BMICLASS==1, "U", ifelse(BMICLASS==2, "N", ifelse(BMICLASS==3, "O", ifelse(BMICLASS==4, "MO", "what?")))))) %>%
      mutate( child_sex=ifelse(is.na(SEX)==T | SEX==6 | SEX == 7 , NA, ifelse(SEX == 2, "F", ifelse(SEX==1, "M", "what?"))))%>% 
      mutate( Income_class=ifelse(is.na(POVERTY_LEVELR)==T | POVERTY_LEVELR=="M" , NA, ifelse(POVERTY_LEVELR == 1, "A", ifelse(
        POVERTY_LEVELR==2, "B", ifelse(POVERTY_LEVELR == 3, "C", ifelse(POVERTY_LEVELR == 4, "D", ifelse(POVERTY_LEVELR == 5, "E", ifelse(POVERTY_LEVELR == 6, "F", ifelse(
          POVERTY_LEVELR == 7, "G", ifelse(POVERTY_LEVELR == 8, "H", "what?"))))))))))
    
    
    return(dataset[,c(column_list, "Income_class","child_sex", "BMI_category","BMI","TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE", "COUNTS","WEIGHT")])
  }
  
  else if (length(grep(diagnostic[1],test))>0) { #variable present in the 2007 and 2003 datasets
    var2 <- c("S1Q01" , "PLANGUAGE", "EDUCATIONR","C11Q11", "C11Q11A", "C11Q11B","BMICLASS", "POVERTY_LEVELR","S2Q03R", "S2Q02R") #dataset-specific values not to be included in output
    dots <- lapply(c(column_list, var2), as.name)
    dataset <- dataset %>% group_by_(.dots=dots) %>% 
      summarize(WEIGHT=sum(WEIGHT_I, na.rm=TRUE), COUNTS = length(WEIGHT_I))  #compress based of selected features. Useful for increasing speed, but should not count that the final result of this function is compressed 
      #ceiling() ensures numbers<0 will be 1 and no decimals
    dataset <- dataset %>% mutate(LANGUAGE=ifelse(is.na(PLANGUAGE)==T | PLANGUAGE==6 | PLANGUAGE == 7, NA, ifelse(PLANGUAGE == 1, "ENG", ifelse(PLANGUAGE== 2,"NENG", "what?")))) %>%
      mutate(EDUCATION_LVL = ifelse( is.na(EDUCATIONR)==T | EDUCATIONR==6 | EDUCATIONR==7, NA, ifelse(EDUCATIONR==1, "highschool-", ifelse(EDUCATIONR==2, "highschool", ifelse(EDUCATIONR==3, "highschool+", "what?"))))) %>%
      mutate(BMI=ifelse(is.na(BMICLASS)==T, NA, (S2Q03R*0.45)/(S2Q02R*0.025)^2)) %>%
      mutate(TANF=ifelse(is.na(C11Q11)==T | C11Q11==6 | C11Q11 == 7 | C11Q11 == "P", NA, ifelse(C11Q11 == 0 | C11Q11 == "L", 0, 1))) %>%
      mutate(SNAP=ifelse(is.na(C11Q11A)==T | C11Q11A==6 | C11Q11A == 7 | C11Q11A == "P", NA, ifelse(C11Q11A == 0 | C11Q11A == "L", 0, 1))) %>%
      mutate(WIC=ifelse(is.na(C11Q11B)==T | C11Q11B==6 | C11Q11B == 7 | C11Q11B == "P", NA, ifelse(C11Q11B == 0 | C11Q11B == "L", 0, 1))) %>%
      mutate(BMI_category = ifelse( is.na(BMICLASS)==T | BMICLASS=="M" | BMICLASS=="N", NA, ifelse(BMICLASS==1, "U", ifelse(BMICLASS==2, "N", ifelse(BMICLASS==3, "O", ifelse(BMICLASS==4, "MO", "what?")))))) %>%
      mutate( child_sex=ifelse(is.na(S1Q01)==T | S1Q01==6 | S1Q01 == 7 , NA, ifelse(S1Q01 == 2, "F", ifelse(S1Q01==1, "M", "what?"))))%>% 
      mutate( Income_class=ifelse(is.na(POVERTY_LEVELR)==T | POVERTY_LEVELR=="M" , NA, ifelse(POVERTY_LEVELR == 1, "A", ifelse(
        POVERTY_LEVELR==2, "B", ifelse(POVERTY_LEVELR == 3, "C", ifelse(POVERTY_LEVELR == 4, "D", ifelse(POVERTY_LEVELR == 5, "E", ifelse(POVERTY_LEVELR == 6, "F", ifelse(
          POVERTY_LEVELR == 7, "G", ifelse(POVERTY_LEVELR == 8, "H", "what?"))))))))))
    
    return(dataset[,c(column_list,"Income_class","child_sex","BMI_category", "BMI", "TANF","SNAP","WIC","EDUCATION_LVL","LANGUAGE","COUNTS", "WEIGHT")])
  } 
  
  else { #in the case of unknown formats
    print("I cannot recognize the dataset you entered as a SLAITS survey table. Please input one of the 2011-2012, 2007, or 2003 data tables.")
  }
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data2freq <- function(dataset, var = as.name("COUNTS")){ #possible variables are "COUNTS" and "WEIGHT"{
  require("dplyr")
  column_list=colnames(dataset)
  dots <- lapply(column_list, as.name)
  dataset <- dataset %>% group_by_(.dots=dots) %>% summarize(TOT=ceiling(sum(var, na.rm=TRUE)/100))
  #ceiling() ensures numbers<0 will be 1 and no decimals
  dataset <- dataset[,c(column_list, "TOT")]
  
  library("splitstackshape")
  dataset <- expandRows(dataset, "TOT")
}
 
