source("/Users/NikiAthanasiadou/RiskEcon/Project_Health/SLAITS_analysis/SLAITS_functions.R")

#import raw tables, select variables and fix values (beautify())
n2003 <- beautify(read.csv("~/Datasets/SLAITS/nschpuf3.csv"))
n2007 <- beautify(read.csv("~/Datasets/SLAITS/nsch_2007_puf.csv"))
n2011 <- beautify(read.csv("~/Datasets/SLAITS/nsch_2011_2012_puf.csv"))


library(dplyr)
library(tidyr)
library(ggplot2)

############~~~~~~~~~~~~~~~~~~~     WIC vs SNAP : the test     ~~~~~~~~~~~~~~~~~~~############
#1.   Combine surveys approach (fixes problem of low sampling numbers per stratum (sisel) per group (snap/wic))

############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
compress <- function(dataset){
  dataset <- ungroup(dataset)
  nm <- colnames(dataset)[!(colnames(dataset) %in% "COUNTS")]
  dots <- lapply(nm, as.name)
  dataset <- dataset %>% group_by_(.dots=dots) %>% 
    summarize(COUNTS_ = sum(COUNTS, na.rm=T)) 
  dataset <- ungroup(dataset)
}
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
stratify <- function(dataset, group){
  if (group == "A"){
    grouped <- filter(dataset, WIC == 0 & SNAP == 0) }
  if (group == "B"){
    grouped <- filter(dataset, WIC == 1 & SNAP == 1) }
  if (group == "C") {
    grouped <- filter(dataset, WIC == 1 & SNAP == 0 ) }
  if (group == "D") {
    grouped <- filter(dataset, WIC == 0  & SNAP == 1) }
  
  columns <-  c("STATE","Income_class", "child_sex",  "EDUCATION_LVL", "LANGUAGE", "BMI_category", "COUNTS")
  A_<- compress(grouped[,columns])
  dots <- lapply(c("STATE","Income_class", "child_sex",  "EDUCATION_LVL", "LANGUAGE"), as.name)
  A_ <- A_  %>% 
    mutate(U = ifelse(BMI_category=="U" & is.na(BMI_category)==FALSE,COUNTS_ , 0),
           N = ifelse(BMI_category=="N" & is.na(BMI_category)==FALSE ,COUNTS_ , 0),
           O = ifelse(BMI_category=="O" & is.na(BMI_category)==FALSE,COUNTS_ , 0),
           MO = ifelse(BMI_category=="MO" & is.na(BMI_category)==FALSE,COUNTS_ , 0),
           MISSING = ifelse(is.na(BMI_category)==TRUE ,COUNTS_ , 0)) %>%
    select(STATE, Income_class, child_sex, EDUCATION_LVL, LANGUAGE, U, N, O, MO, MISSING) %>%
    group_by_(.dots=dots) %>%
    mutate(U = sum(U, na.rm=T),
           N = sum(N, na.rm=T),
           O = sum(O, na.rm=T),
           MO = sum(MO, na.rm=T),
           MIS = sum(MISSING, na.rm=T)) %>%
    ungroup() %>%
    group_by_(.dots = lapply(c("STATE","Income_class", "child_sex",  "EDUCATION_LVL", "LANGUAGE", "U", "N", "O", "MO","MISSING"), as.name)) %>%
    distinct() %>%
    ungroup()
  return(A_)
    }  
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
normalize <- function(stratified_table){
  tab <- stratified_table %>%
    mutate(SUM = U + N + O + MO + MISSING) %>%
    mutate(adj.U = U/SUM*mean(SUM),
           adj.N = N/SUM*mean(SUM),
           adj.O = O/SUM*mean(SUM),
           adj.MO = MO/SUM*mean(SUM),
           adj.MISSING = MISSING/SUM*mean(SUM)) %>%
    select(STATE,  Income_class, child_sex, EDUCATION_LVL, LANGUAGE, adj.U, adj.N, adj.O, adj.MO, adj.MISSING )
  return(tab)
    }
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
contigency <- function(dataA, dataB, dataC, dataD, freq = TRUE){
  dataA <- dataA[c("adj.U", "adj.N", "adj.O", "adj.MO", "adj.MISSING")] %>% 
    summarize_each( funs(sum)) %>%
    summarize_each(funs(as.integer))
  dataB <- dataB[c("adj.U", "adj.N", "adj.O", "adj.MO", "adj.MISSING")] %>% 
    summarize_each( funs(sum)) %>%
    summarize_each(funs(as.integer))
  dataC <- dataC[c("adj.U", "adj.N", "adj.O", "adj.MO", "adj.MISSING")] %>% 
    summarize_each( funs(sum)) %>%
    summarize_each(funs(as.integer))
  dataD <- dataD[c("adj.U", "adj.N", "adj.O", "adj.MO", "adj.MISSING")] %>% 
    summarize_each( funs(sum)) %>%
    summarize_each(funs(as.integer))
  TAB <- as.matrix(rbind(dataA, dataB, dataC, dataD))
  rownames(TAB)<- c("A","B","C","D")
  return(TAB)
}
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
contigency_pct <- function(contigency_tab){
  tab <- tbl_df(contigency_tab[,1:4])
  tab <- tab %>%
    mutate(pct.adj.U = adj.U*100/(adj.U + adj.N + adj.O + adj.MO),
           pct.adj.N = adj.N*100/(adj.U + adj.N + adj.O + adj.MO), 
           pct.adj.O = adj.O*100/(adj.U + adj.N + adj.O + adj.MO),
           pct.adj.MO = adj.MO*100/(adj.U + adj.N + adj.O + adj.MO))
  tab<- as.matrix(tab)
  rownames(tab)<- c("A","B","C","D")
  return(tab[,5:8])
}
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############

c2011 <- contigency(normalize(stratify(n2011,"A")), normalize(stratify(n2011,"B")), normalize(stratify(n2011,"C")), normalize(stratify(n2011,"D")))
c2007 <- contigency(normalize(stratify(n2007,"A")), normalize(stratify(n2007,"B")), normalize(stratify(n2007,"C")), normalize(stratify(n2007,"D")))
c2003 <- contigency(normalize(stratify(n2003,"A")), normalize(stratify(n2003,"B")), normalize(stratify(n2003,"C")), normalize(stratify(n2003,"D")))

c2011pct <-contigency_pct(c2011)
c2007pct <-contigency_pct(c2007)
c2003pct <-contigency_pct(c2003)

#Convert the percent contigency tables to mean and sd of each measurement
t2011<-gather(tbl_df(c2011pct), Outcome, percent_total)
t2011 <- as.data.frame(t2011)
t2011$category <-  rep(c("A","B","C","D"),4)
t2011<-t2011[,c(3,1,2)]

t2007<-gather(tbl_df(c2007pct), Outcome, percent_total)
t2007 <- as.data.frame(t2007)
t2007$category <-  rep(c("A","B","C","D"),4)
t2007<-t2007[,c(3,1,2)]

t2003<-gather(tbl_df(c2003pct), Outcome, percent_total)
t2003 <- as.data.frame(t2003)
t2003$category <-  rep(c("A","B","C","D"),4)
t2003<-t2003[,c(3,1,2)]

master <- cbind(t2011[,1:2],t2011 = t2011$percent_total, t2007 = t2007$percent_total, t2003 = t2003$percent_total)
master$pct_category <- apply(master[,3:5], FUN = mean, MARGIN=1)
master$sd <- apply(master[,3:5], FUN = sd, MARGIN=1)
master_simplified <-master[,c(1,2,6,7)]

#Create ggplot
ggplot(master_simplified, aes(x=Outcome, y=pct_category, fill=category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=pct_category-(sd/sqrt(3)), ymax=pct_category+(sd/sqrt(3))),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

#Pair-wise student's t-test
#category U
temp<-master[1:4,c(3,4,5)]
print("A,B")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[2,1:3]), paired=TRUE)
print("B,C")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("C,D")
t.test(as.numeric(temp[3,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("A,C")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("A,D")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D (no 2011")
t.test(as.numeric(temp[2,2:3]),as.numeric(temp[4,2:3]), paired=TRUE)

#category N
temp<-master[5:8,c(3,4,5)]
print("A,B")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[2,1:3]), paired=TRUE)
print("B,C")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("C,D")
t.test(as.numeric(temp[3,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("A,C")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("A,D")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D (no 2011")
t.test(as.numeric(temp[2,2:3]),as.numeric(temp[4,2:3]), paired=TRUE)

#category O
temp<-master[9:12,c(3,4,5)]
print("A,B")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[2,1:3]), paired=TRUE)
print("B,C")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("C,D")
t.test(as.numeric(temp[3,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("A,C")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("A,D")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D (no 2011")
t.test(as.numeric(temp[2,2:3]),as.numeric(temp[4,2:3]), paired=TRUE)

#category MO
temp<-master[13:16,c(3,4,5)]
print("A,B")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[2,1:3]), paired=TRUE)
print("B,C")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("C,D")
t.test(as.numeric(temp[3,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("A,C")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[3,1:3]), paired=TRUE)
print("A,D")
t.test(as.numeric(temp[1,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D")
t.test(as.numeric(temp[2,1:3]),as.numeric(temp[4,1:3]), paired=TRUE)
print("B,D (no 2011")
t.test(as.numeric(temp[2,2:3]),as.numeric(temp[4,2:3]), paired=TRUE)
