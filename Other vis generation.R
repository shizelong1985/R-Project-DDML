library(haven)
library(dplyr)
library(xtable)
library(tidyverse)
library(sjlabelled)
library(fastDummies)
library(matrixStats)
rm(list=ls())

df <- read_dta("C:/Users/jbash/OneDrive - Carleton University/Classes/ECON5880/Project/Datasets/25-03-2022 for 1997 & 2015/25-03-2022-data-2015-1997/19972015dataset.dta") 


#AVERAGE CHILDREN PER COLLEGE CATEGORY 

labelsCollege <- rev(get_labels(df$college2015))

avgKidsCollege <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$college2015))), ncol=2, dimnames=list( labelsCollege, c("meanChildren", "meanChildrenMarried") )  ) )  


for(i in 1:nrow(avgKidsCollege)){
  avgKidsCollege[i,1] <-   mean(  filter(  df, labelsCollege[college2015+1]==labelsCollege[i]  )$fertility2015  )
  avgKidsCollege[i,2] <-   mean(  filter(  df, (labelsCollege[college2015+1]==labelsCollege[i]) & marStat2015==1  )$fertility2015  )
}

xtable(avgKidsCollege)

df$marStat2015


# THE AVERAGE RESPONDENT IN SAMPLE:

rows <- c( "Number of children",NA,  "Went to college",NA, "Bullied",NA,  "Age (2015)",NA, "Gross family income (2015)" ,NA,
           "Number of siblings",NA, "Married",NA, "Dad's Highest Grade",NA, "Mom's Highest Grade",NA, "Black",NA, "Hispanic",NA, "Mixed Race",NA, "Other" ,
           NA, "Catholic" ,NA, "Baptist",NA, "Other Christian",NA, "Other/No religion",NA)

temp <- df[, c("fertility2015", "college2015",  "bullied1997", "age2015", "grossFamInc2015", "numSiblings", "marStat2015", "dadHighestGrade",
               "momHighestGrade", "race", "religion")]

avgRespInSample <- as.data.frame(  matrix( nrow=length(rows), ncol=2, dimnames=list( rows, c("Entire Sample", "Married Women") )  ) )

index=1
for(i in 1:9){
  avgRespInSample[index,1] <- mean(temp[[i]])
  avgRespInSample[index+1,1] <- sd(temp[[i]])
  avgRespInSample[index,2] <- mean(  filter(temp, marStat2015==1)[[i]]   )
  avgRespInSample[index+1,2] <- sd(filter(temp, marStat2015==1)[[i]])
  index=index+2
}


#RACE:
#ALL: 
raceDF <- dummy_cols(df$race, remove_first_dummy = FALSE)[, -1]
colnames(raceDF) <- get_labels(df$race)
raceMeans <- colMeans(raceDF)
raceSds <- colSds(as.matrix(raceDF))

#MARRIED: 
raceDF2 <- dummy_cols(filter(df, marStat2015==1)$race, remove_first_dummy = FALSE)[, -1]
colnames(raceDF2) <- get_labels(df$race)
raceMeans2 <- colMeans(raceDF2)
raceSds2 <- colSds(as.matrix(raceDF2))


index=19
for(i in 1:length(raceMeans)){
  avgRespInSample[index , 1] <- raceMeans[i]
  avgRespInSample[1+index ,1] <- raceSds[i]
  avgRespInSample[index , 2] <- raceMeans2[i]
  avgRespInSample[1+index ,2] <- raceSds2[i]
  index=index+2
}
rm(raceDF,raceMeans,raceSds )

#RELIGION: 
relDF <- dummy_cols(df$religion, remove_first_dummy = FALSE)[, -1]
colnames(relDF) <- get_labels(df$religion)
relMeans <- colMeans(relDF)
relSds <- colSds(as.matrix(relDF))

#MARRIED: 
relDF2 <- dummy_cols(filter(df, marStat2015==1)$religion, remove_first_dummy = FALSE)[, -1]
colnames(relDF2) <- get_labels(df$religion)
relMeans2 <- colMeans(relDF2)
relSds2 <- colSds(as.matrix(relDF2))

index=27
for(i in 1:length(relMeans)){
  avgRespInSample[index , 1] <- relMeans[i]
  avgRespInSample[1+index ,1] <- relSds[i]
  avgRespInSample[index , 2] <- relMeans2[i]
  avgRespInSample[1+index ,2] <- relSds2[i]
  index=index+2
}
rm(relDF,relMeans,relSds )


xtable(avgRespInSample)






## PERCENT COLLEGE PER BULLIED CATEGORY: 

labelsBullied <- get_labels(df$bullied1997)
avgCollege <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$bullied1997))), ncol=1, dimnames=list( labelsBullied, c("meanCollege") )  ) )  


for(i in 1:nrow(avgCollege)){
  avgCollege[i,1] <-   mean(  filter(  df, labelsBullied[bullied1997+1]==labelsBullied[i]  )$college2015  )
}

xtable(avgCollege)


# MEAN NUMBER OF CHILDREN, CONDITIONAL ON BEING BULLIED

labelsBullied <- get_labels(df$bullied1997)
avgChildrenBull <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$bullied1997))), ncol=1, dimnames=list( labelsBullied, c("meanChildren") )  ) )  


for(i in 1:nrow(avgChildrenBull)){
  avgChildrenBull[i,1] <-   mean(  filter(  df, labelsBullied[bullied1997+1]==labelsBullied[i]  )$fertility2015  )
}

xtable(avgChildrenBull)






#MEAN NUMBER OF CHILDREN, CONDITIONAL ON BULLIED CATEGORY AND COLLEGE ATTENDANCE 

labelsBullied
labelsCollege
labelsConcat<- rep(NA,4)

index=1
for(i in 1:2){
  for(j in 1:2){
    labelsConcat[index]<- paste(labelsBullied[i], "and", labelsCollege[j])
    index=index+1
  }
}  


avgChildrenBullColl <- as.data.frame(  matrix( nrow=4, ncol=1, dimnames=list( labelsConcat, c("meanChildren") )  ) )

#Fill it: 
avgChildrenBullColl[1,1] <- mean(filter(df, bullied1997==0 & college2015==0)$fertility2015)
avgChildrenBullColl[2,1] <- mean(filter(df, bullied1997==0 & college2015==1)$fertility2015)
avgChildrenBullColl[3,1] <- mean(filter(df, bullied1997==1 & college2015==0)$fertility2015)
avgChildrenBullColl[4,1] <- mean(filter(df, bullied1997==1 & college2015==1)$fertility2015)

xtable(avgChildrenBullColl)



dim(filter(df, marStat2015==1))



df$marStat2015
































## PRESENTATION: 
df2 <- as.data.frame(df)

###########################################################################################################################
#Gen conditional expectation of college attendance, conditional on number of kids. 
CondExpCollegeOnFert <- data.frame(matrix(ncol=1, nrow=7, dimnames=list(NULL, "Exp College")))

for(i in 0:6){
  temp <- filter(df2, fertility2015==i)  
  CondExpCollegeOnFert[i+1,] <- mean(temp$college2015)
}

#Gen percentage of women with each no. of kids. 
df3 <- df2 %>% 
  group_by(fertility2015) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

#Kids on its own: 
ChildGraph <- ggplot(data=df3, aes(x=fertility2015, y=perc)) + geom_bar(stat="identity")  + xlab("No. Of Children") + 
            scale_x_discrete(limits=0:6)  + theme_light()+  ylab("Percentage")

#Kids and college together: 
df3$CexpFertility <- unlist(CondExpCollegeOnFert)

ChildGraph <- ggplot(data=df3) + geom_bar(aes(x=fertility2015, y=perc), stat="identity")  + xlab("No. Of Children") +
  ylab("Percentage") +
  scale_x_discrete(limits=0:6)  + theme_light() + geom_line(aes(x=fertility2015, y=CexpFertility), stat="identity", color="red", size=1) +
  geom_point(aes(x=fertility2015, y=CexpFertility), stat="identity", color="black", size=2)
ChildGraph              

colnames(df3)

###########################################################################################################################
CondExpFertOnCollege <- data.frame(matrix(ncol=1, nrow=2, dimnames=list(0:1, "Exp Fertility")))

for(i in 0:1){
  temp <- filter(df2, college2015==i)  
  CondExpFertOnCollege[i+1,] <- mean(temp$fertility2015)
}
CondExpFertOnCollege

coll1 <- mean(df2$college2015)
coll0 <- 1- coll1
collperc <- c(coll0, coll1)

CondExpFertOnCollege$collperc <-collperc


CollGraph <- ggplot(data=CondExpFertOnCollege, aes(x=0:1, y=Exp.Fertility  )) + geom_bar(stat="identity")  + xlab("College Attendance Status") +
  ylab("Average Number of Children")  + scale_x_discrete(limits=c(0,1), labels=c("No College", "College"))  + theme_light() +
  geom_text(label=c("%52.3 of sample", "%47.7 of sample"), vjust=5, color="yellow")
CollGraph
CondExpFertOnCollege








