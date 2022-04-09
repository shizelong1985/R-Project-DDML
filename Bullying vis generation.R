library(haven)
library(dplyr)
library(xtable)
library(tidyverse)
library(sjlabelled)


df <- read_dta("C:/Users/jbash/OneDrive - Carleton University/Classes/ECON5880/Project/Datasets/23-03-2022 for 1997 & 2015/Stata generation/19972015dataset.dta") 

df$age1997 <- as.factor(df$age1997)




# PROBABILITY OF BEING BULLIED: 

# CONDITIONAL ON AGE: 
avgBullByAge <- as.data.frame(  matrix( nrow=length(levels(df$age1997)), ncol=2, dimnames=list( NULL, c("age","meanBullying") )  ) )  
for(i in 1:nrow(avgBullByAge)){
  avgBullByAge[i,2] <-   mean(  filter( df, age1997==as.integer(levels(df$age1997))[i]  )$bullied1997  )
  avgBullByAge[i,1] <- as.integer(levels(df$age1997))[i]
}
# avgBullByAge$age
# figBullyingByAge <- ggplot(data=avgBullByAge, aes(x=meanBullying)) + geom_bar()
# figBullyingByAge
xtable(avgBullByAge)

#CONDITIONAL ON RACE: 

labelsRace <- get_labels(df$race)
avgBullByRace <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$race))), ncol=1, dimnames=list( labelsRace, c("meanBullying") )  ) )  

for(i in 1:nrow(avgBullByRace)){
  avgBullByRace[i,1] <-   mean(  filter(df, labelsRace[race]==labelsRace[i])$bullied1997  )
}




xtable(avgBullByRace)



#HARD TIMES: 

labelsHardTimes <- get_labels(df$hardTimes1997)
avgBullByHardTimes <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$hardTimes1997))), ncol=1, dimnames=list( labelsHardTimes, c("meanBullying") )  ) )  
df$hardTimes1997

for(i in 1:nrow(avgBullByHardTimes)){
  avgBullByHardTimes[i,1] <-   mean(  filter(  df, labelsHardTimes[hardTimes1997+1]==labelsHardTimes[i]  )$bullied1997  )
}

xtable(avgBullByHardTimes)


#MOMS AGE WHEN BORN: 
avgBullByAgeM <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$ageMBorn1997))), ncol=2, dimnames=list( NULL, c("ageM","meanBullying") )  ) )  
for(i in 1:nrow(avgBullByAgeM)){
  avgBullByAgeM[i,2] <-   mean(  filter( df, ageMBorn1997==as.integer(levels(as.factor(df$ageMBorn1997)))[i]  )$bullied1997  )
  avgBullByAgeM[i,1] <- as.integer(levels(as.factor(df$ageMBorn1997)))[i]
}
figAvgBullByAgeM <- ggplot(data=avgBullByAgeM, aes(x=ageM, y=meanBullying)) + geom_line() + theme_light() + xlab("Mother's Age at Birth") + ylab("% of children bullied in 1997")
figAvgBullByAgeM

#FAM NET WORTH: 
avgBullNet <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$famNetWorth1997))), ncol=2, dimnames=list( NULL, c("Familynetworth","meanBullying") )  ) )  
for(i in 1:nrow(avgBullNet)){
  avgBullNet[i,2] <-   mean(  filter( df, famNetWorth1997==as.integer(levels(as.factor(df$famNetWorth1997)))[i]  )$bullied1997  )
  avgBullNet[i,1] <- as.integer(levels(as.factor(df$famNetWorth1997)))[i]
}
figAvgBullNet <- ggplot(data=avgBullNet, aes(x=Familynetworth, y=meanBullying)) + geom_smooth() + theme_light() + xlab("Family's Networth") + ylab("% of children bullied in 1997")
figAvgBullNet

#PERCENT PEERS IN GANG: 
 

labelsGang <- get_labels(df$percentPeersGang1997)
avgBullGang <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$percentPeersGang1997))), ncol=1, dimnames=list( labelsGang, c("meanBullying") )  ) )  


for(i in 1:nrow(avgBullGang)){
  avgBullGang[i,1] <-   mean(  filter(  df, labelsGang[percentPeersGang1997]==labelsGang[i]  )$bullied1997  )
}

xtable(avgBullGang)



#PERCENT PEERS DOING DRUGS: 

labelsDrugs <- get_labels(df$percentPeersDrugs1997)
avgBullDrugs <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$percentPeersDrugs1997))), ncol=1, dimnames=list( labelsDrugs, c("meanBullying") )  ) )  


for(i in 1:nrow(avgBullDrugs)){
  avgBullDrugs[i,1] <-   mean(  filter(  df, labelsDrugs[percentPeersDrugs1997]==labelsDrugs[i]  )$bullied1997  )
}

xtable(avgBullDrugs)


#FAM ROUTINES INDEX: 
avgFamRoutines <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$indexFamRoutines1997))), ncol=2, dimnames=list( NULL, c("IndexFamRoutine","meanBullying") )  ) )  
for(i in 1:nrow(avgFamRoutines)){
  avgFamRoutines[i,2] <-   mean(  filter( df, indexFamRoutines1997==as.integer(levels(as.factor(df$indexFamRoutines1997)))[i]  )$bullied1997  )
  avgFamRoutines[i,1] <- as.integer(levels(as.factor(df$indexFamRoutines1997)))[i]
}
figFamRoutines <- ggplot(data=avgFamRoutines, aes(x=IndexFamRoutine, y=meanBullying)) + geom_smooth() + theme_light() + xlab("Family Routines Index") + ylab("% of children bullied in 1997")
figFamRoutines


#FAM RISK INDEX: 
avgFamRisk <- as.data.frame(  matrix( nrow=length(levels(as.factor(df$indexFamRisk1997))), ncol=2, dimnames=list( NULL, c("IndexFamRisk","meanBullying") )  ) )  
for(i in 1:nrow(avgFamRisk)){
  avgFamRisk[i,2] <-   mean(  filter( df, indexFamRisk1997==as.integer(levels(as.factor(df$indexFamRisk1997)))[i]  )$bullied1997  )
  avgFamRisk[i,1] <- as.integer(levels(as.factor(df$indexFamRisk1997)))[i]
}
figFamRisk <- ggplot(data=avgFamRisk, aes(x=IndexFamRisk, y=meanBullying)) + geom_smooth() + theme_light() + xlab("Family Risk Index") + ylab("% of children bullied in 1997")
figFamRisk










