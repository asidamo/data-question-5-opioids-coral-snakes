
rm(list=ls())
memory.size(max=F)
setwd("C:\\Users\\fevty\\Desktop\\NSS\\data-question-5-opioids-coral-snakes")
library(tidyverse)

# questions
# opiodis data 
# overdose data state level informations
# prescribers data includes both opiodis and other drugs.
# 1. explore the data, plot by states
# 2. states with highest number of overdoses and opioids prescribers
# 3. plot box, historgrams or any appropriate plot
# 4. pareto effect of opiodis
# 5. build model to to predict number of opiodis fuetal death
# 6. find other data that may affect health outcomes
# 7. variability in numberof opiodis and mean , median
# prescribers data
# NPI is key indicator in your data
# find number of prescriptions they provided.


opioids_df<-read_csv('opioids.csv')

tail(opioids_df)

overdose_df<-read_csv('overdoses.csv')
head(overdose_df,10)


filter(overdose_df,Deaths>2000)

overdose_df %>%
  filter(State=="Tennessee")%>%
  View()


# Explore overdoses datasets

# sort overdoses data frame by number of deaths


head(overdose_df,2)

overdose_df_SORTED<-overdose_df[order(overdose_df$Deaths,decreasing=TRUE),]


head(overdose_df_SORTED,5)

# bar plot number of overdoses related deaths by states
deaths_by_state<-ggplot(data=overdose_df_SORTED, aes(x=State, y=Deaths,fill='red')) +
  geom_bar(stat="identity")
# to plot the bar plot horizontally  coord_flip to plot verticall bars just run deaths_by_state
deaths_by_state + coord_flip()

# map showing increase in overdose related deaths by States
# https://www.cdc.gov/drugoverdose/data/prescribing.html
# https://www.cdc.gov/media/releases/2018/p0329-drug-overdose-deaths.html 

# data for diabetic diseases by states
# https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html

diabetes<- read_csv('DiabetesAtlasData.csv',skip =2)

str(diabetes)
diabetes_by_state<-ggplot(data=diabetes, aes(x=State, y=Percentage,fill='red')) + geom_bar(stat="identity")

diabetes_by_state + coord_flip()

tail(diabetes)


#  read prescribers informations

prescribers<-read_csv('prescriber-info.csv')

head(prescribers,5)

# change the shape of the

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)
head(rx_per_prescriber,5)


matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROMORPHONE.HCL", "METHADONE.HCL", "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

opioids_prescribed <- rx_per_prescriber$rx %in% matches
rx_opioids <- rx_per_prescriber[opioids_prescribed == TRUE,]

head(rx_opioids)

# question # 2 test pareto effect (20% of top prescribers are prescribing 80% of prescriptions)

# let's sort rx_opiodis dataset by countrx (count of presscriptions)
rx_opioids<-rx_opioids[order(rx_opioids$countrx,decreasing=TRUE),]
head(rx_opioids)

# let's find total opiodis prescription in by using 
tot_prescription<-sum(rx_opioids$countrx,na.rm = TRUE)

print(tot_prescription)
# calculate opioids precentage per drug per NPI 
rx_opioids$pct_prescribed<- (rx_opioids$countrx/tot_prescription)*100

head(rx_opioids)

# take only NPI, countrx and pct_prescribed columns

rx_opioids_per_NPI<-data.frame(rx_opioids$NPI,rx_opioids$countrx,rx_opioids$pct_prescribed)

head(rx_opioids_per_NPI)

# group by NPI 
prescription_per_NPI<-rx_opioids_per_NPI%>% 
  group_by(rx_opioids.NPI)%>%
summarise(rx_opioids.countrx=sum(rx_opioids.countrx),rx_opioids.pct_prescribed=sum(rx_opioids.pct_prescribed))

head(prescription_per_NPI)


sum(prescription_per_NPI$pct_prescribed)
# rename the columns as "NPI","countrx","pct_prescribed"

colnames(prescription_per_NPI)<-c("NPI","countrx","pct_prescribed")

# sort by descending order by percentage of opioids prescribed per NPI

prescription_per_NPI<-prescription_per_NPI[order(prescription_per_NPI$pct_prescribed,decreasing=TRUE),]

# create cummulative sum of percentages prescribed by prescribers
head(prescription_per_NPI)
prescription_per_NPI<-prescription_per_NPI%>%
  mutate(cummulative_pct=cumsum(pct_prescribed))

head(prescription_per_NPI)


# let us take commulative of the percentages prescribed for NPIs

# select records qualified to make 10 prescriptions with dummy variable==1 and
# not qualified to makes 10 prescriptions 

# frequent_prescribers<-rx_per_prescriber %>%
 #                         filter(Opioid.Prescriber==1)

#non_frequent_prescribers<-rx_per_prescriber %>%
#    filter(Opioid.Prescriber==0)

#head(frequent_prescribers)

#tail(non_frequent_prescribers)


# group by states the number of opioids prescribers



#match(opioids_drugs, frequent_prescribers, nomatch = NA_integer_, incomparables = NULL)

# vector for opioids drug names
#opioids_drugs<-opioids_df$`Generic Name`

#opioids_list<-unique(opioids_drugs)
#opioids_list_unq<-list(opioids_list)

#prescribers_list<-unique(rx_per_prescriber$rx)
#prescribers_list_unq<-list(prescribers_list)


#write.csv(opioids_list_unq,"opioids_list.csv")
#write.csv(prescribers_list_unq,"prescribers_list.csv")


