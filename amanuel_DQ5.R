
rm(list=ls())
memory.size(max=F)
setwd("C:\\Users\\fevty\\Desktop\\NSS\\data-question-5-opioids-coral-snakes")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)
library(gdata)


# questions
# opiodis data 
# overdose data state level informations
# prescribers data includes both opiodis and other drugs.
# 1. explore the data, plot by states
# 2. states with highest number of overdoses and opioids prescribers
# 3. plot box, historgrams or any appropriate plot
# 4. pareto effect of opiodis
# 5. build model to to predict number of opiodis fatal death
# 6. find other data that may affect health outcomes
# 7. variability in numberof opiodis and mean , median
# prescribers data
# NPI is key indicator in your data
# find number of prescriptions they provided.


opioids_df<-read_csv('opioids.csv')

tail(opioids_df)

overdose_df<-read_csv('overdoses.csv')
head(overdose_df,10)



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
# keep only qualified prescribers and countrx column not missing

rx_opioids<- filter(rx_opioids,Opioid.Prescriber==1,countrx!='na')
head(rx_opioids)
rx_opioids$NPI
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

# rename the columns as "NPI","countrx","pct_prescribed"
colnames(rx_opioids_per_NPI)<-c("NPI","countrx","pct_prescribed")

# uncomment the next line to compute for actual prescribers only.
#rx_opioids_per_NPI<-  filter(rx_opioids_per_NPI,countrx!=0)


# group by NPI 
prescription_per_NPI<-rx_opioids_per_NPI%>% 
group_by(NPI)%>%
summarise(countrx=sum(countrx),pct_prescribed=sum(pct_prescribed))

head(prescription_per_NPI)


sum(prescription_per_NPI$pct_prescribed)


# sort by descending order by percentage of opioids prescribed per NPI

prescription_per_NPI<-prescription_per_NPI[order(prescription_per_NPI$pct_prescribed,decreasing=TRUE),]

# create cummulative sum of percentages prescribed by prescribers
head(prescription_per_NPI)
prescription_per_NPI<-prescription_per_NPI%>%
  mutate(cummulative_pct=cumsum(pct_prescribed))

head(prescription_per_NPI)
# select 80% of prescriptions
prescription_80_pct<-prescription_per_NPI[prescription_per_NPI$cummulative_pct<=80,]

# calculate count of top 80 percent prescribers
count_top_80_pct<-nrow(prescription_80_pct)
count_top_80_pct

# calculate count of bottom 20 percent prescribers
bottom_20_pct<-prescription_per_NPI[prescription_per_NPI$cummulative_pct>80,]
count_bottom_20_pct<-nrow(bottom_20_pct)
count_bottom_20_pct

pareto_effect<-count_top_80_pct*100/(count_bottom_20_pct+count_top_80_pct)

# Only 20.26% of the qualified prescribers have prescribed 80% of Opioids prescriptions
# when only actuall prescribers are considered 30.75% of prescribers prescribed 80% of the Opioids
pareto_effect

# plot histogram 
ggplot(
  data = prescription_per_NPI,
  aes(x = cummulative_pct,fill='orange2')
) +
  geom_histogram(binwidth =20)

#prescription_per_NPI<-prescription_per_NPI %>%
 # mutate(cum_cdf=ecdf(pct_prescribed))

head(prescription_per_NPI)
plot(prescription_per_NPI$cummulative_pct, xlab = 'count_of_prescriptions', ylab = 'cummulative_percent', main = 'Empirical Cumluative Distribution\nOpioids Prescriptions')



# Question # 3 module for prescription practices 

# data for diabetic diseases by states
# https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html

diabetes<- read_csv('DiabetesAtlasData.csv',skip =2)

str(diabetes)
diabetes_by_state<-ggplot(data=diabetes, aes(x=State, y=Percentage,fill='red')) + geom_bar(stat="identity")

diabetes_by_state + coord_flip()

tail(diabetes)

# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk 
race<-read.csv('PEP_2014_PEPSR6H_with_ann.csv')

tail(race,15)

# https://www.census.gov/library/publications/2015/demo/p60-253.html 

insurance<-read.csv('hic04_acs.xls')


# https://www.kff.org/other/state-indicator/health-spending-per-capita/?currentTimeframe=0&selectedRows=%7B%22states%22:%7B%22all%22:%7B%7D%7D,%22wrapups%22:%7B%22united-states%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

percapita_HE<-read_csv('hic04_acs.csv')

head(percapita_HE)