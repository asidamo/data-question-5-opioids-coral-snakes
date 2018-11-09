
rm(list=ls())
memory.size(max=F)
setwd("C:\\Users\\fevty\\Desktop\\NSS\\DQ5")
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
deaths_by_state<-ggplot(data=overdose_df_SORTED, aes(x=State, y=Deaths)) +
  geom_bar(stat="identity")
# to plot the bar plot horizontally  coord_flip to plot verticall bars just run deaths_by_state
deaths_by_state + coord_flip()

# map showing increase in overdose related deaths by States
# https://www.cdc.gov/drugoverdose/data/prescribing.html


#  read prescribers informations

prescribers<-read_csv('prescriber-info.csv')

head(prescribers,5)

# change the shape of the

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)
head(rx_per_prescriber,5)

# select records qualified to make 10 prescriptions with dummy variable==1 and
# not qualified to makes 10 prescriptions 

frequent_prescribers<-rx_per_prescriber %>%
                          filter(Opioid.Prescriber==1)

non_frequent_prescribers<-rx_per_prescriber %>%
    filter(Opioid.Prescriber==0)

head(frequent_prescribers)

tail(non_frequent_prescribers)


# group by states the number of opioids prescribers



#match(opioids_drugs, frequent_prescribers, nomatch = NA_integer_, incomparables = NULL)

# vector for opioids drug names
opioids_drugs<-opioids_df$`Generic Name`



