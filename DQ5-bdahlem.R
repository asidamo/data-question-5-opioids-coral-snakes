library(tidyverse)
library(ggplot2)
library(magrittr)
library(scales)
library(RColorBrewer)
library(reshape2)

overdoses <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/overdoses.csv')

prescribers <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/prescriber-info.csv')

opioids <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/opioids.csv')

prescribers_2016 <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/prescriber-info_CY16.csv')

deaths_2016 <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/deaths-by-state-US-2016.csv')

prescribers_2016

colnames(overdoses)[colnames(overdoses) == 'State'] <- 'Full_State_Name'

colnames(overdoses)[colnames(overdoses) == 'Abbrev'] <- 'State'

colnames(deaths_2016)[colnames(deaths_2016) == 'number'] <- 'Deaths_2016'

colnames(deaths_2016)[colnames(deaths_2016) == 'State'] <- 'State_2016'

deaths_2016

overdoses

deaths_2016 %<>% 
  mutate(Population = round(deaths_2016$Deaths_2016/(deaths_2016$rate/100000)))

colnames(deaths_2016)[colnames(deaths_2016) == 'Population'] <- 'Population_2016'

deaths_2016

rx_per_2016 <- gather(prescribers_2016, rx, countrx, ACETAMINOPHEN.CODEINE:ZOLPIDEM.TARTRATE)

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 1)
non_prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 0)

prescriber_2016 <- rx_per_2016 %>% 
                    filter(Opioid.Prescriber == 1)

non_prescriber_2016 <- rx_per_2016 %>% 
                        filter(Opioid.Prescriber == 0)

drops <- c("PR", "ZZ", "AA", "AE", "GU", "VI", "DC", "AP", NA)

drop_states <- prescriber$State %in% drops

drop_deaths_2016 <- deaths_2016$State_2016 %in% drops

drop_deaths_2016

drop_states

drop_states_2016 <- prescriber_2016$State %in% drops

drop_states_2016

non_drop_states <- non_prescriber$State %in% drops

non_drop_states

non_drop_states_2016 <- non_prescriber_2016$State %in% drops

non_drop_states_2016

prescriber <- prescriber[drop_states == FALSE, ]

deaths_2016 <- deaths_2016[drop_deaths_2016 == FALSE, ]

deaths_2016

prescriber_2016 <- prescriber_2016[drop_states_2016 == FALSE, ]

prescriber_2016

non_prescriber <- non_prescriber[non_drop_states == FALSE, ]

non_prescriber_2016 <- non_prescriber_2016[non_drop_states_2016 == FALSE, ]

unique(non_prescriber_2016$State)

prescriber_by_rx <- prescriber %>% 
                    group_by(rx)

matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROCODONE.ACETAMINOPHEN", "HYDROMORPHONE.HCL", "METHADONE.HCL",
             "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

op_match <- prescriber$rx %in% matches

op_match_2016 <- prescriber_2016$rx %in% matches

non_op_match <- non_prescriber$rx %in% matches

non_op_match_2016 <- non_prescriber_2016$rx %in% matches

op_match <- as.data.frame(op_match)

op_match

op_match <- prescriber[op_match == TRUE, ]

op_match_2016 <- prescriber[op_match == TRUE, ]

op_mismatch <- prescriber[op_match != TRUE, ]

op_mismatch_2016 <- prescriber_2016[op_match != TRUE, ]

head(op_match)

non_op_match <- non_prescriber[non_op_match == TRUE, ]

non_op_match_2016 <- non_prescriber_2016[non_op_match_2016 == TRUE, ]

head(non_op_match)

summary(op_match)

summary(non_op_match)

op_match_by_state <- aggregate(data.frame(count = op_match$State), list(value = op_match$State), length)

op_match_by_state_2016 <- aggregate(data.frame(count = op_match_2016$State), list(value = op_match_2016$State), length)

op_match_by_rx <- aggregate(data.frame(count = op_match$rx), list(value = op_match$rx), length)

head(op_match_by_state)

head(op_match_by_state_2016)

op_match_by_state <- as.data.frame(op_match_by_state)

op_match_by_state_2016 <- as.data.frame(op_match_by_state_2016)

str(op_match_by_state)

str(op_match_by_state_2016)

op_match_by_state$count <- as.numeric(op_match_by_state$count)

op_match_by_state_2016$count <- as.numeric(op_match_by_state_2016$count)

str(op_match_by_state)

str(op_match_by_state_2016)

count_by_state <- op_match_by_state %>% 
  group_by(value) %>% 
  summarise(total = sum(count))

count_by_state_2016 <- op_match_by_state_2016 %>% 
  group_by(value) %>% 
  summarise(total = sum(count))

count_by_state_2016

zero_value <- c(0)

drop_null_rx <- prescriber$countrx %in% zero_value

drop_null_rx_2016 <- prescriber_2016$countrx %in% zero_value

drop_null_rx

nondrop_null_rx <- non_prescriber$countrx %in% zero_value

nondrop_null_rx_2016 <- non_prescriber_2016$countrx %in% zero_value

nondrop_null_rx

nondrop_null_rx_2016

prescriber_no_null <- prescriber[drop_null_rx == FALSE, ]

prescriber_no_null_2016 <- prescriber_2016[drop_null_rx_2016 == FALSE, ]

non_prescriber_no_null <- non_prescriber[nondrop_null_rx == FALSE, ]

non_prescriber_no_null_2016 <- non_prescriber_2016[nondrop_null_rx_2016 == FALSE, ]

head(non_prescriber_no_null)

head(prescriber_no_null_2016)

summary(non_prescriber_no_null)

head(prescriber_no_null)

summary(prescriber_no_null)

op_match_nonull <- prescriber_no_null$rx %in% matches

op_match_nonull_2016 <- prescriber_no_null_2016$rx %in% matches

op_match_nonull <- prescriber_no_null[op_match_nonull == TRUE, ]

op_match_nonull_2016 <- prescriber_no_null_2016[op_match_nonull_2016 == TRUE, ]

non_op_match_nonull <- non_prescriber_no_null$rx %in% matches

non_op_match_nonull_2016 <- non_prescriber_no_null_2016$rx %in% matches

non_op_match_nonull <- non_prescriber_no_null[non_op_match_nonull == TRUE, ]

non_op_match_nonull_2016 <- non_prescriber_no_null_2016[non_op_match_nonull_2016 == TRUE, ]

#The dataset listed below is cleaned to include ONLY valid US states, non-null values for 'countrx', and prescription drugs listed as opioids

op_match_nonull

op_match_nonull_2016

non_op_match_nonull

non_op_match_nonull_2016

NPI_count <- op_match_nonull %>% 
  count(op_match_nonull$NPI)

NPI_count_2016 <- op_match_nonull_2016 %>% 
  count(op_match_nonull_2016$npi)

nrow(NPI_count) 

nrow(NPI_count_2016)

sum(op_match_nonull$countrx) / nrow(NPI_count)

sum(op_match_nonull_2016$countrx) / nrow(NPI_count_2016)

head(overdoses)

overdoses = mutate(overdoses, Ratio = round(Deaths/Population * 100000, digits = 2))

count_of_opioids <- op_match_nonull %>% 
  group_by(rx) %>% 
  summarise(total = sum(countrx))

count_of_opioids

count_of_opioids_2016 <- op_match_nonull_2016 %>% 
  group_by(rx) %>% 
  summarise(total = sum(countrx))

count_of_opioids_2016

op_count_by_state <- op_match_nonull %>% 
  group_by(State, rx) %>% 
  summarise(total = sum(countrx))

op_count_by_state_2016 <- op_match_nonull_2016 %>% 
  group_by(State, rx) %>% 
  summarise(total = sum(countrx))

op_count_by_state = mutate(op_count_by_state, Ratio = total/sum(total))

op_count_by_state_2016 = mutate(op_count_by_state_2016, Ratio = total/sum(total))

op_count_by_state_2016

#ggplot(op_count_by_state, aes(State)) + geom_bar(aes(fill = rx), position = position_stack(reverse = FALSE)) + coord_flip()

prescription_plot_1 <- ggplot() + 
  geom_bar(aes(y = (Ratio * 100), x = State, fill = rx), data = op_count_by_state, stat="identity") + 
  coord_flip() +
  labs(x = "State", y = "Ratio", fill = 'Opioid Prescribed') + 
  theme(axis.text.y = element_text(vjust = 1)) + 
  ggtitle('Opioid Prescriptions by State')

prescription_plot_2016 <- ggplot() + 
  geom_bar(aes(y = (Ratio * 100), x = State, fill = rx), data = op_count_by_state_2016, stat="identity") + 
  coord_flip() +
  labs(x = "State", y = "Ratio", fill = 'Opioid Prescribed') + 
  theme(axis.text.y = element_text(vjust = 1)) + 
  ggtitle('Opioid Prescriptions by State, 2016')

prescription_plot_1

prescription_plot_2016

op_count_TX <- op_count_by_state %>% 
  filter(State == 'TX')

op_count_NH <- op_count_by_state %>% 
  filter(State == 'NH')

op_count_MT <- op_count_by_state %>% 
  filter(State == 'MT')

op_count_RI <- op_count_by_state %>% 
  filter(State == 'RI')

op_count_WV <- op_count_by_state %>% 
  filter(State == 'WV')

op_count_WV = mutate(op_count_WV, Ratio = total / sum(total))

op_count_NM <- op_count_by_state %>% 
  filter(State == 'NM')

op_count_KY <- op_count_by_state %>% 
  filter(State == 'KY')

ggplot(op_count_TX, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Texas Prescription Count')

ggplot(op_count_NH, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('New Hampshire Prescription Count')

ggplot(op_count_MT, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Montana Prescription Count')

ggplot(op_count_RI, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Rhode Island Prescription Count')

ggplot(op_count_WV, aes(x = reorder(rx, total), y = total)) + geom_col() + coord_flip() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('West Virginia Prescription Count')

ggplot(op_count_NM, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('New Mexico Prescription Count')

ggplot(op_count_KY, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Kentucky Prescription Count')

opioid_plot_1 <- ggplot() + 
  geom_bar(aes(y = Deaths, x = State, fill = Ratio), data = overdoses, stat="identity") + 
  labs(x = "State", y = "Deaths", fill = 'Deaths Per 100,000') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle('Overdoses by State')
 

opioid_plot_2 <- ggplot(data = overdoses,aes(x = State, y = Deaths)) + 
  geom_bar(aes(fill = factor(round(100000*Ratio))), stat = 'identity') + 
  scale_colour_manual(values = (brewer.pal(3,"OrRd")), aesthetics = 'fill') +
  labs(x = "State", y = "Deaths", fill = 'Deaths Per 100,000') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle('Overdoses by State')

colnames(deaths_2016)[colnames(deaths_2016) == 'Ratio'] <- 'Ratio_2016'

opioid_plot_2016 <- ggplot() + 
  geom_bar(aes(y = Deaths_2016, x = State_2016, fill = 100000*Ratio_2016), data = deaths_2016, stat="identity") + 
  labs(x = "State", y = "Deaths", fill = 'Deaths Per 100,000') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle('Overdoses by State 2016')

opioid_plot_2

opioid_plot_1

opioid_plot_2016

as.data.frame(count_of_opioids)

overdoses

summary(deaths_2016)

unique(deaths_2016$State_2016)

overdoses_1 <- overdoses[,c("State", "Deaths", "Population", 'Ratio')]

overdoses <- overdoses_1

overdoses

list1 <- 1:25
list2 <- rep(("2014"),length(list1))
Year_2014 <- cbind(list2, list1)
list4 <- rep(('2016'), length(list1))
list6 <- 1:549
list7 <- rep(('2014'), length(list6))
list8 <- 1:547
list9 <- rep(('2016'), length(list8))

length(list3)

Year_2014 <- data.frame(overdoses, Year = list2)

Opioids_2014 <- cbind(list6, list7)

Opioids_2014 <- data.frame(op_count_by_state, Year = list7)

Opioids_2014

Opioids_2016 <- cbind(list8, list9)

Opioids_2016 <- data.frame(op_count_by_state_2016, Year = list9)

Opioids_2016

Year_2014

deaths_2016$Ratio_2016 <- round(deaths_2016$Deaths_2016/deaths_2016$Population_2016 * 100000, digits = 3)

deaths_2016

deaths_2016 <- deaths_2016[ ,c('State_2016', 'Deaths_2016', 'Population_2016', 'Ratio_2016')]

deaths_2016

Year_2016 <- data.frame(deaths_2016, Year = list4)

Year_2016

names(Year_2014) <- names(Year_2016) 

Year_merge <- rbind(Year_2014, Year_2016)

Year_merge

names(Opioids_2014) <- names(Opioids_2016)

Opioid_merge <- rbind(Opioids_2014, Opioids_2016)

Opioid_merge

summary(Opioid_merge)
death_merge <- cbind(overdoses, deaths_2016)

death_merge

colnames(deaths_2016)[colnames(deaths_2016) == 'Population'] <- 'Population_2016'

death_merge$Death_delta <- death_merge$Deaths_2016 - death_merge$Deaths

Year_merge$Death_delta <- Year_merge$Deaths_2016 - Year_merge$Deaths

death_merge$Ratio_delta <- death_merge$Ratio_2016 - death_merge$Ratio

Year_merge$Ratio_delta <- Year_merge$Ratio_2016 - Year_merge$Ratio

as.data.frame(death_merge)

as.data.frame(Year_merge)

death_merge_2 <- death_merge[, c('State','Population_2016', 'Population', 'Deaths_2016', 'Deaths', 'Ratio_2016', 'Ratio', 'Death_delta', 'Ratio_delta')]

death_merge <- death_merge_2

death_merge

op_count_by_state_2016

facet_death <- ggplot() + geom_bar(aes(y = Deaths, x = State, fill = Ratio_delta), data = death_merge, stat="identity") + geom_bar(aes(y = Deaths_2016, x = State, fill = Ratio_delta), data = death_merge, stat="identity")

facet_death

ggplot() + 
  geom_bar(data = Year_merge, stat = 'identity', aes(x = State_2016, y = Deaths_2016, fill = Ratio_2016)) + 
  facet_grid(Year ~.) +
  labs(x = "State", y = "Deaths", fill = 'Deaths Per 100,000') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle('Deaths per State, 2014 vs. 2016')

Op_merge_plot <- ggplot() + 
  geom_bar(aes(y = (Ratio * 100), x = State, fill = rx), data = Opioid_merge, stat="identity") + 
  facet_grid(Year ~.) +
  coord_flip() +
  labs(x = "State", y = "Ratio", fill = 'Opioid Prescribed') + 
  theme(axis.text.y = element_text(size = 5, angle = 45, hjust = 1)) + 
  ggtitle('Opioid Prescriptions by State, 2014 vs. 2016')

Op_merge_plot
