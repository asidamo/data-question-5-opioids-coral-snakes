library(tidyverse)
library(ggplot2)
library(magrittr)

View(prescriber_info)

overdoses <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/overdoses.csv')

prescribers <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/prescriber-info.csv')

opioids <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/opioids.csv')

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 1)
non_prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 0)

drops <- c("PR", "ZZ", "AA", "AE", "GU", "VI", "DC")

drop_states <- prescriber$State %in% drops

drop_states

non_drop_states <- non_prescriber$State %in% drops

non_drop_states

prescriber <- prescriber[drop_states == FALSE, ]

prescriber

unique(prescriber$State)

summary(prescriber)

non_prescriber <- non_prescriber[non_drop_states == FALSE, ]

non_prescriber

unique(non_prescriber$State)

summary(non_prescriber)

prescriber_by_rx <- prescriber %>% 
                    group_by(rx)

prescriber %>% 
  summarise(sum(countrx, na.rm = TRUE))

non_prescriber %>% 
  summarise(sum(countrx, na.rm = TRUE))

prescriber %>% 
  summarise(mean_rx = mean(countrx, na.rm = TRUE),
              min_rx = min(countrx, na.rm = TRUE),
              max_rx = max(countrx, na.rm = TRUE))

head(prescriber)
rxcount <- sum(prescriber$countrx)
rxcount
matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROCODONE.ACETAMINOPHEN", "HYDROMORPHONE.HCL", "METHADONE.HCL",
             "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

op_match <- prescriber$rx %in% matches

non_op_match <- non_prescriber$rx %in% matches

non_op_match

summary(op_match)

op_match <- as.data.frame(op_match)

op_match

op_match <- prescriber[op_match == TRUE, ]

op_mismatch <- prescriber[op_match != TRUE, ]

head(op_match)

non_op_match <- non_prescriber[non_op_match == TRUE, ]

head(non_op_match)

summary(op_match)

summary(non_op_match)

op_match_by_state <- aggregate(data.frame(count = op_match$State), list(value = op_match$State), length)

aggregate(data.frame(count = op_match$rx), list(value = op_match$rx), length)

head(op_match_by_state)

op_match_by_state <- as.data.frame(op_match_by_state)

str(op_match_by_state)

op_match_by_state$count <- as.numeric(op_match_by_state$count)

str(op_match_by_state)

count_by_state <- op_match_by_state %>% 
  group_by(value) %>% 
  summarise(total = sum(count))

str(op_match)

str(prescriber)

zero_value <- c(0)

drop_null_rx <- prescriber$countrx %in% zero_value

drop_null_rx

nondrop_null_rx <- non_prescriber$countrx %in% zero_value

nondrop_null_rx

prescriber_no_null <- prescriber[drop_null_rx == FALSE, ]

non_prescriber_no_null <- non_prescriber[nondrop_null_rx == FALSE, ]

head(non_prescriber_no_null)

summary(non_prescriber_no_null)

head(prescriber_no_null)

summary(prescriber_no_null)

op_match_nonull <- prescriber_no_null$rx %in% matches

op_match_nonull <- prescriber_no_null[op_match_nonull == TRUE, ]

non_op_match_nonull <- non_prescriber_no_null$rx %in% matches

non_op_match_nonull <- non_prescriber_no_null[non_op_match_nonull == TRUE, ]

#The dataset listed below is cleaned to include ONLY valid US states, non-null values for 'countrx', and prescription drugs listed as opioids

op_match_nonull

non_op_match_nonull

NPI_count <- op_match_nonull %>% 
  count(op_match_nonull$NPI)

nrow(NPI_count) 

sum(op_match_nonull$countrx) / nrow(NPI_count)

head(overdoses)

overdoses = mutate(overdoses, Ratio = Deaths/Population)

count_of_opioids <- op_match_nonull %>% 
  group_by(rx) %>% 
  summarise(total = sum(countrx))

op_count_by_state <- op_match_nonull %>% 
  group_by(State, rx) %>% 
  summarise(total = sum(countrx))

op_count_by_state = mutate(op_count_by_state, Ratio = total/sum(total))

ggplot(op_count_by_state, aes(State)) + geom_bar(aes(fill = rx), position = position_stack(reverse = FALSE)) + coord_flip()

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

ggplot(op_count_WV, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('West Virginia Prescription Count')

ggplot(op_count_NM, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('New Mexico Prescription Count')

ggplot(op_count_KY, aes(x = reorder(rx, total), y = total)) + geom_col() + labs(x = "Drug Names", y = "Total Prescriptions") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Kentucky Prescription Count')

as.data.frame(count_of_opioids)

ggplot(count_of_opioids, aes(x = reorder(rx, total), y = total)) + geom_col()