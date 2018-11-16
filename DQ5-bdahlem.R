library(tidyverse)
library(ggplot2)
library(magrittr)

View(prescriber_info)

overdoses <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/overdoses.csv')

prescribers <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/prescriber-info.csv')

opioids <- read_csv('~/Desktop/DQ5/data-question-5-opioids-coral-snakes/opioids.csv')

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

rx_TX <- rx_per_prescriber %>% 
        filter(State == 'TX')
head(rx_TX)


prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 1)
non_prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 0)

unique(prescriber$State)

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

op_match

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
2129027/33686

NPI_count <- op_match_nonull %>% 
  count(op_match_nonull$NPI)

nrow(NPI_count) 

sum(op_match_nonull$countrx) / nrow(NPI_count)

head(overdoses)


overdoses %>% 
  rename(overdoses.State = overdoses.Abbrev)

ggplot(overdoses, aes(overdoses.Abbrev)) + geom_histogram()

death_ratio <- as.data.frame(overdoses$Population / overdoses$Deaths)

death_ratio

count_of_opioids <- op_match_nonull %>% 
  group_by(rx) %>% 
  summarise(total = sum(countrx))

as.data.frame(count_of_opioids)

ggplot(count_of_opioids, aes(x = reorder(rx, total), y = total)) + geom_col()


#ggplot(prescriber_no_null, aes(countrx, colour = State)) + 
#  geom_histogram()

#plot(prescriber_no_null)

#op_match <- prescriber_no_null$rx %in% matches

#head(op_match)


#zero_value <- prescriber$countrx == 0

#zero_value

#prescriber <- prescriber[zero_value == FALSE, ]

#str(prescriber)

#head(prescriber)

#op_match_by_count <- op_match_by_state$count

#op_match_by_value <- op_match_by_state$value

#plot(op_match_by_state, x = op_match_by_count, y = op_match_by_value)
