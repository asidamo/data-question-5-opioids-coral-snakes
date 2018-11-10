library(tidyverse)
library(ggplot2)

View(prescriber_info)

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

rx_TX <- rx_per_prescriber %>% 
        filter(State == 'TX')
head(rx_TX)


prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 1)
non_prescriber <- rx_per_prescriber %>% 
              filter(Opioid.Prescriber == 0)

prescriber_by_rx <- prescriber %>% 
                    group_by(rx)

prescriber %>% 
  summarise(sum(countrx, na.rm = TRUE))

non_prescriber %>% 
  summarise(sum(countrx, na.rm = TRUE))

OxyContin <- prescriber %>% 
  filter(rx == 'OXYCONTIN') %>% 
  View()

prescriber %>% 
  summarise(mean_rx = mean(countrx, na.rm = TRUE),
              min_rx = min(countrx, na.rm = TRUE),
              max_rx = max(countrx, na.rm = TRUE))

