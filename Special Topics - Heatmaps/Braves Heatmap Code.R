## Heatmap Generator ##

library(tidyverse)

## Read in Data ##

library(readxl)

df <- read_xlsx("C:/Users/abrow708/Downloads/sixplayers.xlsx",
                sheet = "Sheet2")

## Convert to Long ##

library(tidyr)

df_long <- df |>
  select(!`Median Weekly Hit Total`) |>
  pivot_longer(!Player,names_to = "Week",values_to = "Hits")

## Add Player Name from Lahman::People ##

df_long <- df_long |>
  left_join(Lahman::People |>
              select(playerID,nameFirst,nameLast),
                     by = c("Player" = "playerID")
            )

df_long <- df_long |>
  mutate(Name = paste(nameFirst," ",nameLast,sep=""),
         .before = 2)

## Add Median Weekly Hits ##

df_long <- df_long |>
  left_join(df |>
              select(Player,`Median Weekly Hit Total`)
  )

## Create Indicator Variables & Generate Visualization ##

cols <- c("Yes" = '#CE1141',
          "No" = "#13274F")

df_long |>
  mutate(`Over/Under` = if_else(Hits >= `Median Weekly Hit Total`,"Yes","No"),
         Week1 = as.numeric(gsub("Week","",Week)),
         Name2 = paste(Name,"\nMedian Number of Hits = ",`Median Weekly Hit Total`,sep="")) |>
  ggplot(aes(x = Week1, y = Name2, fill = factor(`Over/Under`))) +
  geom_tile(color = 'white') +
  geom_text(aes(label = Hits), color = "white") +
  labs(x = "Week",
       y = "Player Name",
       fill = "Exceeding Estimated\nMedian Number of Hits?",
       title = "Atlanta Braves Players' Weekly Hit Totals",
       subtitle = "2023 Regular Season") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.50,
                                  face = 'bold'),
        plot.subtitle = element_text(hjust = 0.50,
                                  face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold')) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(1,26,by=2))


