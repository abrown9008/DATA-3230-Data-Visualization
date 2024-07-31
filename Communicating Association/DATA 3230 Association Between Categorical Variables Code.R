## Visualizing Association Between Categorical
## Variables Code ##

library(tidyverse)

## Using the ggplot2::diamonds dataset, suppose I'm interested
## in understanding the relationship between cut and color ##

## Let's get the data into the right format ##

diamonds_df <- ggplot2::diamonds |>
  group_by(cut,color) |>
  count()

diamonds_df |>
  slice_head(n=8)

## Create a grouped bar chart ##

diamonds_df |>
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  labs(x = "Diamond Color Classification\n(from best (D) to worst (J))",
       y = "Frequency",
       fill = "Diamond Cut\nClassification",
       title = "Relationship Between Diamond Cut and Color") +
  theme_classic()

## Creating a Stacked Bar Chart ##

diamonds_df |>
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = 'identity', 
           position = position_stack()) +
  labs(x = "Diamond Color Classification\n(from best (D) to worst (J))",
       y = "Frequency",
       fill = "Diamond Cut\nClassification",
       title = "Relationship Between Diamond Cut and Color") +
  theme_classic()

## Creating a 100% Stacked Bar Chart ##

diamonds_df |>
  group_by(color) |>
  mutate(pct = n/sum(n)) |>
  ggplot(aes(x = color, y = pct, fill = cut)) +
  geom_bar(stat = 'identity', 
           position = position_stack()) +
  labs(x = "Diamond Color Classification\n(from best (D) to worst (J))",
       y = "Proportion",
       fill = "Diamond Cut\nClassification",
       title = "Relationship Between Diamond Cut and Color") +
  theme_classic()

## Changing the y-axis units and adding data labels: ##

library(viridis)

diamonds_df |>
  group_by(color) |>
  mutate(pct = n/sum(n)) |>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = color, y = pct, fill = cut)) +
  geom_bar(stat = 'identity', 
           position = position_stack()) +
  geom_label(aes(label = pct1),
             position=position_stack(vjust=0.5),
             color='white',
             size = 3,
             fontface='bold') +
  labs(x = "Diamond Color Classification\n(from best (D) to worst (J))",
       y = "Percentage",
       fill = "Diamond Cut\nClassification",
       title = "Relationship Between Diamond Cut and Color") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(option="E",
                     discrete=T)