## Visualizing Association Between Categorical
## Variables Code ##

library(tidyverse)

## Read in Jeopardy Question Data ##

library(readxl)

jeopardy <- read_xlsx("Jeopardy Questions.xlsx")

jeopardy |>
  glimpse()

## Suppose I want to visualize the relationship 
## between the 5 most recurring categories and their
## dollar values in the 2000s (examining Jeopardy! round
## and using traditional values (200 - 1000 in 200 increments) ##

## First, we have to get the data in the right
## format! Let's subset to the 2000s decade,
## the Jeopardy! round, and the specific values! ##

jeopardy2 <- jeopardy |>
  filter(between(Year,2000,2009) &
           round == "Jeopardy!" &
           value %in% paste("$",seq(200,1000,by=200),
                            sep=""))

## Now, what are the most common categories?? ##

common_categories <- jeopardy2 |>
  group_by(category) |>
  count() |>
  arrange(desc(n)) |>
  head(5)

cc1 <- common_categories$category

## Now, we can subset jeopardy2 by these categories! ##

jeopardy3 <- jeopardy2 |>
  filter(category %in% cc1)

## Great! Now, let's build a contingency table
## to evaluate our primary question: ##

table(jeopardy3$category,
      jeopardy3$value)

## Can we tell if there is any relationship
## between category and value based on what we
## see here? Kinda hard, right? ##

## Let's add column and row totals ##

tab <- table(jeopardy3$category,
             jeopardy3$value)

tab1 <- cbind(tab,rowSums(tab))

tab1

tab2 <- rbind(tab1,colSums(tab1))

tab2

## Since we have differing row totals, 
## it isn't too easy to determine if the
## proportion of values changes across
## the levels of the categories ##

tab |> prop.table(margin = 1)

## By running the above code, we transform our 
## frequencies into the proportion of row total.
## Based on this, it looks like the proportions are 
## fairly similar for each value (kinda makes sense!!) ##

## How could we visualize this in a better medium? 
## Maybe a bar chart?? In general, we have four types of
## bar charts we can use in this circumstance. ##

## (1) Grouped Bar Chart ##

jdf <- jeopardy3 |>
  group_by(category,value) |>
  count()

jdf

jdf |>
  ggplot(aes(x = value, y = n, fill = category)) +
  geom_bar(stat = 'identity', position = position_dodge())

## (2) Stacked Bar Chart ##

jdf |>
  ggplot(aes(x = value, y = n, fill = category)) +
  geom_bar(stat = 'identity', position = position_stack())

## (3) 100% Stacked Bar Chart (Note, group_by(x)) ##

jdf |>
  group_by(value) |>
  mutate(pct = n/sum(n)) |>
  ggplot(aes(x = value, y = pct, fill = category)) +
  geom_bar(stat = 'identity', position = position_stack())

## Looking specifically at the 100% stacked bar chart,
## let's do a little cleaning. First, let's reorder the
## values to go in ascending order as we'd expect. ##

jdf |>
  group_by(value) |>
  mutate(pct = n/sum(n),
         value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = pct, fill = category)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  labs(x = "Value",
       y = "Proportion",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent)

## Now, what if we want to add data labels? We can do that
## using geom_text :) ##

jdf |>
  group_by(value) |>
  mutate(pct = n/sum(n),
         value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = pct, fill = category)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = round(pct,2)),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Value",
       y = "Proportion",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent)

## How do we turn the labels in to a percentage? ##

jdf |>
  group_by(value) |>
  mutate(pct = n/sum(n),
         value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = value, y = pct, fill = category)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Value",
       y = "Proportion",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent)

## Let's go back and look at the grouped bar chart ##

jdf |>
  ggplot(aes(x = value, y = n, fill = category)) +
  geom_bar(stat = 'identity', position = position_dodge())

## We can clean it up as well, like we did with the stacked bar ##

jdf |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = category)) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## We can also order the bars within the value groups, like
## we did with the how much/how many examples! ##

jdf |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = reorder(category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## If we want to add data labels, we can using geom_text
## just like before! ##

jdf |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = reorder(category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_text(aes(label=n),
            position=position_dodge(width=0.9),
            vjust = -0.15) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## Now, let's see if we can build 4 100% stacked bar charts, 
## one for 2000, 2005, 2008, and 2010 ##

## 2000 ##

j2000 <- jeopardy |>
  filter(Year == 2000 &
           round == "Jeopardy!" &
           value %in% paste("$",seq(200,1000,by=200),
                            sep=""))

j2000_2 <- j2000 |>
  filter(category %in% cc1)

jdf2000 <- j2000_2 |>
  group_by(category,value) |>
  count()

## Create and save a 100% stacked bar chart ##

jdf2000 |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = reorder(category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_text(aes(label=n),
            position=position_dodge(width=0.9),
            vjust = -0.15) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value Year 2000") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) -> p00

## 2005 ##

j2005 <- jeopardy |>
  filter(Year == 2005 &
           round == "Jeopardy!" &
           value %in% paste("$",seq(200,1000,by=200),
                            sep=""))

j2005_2 <- j2005 |>
  filter(category %in% cc1)

jdf2005 <- j2005_2 |>
  group_by(category,value) |>
  count()

## Create and save a 100% stacked bar chart ##

jdf2005 |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = reorder(category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_text(aes(label=n),
            position=position_dodge(width=0.9),
            vjust = -0.15) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value Year 2005") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) -> p05

## 2008 ##

j2008 <- jeopardy |>
  filter(Year == 2008 &
           round == "Jeopardy!" &
           value %in% paste("$",seq(200,1000,by=200),
                            sep=""))

j2008_2 <- j2008 |>
  filter(category %in% cc1)

jdf2008 <- j2008_2 |>
  group_by(category,value) |>
  count()

## Create and save a 100% stacked bar chart ##

jdf2008 |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = reorder(category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_text(aes(label=n),
            position=position_dodge(width=0.9),
            vjust = -0.15) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value Year 2008") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) -> p08

## 2010 ##

j2010 <- jeopardy |>
  filter(Year == 2010 &
           round == "Jeopardy!" &
           value %in% paste("$",seq(200,1000,by=200),
                            sep=""))

j2010_2 <- j2010 |>
  filter(category %in% cc1)

jdf2010 <- j2010_2 |>
  group_by(category,value) |>
  count()

## Create and save a 100% stacked bar chart ##

jdf2010 |>
  group_by(value) |>
  mutate(value = factor(value,levels = c("$200",
                                         "$400",
                                         "$600",
                                         "$800",
                                         "$1000"))) |>
  ggplot(aes(x = value, y = n, fill = category,-n))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_text(aes(label=n),
            position=position_dodge(width=0.9),
            vjust = -0.15) +
  labs(x = "Value",
       y = "Frequency",
       fill = "Category",
       title = "Proportion of Most Popular Jeopardy Categories",
       subtitle = "by Question Value Year 2010") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) -> p10

## Okay, so now how can I arrange all of these into
## a single figure? Let's use patchwork :) ##

library(patchwork)

(p00 + p05)/(p08 + p10)

## We can add an overall title ##

(p00 + p05)/(p08 + p10) +
  plot_annotation(title = "Jeopardy Question Data Analysis",
                  theme = theme(plot.title = element_text(hjust=0.50)))

## Now in this case, it would probably be best practice to eliminate
## the individual plot titles (keep year) and make colors consistent
## for each category. ##

## Let's look at another example: ##

## I want you to once again use the schrute::theoffice 
## dataset to create some visualizations ##

## First, create vertical side-by-side boxplots for episode 
## IMDB rating (imdb_rating) using color to differentiate 
## between the seasons ##

## Second, create a horizontal, ordered bar chart showing 
## the average number of lines per episode Dwight has per season, 
## also differentiating the bar color by season ##

## Third, create another horizontal ordered bar chart to display 
## the maximum imdb_rating per season once again using
## color to differentiate the seasons as before ## 

## Finally, combine the visualizations using the patchwork 
## package being mindful of unnecessary replication of legends ##