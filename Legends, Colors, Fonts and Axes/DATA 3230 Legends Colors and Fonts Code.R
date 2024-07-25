## DATA 3230 - Using Legends, Colors, Fonts & Axes ##

library(tidyverse)
library(readxl)

## Let's start with our NYC Trash Horizontal Bar Chart ##

nyc <- read_xlsx("Legends, Colors, Fonts and Axes/NYC Trash Data.xlsx")

## Subset to September 2011 ##

nyc_sept11 <- nyc |>
  filter(MONTH == 9 & YEAR == 2011)

## Sum up REFUSETONSCOLLECTED variable by Borough ##

trash_tot <- nyc_sept11 |>
  group_by(BOROUGH) |>
  summarize(Sum_Trash = sum(REFUSETONSCOLLECTED))

## Visualization we left off with: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## If we want to add labels to this visualization to show how much
## trash was produced by each borough, we can use a new geom called
## geom_text: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=Sum_Trash)) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Okay cool! But what are some problems? For starters,
## the label is centered at the end of the bar, making
## the text difficult to read. We can change the justification
## of the text by using the hjust argument: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=Sum_Trash),hjust=1) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Rounding to one decimal point is also a bit unusual. We 
## can adjust this by using the round function within
## geom_text: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

#### FONTS ####

## What if we wanted to control the font in terms of size,
## color, and font type? Well, what options are available to us? 
## Let's see! ##

## serif, bold-italic, & Braves Red #CE1141 ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## mono, italicized, & NY Knicks Orange ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='mono',
            fontface='italic',color='#F58426',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## serif, bold.italic, & Manchester United blue ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#6CABDD',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

#### AXES ####

## Using the Braves color visualization, what if we wanted to move
## the labels to the outside of bars? ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Whoops! Now I can't see Brooklyn's label! This means I now 
## need to change the viewing window. One way is by increasing the 
## x-axis length. We can do this by using scale_x_continuous function: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000))

## Okay cool! That solved the problem! What if we want to 
## increase the number of tick marks to be in increments of 
## 10K? We can again use scale_x_continuous: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000))

## Now, to the point that was made in last class, we don't really
## need the three extra zeros indicating that we are working in the
## 10s of thousands. We could get by with 10K, 20K, and so on. We once
## again can use the scales package & 
## scale_x_continuous function to do so: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## We can also change the font of the axis titles to match
## our data labels using the theme function! ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic',
                                   color = '#CE1141'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic',
                                    color = '#CE1141'),
        axis.text.y = element_text(family = 'serif',
                                   face = 'bold.italic',
                                   color = '#CE1141'),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic',
                                    color = '#CE1141')) +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

#### LEGENDS ####

## Suppose I want the bars to be colored by the boroughs. We can
## use the fill argument within the aes function to do so: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic')) +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## Cool! But now we don't need the y-axis labels. We can remove
## them by using the theme function: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## We can modify the legend title and labels using the labs function: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## We can also control the position of the legend using the theme function: ##

## Top ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## Bottom ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

#### COLORS ####

## What if we wanted to change the colors of the bars? We can use
## packages like RColorBrewer and viridis to do so! ##

## viridis ##

library(viridis)

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3)) +
  scale_fill_viridis(discrete = T)


## Within scale_fill_viridis, we can change the color palette by
## using the option argument, A - H. So if we want "turbo" (H) ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3)) +
  scale_fill_viridis(discrete = T,
                     option = "H")

## RColorBrewer ##

library(RColorBrewer)
print(brewer.pal.info)

## Let's use Pastel1 ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3)) +
  scale_fill_brewer(palette = "Pastel1")

## Custom Palettes ##

## We can also create our own custom palettes using the scale_fill_manual function: ##

## Specifying Custom Palette ##

borough_colors <- c("Bronx" = 'red',
                    "Brooklyn" = 'blue',
                    "Manhattan" = "orange",
                    "Queens" = "yellow",
                    "Staten Island" = 'violet')

## Generating Visualization ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash),fill=BOROUGH)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011",
       fill = "Borough") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.text.y = element_blank(),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic'),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(limits = c(0,80000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3)) +
  scale_fill_manual(values = borough_colors)
