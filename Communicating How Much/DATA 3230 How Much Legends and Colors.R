## How Much Code Part 3: Colors ##

library(tidyverse)

## Let's start with our NYC Trash Horizontal Bar Chart ##

nyc <- readxl::read_xlsx("NYC Trash Data.xlsx")

## Subset to September 2011 ##

nyc_sept11 <- nyc |>
  dplyr::filter(MONTH == 9 & YEAR == 2011)

## Sum up REFUSETONSCOLLECTED variable by Borough ##

trash_tot <- nyc_sept11 |>
  dplyr::group_by(BOROUGH) |>
  dplyr::summarize(Sum_Trash = sum(REFUSETONSCOLLECTED))

## Visualization we left off with: ##

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

## Suppose I want to have the colors of the bars differ by the 
## particular borough they're representing. We can do so with
## a very slight modification to the existing code: ##

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

## Cool, right? But now, we don't really have a need for the
## y-axis labels. We can suprress those & the tick marks 
## using the theme function: ##

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

## Notice the legend title is all caps. We can modify the legend title
## in labs: ##

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

## We can also move the legend around the figure: ##

## The options we have available to us are: 
## top, left, right, bottom, or none ##

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

## Now notice, the order of items is alphabetically increasing. 
## What if we wanted the boroughs to be ordered in the same
## manner they are ordered on the bar chart? We can manually
## achieve this using another scale prefixed function: ##

## Have the computer determine the order: ##

trash_tot |>
  arrange(desc(Sum_Trash)) |>
  select(BOROUGH)

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
  scale_fill_discrete(limits = c("Brooklyn",
                                 "Queens",
                                 "Manhattan",
                                 "Bronx",
                                 "Staten Island"))

## Now what if we want to change the color palette from these
## defaults to something else? We can use some palettes built 
## for us, like the colorblind-friendly viridis: ##

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
  scale_fill_viridis(limits = c("Brooklyn",
                                 "Queens",
                                 "Manhattan",
                                 "Bronx",
                                 "Staten Island"),
                     discrete = T)

## We have options within the viridis package: ##

## magma ##

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
  scale_fill_viridis(limits = c("Brooklyn",
                                "Queens",
                                "Manhattan",
                                "Bronx",
                                "Staten Island"),
                     discrete = T,
                     option = "A")

## inferno ##

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
  scale_fill_viridis(limits = c("Brooklyn",
                                "Queens",
                                "Manhattan",
                                "Bronx",
                                "Staten Island"),
                     discrete = T,
                     option = "B")

## plasma ##

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
  scale_fill_viridis(limits = c("Brooklyn",
                                "Queens",
                                "Manhattan",
                                "Bronx",
                                "Staten Island"),
                     discrete = T,
                     option = "C")

## Option D is the viridis default ##

## cividis ##

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
  scale_fill_viridis(limits = c("Brooklyn",
                                "Queens",
                                "Manhattan",
                                "Bronx",
                                "Staten Island"),
                     discrete = T,
                     option = "E")

## RColorBrewer package also has some nice options for us: ##

library(RColorBrewer)

display.brewer.all()

## The first set of palettes are best for quantitative data ##

## The third set are best for quantitative data with clear 
## extremes ##

## The middle set is what would be most appropriate for us:
## the qualitative palettes. Let's try Pastel1: ##

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

## What if we don't like any of these and instead want to try to use our own?
## For example, what if we want to know which teams in the national league
## east (the Braves' division) hit the most and least homeruns in 2022? ##

## First, let's get the data! ##

nleast <- Lahman::Batting |>
  filter(yearID == 2022 & teamID %in% c("ATL",
                                        "MIA",
                                        "NYN",
                                        "PHI",
                                        "WAS")) |>
  group_by(teamID) |>
  summarize(HR = sum(HR))

nleast

## Specify colors for each team: ##

braves <- "#CE1141"
mets <- "#FF5910"
phillies <- "#E81828"
nats <- "#14225A"
marlins <- "#00A3E0"

cols <- c("ATL" = braves, "NYN" = mets,
          "MIA" = marlins, "WAS" = nats,"PHI" = phillies)

## Okay! So we can make our regular horizontal bar chart! ##

nleast |>
  ggplot(aes(x=HR,y=reorder(teamID,HR),fill=teamID)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=HR),hjust=-0.5,
            fontface='bold.italic') +
  labs(x = "Total Team Homeruns Hit",
       y = "National League East Team",
       title = "Total Team Homeruns for the National League East",
       subtitle = "2022 Regular Season",
       fill = "Team \nNickname") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50)) +
  scale_y_discrete(breaks = c("ATL",
                              "PHI",
                              "NYN",
                              "MIA",
                              "WAS"),
                   labels = c("Atlanta",
                              "Philadelphia",
                              "New York",
                              "Miami",
                              "Washington DC")) +
  scale_x_continuous(limits = c(0,275),
                     breaks = seq(0,275,by=25)) +
  scale_fill_manual(values = cols,
                    breaks = c("ATL","PHI","NYN",
                               "MIA","WAS"),
                    labels = c("Braves","Phillies","Mets",
                               "Marlins","Nationals"))
  
