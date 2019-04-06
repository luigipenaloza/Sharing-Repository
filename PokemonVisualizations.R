#Final Project Poster (Pokemon)
#Information Visualization
#IST 719
#Luigi Penaloza


#LOADING LIBRARIES

#For making plots
install.packages("ggplot2")
library(ggplot2) 

#For data manipulation
install.packages("plyr")
library(dplyr)  

#For adding themes to plots and graphs
install.packages("GGally")
library(ggthemes)   

#For data manipulation
install.packages("tidyverse")
library(tidyr)      

#LOADING DATASET AND EXPLORING DATA

file.choose()
myfile<-file.choose()
pokemon<-read.csv(myfile, header = TRUE)
pokemon

dim(pokemon)

names(pokemon)
str(pokemon)

##pokemon$Name <- as.character(pokemon$Name)
length(unique(pokemon$name))


#Coorelation and Distribution of Variables
ggpairs(pokemon, columns = c('attack', 'defense', 'hp', 'sp.atk', 'sp.def', 'speed')) +
  theme_bw() +
  labs(title = 'Correlation Matrix of Pokemon Stats')


1)#######NUMBER OF POKEMON BY TYPE (IT WORKS!- I MADE IT WORK :P)

pokemon %>%
  group_by(type1) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = reorder(type1, number), y = number , fill = type1)) +
  geom_bar(stat = 'identity') +
  xlab(label = "Pokemon Type") +
  ylab(label = "Pokemon Count") +
  ggtitle(label = "Pokemon Count by Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip() +
  geom_text(aes(label = number), hjust = -1.0) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

#POKEMON BY SECONDARY TYPES (GOT IT TO WORK BUT NOT PERFECT)
#Figure out what to use instead of filter or how to filter
pokemon %>%
  group_by(type2) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = reorder(type2, number), y = number , fill = type2)) +
  geom_bar(stat = 'identity') +
  xlab(label = "Type of Pokemon") +
  ylab(label = "Number of Pokemon") +
  ggtitle(label = "Number of Pokemon by Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip() +
  geom_text(aes(label = number), hjust = -1.0) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

2)########HEATMAP DISTRIBUTION OF POKEMON STATS BY TYPE (DID NOT WORK)
#Error: unexpected symbol in:
"  theme(legend.position='bottom'
ggplot"

pokemon %>%
  gather(., key, value, hp:speed) %>%
  group_by(., type1, key) %>%
  summarise(., base_total = as.integer(mean(value))) %>%
  ggplot(., aes(y=type1, x=key)) + 
  geom_tile(aes(fill =base_total)) +
  theme_bw() + 
  theme(legend.position = 'bottom') +
  geom_text(aes(label = base_total), color = 'white', size = 3) +
  labs(x='Stat Category', y='Pokemon Type', title = 'Heatmap Distribution of Pokemon Stats by Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position='bottom'
        
3)#TOTAL SCORE OF POKEMON BY GENERATION AND LEGENDARY TYPE

ggplot(data = pokemon, aes(x = generation, y = base_total, fill = as.factor(generation))) +
geom_boxplot() +
xlab(label = "Pokemon Generation") +
ylab(label = "Total Base Stats of Pokemon") +
ggtitle(label = "Pokemon Total Base Stats by Generation facet by Legendary Flag) +
theme(plot.title = element_text(hjust = 0.5)) +
theme(legend.position="none") +
facet_grid( ~ is_legendary)+
theme(panel.background = element_rect(fill = 'white', colour = 'white'))




4)############TOTAL SCORE OF POKEMON BY TYPE (WORKS!!! :)HEHE

ggplot(data = pokemon, aes(x = type1, y = base_total, fill = type1,)) +
  geom_boxplot() +
  xlab(label = "Pokemon Type") +
  ylab(label = "Total Base Stats of Pokemon") +
  ggtitle(label = "Pokemon Total Base Stats by Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

  

#DISTRIBUTION OF ALL SCORES (SHOULD WORK BUT IT'S NOT WORKING)
pokemon %>%
  gather(key, value, hp:speed) %>%
  ggplot(aes(x=key, y=value, fill = key)) +
  geom_boxplot() +
  theme(legend.position = 'none') +
  labs(y='Stats', x='Category', title = 'Boxplot Distribution of Overall Pokemon Stats') +
  theme(plot.title = element_text(hjust = 0.5))

#SCORE OF POKEMON BY GENERATION

pokemon %>%
  gather(key, value, hp:speed) %>%
  ggplot(aes(x = generation, y = value, fill = as.factor(generation))) +
  geom_boxplot() +
  facet_grid(~key) +
  xlab(label = "Generation") +
  ylab(label = "Score") +
  ggtitle("Various score based on Generation flag") +
  theme(plot.title = element_text(hjust = 0.5))

#SCORE OF POKEMON BY LEGENDARY TYPE (SHOULD WORK BUT PLOT NOT SHOWING,)
#Warning message:attributes are not identical across measure variables;
#they will be dropped

pokemon %>%
  gather(key, value, hp:speed) %>%
  ggplot(aes(x = is_legendary, y = value, fill = as.factor(is_legendary))) +
  geom_boxplot() +
  facet_grid(~key) +
  xlab(label = "Lengendary") +
  ylab(label = "Score") +
  ggtitle("Various score based on Lengendary flag") +
  theme(plot.title = element_text(hjust = 0.5))

#Change color of graphs
#Add neural Network Graphf


#Neural Network Graph



