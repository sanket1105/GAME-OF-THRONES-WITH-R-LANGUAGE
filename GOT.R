
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(igraph)


setwd("C:\\Users\\Sanket\\Desktop\\kaglle\\Game Of Thrones")
battles=read.csv("datasets_43_77_battles.csv",stringsAsFactors = F,na.strings=c("","NA"))
characdeath=read.csv("datasets_43_77_character-deaths.csv",stringsAsFactors = F,na.strings=c("","NA"))
characpred=read.csv("datasets_43_77_character-predictions.csv",stringsAsFactors = F,na.strings=c("","NA"))

glimpse(battles)
glimpse(characdeath)
glimpse(characpred)

#=========================================================================

View(battles)

## battle type and attacker outcome has one missing data
### lets say battle type as siege and loss

battles$battle_type[38]="siege"
  battles$attacker_outcome[38]="loss"

#=============================================================

  ## total number of battles : 38
  battles %>% summarise(mean(attacker_outcome == 'win'), sum(attacker_outcome == 'win'))
  
## adding the winner variable in the data set  
battles=battles %>% 
  mutate(winner=ifelse(attacker_outcome=="win","attacker","defender"))
  
  ggplot(battles,aes(x=1, fill=winner)) +
    geom_bar(width=1) +
    coord_polar("y",start=0) + 
  ggtitle(' Winners - Attackers VS Defenders')
  
  
  data.frame(battles$battle_type,battles$attacker_outcome)

  
  ggplot(battles,aes(x=battle_type,y=(attacker_outcome="win")))+geom_bar(stat="identity")
  
## to see the outcome of the battles
    
  housedata=battles %>% 
    gather(attacker, AttcakerHouse, matches('attacker_\\d')) %>%
    gather(defender, DefenderHouse, matches('defender_\\d')) %>% 
    mutate(winnerhouse=ifelse(attacker_outcome=="win",AttcakerHouse,DefenderHouse),
    loserhouse=ifelse(attacker_outcome=="win",DefenderHouse,AttcakerHouse)) %>% 
    select(winnerhouse,loserhouse,name) %>% 
    na.omit
  

  #==========================================================================
  HouseTree <- housetally %>% 
    select(loserhouse, winnerhouse) %>% 
    as.matrix %>%  #just an annoying thing you have to do
    graph_from_edgelist
#=======================================================================================  

  V(HouseTree)$wins <- degree(HouseTree, mode='in')   ## arrow pointing in means battle won
  V(HouseTree)$losses <- degree(HouseTree, mode='out')  ## arrow pointing out means battle lost
  V(HouseTree)$battles <- degree(HouseTree, mode='all')
  E(HouseTree)$weight <- 1
  
  E(HouseTree)$battle_name <- housedata$name
#=====================================================================================
  
  simple_HouseTree <- HouseTree %>% simplify(remove.loops = F)
  
#==================================================================================
  
  ## plotting the battle chart among various houses
  
f <- colorRampPalette(c('black', 'red'))
colors <- f(length(unique(E(simple_HouseTree)$weight)))
colors <- colors[E(simple_HouseTree)$weight]
   
plot(simple_HouseTree, vertex.color="light blue",edge.arrow.size = 1, edge.width =E(HouseTree)$weight , edge.color=colors, 
       vertex.frame.color="red", layout = layout.davidson.harel, edge.curved = 0.2, vertex.label.cex = 1)
  
#===============================================================================================================

## representing in circular form

  plot(simple_HouseTree,vertex.color="grey", vertex.edge.arrow.size = 1, edge.width = E(HouseTree)$weight, edge.color=colors, vertex.frame.color='orange',
       layout =layout.davidson.harel, edge.curved = 0.3, vertex.label.cex = 01)+
  title(' Winners and Losers : GAME OF THRONES')
  
  #=========================================================================================================
  
## data frame for the battles fought and won by various houses

  data.frame(house = V(HouseTree)$name, 
             wins = V(HouseTree)$wins,
             losses = V(HouseTree)$losses,
             houses_fought = V(HouseTree)$battles,
             percentage = round(V(HouseTree)$wins/V(HouseTree)$battles, 2)) %>%
    arrange(desc(percentage), desc(wins), desc(houses_fought))
#======================================================================================================  
  

  ## to decide which house is more powerful among all:
## lets use page_rank algo
  
  powerfulhouse<- HouseTree %>% page_rank
  pr
  sort(powerfulhouse$vector, decreasing=T)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  