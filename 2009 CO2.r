## Final Project
## Title
## Lillian Elek


## Install Packages ##
install.packages(c("maps", "mapdata"))
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))

library(maps)
library(mapdata)
library(ggplot2)

## Read in Data ##

state.energy.consumption <- read.csv("energy by state and sector.csv", header = T, nrows=52)
attach(state.energy.consumption)

head(state.energy.consumption)
## reads in top 10 from file##

######################################################
## GGplot Histogram of Carbon Dioxide ## 
carbon.dioxide <- (Carbon.Dioxide.emissions..million.metric.tons.)

ggplot(data=state.energy.consumption, aes(carbon.dioxide)) + geom_histogram(fill="red", col="white")+
  ggtitle("Histogram of Carbon Dioxide") + xlab("CO2 in million metric tons")


## GGplot Histogram of Population ## 
ggplot(data=state.energy.consumption, aes(population)) + geom_histogram(fill="light blue", col="white")+
  ggtitle("Histogram of Population") + xlab("Population in Millions") 

##GGPlot Histogram of Total Energy Generated ##
ggplot(data=state.energy.consumption, aes(Total.Generated)) + geom_histogram(fill="green", col="white")+
  ggtitle("Histogram of Total Energy Generated") + xlab("Total Energy Generated in MWH") 


## GGplot looking at rise of CO2 Emmissions with rise of Population ##
populationCO2 <- read.csv("CarbonPopulation.csv", header = T, nrows = 52)
attach(populationCO2)

carbon.dioxide.pop <- (Carbon.Dioxide.emissions..million.metric.tons.)

summary(lm(carbon.dioxide.pop ~ population))

ggplot(data.frame(population, carbon.dioxide.pop), aes(population, carbon.dioxide.pop)) +
  geom_point(shape=16, col="red") + ggtitle("CO2 Emmissions by Population")+
  ylab("CO2 in million metric tons")+ xlab("Population") + geom_smooth(method=lm)



## Map of population ## 

usa <- map_data("usa")
map("usa", fill = TRUE, col = "white", namesonly = TRUE)

title("2009 US Population")

## Select States with highest Carbon Dioxide ##
for(i in 1:50){
  if(population[i] < 2000000) points(Longitude[i], Latitude[i], pch=20,cex=1,col="green")	
  if(population[i] > 2000000 & population[i] <4000000) points(Longitude[i],Latitude[i], pch=20,cex=1.5,col="dark green")
  if(population[i] > 4000000 & population[i] <6000000) points(Longitude[i],Latitude[i], pch=20,cex=2,col="yellow")
  if(population[i] > 6000000 & population[i] <8000000) points(Longitude[i],Latitude[i], pch=20,cex=2.5,col="orange")
  if(population[i] > 8000000 & population[i] <10000000) points(Longitude[i],Latitude[i], pch=20,cex=3,col="red")
  if(population[i] > 10000000) points(Longitude[i], Latitude[i], pch=20,cex=3.5,col="black")	
  
}
legend(-79,35,c("Pop < 2 mil","Pop 2-4 mil","Pop 4-6 mil", "Pop 6-8 mil", "Pop 6-10 mil", "Pop > 10 mil")
       ,fill=c("green","dark green","yellow","orange", "red", "black"),text.width=0.7,cex=0.53,bty="n")




## Map of USA Carbon Dioxide Emissions ##
usa <- map_data("usa")
map("usa", fill = TRUE, col = "white", namesonly = TRUE)

title("USA Carbon Dioxide Emissions Generated in 2009")

## Select States with highest Carbon Dioxide ##
for(i in 1:50){
  if(carbon.dioxide[i] < 100) points(Longitude[i], Latitude[i], pch=20,cex=1,col="green")	
  if(carbon.dioxide[i] > 100 & carbon.dioxide[i] <200) points(Longitude[i],Latitude[i], pch=20,cex=1.5,col="dark green")
  if(carbon.dioxide[i] > 200 & carbon.dioxide[i] <300) points(Longitude[i],Latitude[i], pch=20,cex=2,col="orange")
  if(carbon.dioxide[i] > 300 & carbon.dioxide[i] <400) points(Longitude[i],Latitude[i], pch=20,cex=2.5,col="red")
  if(carbon.dioxide[i] > 400 & carbon.dioxide[i] <600) points(Longitude[i],Latitude[i], pch=20,cex=3,col="black")
  
}
legend(-79,34,c("CO2 < 100","CO2 100-200","CO2 200-300", "CO2 300-400", "CO2 400-600")
       ,fill=c("green","dark green","orange", "red", "black"),text.width=0.7,cex=0.53,bty="n")



####################################################################3

## Anova for Carbon Dixoide Emissions ##
anova(lm(carbon.dioxide ~ Hydro + Coal + Gas + Biomass + Geothermal + Solar + Wind + Petroleum + Nuclear + Other))
#F-Test 

summary(lm(carbon.dioxide ~ Hydro + Coal + Gas + Biomass + Geothermal + Solar + Wind + Petroleum + Nuclear + Other))
#T-test

#Fit Model
summary(lm(carbon.dioxide ~ Coal + Gas  + Geothermal + Nuclear ))


##############################################################
##Anova for Total Generated Energy ##

state.energy.consumption <- read.csv("energy by state and sector.csv", header = T, nrows=52)
attach(state.energy.consumption)


anova(lm(Total.Generated ~ Region + population + Occupied.Housing.Units))
#F-Test

summary(lm(Total.Generated ~ Region + population + Occupied.Housing.Units))
#T-Test



###############################################################

## GGplot looking at CO2 Emmissions in each Region ##
ggplot(data.frame(Region, carbon.dioxide), aes(Region, carbon.dioxide))+
  geom_point(shape=16, col="red") + ggtitle("CO2 Emmissions per Region") +
  ylim(0,600) + ylab("CO2 in million metric tons")



