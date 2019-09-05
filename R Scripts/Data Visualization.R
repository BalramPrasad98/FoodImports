library(ggplot2)

##Formatting Data
Dairy <- read.csv("Dairy.csv", header = TRUE)
Fruit <- read.csv("Fruit.csv")
Grain <- read.csv("Grains.csv")
Meat <- read.csv("Meat.csv")
Vegetables <- read.csv("Vegetables.csv")

titles <- c("Country",2002:2017)
colnames(Dairy) <- titles
colnames(Fruit) <- titles
colnames(Grain) <- titles
colnames(Meat) <- titles
colnames(Vegetables) <- titles

#Formatting Dairy DataFrame for ggplot
Dairy2 <- as.data.frame(t(Dairy))
Country <- Dairy2[1,]
Dairy3 <- Dairy2[-1,]
Dairy3$Year <- 2002:2017

#Formatting Fruit DataFrame for ggplot
Fruit2 <- as.data.frame(t(Fruit))
Country <- Fruit2[1,]
Fruit3 <- Fruit2[-1,]
Fruit3$Year <- 2002:2017

#Formatting Grain DataFrame for ggplot
Grain2 <- as.data.frame(t(Grain))
Grain_Country <- Grain2[1,]
Grain3 <- Grain2[-1,]
Grain3 <- Grain3[-nrow(Grain3),]
Grain3$Year <- 2002:2017

#Formatting Meat DataFrame for ggplot
Meat2 <- as.data.frame(t(Meat))
Meat_Country <- Meat2[1,]
Meat3 <- Meat2[-1,]
Meat3$Year <- 2002:2017

#Formatting Vegetable DataFrame for ggplot
Vegetables2 <- as.data.frame(t(Vegetables))
Country <- Vegetables2[1,]
Vegetables3 <- Vegetables2[-1,]
Vegetables3$Year <- 2002:2017


##Year versus total dairy imports 
Dairy3$Total <- as.numeric(as.character(Dairy3$V10))
Total_Dairy <- ggplot(Dairy3, aes(x=Year, y=Total)) + geom_point(colour = "blue") + 
  labs(title="Total Dairy Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(1000,2000), breaks = seq(1000, 2000, 500))
Total_Dairy

##Total Fruits Visualization
Fruit3$Total <- as.numeric(as.character(Fruit3$V12))
Total_Fruit <- ggplot(Fruit3, aes(x=Year, y=Total)) + geom_point(colour = "orange") +
  labs(title="Total Fruit Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(5000,20000), breaks = seq(5000, 20000, 5000))
Total_Fruit

##Total Grains Visualization
Grain3$Total <- as.numeric(as.character(Grain3$V11))
Total_Grain <- ggplot(Grain3, aes(x=Year, y=Total)) + geom_point(colour = "brown") +
  labs(title="Total Grain Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(3000,11000), breaks = seq(3000, 11000, 2000))
Total_Grain

##Total Meat Visualization
Meat3$Total <- as.numeric(as.character(Meat3$V9))
Total_Meat <- ggplot(Meat3, aes(x=Year, y=Total)) + geom_point(colour = "red") +
  labs(title="Total Meat Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(3000,11000), breaks = seq(3000, 11000, 2000))
Total_Meat

##Total Vegetables Visualization
Vegetables3$Total <- as.numeric(as.character(Vegetables3$V9))
Total_Vegetables <- ggplot(Vegetables3, aes(x=Year, y=Total)) + geom_point(colour = "darkgreen") +
  labs(title="Total Vegetables Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(4000,13000), breaks = seq(4000, 13000, 2000))
Total_Vegetables

Total_Dairy
Total_Fruit 
Total_Grain 
Total_Meat 
Total_Vegetables

#Dairy ggplots
## Italy - Year versus total dairy imports
Dairy3$Italy <- as.numeric(as.character(Dairy3$V1))
Italy_graph <- ggplot(Dairy3, aes(x=Year, y=Italy)) + geom_point() + 
  labs(title="Total Dairy imports from Italy", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(150,350), breaks = seq(150,350,50))
Italy_graph

## New Zealand - Year versus total dairy imports 
Dairy3$NewZealand <- as.numeric(as.character(Dairy3$V2))
NewZealand_graph <- ggplot(Dairy3, aes(x=Year, y=NewZealand)) + geom_point() + 
  labs(title="Total Dairy imports from New Zealand", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(150,350), breaks = seq(150,350,50))
NewZealand_graph


## France - Year versus total dairy imports 
Dairy3$France <- as.numeric(as.character(Dairy3$V3))
France_graph <- ggplot(Dairy3, aes(x=Year, y=France)) + geom_point() + 
  labs(title="Total Dairy imports from France", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(50,200), breaks = seq(50,200,50))
France_graph

## Ireland - Year versus total dairy imports 
Dairy3$Ireland <- as.numeric(as.character(Dairy3$V4))
Ireland_graph <- ggplot(Dairy3, aes(x=Year, y=Ireland)) + geom_point() + 
  labs(title="Total Dairy imports from Ireland", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,150), breaks = seq(0,150,50))
Ireland_graph

## Canada - Year versus total dairy imports 
Dairy3$Canada <- as.numeric(as.character(Dairy3$V5))
Canada_graph <- ggplot(Dairy3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Dairy imports from Canada", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,150), breaks = seq(0,150,50))
Canada_graph

## Spain - Year versus total dairy imports 
Dairy3$Spain <- as.numeric(as.character(Dairy3$V6))
Spain_graph <- ggplot(Dairy3, aes(x=Year, y=Spain)) + geom_point() + 
  labs(title="Total Dairy imports from Spain", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,150), breaks = seq(0,150,50))
Spain_graph

## Netherlands - Year versus total dairy imports 
Dairy3$Spain <- as.numeric(as.character(Dairy3$V6))
Spain_graph <- ggplot(Dairy3, aes(x=Year, y=Spain)) + geom_point() + 
  labs(title="Total Dairy imports from Spain", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,125), breaks = seq(0,125,25))
Spain_graph


## Netherlands - Year versus total dairy imports 
Dairy3$Netherlands <- as.numeric(as.character(Dairy3$V7))
Netherlands_graph <- ggplot(Dairy3, aes(x=Year, y=Netherlands)) + geom_point() + 
  labs(title="Total Dairy imports from Netherlands", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,125), breaks = seq(0,125,25))
Netherlands_graph

## Mexico - Year versus total dairy imports 
Dairy3$Mexico <- as.numeric(as.character(Dairy3$V8))
Mexico_graph <- ggplot(Dairy3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Dairy imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(00,100), breaks = seq(0,100,25))
Mexico_graph

## Rest of World - Year versus total dairy imports 
Dairy3$RestofWorld <- as.numeric(as.character(Dairy3$V9))
ROW_graph <- ggplot(Dairy3, aes(x=Year, y=RestofWorld)) + geom_point() + 
  labs(title="Total Dairy imports from Rest of World", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(350,600), breaks = seq(350,600,50))
ROW_graph

##Year versus total dairy imports 
Dairy3$Total <- as.numeric(as.character(Dairy3$V10))
Total_graph <- ggplot(Dairy3, aes(x=Year, y=Total)) + geom_point() +
  labs(title="Total Dairy Imports", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(1000,2000), breaks = seq(1000, 2000, 50))
Total_graph

Fruit3
#Fruit ggplots
## Mexico - Year versus total fruit imports
Fruit3$Mexico <- as.numeric(as.character(Fruit3$V1))
Mexico_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Fruit imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(850,6250), breaks = seq(850,6250,500))
Mexico_fruit_graph

## Mexico - Year versus total fruit imports
Fruit3$Mexico <- as.numeric(as.character(Fruit3$V1))
Mexico_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Fruit imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(850,6250), breaks = seq(850,6250,500))
Mexico_fruit_graph

## Chile  - Year versus total fruit imports
Fruit3$Chile <- as.numeric(as.character(Fruit3$V2))
Chile_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Chile)) + geom_point() + 
  labs(title="Total Fruit imports from Chile", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(800,2500), breaks = seq(800,2500,100))
Chile_fruit_graph

## Guatemala  - Year versus total fruit imports
Fruit3$Guatemala <- as.numeric(as.character(Fruit3$V3))
Guatemala_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Guatemala)) + geom_point() + 
  labs(title="Total Fruit imports from Guatemala", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(250,1250), breaks = seq(250,1250,250))
Guatemala_fruit_graph

## Costa Rica  - Year versus total fruit imports
Fruit3$CostaRica <- as.numeric(as.character(Fruit3$V4))
CostaRica_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=CostaRica)) + geom_point() + 
  labs(title="Total Fruit imports from Costa Rica", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(500,1250), breaks = seq(500,1250,250))
CostaRica_fruit_graph

## China  - Year versus total fruit imports
Fruit3$China <- as.numeric(as.character(Fruit3$V5))
China_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=China)) + geom_point() + 
  labs(title="Total Fruit imports from China", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(150,1500), breaks = seq(150,1500,250))
China_fruit_graph

## Peru  - Year versus total fruit imports
Fruit3$Peru <- as.numeric(as.character(Fruit3$V6))
Peru_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Peru)) + geom_point() + 
  labs(title="Total Fruit imports from Peru", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,50))
Peru_fruit_graph

## Canada  - Year versus total fruit imports
Fruit3$Canada <- as.numeric(as.character(Fruit3$V7))
Canada_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Fruit imports from Canada", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(200,750), breaks = seq(200,750,50))
Canada_fruit_graph

## Brazil  - Year versus total fruit imports
Fruit3$Brazil <- as.numeric(as.character(Fruit3$V8))
Brazil_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Fruit imports from Brazil", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(100,550), breaks = seq(100,550,50))
Brazil_fruit_graph

## Ecuador  - Year versus total fruit imports
Fruit3$Ecuador <- as.numeric(as.character(Fruit3$V9))
Ecuador_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Ecuador)) + geom_point() + 
  labs(title="Total Fruit imports from Ecuador", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(250,600), breaks = seq(250,600,50))
Ecuador_fruit_graph

## Thailand  - Year versus total fruit imports
Fruit3$Thailand <- as.numeric(as.character(Fruit3$V10))
Thailand_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Thailand)) + geom_point() + 
  labs(title="Total Fruit imports from Thailand", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  scale_y_continuous(limits = c(150,500), breaks = seq(150,500,50))
Thailand_fruit_graph

## Rest of World - Year versus total fruit imports 
Fruit3$RestofWorld <- as.numeric(as.character(Fruit3$V11))
ROW_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=RestofWorld)) + geom_point() + 
  labs(title="Total Fruit imports from Rest of World", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(1400,3000), breaks = seq(1400,3000,250))
ROW_fruit_graph


## Total - Year versus total fruit imports 
Fruit3$Total <- as.numeric(as.character(Fruit3$V12))
Total_fruit_graph <- ggplot(Fruit3, aes(x=Year, y=Total)) + geom_point() + 
  labs(title="Total Fruit imports from Total", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(5000,20000), breaks = seq(5000,20000,2500))
Total_fruit_graph

Grain[1]
Grain3
## Grain Plots
## Canada - Year versus total grain imports 
Grain3$Canada <- as.numeric(as.character(Grain3$V1))
Canada_grain_graph <- ggplot(Grain3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Grain imports from Canada", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(1500,5000), breaks = seq(1500,5000,250))
Canada_grain_graph

## Mexico - Year versus total grain imports 
Grain3$Mexico <- as.numeric(as.character(Grain3$V2))
Mexico_grain_graph <- ggplot(Grain3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Grain imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + scale_y_continuous(limits = c(200,1500), breaks = seq(200,1500,250))
Mexico_grain_graph

## Thailand - Year versus total grain imports 
Grain3$Thailand <- as.numeric(as.character(Grain3$V3))
Thailand_grain_graph <- ggplot(Grain3, aes(x=Year, y=Thailand)) + geom_point() + 
  labs(title="Total Grain imports from Thailand", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(100,600), breaks = seq(100,600,50))
Thailand_grain_graph

## Italy - Year versus total grain imports 
Grain3$Italy <- as.numeric(as.character(Grain3$V4))
Italy_grain_graph <- ggplot(Grain3, aes(x=Year, y=Italy)) + geom_point() + 
  labs(title="Total Grain imports from Italy", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(150,550), breaks = seq(150,550,50))
Italy_grain_graph

## Germany - Year versus total grain imports 
Grain3$Germany <- as.numeric(as.character(Grain3$V5))
Germany_grain_graph <- ggplot(Grain3, aes(x=Year, y=Germany)) + geom_point() + 
  labs(title="Total Grain imports from Germany", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(50,350), breaks = seq(50,350,50))
Germany_grain_graph

## India - Year versus total grain imports 
Grain3$India <- as.numeric(as.character(Grain3$V6))
India_grain_graph <- ggplot(Grain3, aes(x=Year, y=India)) + geom_point() + 
  labs(title="Total Grain imports from India", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(50,350), breaks = seq(50,350,50))
India_grain_graph

## China - Year versus total grain imports 
Grain3$China <- as.numeric(as.character(Grain3$V7))
China_grain_graph <- ggplot(Grain3, aes(x=Year, y=China)) + geom_point() + 
  labs(title="Total Grain imports from China", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,50))
China_grain_graph

## France - Year versus total grain imports 
Grain3$France <- as.numeric(as.character(Grain3$V8))
France_grain_graph <- ggplot(Grain3, aes(x=Year, y=France)) + geom_point() + 
  labs(title="Total Grain imports from France", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(50,250), breaks = seq(50,250,50))
France_grain_graph

## United Kingdom - Year versus total grain imports 
Grain3$UK <- as.numeric(as.character(Grain3$V9))
UK_grain_graph <- ggplot(Grain3, aes(x=Year, y=UK)) + geom_point() + 
  labs(title="Total Grain imports from UK", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(50,200), breaks = seq(50,200,50))
UK_grain_graph

## Rest of World - Year versus total grain imports 
Grain3$ROW <- as.numeric(as.character(Grain3$V10))
ROW_grain_graph <- ggplot(Grain3, aes(x=Year, y=ROW)) + geom_point() + 
  labs(title="Total Grain imports from ROW", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(600,2000), breaks = seq(600,2000,250))
ROW_grain_graph

###### INSERT TOTAL FOR GRAIN ######

Meat[1]

## Meat plots

## Canada - Year versus total meat imports 
Meat3$Canada <- as.numeric(as.character(Meat3$V1))
Meat3$Canada
Canada_meat_graph <- ggplot(Meat3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Meat imports from Canada", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(1500,2250), breaks = seq(1500, 2250, 250))
Canada_meat_graph

##### SKIPPING AUSTRALIA #####
## Australia - Year versus total meat imports 
Meat3$Australia <- as.numeric(as.character(Meat3$V2))
Australia_meat_graph <- ggplot(Meat3, aes(x=Year, y=Australia)) + geom_point() + 
  labs(title="Total Meat imports from Australia", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(1000,2250), breaks = seq(1000,2250,250))
Australia_meat_graph

## New Zealand - Year versus total meat imports 
Meat3$NewZealand <- as.numeric(as.character(Meat3$V3))
NZ_meat_graph <- ggplot(Meat3, aes(x=Year, y=NewZealand)) + geom_point() + 
  labs(title="Total Meat imports from New Zealand", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(500,1250), breaks = seq(500, 1250, 250))
NZ_meat_graph

## Mexico - Year versus total meat imports 
Meat3$Mexico <- as.numeric(as.character(Meat3$V4))
Mexico_meat_graph <- ggplot(Meat3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Meat imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(500,1250), breaks = seq(500, 1250, 250))
Mexico_meat_graph


## Nicaragua - Year versus total meat imports 
Meat3$Nicaragua <- as.numeric(as.character(Meat3$V5))
Nicaragua_meat_graph <- ggplot(Meat3, aes(x=Year, y=Nicaragua)) + geom_point() + 
  labs(title="Total Meat imports from Nicaragua", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, 50))
Nicaragua_meat_graph


## Poland - Year versus total meat imports 
Meat3$Poland <- as.numeric(as.character(Meat3$V6))
Poland_meat_graph <- ggplot(Meat3, aes(x=Year, y=Poland)) + geom_point() + 
  labs(title="Total Meat imports from Poland", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, 50))
Poland_meat_graph


## Uraguay - Year versus total meat imports 
Meat3$Uraguay<- as.numeric(as.character(Meat3$V7))
Uraguay_meat_graph <- ggplot(Meat3, aes(x=Year, y=Uraguay)) + geom_point() + 
  labs(title="Total Meat imports from Uraguay", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0, 300, 50))
Uraguay_meat_graph


## ROW - Year versus total Meat imports 
Meat3$ROW<- as.numeric(as.character(Meat3$V8))
ROW_meat_graph <- ggplot(Meat3, aes(x=Year, y=ROW)) + geom_point() + 
  labs(title="Total Meat imports from Rest of World", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,500), breaks = seq(0, 500, 50))
ROW_meat_graph


##### ADD IN TOTAL #######




Vegetables[1]
## Veg. plots
## Mexico - Year versus total veg imports 
Vegetables3$Mexico<- as.numeric(as.character(Vegetables3$V1))
Mexico_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Mexico)) + geom_point() + 
  labs(title="Total Vegetable imports from Mexico", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(1750,6000), breaks = seq(1750, 6000, 250))
Mexico_veg_graph


## Canada - Year versus total veg imports 
Vegetables3$Canada<- as.numeric(as.character(Vegetables3$V2))
Canada_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Canada)) + geom_point() + 
  labs(title="Total Vegetable imports from Canada", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(1000,3000), breaks = seq(1000, 3000, 250))
Canada_veg_graph


## China - Year versus total veg imports 
Vegetables3$China<- as.numeric(as.character(Vegetables3$V3))
China_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=China)) + geom_point() + 
  labs(title="Total Vegetable imports from China", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(100,700), breaks = seq(100, 700,100))
China_veg_graph


## Peru - Year versus total veg imports 
Vegetables3$Peru<- as.numeric(as.character(Vegetables3$V4))
Peru_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Peru)) + geom_point() + 
  labs(title="Total Vegetable imports from Peru", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(100,700), breaks = seq(100, 700,100))
Peru_veg_graph


## Spain - Year versus total veg imports 
Vegetables3$Spain<- as.numeric(as.character(Vegetables3$V5))
Spain_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Spain)) + geom_point() + 
  labs(title="Total Vegetable imports from Spain", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(200,400), breaks = seq(200, 400,50))
Spain_veg_graph


## Guatemala - Year versus total veg imports 
Vegetables3$Guatemala<- as.numeric(as.character(Vegetables3$V6))
Guatemala_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Guatemala)) + geom_point() + 
  labs(title="Total Vegetable imports from Guatemala", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0, 200,50))
Guatemala_veg_graph



## Netherlands - Year versus total veg imports 
Vegetables3$Netherlands<- as.numeric(as.character(Vegetables3$V7))
Netherlands_veg_graph <- ggplot(Vegetables3, aes(x=Year, y=Netherlands)) + geom_point() + 
  labs(title="Total Vegetable imports from Netherlands", x="Year", y="Million $") + 
  scale_x_continuous(breaks=2002:2017) + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_y_continuous(limits = c(50,200), breaks = seq(50, 200,50))
Netherlands_veg_graph
