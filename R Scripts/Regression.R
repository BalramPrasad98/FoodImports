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

#################### Dairy #################### 

#Formatting Dairy DataFrame 
Dairy2 <- as.data.frame(t(Dairy))
Country <- Dairy2[1,]
Dairy3 <- Dairy2[-1,]
Dairy3$Year <- 2002:2017
Dairy3$Total <- as.numeric(as.character(Dairy3$V10))


# Model
Dairy4 <- Dairy3
Dairy4$Year <- 0:15

Dairy_Model <- lm(Total~ Year + I(Year^2), Dairy4)
summary(Dairy_Model)
Dairy_Model

#################### Fruit #################### 

## Formatting Fruit dataframe
Fruit2 <- as.data.frame(t(Fruit))
Fruit_Country <- Fruit2[1,]
Fruit3 <- Fruit2[-1,]
Fruit3$Year <- 2002:2017
Fruit3$Total <- as.numeric(as.character(Fruit3$V12))


# Model
Fruit4 <- Fruit3
Fruit4$Year <- 0:15
Fruit_Model <- lm(Total~ Year,Fruit4)
Fruit_Model
summary(Fruit_Model)

# check MLR assumptions
Fruit_Output <- data.frame(fitted = Fruit_Model$fitted.values,
                           residuals = Fruit_Model$residuals)

ggplot(Fruit_Output,aes(x=fitted,y=residuals)) +
  geom_point() + 
  labs(title="Fruit Model Residual Plot") +
  geom_hline(yintercept = 0,col="red",size=2) 

#################### Grain #################### 

## Formatting Grains
Grain2 <- as.data.frame(t(Grain))
Grain_Country <- Grain2[1,]
Grain3 <- Grain2[-1,]
Grain3$Year <- 2002:2017
Grain3$Total <- as.numeric(as.character(Grain3$V11))

# Model
Grain4 <- Grain3
Grain4$Year <- 0:15
Grain_Model <- lm(Total~ Year,Grain4)
Grain_Model
summary(Grain_Model)

# check MLR assumptions
Grain_Output <- data.frame(fitted = Grain_Model$fitted.values,
                           residuals = Grain_Model$residuals)

ggplot(Grain_Output,aes(x=fitted,y=residuals)) +
  geom_point() + 
  labs(title="Grain Model Residual Plot") +
  geom_hline(yintercept = 0,col="red",size=2) 

#################### Meat #################### 

## Formatting Meat
Meat2 <- as.data.frame(t(Meat))
Meat_Country <- Meat2[1,]
Meat3 <- Meat2[-1,]
Meat3$Year <- 2002:2017
Meat3$Total <- as.numeric(as.character(Meat3$V9))

# Model
Meat4 <- Meat3
Meat4$Year <- 1:16
Meat_Model <- lm(Total~ Year, Meat4)
summary(Meat_Model)
Meat_Model

# check MLR assumptions
Meat_Output <- data.frame(fitted = Meat_Model$fitted.values,
                          residuals = Meat_Model$residuals)

ggplot(Meat_Output,aes(x=fitted,y=residuals)) +
  geom_point() + 
  labs(title="Meat Model Residual Plot") +
  geom_hline(yintercept = 0,col="red",size=2) 

#################### Vegetables #################### 

#Formatting Vegetables DataFrame for ggplot
Vegetables2 <- as.data.frame(t(Vegetables))
Country <- Vegetables2[1,]
Vegetables3 <- Vegetables2[-1,]
Vegetables3$Year <- 2002:2017
Vegetables3$Total <- as.numeric(as.character(Vegetables3$V9))

# Model
Vegetables4 <- Vegetables3
Vegetables4$Year <- 0:15
Vegetables_Model <- lm(Total~ Year,Vegetables4)
Vegetables_Model
summary(Vegetables_Model)

# check MLR assumptions
Vegetables_Output <- data.frame(fitted = Vegetables_Model$fitted.values,
                                residuals = Vegetables_Model$residuals)

ggplot(Vegetables_Output,aes(x=fitted,y=residuals)) +
  geom_point() + 
  labs(title="Vegetables Model Residual Plot") +
  geom_hline(yintercept = 0,col="red",size=2) 

